use anyhow::{anyhow, Result};
use futures::executor::block_on;
use serde::Deserialize;
use serde_yaml::Mapping as YamlMapping;
use std::path::{self, PathBuf};
use std::{env, fs, process};
use structopt::StructOpt;

use upcake::observer::write::WriteObserver;
use upcake::{upcake, Config as UpcakeConfig, RequestConfig, RequestData, UpcakeResult};

/// Returns a value of $path scoped to $base_path.
macro_rules! set_scoped_path {
	($base_path:expr, $path:expr) => {
		if $path.is_some() {
			let config_path: PathBuf = $path.as_ref().unwrap().into();
			let path: PathBuf = [$base_path, &config_path].iter().collect();

			if !path.is_file() {
				return Err(anyhow!("File not found: {}", $path.unwrap()));
			}

			let path = path.into_os_string().into_string();
			let ret: Result<Option<String>> = match path {
				Ok(path) => Ok(Some(format!("{}", path))),
				Err(_) => return Err(anyhow!("Invalid path: {}", $path.unwrap())),
			};

			ret
		} else {
			Ok(None)
		}
	};
}

const UPCAKE_CONFIG_ENV_KEY: &str = "UPCAKE_CONFIG";
const UPCAKE_CONFIG_DEFAULT_FILE: &str = "Upcakefile.yaml";

/// Configuration applied to all requests
#[derive(StructOpt, Deserialize, Default, Debug, Clone)]
struct Config {
	/// An optional prefix to filter environment variables injected into the template context
	#[serde(default)]
	#[structopt(long = "env-var-prefix")]
	env_var_prefix: Option<String>,

	/// Follow redirects
	#[serde(default)]
	#[structopt(short = "L", long = "location")]
	location: bool,

	/// Allow insecure server connections when using SSL
	#[serde(default)]
	#[structopt(short = "k", long = "insecure")]
	insecure: bool,

	/// Client certificate file
	#[serde(default)]
	#[structopt(name = "cert file", short = "E", long = "cert")]
	client_cert: Option<String>,

	/// Private key file
	#[serde(default)]
	#[structopt(name = "key file", long = "key")]
	client_key: Option<String>,

	/// CA certificate to verify against
	#[serde(default)]
	#[structopt(name = "ca file", long = "cacert")]
	ca_cert: Option<String>,

	/// Maximum time allowed for connection
	#[serde(default)]
	#[structopt(name = "MILLIS", long = "connect-timeout")]
	connect_timeout: Option<u64>,

	/// Set additional variables as YAML. For a file prepend with @
	#[serde(default)]
	#[structopt(skip)]
	extra_vars: YamlMapping,

	/// Set additional variables as key=value or YAML. For a file prepend with @
	#[serde(skip_deserializing)]
	#[structopt(name = "EXTRA_VARS", short = "e", long = "extra-vars")]
	extra_vars_opt: Vec<String>,

	/// Verbose output
	#[serde(default)]
	#[structopt(short = "v", long = "verbose")]
	verbose: bool,

	/// Fail on HTTP error response codes
	#[serde(default)]
	#[structopt(long = "fail-request")]
	fail_request: bool,

	/// Maximum response size in bytes
	#[serde(default)]
	#[structopt(name = "BYTES", short = "s", long = "max-response-size")]
	max_response_size: Option<usize>,

	/// Path to the request config file
	#[serde(skip_deserializing)]
	#[structopt(name = "PATH", short = "c", long = "config-file", env = UPCAKE_CONFIG_ENV_KEY)]
	config_file: Option<String>,

	/// Requests to run
	#[structopt(skip)]
	requests: Vec<RequestConfig>,
}

macro_rules! apply_option_field {
	($field:expr, $new_val:expr) => {
		if let Some(value) = $new_val {
			*$field = Some(value);
		}
	};
}

fn apply_boolean_field(current: &mut bool, new: bool) {
	if new {
		*current = new;
	}
}

impl Config {
	fn apply(&mut self, config: Config) -> Result<()> {
		apply_boolean_field(&mut self.location, config.location);
		apply_boolean_field(&mut self.insecure, config.insecure);
		apply_boolean_field(&mut self.verbose, config.verbose);
		apply_boolean_field(&mut self.fail_request, config.fail_request);

		apply_option_field!(&mut self.env_var_prefix, config.env_var_prefix);
		apply_option_field!(&mut self.client_cert, config.client_cert);
		apply_option_field!(&mut self.client_key, config.client_key);
		apply_option_field!(&mut self.ca_cert, config.ca_cert);
		apply_option_field!(&mut self.connect_timeout, config.connect_timeout);
		apply_option_field!(&mut self.max_response_size, config.max_response_size);

		// Add any extra variables set through the CLI
		for var in config.extra_vars_opt.iter() {
			if let Some(path) = var.strip_prefix('@') {
				// Fetch extra vars stored in a YAML file
				let yaml = serde_yaml::from_str::<YamlMapping>(&fs::read_to_string(&path)?)?;
				for (k, v) in yaml.into_iter() {
					self.extra_vars.insert(k, v);
				}
			} else if let Ok(yaml) = serde_yaml::from_str::<YamlMapping>(var) {
				// Set extra vars from YAML passed in through args
				for (k, v) in yaml.into_iter() {
					self.extra_vars.insert(k, v);
				}
			} else if let Some((k, v)) = var.split_once('=') {
				// Set extra vars from key/value pairs passed in through args
				self.extra_vars.insert(k.into(), v.into());
			}
		}

		Ok(())
	}
}

impl From<Config> for UpcakeConfig {
	fn from(config: Config) -> UpcakeConfig {
		UpcakeConfig {
			env_var_prefix: config.env_var_prefix,
			location: config.location,
			insecure: config.insecure,
			connect_timeout: config.connect_timeout,
			verbose: config.verbose,
			fail_request: config.fail_request,
			max_response_size: config.max_response_size,
			requests: config.requests,
			client_cert: config.client_cert,
			client_key: config.client_key,
			ca_cert: config.ca_cert,
		}
	}
}

fn main() -> Result<()> {
	match env::var(UPCAKE_CONFIG_ENV_KEY) {
		Ok(path) => {
			if path.trim().is_empty() {
				env::set_var(UPCAKE_CONFIG_ENV_KEY, UPCAKE_CONFIG_DEFAULT_FILE);
			}
		}
		Err(_) => {
			env::set_var(UPCAKE_CONFIG_ENV_KEY, UPCAKE_CONFIG_DEFAULT_FILE);
		}
	}

	let opt = Config::from_args();

	let mut config: Config;
	let mut config_dir: PathBuf = env::current_dir()?;

	if let Some(ref config_file) = opt.config_file {
		if let Some(parent) = PathBuf::from(config_file).parent() {
			if !parent.as_os_str().is_empty() {
				config_dir = parent.to_path_buf();
			}
		}

		match fs::read_to_string(&path::Path::new(config_file)) {
			Ok(config_file) => {
				config = serde_yaml::from_str(&config_file)?;

				config.client_cert = set_scoped_path!(&config_dir, config.client_cert)?;
				config.client_key = set_scoped_path!(&config_dir, config.client_key)?;
				config.ca_cert = set_scoped_path!(&config_dir, config.ca_cert)?;
			}
			Err(_) => return Err(anyhow!("Invalid config file: {}", config_file)),
		}
	} else {
		unreachable!();
	}

	config.apply(opt)?;

	let mut context = YamlMapping::new();
	for (k, v) in config.extra_vars.iter() {
		context.insert(k.clone(), v.clone());
	}

	for request in &mut config.requests {
		// Update the file paths for request data to be scoped to the config directory
		if let Some(RequestData::FilePath(ref mut path)) = request.data {
			*path = set_scoped_path!(&config_dir, Some(path.clone()))?.unwrap();
		}
	}

	let observer = Box::new(WriteObserver::default());
	let res = block_on(upcake(config.into(), Some(context), observer))?;

	if let UpcakeResult::Failures(failure_count) = res {
		process::exit(failure_count as i32);
	}

	Ok(())
}
