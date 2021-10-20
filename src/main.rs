use anyhow::{anyhow, Result};
use futures::executor::block_on;
use httpstat::Header;
use serde::Deserialize;
use serde_yaml::Mapping;
use std::path::{self, PathBuf};
use std::{env, fs, process};
use structopt::StructOpt;

use upcake::assertions::{AssertionConfig, Equal, RequestAssertionConfig};
use upcake::reporters::SimpleReporter;
use upcake::{upcake, Config as UpcakeConfig, RequestConfig, UpcakeResult};

const UPCAKE_CONFIG_ENV_KEY: &str = "UPCAKE_CONFIG";
const UPCAKE_CONFIG_DEFAULT_FILE: &str = "Upcakefile.yaml";

/// Configuration applied to all requests
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
	/// An optional prefix to filter environment variables injected into the template context
	#[serde(default)]
	pub env_var_prefix: Option<String>,
	/// Follow redirects
	#[serde(default)]
	pub location: bool,
	/// Allow insecure server connections when using SSL
	#[serde(default)]
	pub insecure: bool,
	/// Maximum time allowed for connection
	#[serde(default)]
	pub connect_timeout: Option<u64>,
	/// Set additional variables as YAML. For a file prepend with @
	#[serde(default)]
	extra_vars: Option<Mapping>,
	/// Verbose output
	#[serde(default)]
	pub verbose: bool,
	/// Maximum response size in bytes
	#[serde(default)]
	pub max_response_size: Option<usize>,
	/// Requests to run
	pub requests: Vec<RequestConfig>,
}

#[derive(Debug, Clone, StructOpt)]
#[structopt()]
struct Opt {
	/// An optional prefix to filter environment variables injected into the template context
	#[structopt(long = "env-var-prefix")]
	env_var_prefix: Option<String>,
	/// Follow redirects
	#[structopt(short = "L", long = "location")]
	location: bool,

	/// Allow insecure server connections when using SSL
	#[structopt(short = "k", long = "insecure")]
	insecure: bool,

	/// Maximum time allowed for connection
	#[structopt(name = "MILLIS", long = "connect-timeout")]
	connect_timeout: Option<u64>,

	/// Set additional variables as key=value or YAML. For a file prepend with @
	#[structopt(name = "EXTRA_VARS", short = "e", long = "extra-vars")]
	extra_vars: Option<Vec<String>>,

	/// Verbose output
	#[structopt(short = "v", long = "verbose")]
	verbose: bool,

	/// Maximum response size in bytes
	#[structopt(name = "BYTES", short = "s", long = "max-response-size")]
	max_response_size: Option<usize>,

	/// Path to the request config file
	#[structopt(name = "PATH", short = "c", long = "config-file", env = UPCAKE_CONFIG_ENV_KEY)]
	config: Option<String>,

	#[structopt(flatten)]
	request: Request,
}

#[derive(Debug, Clone, StructOpt)]
#[structopt()]
struct Request {
	/// URL to run default assertions against (Uses default request config)
	#[structopt(name = "URL", short = "u", long = "url")]
	url: Option<String>,

	/// Specify request method to use. Used in conjunction with --url.
	#[structopt(
		name = "command",
		short = "X",
		long = "request-method",
		default_value = "GET"
	)]
	request_method: String,

	/// Pass custom header(s) to server. Used in conjunction with --url.
	#[structopt(short = "H", long = "header")]
	headers: Option<Vec<Header>>,

	/// Verify the response code. Used in conjunction with --url.
	#[structopt(long = "status-code", default_value = "200")]
	status_code: u16,
}

impl Opt {
	fn merge_onto_config(&self, config: &mut Config) {
		if let Some(ref env_var_prefix) = self.env_var_prefix {
			config.env_var_prefix = Some(env_var_prefix.clone());
		}
		if self.location {
			config.location = self.location;
		}
		if self.insecure {
			config.insecure = self.insecure;
		}
		if let Some(timeout) = self.connect_timeout {
			config.connect_timeout = Some(timeout);
		}
		if self.verbose {
			config.verbose = self.verbose;
		}
		if let Some(max_response_size) = self.max_response_size {
			config.max_response_size = Some(max_response_size);
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

	let opt = Opt::from_args();

	let mut config: Config;
	let mut config_dir: PathBuf = env::current_dir()?;

	if let Some(ref url) = opt.request.url {
		config = Config {
			requests: vec![RequestConfig {
				url: url.clone(),
				request_method: opt.request.request_method.clone(),
				headers: opt.request.headers.clone(),
				assertions: vec![AssertionConfig::Equal(RequestAssertionConfig {
					skip: None,
					path: Some(".\"response_code\"".into()),
					assertion: Equal::new(opt.request.status_code),
				})],
				..Default::default()
			}],
			..Default::default()
		}
	} else if let Some(ref config_file) = opt.config {
		config_dir = PathBuf::from(config_file);
		config_dir.pop();
		match fs::read_to_string(&path::Path::new(config_file)) {
			Ok(config_file) => config = serde_yaml::from_str(&config_file)?,
			Err(_) => return Err(anyhow!("Invalid config file: {}", config_file)),
		}
	} else {
		unreachable!()
	}

	opt.merge_onto_config(&mut config);

	let mut context = Mapping::new();
	if let Some(extra_vars) = config.extra_vars {
		for (k, v) in extra_vars.iter() {
			context.insert(k.clone(), v.clone());
		}
	}

	// Add any extra variables set through the CLI
	if let Some(extra_vars) = opt.extra_vars {
		for var in extra_vars.into_iter() {
			if let Some(path) = var.strip_prefix('@') {
				// Fetch extra vars stored in a YAML file
				let yaml = serde_yaml::from_str::<Mapping>(&fs::read_to_string(&path)?)?;
				for (k, v) in yaml.into_iter() {
					context.insert(k, v);
				}
			} else if let Ok(yaml) = serde_yaml::from_str::<Mapping>(&var) {
				// Set extra vars from YAML passed in through args
				for (k, v) in yaml.into_iter() {
					context.insert(k, v);
				}
			} else if let Some((k, v)) = var.split_once('=') {
				// Set extra vars from key/value pairs passed in through args
				context.insert(k.into(), v.into());
			}
		}
	}

	// Set the current working dir to be relative to the whatever directory the config file was
	// loaded from.
	let current_working_dir = env::current_dir()?;
	env::set_current_dir(config_dir)?;
	for mut request in &mut config.requests {
		let mut file_data = None;
		if let Some(ref data) = request.data {
			// Read in a file for request content if the data property is prefixed with `'@'`,
			// otherwise use whatever is set on [`RequestConfig::data`].
			if let Some(path) = data.strip_prefix('@') {
				file_data = Some(fs::read_to_string(path)?);
			}
		}

		if file_data.is_some() {
			request.data = file_data;
		}
	}
	env::set_current_dir(current_working_dir)?;

	let config = UpcakeConfig {
		env_var_prefix: config.env_var_prefix,
		location: config.location,
		insecure: config.insecure,
		connect_timeout: config.connect_timeout,
		verbose: config.verbose,
		max_response_size: config.max_response_size,
		requests: config.requests,
	};

	let mut reporter = SimpleReporter;
	let res = block_on(upcake(config, Some(context), &mut reporter))?;

	if let UpcakeResult::Failures(failure_count) = res {
		process::exit(failure_count as i32);
	}

	Ok(())
}
