use anyhow::Result;
use futures::executor::block_on;
use serde::Deserialize;
use serde_yaml::Mapping;
use std::fs;
use structopt::StructOpt;

use upcake::reporters::SimpleReporter;
use upcake::{upcake, Config as UpcakeConfig, RequestConfig};

/// Configuration applied to all requests
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
	/// Follow redirects
	#[serde(default)]
	pub location: bool,
	/// Allow insecure server connections when using SSL
	#[serde(default)]
	pub insecure: bool,
	/// Maximum time allowed for connection
	#[serde(default)]
	pub connect_timeout: Option<u64>,
	/// Set additional variables as key=value or YAML. For a file prepend with @
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
	#[structopt(short = "L", long = "location")]
	/// Follow redirects
	location: bool,

	#[structopt(short = "k", long = "insecure")]
	/// Allow insecure server connections when using SSL
	insecure: bool,

	#[structopt(name = "MILLIS", long = "connect-timeout")]
	/// Maximum time allowed for connection
	connect_timeout: Option<u64>,

	#[structopt(name = "EXTRA_VARS", short = "e", long = "extra-vars")]
	/// Set additional variables as key=value or YAML. For a file prepend with @
	extra_vars: Option<Vec<String>>,

	#[structopt(short = "v", long = "verbose")]
	/// Verbose output
	verbose: bool,

	#[structopt(name = "BYTES", short = "s", long = "max-response-size")]
	/// Maximum response size in bytes
	max_response_size: Option<usize>,

	/// Path to the request config
	config: String,
}

impl Opt {
	fn merge_onto_config(&self, config: &mut Config) {
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
	let opt = Opt::from_args();
	let mut config: Config = serde_yaml::from_str(&fs::read_to_string(&opt.config)?)?;

	opt.merge_onto_config(&mut config);

	let mut context = Mapping::new();
	if let Some(extra_vars) = config.extra_vars {
		for (k, v) in extra_vars.iter() {
			context.insert(k.clone(), v.clone());
		}
	}

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

	let config = UpcakeConfig {
		location: config.location,
		insecure: config.insecure,
		connect_timeout: config.connect_timeout,
		verbose: config.verbose,
		max_response_size: config.max_response_size,
		requests: config.requests,
	};

	let mut reporter = SimpleReporter;
	block_on(upcake(config, Some(context), &mut reporter))
}
