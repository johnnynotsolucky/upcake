use anyhow::Result;
use futures::executor::block_on;
use serde_yaml::Mapping;
use std::fs;
use structopt::StructOpt;

use upcake::reporters::TapReporter;
use upcake::{upcake, Config, RequestConfig};

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

// fn fold_kv_map(mut state: HashMap<String, String>, val: &str) -> Result<HashMap<String, String>> {
// state
// }

fn main() -> Result<()> {
	let opt = Opt::from_args();
	let requests: Vec<RequestConfig> = serde_yaml::from_str(&fs::read_to_string(&opt.config)?)?;

	let config = Config {
		location: opt.location,
		insecure: opt.insecure,
		connect_timeout: opt.connect_timeout,
		verbose: opt.verbose,
		max_response_size: opt.max_response_size,
	};

	let mut context = Mapping::new();

	if let Some(extra_vars) = opt.extra_vars {
		for var in extra_vars.into_iter() {
			if let Some(path) = var.strip_prefix('@') {
				let yaml = serde_yaml::from_str::<Mapping>(&fs::read_to_string(&path)?)?;
				for (k, v) in yaml.into_iter() {
					context.insert(k, v);
				}
			} else if let Ok(yaml) = serde_yaml::from_str::<Mapping>(&var) {
				for (k, v) in yaml.into_iter() {
					context.insert(k, v);
				}
			} else if let Some((k, v)) = var.split_once('=') {
				context.insert(k.into(), v.into());
			}
		}
	}

	let mut reporter = TapReporter::new();
	block_on(upcake(config, requests, Some(context), &mut reporter))
}
