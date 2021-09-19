use anyhow::Result;
use serde::Serialize;
use std::collections::HashMap;
use std::env;
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

	#[structopt(name = "millis", long = "connect-timeout")]
	/// Maximum time allowed for connection
	connect_timeout: Option<u64>,

	#[structopt(short = "v", long = "verbose")]
	/// Verbose output
	verbose: bool,

	#[structopt(name = "bytes", short = "s", long = "max-response-size")]
	/// Maximum response size in bytes
	max_response_size: Option<usize>,

	/// Path to the request config
	config: String,
}

#[tokio::main]
async fn main() -> Result<()> {
	let opt = Opt::from_args();
	let requests: Vec<RequestConfig> = serde_yaml::from_str(&fs::read_to_string(&opt.config)?)?;

	let config = Config {
		location: opt.location,
		insecure: opt.insecure,
		connect_timeout: opt.connect_timeout,
		verbose: opt.verbose,
		max_response_size: opt.max_response_size,
	};

	let context = Context {
		env: env::vars().collect(),
	};

	let mut reporter = TapReporter::new();
	upcake(config, requests, context, &mut reporter).await
}

#[derive(Serialize, Debug, Clone)]
pub struct Context {
	pub env: HashMap<String, String>,
}
