use eyre::Result;
use httpstat::{httpstat, Config as HttpstatConfig};
use std::fs;
use std::time::Duration;
use structopt::StructOpt;

use upcake::reporters::{Reporter, TapReporter};
use upcake::{Config, RequestConfig, StatResult};

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

	/// Path to the request config
	config: String,
}

// TODO return maintain request state
fn run_request(
	opt: &Opt,
	request_config: RequestConfig,
	reporter: &mut dyn Reporter,
) -> Result<()> {
	let httpstat_config = HttpstatConfig {
		location: opt.location,
		insecure: opt.insecure,
		verbose: opt.verbose,
		connect_timeout: opt.connect_timeout.map(Duration::from_millis),

		request: request_config.request.clone(),
		url: request_config.url.clone(),
		data: request_config.data.clone(),
		headers: request_config.headers.clone(),
	};

	match httpstat(&httpstat_config) {
		Ok(httpstat_result) => {
			let stat_result: StatResult = httpstat_result.into();
			let json_result = serde_json::to_value(&stat_result)?;

			for assertion in request_config.assertions.iter() {
				reporter.step_result(assertion.assert(&json_result)?);
			}
		}
		Err(error) => {
			reporter.bail(error.to_string());
		}
	}

	Ok(())
}

fn main() -> Result<()> {
	let opt = Opt::from_args();

	let config: Config = serde_yaml::from_str(&fs::read_to_string(&opt.config)?)?;

	let mut reporter = TapReporter::new();
	reporter.start();
	for request_config in config.0 {
		reporter.step_suite(&request_config);
		run_request(&opt, request_config, &mut reporter)?;
	}
	reporter.end();

	Ok(())
}
