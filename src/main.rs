use eyre::Result;
use httpstat::{
    httpstat, Config as HttpstatConfig, Header, StatResult as HttpstatResult,
    Timing as HttpstatTiming,
};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::time::Duration;
use std::{fs, str};
use structopt::StructOpt;

use upcake::assertions::Assertion;
use upcake::reporters::{Reporter, TapReporter};

#[derive(Deserialize, Default, Debug, Clone)]
pub struct RequestConfig {
    pub request: String,
    pub connect_timeout: Option<u64>,
    pub data: Option<String>,
    pub headers: Option<Vec<String>>,
    pub url: String,
    pub assertions: Vec<Assertion>,
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config(Vec<RequestConfig>);

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Timing {
    pub namelookup_time: u64,
    pub connect_time: u64,
    pub pretransfer_time: u64,
    pub starttransfer_time: u64,
    pub total_time: u64,
    pub dns_resolution_time: u64,
    pub tcp_connection_time: u64,
    pub tls_connection_time: u64,
    pub server_processing_time: u64,
    pub content_transfer_time: u64,
}

impl From<HttpstatTiming> for Timing {
    fn from(timing: HttpstatTiming) -> Self {
        Self {
            namelookup_time: timing.namelookup_time.as_millis() as u64,
            connect_time: timing.connect_time.as_millis() as u64,
            pretransfer_time: timing.pretransfer_time.as_millis() as u64,
            starttransfer_time: timing.starttransfer_time.as_millis() as u64,
            total_time: timing.total_time.as_millis() as u64,
            dns_resolution_time: timing.dns_resolution_time.as_millis() as u64,
            tcp_connection_time: timing.tcp_connection_time.as_millis() as u64,
            tls_connection_time: timing.tls_connection_time.as_millis() as u64,
            server_processing_time: timing.server_processing_time.as_millis() as u64,
            content_transfer_time: timing.content_transfer_time.as_millis() as u64,
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct StatResult {
    pub http_version: String,
    pub response_code: i32,
    pub response_message: Option<String>,
    pub headers: Vec<Header>,
    pub timing: Timing,
    pub json: Option<JsonValue>,
    pub body: Option<String>,
}

impl From<HttpstatResult> for StatResult {
    fn from(result: HttpstatResult) -> Self {
        let mut stat_result = Self {
            http_version: result.http_version,
            response_code: result.response_code,
            response_message: result.response_message,
            headers: result.headers,
            timing: result.timing.into(),
            json: None,
            body: None,
        };

        if let Ok(body) = str::from_utf8(&result.body[..]) {
            let mut in_content = false;
            let lines: Vec<&str> = body
                .lines()
                .filter(|line| {
                    if line.is_empty() && !in_content {
                        in_content = true;
                        return false; // Don't include this line though.
                    }

                    in_content
                })
                .collect();
            let content = lines.join("\n");

            if let Ok(json_content) = serde_json::from_str(&content) {
                stat_result.json = json_content;
            }

            stat_result.body = content.into();
        }

        stat_result
    }
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
fn run_request(opt: &Opt, request_config: RequestConfig) -> Result<()> {
    let httpstat_config = HttpstatConfig {
        location: opt.location,
        insecure: opt.insecure,
        verbose: opt.verbose,
        connect_timeout: opt.connect_timeout.map(Duration::from_millis),

        request: request_config.request,
        url: request_config.url,
        data: request_config.data,
        headers: request_config.headers,
    };

    let assertions = request_config.assertions;

    let mut reporter = TapReporter::new();
    reporter.plan(&assertions);

    match httpstat(&httpstat_config) {
        Ok(httpstat_result) => {
            let stat_result: StatResult = httpstat_result.into();
            let json_result = serde_json::to_value(&stat_result)?;

            for (idx, assertion) in assertions.iter().enumerate() {
                reporter.step_result(idx, assertion.assert(&json_result)?);
            }
        }
        Err(error) => {
            reporter.bail(error.to_string());
        }
    }

    reporter.end();

    Ok(())
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    let config: Config = serde_yaml::from_str(&fs::read_to_string(&opt.config)?)?;

    for request_config in config.0 {
        run_request(&opt, request_config)?;
    }

    Ok(())
}
