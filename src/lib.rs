pub mod assertions;
pub mod reporters;

use anyhow::Result;
use futures::future;
use handlebars::Handlebars;
use httpstat::{httpstat, Config as HttpstatConfig};
use httpstat::{Header, StatResult as HttpstatResult, Timing as HttpstatTiming};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::str;
use std::sync::Arc;
use std::time::Duration;
use thiserror::Error;
use tokio::task;

use assertions::{AssertionConfig, Equal, RequestAssertionConfig};
use reporters::Reporter;

#[derive(Deserialize, Default, Debug, Clone)]
pub struct RequestConfig {
	pub summary: Option<String>,
	#[serde(default = "default_request_method")]
	pub request: String,
	pub connect_timeout: Option<u64>,
	pub data: Option<String>,
	pub headers: Option<HashMap<String, String>>,
	pub url: String,
	#[serde(default = "default_assertions")]
	pub assertions: Vec<AssertionConfig>,
}

pub fn default_request_method() -> String {
	"GET".into()
}

pub fn default_assertions() -> Vec<AssertionConfig> {
	vec![AssertionConfig::Equal(RequestAssertionConfig {
		skip: None,
		path: ".\"response_code\"".into(),
		assertion: Equal {
			value: serde_yaml::to_value(200).unwrap(),
		},
	})]
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
	pub location: bool,
	pub insecure: bool,
	pub connect_timeout: Option<u64>,
	pub verbose: bool,
}

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

#[derive(Debug, Error)]
pub enum SomeError {
	#[error("{0:?}")]
	RequestError(RequestConfig, String),
}

fn run_request<T: Serialize>(
	config: Arc<Config>,
	request_config: RequestConfig,
	context: Arc<T>,
) -> Result<(RequestConfig, StatResult)> {
	let handlebars = Handlebars::new();
	let headers = match request_config.headers {
		Some(ref headers) => {
			let mut joined_headers: Vec<String> = Vec::new();

			for (header, value) in headers {
				let rendered_value = handlebars.render_template(value, &*context)?;
				joined_headers.push(format!("{}: {}", header, rendered_value));
			}

			Some(joined_headers)
		}
		None => None,
	};

	let url = handlebars.render_template(&request_config.url, &*context)?;
	let data = match request_config.data {
		Some(ref data) => Some(handlebars.render_template(data, &*context)?),
		None => None,
	};

	let httpstat_config = HttpstatConfig {
		location: config.location,
		insecure: config.insecure,
		verbose: config.verbose,
		connect_timeout: config.connect_timeout.map(Duration::from_millis),

		request: request_config.request.clone(),
		url,
		data,
		headers,
	};

	match httpstat(&httpstat_config) {
		Ok(httpstat_result) => Ok((request_config, httpstat_result.into())),
		Err(error) => Err(SomeError::RequestError(request_config, error.to_string()).into()),
	}
}

pub async fn upcake<T>(config: Config, requests: Vec<RequestConfig>, context: T, reporter: &mut dyn Reporter) -> Result<()>
where
	T: Serialize + std::marker::Sync + std::marker::Send + 'static,
{
	let context = Arc::new(context);
	let config = Arc::new(config);
	let mut request_futures = Vec::new();

	for request_config in requests {
		let context = context.clone();
		let config = config.clone();
		// let request_config = request_config.clone();

		request_futures.push(task::spawn_blocking(|| {
			run_request(config, request_config, context)
		}))
	}

	reporter.start();

	let results = future::join_all(request_futures).await;

	for result in results {
		match result? {
			Ok((request_config, stat_result)) => {
				reporter.step_suite(&request_config);
				let json_result = serde_json::to_value(&stat_result)?;

				for assertion in request_config.assertions.iter() {
					reporter.step_result(assertion.assert(&json_result)?);
				}
			}
			Err(error) => match error.downcast_ref::<SomeError>() {
				Some(error) => match error {
					SomeError::RequestError(request_config, error) => {
						reporter.step_suite(request_config);
						reporter.bail(error.to_string());
					}
				},
				None => {
					reporter.bail(format!("{}", error));
				}
			},
		}
	}

	reporter.end();

	Ok(())
}
