pub mod assertions;
pub mod reporters;

use anyhow::Result;
use handlebars::Handlebars;
use httpstat::{httpstat, Config as HttpstatConfig};
use httpstat::{Header, StatResult as HttpstatResult, Timing as HttpstatTiming};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::str;
use std::sync::Arc;
use std::task::{Context, Poll};
use std::time::Duration;
use thiserror::Error;

use assertions::{AssertionConfig, Equal, RequestAssertionConfig};
use reporters::Reporter;

/// Configuration for individual requests
#[derive(Deserialize, Default, Debug, Clone)]
pub struct RequestConfig {
	/// Name of the request
	pub name: Option<String>,
	/// Specify the request command to use, i.e. "GET"
	#[serde(default = "default_request_method")]
	pub request_method: String,
	/// Data to pass with the request.
	///
	/// Contents are rendered with Handlebars.
	///
	/// Send the contents of a file by prefixing with an @: `"@/path/to/file.json"`
	pub data: Option<String>,
	/// Key/Value pairs of headers to send with the request
	pub headers: Option<HashMap<String, String>>,
	/// The url to request
	pub url: String,
	/// A list of assertions to perform on the response
	#[serde(default = "default_assertions")]
	pub assertions: Vec<AssertionConfig>,
}

pub(crate) fn default_request_method() -> String {
	"GET".into()
}

pub(crate) fn default_assertions() -> Vec<AssertionConfig> {
	vec![AssertionConfig::Equal(RequestAssertionConfig {
		skip: None,
		path: ".\"response_code\"".into(),
		assertion: Equal(serde_yaml::to_value(200).unwrap()),
	})]
}

/// Configuration applied to all requests
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
	/// Follow redirects
	pub location: bool,
	/// Allow insecure server connections when using SSL
	pub insecure: bool,
	/// Maximum time allowed for connection
	pub connect_timeout: Option<u64>,
	/// Verbose output
	pub verbose: bool,
	/// Maximum response size in bytes
	pub max_response_size: Option<usize>,
}

/// Timing results for a request
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Timing {
	/// Duration in milliseconds from the start of the request until name lookup resolved
	pub namelookup_time: u64,
	/// Duration in milliseconds from the start of the request until a connection to the remote
	/// host is established
	pub connect_time: u64,
	/// Duration in milliseconds from the start of the request until file transfer was about to
	/// begin
	pub pretransfer_time: u64,
	/// Duration in milliseconds from the start of the request until the first byte was received
	pub starttransfer_time: u64,
	/// Duration in milliseconds from the start of the request until the request ended
	pub total_time: u64,
	/// Same as [`Timing::namelookup_time`]
	pub dns_resolution_time: u64,
	/// Difference of [`Timing::connect_time`] and [`Timing::namelookup_time`]
	pub tcp_connection_time: u64,
	/// Difference of [`Timing::pretransfer_time`] and [`Timing::connect_time`]
	pub tls_connection_time: u64,
	/// Difference of [`Timing::starttransfer_time`] and [`Timing::pretransfer_time`]
	pub server_processing_time: u64,
	/// Difference of [`Timing::total_time`] and [`Timing::starttransfer_time`]
	pub content_transfer_time: u64,
}

/// Converts from [`httpstat::Timing`] to [`Self`]
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
			if let Ok(json_content) = serde_json::from_str(body) {
				stat_result.json = Some(json_content);
			}

			stat_result.body = Some(body.into());
		}

		stat_result
	}
}

#[derive(Debug, Error)]
pub enum Error {
	#[error("{0:?}")]
	RequestError(RequestConfig, String),
}

async fn run_request<T: Serialize>(
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
		max_response_size: config.max_response_size,

		request: request_config.request_method.clone(),
		url,
		data,
		headers,
	};

	match httpstat(&httpstat_config).await {
		Ok(httpstat_result) => Ok((request_config, httpstat_result.into())),
		Err(error) => Err(Error::RequestError(request_config, error.to_string()).into()),
	}
}

enum State<F, T>
where
	F: Future<Output = Result<T>>,
{
	Future(F),
	Ok(Result<T>),
}

struct JoinAll<F, T>(Vec<State<F, T>>)
where
	F: Future<Output = Result<T>>;

impl<F, T> Future for JoinAll<F, T>
where
	F: Future<Output = Result<T>>,
{
	type Output = Result<Vec<Result<T>>>;

	fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		let this = unsafe { self.get_unchecked_mut() };
		let states = &mut this.0;

		let mut all_ready = true;

		for state in states.iter_mut() {
			if let State::Future(future) = state {
				match unsafe { Pin::new_unchecked(future) }.poll(context) {
					Poll::Ready(result) => *state = State::Ok(result),
					Poll::Pending => {
						all_ready = false;
						continue;
					}
				}
			}
		}

		if all_ready {
			let states = std::mem::take(states);
			let results = states
				.into_iter()
				.map(|state| match state {
					State::Ok(result) => result,
					_ => unreachable!(),
				})
				.collect();

			Poll::Ready(Ok(results))
		} else {
			Poll::Pending
		}
	}
}

fn join_all<F, T>(futures: Vec<F>) -> impl Future<Output = Result<Vec<Result<T>>>>
where
	F: Future<Output = Result<T>>,
{
	JoinAll(futures.into_iter().map(State::Future).collect())
}

pub async fn upcake<T>(
	config: Config,
	requests: Vec<RequestConfig>,
	context: T,
	reporter: &mut dyn Reporter,
) -> Result<()>
where
	T: Serialize + std::marker::Sync + std::marker::Send + 'static,
{
	let context = Arc::new(context);
	let config = Arc::new(config);
	let mut request_futures = Vec::new();

	for request_config in requests {
		let context = context.clone();
		let config = config.clone();

		request_futures.push(run_request(config, request_config, context))
	}

	reporter.start();

	let results = join_all(request_futures).await?;

	for result in results {
		match result {
			Ok((request_config, stat_result)) => {
				reporter.step_suite(&request_config);
				let result = serde_yaml::to_value(&stat_result)?;

				for assertion in request_config.assertions.iter() {
					reporter.step_result(assertion.assert(&result)?);
				}
			}
			Err(error) => match error.downcast_ref::<Error>() {
				Some(error) => match error {
					Error::RequestError(request_config, error) => {
						reporter.step_suite(request_config);
						reporter.bail(error.to_string());
						break;
					}
				},
				None => {
					// TODO return an error which includes the request_config
					reporter.bail(format!("{}", error));
					break;
				}
			},
		}
	}

	reporter.end();

	Ok(())
}
