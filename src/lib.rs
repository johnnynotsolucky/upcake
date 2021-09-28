pub mod assertions;
pub mod reporters;

use anyhow::Result;
use handlebars::Handlebars;
use httpstat::{httpstat, Config as HttpstatConfig};
use httpstat::{Header, StatResult as HttpstatResult, Timing as HttpstatTiming};
use serde::ser::{SerializeMap, Serializer};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::{HashMap, HashSet};
use std::env;
use std::future::Future;
use std::pin::Pin;
use std::str;
use std::sync::{Arc, Mutex};
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
	/// Vec of requests which this request depends on
	#[serde(default)]
	pub depends: Vec<String>,
	/// Specify the request command to use, i.e. "GET"
	#[serde(default = "default_request_method")]
	pub request_method: String,
	/// Data to pass with the request
	///
	/// Contents are rendered with Handlebars.
	///
	/// Send the contents of a file by prefixing with an @: `"@/path/to/file.json"`
	pub data: Option<String>,
	/// Key/Value pairs of headers to send with the request
	///
	/// Header values are rendered with Handlebars.
	pub headers: Option<HashMap<String, String>>,
	/// The url to request
	///
	/// The url string is rendered with Handlebars.
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
		assertion: Equal::new(200),
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
	pub namelookup: u64,
	/// Duration in milliseconds from the start of the request until a connection to the remote
	/// host is established
	pub connect: u64,
	/// Duration in milliseconds from the start of the request until file transfer was about to
	/// begin
	pub pretransfer: u64,
	/// Duration in milliseconds from the start of the request until the first byte was received
	pub starttransfer: u64,
	/// Duration in milliseconds from the start of the request until the request ended
	pub total: u64,
	/// Same as [`Timing::namelookup`]
	pub dns_resolution: u64,
	/// Difference of [`Timing::connect`] and [`Timing::namelookup`]
	pub tcp_connection: u64,
	/// Difference of [`Timing::pretransfer`] and [`Timing::connect`]
	pub tls_connection: u64,
	/// Difference of [`Timing::starttransfer`] and [`Timing::pretransfer`]
	pub server_processing: u64,
	/// Difference of [`Timing::total`] and [`Timing::starttransfer`]
	pub content_transfer: u64,
}

/// Converts from [`httpstat::Timing`] to [`Self`]
impl From<HttpstatTiming> for Timing {
	fn from(timing: HttpstatTiming) -> Self {
		Self {
			namelookup: timing.namelookup_time.as_millis() as u64,
			connect: timing.connect_time.as_millis() as u64,
			pretransfer: timing.pretransfer_time.as_millis() as u64,
			starttransfer: timing.starttransfer_time.as_millis() as u64,
			total: timing.total_time.as_millis() as u64,
			dns_resolution: timing.dns_resolution_time.as_millis() as u64,
			tcp_connection: timing.tcp_connection_time.as_millis() as u64,
			tls_connection: timing.tls_connection_time.as_millis() as u64,
			server_processing: timing.server_processing_time.as_millis() as u64,
			content_transfer: timing.content_transfer_time.as_millis() as u64,
		}
	}
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(untagged)]
pub enum ResponseContent {
	NoContent,
	Other(String),
	Json(JsonValue),
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct StatResult {
	pub http_version: String,
	pub response_code: i32,
	pub response_message: Option<String>,
	pub headers: Vec<Header>,
	pub timing: Timing,
	pub content: ResponseContent,
}

impl From<HttpstatResult> for StatResult {
	fn from(result: HttpstatResult) -> Self {
		let mut stat_result = Self {
			http_version: result.http_version,
			response_code: result.response_code,
			response_message: result.response_message,
			headers: result.headers,
			timing: result.timing.into(),
			content: ResponseContent::NoContent,
		};

		if let Ok(body) = str::from_utf8(&result.body[..]) {
			if let Ok(json_content) = serde_json::from_str(body) {
				stat_result.content = ResponseContent::Json(json_content);
			} else {
				stat_result.content = ResponseContent::Other(body.into());
			}
		}

		stat_result
	}
}

#[derive(Debug, Clone)]
pub struct ResponseData {
	inner: Arc<StatResult>,
}

impl From<Arc<StatResult>> for ResponseData {
	fn from(result: Arc<StatResult>) -> Self {
		Self { inner: result }
	}
}

impl Serialize for ResponseData {
	fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		let mut map = serializer.serialize_map(Some(2))?;
		map.serialize_entry("headers", &self.inner.headers)?;
		map.serialize_entry("content", &self.inner.content)?;
		map.end()
	}
}

#[derive(Debug, Error)]
pub enum Error {
	#[error("{0:?}")]
	RequestError(Arc<RequestConfig>, String),
}

async fn run_request<C>(
	config: Arc<Config>,
	request_config: Arc<RequestConfig>,
	context: Arc<Mutex<TemplateContext<C>>>,
) -> RequestResult
where
	C: Serialize,
{
	let handlebars = Handlebars::new();

	let headers = match request_config.headers {
		Some(ref headers) => {
			let mut joined_headers: Vec<String> = Vec::new();

			for (header, value) in headers {
				let rendered_value =
					handlebars.render_template(value, &*context.lock().unwrap())?;
				joined_headers.push(format!("{}: {}", header, rendered_value));
			}

			Some(joined_headers)
		}
		None => None,
	};

	let url = handlebars.render_template(&request_config.url, &*context.lock().unwrap())?;
	let data = match request_config.data {
		Some(ref data) => Some(handlebars.render_template(data, &*context.lock().unwrap())?),
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
		Ok(httpstat_result) => Ok((request_config, Arc::new(httpstat_result.into()))),
		Err(error) => Err(Error::RequestError(request_config, error.to_string()).into()),
	}
}

enum State<F>
where
	F: Future<Output = RequestResult>,
{
	/// Request configuration for requests which haven't started running yet
	Wait(Arc<RequestConfig>),
	/// The request future for running requests
	Future(F),
	/// Request has completed
	///
	/// Boxed so that allocation for [`State`] is not oversized just for this variant.
	Done(Box<RequestResult>),
}

type RequestResult = Result<(Arc<RequestConfig>, Arc<StatResult>)>;
type RequestState = State<Pin<Box<dyn Future<Output = RequestResult>>>>;

struct Requests<C>
where
	C: Serialize,
{
	config: Arc<Config>,
	context: Arc<Mutex<TemplateContext<C>>>,
	states: HashMap<String, RequestState>,
}

impl<C: 'static> Future for Requests<C>
where
	C: Serialize,
{
	type Output = Result<Vec<RequestResult>>;

	fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		let this = unsafe { self.get_unchecked_mut() };
		let states = &mut this.states;

		// Start any requests which have no depends or have depends which have all completed
		// successfully.
		let done_set: HashSet<_> = states
			.iter()
			.filter_map(|(key, state)| match state {
				State::Done(ref result) => match **result {
					Ok(_) => Some(key),
					_ => None,
				},
				_ => None,
			})
			.cloned()
			.collect();

		for (_, state) in states.iter_mut() {
			if let State::Wait(request_config) = state {
				// TODO validate that the depends keys actually exist
				let depends_set: HashSet<_> = request_config.depends.clone().into_iter().collect();
				let intersection: HashSet<_> = done_set.intersection(&depends_set).collect();

				if intersection.len() == depends_set.len() {
					*state = State::Future(Box::pin(run_request(
						this.config.clone(),
						request_config.clone(),
						this.context.clone(),
					)));
				}
			}
		}

		let mut all_ready = true;

		for (name, state) in states.iter_mut() {
			// Immediately poll...?
			match state {
				State::Future(future) => {
					match unsafe { Pin::new_unchecked(future) }.poll(context) {
						Poll::Ready(result) => {
							let result = Box::new(result);

							if let Ok((_, ref stat_result)) = *result {
								let mut context = this.context.lock().unwrap();
								context
									.requests
									.insert(name.clone(), ResponseData::from(stat_result.clone()));
							}

							*state = State::Done(result);
						}
						Poll::Pending => {
							all_ready = false;
							continue;
						}
					}
				}
				State::Wait(_) => {
					all_ready = false;
					continue;
				}
				State::Done(_) => continue,
			}
		}

		if all_ready {
			let states = std::mem::take(states);
			let results = states
				.into_values()
				.map(|state| match state {
					State::Done(result) => *result,
					_ => unreachable!(),
				})
				.collect();

			Poll::Ready(Ok(results))
		} else {
			Poll::Pending
		}
	}
}

#[derive(Serialize, Debug, Clone)]
struct TemplateContext<C>
where
	C: Serialize,
{
	user: Option<C>,
	env: HashMap<String, String>,
	/// requests is serialized when rendering templates, but we need an [`Arc`] so that we can pass
	/// the result around internally.
	requests: HashMap<String, ResponseData>,
}

pub async fn upcake<C: 'static>(
	config: Config,
	requests: Vec<RequestConfig>,
	context: Option<C>,
	reporter: &mut dyn Reporter,
) -> Result<()>
where
	C: Serialize,
{
	let mut request_config_map = HashMap::new();

	for (idx, request_config) in requests.into_iter().enumerate() {
		// TODO probably make sure name is not something silly like "" or " "
		let name = request_config
			.name
			.clone()
			.unwrap_or_else(|| format!("{}", idx));
		request_config_map.insert(name, State::Wait(Arc::new(request_config)));
	}

	reporter.start();

	let requests = Requests {
		context: Arc::new(Mutex::new(TemplateContext {
			user: context,
			env: env::vars().collect(),
			requests: HashMap::new(),
		})),
		config: Arc::new(config),
		states: request_config_map,
	};
	let results = requests.await?;

	for result in results {
		match result {
			Ok((request_config, stat_result)) => {
				reporter.step_suite(&request_config);
				let result = serde_yaml::to_value(&*stat_result)?;

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
