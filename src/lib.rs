pub mod assertions;
pub mod reporters;

use anyhow::Result;
use handlebars::Handlebars;
use httpstat::{httpstat, Config as HttpstatConfig};
use httpstat::{Header, StatResult as HttpstatResult, Timing as HttpstatTiming};
use serde::de::Deserializer;
use serde::ser::{SerializeMap, Serializer};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::future::Future;
use std::pin::Pin;
use std::str;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};
use std::time::Duration;
use thiserror::Error;

use assertions::{AssertionConfig, AssertionResult, Equal, RequestAssertionConfig};
use reporters::Reporter;

/// Re-export [`serde_yaml::Value`].
pub use serde_yaml::Value;

/// Configuration for individual requests
#[derive(Deserialize, Default, Debug, Clone)]
pub struct RequestConfig {
	/// Name of the request
	#[serde(default, deserialize_with = "ensure_not_empty")]
	pub name: Option<String>,
	/// List of request names which this request requires before it can run
	#[serde(default)]
	pub requires: Vec<String>,
	/// Specify the request command to use, i.e. "GET"
	#[serde(default = "default_request_method")]
	pub request_method: String,
	/// Data to pass with the request
	///
	/// Send the contents of a file by prefixing with an @: `"@/path/to/file.json"`
	///
	/// Contents are rendered with [`mod@handlebars`].
	pub data: Option<String>,
	/// Key/Value pairs of headers to send with the request
	///
	/// Header values are rendered with [`mod@handlebars`].
	pub headers: Option<HashMap<String, String>>,
	/// The url to request
	///
	/// The url string is rendered with [`mod@handlebars`].
	pub url: String,
	/// A list of assertions to perform on the response
	#[serde(default = "default_assertions")]
	pub assertions: Vec<AssertionConfig>,
}

/// Ensures empty values or values with only whitespace are set as None
pub(crate) fn ensure_not_empty<'de, D>(deserializer: D) -> Result<Option<String>, D::Error>
where
	D: Deserializer<'de>,
{
	let result: Result<Option<String>, _> = Option::deserialize(deserializer);

	if let Ok(Some(value)) = result {
		if !value.trim().is_empty() {
			return Ok(Some(value));
		}
	}

	Ok(None)
}

pub(crate) fn default_request_method() -> String {
	"GET".into()
}

pub(crate) fn default_assertions() -> Vec<AssertionConfig> {
	vec![AssertionConfig::Equal(RequestAssertionConfig {
		skip: None,
		path: Some(".\"response_code\"".into()),
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
	/// Requests to run
	pub requests: Vec<RequestConfig>,
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

/// Response content variations
#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(untagged)]
pub enum ResponseContent {
	/// No content returned
	NoContent,
	/// JSON content
	Json(JsonValue),
	/// Content which could not be parsed
	Other(String),
}

/// Wraps [`HttpstatResult`] with more useful properties
#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct StatResult {
	/// HTTP version used
	pub http_version: String,
	/// HTTP response code from target host
	pub response_code: i32,
	/// HTTP response message from target host
	pub response_message: Option<String>,
	/// Response headers
	pub headers: Vec<Header>,
	/// Timing data
	pub timing: Timing,
	/// Possibly parsed response content
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
				// Attempt to parse json content
				stat_result.content = ResponseContent::Json(json_content);
			} else {
				// Otherwise set the content as a string
				stat_result.content = ResponseContent::Other(body.into());
			}
		}

		stat_result
	}
}

/// A wrapper for [`StatResult`] which is serialized down to only header and content response data.
///
/// Available in templates through `requests.{name}`
#[derive(Debug, Clone)]
pub struct ResponseData {
	/// Wrapped [`StatResult`]
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
	/// Request failed
	#[error("Request failed: {1:?}")]
	RequestError(Arc<RequestConfig>, String),
	/// Dependencies for a request failed, possibly with [`Error::RequestError`]
	#[error("Request dependencies failed: {1:?}")]
	DependencyError(Arc<RequestConfig>, String),
}

/// Run the request
///
/// Handles set up of request configuration to call [`mod@httpstat()`] with and handle rendering of templated request
/// data.
async fn run_request<C>(
	config: Arc<Config>,
	request_config: Arc<RequestConfig>,
	context: Arc<Mutex<TemplateContext<C>>>,
) -> RequestResult
where
	C: Serialize,
{
	let handlebars = Handlebars::new();

	// Render header values with the template context
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

	let data = if let Some(ref data) = request_config.data {
		// Read in a file for request content if the data property is prefixed with `'@'`,
		// otherwise use whatever is set on [`RequestConfig::data`].
		let data = match data.strip_prefix('@') {
			Some(path) => fs::read_to_string(path)?,
			None => data.clone(),
		};

		Some(handlebars.render_template(&data, &*context.lock().unwrap())?)
	} else {
		None
	};

	let httpstat_config = HttpstatConfig {
		location: config.location,
		insecure: config.insecure,
		verbose: config.verbose,
		connect_timeout: config.connect_timeout.map(Duration::from_millis),
		max_response_size: config.max_response_size,

		request_method: request_config.request_method.clone().into(),
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

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum StateKey {
	Name(String),
	Idx(usize),
}

type RequestResult = Result<(Arc<RequestConfig>, Arc<StatResult>)>;
type RequestState = State<Pin<Box<dyn Future<Output = RequestResult>>>>;

struct RequestsFuture<C>
where
	C: Serialize,
{
	config: Arc<Config>,
	context: Arc<Mutex<TemplateContext<C>>>,
	states: HashMap<StateKey, RequestState>,
}

impl<C> RequestsFuture<C>
where
	C: Serialize,
{
	fn new(mut config: Config, context: Option<C>) -> Self {
		let mut request_config_map = HashMap::new();

		let requests = std::mem::take(&mut config.requests);
		// Move requests into a map with the request names as keys, or if the name is not set, the
		// string representation of the request's index in the iterator.
		for (idx, request_config) in requests.into_iter().enumerate() {
			let state_key = match request_config.name {
				Some(ref key) => StateKey::Name(key.clone()),
				None => StateKey::Idx(idx),
			};
			request_config_map.insert(state_key, State::Wait(Arc::new(request_config)));
		}
		Self {
			// Set up the template context to include the optional user context, environment variables
			// and an empty map of requests.
			context: Arc::new(Mutex::new(TemplateContext {
				user: context,
				env: env::vars().collect(),
				requests: HashMap::new(),
			})),
			config: Arc::new(config),
			states: request_config_map,
		}
	}
}

impl<C> Future for RequestsFuture<C>
where
	C: Serialize + 'static,
{
	type Output = Result<Vec<RequestResult>>;

	fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		let mut this = unsafe { self.get_unchecked_mut() };

		// Mark states which are ready to be polled.
		mark_ready_states(&mut this);

		// When all the states are the Ready variant, the results can be mapped to a list
		if poll_states(&mut this, context) {
			// We don't need states anymore, shadow the variable and map into the result Vec.
			let states = std::mem::take(&mut this.states);
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

enum RequirementState {
	Wait,
	Success,
	Error,
}

impl<F> From<&State<F>> for RequirementState
where
	F: Future<Output = RequestResult>,
{
	fn from(state: &State<F>) -> Self {
		match state {
			// result is a reference to a Box
			State::Done(result) => match **result {
				Ok(_) => Self::Success,
				Err(_) => Self::Error,
			},
			_ => Self::Wait,
		}
	}
}

/// Iterate over request states and check whether each waiting state should be marked as running.
fn mark_ready_states<C>(requests_future: &mut RequestsFuture<C>)
where
	C: Serialize + 'static,
{
	let states = &mut requests_future.states;
	// Create sets of errored and successful requests to match against request dependencies
	// when checking whether a request can be started.
	//
	// Create a hashmap of all requirement states so that waiting requests can be validated.
	// The map uses owned values for the keys because when the code that references this is inside
	// an exclusive reference. So if this data was references to data in `state`, it would not
	// compile.
	//
	// Only named requests are included.
	let all_map: HashMap<String, RequirementState> = states
		.iter()
		.filter_map(|(ref state_key, state)| match state_key {
			StateKey::Name(key) => Some((key.clone(), RequirementState::from(state))),
			_ => None,
		})
		.collect();

	let mut error_set: HashSet<&str> = HashSet::new();
	let mut success_set: HashSet<&str> = HashSet::new();

	for (key, state) in all_map.iter() {
		match state {
			RequirementState::Success => {
				success_set.insert(key);
			}
			RequirementState::Error => {
				error_set.insert(key);
			}
			_ => {}
		};
	}

	for (state_key, state) in states.iter_mut() {
		if let State::Wait(request_config) = state {
			let requirements_validated = if !request_config.requires.is_empty() {
				// If the request has requirements, ensure that they all exist otherwise it will
				// never leave the Wait state.
				let exists = all_map
					.keys()
					.all(|key| request_config.requires.iter().any(|r| key == r));

				// If the request requires itself, it will sit in Wait indefinitely.
				let requires_self = request_config.requires.iter().any(|r| match state_key {
					StateKey::Name(ref key) => r == key,
					_ => false,
				});

				exists && !requires_self
			} else {
				true
			};

			if requirements_validated {
				// Get the intersection of the request dependencies and the requests which have
				// completed successfully. If they intersection set matches the requires set,
				// then this request can be started.
				let requires_set: HashSet<_> =
					request_config.requires.iter().map(|s| s.as_str()).collect();

				let success_intersection: HashSet<_> =
					success_set.intersection(&requires_set).collect();
				let error_intersection: HashSet<_> =
					error_set.intersection(&requires_set).collect();

				if !error_intersection.is_empty() {
					// Update this requests state so that it is Done and skips polling.
					*state = State::Done(Box::new(Err(Error::DependencyError(
						request_config.clone(),
						// Non-exhaustive list of dependencies which failed before attempting to
						// start this request.
						error_intersection
							.into_iter()
							.cloned()
							.collect::<Vec<&str>>()
							.join(", "),
					)
					.into())));
				} else if success_intersection.len() == requires_set.len() {
					// This request is ready to start, update this requests state so that it can be
					// polled.
					*state = State::Future(Box::pin(run_request(
						requests_future.config.clone(),
						request_config.clone(),
						requests_future.context.clone(),
					)));
				}
			} else {
				*state = State::Done(Box::new(Err(Error::DependencyError(
					request_config.clone(),
					"Invalid requirements".into(),
				)
				.into())));
			}
		}
	}
}

/// Iterate over running futures and update their states if they're done.
///
/// Returns true if all states are done
fn poll_states<C>(requests_future: &mut RequestsFuture<C>, context: &mut Context<'_>) -> bool
where
	C: Serialize + 'static,
{
	let states = &mut requests_future.states;
	let mut all_ready = true;

	for (state_key, state) in states.iter_mut() {
		match state {
			State::Future(future) => {
				// This future can be polled
				match Pin::new(future).poll(context) {
					// If it is Ready, mark it's state as Done
					Poll::Ready(result) => {
						let result = Box::new(result);

						// If there is an Ok result from the request future, add that to the
						// TemplateContext under the name of the request.
						if let Ok((_, ref stat_result)) = *result {
							// Only named requests get added to the context
							if let StateKey::Name(name) = state_key {
								let mut context = requests_future.context.lock().unwrap();
								context
									.requests
									.insert(name.clone(), ResponseData::from(stat_result.clone()));
							}
						}

						*state = State::Done(result);
					}
					Poll::Pending => {
						// If the request has started, but is still pending, mark the future
						// as incomplete.
						all_ready = false;
						continue;
					}
				}
			}
			State::Wait(_) => {
				// If the request is still waiting to start, mark the future as incomplete.
				all_ready = false;
				continue;
			}
			State::Done(_) => continue, // Ignore Done, there's no further work needed.
		}
	}

	all_ready
}

/// Context applied to request data such as headers, urls, content body.
#[derive(Serialize, Debug, Clone)]
struct TemplateContext<C>
where
	C: Serialize,
{
	/// Optional user context
	user: Option<C>,
	/// Map of available environment variables
	env: HashMap<String, String>,
	// `requests` is serialized when rendering templates, but we need an [`Arc`] so that we can
	// pass the result around internally.
	/// Map of completed and successful request responses
	requests: HashMap<String, ResponseData>,
}

///
pub async fn upcake<C, R>(config: Config, context: Option<C>, reporter: &mut R) -> Result<()>
where
	C: Serialize + 'static,
	R: Reporter,
{
	reporter.start();

	let results = RequestsFuture::new(config, context).await?;

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
				Some(error) => {
					let request_config = match error {
						Error::RequestError(request_config, _) => request_config,
						Error::DependencyError(request_config, _) => request_config,
					};
					reporter.step_suite(request_config);
					reporter.step_result(AssertionResult::FailureOther(None, error.to_string()));
				}
				None => {
					reporter.step_result(AssertionResult::FailureOther(None, format!("{}", error)));
				}
			},
		}
	}

	reporter.end();

	Ok(())
}
