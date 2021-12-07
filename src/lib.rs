pub mod assertions;
pub mod observer;

use anyhow::Result;
use handlebars::Handlebars;
use httpstat::{httpstat, Config as HttpstatConfig};
use httpstat::{Header, StatResult as HttpstatResult, Timing as HttpstatTiming};
use serde::de::value::SeqAccessDeserializer;
use serde::de::{Deserializer, IntoDeserializer, SeqAccess, Visitor};
use serde::ser::{SerializeMap, Serializer};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Formatter};
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll};
use std::time::Duration;
use std::{env, fs, str};
use thiserror::Error;
use url::Url;

use assertions::{AssertionConfig, AssertionResult, Equal, RequestAssertionConfig};

use observer::{Event, Observer, RequestState as ObserverRequestState};

/// Re-export [`serde_yaml::Value`].
pub use serde_yaml::Value;

mod handlebars_helpers {
	use handlebars::handlebars_helper;

	handlebars_helper!(eqi: |x: String, y: String| x.to_lowercase() == y.to_lowercase());
	handlebars_helper!(nei: |x: String, y: String| x.to_lowercase() != y.to_lowercase());
}

#[derive(Debug, Clone)]
pub enum HeaderValue {
	/// List of headers to send with the request
	///
	/// Header values are rendered with [`mod@handlebars`].
	List(Vec<Header>),
	/// Render raw headers from a template
	///
	/// Template is rendered with [`mod@handlebars`].
	Template(String),
}

impl<'de> Deserialize<'de> for HeaderValue {
	fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
	where
		D: Deserializer<'de>,
	{
		struct HeaderValueVisitor;

		impl<'de> Visitor<'de> for HeaderValueVisitor {
			type Value = HeaderValue;

			fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
				formatter.write_str("Header template or list of headers")
			}

			fn visit_str<E>(self, value: &str) -> Result<HeaderValue, E>
			where
				E: serde::de::Error,
			{
				let template: String = Deserialize::deserialize(value.into_deserializer())?;
				Ok(HeaderValue::Template(template))
			}

			fn visit_seq<V>(self, seq: V) -> Result<HeaderValue, V::Error>
			where
				V: SeqAccess<'de>,
			{
				let headers: Vec<Header> =
					Deserialize::deserialize(SeqAccessDeserializer::new(seq))?;
				Ok(HeaderValue::List(headers))
			}
		}

		deserializer.deserialize_any(HeaderValueVisitor)
	}
}

/// The representation of the data to send with the request
#[derive(Deserialize, Debug, Clone)]
pub enum RequestData {
	/// A string containing template data
	StringValue(String),
	/// A file to be read and rendered when the request is executed
	FilePath(String),
}

/// Configuration for individual requests
#[derive(Deserialize, Debug, Clone)]
#[serde(default)]
pub struct RequestConfig {
	/// Name of the request
	#[serde(deserialize_with = "ensure_not_empty")]
	pub name: Option<String>,
	/// List of request names which this request requires before it can run
	pub requires: Vec<String>,
	/// Specify the request command to use, i.e. "GET"
	pub request_method: String,
	/// Data to pass with the request
	///
	/// Send the contents of a file by prefixing with an @: `"@/path/to/file.json"`
	///
	/// Contents are rendered with [`mod@handlebars`].
	#[serde(deserialize_with = "request_data")]
	pub data: Option<RequestData>,
	/// Headers to pass to the request
	pub headers: Option<HeaderValue>,
	/// The url to request
	///
	/// The url string is rendered with [`mod@handlebars`].
	pub url: String,
	/// A list of assertions to perform on the response
	pub assertions: Vec<AssertionConfig>,
}

impl Default for RequestConfig {
	fn default() -> Self {
		Self {
			name: Default::default(),
			requires: Default::default(),
			request_method: "GET".into(),
			data: Default::default(),
			headers: Default::default(),
			url: Default::default(),
			assertions: vec![AssertionConfig::Equal(RequestAssertionConfig {
				skip: None,
				path: Some(".\"response_code\"".into()),
				assertion: Equal::new(200),
			})],
		}
	}
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

/// Set the field to a FilePath if the value is prefixed with @, otherwise use StringValue
pub(crate) fn request_data<'de, D>(deserializer: D) -> Result<Option<RequestData>, D::Error>
where
	D: Deserializer<'de>,
{
	let result: Result<Option<String>, _> = Option::deserialize(deserializer);

	if let Ok(Some(value)) = result {
		if let Some(path) = value.strip_prefix('@') {
			return Ok(Some(RequestData::FilePath(path.into())));
		} else {
			return Ok(Some(RequestData::StringValue(value)));
		}
	}

	Ok(None)
}

/// Configuration applied to all requests
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
	/// An optional prefix to filter environment variables injected into the template context
	pub env_var_prefix: Option<String>,
	/// Follow redirects
	pub location: bool,
	/// Allow insecure server connections when using SSL
	pub insecure: bool,
	/// Client certificate file
	pub client_cert: Option<String>,
	/// Private key file
	pub client_key: Option<String>,
	/// CA certificate to verify against
	pub ca_cert: Option<String>,
	/// Maximum time allowed for connection
	pub connect_timeout: Option<u64>,
	/// Verbose output
	pub verbose: bool,
	/// Fail on HTTP error response codes
	pub fail_request: bool,
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
			namelookup: timing.namelookup.as_millis() as u64,
			connect: timing.connect.as_millis() as u64,
			pretransfer: timing.pretransfer.as_millis() as u64,
			starttransfer: timing.starttransfer.as_millis() as u64,
			total: timing.total.as_millis() as u64,
			dns_resolution: timing.dns_resolution.as_millis() as u64,
			tcp_connection: timing.tcp_connection.as_millis() as u64,
			tls_connection: timing.tls_connection.as_millis() as u64,
			server_processing: timing.server_processing.as_millis() as u64,
			content_transfer: timing.content_transfer.as_millis() as u64,
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
	inner: StatResult,
}

impl From<StatResult> for ResponseData {
	fn from(result: StatResult) -> Self {
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
enum RequestError {
	/// Request failed
	#[error("Request failed: {0:?}")]
	RequestError(String),
	/// Dependencies for a request failed, possibly with [`RequestError::RequestError`]
	#[error("Request dependencies failed: {0:?}")]
	DependencyError(String),
}

enum FutureState<F>
where
	F: Future<Output = RequestResult>,
{
	/// Request hasn't started yet
	Wait,
	/// The request future for running requests
	Future(F),
	/// Request has completed
	Done(Box<RequestResult>),
}

/// An enum used for identifying requests in a map
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum RequestKey {
	Name(String),
	Idx(usize),
}

type RequestStateMap = HashMap<RequestKey, RequestState>;
type RequestConfigMap = HashMap<RequestKey, RequestConfig>;
type RequestResult = Result<StatResult>;
type RequestState = FutureState<Pin<Box<dyn Future<Output = RequestResult>>>>;

struct RequestsFuture {
	poll_fn: Box<dyn FnMut(&mut Context<'_>) -> Poll<Result<()>>>,
}

impl Future for RequestsFuture {
	type Output = Result<()>;

	fn poll(self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
		(self.get_mut().poll_fn)(context)
	}
}

#[derive(Debug)]
enum RequirementState {
	Wait,
	Success,
	Error,
}

impl<F> From<&FutureState<F>> for RequirementState
where
	F: Future<Output = RequestResult>,
{
	fn from(state: &FutureState<F>) -> Self {
		match state {
			FutureState::Done(result) => match **result {
				Ok(_) => Self::Success,
				Err(_) => Self::Error,
			},
			_ => Self::Wait,
		}
	}
}

/// Iterate over request states and check whether each waiting state should be marked as running.
///
/// Returns a list of RequestKey's for requests which were moved into Done(Err) state.
fn mark_ready_states<C>(internal_state: Arc<Mutex<State<C>>>) -> Vec<RequestKey>
where
	C: Serialize + Clone + 'static,
{
	let internal_state = &mut *internal_state.lock().unwrap();

	// Create sets of errored and successful requests to match against request dependencies
	// when checking whether a request can be started.
	//
	// Create a hashmap of all requirement states so that waiting requests can be validated.
	// The map uses owned values for the keys because when the code that references this is inside
	// an exclusive reference. So if this data was references to data in `state`, it would not
	// compile.
	//
	// Only named requests are included.
	let all_map: HashMap<String, RequirementState> = internal_state
		.request_states
		.iter()
		.filter_map(|(ref request_key, state)| match request_key {
			RequestKey::Name(key) => Some((key.clone(), RequirementState::from(state))),
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

	let mut dependency_error_keys = Vec::new();

	for (request_key, state) in internal_state.request_states.iter_mut() {
		if let FutureState::Wait = state {
			let request_config = internal_state.request_map.get(request_key).unwrap();
			let requirements_validated = if !request_config.requires.is_empty() {
				// If the request has requirements, ensure that they all exist otherwise it will
				// never leave the Wait state.
				let exists = request_config
					.requires
					.iter()
					.all(|r| all_map.keys().any(|key| r == key));

				// If the request requires itself, it will sit in Wait indefinitely.
				let requires_self = request_config.requires.iter().any(|r| match request_key {
					RequestKey::Name(ref key) => r == key,
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
					*state = FutureState::Done(Box::new(Err(RequestError::DependencyError(
						// Non-exhaustive list of dependencies which failed before attempting to
						// start this request.
						error_intersection
							.into_iter()
							.cloned()
							.collect::<Vec<&str>>()
							.join(", "),
					)
					.into())));
					dependency_error_keys.push(request_key.clone());
				} else if success_intersection.len() == requires_set.len() {
					// This request is ready to start, update this requests state so that it can be
					// polled.
					let future = run_request(
						internal_state.config.clone(),
						request_config.clone(),
						internal_state.template_context.clone(),
					);
					*state = FutureState::Future(Box::pin(future));
				}
			} else {
				*state = FutureState::Done(Box::new(Err(RequestError::DependencyError(
					"Invalid requirements".into(),
				)
				.into())));
				dependency_error_keys.push(request_key.clone());
			}
		}
	}

	dependency_error_keys
}

/// Iterate over running futures and update their states if they're done.
///
/// Returns true if all states are done
fn poll_states<C>(
	internal_state: Arc<Mutex<State<C>>>,
	task_context: &mut Context<'_>,
) -> (bool, Vec<RequestKey>)
where
	C: Serialize + Clone,
{
	let mut new_done_keys: Vec<RequestKey> = Vec::new();

	let mut all_ready = true;

	let internal_state = &mut *internal_state.lock().unwrap();
	for (request_key, state) in internal_state.request_states.iter_mut() {
		match state {
			FutureState::Future(future) => {
				// This future can be polled
				match Pin::new(future).poll(task_context) {
					// If it is Ready, mark it's state as Done
					Poll::Ready(result) => {
						let result = result;

						// If there is an Ok result from the request future, add that to the
						// TemplateContext under the name of the request.
						if let Ok(ref stat_result) = result {
							// Only named requests get added to the context
							if let RequestKey::Name(name) = request_key {
								let template_context = &mut internal_state.template_context;
								template_context
									.requests
									.insert(name.clone(), ResponseData::from(stat_result.clone()));
							}
						}

						*state = FutureState::Done(Box::new(result));

						new_done_keys.push(request_key.clone());
					}
					Poll::Pending => {
						// If the request has started, but is still pending, mark the future
						// as incomplete.
						all_ready = false;
						continue;
					}
				}
			}
			FutureState::Wait => {
				// If the request is still waiting to start, mark the future as incomplete.
				all_ready = false;
				continue;
			}
			FutureState::Done(_) => continue, // Ignore Done, there's no further work needed.
		}
	}

	(all_ready, new_done_keys)
}

/// Run the request
///
/// Handles set up of request configuration to call [`mod@httpstat()`] with and handle rendering of templated request
/// data.
async fn run_request<C>(
	config: Config,
	request_config: RequestConfig,
	template_context: TemplateContext<C>,
) -> RequestResult
where
	C: Serialize + Clone,
{
	// TODO move Handlebars setup out of run_request
	let mut handlebars = Handlebars::new();
	handlebars.register_helper("eqi", Box::new(handlebars_helpers::eqi));
	handlebars.register_helper("nei", Box::new(handlebars_helpers::nei));

	let template_context = template_context;
	// Render header values with the template context
	let headers = match request_config.headers {
		Some(HeaderValue::List(ref headers)) => {
			// Some(ref headers) => {
			let mut rendered_headers = Vec::new();

			for header in headers {
				rendered_headers.push(Header {
					name: header.name.clone(),
					value: handlebars.render_template(&header.value, &template_context)?,
				});
			}

			rendered_headers
		}
		Some(HeaderValue::Template(ref template)) => {
			let rendered_template = handlebars.render_template(template, &template_context)?;

			let mut rendered_headers = Vec::new();
			for line in rendered_template.lines() {
				if let Some((name, value)) = line.split_once(':') {
					rendered_headers.push(Header {
						name: name.into(),
						value: value.trim_start().into(),
					});
				}
			}

			rendered_headers
		}
		None => Vec::new(),
	};

	let url = Url::parse(&handlebars.render_template(&request_config.url, &template_context)?)?;

	let data = match request_config.data {
		Some(RequestData::StringValue(ref data)) => {
			Some(handlebars.render_template(data, &template_context)?)
		}
		Some(RequestData::FilePath(ref path)) => {
			Some(handlebars.render_template(&fs::read_to_string(path)?, &template_context)?)
		}
		None => None,
	};

	drop(template_context);

	let fail_request = config.fail_request;

	let httpstat_config = HttpstatConfig {
		location: config.location,
		insecure: config.insecure,
		client_cert: config.client_cert.clone(),
		client_key: config.client_key.clone(),
		ca_cert: config.ca_cert.clone(),
		verbose: config.verbose,
		connect_timeout: config.connect_timeout.map(Duration::from_millis),
		max_response_size: config.max_response_size,

		request_method: request_config.request_method.clone().into(),
		url: url.into(),
		data,
		headers,
	};

	match httpstat(&httpstat_config).await {
		Ok(httpstat_result) => {
			if fail_request && (400..600).contains(&httpstat_result.response_code) {
				Err(
					RequestError::RequestError(format!("HTTP {}", httpstat_result.response_code))
						.into(),
				)
			} else {
				Ok(httpstat_result.into())
			}
		}
		Err(error) => Err(RequestError::RequestError(error.to_string()).into()),
	}
}

/// Context applied to request data such as headers, urls, content body.
#[derive(Serialize, Debug, Clone)]
struct TemplateContext<C>
where
	C: Serialize + Clone,
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

/// Final results of all requests and assertions.
pub enum UpcakeResult {
	/// Variant returned when all requests were successful and their assertions passed.
	Success,
	/// Count of failed requests and/or assertion failures.
	Failures(usize),
}

pub struct State<C>
where
	C: Serialize + Clone,
{
	config: Config,
	request_states: RequestStateMap,
	request_map: RequestConfigMap,
	template_context: TemplateContext<C>,
	failure_count: usize,
}

/// TODO Add docs please
///
/// Returns
pub async fn upcake<C>(
	mut config: Config,
	context: Option<C>,
	mut observer: Box<dyn Observer<C>>,
) -> Result<UpcakeResult>
where
	C: Serialize + Clone + 'static,
{
	let mut request_states: RequestStateMap = HashMap::new();
	let mut request_map: RequestConfigMap = HashMap::new();

	let requests = std::mem::take(&mut config.requests);
	// Move requests into a map with the request names as keys, or if the name is not set, the
	// string representation of the request's index in the iterator.
	for (idx, request_config) in requests.into_iter().enumerate() {
		let request_key = match request_config.name {
			Some(ref key) => RequestKey::Name(key.clone()),
			None => RequestKey::Idx(idx),
		};
		request_states.insert(request_key.clone(), FutureState::Wait);
		request_map.insert(request_key, request_config);
	}

	let envvars = env::vars()
		.filter(|(key, _value)| match config.env_var_prefix {
			Some(ref env_var_prefix) => key.starts_with(env_var_prefix),
			None => true,
		})
		.collect();

	let template_context = TemplateContext {
		user: context,
		env: envvars,
		requests: Default::default(),
	};

	let internal_state = Arc::new(Mutex::new(State {
		config,
		request_states,
		request_map,
		template_context,
		failure_count: 0,
	}));

	observer.setup(&*internal_state.lock().unwrap());

	// Get a new reference to internal_state so that it can be used inside poll.
	let poll_state = internal_state.clone();
	let poll = move |context: &mut Context<'_>| -> Poll<Result<()>> {
		// Mark states which are ready to be polled.
		let dependency_error_keys = mark_ready_states(poll_state.clone());
		if !dependency_error_keys.is_empty() {
			let internal_state = &mut *poll_state.lock().unwrap();
			for key in dependency_error_keys {
				if let Some(FutureState::Done(result)) = internal_state.request_states.get(&key) {
					if let Err(ref error) = **result {
						internal_state.failure_count += 1;
						observer.on_notify(
							&key,
							Event::AssertionResultAdded(&AssertionResult::FailureOther(
								None,
								error.to_string(),
							)),
						)?;
					}
				} else {
					unreachable!();
				}
			}
		}

		// When all the states are the Ready variant, the results can be mapped to a list
		let (all_ready, done_keys) = poll_states(poll_state.clone(), context);

		// If some requests have completed, run assertions against their results
		if !done_keys.is_empty() {
			let internal_state = &mut *poll_state.lock().unwrap();

			for key in done_keys {
				// If this key actually exists.
				if let Some(FutureState::Done(result)) = internal_state.request_states.get(&key) {
					match **result {
						Ok(ref stat_result) => {
							observer.on_notify(
								&key,
								Event::RequestStateChanged(ObserverRequestState::Success(
									stat_result,
								)),
							)?;
							let request_config = internal_state.request_map.get(&key).unwrap();
							let result = serde_yaml::to_value(&stat_result)?;

							for assertion in request_config.assertions.iter() {
								let assertion_result = assertion.assert(&result)?;
								observer.on_notify(
									&key,
									Event::AssertionResultAdded(&assertion_result),
								)?;

								// If there is any type of failure, increment the failure_count.
								match assertion_result {
									AssertionResult::Failure(_, _)
									| AssertionResult::FailureOther(_, _) => {
										internal_state.failure_count += 1;
									}
									_ => {
										// Do nothing.
									}
								}
							}
						}
						Err(ref error) => {
							// Request errors count towards failure counts.
							internal_state.failure_count += 1;
							match error.downcast_ref::<RequestError>() {
								Some(error) => {
									observer.on_notify(
										&key,
										Event::AssertionResultAdded(
											&AssertionResult::FailureOther(None, error.to_string()),
										),
									)?;
								}
								None => {
									observer.on_notify(
										&key,
										Event::RequestStateChanged(ObserverRequestState::Error(
											error,
										)),
									)?;
								}
							};
						}
					}
				} else {
					unreachable!();
				}
			}
		}

		if all_ready {
			Poll::Ready(Ok(()))
		} else {
			context.waker().wake_by_ref();
			Poll::Pending
		}
	};

	RequestsFuture {
		poll_fn: Box::new(poll),
	}
	.await?;

	let internal_state = &*internal_state.lock().unwrap();
	if internal_state.failure_count == 0 {
		Ok(UpcakeResult::Success)
	} else {
		Ok(UpcakeResult::Failures(internal_state.failure_count))
	}
}
