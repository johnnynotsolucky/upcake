use anyhow::Result;
use serde::Serialize;
use std::io::{self, Write};

use crate::assertions::AssertionResult;
use crate::{RequestConfigMap, RequestKey, State};

use super::{Event, Observer, RequestState};

pub struct WriteObserver {
	writer: Box<dyn Write>,
	request_map: RequestConfigMap,
}

impl Default for WriteObserver {
	fn default() -> Self {
		Self {
			writer: Box::new(io::stdout()),
			request_map: Default::default(),
		}
	}
}

impl WriteObserver {
	fn write_request_data(&mut self, key: &RequestKey) -> Result<()> {
		if let Some(request) = self.request_map.get(key) {
			if let Some(ref name) = request.name {
				writeln!(self.writer, "{}", name)?;
			} else {
				writeln!(self.writer, "{} {}", request.request_method, request.url)?;
			}
		}

		Ok(())
	}
}

impl<C> Observer<C> for WriteObserver
where
	C: Serialize + Clone,
{
	fn setup(&mut self, state: &State<C>) {
		self.request_map = state.request_map.clone();
	}

	fn on_notify(&mut self, key: &RequestKey, event: Event) -> Result<()> {
		match event {
			Event::AssertionResultAdded(result) => {
				self.write_request_data(key)?;
				match result {
					AssertionResult::Skip(assertion, reason) => {
						writeln!(self.writer, "  🟰 {}", assertion)?;
						writeln!(self.writer, "	skipped: {} ", reason)?;
					}
					AssertionResult::Success(assertion, _value) => {
						writeln!(self.writer, "  ✔ {}", assertion)?;
					}
					AssertionResult::Failure(assertion, value) => {
						writeln!(self.writer, "  ✖ {}", assertion)?;

						let message = serde_yaml::to_string(&value).unwrap();

						// Skip 1 to remove serde_yamls ---
						let lines: Vec<&str> = message.lines().skip(1).collect();

						if lines.len() > 1 {
							writeln!(self.writer, "	found:")?;
							for line in lines.iter() {
								writeln!(self.writer, "	  {}", line)?;
							}
						} else {
							writeln!(self.writer, "	found: {}", lines[0])?;
						}
					}
					AssertionResult::FailureOther(Some(assertion), message) => {
						writeln!(self.writer, "  ✖ {}", assertion)?;
						writeln!(self.writer, "	message: {}", message)?;
					}
					AssertionResult::FailureOther(None, message) => {
						writeln!(self.writer, "  ✖ {}", message)?;
					}
				}
			}
			Event::RequestStateChanged(request_state) => {
				if let RequestState::Error(error) = request_state {
					self.write_request_data(key)?;
					writeln!(self.writer, "  ✖ {}", error)?;
				}
			}
		}

		Ok(())
	}
}
