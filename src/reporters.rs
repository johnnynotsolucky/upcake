use crate::assertions::AssertionResult;
use crate::RequestConfig;

pub trait Reporter {
	fn start(&mut self);
	fn step_suite(&mut self, request_config: &RequestConfig);
	fn step_result(&mut self, result: AssertionResult);
	fn bail(&mut self, reason: String);
	fn end(&mut self);
}

#[derive(Debug, Clone, Default)]
pub struct NoopReporter;

impl Reporter for NoopReporter {
	fn start(&mut self) {}

	fn step_suite(&mut self, _request_config: &RequestConfig) {}

	fn step_result(&mut self, _result: AssertionResult) {}

	fn bail(&mut self, _reason: String) {}

	fn end(&mut self) {}
}

#[derive(Debug, Clone, Default)]
pub struct TapReporter {
	assertion_count: usize,
	bailed: bool,
}

impl TapReporter {
	pub fn new() -> Self {
		Self {
			assertion_count: 0,
			bailed: false,
		}
	}
}

impl Reporter for TapReporter {
	fn start(&mut self) {
		println!("TAP version 13");
	}

	fn step_suite(&mut self, request_config: &RequestConfig) {
		if self.bailed {
			return;
		}

		if let Some(ref name) = request_config.name {
			println!("#\n# {}\n#", name);
		} else {
			println!(
				"#\n# {} {}\n#",
				request_config.request_method, request_config.url
			);
		}
	}

	fn step_result(&mut self, result: AssertionResult) {
		if self.bailed {
			return;
		}

		self.assertion_count += 1;

		match result {
			AssertionResult::Skip(assertion, reason) => {
				println!(
					"ok {} - {} # SKIP {}",
					self.assertion_count, assertion, reason
				);
			}
			AssertionResult::Success(assertion, _value) => {
				println!("ok {} - {}", self.assertion_count, assertion);
			}
			AssertionResult::Failure(assertion, value) => {
				println!("not ok {} - {}", self.assertion_count, assertion);

				println!("  ---");
				println!("  assertion: {}", assertion);

				// Maybe its fine to unwrap by the time the program has reached this point?
				let message = serde_yaml::to_string(&value).unwrap();

				// Skip 1 to remove serde_yamls ---
				let lines: Vec<&str> = message.lines().skip(1).collect();

				if lines.len() > 1 {
					println!("  found:");
					for line in lines.iter() {
						println!("   {}", line);
					}
				} else {
					println!("  found: {}", lines[0]);
				}

				println!("  ---");
			}
			AssertionResult::FailureOther(assertion, message) => {
				println!("not ok {} - {}", self.assertion_count, assertion);
				println!("  ---");
				println!("  assertion: {}", assertion);
				println!("  message: {}\n", message);
				println!("  ---");
			}
		}
	}

	fn bail(&mut self, reason: String) {
		self.bailed = true;
		println!("Bail out! {}", reason);
	}

	fn end(&mut self) {
		if self.bailed {
			return;
		}

		println!("1..{}", self.assertion_count);
	}
}
