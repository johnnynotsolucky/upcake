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
		if let Some(summary) = &request_config.summary {
			println!("#\n# {}\n#", summary);
		} else {
			println!("#\n# {} {}\n#", request_config.request, request_config.url);
		}
	}

	fn step_result(&mut self, result: AssertionResult) {
		self.assertion_count += 1;

		match result {
			AssertionResult::Skip(assertion, reason) => {
				println!("ok {} - {} # {}", self.assertion_count, assertion, reason);
			}
			AssertionResult::Success(assertion, _value) => {
				println!("ok {} - {}", self.assertion_count, assertion);
			}
			AssertionResult::Failure(assertion, value, message) => {
				println!("not ok {} - {}", self.assertion_count, assertion);

				println!("  ---");
				println!("  assertion: {}", assertion);

				if let Some(message) = message {
					println!("  message: {}\n", message);
				} else if let Some(value) = value {
					// Maybe its fine to unwrap by the time the program has reached this point?
					let message = serde_yaml::to_string(&value).unwrap();

					// Skip 1 to remove serde_yamls ---
					let lines: Vec<&str> = message.lines().skip(1).collect();

					if lines.len() > 1 {
						println!("  result:");
						for line in lines.iter() {
							println!("   {}", line);
						}
					} else {
						println!("  result: {}", lines[0]);
					}
				}

				println!("  ---");
			}
		}
	}

	fn bail(&mut self, reason: String) {
		self.bailed = true;
		println!("Bail out! {}", reason);
	}

	fn end(&mut self) {
		println!("1..{}", self.assertion_count);
	}
}
