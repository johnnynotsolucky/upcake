use crate::assertions::AssertionResult;
use crate::RequestConfig;

pub trait Reporter {
	fn start(&mut self) {}

	fn step_suite(&mut self, _request_config: &RequestConfig) {}

	fn step_result(&mut self, _result: AssertionResult) {}

	fn end(&mut self) {}
}

#[derive(Debug, Clone, Default)]
pub struct NoopReporter;

impl Reporter for NoopReporter {}

#[derive(Debug, Clone, Default)]
pub struct SimpleReporter;

impl Reporter for SimpleReporter {
	fn step_suite(&mut self, request_config: &RequestConfig) {
		if let Some(ref name) = request_config.name {
			println!("{}", name);
		} else {
			println!("{} {}", request_config.request_method, request_config.url);
		}
	}

	fn step_result(&mut self, result: AssertionResult) {
		match result {
			AssertionResult::Skip(assertion, reason) => {
				println!("  🟰 {}", assertion);
				println!("    skipped: {} ", reason);
			}
			AssertionResult::Success(assertion, _value) => {
				println!("  ✔ {}", assertion);
			}
			AssertionResult::Failure(assertion, value) => {
				println!("  ✖ {}", assertion);

				let message = serde_yaml::to_string(&value).unwrap();

				// Skip 1 to remove serde_yamls ---
				let lines: Vec<&str> = message.lines().skip(1).collect();

				if lines.len() > 1 {
					println!("    found:");
					for line in lines.iter() {
						println!("      {}", line);
					}
				} else {
					println!("    found: {}", lines[0]);
				}
			}
			AssertionResult::FailureOther(Some(assertion), message) => {
				println!("  ✖ {}", assertion);
				println!("    message: {}", message);
			}
			AssertionResult::FailureOther(None, message) => {
				println!("  ✖ {}", message);
			}
		}
	}
}

#[derive(Debug, Clone, Default)]
pub struct TapReporter {
	assertion_count: usize,
}

impl TapReporter {
	pub fn new() -> Self {
		Self { assertion_count: 0 }
	}
}

impl Reporter for TapReporter {
	fn start(&mut self) {
		println!("TAP version 13");
	}

	fn step_suite(&mut self, request_config: &RequestConfig) {
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
			AssertionResult::FailureOther(Some(assertion), message) => {
				println!("not ok {} - {}", self.assertion_count, assertion);
				println!("  ---");
				println!("  assertion: {}", assertion);
				println!("  message: {}\n", message);
				println!("  ---");
			}
			AssertionResult::FailureOther(None, message) => {
				println!("not ok {} - {}", self.assertion_count, message);
			}
		}
	}

	fn end(&mut self) {
		println!("1..{}", self.assertion_count);
	}
}
