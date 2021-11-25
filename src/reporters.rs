use anyhow::Result;
use std::io::{self, Write};

use crate::assertions::AssertionResult;
use crate::RequestConfig;

pub trait Reporter {
	fn start(&mut self) -> Result<()> {
		Ok(())
	}

	fn step_suite(&mut self, _request_config: &RequestConfig) -> Result<()> {
		Ok(())
	}

	fn step_result(&mut self, _result: AssertionResult) -> Result<()> {
		Ok(())
	}

	fn end(&mut self) -> Result<()> {
		Ok(())
	}
}

#[derive(Debug, Clone, Default)]
pub struct NoopReporter;

impl Reporter for NoopReporter {}

pub struct SimpleReporter {
	writer: Box<dyn Write>,
}

impl Default for SimpleReporter {
	fn default() -> Self {
		Self {
			writer: Box::new(io::stdout()),
		}
	}
}

impl Reporter for SimpleReporter {
	fn step_suite(&mut self, request_config: &RequestConfig) -> Result<()> {
		if let Some(ref name) = request_config.name {
			writeln!(self.writer, "{}", name)?;
		} else {
			writeln!(
				self.writer,
				"{} {}",
				request_config.request_method, request_config.url
			)?;
		}

		Ok(())
	}

	fn step_result(&mut self, result: AssertionResult) -> Result<()> {
		match result {
			AssertionResult::Skip(assertion, reason) => {
				writeln!(self.writer, "  🟰 {}", assertion)?;
				writeln!(self.writer, "    skipped: {} ", reason)?;
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
					writeln!(self.writer, "    found:")?;
					for line in lines.iter() {
						writeln!(self.writer, "      {}", line)?;
					}
				} else {
					writeln!(self.writer, "    found: {}", lines[0])?;
				}
			}
			AssertionResult::FailureOther(Some(assertion), message) => {
				writeln!(self.writer, "  ✖ {}", assertion)?;
				writeln!(self.writer, "    message: {}", message)?;
			}
			AssertionResult::FailureOther(None, message) => {
				writeln!(self.writer, "  ✖ {}", message)?;
			}
		}

		Ok(())
	}
}

pub struct TapReporter {
	assertion_count: usize,
}

impl Default for TapReporter {
	fn default() -> Self {
		Self { assertion_count: 0 }
	}
}

impl Reporter for TapReporter {
	fn start(&mut self) -> Result<()> {
		println!("TAP version 13");

		Ok(())
	}

	fn step_suite(&mut self, request_config: &RequestConfig) -> Result<()> {
		if let Some(ref name) = request_config.name {
			println!("#\n# {}\n#", name);
		} else {
			println!(
				"#\n# {} {}\n#",
				request_config.request_method, request_config.url
			);
		}

		Ok(())
	}

	fn step_result(&mut self, result: AssertionResult) -> Result<()> {
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

		Ok(())
	}

	fn end(&mut self) -> Result<()> {
		println!("1..{}", self.assertion_count);

		Ok(())
	}
}
