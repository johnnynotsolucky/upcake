use crate::assertions::{Assertion, AssertionResult};

pub trait Reporter {
    fn plan(&mut self, plan: &Vec<Assertion>);
    fn step_result(&self, idx: usize, result: AssertionResult);
    fn bail(&mut self, reason: String);
    fn end(&mut self);
}

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
    fn plan(&mut self, plan: &Vec<Assertion>) {
        println!("TAP version 13");
        self.assertion_count = plan.len();
    }

    fn step_result(&self, idx: usize, result: AssertionResult) {
        match result {
            AssertionResult::Success(assertion, _value) => {
                println!("ok {} - {}", idx + 1, assertion);
            }
            AssertionResult::Failure(assertion, value, message) => {
                println!("not ok {} - {}", idx + 1, assertion);

                if let Some(message) = message {
                    println!(" ---\n message: {}\n ---", message);
                } else if let Some(value) = value {
                    // Maybe its fine to unwrap by the time the program has reached this point?
                    let message = serde_yaml::to_string(&value).unwrap();

                    println!(" ---");

                    // Skip 1 to remove serde_yamls ---
                    let lines: Vec<&str> = message.lines().skip(1).collect();

                    if lines.len() > 1 {
                        println!(" result:");
                        for line in lines.iter() {
                            println!("   {}", line);
                        }
                    } else {
                        println!(" result: {}", lines[0]);
                    }

                    println!(" ---");
                }
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
