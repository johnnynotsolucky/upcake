use eyre::Result;
use httpstat::{httpstat, Config as HttpstatConfig, StatResult};
use serde::{Deserialize, Serialize};
use serde_yaml::Value;
use std::fs;
use std::time::Duration;
use structopt::StructOpt;

#[derive(Deserialize, Serialize, Default, Debug, Clone)]
pub struct Config {
    pub request: String,
    pub data: Option<String>,
    pub headers: Option<Vec<String>>,
    pub url: String,
    pub assertions: Option<Value>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct RequestAssertion<T> {
    #[serde(flatten)]
    assertion: T,
}

trait Assert {
    fn assert(&self, result: &Value, path: String, callback: AssertCallback) -> Result<()>;
}

#[derive(Deserialize, Serialize, Default, Debug, Clone)]
pub struct WithinRange {
    pub min: Value,
    pub max: Value,
}

fn get_field_value(value: &Value, field: String) -> Option<Value> {
    let mut result = value.clone();
    let path = field.split('.');
    for section in path {
        if let Some(nested) = result.get(section) {
            result = nested.clone();
        } else {
            return None;
        }
    }

    Some(result)
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "null".into(),
        Value::Bool(inner) => format!("{}", inner),
        Value::Number(inner) => {
            if inner.is_i64() {
                format!("{}", inner.as_i64().unwrap())
            } else {
                format!("{}", inner.as_f64().unwrap())
            }
        }
        Value::String(inner) => inner.into(),
        Value::Sequence(inner) => serde_yaml::to_string(inner).unwrap(),
        Value::Mapping(inner) => serde_yaml::to_string(inner).unwrap(),
    }
}

impl Assert for RequestAssertion<WithinRange> {
    fn assert(&self, value: &Value, path: String, callback: AssertCallback) -> Result<()> {
        let min = &self.assertion.min;
        let max = &self.assertion.max;
        if *value < *min || *value > *max {
            let message = format!(
                "{} not within range of [{}, {}]",
                value_to_string(value),
                value_to_string(min),
                value_to_string(max)
            );

            callback(AssertionResult::Failure(path, message));
        } else {
            callback(AssertionResult::Success);
        }

        Ok(())
    }
}

#[derive(Deserialize, Serialize, Default, Debug, Clone)]
pub struct Equal {
    pub value: Value,
}

impl Assert for RequestAssertion<Equal> {
    fn assert(&self, value: &Value, path: String, callback: AssertCallback) -> Result<()> {
        let expected_value = &self.assertion.value;
        if *value != *expected_value {
            let message = format!(
                "Expected {} to equal {}, found {}",
                path,
                value_to_string(expected_value),
                value_to_string(value)
            );

            callback(AssertionResult::Failure(path, message));
        } else {
            callback(AssertionResult::Success);
        }

        Ok(())
    }
}

#[derive(Deserialize, Serialize, Default, Debug, Clone)]
pub struct NotEqual {
    pub value: Value,
}

impl Assert for RequestAssertion<NotEqual> {
    fn assert(&self, value: &Value, path: String, callback: AssertCallback) -> Result<()> {
        let expected_value = &self.assertion.value;
        if *value == *expected_value {
            let message = format!("Expected {} to not equal {}", path, value_to_string(value));

            callback(AssertionResult::Failure(path, message));
        } else {
            callback(AssertionResult::Success);
        }

        Ok(())
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
#[serde(tag = "__type")]
pub enum Assertion {
    #[serde(rename = "within-range")]
    WithinRange(RequestAssertion<WithinRange>),
    #[serde(rename = "equal")]
    Equal(RequestAssertion<Equal>),
    #[serde(rename = "not-equal")]
    NotEqual(RequestAssertion<NotEqual>),
}

type AssertCallback = fn(AssertionResult);

#[derive(Debug, Clone)]
pub enum AssertionResult {
    Success,
    Failure(String, String),
}

impl Assertion {
    fn assert(&self, result: &StatResult, path: String, callback: AssertCallback) -> Result<()> {
        let result_value = serde_yaml::to_value(result)?;
        let field_value = get_field_value(&result_value, path.clone());
        if path.is_empty() || field_value.is_some() {
            let value = match field_value {
                Some(value) => value,
                None => result_value,
            };

            match &self {
                Self::WithinRange(assertion) => assertion.assert(&value, path, callback),
                Self::Equal(assertion) => assertion.assert(&value, path, callback),
                Self::NotEqual(assertion) => assertion.assert(&value, path, callback),
            }
        } else {
            callback(AssertionResult::Failure(path, "Invalid path".into()));
            Ok(())
        }
    }
}

#[derive(Debug, Clone, StructOpt)]
#[structopt()]
struct Opt {
    #[structopt(short = "L", long = "location")]
    /// Follow redirects
    location: bool,

    #[structopt(name = "millis", long = "connect-timeout")]
    /// Maximum time allowed for connection
    connect_timeout: Option<u64>,

    #[structopt(short = "k", long = "insecure")]
    /// Allow insecure server connections when using SSL
    insecure: bool,

    #[structopt(short = "v", long = "verbose")]
    /// Verbose output
    verbose: bool,

    /// Path to the upcakefile
    upcakefile: String,
}

fn traverse(
    value: Value,
    path: String,
    current_idx: String,
    in_sequence: bool,
    assertions_map: &mut Vec<(String, Assertion)>,
) -> Result<()> {
    let path_parts = vec![path.clone(), current_idx];
    let path_iter = path_parts
        .iter()
        .filter(|s| !s.is_empty())
        .into_iter()
        .cloned();
    let next_path = path_iter.collect::<Vec<String>>().join(".");

    let value_result: Result<Assertion, serde_yaml::Error> = serde_yaml::from_value(value.clone());

    // 1. First check whether the Value is an instance of Assertion
    match value_result {
        Ok(assertion) => {
            // If we're currently iterating over a sequence, we don't want to add the index for
            // each assertion instance to the path.
            if in_sequence {
                assertions_map.push((path, assertion));
            } else {
                assertions_map.push((next_path, assertion));
            }
        }
        _ => {
            // 2. Otherwise if it is a sequence or mapping, recursively iterate over its entries
            match value {
                Value::Mapping(v) => {
                    for (k, v1) in v.iter() {
                        // Currently only bothered with string keys
                        if let Some(idx) = k.as_str() {
                            traverse(
                                v1.clone(),
                                next_path.clone(),
                                idx.into(),
                                false,
                                assertions_map,
                            )?;
                        }
                    }
                }
                Value::Sequence(v) => {
                    for (idx, v1) in v.iter().enumerate() {
                        traverse(
                            v1.clone(),
                            next_path.clone(),
                            format!("{}", idx),
                            true,
                            assertions_map,
                        )?;
                    }
                }
                _ => { /* We don't care for this */ }
            }
        }
    }

    Ok(())
}

fn main() -> Result<()> {
    let opt = Opt::from_args();

    // let upcake_config: Config = serde_yaml::from_str(&fs::read_to_string(opt.upcakefile)?)?;
    let upcake_config: Config = serde_yaml::from_str(&fs::read_to_string(opt.upcakefile)?)?;

    let httpstat_config = HttpstatConfig {
        location: opt.location,
        connect_timeout: opt.connect_timeout.map(Duration::from_millis),
        insecure: opt.insecure,
        verbose: opt.verbose,

        request: upcake_config.request,
        data: upcake_config.data,
        headers: upcake_config.headers,
        url: upcake_config.url,
    };

    let stat_result = httpstat(httpstat_config)?;

    if let Some(assertion) = upcake_config.assertions {
        let mut assertions: Vec<(String, Assertion)> = Vec::new();
        traverse(
            assertion.clone(),
            "".into(),
            "".into(),
            assertion.is_sequence(),
            &mut assertions,
        )?;

        for (path, assertion) in assertions.iter() {
            assertion.assert(&stat_result, path.clone(), |result| {
                println!("Result: {:?}", result)
            })?;
        }
    } else {
        // Can't assert!
    }

    Ok(())
}
