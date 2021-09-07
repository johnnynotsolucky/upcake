use eyre::Result;
use httpstat::{httpstat, Config as HttpstatConfig, Header, StatResult as HttpstatResult, Timing};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use serde_yaml::Value as YamlValue;
use std::time::Duration;
use std::{fs, str};
use structopt::StructOpt;

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
    pub request: String,
    pub data: Option<String>,
    pub headers: Option<Vec<String>>,
    pub url: String,
    pub assertions: Vec<Assertion>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct RequestAssertion<T> {
    #[serde(default)]
    path: String,
    #[serde(flatten)]
    assertion: T,
}

trait Assert {
    fn assert(&self, result: &YamlValue) -> bool;
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct WithinRange {
    pub min: YamlValue,
    pub max: YamlValue,
}

fn value_to_string(value: &YamlValue) -> String {
    match value {
        YamlValue::Null => "null".into(),
        YamlValue::Bool(inner) => format!("{}", inner),
        YamlValue::Number(inner) => {
            if inner.is_i64() {
                format!("{}", inner.as_i64().unwrap())
            } else {
                format!("{}", inner.as_f64().unwrap())
            }
        }
        YamlValue::String(inner) => inner.into(),
        YamlValue::Sequence(inner) => serde_json::to_string(inner).unwrap(),
        YamlValue::Mapping(inner) => serde_json::to_string(inner).unwrap(),
    }
}

impl Assert for RequestAssertion<WithinRange> {
    fn assert(&self, value: &YamlValue) -> bool {
        *value >= self.assertion.min && *value <= self.assertion.max
        // let message = format!(
        // 	"{} not within range of [{}, {}]",
        // 	value_to_string(value),
        // 	value_to_string(min),
        // 	value_to_string(max)
        // );
        //
        // callback(AssertionResult::Failure(
        // 	Assertion::WithinRange(self.clone()),
        // 	message,
        // ));
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Equal {
    pub value: YamlValue,
}

impl Assert for RequestAssertion<Equal> {
    fn assert(&self, value: &YamlValue) -> bool {
        *value == self.assertion.value
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct Length {
    pub length: Box<Assertion>,
}

impl Assert for RequestAssertion<Length> {
    fn assert(&self, value: &YamlValue) -> bool {
        let length = match value {
            YamlValue::String(value) => Some(value.len()),
            YamlValue::Mapping(value) => Some(value.len()),
            YamlValue::Sequence(value) => Some(value.len()),
            _ => None,
        };

        length.map_or(false, |length| {
            self.assertion.length.inner_assert(&YamlValue::from(length))
        })
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct NotEqual {
    pub value: YamlValue,
}

impl Assert for RequestAssertion<NotEqual> {
    fn assert(&self, value: &YamlValue) -> bool {
        *value != self.assertion.value
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Contains {
    pub value: YamlValue,
}

impl Assert for RequestAssertion<Contains> {
    fn assert(&self, value: &YamlValue) -> bool {
        match value {
            YamlValue::String(value) => self
                .assertion
                .value
                .as_str()
                .map_or(false, |substring| value.contains(substring)),
            YamlValue::Mapping(value) => self
                .assertion
                .value
                .as_str()
                .map_or(false, |key| value.contains_key(&YamlValue::from(key))),
            YamlValue::Sequence(value) => self.assertion.value.as_i64().map_or(false, |idx| {
                // serde_yaml::Value only converts to i64, we'd like to not panic if the assertion
                // uses something less than 0.
                // TODO at somepoint we should validate the inputs
                if idx >= 0 {
                    value.len() >= idx as usize
                } else {
                    false
                }
            }),
            _ => false,
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "__type")]
pub enum Assertion {
    #[serde(rename = "within-range")]
    WithinRange(RequestAssertion<WithinRange>),
    #[serde(rename = "equal")]
    Equal(RequestAssertion<Equal>),
    #[serde(rename = "not-equal")]
    NotEqual(RequestAssertion<NotEqual>),
    #[serde(rename = "length")]
    Length(RequestAssertion<Length>),
    #[serde(rename = "contains")]
    Contains(RequestAssertion<Contains>),
}

type AssertCallback = fn(AssertionResult);

#[derive(Debug, Clone)]
pub enum AssertionResult {
    Success(Assertion, JsonValue),
    Failure(Assertion, Option<JsonValue>, String),
}

impl Assertion {
    fn assert(&self, value: &JsonValue, callback: AssertCallback) -> Result<()> {
        if let Ok(found_value) = jql::walker(value, Some(self.path())) {
            let result = serde_yaml::to_value(&found_value)?;
            let assertion_result = self.inner_assert(&result);

            if assertion_result {
                callback(AssertionResult::Success(self.clone(), found_value));
            } else {
                // TODO implement Display for AssertionResult
                callback(AssertionResult::Failure(
                    self.clone(),
                    Some(found_value),
                    format!("{} failed for reasons", &self.path()),
                ));
            }
        } else {
            callback(AssertionResult::Failure(
                self.clone(),
                None,
                format!("{} is invalid", &self.path()),
            ));
        }

        Ok(())
    }

    fn path(&self) -> &str {
        match &self {
            Self::WithinRange(assertion) => &assertion.path,
            Self::Equal(assertion) => &assertion.path,
            Self::NotEqual(assertion) => &assertion.path,
            Self::Length(assertion) => &assertion.path,
            Self::Contains(assertion) => &assertion.path,
        }
    }

    fn inner_assert(&self, value: &YamlValue) -> bool {
        match &self {
            Self::WithinRange(assertion) => assertion.assert(value),
            Self::Equal(assertion) => assertion.assert(value),
            Self::NotEqual(assertion) => assertion.assert(value),
            Self::Length(assertion) => assertion.assert(value),
            Self::Contains(assertion) => assertion.assert(value),
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct StatResult {
    pub http_version: String,
    pub response_code: i32,
    pub response_message: Option<String>,
    pub headers: Vec<Header>,
    pub timing: Timing,
    pub json: Option<JsonValue>,
    pub body: Option<String>,
}

impl From<HttpstatResult> for StatResult {
    fn from(result: HttpstatResult) -> Self {
        let mut stat_result = Self {
            http_version: result.http_version,
            response_code: result.response_code,
            response_message: result.response_message,
            headers: result.headers,
            timing: result.timing,
            json: None,
            body: None,
        };

        if let Ok(body) = str::from_utf8(&result.body[..]) {
            let mut in_content = false;
            let lines: Vec<&str> = body
                .lines()
                .filter(|line| {
                    if line.is_empty() && !in_content {
                        in_content = true;
                        return false; // Don't include this line though.
                    }

                    in_content
                })
                .collect();
            let content = lines.join("\n");

            if let Ok(json_content) = serde_json::from_str(&content) {
                stat_result.json = json_content;
            }

            stat_result.body = content.into();
        }

        stat_result
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

fn main() -> Result<()> {
    let opt = Opt::from_args();

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

    let stat_result: StatResult = httpstat(httpstat_config)?.into();

    let json_result = serde_json::to_value(&stat_result)?;

    for assertion in upcake_config.assertions.iter() {
        assertion.assert(&json_result, |result| println!("{:?}", result))?;
    }

    Ok(())
}
