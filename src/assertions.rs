use eyre::Result;
use serde::Deserialize;
use serde_json::Value as JsonValue;
use serde_yaml::Value as YamlValue;

trait Assert {
    fn assert(&self, result: &YamlValue) -> bool;
}

#[derive(Deserialize, Debug, Clone)]
pub struct RequestAssertion<T> {
    #[serde(default)]
    path: String,
    #[serde(flatten)]
    assertion: T,
}

#[derive(Debug, Clone)]
pub enum AssertionResult {
    Success(Assertion, JsonValue),
    Failure(Assertion, Option<JsonValue>, Option<String>),
}

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum Assertion {
    #[serde(rename = "between")]
    Between(RequestAssertion<Between>),
    #[serde(rename = "equal")]
    Equal(RequestAssertion<Equal>),
    #[serde(rename = "not-equal")]
    NotEqual(RequestAssertion<NotEqual>),
    #[serde(rename = "length")]
    Length(RequestAssertion<Length>),
    #[serde(rename = "contains")]
    Contains(RequestAssertion<Contains>),
    #[serde(rename = "exists")]
    Exists(RequestAssertion<Exists>),
    #[serde(rename = "greater-than")]
    GreaterThan(RequestAssertion<GreaterThan>),
    #[serde(rename = "less-than")]
    LessThan(RequestAssertion<LessThan>),
    #[serde(skip_deserializing)]
    ErrorAssertion,
}

impl Assertion {
    pub fn assert(&self, value: &JsonValue) -> Result<AssertionResult> {
        if let Ok(found_value) = jql::walker(value, Some(self.path())) {
            let result = serde_yaml::to_value(&found_value)?;
            let assertion_result = self.inner_assert(&result);

            if assertion_result {
                Ok(AssertionResult::Success(self.clone(), found_value))
            } else {
                // TODO implement Display for AssertionResult
                Ok(AssertionResult::Failure(
                    self.clone(),
                    Some(found_value),
                    None,
                ))
            }
        } else {
            Ok(AssertionResult::Failure(
                self.clone(),
                None,
                Some("Invalid path".into()),
            ))
        }
    }

    fn path(&self) -> &str {
        match &self {
            Self::Between(assertion) => &assertion.path,
            Self::Equal(assertion) => &assertion.path,
            Self::NotEqual(assertion) => &assertion.path,
            Self::Length(assertion) => &assertion.path,
            Self::Contains(assertion) => &assertion.path,
            Self::Exists(assertion) => &assertion.path,
            Self::GreaterThan(assertion) => &assertion.path,
            Self::LessThan(assertion) => &assertion.path,
            _ => panic!("Invalid assertion configuration"),
        }
    }

    fn inner_assert(&self, value: &YamlValue) -> bool {
        match &self {
            Self::Between(assertion) => assertion.assert(value),
            Self::Equal(assertion) => assertion.assert(value),
            Self::NotEqual(assertion) => assertion.assert(value),
            Self::Length(assertion) => assertion.assert(value),
            Self::Contains(assertion) => assertion.assert(value),
            Self::Exists(assertion) => assertion.assert(value),
            Self::GreaterThan(assertion) => assertion.assert(value),
            Self::LessThan(assertion) => assertion.assert(value),
            _ => panic!("Invalid assertion configuration"),
        }
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Between {
    pub min: YamlValue,
    pub max: YamlValue,
    #[serde(default)]
    pub inclusive: bool,
}

impl Assert for RequestAssertion<Between> {
    fn assert(&self, value: &YamlValue) -> bool {
        if self.assertion.inclusive {
            *value >= self.assertion.min && *value <= self.assertion.max
        } else {
            *value > self.assertion.min && *value < self.assertion.max
        }
        // let message = format!(
        // 	"{} not within range of [{}, {}]",
        // 	value_to_string(value),
        // 	value_to_string(min),
        // 	value_to_string(max)
        // );
        //
        // callback(AssertionResult::Failure(
        // 	Assertion::Between(self.clone()),
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

#[derive(Deserialize, Default, Debug, Clone)]
pub struct GreaterThan {
    pub value: YamlValue,
    #[serde(default)]
    pub inclusive: bool,
}

impl Assert for RequestAssertion<GreaterThan> {
    fn assert(&self, value: &YamlValue) -> bool {
        if self.assertion.inclusive {
            *value >= self.assertion.value
        } else {
            *value > self.assertion.value
        }
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct LessThan {
    pub value: YamlValue,
    #[serde(default)]
    pub inclusive: bool,
}

impl Assert for RequestAssertion<LessThan> {
    fn assert(&self, value: &YamlValue) -> bool {
        if self.assertion.inclusive {
            *value <= self.assertion.value
        } else {
            *value < self.assertion.value
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct Length {
    #[serde(rename = "assertion")]
    pub inner: Box<Assertion>,
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
            self.assertion.inner.inner_assert(&YamlValue::from(length))
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
            YamlValue::Sequence(value) => {
                let search_value = YamlValue::from(self.assertion.value.clone());
                value.iter().find(|item| {
                    **item == search_value
                }).is_some()
            },
            _ => false,
        }
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Exists {
    pub value: YamlValue,
}

impl Assert for RequestAssertion<Exists> {
    fn assert(&self, value: &YamlValue) -> bool {
        match value {
            YamlValue::Mapping(value) => value.contains_key(&self.assertion.value),
            // YamlValue::Mapping(value) => self
            //     .assertion
            //     .value
            //     .as_str()
            //     .map_or(false, |key| value.contains_key()),
            _ => false,
        }
    }
}