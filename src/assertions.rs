use eyre::Result;
use serde::Deserialize;
use serde_json::Value as JsonValue;
use serde_yaml::Value as YamlValue;
use std::fmt;

trait Assert {
	fn assert(&self, result: &YamlValue) -> bool;
}

#[derive(Deserialize, Debug, Clone)]
pub struct RequestAssertion<T: ?Sized> {
	#[serde(default)]
	skip: Option<String>,
	#[serde(default)]
	path: String,
	#[serde(flatten)]
	assertion: T,
}

#[derive(Debug, Clone)]
pub enum AssertionResult {
	Success(Assertion, JsonValue),
	Failure(Assertion, Option<JsonValue>, Option<String>),
	Skip(Assertion, String),
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
		let inner = self.inner();
		if let Some(skip) = inner.skip {
			Ok(AssertionResult::Skip(self.clone(), skip))
		} else if let Ok(found_value) = jql::walker(value, Some(&inner.path)) {
			let result = serde_yaml::to_value(&found_value)?;
			let assertion_result = inner.assertion.assert(&result);

			if assertion_result {
				Ok(AssertionResult::Success(self.clone(), found_value))
			} else {
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

	fn inner(&self) -> Box<RequestAssertion<dyn Assert>> {
		match self {
			Self::Between(assertion) => Box::new(assertion.clone()),
			Self::Equal(assertion) => Box::new(assertion.clone()),
			Self::NotEqual(assertion) => Box::new(assertion.clone()),
			Self::Length(assertion) => Box::new(assertion.clone()),
			Self::Contains(assertion) => Box::new(assertion.clone()),
			Self::Exists(assertion) => Box::new(assertion.clone()),
			Self::GreaterThan(assertion) => Box::new(assertion.clone()),
			Self::LessThan(assertion) => Box::new(assertion.clone()),
			_ => panic!("Invalid assertion configuration"),
		}
	}

	fn inner_assert(&self, value: &YamlValue) -> bool {
		self.inner().assertion.assert(value)
	}
}

impl fmt::Display for Assertion {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match &self {
			Self::Between(assertion) => assertion.fmt(f),
			Self::Equal(assertion) => assertion.fmt(f),
			Self::NotEqual(assertion) => assertion.fmt(f),
			Self::Length(assertion) => assertion.fmt(f),
			Self::Contains(assertion) => assertion.fmt(f),
			Self::Exists(assertion) => assertion.fmt(f),
			Self::GreaterThan(assertion) => assertion.fmt(f),
			Self::LessThan(assertion) => assertion.fmt(f),
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

impl Assert for Between {
	fn assert(&self, value: &YamlValue) -> bool {
		if self.inclusive {
			*value >= self.min && *value <= self.max
		} else {
			*value > self.min && *value < self.max
		}
	}
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

impl fmt::Display for RequestAssertion<Between> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"between {} and {} ({})",
			value_to_string(&self.assertion.min),
			value_to_string(&self.assertion.max),
			if self.assertion.inclusive {
				"inclusive"
			} else {
				"exclusive"
			}
		)
	}
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Equal {
	pub value: YamlValue,
}

impl Assert for Equal {
	fn assert(&self, value: &YamlValue) -> bool {
		*value == self.value
	}
}

impl fmt::Display for RequestAssertion<Equal> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "equals {}", value_to_string(&self.assertion.value))
	}
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct GreaterThan {
	pub value: YamlValue,
	#[serde(default)]
	pub inclusive: bool,
}

impl Assert for GreaterThan {
	fn assert(&self, value: &YamlValue) -> bool {
		if self.inclusive {
			*value >= self.value
		} else {
			*value > self.value
		}
	}
}

impl fmt::Display for RequestAssertion<GreaterThan> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"greater than {} ({})",
			value_to_string(&self.assertion.value),
			if self.assertion.inclusive {
				"inclusive"
			} else {
				"exclusive"
			}
		)
	}
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct LessThan {
	pub value: YamlValue,
	#[serde(default)]
	pub inclusive: bool,
}

impl Assert for LessThan {
	fn assert(&self, value: &YamlValue) -> bool {
		if self.inclusive {
			*value <= self.value
		} else {
			*value < self.value
		}
	}
}

impl fmt::Display for RequestAssertion<LessThan> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"less than {} ({})",
			value_to_string(&self.assertion.value),
			if self.assertion.inclusive {
				"inclusive"
			} else {
				"exclusive"
			}
		)
	}
}

#[derive(Deserialize, Debug, Clone)]
pub struct Length {
	#[serde(rename = "assertion")]
	pub inner: Box<Assertion>,
}

impl Assert for Length {
	fn assert(&self, value: &YamlValue) -> bool {
		let length = match value {
			YamlValue::String(value) => Some(value.len()),
			YamlValue::Mapping(value) => Some(value.len()),
			YamlValue::Sequence(value) => Some(value.len()),
			_ => None,
		};

		length.map_or(false, |length| {
			self.inner.inner_assert(&YamlValue::from(length))
		})
	}
}

impl fmt::Display for RequestAssertion<Length> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "length ")?;
		self.assertion.inner.fmt(f)
	}
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct NotEqual {
	pub value: YamlValue,
}

impl Assert for NotEqual {
	fn assert(&self, value: &YamlValue) -> bool {
		*value != self.value
	}
}

impl fmt::Display for RequestAssertion<NotEqual> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "not equals {}", value_to_string(&self.assertion.value))
	}
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Contains {
	pub value: YamlValue,
}

impl Assert for Contains {
	fn assert(&self, value: &YamlValue) -> bool {
		match value {
			YamlValue::String(value) => self
				.value
				.as_str()
				.map_or(false, |substring| value.contains(substring)),
			YamlValue::Sequence(value) => {
				let search_value = self.value.clone();
				value.iter().any(|item| *item == search_value)
			}
			_ => false,
		}
	}
}

impl fmt::Display for RequestAssertion<Contains> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "contains {}", value_to_string(&self.assertion.value))
	}
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Exists {
	pub value: YamlValue,
}

impl Assert for Exists {
	fn assert(&self, value: &YamlValue) -> bool {
		match value {
			YamlValue::Mapping(value) => value.contains_key(&self.value),
			_ => false,
		}
	}
}

impl fmt::Display for RequestAssertion<Exists> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "exists {}", value_to_string(&self.assertion.value))
	}
}
