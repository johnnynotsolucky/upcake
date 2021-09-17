use eyre::Result;
use serde::Deserialize;
use serde_json::Value as JsonValue;
use serde_yaml::Value as YamlValue;
use std::fmt;

pub trait Assert {
	fn assert(&self, result: &YamlValue) -> bool;
}

#[derive(Deserialize, Debug, Clone)]
pub struct RequestAssertionConfig<T: Assert + ?Sized> {
	#[serde(default)]
	pub skip: Option<String>,
	#[serde(default)]
	pub path: String,
	#[serde(flatten)]
	pub assertion: T,
}

#[derive(Debug, Clone)]
pub enum AssertionResult {
	Success(AssertionConfig, JsonValue),
	Failure(AssertionConfig, Option<JsonValue>, Option<String>),
	Skip(AssertionConfig, String),
}

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum AssertionConfig {
	#[serde(rename = "between")]
	Between(RequestAssertionConfig<Between>),
	#[serde(rename = "equal")]
	Equal(RequestAssertionConfig<Equal>),
	#[serde(rename = "not-equal")]
	NotEqual(RequestAssertionConfig<NotEqual>),
	#[serde(rename = "length")]
	Length(RequestAssertionConfig<Length>),
	#[serde(rename = "contains")]
	Contains(RequestAssertionConfig<Contains>),
	#[serde(rename = "exists")]
	Exists(RequestAssertionConfig<Exists>),
	#[serde(rename = "greater-than")]
	GreaterThan(RequestAssertionConfig<GreaterThan>),
	#[serde(rename = "less-than")]
	LessThan(RequestAssertionConfig<LessThan>),
}

impl AssertionConfig {
	pub fn assert(&self, value: &JsonValue) -> Result<AssertionResult> {
		let inner = self.inner();
		if let Some(ref skip) = inner.skip {
			Ok(AssertionResult::Skip(self.clone(), skip.clone()))
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

	fn inner(&self) -> &RequestAssertionConfig<dyn Assert> {
		match self {
			Self::Between(assertion) => assertion,
			Self::Equal(assertion) => assertion,
			Self::NotEqual(assertion) => assertion,
			Self::Length(assertion) => assertion,
			Self::Contains(assertion) => assertion,
			Self::Exists(assertion) => assertion,
			Self::GreaterThan(assertion) => assertion,
			Self::LessThan(assertion) => assertion,
		}
	}

	fn inner_assert(&self, value: &YamlValue) -> bool {
		self.inner().assertion.assert(value)
	}
}

impl fmt::Display for AssertionConfig {
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

impl fmt::Display for RequestAssertionConfig<Between> {
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

impl fmt::Display for RequestAssertionConfig<Equal> {
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

impl fmt::Display for RequestAssertionConfig<GreaterThan> {
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

impl fmt::Display for RequestAssertionConfig<LessThan> {
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
	pub inner: Box<AssertionConfig>,
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

impl fmt::Display for RequestAssertionConfig<Length> {
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

impl fmt::Display for RequestAssertionConfig<NotEqual> {
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

impl fmt::Display for RequestAssertionConfig<Contains> {
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

impl fmt::Display for RequestAssertionConfig<Exists> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "exists {}", value_to_string(&self.assertion.value))
	}
}
