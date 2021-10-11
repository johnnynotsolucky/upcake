use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::fmt;

use crate::Value;

pub trait Assert {
	fn assert(&self, result: &Value) -> bool;
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
		Value::Sequence(inner) => serde_json::to_string(inner).unwrap(),
		Value::Mapping(inner) => serde_json::to_string(inner).unwrap(),
	}
}

/// Configuration for the assertion to applied against the response results
#[derive(Deserialize, Debug, Clone)]
pub struct RequestAssertionConfig<T>
where
	T: Assert + ?Sized,
{
	/// Whether this assertion should be skipped
	#[serde(default)]
	pub skip: Option<String>,
	/// The [`jql`] path to the field being asserted
	///
	/// Defaults to `.` when not set during deserialization.
	///
	/// Ignored when set on inner assertions, for example [`Length::inner`].
	#[serde(default = "default_path")]
	pub path: Option<String>,
	/// The assertion to apply
	#[serde(flatten)]
	pub assertion: T,
}

pub(crate) fn default_path() -> Option<String> {
	Some(".".into())
}

impl<T> fmt::Display for RequestAssertionConfig<T>
where
	T: Assert + fmt::Display,
{
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.path {
			Some(ref path) => write!(f, "{} {}", path, self.assertion),
			None => write!(f, "{}", self.assertion),
		}
	}
}

/// Result of the assertion
#[derive(Debug, Clone)]
pub enum AssertionResult {
	/// Assertion was successful
	Success(
		/// Assertion configuration used
		AssertionConfig,
		/// Found value
		Value,
	),
	/// Assertion failed
	Failure(
		/// Assertion configuration used
		AssertionConfig,
		/// Found value
		Value,
	),
	/// Failure not related to the assertion
	FailureOther(
		/// Assertion configuration used
		Option<AssertionConfig>,
		/// Failure reason
		String,
	),
	/// Assertion skipped
	Skip(
		/// Assertion configuration used
		AssertionConfig,
		/// Reason for skipping
		String,
	),
}

///
///
#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum AssertionConfig {
	/// ```yaml
	/// - type: between
	///   path: ."response_code"
	///   min: 200
	///   max: 399
	///   inclusive: true
	/// ```
	#[serde(rename = "between")]
	Between(RequestAssertionConfig<Between>),
	/// ```yaml
	/// - type: equal
	///   path: ."response_code"
	///   value: 200
	/// ```
	#[serde(rename = "equal")]
	Equal(RequestAssertionConfig<Equal>),
	/// ```yaml
	/// - type: not-equal
	///   path: ."response_code"
	///   value: 204
	/// ```
	#[serde(rename = "not-equal")]
	NotEqual(RequestAssertionConfig<NotEqual>),
	/// ```yaml
	/// - type: length
	///   path: ."headers".[]
	///   assertion:
	///     - type: equal
	///       value: 5
	/// ```
	#[serde(rename = "length")]
	Length(RequestAssertionConfig<Length>),
	/// # Assert mapping contains all key/value pairs
	///
	/// ```yaml
	/// - type: contains
	///   path: ."content".{}
	///   value:
	///     key: Value
	///     another_property: Some other value
	/// ```
	///
	/// # Assert array contains a value
	///
	/// ```yaml
	/// - type: contains
	///   path: ."content"."my_integer_array".[]
	///   value: 10
	/// ```
	/// or
	///
	/// ```yaml
	/// - type: contains
	///   path: ."content"."my_object_array".[]
	///   value:
	///     id: item_10
	///     value: Item Value
	/// ```
	///
	/// # Assert substring appears in response value
	///
	/// ```yaml
	/// - type: contains
	///   path: ."content"."my_string"
	///   value: "value"
	/// ```
	#[serde(rename = "contains")]
	Contains(RequestAssertionConfig<Contains>),
	/// ```yaml
	/// - type: exists
	///   path: ."content"."my_object".{}
	///   value: id
	/// ```
	#[serde(rename = "exists")]
	Exists(RequestAssertionConfig<Exists>),
	/// ```yaml
	/// - type: greater-than
	///   path: ."response_code"
	///   value: 200
	/// ```
	#[serde(rename = "greater-than")]
	GreaterThan(RequestAssertionConfig<GreaterThan>),
	/// ```yaml
	/// - type: greater-than-equal
	///   path: ."response_code"
	///   value: 200
	/// ```
	#[serde(rename = "greater-than-equal")]
	GreaterThanEqual(RequestAssertionConfig<GreaterThanEqual>),
	/// ```yaml
	/// - type: less-than
	///   path: ."response_code"
	///   value: 400
	/// ```
	#[serde(rename = "less-than")]
	LessThan(RequestAssertionConfig<LessThan>),
	/// ```yaml
	/// - type: less-than-equal
	///   path: ."timing"."starttransfer"
	///   value: 100
	/// ```
	#[serde(rename = "less-than-equal")]
	LessThanEqual(RequestAssertionConfig<LessThanEqual>),
}

impl AssertionConfig {
	pub fn assert(&self, value: &Value) -> Result<AssertionResult> {
		let inner = self.inner();
		if let Some(ref skip) = inner.skip {
			Ok(AssertionResult::Skip(self.clone(), skip.clone()))
		} else {
			let inner_path: Option<&str> = match &inner.path {
				Some(path) => Some(path),
				None => None,
			};

			if let Ok(found_value) = jql::walker(&serde_json::to_value(value)?, inner_path) {
				let result = serde_yaml::to_value(&found_value)?;
				let assertion_result = inner.assertion.assert(&result);

				if assertion_result {
					Ok(AssertionResult::Success(
						self.clone(),
						serde_yaml::to_value(found_value)?,
					))
				} else {
					Ok(AssertionResult::Failure(
						self.clone(),
						serde_yaml::to_value(found_value)?,
					))
				}
			} else {
				Ok(AssertionResult::FailureOther(
					Some(self.clone()),
					"Invalid path".into(),
				))
			}
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
			Self::GreaterThanEqual(assertion) => assertion,
			Self::LessThanEqual(assertion) => assertion,
		}
	}

	fn inner_assert(&self, value: &Value) -> bool {
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
			Self::GreaterThanEqual(assertion) => assertion.fmt(f),
			Self::LessThanEqual(assertion) => assertion.fmt(f),
		}
	}
}

/// Assert that a value is within a range
///
/// See [`AssertionConfig::Between`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Between {
	/// Start of range
	pub min: Value,
	/// End of range
	pub max: Value,
	/// Whether to include [`Between::min`] and [`Between::max`]
	#[serde(default)]
	pub inclusive: bool,
}

impl Between {
	pub fn new<T: Serialize>(min: T, max: T) -> Self {
		Self {
			min: serde_yaml::to_value(min).unwrap(),
			max: serde_yaml::to_value(max).unwrap(),
			inclusive: false,
		}
	}

	pub fn new_inclusive<T: Serialize>(min: T, max: T) -> Self {
		let mut s = Self::new(min, max);
		s.inclusive = true;
		s
	}
}

impl Assert for Between {
	fn assert(&self, value: &Value) -> bool {
		if self.inclusive {
			*value >= self.min && *value <= self.max
		} else {
			*value > self.min && *value < self.max
		}
	}
}

impl fmt::Display for Between {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"between {} and {} ({})",
			value_to_string(&self.min),
			value_to_string(&self.max),
			if self.inclusive {
				"inclusive"
			} else {
				"exclusive"
			}
		)
	}
}

/// Assert that a value equals the given value
///
/// See [`AssertionConfig::Equal`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Equal {
	/// Value to assert against
	value: Value,
}

impl Equal {
	pub fn new<T: Serialize>(value: T) -> Self {
		Self {
			value: serde_yaml::to_value(value).unwrap(),
		}
	}
}

impl Assert for Equal {
	fn assert(&self, value: &Value) -> bool {
		*value == self.value
	}
}

impl fmt::Display for Equal {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "equals {}", value_to_string(&self.value))
	}
}

/// Assert that a value is not equal to the given value
///
/// See [`AssertionConfig::NotEqual`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct NotEqual {
	/// Value to assert against
	value: Value,
}

impl NotEqual {
	pub fn new<T: Serialize>(value: T) -> Self {
		Self {
			value: serde_yaml::to_value(value).unwrap(),
		}
	}
}

impl Assert for NotEqual {
	fn assert(&self, value: &Value) -> bool {
		*value != self.value
	}
}

impl fmt::Display for NotEqual {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "not equals {}", value_to_string(&self.value))
	}
}

/// Assert that a value is greater than the given value
///
/// See [`AssertionConfig::GreaterThan`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct GreaterThan {
	/// Value to assert against
	pub value: Value,
}

impl Assert for GreaterThan {
	fn assert(&self, value: &Value) -> bool {
		*value > self.value
	}
}

impl fmt::Display for GreaterThan {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "greater than {}", value_to_string(&self.value),)
	}
}

/// Assert that a value is greater or equal to the given value
///
/// See [`AssertionConfig::GreaterThanEqual`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct GreaterThanEqual {
	/// Value to assert against
	pub value: Value,
}

impl Assert for GreaterThanEqual {
	fn assert(&self, value: &Value) -> bool {
		*value >= self.value
	}
}

impl fmt::Display for GreaterThanEqual {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"greater than or equal to {}",
			value_to_string(&self.value),
		)
	}
}

/// Assert that a value is less than the given value
///
/// See [`AssertionConfig::LessThan`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct LessThan {
	pub value: Value,
}

impl Assert for LessThan {
	fn assert(&self, value: &Value) -> bool {
		*value < self.value
	}
}

impl fmt::Display for LessThan {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "less than {}", value_to_string(&self.value),)
	}
}

/// Assert that a value is less than or equal to the given value
///
/// See [`AssertionConfig::LessThanEqual`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct LessThanEqual {
	pub value: Value,
}

impl Assert for LessThanEqual {
	fn assert(&self, value: &Value) -> bool {
		*value <= self.value
	}
}

impl fmt::Display for LessThanEqual {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "less than or equal {}", value_to_string(&self.value),)
	}
}

/// Assert that the length of a value passes the given assertion
///
/// See [`AssertionConfig::Length`] for examples.
#[derive(Deserialize, Debug, Clone)]
pub struct Length {
	#[serde(rename = "assertion")]
	pub inner: Box<AssertionConfig>,
}

impl Length {
	pub fn new(assertion_config: AssertionConfig) -> Self {
		Self {
			inner: Box::new(assertion_config),
		}
	}
}

impl Assert for Length {
	fn assert(&self, value: &Value) -> bool {
		let length = match value {
			Value::String(value) => Some(value.len()),
			Value::Mapping(value) => Some(value.len()),
			Value::Sequence(value) => Some(value.len()),
			_ => None,
		};

		length.map_or(false, |length| {
			self.inner.inner_assert(&Value::from(length))
		})
	}
}

impl fmt::Display for Length {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "length {}", self.inner)
	}
}

/// Assert that a response value contains the given value.
///
/// - For strings, it asserts that the substring is present in the value;
/// - For arrays, it asserts that the value is present in the array;
/// - For mappings (dictionary/object types), asserts that the input map is present in the response map.
///
/// See [`AssertionConfig::Contains`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Contains {
	value: Value,
}

impl Contains {
	pub fn new<T: Serialize>(value: T) -> Self {
		Self {
			value: serde_yaml::to_value(value).unwrap(),
		}
	}
}

impl Assert for Contains {
	fn assert(&self, value: &Value) -> bool {
		match value {
			Value::String(value) => self
				.value
				.as_str()
				.map_or(false, |substring| value.contains(substring)),
			Value::Sequence(value) => value.iter().any(|item| item == &self.value),
			Value::Mapping(value) => {
				let mut all_found = true;
				match self.value {
					Value::Mapping(ref search_value) => {
						// If the search value is a Mapping, ensure that all key/value pairs it holds
						// are found in the response value.
						for search_value in search_value.iter() {
							match value.get(search_value.0) {
								Some(value) => {
									if value != search_value.1 {
										all_found = false;
									}
								}
								None => all_found = false,
							}
						}
					}
					_ => all_found = false,
				}

				all_found
			}
			_ => false,
		}
	}
}

impl fmt::Display for Contains {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "contains {}", value_to_string(&self.value))
	}
}

/// Assert that the given value exists as a key in the response value
///
/// See [`AssertionConfig::Exists`] for examples.
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Exists {
	value: Value,
}

impl Exists {
	pub fn new<T: Serialize>(value: T) -> Self {
		Self {
			value: serde_yaml::to_value(value).unwrap(),
		}
	}
}

impl Assert for Exists {
	fn assert(&self, value: &Value) -> bool {
		match value {
			Value::Mapping(value) => value.contains_key(&self.value),
			_ => false,
		}
	}
}

impl fmt::Display for Exists {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "exists {}", value_to_string(&self.value))
	}
}

#[cfg(test)]
mod tests {
	use crate::assertions::*;
	use anyhow::Result;
	use serde::Serialize;
	use serde_yaml::Value;

	#[derive(Serialize)]
	struct Mapping {
		value_a: usize,
		value_b: String,
	}

	fn as_value<T: Serialize>(v: T) -> Value {
		serde_yaml::to_value(v).unwrap()
	}

	#[test]
	fn assert_between_is_between_exclusive() -> Result<()> {
		let test_cases = vec![
			(
				as_value(1),
				as_value(3),
				as_value(2),
				"2 is between 1 and 3, exclusive",
			),
			(
				as_value(0),
				as_value(100),
				as_value(20),
				"20 is between 0 and 100, exclusive",
			),
			(
				as_value(-100),
				as_value(100),
				as_value(0),
				"0 is between -100 and 100, exclusive",
			),
			(
				as_value(0.0),
				as_value(1.0),
				as_value(0.2),
				"0.2 is between 0.0 and 1.0, exclusive",
			),
		];

		for (min, max, value, msg) in test_cases {
			let min = serde_yaml::to_value(min)?;
			let max = serde_yaml::to_value(max)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Between {
				min,
				max,
				inclusive: false,
			};
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_between_is_not_between_exclusive() -> Result<()> {
		let test_cases = vec![
			(
				as_value(1),
				as_value(3),
				as_value(1),
				"1 is not between 1 and 3, exclusive",
			),
			(
				as_value(0),
				as_value(100),
				as_value(120),
				"120 is not between 0 and 100, exclusive",
			),
			(
				as_value(0),
				as_value(100),
				as_value(-1),
				"-1 is not between 0 and 100, exclusive",
			),
			(
				as_value(1),
				as_value(2),
				as_value(2),
				"2 is not between 1 and 2, exclusive",
			),
			(
				as_value(1),
				as_value(1),
				as_value(1),
				"1 is not between 1 and 1, exclusive",
			),
			(
				as_value(0.0),
				as_value(1.0),
				as_value(0.0),
				"0.0 is not between 0.0 and 1.0, exclusive",
			),
			(
				as_value(0.0),
				as_value(1.0),
				as_value(1.1),
				"1.1 is not between 0.0 and 1.0, exclusive",
			),
		];

		for (min, max, value, msg) in test_cases {
			let min = serde_yaml::to_value(min)?;
			let max = serde_yaml::to_value(max)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Between {
				min,
				max,
				inclusive: false,
			};
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_between_is_between_inclusive() -> Result<()> {
		let test_cases = vec![
			(
				as_value(1),
				as_value(3),
				as_value(2),
				"2 is between 1 and 3, inclusive",
			),
			(
				as_value(0),
				as_value(100),
				as_value(20),
				"20 is between 0 and 100, inclusive",
			),
			(
				as_value(-100),
				as_value(100),
				as_value(0),
				"0 is between -100 and 100, inclusive",
			),
			(
				as_value(0.0),
				as_value(1.0),
				as_value(0.2),
				"0.2 is between 0.0 and 1.0, inclusive",
			),
			(
				as_value(1),
				as_value(3),
				as_value(1),
				"1 is between 1 and 3, inclusive",
			),
			(
				as_value(1),
				as_value(2),
				as_value(2),
				"2 is between 1 and 2, inclusive",
			),
			(
				as_value(1),
				as_value(1),
				as_value(1),
				"1 is between 1 and 1, inclusive",
			),
			(
				as_value(0.0),
				as_value(1.0),
				as_value(0.0),
				"0.0 is between 0.0 and 1.0, inclusive",
			),
		];

		for (min, max, value, msg) in test_cases {
			let min = serde_yaml::to_value(min)?;
			let max = serde_yaml::to_value(max)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Between {
				min,
				max,
				inclusive: true,
			};
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_between_is_not_between_inclusive() -> Result<()> {
		let test_cases = vec![
			(
				as_value(0),
				as_value(100),
				as_value(120),
				"120 is not between 0 and 100, inclusive",
			),
			(
				as_value(0),
				as_value(100),
				as_value(-1),
				"-1 is not between 0 and 100, inclusive",
			),
			(
				as_value(0.0),
				as_value(1.0),
				as_value(1.1),
				"1.1 is not between 0.0 and 1.0, inclusive",
			),
			(
				as_value(-1.0),
				as_value(1.0),
				as_value(-1.1),
				"-1.1 is not between -1.0 and 1.0, inclusive",
			),
		];

		for (min, max, value, msg) in test_cases {
			let min = serde_yaml::to_value(min)?;
			let max = serde_yaml::to_value(max)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Between {
				min,
				max,
				inclusive: false,
			};
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_equal_is_equal() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(1), "1 equal 1"),
			(as_value(true), as_value(true), "true equal true"),
			(as_value('a'), as_value('a'), "'a' equal 'a'"),
			(
				as_value("string"),
				as_value("string"),
				"\"string\" (&str) equal \"string\" (&str)",
			),
			(
				as_value(Some(1)),
				as_value(Some(1)),
				"Some(1) equal Some(1)",
			),
			(
				as_value(None as Option<()>),
				as_value(None as Option<()>),
				"None equal None",
			),
			(as_value(1.1), as_value(1.1), "1.1 equal 1.1"),
			(
				as_value("string".to_string()),
				as_value("string".to_string()),
				"\"string\" (String) equal \"string\" (String)",
			),
			(
				as_value(vec![1, 2, 3]),
				as_value(vec![1, 2, 3]),
				"[1, 2, 3] equal [1, 2, 3]",
			),
			(
				as_value(vec!["a", "b", "c"]),
				as_value(vec!["a", "b", "c"]),
				"[\"a\", \"b\", \"c\"] (Vec<&str>) equal [\"a\", \"b\", \"c\"] (Vec<&str>)",
			),
			(
				as_value(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
				as_value(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
				"[\"a\", \"b\", \"c\"] (Vec<String>) equal [\"a\", \"b\", \"c\"] (Vec<String>)",
			),
			(
				as_value(vec!['a', 'b', 'c']),
				as_value(vec!['a', 'b', 'c']),
				"['a', 'b', 'c'] (Vec<char>) equal ['a', 'b', 'c'] (Vec<char>)",
			),
			(
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				"{\"value_a\": 1, \"value_b\": \"String\"} equal {\"value_a\": 1, \"value_b\": \"String\"}",
			),
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Equal::new(expected);
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_equal_is_not_equal() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(2), "1 not equal 2"),
			(as_value(true), as_value(false), "true not equal false"),
			(as_value('a'), as_value('b'), "'a' not equal 'b'"),
			(
				as_value("string"),
				as_value("gnirts"),
				"\"string\" (&str) not equal \"gnirts\" (&str)",
			),
			(
				as_value(Some(1)),
				as_value(Some(2)),
				"Some(1) not equal Some(2)",
			),
			(
				as_value(None as Option<()>),
				as_value(Some(true)),
				"None not equal Some(true)",
			),
			(as_value(1.1), as_value(1.2), "1.1 not equal 1.2"),
			(
				as_value("string".to_string()),
				as_value("gnirts".to_string()),
				"\"string\" (String) not equal \"gnirts\" (String)",
			),
			(
				as_value(vec![1, 2, 3]),
				as_value(vec![3, 2, 1]),
				"[1, 2, 3] not equal [3, 2, 1]",
			),
			(
				as_value(vec!["a", "b", "c"]),
				as_value(vec!["c", "b", "a"]),
				"[\"a\", \"b\", \"c\"] (Vec<&str>) not equal [\"c\", \"b\", \"a\"] (Vec<&str>)",
			),
			(
				as_value(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
				as_value(vec!["c".to_string(), "b".to_string(), "a".to_string()]),
				"[\"a\", \"b\", \"c\"] (Vec<String>) not equal [\"c\", \"b\", \"a\"] (Vec<String>)",
			),
			(
				as_value(vec!['a', 'b', 'c']),
				as_value(vec!['c', 'b', 'a']),
				"['a', 'b', 'c'] (Vec<char>) not equal ['c', 'b', 'a'] (Vec<char>)",
			),
			(
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				as_value(Mapping { value_a: 2, value_b: "Gnirts".into() }),
				"{\"value_a\": 1, \"value_b\": \"String\"} not equal {\"value_a\": 2, \"value_b\": \"Gnirts\"}",
			),
			(as_value(1), as_value(1.0), "1 not equal 1.0"),
			(as_value(true), as_value("true"), "true not equal \"true\""),
			(
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				as_value(vec![1, 2]),
				"{\"value_a\": 1, \"value_b\": \"String\"} not equal [1, 2]",
			),
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Equal::new(expected);
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_not_equal_is_equal() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(1), "1 equal 1"),
			(as_value(true), as_value(true), "true equal true"),
			(as_value('a'), as_value('a'), "'a' equal 'a'"),
			(
				as_value("string"),
				as_value("string"),
				"\"string\" (&str) equal \"string\" (&str)",
			),
			(
				as_value(Some(1)),
				as_value(Some(1)),
				"Some(1) equal Some(1)",
			),
			(
				as_value(None as Option<()>),
				as_value(None as Option<()>),
				"None equal None",
			),
			(as_value(1.1), as_value(1.1), "1.1 equal 1.1"),
			(
				as_value("string".to_string()),
				as_value("string".to_string()),
				"\"string\" (String) equal \"string\" (String)",
			),
			(
				as_value(vec![1, 2, 3]),
				as_value(vec![1, 2, 3]),
				"[1, 2, 3] equal [1, 2, 3]",
			),
			(
				as_value(vec!["a", "b", "c"]),
				as_value(vec!["a", "b", "c"]),
				"[\"a\", \"b\", \"c\"] (Vec<&str>) equal [\"a\", \"b\", \"c\"] (Vec<&str>)",
			),
			(
				as_value(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
				as_value(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
				"[\"a\", \"b\", \"c\"] (Vec<String>) equal [\"a\", \"b\", \"c\"] (Vec<String>)",
			),
			(
				as_value(vec!['a', 'b', 'c']),
				as_value(vec!['a', 'b', 'c']),
				"['a', 'b', 'c'] (Vec<char>) equal ['a', 'b', 'c'] (Vec<char>)",
			),
			(
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				"{\"value_a\": 1, \"value_b\": \"String\"} equal {\"value_a\": 1, \"value_b\": \"String\"}",
			),
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = NotEqual::new(expected);
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_not_equal_is_not_equal() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(2), "1 not equal 2"),
			(as_value(true), as_value(false), "true not equal false"),
			(as_value('a'), as_value('b'), "'a' not equal 'b'"),
			(
				as_value("string"),
				as_value("gnirts"),
				"\"string\" (&str) not equal \"gnirts\" (&str)",
			),
			(
				as_value(Some(1)),
				as_value(Some(2)),
				"Some(1) not equal Some(2)",
			),
			(
				as_value(None as Option<()>),
				as_value(Some(true)),
				"None not equal Some(true)",
			),
			(as_value(1.1), as_value(1.2), "1.1 not equal 1.2"),
			(
				as_value("string".to_string()),
				as_value("gnirts".to_string()),
				"\"string\" (String) not equal \"gnirts\" (String)",
			),
			(
				as_value(vec![1, 2, 3]),
				as_value(vec![3, 2, 1]),
				"[1, 2, 3] not equal [3, 2, 1]",
			),
			(
				as_value(vec!["a", "b", "c"]),
				as_value(vec!["c", "b", "a"]),
				"[\"a\", \"b\", \"c\"] (Vec<&str>) not equal [\"c\", \"b\", \"a\"] (Vec<&str>)",
			),
			(
				as_value(vec!["a".to_string(), "b".to_string(), "c".to_string()]),
				as_value(vec!["c".to_string(), "b".to_string(), "a".to_string()]),
				"[\"a\", \"b\", \"c\"] (Vec<String>) not equal [\"c\", \"b\", \"a\"] (Vec<String>)",
			),
			(
				as_value(vec!['a', 'b', 'c']),
				as_value(vec!['c', 'b', 'a']),
				"['a', 'b', 'c'] (Vec<char>) not equal ['c', 'b', 'a'] (Vec<char>)",
			),
			(
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				as_value(Mapping { value_a: 2, value_b: "Gnirts".into() }),
				"{\"value_a\": 1, \"value_b\": \"String\"} not equal {\"value_a\": 2, \"value_b\": \"Gnirts\"}",
			),
			(as_value(1), as_value(1.0), "1 not equal 1.0"),
			(as_value(true), as_value("true"), "true not equal \"true\""),
			(
				as_value(Mapping { value_a: 1, value_b: "String".into() }),
				as_value(vec![1, 2]),
				"{\"value_a\": 1, \"value_b\": \"String\"} not equal [1, 2]",
			),
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = NotEqual::new(expected);
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_contains_contains_value() -> Result<()> {
		let test_cases = vec![
			(
				as_value("string"),
				as_value("string"),
				"\"string\" contains \"string\"",
			),
			(
				as_value("s"),
				as_value("string"),
				"\"string\" contains \"s\"",
			),
			(
				as_value("tri"),
				as_value("string"),
				"\"string\" contains \"tri\"",
			),
			(
				as_value("ing"),
				as_value("ing"),
				"\"string\" contains \"ing\"",
			),
			(
				as_value(vec![2, 3]),
				as_value(vec![vec![], vec![2, 3], vec![]]),
				"[[], [2, 3], []] contains [2, 3]",
			),
			(as_value(2), as_value(vec![1, 2, 3]), "[1, 2, 3] contains 2"),
			(
				as_value(0.0),
				as_value(vec![0.0, 1.0, 2.0]),
				"[0.0, 1.0, 2.0] contains 0.0",
			),
			(as_value(0), as_value(vec![0]), "[0] contains 0"),
			(
				as_value(2.0),
				as_value(vec![0.0, 1.0, 2.0]),
				"[0.0, 1.0, 2.0] contains 2.0",
			),
			(
				serde_yaml::from_str::<Value>("---\nvalue_a: 1")?,
				serde_yaml::from_str::<Value>("---\nvalue_a: 1\nvalue_b: 2")?,
				"{\"value_a\": 1} is in {\"value_a\": 1, \"value_b\": 2}",
			),
			(
				serde_yaml::from_str::<Value>("---\nvalue_a: 1\nvalue_c: 3")?,
				serde_yaml::from_str::<Value>("---\nvalue_a: 1\nvalue_b: 2\nvalue_c: 3")?,
				"{\"value_a\": 1, \"value_c\": 3} is in {\"value_a\": 1, \"value_b\": 2, \"value_c\": 3}",
			),
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Contains::new(expected);
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_contains_does_not_contain_value() -> Result<()> {
		let test_cases = vec![
			(
				as_value("gnirts"),
				as_value("string"),
				"\"string\" does not contain \"gnirts\"",
			),
			(
				as_value("a"),
				as_value("string"),
				"\"string\" does not contain \"a\"",
			),
			(
				as_value(vec![2, 3]),
				as_value(vec![vec![], vec![1, 4], vec![]]),
				"[[], [1, 4], []] does not contain [2, 3]",
			),
			(
				as_value(4),
				as_value(vec![1, 2, 3]),
				"[1, 2, 3] does not contain 4",
			),
			(
				as_value(-1.0),
				as_value(vec![0.0, 1.0, 2.0]),
				"[0.0, 1.0, 2.0] does not contain -1.0",
			),
			(as_value(1), as_value(vec![0]), "[0] does not contain 1"),
			(
				as_value(2),
				as_value(vec![0.0, 1.0, 2.0]),
				"[0.0, 1.0, 2.0] does not contain 2",
			),
			(
				serde_yaml::from_str::<Value>("---\nvalue_a: 2")?,
				serde_yaml::from_str::<Value>("---\nvalue_a: 1\nvalue_b: 2")?,
				"{\"value_a\": 2} is not in {\"value_a\": 1, \"value_b\": 2}",
			),
			(
				serde_yaml::from_str::<Value>("---\nvalue_a: 1\nvalue_c: 4")?,
				serde_yaml::from_str::<Value>("---\nvalue_a: 1\nvalue_b: 2\nvalue_c: 3")?,
				"{\"value_a\": 1, \"value_c\": 4} is not in {\"value_a\": 1, \"value_b\": 2, \"value_c\": 3}",
			),
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Contains::new(expected);
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_exists_key_exists() -> Result<()> {
		let test_cases = vec![
			(
				as_value("value_a"),
				serde_yaml::from_str::<Value>("---\nvalue_a: 1")?,
				"\"value_a\" exists in {\"value_a\": 1}",
			),
			(
				as_value("value_a"),
				serde_yaml::from_str::<Value>("---\nvalue_a: \"value_a\"\nvalue_b: \"value_b\"")?,
				"\"value_a\" exists in {\"value_a\": \"value_a\", \"value_b\": \"value_b\"}",
			),
		];

		for (key, value, msg) in test_cases {
			let key = serde_yaml::to_value(key)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Exists::new(key);
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_exists_key_does_not_exist() -> Result<()> {
		let test_cases = vec![
			(
				as_value("value_c"),
				serde_yaml::from_str::<Value>("---\nvalue_a: 1")?,
				"\"value_a\" exists in {\"value_a\": 1}",
			),
			(
				as_value("value_d"),
				serde_yaml::from_str::<Value>("---\nvalue_a: \"value_a\"\nvalue_b: \"value_b\"")?,
				"\"value_a\" exists in {\"value_a\": \"value_a\", \"value_b\": \"value_b\"}",
			),
			(
				as_value("value_a"),
				as_value(vec!["value_a", "value_b"]),
				"\"value_a\" exists in [\"value_a\", \"value_b\"]",
			),
		];

		for (key, value, msg) in test_cases {
			let key = serde_yaml::to_value(key)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Exists::new(key);
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_greater_than_is_gt() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(0), "1 is greater than 0"),
			(as_value(0.1), as_value(0), "0.1 is greater than 0"),
			(as_value(0), as_value(-1), "0 is greater than -1"),
			(as_value(1), as_value(-1), "1 is greater than -1"),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = GreaterThan { value };
			assert!(assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_greater_than_is_not_gt() -> Result<()> {
		let test_cases = vec![
			(as_value(0), as_value(0), "0 is not greater than 0"),
			(as_value(0), as_value(0.1), "0 is not greater than 0.1"),
			(as_value(-1), as_value(0), "-1 is not greater than 0"),
			(as_value(-1), as_value(1), "-1 is not greater than 1"),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = GreaterThan { value };
			assert!(!assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_greater_than_equal_is_gte() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(1), "1 is greater than or equal to 1"),
			(
				as_value(0.0),
				as_value(0.0),
				"0.0 is greater than or equal to 0.0",
			),
			(as_value(1), as_value(0), "1 is greater than or equal to 0"),
			(
				as_value(0.1),
				as_value(0),
				"0.1 is greater than or equal to 0",
			),
			(
				as_value(0),
				as_value(-1),
				"0 is greater than or equal to -1",
			),
			(
				as_value(1),
				as_value(-1),
				"1 is greater than or equal to -1",
			),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = GreaterThanEqual { value };
			assert!(assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_greater_than_equal_is_not_gte() -> Result<()> {
		let test_cases = vec![
			(
				as_value(0),
				as_value(0.1),
				"0 is not greater than or equal to 0.1",
			),
			(
				as_value(-1),
				as_value(0),
				"-1 is not greater than or equal to 0",
			),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = GreaterThanEqual { value };
			assert!(!assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_less_than_is_lt() -> Result<()> {
		let test_cases = vec![
			(as_value(0), as_value(1), "0 is less than 1"),
			(as_value(0), as_value(0.1), "0 is less than 0.1"),
			(as_value(-1), as_value(0), "-1 is less than 0"),
			(as_value(-1), as_value(1), "-1 is less than 1"),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = LessThan { value };
			assert!(assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_less_than_is_not_lt() -> Result<()> {
		let test_cases = vec![
			(as_value(0), as_value(0), "0 is not less than 0"),
			(as_value(0.1), as_value(0), "0.1 is not less than 0"),
			(as_value(0), as_value(-1), "0 is not less than -1"),
			(as_value(1), as_value(-1), "1 is not less than -1"),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = LessThan { value };
			assert!(!assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_less_than_equal_is_lte() -> Result<()> {
		let test_cases = vec![
			(as_value(1), as_value(1), "1 is less than or equal to 1"),
			(
				as_value(0.0),
				as_value(0.0),
				"0.0 is less than or equal to 0.0",
			),
			(as_value(0), as_value(1), "0 is less than or equal to 1"),
			(as_value(0), as_value(0.1), "0 is less than or equal to 0.1"),
			(as_value(-1), as_value(0), "-1 is less than or equal to 0"),
			(as_value(-1), as_value(1), "-1 is less than or equal to 1"),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = LessThanEqual { value };
			assert!(assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_less_than_equal_is_not_lte() -> Result<()> {
		let test_cases = vec![
			(
				as_value(0.1),
				as_value(0),
				"0.1 is not less than or equal to 0",
			),
			(
				as_value(0),
				as_value(-1),
				"0 is not less than or equal to -1",
			),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = LessThanEqual { value };
			assert!(!assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_length_equal() -> Result<()> {
		let mut mapping = std::collections::HashMap::new();
		mapping.insert("one", 1);
		mapping.insert("two", 2);
		mapping.insert("three", 3);

		let test_cases = vec![
			(
				as_value("some string"),
				as_value(11),
				"length of \"some string\" equal 11",
			),
			(
				as_value(vec![1, 2, 3]),
				as_value(3),
				"length of [1, 2, 3] equal 3",
			),
			(
				as_value(mapping),
				as_value(3),
				"length of {\"one\": 1, \"two\": 2, \"three\": 3} equal 3",
			),
		];

		for (input, value, msg) in test_cases {
			let input = serde_yaml::to_value(input)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Length {
				inner: Box::new(AssertionConfig::Equal(RequestAssertionConfig {
					skip: None,
					path: None,
					assertion: Equal::new(value),
				})),
			};
			assert!(assertion.assert(&input), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_skip_assertion() -> Result<()> {
		let assertion_config = AssertionConfig::Equal(RequestAssertionConfig {
			skip: Some("Skip".into()),
			path: None,
			assertion: Equal::new(None as Option<()>),
		});

		match assertion_config.assert(&as_value(None as Option<()>))? {
			AssertionResult::Skip(_, reason) => {
				assert!(
					reason == "Skip".to_string(),
					"skip assertion reason equal \"Skip\""
				)
			}
			_ => assert!(false, "assertion should return Skip variant result"),
		}

		Ok(())
	}

	#[test]
	fn assert_failure_invalid_path() -> Result<()> {
		let assertion_config = AssertionConfig::Equal(RequestAssertionConfig {
			skip: None,
			path: Some(".\"invalid\"".into()),
			assertion: Equal::new(None as Option<()>),
		});

		match assertion_config.assert(&as_value(None as Option<()>))? {
			AssertionResult::FailureOther(_, reason) => {
				assert!(
					reason == "Invalid path".to_string(),
					"assertion failure reason equal \"Invalid path\""
				)
			}
			_ => assert!(false, "assertion should return FailureOther variant result"),
		}

		Ok(())
	}

	#[test]
	fn assert_failure_failed_assert() -> Result<()> {
		let assertion_config = AssertionConfig::Equal(RequestAssertionConfig {
			skip: None,
			path: Some(".".into()),
			assertion: Equal::new(true),
		});

		match assertion_config.assert(&as_value(false))? {
			AssertionResult::Failure(_, found) => {
				assert!(
					found == as_value(false),
					"assertion failure found value {:?}",
					found
				)
			}
			_ => assert!(false, "assertion should return Failure variant result"),
		}

		Ok(())
	}

	#[test]
	fn assert_success_assert() -> Result<()> {
		let assertion_config = AssertionConfig::Equal(RequestAssertionConfig {
			skip: None,
			path: Some(".".into()),
			assertion: Equal::new(true),
		});

		match assertion_config.assert(&as_value(true))? {
			AssertionResult::Success(_, found) => {
				assert!(
					found == as_value(true),
					"assertion success found value {:?}",
					found
				)
			}
			_ => assert!(false, "assertion should return Success variant result"),
		}

		Ok(())
	}
}
