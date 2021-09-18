use anyhow::Result;
use serde::Deserialize;
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
	Success(AssertionConfig, YamlValue),
	Failure(AssertionConfig, Option<YamlValue>, Option<String>),
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
	pub fn assert(&self, value: &YamlValue) -> Result<AssertionResult> {
		let inner = self.inner();
		if let Some(ref skip) = inner.skip {
			Ok(AssertionResult::Skip(self.clone(), skip.clone()))
		} else if let Ok(found_value) =
			jql::walker(&serde_json::to_value(value)?, Some(&inner.path))
		{
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
					Some(serde_yaml::to_value(found_value)?),
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

#[cfg(test)]
mod tests {
	use crate::assertions::{
		Assert, Between, Contains, Equal, Exists, GreaterThan, Length, LessThan,
	};
	use anyhow::Result;
	use serde::Serialize;
	use serde_yaml::Value as YamlValue;

	#[derive(Serialize)]
	struct Mapping {
		value_a: usize,
		value_b: String,
	}

	fn as_value<T: Serialize>(v: T) -> YamlValue {
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
				as_value(None as Option<bool>),
				as_value(None as Option<bool>),
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
			let assertion = Equal { value: expected };
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
				as_value(None as Option<bool>),
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
			let assertion = Equal { value: expected };
			assert!(!assertion.assert(&value), "{}", msg);
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
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Contains { value: expected };
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
		];

		for (expected, value, msg) in test_cases {
			let expected = serde_yaml::to_value(expected)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Contains { value: expected };
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_exists_key_exists() -> Result<()> {
		let test_cases = vec![
			(
				as_value("value_a"),
				serde_yaml::from_str::<YamlValue>("---\nvalue_a: 1")?,
				"\"value_a\" exists in {\"value_a\": 1}",
			),
			(
				as_value("value_a"),
				serde_yaml::from_str::<YamlValue>(
					"---\nvalue_a: \"value_a\"\nvalue_b: \"value_b\"",
				)?,
				"\"value_a\" exists in {\"value_a\": \"value_a\", \"value_b\": \"value_b\"}",
			),
		];

		for (key, value, msg) in test_cases {
			let key = serde_yaml::to_value(key)?;
			let value = serde_yaml::to_value(value)?;
			let assertion = Exists { value: key };
			assert!(assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_exists_key_does_not_exist() -> Result<()> {
		let test_cases = vec![
			(
				as_value("value_c"),
				serde_yaml::from_str::<YamlValue>("---\nvalue_a: 1")?,
				"\"value_a\" exists in {\"value_a\": 1}",
			),
			(
				as_value("value_d"),
				serde_yaml::from_str::<YamlValue>(
					"---\nvalue_a: \"value_a\"\nvalue_b: \"value_b\"",
				)?,
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
			let assertion = Exists { value: key };
			assert!(!assertion.assert(&value), "{}", msg);
		}

		Ok(())
	}

	#[test]
	fn assert_greater_than_is_gt_exclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_greater_than_is_not_gt_exclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_greater_than_is_gt_inclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_greater_than_is_not_gt_inclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_less_than_is_lt_exclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_less_than_is_not_lt_exclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_less_than_is_lt_inclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_less_than_is_not_lt_inclusive() -> Result<()> {
		todo!()
	}

	#[test]
	fn assert_length_() -> Result<()> {
		todo!()
	}
}
