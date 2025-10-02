#[cfg(feature = "schemars")]
use std::borrow::Cow;
use std::{
    ops::{Deref, DerefMut},
    str::FromStr,
};

use jiff::{Span, Timestamp, ToSpan, tz::TimeZone};
use rustc_hash::FxHashMap;
use uv_normalize::PackageName;

/// A timestamp that excludes files newer than it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExcludeNewerTimestamp {
    /// The resolved timestamp
    timestamp: Timestamp,
    /// The original span string if this was created from a relative timestamp
    span: Option<String>,
}

impl serde::Serialize for ExcludeNewerTimestamp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.timestamp.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for ExcludeNewerTimestamp {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Self::from_str(&s).map_err(serde::de::Error::custom)
    }
}

impl ExcludeNewerTimestamp {
    /// Returns the timestamp in milliseconds.
    pub fn timestamp_millis(&self) -> i64 {
        self.timestamp.as_millisecond()
    }

    /// Returns the timestamp.
    pub fn timestamp(&self) -> Timestamp {
        self.timestamp
    }

    /// Returns the original span string if this was created from a relative timestamp.
    pub fn span(&self) -> Option<&str> {
        self.span.as_deref()
    }

    /// Creates a new ExcludeNewerTimestamp with the given timestamp and optional span.
    pub fn with_span(timestamp: Timestamp, span: Option<String>) -> Self {
        Self { timestamp, span }
    }
}

impl From<Timestamp> for ExcludeNewerTimestamp {
    fn from(timestamp: Timestamp) -> Self {
        Self {
            timestamp,
            span: None,
        }
    }
}

impl FromStr for ExcludeNewerTimestamp {
    type Err = String;

    /// Parse an [`ExcludeNewerTimestamp`] from a string.
    ///
    /// Accepts RFC 3339 timestamps (e.g., `2006-12-02T02:07:43Z`), local dates in the same format
    /// (e.g., `2006-12-02`), and relative durations (e.g., `1 week`, `30 days`).
    fn from_str(input: &str) -> Result<Self, Self::Err> {
        // Try parsing as a timestamp first
        if let Ok(timestamp) = input.parse::<Timestamp>() {
            return Ok(Self {
                timestamp,
                span: None,
            });
        }

        // Try parsing as a date
        // In Jiff, if an RFC 3339 timestamp could be parsed, then it must necessarily be the case
        // that a date can also be parsed. So we can collapse the error cases here. That is, if we
        // fail to parse a timestamp and a date, then it should be sufficient to just report the
        // error from parsing the date. If someone tried to write a timestamp but committed an error
        // in the non-date portion, the date parsing below will still report a holistic error that
        // will make sense to the user. (I added a snapshot test for that case.)
        let date_err = match input.parse::<jiff::civil::Date>() {
            Ok(date) => {
                let timestamp = date
                    .checked_add(1.day())
                    .and_then(|date| date.to_zoned(TimeZone::system()))
                    .map(|zdt| zdt.timestamp())
                    .map_err(|err| {
                        format!(
                            "`{input}` parsed to date `{date}`, but could not \
                         be converted to a timestamp: {err}",
                        )
                    })?;
                return Ok(Self {
                    timestamp,
                    span: None,
                });
            }
            Err(err) => err,
        };

        // Try parsing as a span
        let span_err = match input.parse::<Span>() {
            Ok(span) => {
                let now = Timestamp::now();

                // For spans with calendar units (years, months, weeks, days)
                if span.get_years() != 0
                    || span.get_months() != 0
                    || span.get_weeks() != 0
                    || span.get_days() != 0
                {
                    // Convert to a date, subtract the calendar units, then back to a timestamp
                    let now_date = now.to_zoned(TimeZone::system()).date();
                    let past_date = now_date.checked_sub(span).map_err(|err| {
                        format!(
                            "Duration `{input}` is too large to subtract from current date: {err}"
                        )
                    })?;
                    let cutoff = past_date
                        .to_zoned(TimeZone::system())
                        .map_err(|err| format!("Could not convert date back to timestamp: {err}"))?
                        .timestamp();
                    Ok(Self {
                        timestamp: cutoff,
                        span: Some(input.to_string()),
                    })
                } else {
                    // Only time units - can subtract directly from timestamp
                    let cutoff = now.checked_sub(span).map_err(|err| {
                        format!(
                            "Duration `{input}` is too large to subtract from current time: {err}"
                        )
                    })?;
                    Ok(Self {
                        timestamp: cutoff,
                        span: Some(input.to_string()),
                    })
                }
            }
            Err(span_err) => {
                // Return a comprehensive error message
                Err(format!(
                    "`{input}` could not be parsed as a timestamp, date, or relative duration: {span_err}"
                ))
            }
        }
    }
}

impl std::fmt::Display for ExcludeNewerTimestamp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.timestamp.fmt(f)
    }
}

/// A package-specific exclude-newer entry.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct ExcludeNewerPackageEntry {
    pub package: PackageName,
    pub timestamp: ExcludeNewerTimestamp,
}

impl FromStr for ExcludeNewerPackageEntry {
    type Err = String;

    /// Parses a [`ExcludeNewerPackageEntry`] from a string in the format `PACKAGE=DATE`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let Some((package, date)) = s.split_once('=') else {
            return Err(format!(
                "Invalid `exclude-newer-package` value `{s}`: expected format `PACKAGE=DATE`"
            ));
        };

        let package = PackageName::from_str(package).map_err(|err| {
            format!("Invalid `exclude-newer-package` package name `{package}`: {err}")
        })?;
        let timestamp = ExcludeNewerTimestamp::from_str(date)
            .map_err(|err| format!("Invalid `exclude-newer-package` timestamp `{date}`: {err}"))?;

        Ok(Self { package, timestamp })
    }
}

impl From<(PackageName, ExcludeNewerTimestamp)> for ExcludeNewerPackageEntry {
    fn from((package, timestamp): (PackageName, ExcludeNewerTimestamp)) -> Self {
        Self { package, timestamp }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct ExcludeNewerPackage(FxHashMap<PackageName, ExcludeNewerTimestamp>);

impl Deref for ExcludeNewerPackage {
    type Target = FxHashMap<PackageName, ExcludeNewerTimestamp>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ExcludeNewerPackage {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl FromIterator<ExcludeNewerPackageEntry> for ExcludeNewerPackage {
    fn from_iter<T: IntoIterator<Item = ExcludeNewerPackageEntry>>(iter: T) -> Self {
        Self(
            iter.into_iter()
                .map(|entry| (entry.package, entry.timestamp))
                .collect(),
        )
    }
}

impl IntoIterator for ExcludeNewerPackage {
    type Item = (PackageName, ExcludeNewerTimestamp);
    type IntoIter = std::collections::hash_map::IntoIter<PackageName, ExcludeNewerTimestamp>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> IntoIterator for &'a ExcludeNewerPackage {
    type Item = (&'a PackageName, &'a ExcludeNewerTimestamp);
    type IntoIter = std::collections::hash_map::Iter<'a, PackageName, ExcludeNewerTimestamp>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl ExcludeNewerPackage {
    /// Convert to the inner `HashMap`.
    pub fn into_inner(self) -> FxHashMap<PackageName, ExcludeNewerTimestamp> {
        self.0
    }
}

/// A setting that excludes files newer than a timestamp, at a global level or per-package.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
#[cfg_attr(feature = "schemars", derive(schemars::JsonSchema))]
pub struct ExcludeNewer {
    /// Global timestamp that applies to all packages if no package-specific timestamp is set.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub global: Option<ExcludeNewerTimestamp>,
    /// Per-package timestamps that override the global timestamp.
    #[serde(default, skip_serializing_if = "FxHashMap::is_empty")]
    pub package: ExcludeNewerPackage,
}

impl ExcludeNewer {
    /// Create a new exclude newer configuration with just a global timestamp.
    pub fn global(global: ExcludeNewerTimestamp) -> Self {
        Self {
            global: Some(global),
            package: ExcludeNewerPackage::default(),
        }
    }

    /// Create a new exclude newer configuration.
    pub fn new(global: Option<ExcludeNewerTimestamp>, package: ExcludeNewerPackage) -> Self {
        Self { global, package }
    }

    /// Create from CLI arguments.
    pub fn from_args(
        global: Option<ExcludeNewerTimestamp>,
        package: Vec<ExcludeNewerPackageEntry>,
    ) -> Self {
        let package: ExcludeNewerPackage = package.into_iter().collect();

        Self { global, package }
    }

    /// Returns the timestamp for a specific package, falling back to the global timestamp if set.
    pub fn exclude_newer_package(
        &self,
        package_name: &PackageName,
    ) -> Option<ExcludeNewerTimestamp> {
        self.package
            .get(package_name)
            .cloned()
            .or(self.global.clone())
    }

    /// Returns true if this has any configuration (global or per-package).
    pub fn is_empty(&self) -> bool {
        self.global.is_none() && self.package.is_empty()
    }
}

impl std::fmt::Display for ExcludeNewer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(global) = &self.global {
            write!(f, "global: {global}")?;
            if !self.package.is_empty() {
                write!(f, ", ")?;
            }
        }
        let mut first = true;
        for (name, timestamp) in &self.package {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{name}: {timestamp}")?;
            first = false;
        }
        Ok(())
    }
}

#[cfg(feature = "schemars")]
impl schemars::JsonSchema for ExcludeNewerTimestamp {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("ExcludeNewerTimestamp")
    }

    fn json_schema(_generator: &mut schemars::generate::SchemaGenerator) -> schemars::Schema {
        schemars::json_schema!({
            "type": "string",
            "pattern": r"^\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}(Z|[+-]\d{2}:\d{2}))?$",
            "description": "Exclude distributions uploaded after the given timestamp.\n\nAccepts both RFC 3339 timestamps (e.g., `2006-12-02T02:07:43Z`) and local dates in the same format (e.g., `2006-12-02`), as well as relative durations (e.g., `1 week`, `30 days`, `6 months`). Relative durations are resolved to an absolute timestamp at lock time.",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[test]
    fn test_exclude_newer_timestamp_absolute() {
        // Test RFC 3339 timestamp
        let timestamp = ExcludeNewerTimestamp::from_str("2023-01-01T00:00:00Z").unwrap();
        assert!(timestamp.to_string().contains("2023-01-01"));

        // Test local date
        let timestamp = ExcludeNewerTimestamp::from_str("2023-06-15").unwrap();
        assert!(timestamp.to_string().contains("2023-06-16")); // Should be next day
    }

    #[test]
    fn test_exclude_newer_timestamp_relative() {
        // Test "1 hour" - simpler test case
        let timestamp = ExcludeNewerTimestamp::from_str("1 hour").unwrap();
        let now = jiff::Timestamp::now();
        let diff = now.as_second() - timestamp.timestamp.as_second();
        // Should be approximately 1 hour (3600 seconds) ago
        assert!(
            diff >= 3550 && diff <= 3650,
            "Expected ~3600 seconds, got {diff}"
        );

        // Test that we get a timestamp in the past
        assert!(timestamp.timestamp < now, "Timestamp should be in the past");

        // Test parsing succeeds for various formats
        assert!(ExcludeNewerTimestamp::from_str("2 days").is_ok());
        assert!(ExcludeNewerTimestamp::from_str("1 week").is_ok());
        assert!(ExcludeNewerTimestamp::from_str("30 days").is_ok());
    }

    #[test]
    fn test_exclude_newer_timestamp_invalid() {
        // Test invalid formats
        assert!(ExcludeNewerTimestamp::from_str("invalid").is_err());
        assert!(ExcludeNewerTimestamp::from_str("not a date").is_err());
        assert!(ExcludeNewerTimestamp::from_str("").is_err());
    }

    #[test]
    fn test_exclude_newer_package_entry() {
        let entry = ExcludeNewerPackageEntry::from_str("numpy=2023-01-01T00:00:00Z").unwrap();
        assert_eq!(entry.package.as_ref(), "numpy");
        assert!(entry.timestamp.to_string().contains("2023-01-01"));

        // Test with relative timestamp
        let entry = ExcludeNewerPackageEntry::from_str("requests=7 days").unwrap();
        assert_eq!(entry.package.as_ref(), "requests");
        // Just verify it parsed without error - the timestamp will be relative to now
    }
}
