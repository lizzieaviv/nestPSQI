# ====================================================================
# File: psqi2.R
# Description: Functions related to PSQI item 2
# Includes:
#   - standardize_latency()
#   - extract_latency_times()
#   - convert_to_minutes()
#   - finalize_latency()
# ====================================================================

# ---- standardize_latency --------------------------------------------------------
#' Standardize and clean free-text latency strings for PSQI2
#'
#' Converts messy or inconsistent text entries (e.g., "1 hr 30 mins", "one hour and a half") into a standardized format
#' containing numeric values and consistent units ("1 hr 30 min"). Also handles ranges (e.g., "1-2 hr").
#'
#' @param data A data frame
#' @param time_str Unquoted column name containing raw latency text
#'
#' @return A data frame with a new column `helper_clean` containing the cleaned and standardized string
#' @export
standardize_latency <- function(data, time_str) {
  data %>%
    mutate(
      helper_clean = str_to_lower({{ time_str }}),
      helper_clean = str_replace_all(helper_clean, "(?<=\\d)\\s*h\\b|\\b(h|hours|hrs|hour|hr)\\b", "hr"), # Standardize hour
      helper_clean = str_replace_all(helper_clean, "(?<=\\d)\\s*m\\b|\\b(m|mins|minutes|minute|min|mns|mn)\\b", "min"), # Standardize min
      helper_clean = str_replace_all(helper_clean, "\\b(one|an)\\b", "1"), # Convert "one" and "an" to "1"
      helper_clean = str_replace_all(helper_clean, "\\btwo\\b", "2"), # Convert "two" to "2"
      helper_clean = str_replace_all(helper_clean, "\\s*\\bto\\b\\s*|/", "-"), # Replace "to" and "/" with "-"
      helper_clean = str_replace_all(helper_clean, "(\\d+)(hr|min)", "\\1 \\2"), # Ensure space before "hr"/"min"
      helper_clean = str_extract_all(helper_clean, "[0-9\\.]+|hr|min|-"), # Extract only relevant parts
      helper_clean = sapply(helper_clean, paste, collapse = " "), # Convert list to string
      helper_clean = str_replace_all(helper_clean, "^[\\s.-]+|[\\s.-]+$", "") # Remove leading/trailing "-", ".", or spaces
    )
}


# ---- extract_latency_times --------------------------------------------------------
#' Extract start, end, and unit values from cleaned latency strings for PSQI2
#'
#' Parses a cleaned latency string (e.g., "1 hr - 2 hr", "90 min") to extract start and end numeric values
#' and identify whether the units are in hours, minutes, or both.
#'
#' @param data A data frame
#' @param time_str Unquoted column name containing the cleaned latency string (e.g., from `helper_clean`)
#'
#' @return A data frame with three new columns: `helper_start`, `helper_end`, and `helper_unit`
#' @export
extract_latency_times <- function(data, time_str) {
  data %>%
    mutate(
      helper_start = str_extract({{ time_str }}, "^\\d+(\\.\\d+)?"), # Extract start time
      helper_end = case_when(
        str_detect({{ time_str }}, "-") ~ str_extract({{ time_str }}, "(?<=-)\\s*\\d+(\\.\\d+)?"),
        .default = NA_character_ # Extract end time if dash is present, otherwise NA
      ),
      helper_unit = case_when(
        str_detect({{ time_str }}, "hr") & str_detect({{ time_str }}, "min") ~ "both",  # Both "hr" and "min" present
        str_detect({{ time_str }}, "hr") ~ "hr",  # Only "hr" present
        str_detect({{ time_str }}, "min") ~ "min",  # Only "min" present
        .default = "min" # Default to "min" if neither is present
      )
    )
}

# ---- convert_to_minutes --------------------------------------------------------
#' Convert extracted latency values to minutesfor PSQI2
#'
#' Takes numeric start and end values and converts them to minutes based on the unit column.
#' Supports values expressed in hours, minutes, or both.
#'
#' @param data A data frame
#' @param start_col Unquoted name of the column containing the start value
#' @param end_col Unquoted name of the column containing the end value
#' @param unit_col Unquoted name of the column indicating the time unit ("hr", "min", or "both")
#'
#' @return A data frame with start and end columns converted to minutes
#' @export
convert_to_minutes <- function(data, start_col, end_col, unit_col) {
  data %>%
    mutate(across(c({{ start_col }}, {{ end_col }}), as.numeric)) %>%  # Convert both columns to numeric
    mutate(
      {{ start_col }} := case_when(
        {{ unit_col }} == "hr" ~ {{ start_col }} * 60, # if units are hours, multiply start time by 60
        TRUE ~ {{ start_col }} # otherwise, keep value as is
      ),
      {{ end_col }} := case_when(
        {{ unit_col }} == "hr" ~ {{ end_col }} * 60,    # if units are hours, multiply end time by 60
        {{ unit_col }} == "both" ~ {{ end_col }} * 60,   # if both units are listed, multiply end time by 60
        TRUE ~ {{ end_col }} # otherwise, keep value as is
      )
    )
}


# ---- finalize_latency --------------------------------------------------------
#' Calculate final PSQI latency value from start and end values for PSQI2
#'
#' Computes the final latency value for PSQI scoring. If both start and end values are present,
#' calculates their midpoint. If only the start value is present, uses that. Values of 400 or more are set to NA.
#'
#' @param data A data frame
#' @param final_col Unquoted name of the column to create for the final latency value
#' @param start_col Unquoted name of the column containing the start value (in minutes)
#' @param end_col Unquoted name of the column containing the end value (in minutes)
#'
#' @return A data frame with a new column containing the final latency value
#' @export
finalize_latency <- function(data, final_col, start_col, end_col) {
  data %>%
    mutate({{ final_col }} := case_when(
      {{ start_col }} >= 400 ~ NA_real_,  # Convert to NA if start time is 400+
      is.na({{ end_col }}) ~ {{ start_col }},  # If end time is NA, use start time
      TRUE ~ ({{ start_col }} + {{ end_col }}) / 2  # Otherwise, compute the midpoint
    ))
}
