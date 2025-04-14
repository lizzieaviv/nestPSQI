# ====================================================================
# File: psqi4.R
# Description: Functions related to PSQI item 4 (total hours of sleep)
# Includes:
#   - standardize_hours()
#   - extract_hours()
#   - finalize_hours()
# ====================================================================

# ---- standardize_hours --------------------------------------------------------
#' Standardize and clean free-text entries for total sleep hours for PSQI4
#'
#' Converts messy text strings describing total sleep (e.g., "7-8 hrs", "7.5", "seven and a half")
#' into a consistent numeric-friendly format, preserving ranges where present.
#'
#' @param data A data frame
#' @param time_str Unquoted column name containing the raw sleep duration string
#'
#' @return A data frame with a new column `helper_clean` containing the cleaned string
#' @export
standardize_hours <- function(data, time_str) {
  data %>%
    mutate(
      helper_clean = str_replace_all({{ time_str }}, "\\s*\\b(to|and)\\b\\s*|/", "-"), # Replace "to", "and", and "/" with "-"
      helper_clean = str_replace_all(helper_clean, ":30", ".5"),  # Convert ":30" to ".5"
      helper_clean = str_replace_all(helper_clean, ":00", ".0"),  # Convert ":00" to ".0"
      helper_clean = str_extract_all(helper_clean, "[0-9\\.\\-]+"),  # Extract only digits, decimals, and hyphens
      helper_clean = sapply(helper_clean, paste, collapse = " "),  # Convert list to string
      helper_clean = str_replace_all(helper_clean, "^[\\s.-]+|[\\s.-]+$", "")  # Remove leading/trailing hyphens, spaces, and periods
    )
}


# ---- extract_hours --------------------------------------------------------
#' Extract start and end values from standardized total sleep hours for PSQI4
#'
#' Parses a cleaned total sleep duration string (e.g., "7 - 8") to extract numeric start and end values.
#' If no range is present, only the start value is extracted.
#'
#' @param data A data frame
#' @param time_str Unquoted column name containing the cleaned hours string (e.g., from `helper_clean`)
#'
#' @return A data frame with two new columns: `helper_start` and `helper_end`
#' @export
extract_hours <- function(data, time_str) {
  data %>%
    mutate(
      helper_start = str_extract({{ time_str }}, "^\\d+(\\.\\d+)?"), # Extract start time (first number)
      helper_end = case_when(
        str_detect({{ time_str }}, "-") ~ str_extract({{ time_str }}, "(?<=-)\\s*\\d+(\\.\\d+)?"), # Extract end time if dash is present
        .default = NA_character_
      )
    )
}

# ---- finalize_hours --------------------------------------------------------
#' Calculate final total sleep hours value for PSQI4
#'
#' Computes the final total sleep duration value for PSQI scoring. If both start and end values are present,
#' returns their midpoint. If only the start value is available, uses that. Values greater than 20 are set to NA.
#'
#' @param data A data frame
#' @param final_col Unquoted name of the column to create for the final sleep value
#' @param start_col Unquoted column name containing the start value (in hours)
#' @param end_col Unquoted column name containing the end value (in hours)
#'
#' @return A data frame with a new column containing the final total sleep hours value
#' @export
finalize_hours <- function(data, final_col, start_col, end_col) {
  data %>%
    mutate(across(c({{ start_col }}, {{ end_col }}), as.numeric)) %>%  # Convert both start and end columns to numeric
    mutate(
      {{ final_col }} := case_when(
        {{ start_col }} > 20 ~ NA_real_,  # Convert to NA if start time is 15+
        is.na({{ end_col }}) ~ {{ start_col }},  # If end time is NA, use start time
        TRUE ~ ({{ start_col }} + {{ end_col }}) / 2  # Otherwise, compute the midpoint
      )
    )
}
