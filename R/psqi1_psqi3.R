# ====================================================================
# File: psqi1_psqi3.R
# Description: Functions related to PSQI items 1 and 3
# Includes:
#   - clean_time_range()
#   - extract_time_range()
#   - convert_to_military_night() -- for PSQI1
#   - convert_to_military_morning() -- for PSQI3
#   - finalize_time()
# ====================================================================

# ---- clean_time_range --------------------------------------------------------
#' Clean and standardize a free-text time range column for PSQI1/PSQI3
#'
#' Cleans inconsistent or messy time strings into a consistent format like "9:00 pm - 11:00 pm".
#'
#' @param data A data frame
#' @param time_col Unquoted column name with time strings (e.g., bedtime/wake time)
#'
#' @return A data frame with a new column `helper_clean`
#' @export
clean_time_range <- function(data, time_col) {
  data %>%
    mutate(
      helper_clean = str_to_lower({{ time_col }}), # Convert to lowercase

      # Replace "midnight" with "12:00 am"
      helper_clean = str_replace_all(helper_clean, "\\bmidnight\\b", "12:00 am"),

      # Remove unnecessary words before the first number but keep spaces intact
      helper_clean = str_replace(helper_clean, ".*?(\\d{1,2}(:\\d{2})?\\s*(am|pm)?)", "\\1"),

      # Replace "to", "or", "and", and "/" with " - " (ensuring spaces remain)
      helper_clean = str_replace_all(helper_clean, "\\s*(to|or|and|/)\\s*", " - "),

      # Ensure dashes have spaces (handles cases like "7pm-8pm" → "7pm - 8pm")
      helper_clean = str_replace_all(helper_clean,
                                     "(\\d{1,2}(:\\d{2})?\\s*(am|pm)?)\\s*-\\s*(\\d{1,2}(:\\d{2})?\\s*(am|pm)?)", "\\1 - \\4"),

      # Remove extra spaces but keep needed ones
      helper_clean = str_trim(str_replace_all(helper_clean, "\\s+", " ")),

      # Convert empty strings to NA
      helper_clean = ifelse(helper_clean == "", NA, helper_clean)
    )
}

# ---- extract_time_range --------------------------------------------------------
#' Extract start and end times from a cleaned time range column for PSQI1/PSQI3
#'
#' After `clean_time_range()` standardizes the text, this function extracts the start and end times
#' (before and after the " - ") into two new columns: `helper_start` and `helper_end`.
#'
#' @param data A data frame
#' @param time_col Unquoted column name containing the cleaned time range (e.g., output of `helper_clean`)
#'
#' @return A data frame with new columns `helper_start` and `helper_end`
#' @export
extract_time_range <- function(data, time_col) {
  data %>%
    mutate(
      helper_start = str_extract({{ time_col }}, "^[^ -]+"), # Extract first part (before " - ")
      helper_end = str_extract({{ time_col }}, "(?<= - )[^ -]+") # Extract second part (after " - ")
    )
}

# ---- convert_to_military_night --------------------------------------------------------
#' Convert night-time text entries to military time format for PSQI1
#'
#' Cleans and standardizes text entries (e.g., bedtimes) into military time format (e.g., "9pm" → "21:00").
#' This version is intended for PSQI Item 1 (nighttime sleep onset).
#'
#' @param data A data frame
#' @param columns A character vector of column names to convert
#'
#' @return A data frame with modified columns in military time format (HH:MM)
#' @export
convert_to_military_night <- function(data, columns) {
  data %>%
    # Remove spaces and replace '.' with ':'
    mutate(across(all_of(columns), ~ .x %>%
                    str_replace_all(" ", "") %>%
                    str_replace("\\.", ":") %>%
                    str_replace(";", ":"))) %>%

    # Convert various time formats to military time
    mutate(across(all_of(columns), ~ .x %>%
                    str_replace_all("midnight", "00:00") %>%
                    str_replace_all("12am", "00:00") %>%
                    str_replace_all("12", "00") %>%
                    str_replace_all("1am", "01:00") %>%
                    str_replace_all("2am", "02:00") %>%
                    str_replace_all("3am", "03:00") %>%
                    str_replace_all("7am", "07:00") %>%
                    str_replace_all("7pm", "19:00") %>%
                    str_replace("^8", "20") %>%  # Convert 8 at start to 20
                    str_replace("^9", "21") %>%  # Convert 9 at start to 21
                    str_replace_all("10", "22") %>%
                    str_replace_all("11", "23") %>%
                    str_replace_all("eleven", "23") %>%
                    str_replace_all("pm", ":00") %>%
                    str_replace_all("am", ":00") %>%
                    str_replace_all("p", ":00") %>%
                    str_replace_all("a", ":00"))) %>%

    # Trim to five characters and remove trailing colons
    mutate(across(all_of(columns), ~ .x %>%
                    str_sub(start = 1, end = 5) %>%
                    str_replace(":$", ""))) %>%

    # Ensure correct formatting by adding leading zeros and ':00' where needed
    mutate(across(all_of(columns), ~ case_when(
      str_length(.x) == 1 ~ str_c("0", .x, ":00"),  # Single digit gets leading zero and ':00'
      str_length(.x) == 2 ~ str_c(.x, ":00"),        # Two-digit numbers get ':00'
      TRUE ~ .x                                       # Keep everything else unchanged
    ))) %>%

    # Convert specific cases
    mutate(across(all_of(columns), ~ case_when(
      .x == "2030" ~ "20:30",
      .x == "2100" ~ "21:00",
      .x == "2130" ~ "21:30",
      .x == "2200" ~ "22:00",
      .x == "2230" ~ "22:30",
      .x == "2300" ~ "23:00",
      .x == "2330" ~ "23:30",
      .x == "0100" ~ "01:00",
      TRUE ~ .x
    )))
}

# ---- convert_to_military_morning --------------------------------------------------------
#' Convert morning-time text entries to military time format for PSQI3
#'
#' Cleans and standardizes text entries (e.g., wake times) into military time format (e.g., "6:30am" → "06:30").
#' This version is intended for PSQI Item 3 (morning wake time).
#'
#' @param data A data frame
#' @param columns A character vector of column names to convert
#'
#' @return A data frame with modified columns in military time format (HH:MM)
#' @export
convert_to_military_morning <- function(data, columns) {
  data %>%
    # Remove spaces and replace '.' and ';' with ':'
    mutate(across(all_of(columns), ~ .x %>%
                    str_replace_all(" ", "") %>%
                    str_replace("\\.", ":") %>%
                    str_replace(";", ":"))) %>%

    # Convert various time formats to military time
    mutate(across(all_of(columns), ~ .x %>%
                    str_replace_all("h", ":00") %>%
                    str_replace_all("pm", ":00") %>%
                    str_replace_all("am", ":00") %>%
                    str_replace_all("p", ":00") %>%
                    str_replace_all("a", ":00"))) %>%

    # Trim to five characters and remove trailing colons
    mutate(across(all_of(columns), ~ .x %>%
                    str_sub(start = 1, end = 5) %>%
                    str_replace(":$", ""))) %>%

    # Ensure correct formatting by adding leading zeros and ':00' where needed
    mutate(across(all_of(columns), ~ case_when(
      str_length(.x) == 1 ~ str_c("0", .x, ":00"),  # Single digit gets leading zero and ':00'
      str_length(.x) == 2 ~ str_c(.x, ":00"),        # Two-digit numbers get ':00'
      TRUE ~ .x                                       # Keep everything else unchanged
    ))) %>%

    # Fix cases where times like '630:0' appear incorrectly
    mutate(across(all_of(columns), ~ str_replace(.x, ":0$", ""))) %>%

    # Convert improperly formatted times (e.g., '630' -> '6:30', but not '8:00')
    mutate(across(all_of(columns), ~ case_when(
      str_detect(.x, "^\\d{3}$") ~ str_c(substr(.x, 1, 1), ":", substr(.x, 2, 3)), # Convert '630' -> '6:30'
      str_detect(.x, "^\\d{4}$") ~ str_c(substr(.x, 1, 2), ":", substr(.x, 3, 4)), # Convert '0730' -> '07:30'
      TRUE ~ .x                                       # Keep everything else unchanged
    )))
}

# ---- finalize_time --------------------------------------------------------
#' Calculate a single PSQI time value based on start and end times for PSQI1/PSQI3
#'
#' Combines time columns into a single value for PSQI scoring. If both start and end times are available,
#' computes their midpoint. If only start time is available, uses that. Adds dummy dates to support crossing midnight.
#'
#' @param data A data frame
#' @param columns A character vector of column names containing military time strings (e.g., `helper_start`, `helper_end`)
#' @param output_var The name of the output variable to be created (unquoted)
#'
#' @return A data frame with a new time variable for use in PSQI scoring
#' @export
finalize_time <- function(data, columns, output_var) {
  data %>%
    # Add missing lead zero if length is 4
    mutate(across(all_of(columns), ~ case_when(
      str_length(.x) == 4 ~ str_c("0", .x),
      TRUE ~ .x))) %>%

    # Convert to time format (add seconds and convert to chron times), suppressing format warnings
    mutate(across(all_of(columns), ~ suppressWarnings(.x %>%
                                                        str_c(":00") %>%
                                                        times()))) %>%

    # Add dummy dates using chron, suppressing warnings for malformed entries
    mutate(across(all_of(columns), ~ suppressWarnings(chron(
      dates = ifelse(.x >= "00:00:00" & .x <= "12:00:00", "01/02/2000", "01/01/2000"),
      times = .x)))) %>%

    # Rewrite specified output variable as midpoint or start time
    mutate(!!sym(output_var) := suppressWarnings(times(
      (ifelse(is.na(helper_end), as.numeric(times(helper_start)),
              (as.numeric(times(helper_start)) + as.numeric(times(helper_end))) / 2)) %% 1)))
}
