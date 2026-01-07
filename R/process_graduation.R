# ==============================================================================
# Graduation Rate Data Processing
# ==============================================================================
#
# This file contains functions for processing raw graduation rate data into
# a standardized format.
#
# Handles:
# - Era detection (v1: 2019-2022, v2: 2023+)
# - Column name standardization
# - Value parsing (percentages, integers, suppression markers)
# - Entity type indicators
#
# ==============================================================================

#' Detect graduation data era
#'
#' Determines whether the data is v1 (2019-2022, 23 columns, no Level field)
#' or v2 (2023+, 24 columns, has Level field).
#'
#' @param raw_data Raw data frame from get_raw_graduation()
#' @return Character "v1" or "v2"
#' @keywords internal
detect_grad_era <- function(raw_data) {
  if ("Level" %in% names(raw_data)) {
    return("v2")  # 2023+
  } else {
    return("v1")  # 2019-2022
  }
}


#' Parse percentage string to numeric
#'
#' Converts VA percentage format ("  95.3%") to decimal (0.953).
#' Handles leading spaces, % suffix, and commas.
#'
#' @param x Character vector of percentages
#' @return Numeric vector on 0-1 scale
#' @keywords internal
parse_percentage <- function(x) {
  x %>%
    trimws() %>%
    stringr::str_remove_all("%") %>%
    stringr::str_remove_all(",") %>%
    as.numeric() %>%
    `/`(., 100)  # Convert to 0-1 scale
}


#' Parse integer string to numeric
#'
#' Converts VA integer format ("     178" or "1,234") to integer.
#' Handles leading spaces and commas.
#'
#' @param x Character vector of integers
#' @return Integer vector
#' @keywords internal
parse_integer <- function(x) {
  x %>%
    trimws() %>%
    stringr::str_remove_all(",") %>%
    # Handle suppression marker
    dplyr::na_if("<") %>%
    as.integer()
}


#' Standardize graduation data column names
#'
#' Converts raw VDOE column names to standardized lowercase_snake_case.
#'
#' @param raw_data Raw data frame
#' @param era Data era ("v1" or "v2")
#' @return Data frame with standardized column names
#' @keywords internal
standardize_grad_columns <- function(raw_data, era) {

  # Define column mapping for v2 (v1 is the same minus "Level")
  col_mapping <- c(
    "Cohort Year" = "cohort_year",
    "Level" = "level",
    "Division Number" = "division_number",
    "Division Name" = "division_name",
    "School Number" = "school_number",
    "School Name" = "school_name",
    "Type of Graduation Rate" = "graduation_type",
    "Rate Type" = "rate_type",
    "Graduation Rate" = "graduation_rate",
    "Students in Cohort" = "cohort_size",
    "Total Graduates" = "total_graduates",
    "Advanced Studies" = "advanced_studies",
    "IB" = "ib_diploma",
    "Standard" = "standard_diploma",
    "Other Diplomas" = "other_diplomas",
    "Applied Studies" = "applied_studies",
    "GED" = "ged",
    "ISAEP" = "isaep",
    "Certificate of Completion" = "certificate",
    "Completion Rate" = "completion_rate",
    "Dropout Rate" = "dropout_rate",
    "Dropouts" = "dropouts",
    "Still Enrolled" = "still_enrolled",
    "Long-Term Absence" = "long_term_absence"
  )

  # For v1, remove "Level" from mapping
  if (era == "v1") {
    col_mapping <- col_mapping[names(col_mapping) != "Level"]
  }

  # Apply mapping (only for columns that exist)
  existing_cols <- intersect(names(col_mapping), names(raw_data))
  names(raw_data)[names(raw_data) %in% existing_cols] <- col_mapping[existing_cols]

  raw_data
}


#' Process raw graduation data
#'
#' Converts raw VDOE graduation data to standardized format.
#' Handles era detection, column renaming, and value parsing.
#'
#' @param raw_data Raw data from get_raw_graduation()
#' @param end_year School year end
#' @return Data frame with standardized schema
#' @keywords internal
process_graduation <- function(raw_data, end_year) {

  # Remove completely blank rows (data quality issue in source CSVs)
  # Some CSVs have blank rows with NA values
  key_cols <- c("Cohort Year", "Division Number", "School Number")
  if (all(key_cols %in% names(raw_data))) {
    raw_data <- raw_data[!is.na(raw_data[["Cohort Year"]]), ]
  }

  # Detect era
  era <- detect_grad_era(raw_data)

  # Standardize column names
  processed <- standardize_grad_columns(raw_data, era)

  # Ensure IDs are character (preserve leading zeros)
  processed$division_number <- as.character(processed$division_number)
  processed$school_number <- as.character(processed$school_number)

  # Parse numeric columns
  # Percentages
  pct_cols <- c("graduation_rate", "completion_rate", "dropout_rate")
  for (col in pct_cols) {
    if (col %in% names(processed)) {
      processed[[col]] <- parse_percentage(processed[[col]])
    }
  }

  # Integers
  int_cols <- c("cohort_size", "total_graduates",
                "advanced_studies", "ib_diploma", "standard_diploma",
                "other_diplomas", "applied_studies", "ged", "isaep",
                "certificate", "dropouts", "still_enrolled", "long_term_absence")
  for (col in int_cols) {
    if (col %in% names(processed)) {
      processed[[col]] <- parse_integer(processed[[col]])
    }
  }

  # Add entity type indicators
  if (era == "v2" && "level" %in% names(processed)) {
    # v2 has explicit Level field
    processed$is_state <- processed$level == "State"
    processed$is_school <- processed$level == "School"
    processed$is_district <- FALSE  # No district-level records in VA data
  } else {
    # v1: all records are school-level (no state aggregates provided)
    processed$is_state <- FALSE
    processed$is_school <- TRUE
    processed$is_district <- FALSE
  }

  # Handle empty school_number for state record (v2)
  if ("school_number" %in% names(processed)) {
    processed$school_number[processed$school_number == ""] <- NA_character_
  }

  # Add end_year if not present
  if (!"end_year" %in% names(processed)) {
    processed$end_year <- end_year
  }

  # Add era flag for reference
  processed$era <- era

  # Select columns in consistent order
  primary_cols <- c("end_year", "era", "level",
                    "division_number", "division_name",
                    "school_number", "school_name",
                    "graduation_type", "rate_type",
                    "graduation_rate", "cohort_size", "total_graduates",
                    "completion_rate", "dropout_rate",
                    "dropouts", "still_enrolled", "long_term_absence",
                    "is_state", "is_school", "is_district")

  diploma_cols <- c("advanced_studies", "ib_diploma", "standard_diploma",
                    "other_diplomas", "applied_studies", "ged", "isaep", "certificate")

  # Keep all columns that exist
  all_cols <- c(primary_cols, diploma_cols)
  existing_cols <- intersect(all_cols, names(processed))

  processed[, existing_cols]
}
