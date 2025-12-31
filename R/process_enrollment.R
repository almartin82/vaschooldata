# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw enrollment data from the
# Virginia Department of Education (VDOE) into a clean, standardized format.
#
# The standard schema matches other state schooldata packages for consistency.
#
# ==============================================================================

#' Process raw enrollment data
#'
#' Transforms raw VDOE data into a standardized schema combining school
#' and division data.
#'
#' @param raw_data List containing school and division data from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process school data
  school_processed <- process_school_enr(raw_data$school, end_year)

  # Process division data
  division_processed <- process_division_enr(raw_data$division, end_year)

  # Create state aggregate from division data
  state_processed <- create_state_aggregate(division_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, division_processed, school_processed)

  # Ensure consistent column order
  col_order <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "charter_flag",
    "row_total",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Only include columns that exist
  col_order <- col_order[col_order %in% names(result)]
  result <- result[, col_order]

  result
}


#' Process school-level enrollment data
#'
#' @param school_data Data frame with school enrollment from VDOE
#' @param end_year School year end
#' @return Processed school data frame
#' @keywords internal
process_school_enr <- function(school_data, end_year) {

  # Check if we have any data
  if (is.null(school_data) || nrow(school_data) == 0) {
    return(create_empty_school_df(end_year))
  }

  # Standardize column names to lowercase
  names(school_data) <- tolower(names(school_data))

  # Identify key columns based on common VDOE naming patterns
  # School ID column
  id_cols <- c("school_num", "school_number", "sch_num", "schoolid", "school_id", "ncessch")
  id_col <- id_cols[id_cols %in% names(school_data)]

  # Division ID column
  div_cols <- c("division_num", "div_num", "division_number", "divisionid", "division_id", "leaid")
  div_col <- div_cols[div_cols %in% names(school_data)]

  # School name column
  name_cols <- c("school_name", "schoolname", "sch_name", "school")
  name_col <- name_cols[name_cols %in% names(school_data)]

  # Division name column
  div_name_cols <- c("division_name", "divisionname", "div_name", "division")
  div_name_col <- div_name_cols[div_name_cols %in% names(school_data)]

  # Start building result

  result <- school_data

  # Add standard columns
  result$end_year <- end_year
  result$type <- "Campus"

  # Map ID columns
  if (length(id_col) > 0) {
    result$campus_id <- as.character(result[[id_col[1]]])
  } else {
    result$campus_id <- NA_character_
  }

  if (length(div_col) > 0) {
    result$district_id <- as.character(result[[div_col[1]]])
  } else {
    result$district_id <- NA_character_
  }

  # Map name columns
  if (length(name_col) > 0) {
    result$campus_name <- as.character(result[[name_col[1]]])
  } else {
    result$campus_name <- NA_character_
  }

  if (length(div_name_col) > 0) {
    result$district_name <- as.character(result[[div_name_col[1]]])
  } else {
    result$district_name <- NA_character_
  }

  # Extract enrollment columns
  result <- extract_enrollment_columns(result)

  # Calculate row_total if not present
  if (!"row_total" %in% names(result) || all(is.na(result$row_total))) {
    demo_cols <- c("white", "black", "hispanic", "asian",
                   "native_american", "pacific_islander", "multiracial")
    demo_cols <- demo_cols[demo_cols %in% names(result)]
    if (length(demo_cols) > 0) {
      result$row_total <- rowSums(result[, demo_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Select standard columns
  result <- select_standard_columns(result)

  result
}


#' Process division-level enrollment data
#'
#' @param division_data Data frame with division enrollment from VDOE
#' @param end_year School year end
#' @return Processed division data frame
#' @keywords internal
process_division_enr <- function(division_data, end_year) {

  # Check if we have any data
  if (is.null(division_data) || nrow(division_data) == 0) {
    return(create_empty_division_df(end_year))
  }

  # Standardize column names to lowercase
  names(division_data) <- tolower(names(division_data))

  # Identify key columns
  # Division ID column
  div_cols <- c("division_num", "div_num", "division_number", "divisionid", "division_id", "leaid")
  div_col <- div_cols[div_cols %in% names(division_data)]

  # Division name column
  div_name_cols <- c("division_name", "divisionname", "div_name", "division")
  div_name_col <- div_name_cols[div_name_cols %in% names(division_data)]

  # Start building result
  result <- division_data

  # Add standard columns
  result$end_year <- end_year
  result$type <- "District"
  result$campus_id <- NA_character_
  result$campus_name <- NA_character_

  # Map ID columns
  if (length(div_col) > 0) {
    result$district_id <- as.character(result[[div_col[1]]])
  } else {
    result$district_id <- NA_character_
  }

  # Map name columns
  if (length(div_name_col) > 0) {
    result$district_name <- as.character(result[[div_name_col[1]]])
  } else {
    result$district_name <- NA_character_
  }

  # Extract enrollment columns
  result <- extract_enrollment_columns(result)

  # Calculate row_total if not present
  if (!"row_total" %in% names(result) || all(is.na(result$row_total))) {
    demo_cols <- c("white", "black", "hispanic", "asian",
                   "native_american", "pacific_islander", "multiracial")
    demo_cols <- demo_cols[demo_cols %in% names(result)]
    if (length(demo_cols) > 0) {
      result$row_total <- rowSums(result[, demo_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Select standard columns
  result <- select_standard_columns(result)

  result
}


#' Extract enrollment columns from raw data
#'
#' Maps VDOE column names to standard enrollment column names.
#'
#' @param df Data frame with raw VDOE data
#' @return Data frame with standardized enrollment columns
#' @keywords internal
extract_enrollment_columns <- function(df) {

  # Column mapping from VDOE names to standard names
  col_mapping <- list(
    # Total enrollment
    row_total = c("total", "total_enrollment", "enrollment", "count", "membership"),

    # Race/ethnicity columns
    white = c("white", "wh", "white_count", "race_white"),
    black = c("black", "bl", "black_count", "african_american", "race_black"),
    hispanic = c("hispanic", "hi", "hispanic_count", "latino", "race_hispanic"),
    asian = c("asian", "as", "asian_count", "race_asian"),
    native_american = c("american_indian", "am", "native_american",
                        "american_indian_count", "race_american_indian"),
    pacific_islander = c("pacific_islander", "pi", "hp", "native_hawaiian",
                         "native_hawaiian_count", "race_pacific_islander"),
    multiracial = c("multiracial", "two_or_more", "mr", "tr",
                    "two_or_more_count", "race_two_or_more"),

    # Gender columns
    male = c("male", "m", "male_count"),
    female = c("female", "f", "female_count"),

    # Grade columns
    grade_pk = c("pk", "prek", "pre_k", "grade_pk", "prekindergarten"),
    grade_k = c("k", "kg", "kindergarten", "grade_k", "grade_kg"),
    grade_01 = c("g01", "grade_1", "grade_01", "gr1"),
    grade_02 = c("g02", "grade_2", "grade_02", "gr2"),
    grade_03 = c("g03", "grade_3", "grade_03", "gr3"),
    grade_04 = c("g04", "grade_4", "grade_04", "gr4"),
    grade_05 = c("g05", "grade_5", "grade_05", "gr5"),
    grade_06 = c("g06", "grade_6", "grade_06", "gr6"),
    grade_07 = c("g07", "grade_7", "grade_07", "gr7"),
    grade_08 = c("g08", "grade_8", "grade_08", "gr8"),
    grade_09 = c("g09", "grade_9", "grade_09", "gr9"),
    grade_10 = c("g10", "grade_10", "gr10"),
    grade_11 = c("g11", "grade_11", "gr11"),
    grade_12 = c("g12", "grade_12", "gr12"),

    # County
    county = c("county", "county_name", "locality"),

    # Charter flag
    charter_flag = c("charter", "charter_school", "is_charter")
  )

  # Apply mapping
  for (std_name in names(col_mapping)) {
    possible_names <- col_mapping[[std_name]]
    found_col <- possible_names[possible_names %in% names(df)]

    if (length(found_col) > 0 && !(std_name %in% names(df))) {
      df[[std_name]] <- safe_numeric(df[[found_col[1]]])
    } else if (std_name %in% names(df)) {
      df[[std_name]] <- safe_numeric(df[[std_name]])
    }
  }

  df
}


#' Select standard columns for output
#'
#' @param df Data frame with processed data
#' @return Data frame with only standard columns
#' @keywords internal
select_standard_columns <- function(df) {

  # Standard column order
  std_cols <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "charter_flag",
    "row_total",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Add missing columns as NA
  for (col in std_cols) {
    if (!(col %in% names(df))) {
      df[[col]] <- NA
    }
  }

  # Select only standard columns
  df[, std_cols, drop = FALSE]
}


#' Create state-level aggregate from division data
#'
#' @param division_df Processed division data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(division_df, end_year) {

  if (is.null(division_df) || nrow(division_df) == 0) {
    return(create_empty_state_df(end_year))
  }

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "male", "female",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(division_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(as.numeric(division_df[[col]]), na.rm = TRUE)
  }

  state_row
}


#' Create empty school data frame
#'
#' @param end_year School year end
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_school_df <- function(end_year) {
  data.frame(
    end_year = integer(),
    type = character(),
    district_id = character(),
    campus_id = character(),
    district_name = character(),
    campus_name = character(),
    county = character(),
    charter_flag = character(),
    row_total = numeric(),
    white = numeric(),
    black = numeric(),
    hispanic = numeric(),
    asian = numeric(),
    native_american = numeric(),
    pacific_islander = numeric(),
    multiracial = numeric(),
    male = numeric(),
    female = numeric(),
    stringsAsFactors = FALSE
  )
}


#' Create empty division data frame
#'
#' @param end_year School year end
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_division_df <- function(end_year) {
  data.frame(
    end_year = integer(),
    type = character(),
    district_id = character(),
    campus_id = character(),
    district_name = character(),
    campus_name = character(),
    county = character(),
    charter_flag = character(),
    row_total = numeric(),
    white = numeric(),
    black = numeric(),
    hispanic = numeric(),
    asian = numeric(),
    native_american = numeric(),
    pacific_islander = numeric(),
    multiracial = numeric(),
    male = numeric(),
    female = numeric(),
    stringsAsFactors = FALSE
  )
}


#' Create empty state data frame
#'
#' @param end_year School year end
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_state_df <- function(end_year) {
  data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = NA_real_,
    white = NA_real_,
    black = NA_real_,
    hispanic = NA_real_,
    asian = NA_real_,
    native_american = NA_real_,
    pacific_islander = NA_real_,
    multiracial = NA_real_,
    male = NA_real_,
    female = NA_real_,
    stringsAsFactors = FALSE
  )
}
