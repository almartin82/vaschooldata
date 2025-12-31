# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw enrollment data from the
# Urban Institute API into a clean, standardized format.
#
# The standard schema matches other state schooldata packages for consistency.
#
# ==============================================================================

#' Process raw enrollment data
#'
#' Transforms raw API data into a standardized schema combining school
#' and division data.
#'
#' @param raw_data List containing school and division data from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process school data
  school_processed <- process_school_enr(raw_data$school, raw_data$school_dir, end_year)

  # Process division (district) data
  division_processed <- process_division_enr(raw_data$division, raw_data$division_dir, end_year)

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
#' @param school_data List with race, grade, and sex data frames
#' @param school_dir Data frame with school directory information
#' @param end_year School year end
#' @return Processed school data frame
#' @keywords internal
process_school_enr <- function(school_data, school_dir, end_year) {

  # Check if we have any data
  if (is.null(school_data$race) || nrow(school_data$race) == 0) {
    return(data.frame())
  }

  # Process race data to get demographics
  race_wide <- pivot_race_data(school_data$race, "ncessch")

  # Process grade data
  grade_wide <- pivot_grade_data(school_data$grade, "ncessch")

  # Process sex data
  sex_wide <- pivot_sex_data(school_data$sex, "ncessch")

  # Merge all data
  result <- race_wide

  if (nrow(grade_wide) > 0) {
    result <- dplyr::left_join(result, grade_wide, by = "ncessch")
  }

  if (nrow(sex_wide) > 0) {
    result <- dplyr::left_join(result, sex_wide, by = "ncessch")
  }

  # Add directory information (names, charter status, etc.)
  if (!is.null(school_dir) && nrow(school_dir) > 0) {
    dir_cols <- c("ncessch", "leaid", "school_name", "county_name", "charter")
    dir_cols <- dir_cols[dir_cols %in% names(school_dir)]

    if (length(dir_cols) > 0) {
      school_dir_subset <- school_dir[, dir_cols, drop = FALSE]
      result <- dplyr::left_join(result, school_dir_subset, by = "ncessch")
    }
  }

  # Standardize column names
  result <- result %>%
    dplyr::mutate(
      end_year = end_year,
      type = "Campus",
      campus_id = as.character(ncessch),
      district_id = if ("leaid" %in% names(.)) as.character(leaid) else substr(as.character(ncessch), 1, 7),
      campus_name = if ("school_name" %in% names(.)) school_name else NA_character_,
      county = if ("county_name" %in% names(.)) county_name else NA_character_,
      charter_flag = if ("charter" %in% names(.)) {
        dplyr::case_when(
          charter == 1 ~ "Y",
          charter == 2 ~ "N",
          TRUE ~ NA_character_
        )
      } else NA_character_
    )

  # Calculate row_total from race totals or max of grade totals
  if ("total" %in% names(result)) {
    result$row_total <- result$total
  } else {
    # Sum grade columns
    grade_cols <- grep("^grade_", names(result), value = TRUE)
    if (length(grade_cols) > 0) {
      result$row_total <- rowSums(result[, grade_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Add district name from directory if available
  result$district_name <- NA_character_

  # Select and rename to standard schema
  result <- result %>%
    dplyr::select(
      end_year, type,
      district_id, campus_id,
      district_name, campus_name,
      county, charter_flag,
      row_total,
      dplyr::any_of(c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")),
      dplyr::any_of(c("male", "female")),
      dplyr::starts_with("grade_")
    )

  result
}


#' Process division-level enrollment data
#'
#' @param division_data List with race, grade, and sex data frames
#' @param division_dir Data frame with division directory information
#' @param end_year School year end
#' @return Processed division data frame
#' @keywords internal
process_division_enr <- function(division_data, division_dir, end_year) {

  # Check if we have any data
  if (is.null(division_data$race) || nrow(division_data$race) == 0) {
    return(data.frame())
  }

  # Process race data to get demographics
  race_wide <- pivot_race_data(division_data$race, "leaid")

  # Process grade data
  grade_wide <- pivot_grade_data(division_data$grade, "leaid")

  # Process sex data
  sex_wide <- pivot_sex_data(division_data$sex, "leaid")

  # Merge all data
  result <- race_wide

  if (nrow(grade_wide) > 0) {
    result <- dplyr::left_join(result, grade_wide, by = "leaid")
  }

  if (nrow(sex_wide) > 0) {
    result <- dplyr::left_join(result, sex_wide, by = "leaid")
  }

  # Add directory information
  if (!is.null(division_dir) && nrow(division_dir) > 0) {
    dir_cols <- c("leaid", "lea_name", "county_name")
    dir_cols <- dir_cols[dir_cols %in% names(division_dir)]

    if (length(dir_cols) > 0) {
      division_dir_subset <- division_dir[, dir_cols, drop = FALSE]
      result <- dplyr::left_join(result, division_dir_subset, by = "leaid")
    }
  }

  # Standardize column names
  result <- result %>%
    dplyr::mutate(
      end_year = end_year,
      type = "District",
      district_id = as.character(leaid),
      campus_id = NA_character_,
      district_name = if ("lea_name" %in% names(.)) lea_name else NA_character_,
      campus_name = NA_character_,
      county = if ("county_name" %in% names(.)) county_name else NA_character_,
      charter_flag = NA_character_
    )

  # Calculate row_total
  if ("total" %in% names(result)) {
    result$row_total <- result$total
  } else {
    grade_cols <- grep("^grade_", names(result), value = TRUE)
    if (length(grade_cols) > 0) {
      result$row_total <- rowSums(result[, grade_cols, drop = FALSE], na.rm = TRUE)
    }
  }

  # Select and rename to standard schema
  result <- result %>%
    dplyr::select(
      end_year, type,
      district_id, campus_id,
      district_name, campus_name,
      county, charter_flag,
      row_total,
      dplyr::any_of(c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")),
      dplyr::any_of(c("male", "female")),
      dplyr::starts_with("grade_")
    )

  result
}


#' Pivot race data from long to wide format
#'
#' @param race_df Data frame with race enrollment data
#' @param id_col Name of the ID column ("ncessch" or "leaid")
#' @return Wide data frame with race columns
#' @keywords internal
pivot_race_data <- function(race_df, id_col) {

  if (is.null(race_df) || nrow(race_df) == 0) {
    return(data.frame())
  }

  # Map race codes to names
  race_df$race_name <- map_race_code(race_df$race)

  # Filter to relevant race categories
  race_df <- race_df %>%
    dplyr::filter(race_name %in% c("white", "black", "hispanic", "asian",
                                    "native_american", "pacific_islander",
                                    "multiracial", "total"))

  # Pivot to wide format
  result <- race_df %>%
    dplyr::select(dplyr::all_of(id_col), race_name, enrollment) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_col),
      names_from = race_name,
      values_from = enrollment,
      values_fn = sum
    )

  result
}


#' Pivot grade data from long to wide format
#'
#' @param grade_df Data frame with grade enrollment data
#' @param id_col Name of the ID column
#' @return Wide data frame with grade columns
#' @keywords internal
pivot_grade_data <- function(grade_df, id_col) {

  if (is.null(grade_df) || nrow(grade_df) == 0) {
    return(data.frame())
  }

  # Map grade codes to standard format
  grade_df$grade_name <- map_grade_code(grade_df$grade)

  # Filter to standard grades
  valid_grades <- c("PK", "K", "01", "02", "03", "04", "05", "06",
                    "07", "08", "09", "10", "11", "12", "UG")

  grade_df <- grade_df %>%
    dplyr::filter(grade_name %in% valid_grades) %>%
    dplyr::mutate(
      grade_col = dplyr::case_when(
        grade_name == "PK" ~ "grade_pk",
        grade_name == "K" ~ "grade_k",
        grade_name == "UG" ~ "grade_ug",
        TRUE ~ paste0("grade_", grade_name)
      )
    )

  # Pivot to wide format
  result <- grade_df %>%
    dplyr::select(dplyr::all_of(id_col), grade_col, enrollment) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_col),
      names_from = grade_col,
      values_from = enrollment,
      values_fn = sum
    )

  result
}


#' Pivot sex data from long to wide format
#'
#' @param sex_df Data frame with sex enrollment data
#' @param id_col Name of the ID column
#' @return Wide data frame with male/female columns
#' @keywords internal
pivot_sex_data <- function(sex_df, id_col) {

  if (is.null(sex_df) || nrow(sex_df) == 0) {
    return(data.frame())
  }

  # Map sex codes: 1 = Male, 2 = Female, 9 = Total/Unknown
  sex_df <- sex_df %>%
    dplyr::mutate(
      sex_name = dplyr::case_when(
        sex == 1 ~ "male",
        sex == 2 ~ "female",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(sex_name))

  # Pivot to wide format
  result <- sex_df %>%
    dplyr::select(dplyr::all_of(id_col), sex_name, enrollment) %>%
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_col),
      names_from = sex_name,
      values_from = enrollment,
      values_fn = sum
    )

  result
}


#' Create state-level aggregate from division data
#'
#' @param division_df Processed division data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(division_df, end_year) {

  if (nrow(division_df) == 0) {
    return(data.frame())
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
    "grade_09", "grade_10", "grade_11", "grade_12",
    "grade_ug"
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
    state_row[[col]] <- sum(division_df[[col]], na.rm = TRUE)
  }

  state_row
}
