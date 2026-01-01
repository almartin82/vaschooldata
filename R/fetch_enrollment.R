# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains the main user-facing functions for downloading enrollment
# data from Virginia.
#
# Data is sourced directly from the Virginia Department of Education (VDOE)
# via the School Quality Profiles website and Fall Membership reports.
#
# ==============================================================================

#' Fetch Virginia enrollment data
#'
#' Downloads and processes enrollment data from the Virginia Department of
#' Education (VDOE). Data is sourced from VDOE's School Quality Profiles
#' and Fall Membership reports.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 2016-2025.
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from VDOE.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_id, campus_id, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and grade_level columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Filter to specific division
#' fairfax <- enr_2024 |>
#'   dplyr::filter(district_name == "FAIRFAX COUNTY PUBLIC SCHOOLS")
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ", max(available_years),
      ". Got: ", end_year,
      "\nUse get_available_years() to see all available years."
    ))
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  # Get raw data from VDOE
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) |>
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, end_year, cache_type)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2021, 2022, 2023))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2021:2023)
#'
#' # Track enrollment trends
#' enr_multi |>
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#'   dplyr::select(end_year, n_students)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available_years <- get_available_years()
  invalid_years <- end_years[!end_years %in% available_years]

  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "),
      "\nend_year must be between ", min(available_years), " and ", max(available_years),
      "\nUse get_available_years() to see all available years."
    ))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}


#' Fetch enrollment using local file
#'
#' Processes a locally downloaded Fall Membership file from VDOE.
#' Use this function when automatic downloads fail due to CAPTCHA
#' or other restrictions.
#'
#' @param file_path Path to the downloaded Excel or CSV file
#' @param end_year School year end (e.g., 2024 for 2023-24)
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), caches the processed data.
#' @return Data frame with enrollment data
#' @export
#' @examples
#' \dontrun{
#' # Download Fall Membership file manually from:
#' # https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics
#'
#' # Then process it:
#' enr_2024 <- fetch_enr_local("path/to/fall_membership_2024.xlsx", 2024)
#' }
fetch_enr_local <- function(file_path, end_year, tidy = TRUE, use_cache = TRUE) {

  # Import local file
  raw <- import_local_fm(file_path, end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) |>
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    cache_type <- if (tidy) "tidy" else "wide"
    write_cache(processed, end_year, cache_type)
  }

  processed
}
