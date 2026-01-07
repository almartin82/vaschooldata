# ==============================================================================
# Graduation Rate Data Fetching Functions
# ==============================================================================
#
# This file contains the main user-facing functions for downloading graduation
# rate data from Virginia.
#
# Data is sourced directly from the Virginia Department of Education (VDOE)
# via the Open Data Portal (CKAN API).
#
# ==============================================================================

#' Fetch Virginia graduation rate data
#'
#' Downloads and processes graduation rate data from the Virginia Department of
#' Education (VDOE). Data is sourced from VDOE's Cohort Graduation and Dropout
#' Report available through the Open Data Portal.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2022-23
#'   school year is year '2023'. Valid values are 2019-2023.
#' @param tidy If TRUE (default), returns data in long (tidy) format with
#'   diploma_type column. If FALSE, returns wide format with one row per school.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from VDOE.
#' @return Data frame with graduation rate data. Wide format includes columns for
#'   division/school names, counts, and rates by diploma type. Tidy format pivots
#'   diploma counts into diploma_type and diploma_count columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2023 graduation data (2022-23 school year)
#' grad_2023 <- fetch_graduation(2023)
#'
#' # Get wide format
#' grad_wide <- fetch_graduation(2023, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' grad_fresh <- fetch_graduation(2023, use_cache = FALSE)
#'
#' # Filter to state-level
#' grad_2023 |>
#'   dplyr::filter(is_state, diploma_type == "all") |>
#'   dplyr::select(graduation_rate, cohort_size, total_graduates)
#'
#' # Compare schools
#' grad_2023 |>
#'   dplyr::filter(is_school, diploma_type == "all") |>
#'   dplyr::arrange(dplyr::desc(graduation_rate)) |>
#'   dplyr::select(school_name, graduation_rate, cohort_size)
#' }
fetch_graduation <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  available_years <- get_available_grad_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ", max(available_years),
      ". Got: ", end_year,
      "\nUse get_available_grad_years() to see all available years."
    ))
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "grad_tidy" else "grad_wide"

  # Check cache first (only if not explicitly disabled)
  if (use_cache) {
    cache_path <- file.path(get_cache_dir(), paste0(cache_type, "_", end_year, ".rds"))
    if (file.exists(cache_path)) {
      # Check cache age
      file_info <- file.info(cache_path)
      age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

      if (age_days <= 30) {
        message(paste("Using cached data for", end_year))
        return(readRDS(cache_path))
      }
    }
  }

  # Get raw data from VDOE
  raw <- get_raw_graduation(end_year)

  # Process to standard schema
  processed <- process_graduation(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_graduation(processed)
    # Calculate state aggregates for v1 era (no state records in source)
    processed <- calc_state_aggregates(processed)
    # Add "all" subgroup record (summary across diploma types)
    processed <- add_all_subgroup(processed)
  }

  # Cache the result
  if (use_cache) {
    cache_path <- file.path(get_cache_dir(), paste0(cache_type, "_", end_year, ".rds"))
    saveRDS(processed, cache_path)
  }

  processed
}


#' Add "all" subgroup to graduation data
#'
#' Adds a summary record that aggregates all diploma types.
#'
#' @param tidy_data Tidy graduation data
#' @return Data frame with added "all" subgroup records
#' @keywords internal
add_all_subgroup <- function(tidy_data) {

  # Get unique entities (one row per school/state)
  unique_entities <- tidy_data %>%
    dplyr::distinct(
      end_year,
      division_number,
      division_name,
      school_number,
      school_name,
      rate_type,
      cohort_size,
      total_graduates,
      graduation_rate,
      completion_rate,
      dropout_rate,
      dropouts,
      still_enrolled,
      long_term_absence,
      is_state,
      is_school,
      is_district
    )

  # Add "all" diploma type
  all_records <- unique_entities %>%
    dplyr::mutate(
      diploma_type = "all",
      diploma_count = NA_integer_
    )

  # Combine with original data
  dplyr::bind_rows(tidy_data, all_records)
}


#' Fetch graduation rate data for multiple years
#'
#' Downloads and combines graduation rate data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2019, 2020, 2021))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with graduation rate data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 5 years of data
#' grad_multi <- fetch_graduation_multi(2019:2023)
#'
#' # Track graduation rate trends
#' grad_multi |>
#'   dplyr::filter(is_state, diploma_type == "all") |>
#'   dplyr::select(end_year, graduation_rate, cohort_size)
#'
#' # Compare specific school across years
#' grad_multi |>
#'   dplyr::filter(school_number == "0540", diploma_type == "all") |>
#'   dplyr::select(end_year, graduation_rate)
#' }
fetch_graduation_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  available_years <- get_available_grad_years()
  invalid_years <- end_years[!end_years %in% available_years]

  if (length(invalid_years) > 0) {
    stop(paste0(
      "Invalid years: ", paste(invalid_years, collapse = ", "),
      "\nend_year must be between ", min(available_years), " and ", max(available_years),
      "\nUse get_available_grad_years() to see all available years."
    ))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_graduation(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}
