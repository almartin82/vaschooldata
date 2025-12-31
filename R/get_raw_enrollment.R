# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from the
# Urban Institute's Education Data Portal API.
#
# Data comes from NCES Common Core of Data (CCD) via the Urban Institute API.
# The API provides school and district level enrollment data from 1986-2023.
#
# Virginia (FIPS = 51) has 132 school divisions and approximately 2,100 schools.
#
# ==============================================================================

#' Download raw enrollment data from Urban Institute API
#'
#' Downloads school and division (district) enrollment data for Virginia
#' from the Urban Institute's Education Data Portal.
#'
#' @param end_year School year end (e.g., 2023 for 2022-23 school year)
#' @return List with school and division data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  available_years <- get_available_years()
  if (!end_year %in% available_years) {
    stop(paste0(
      "end_year must be between ", min(available_years), " and ", max(available_years),
      ". Got: ", end_year
    ))
  }

  message(paste("Downloading Virginia enrollment data for", end_year, "..."))

  # Download school-level enrollment data
  message("  Downloading school data...")
  school_data <- download_school_enrollment(end_year)

  # Download district-level enrollment data
  message("  Downloading division (district) data...")
  division_data <- download_district_enrollment(end_year)

  # Download directory data for names
  message("  Downloading directory data for names...")
  school_dir <- download_school_directory(end_year)
  division_dir <- download_district_directory(end_year)

  list(
    school = school_data,
    division = division_data,
    school_dir = school_dir,
    division_dir = division_dir
  )
}


#' Download school-level enrollment data
#'
#' @param end_year School year end
#' @return Data frame with school enrollment
#' @keywords internal
download_school_enrollment <- function(end_year) {

  # Build URL for school enrollment with race and grade breakdowns
  # We need to make separate calls for race and grade disaggregation
  va_fips <- get_va_fips()

  # Get total enrollment by race
  url_race <- build_api_url(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    year = end_year,
    by = "race",
    filters = list(fips = va_fips)
  )

  race_data <- fetch_api_data(url_race)

  # Get enrollment by grade
  url_grade <- build_api_url(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    year = end_year,
    by = "grade",
    filters = list(fips = va_fips)
  )

  grade_data <- fetch_api_data(url_grade)

  # Get enrollment by sex
  url_sex <- build_api_url(
    level = "schools",
    source = "ccd",
    topic = "enrollment",
    year = end_year,
    by = "sex",
    filters = list(fips = va_fips)
  )

  sex_data <- fetch_api_data(url_sex)

  list(
    race = race_data,
    grade = grade_data,
    sex = sex_data
  )
}


#' Download district-level enrollment data
#'
#' @param end_year School year end
#' @return Data frame with district enrollment
#' @keywords internal
download_district_enrollment <- function(end_year) {

  va_fips <- get_va_fips()

  # Get total enrollment by race
  url_race <- build_api_url(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    year = end_year,
    by = "race",
    filters = list(fips = va_fips)
  )

  race_data <- fetch_api_data(url_race)

  # Get enrollment by grade
  url_grade <- build_api_url(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    year = end_year,
    by = "grade",
    filters = list(fips = va_fips)
  )

  grade_data <- fetch_api_data(url_grade)

  # Get enrollment by sex
  url_sex <- build_api_url(
    level = "school-districts",
    source = "ccd",
    topic = "enrollment",
    year = end_year,
    by = "sex",
    filters = list(fips = va_fips)
  )

  sex_data <- fetch_api_data(url_sex)

  list(
    race = race_data,
    grade = grade_data,
    sex = sex_data
  )
}


#' Download school directory data
#'
#' Gets school names, addresses, and other identifying information.
#'
#' @param end_year School year end
#' @return Data frame with school directory info
#' @keywords internal
download_school_directory <- function(end_year) {

  va_fips <- get_va_fips()

  url <- build_api_url(
    level = "schools",
    source = "ccd",
    topic = "directory",
    year = end_year,
    filters = list(fips = va_fips)
  )

  fetch_api_data(url)
}


#' Download district directory data
#'
#' Gets district names, addresses, and other identifying information.
#'
#' @param end_year School year end
#' @return Data frame with district directory info
#' @keywords internal
download_district_directory <- function(end_year) {

  va_fips <- get_va_fips()

  url <- build_api_url(
    level = "school-districts",
    source = "ccd",
    topic = "directory",
    year = end_year,
    filters = list(fips = va_fips)
  )

  fetch_api_data(url)
}
