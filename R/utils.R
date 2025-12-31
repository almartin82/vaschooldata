# ==============================================================================
# Utility Functions
# ==============================================================================
#
# This file contains utility functions for the vaschooldata package.
# Data is sourced from the Virginia Department of Education (VDOE).
#
# ==============================================================================

#' Pipe operator
#'
#' See \code{dplyr::\link[dplyr:reexports]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Get available years of Virginia enrollment data
#'
#' Returns a vector of years for which enrollment data is available
#' from the Virginia Department of Education (VDOE).
#'
#' VDOE provides Fall Membership enrollment data through the School Quality
#' Profiles website. Historical data is available from approximately 2016
#' to the present.
#'
#' Note: VDOE typically releases Fall Membership data in late fall/early winter
#' following the school year start. For example, 2024-25 (end_year=2025) data
#' is usually available by December 2024.
#'
#' @return Integer vector of available years (2016-2025)
#' @export
#' @examples
#' get_available_years()
#'
#' # Check the current range
#' range(get_available_years())
get_available_years <- function() {
  # VDOE School Quality Profiles data available from 2016
  # Years reflect end of school year (2024 = 2023-24 school year)
  # Maximum year updated when new data is released
  2016L:2025L
}


#' Virginia FIPS code
#'
#' @return Integer FIPS code for Virginia (51)
#' @keywords internal
get_va_fips <- function() {
  51L
}


#' Convert to numeric, handling suppression markers
#'
#' Handles various markers for suppressed data and formatting issues.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  if (is.numeric(x)) return(x)

  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers used by VDOE
  x[x %in% c("*", ".", "-", "-1", "-2", "-9", "<5", "<10", "N/A", "NA", "", "PS", "S", "s", "DS")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Format school year string
#'
#' Converts an end year to a school year string.
#'
#' @param end_year School year end (e.g., 2024)
#' @return Character string in format "2023-24"
#' @keywords internal
format_school_year <- function(end_year) {
  paste0(end_year - 1, "-", substr(end_year, 3, 4))
}


#' Format long school year string
#'
#' Converts an end year to a full school year string.
#'
#' @param end_year School year end (e.g., 2024)
#' @return Character string in format "2023-2024"
#' @keywords internal
format_school_year_long <- function(end_year) {
  paste0(end_year - 1, "-", end_year)
}


#' Validate year parameter
#'
#' Checks if the provided year is within the valid range.
#'
#' @param end_year School year end to validate
#' @param min_year Minimum valid year (default from get_available_years())
#' @param max_year Maximum valid year (default from get_available_years())
#' @return NULL (throws error if invalid)
#' @keywords internal
validate_year <- function(end_year, min_year = NULL, max_year = NULL) {
  if (is.null(min_year) || is.null(max_year)) {
    available <- get_available_years()
    min_year <- min(available)
    max_year <- max(available)
  }

  if (!is.numeric(end_year) || length(end_year) != 1) {
    stop("end_year must be a single numeric value")
  }

  if (end_year < min_year || end_year > max_year) {
    stop(paste0(
      "end_year must be between ", min_year, " and ", max_year,
      ". Got: ", end_year,
      "\nUse get_available_years() to see all available years."
    ))
  }

  invisible(NULL)
}


#' Map VDOE race/ethnicity codes to standard names
#'
#' Converts VDOE race/ethnicity categories to standardized names.
#'
#' @param race_name Character race/ethnicity from VDOE data
#' @return Character standardized race name
#' @keywords internal
map_vdoe_race <- function(race_name) {
  # VDOE uses various race category names
  race_map <- c(
    # Standard names
    "White" = "white",
    "Black" = "black",
    "Black or African American" = "black",
    "Hispanic" = "hispanic",
    "Hispanic or Latino" = "hispanic",
    "Asian" = "asian",
    "American Indian" = "native_american",
    "American Indian or Alaska Native" = "native_american",
    "Native Hawaiian" = "pacific_islander",
    "Native Hawaiian or Other Pacific Islander" = "pacific_islander",
    "Two or More Races" = "multiracial",
    "Two or more races" = "multiracial",
    "Multiracial" = "multiracial",
    # Common abbreviations
    "WH" = "white",
    "BL" = "black",
    "HI" = "hispanic",
    "AS" = "asian",
    "AM" = "native_american",
    "PI" = "pacific_islander",
    "HP" = "pacific_islander",
    "TR" = "multiracial",
    "MR" = "multiracial"
  )

  result <- sapply(race_name, function(x) {
    if (x %in% names(race_map)) {
      race_map[x]
    } else {
      tolower(gsub(" ", "_", x))
    }
  })

  as.character(result)
}


#' Map grade codes to standard format
#'
#' Converts VDOE grade codes to standardized format.
#'
#' @param grade_code Character or numeric grade code
#' @return Character grade in standard format (PK, K, 01-12)
#' @keywords internal
map_grade_code <- function(grade_code) {
  grade_map <- c(
    # Standard abbreviations
    "PK" = "PK",
    "KG" = "K",
    "K" = "K",
    # Numeric grades
    "1" = "01", "2" = "02", "3" = "03", "4" = "04",
    "5" = "05", "6" = "06", "7" = "07", "8" = "08",
    "9" = "09", "10" = "10", "11" = "11", "12" = "12",
    # Zero-padded
    "01" = "01", "02" = "02", "03" = "03", "04" = "04",
    "05" = "05", "06" = "06", "07" = "07", "08" = "08",
    "09" = "09",
    # Special grades
    "PG" = "PG",  # Post-Graduate
    "UG" = "UG",  # Ungraded
    "UE" = "UE",  # Ungraded Elementary
    "US" = "US",  # Ungraded Secondary
    "TOTAL" = "TOTAL"
  )

  as.character(grade_code) %>%
    sapply(function(x) {
      if (x %in% names(grade_map)) grade_map[x] else x
    })
}


#' Get Virginia school division codes
#'
#' Returns a named vector of Virginia school division codes and names.
#' Virginia has 132 school divisions (called "divisions" not "districts").
#'
#' @return Named character vector with division codes as names
#' @keywords internal
get_division_codes <- function() {
  # This is a subset of major divisions - full list maintained by VDOE
  # Division codes are typically 3 digits
  c(
    "001" = "Accomack County",
    "002" = "Albemarle County",
    "003" = "Alexandria City",
    "004" = "Alleghany County",
    "005" = "Amelia County",
    "006" = "Amherst County",
    "007" = "Appomattox County",
    "008" = "Arlington County",
    "009" = "Augusta County",
    "010" = "Bath County",
    # ... additional divisions
    "090" = "Fairfax County",
    "091" = "Falls Church City",
    "107" = "Loudoun County",
    "126" = "Prince William County",
    "131" = "Virginia Beach City"
    # Full list available from VDOE
  )
}
