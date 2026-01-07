# ==============================================================================
# Raw Graduation Rate Data Download
# ==============================================================================
#
# This file contains functions for downloading raw graduation rate data from
# the Virginia Department of Education (VDOE) Open Data Portal.
#
# Data is sourced from CKAN API CSV downloads.
#
# ==============================================================================

#' Get graduation data download URL
#'
#' Returns the CKAN download URL for a given graduation year.
#'
#' @param end_year School year end (e.g., 2023 for 2022-23 school year)
#' @return Character URL to CSV file
#' @keywords internal
get_graduation_url <- function(end_year) {
  # Hardcoded URLs for stability (from research document)
  urls <- list(
    "2019" = "https://data.virginia.gov/dataset/89179d05-0009-4a08-a8bd-4f31ee47ed4a/resource/209a1d06-8d24-47e5-9bea-d64f6e7b6406/download/cohort_statistics-8.csv",
    "2020" = "https://data.virginia.gov/dataset/c13dbcbb-92fd-4335-9f41-9d6f41b084eb/resource/19b88afb-e16d-450d-80fe-9dd11fa87f0d/download/cohort_statistics-7.csv",
    "2021" = "https://data.virginia.gov/dataset/d2f4192e-a978-44ea-b6e9-c7fcdba2272f/resource/99a342e4-4e83-40f1-9b51-887a5c7e434f/download/cohort_statistics-6.csv",
    "2022" = "https://data.virginia.gov/dataset/92e9facf-079a-4af9-9890-6020db3b527e/resource/49810d27-f16d-42f1-9577-21b173713204/download/cohort_statistics-5.csv",
    "2023" = "https://data.virginia.gov/dataset/554735ce-fd3e-4077-af7f-868c32e51edf/resource/ccf203ad-862d-4522-8e9c-ab50de579ce7/download/cohort_statistics-4.csv"
  )

  year_key <- as.character(end_year)

  if (!year_key %in% names(urls)) {
    stop("Graduation data not available for ", end_year,
         "\nAvailable years: ", paste(names(urls), collapse = ", "))
  }

  urls[[year_key]]
}


#' Download raw graduation rate data
#'
#' Downloads graduation rate CSV from VDOE Open Data Portal and returns it as
#' a data frame with minimal processing.
#'
#' @param end_year School year end (e.g., 2023 for 2022-23 school year)
#' @param cache_dir Directory to cache downloaded files
#' @return Data frame with raw graduation data as provided by VDOE
#' @keywords internal
get_raw_graduation <- function(end_year, cache_dir = get_cache_dir()) {

  # Get URL
  url <- get_graduation_url(end_year)

  # Build cache file path
  filename <- paste0("grad_raw_", end_year, ".csv")
  cache_path <- file.path(cache_dir, filename)

  # Check if already downloaded
  if (file.exists(cache_path)) {
    message(paste("Using cached raw file for", end_year))
    raw <- readr::read_csv(cache_path, show_col_types = FALSE)
  } else {
    # Download file
    message(paste("Downloading graduation data for", end_year, "..."))

    response <- httr::GET(
      url,
      httr::write_disk(cache_path, overwrite = TRUE),
      httr::timeout(60)
    )

    if (httr::http_error(response)) {
      stop("Failed to download graduation data for ", end_year,
           "\nHTTP ", httr::status_code(response),
           "\nURL: ", url)
    }

    # Read CSV
    raw <- readr::read_csv(cache_path, show_col_types = FALSE)
  }

  # Remove blank rows (data quality issue - some CSVs have completely NA rows)
  if ("Cohort Year" %in% names(raw)) {
    raw <- raw[!is.na(raw[["Cohort Year"]]), ]
  }

  # Add year column for reference
  raw$end_year <- end_year

  raw
}


#' Get available years of graduation data
#'
#' Returns a vector of years for which graduation rate data is available
#' from the Virginia Department of Education (VDOE).
#'
#' VDOE provides Cohort Graduation and Dropout Report data through the
#' Open Data Portal. Historical data is available from 2019 to present.
#'
#' @return Integer vector of available years (2019-2023)
#' @export
#' @examples
#' get_available_grad_years()
#'
#' # Check the current range
#' range(get_available_grad_years())
get_available_grad_years <- function() {
  # VDOE Cohort Graduation data available from 2019
  # Years reflect end of school year (2023 = 2022-23 school year)
  2019L:2023L
}
