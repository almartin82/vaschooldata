# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# Virginia Department of Education (VDOE).
#
# Data is sourced directly from the VDOE directory website:
# https://www.va-doeapp.com/Download.aspx?n=School+Listing
#
# The school directory includes school names, addresses, phone numbers,
# principal names, and other identifying information.
#
# ==============================================================================

#' Fetch Virginia school directory data
#'
#' Downloads and processes school directory data from the Virginia Department of
#' Education (VDOE). Data includes school names, addresses, phone numbers, and
#' principal information.
#'
#' @param end_year Not currently used. VDOE provides a single current snapshot.
#'   Included for API consistency with other fetch functions.
#' @param tidy If TRUE (default), returns data in a clean format with standardized
#'   column names. If FALSE, returns data closer to the original source format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from VDOE.
#' @return Data frame with school directory information including:
#'   \describe{
#'     \item{division_id}{3-digit division (district) identifier}
#'     \item{school_id}{4-digit school identifier}
#'     \item{nces_id}{NCES school identifier (12-digit)}
#'     \item{division_name}{Name of the school division}
#'     \item{school_name}{Name of the school}
#'     \item{principal_name}{Name of the school principal}
#'     \item{address}{Street address line 1}
#'     \item{address2}{Street address line 2 (if any)}
#'     \item{city}{City}
#'     \item{state}{State (VA)}
#'     \item{zip}{ZIP code}
#'     \item{phone}{Phone number}
#'     \item{low_grade}{Lowest grade served}
#'     \item{high_grade}{Highest grade served}
#'     \item{school_type}{School type (e.g., "Public school - Regular")}
#'     \item{grade_level}{Grade level category (Elementary, Middle, High, etc.)}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Get school directory
#' directory <- fetch_directory()
#'
#' # Get raw format
#' directory_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' directory_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to specific division
#' fairfax <- directory |>
#'   dplyr::filter(division_name == "Fairfax County Public Schools")
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "directory_tidy" else "directory_raw"

  # Check cache first (use current year as proxy for cache validation)
  cache_year <- as.integer(format(Sys.Date(), "%Y"))

  if (use_cache && dir_cache_exists(cache_type)) {
    message("Using cached directory data")
    return(read_dir_cache(cache_type))
  }


  # Get raw data from VDOE
  raw <- get_raw_directory()

  # Process to standard schema
  if (tidy) {
    processed <- process_directory(raw)
  } else {
    processed <- raw
  }

  # Cache the result
  if (use_cache) {
    write_dir_cache(processed, cache_type)
  }

  processed
}


#' Download raw school directory data from Virginia DOE
#'
#' Downloads school directory data from the Virginia Department of Education's
#' directory website.
#'
#' @return Data frame with raw directory data
#' @keywords internal
get_raw_directory <- function() {

  message("Downloading Virginia school directory data...")

  # VDOE directory download URL
  url <- "https://www.va-doeapp.com/Download.aspx?n=School+Listing"

  temp_file <- tempfile(fileext = ".csv")

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(120),
      httr::user_agent("vaschooldata R package")
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check content type
    content_type <- httr::headers(response)$`content-type`

    # VDOE returns Excel MIME type but CSV content
    if (!grepl("excel|csv|text", content_type, ignore.case = TRUE)) {
      stop(paste("Unexpected content type:", content_type))
    }

    # Read CSV, skipping the first 2 header rows (title lines)
    df <- readr::read_csv(
      temp_file,
      skip = 2,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )

    message(paste("  Downloaded", nrow(df), "school records"))

    # Clean up temp file
    if (file.exists(temp_file)) unlink(temp_file)

    df

  }, error = function(e) {
    if (file.exists(temp_file)) unlink(temp_file)
    stop(paste(
      "Failed to download Virginia school directory data:",
      e$message,
      "\nPlease check your internet connection and try again.",
      "\nIf the problem persists, VDOE data may be temporarily unavailable."
    ))
  })
}


#' Process raw directory data
#'
#' Transforms raw VDOE directory data into a standardized schema with
#' clean column names.
#'
#' @param raw_data Data frame from get_raw_directory
#' @return Processed data frame with standardized columns
#' @keywords internal
process_directory <- function(raw_data) {

  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(create_empty_directory_df())
  }

  # Standardize column names - remove extra spaces and convert to lowercase
  names(raw_data) <- tolower(trimws(gsub("\\s+", "_", names(raw_data))))

  # Build result with standardized column names
  result <- data.frame(
    division_id = clean_id(raw_data$division__num),
    school_id = clean_id(raw_data$school__num),
    nces_id = raw_data$nces_school_num,
    division_name = trimws(raw_data$division_name),
    school_name = trimws(raw_data$school_name),
    principal_name = clean_principal_name(raw_data$principal),
    address = trimws(raw_data$address1),
    address2 = trimws(raw_data$address2),
    city = trimws(raw_data$city),
    state = trimws(raw_data$state),
    zip = trimws(raw_data$zip),
    phone = clean_phone(raw_data$phone__number),
    low_grade = raw_data$low__grades,
    high_grade = raw_data$high__grades,
    school_type = trimws(raw_data$school_description),
    grade_level = trimws(raw_data$grade__standard),
    schedule = trimws(raw_data$schedule),
    division_type = trimws(raw_data$division__description),
    membership_status = trimws(raw_data$fall__membership__schools),
    stringsAsFactors = FALSE
  )

  # Filter out non-school entries (like homebound, jail, etc.) if desired
  # Keep all entries for now to let users filter as needed

  result
}


#' Clean ID values
#'
#' Removes extra whitespace and ensures IDs are character type.
#'
#' @param x Vector of ID values
#' @return Character vector of cleaned IDs
#' @keywords internal
clean_id <- function(x) {
  x <- trimws(as.character(x))
  # Pad division IDs to 3 digits and school IDs to 4 digits if needed
  x
}


#' Clean principal name
#'
#' Cleans up principal name formatting.
#'
#' @param x Vector of principal names
#' @return Character vector of cleaned names
#' @keywords internal
clean_principal_name <- function(x) {
  x <- trimws(as.character(x))
  # Remove extra spaces between name parts
  x <- gsub("\\s+", " ", x)
  x
}


#' Clean phone number
#'
#' Standardizes phone number formatting.
#'
#' @param x Vector of phone numbers
#' @return Character vector of cleaned phone numbers
#' @keywords internal
clean_phone <- function(x) {
  trimws(as.character(x))
}


#' Create empty directory data frame
#'
#' @return Empty data frame with expected columns
#' @keywords internal
create_empty_directory_df <- function() {
  data.frame(
    division_id = character(),
    school_id = character(),
    nces_id = character(),
    division_name = character(),
    school_name = character(),
    principal_name = character(),
    address = character(),
    address2 = character(),
    city = character(),
    state = character(),
    zip = character(),
    phone = character(),
    low_grade = character(),
    high_grade = character(),
    school_type = character(),
    grade_level = character(),
    schedule = character(),
    division_type = character(),
    membership_status = character(),
    stringsAsFactors = FALSE
  )
}


# ==============================================================================
# Directory-specific caching functions
# ==============================================================================

#' Get directory cache file path
#'
#' @param type Cache type ("directory_tidy" or "directory_raw")
#' @return Full path to cache file
#' @keywords internal
get_dir_cache_path <- function(type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(type, ".rds"))
}


#' Check if directory cached data exists and is valid
#'
#' @param type Cache type ("directory_tidy" or "directory_raw")
#' @param max_age Maximum age in days (default 7 for directory data)
#' @return TRUE if valid cache exists
#' @keywords internal
dir_cache_exists <- function(type, max_age = 7) {
  cache_path <- get_dir_cache_path(type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age - directory data updated less frequently
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param type Cache type ("directory_tidy" or "directory_raw")
#' @return Cached data frame
#' @keywords internal
read_dir_cache <- function(type) {
  cache_path <- get_dir_cache_path(type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param df Data frame to cache
#' @param type Cache type ("directory_tidy" or "directory_raw")
#' @return Invisibly returns the cache path
#' @keywords internal
write_dir_cache <- function(df, type) {
  cache_path <- get_dir_cache_path(type)
  saveRDS(df, cache_path)
  invisible(cache_path)
}


#' Clear directory cache
#'
#' Removes cached directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear directory cache
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()
  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
