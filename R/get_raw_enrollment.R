# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from the
# Virginia Department of Education (VDOE).
#
# Data comes from VDOE's School Quality Profiles website:
# https://schoolquality.virginia.gov/download-data
#
# VDOE collects Fall Membership enrollment data annually as of September 30.
# Data includes enrollment by school and division with demographics, grade
# levels, and special population breakdowns.
#
# Virginia has 132 school divisions and approximately 2,100 schools.
#
# Data availability: 2016-present (approx. 9 years of historical data)
#
# ==============================================================================

#' Download raw enrollment data from Virginia DOE
#'
#' Downloads school and division enrollment data from the Virginia Department
#' of Education's School Quality Profiles website.
#'
#' @param end_year School year end (e.g., 2024 for 2023-24 school year)
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

  # Try School Quality Profiles first (primary source)
  result <- tryCatch({
    download_sqp_enrollment(end_year)
  }, error = function(e) {
    message(paste("  School Quality Profiles download failed:", e$message))
    NULL
  })

  if (is.null(result)) {
    stop(paste(
      "Failed to download Virginia enrollment data for year", end_year,
      "\nPlease check your internet connection and try again.",
      "\nIf the problem persists, VDOE data may be temporarily unavailable."
    ))
  }

  result
}


#' Download enrollment from School Quality Profiles
#'
#' Downloads enrollment data from VDOE's School Quality Profiles website.
#' The site provides CSV downloads of Fall Membership data.
#'
#' @param end_year School year end
#' @return List with school and division data frames
#' @keywords internal
download_sqp_enrollment <- function(end_year) {

  message("  Downloading from VDOE School Quality Profiles...")

  # Build school year string (e.g., "2023-2024" for end_year 2024)
  school_year <- paste0(end_year - 1, "-", end_year)

  # Download school-level enrollment data
  message("    Downloading school enrollment data...")
  school_data <- download_sqp_file(end_year, level = "school")

  # Download division-level enrollment data
  message("    Downloading division enrollment data...")
  division_data <- download_sqp_file(end_year, level = "division")

  list(
    school = school_data,
    division = division_data
  )
}


#' Download a specific SQP enrollment file
#'
#' @param end_year School year end
#' @param level "school" or "division"
#' @return Data frame with enrollment data
#' @keywords internal
download_sqp_file <- function(end_year, level) {

  # VDOE School Quality Profiles URL patterns
  # The exact URL pattern may need adjustment based on the actual website structure
  # These URLs are constructed based on common VDOE patterns

  # Build school year identifier
  school_year <- paste0(end_year - 1, "-", end_year)
  sy_short <- paste0(end_year - 1, end_year)

  # Try multiple URL patterns since VDOE may change structure
  url_patterns <- list(
    # Pattern 1: Direct enrollment download endpoint
    sqp_enrollment = paste0(
      "https://schoolquality.virginia.gov/api/export/enrollment/",
      school_year, "/", level, ".csv"
    ),
    # Pattern 2: Data export with year parameter
    sqp_export = paste0(
      "https://schoolquality.virginia.gov/export/",
      level, "_enrollment_", sy_short, ".csv"
    ),
    # Pattern 3: Fall membership from VDOE reports
    vdoe_fm = paste0(
      "https://www.doe.virginia.gov/home/showpublisheddocument/",
      get_vdoe_doc_id(end_year, level)
    )
  )

  # Create temp file

  temp_file <- tempfile(fileext = ".csv")

  df <- NULL

  # Try each URL pattern
  for (pattern_name in names(url_patterns)) {
    url <- url_patterns[[pattern_name]]

    # Skip if URL construction failed
    if (is.null(url) || is.na(url)) next

    result <- tryCatch({
      message(paste("      Trying", pattern_name, "..."))

      response <- httr::GET(
        url,
        httr::write_disk(temp_file, overwrite = TRUE),
        httr::timeout(120),
        httr::user_agent("vaschooldata R package")
      )

      if (!httr::http_error(response)) {
        # Check content type
        content_type <- httr::headers(response)$`content-type`

        if (!is.null(content_type) && grepl("text/csv|application/csv|text/plain", content_type)) {
          # Read CSV
          df <- readr::read_csv(
            temp_file,
            col_types = readr::cols(.default = readr::col_character()),
            show_col_types = FALSE
          )

          if (nrow(df) > 0) {
            message(paste("      Success:", nrow(df), "records"))
            return(df)
          }
        }
      }
      NULL
    }, error = function(e) {
      NULL
    })

    if (!is.null(result)) {
      df <- result
      break
    }
  }

  # Clean up temp file
  if (file.exists(temp_file)) unlink(temp_file)

  # If standard downloads failed, try alternate approach
  if (is.null(df) || nrow(df) == 0) {
    message("      Standard downloads failed, trying Build-A-Table export...")
    df <- download_buildatable_export(end_year, level)
  }

  if (is.null(df) || nrow(df) == 0) {
    stop(paste("Could not download", level, "enrollment data for year", end_year))
  }

  df
}


#' Get VDOE document ID for Fall Membership files
#'
#' Returns the document ID for VDOE's published Excel files.
#' These IDs may need to be updated as VDOE changes their CMS.
#'
#' @param end_year School year end
#' @param level "school" or "division"
#' @return Character string document ID, or NA if not found
#' @keywords internal
get_vdoe_doc_id <- function(end_year, level) {
  # Document IDs for VDOE Fall Membership Excel files
  # These are placeholders - actual IDs need to be discovered from the website
  # Format: https://www.doe.virginia.gov/home/showpublisheddocument/[ID]

  # This lookup table maps years and levels to document IDs
  # These would need to be updated when new data is released
  doc_ids <- list(
    "2025" = list(school = NA, division = NA),
    "2024" = list(school = NA, division = NA),
    "2023" = list(school = NA, division = NA),
    "2022" = list(school = NA, division = NA),
    "2021" = list(school = NA, division = NA),
    "2020" = list(school = NA, division = NA),
    "2019" = list(school = NA, division = NA),
    "2018" = list(school = NA, division = NA),
    "2017" = list(school = NA, division = NA),
    "2016" = list(school = NA, division = NA)
  )

  year_str <- as.character(end_year)
  if (year_str %in% names(doc_ids)) {
    return(doc_ids[[year_str]][[level]])
  }

  NA
}


#' Download from Build-A-Table export
#'
#' Attempts to download data via VDOE's Build-A-Table tool.
#' This is a fallback method that may require browser automation.
#'
#' @param end_year School year end
#' @param level "school" or "division"
#' @return Data frame with enrollment data
#' @keywords internal
download_buildatable_export <- function(end_year, level) {

  # Build-A-Table requires interactive selection and CAPTCHA

  # This function constructs the request but may not work without session

  base_url <- "https://p1pe.doe.virginia.gov/buildatable/fallmembership"

  # Build school year string
  school_year <- paste0(end_year - 1, "-", substr(end_year, 3, 4))

  # Try to access the Build-A-Table export endpoint
  # Note: This may fail due to CAPTCHA requirements

  tryCatch({
    # This is a best-effort approach - the actual endpoint may require authentication
    session_response <- httr::GET(
      base_url,
      httr::timeout(30),
      httr::user_agent("vaschooldata R package")
    )

    # If we got a login/captcha page, we cannot proceed programmatically
    if (httr::http_error(session_response)) {
      message("      Build-A-Table requires interactive session")
      return(NULL)
    }

    content_type <- httr::headers(session_response)$`content-type`
    if (!is.null(content_type) && grepl("text/html", content_type)) {
      message("      Build-A-Table returned HTML (CAPTCHA required)")
      return(NULL)
    }

    NULL

  }, error = function(e) {
    message(paste("      Build-A-Table error:", e$message))
    NULL
  })
}


#' Import local Fall Membership file
#'
#' Imports enrollment data from a locally downloaded Fall Membership file.
#' Use this function if automatic download fails due to CAPTCHA or
#' other restrictions.
#'
#' @param file_path Path to the downloaded Excel or CSV file
#' @param end_year School year end (e.g., 2024 for 2023-24)
#' @return List with school and division data frames
#' @export
#' @examples
#' \dontrun{
#' # Download the Fall Membership file manually from:
#' # https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics
#' # Then import it:
#' raw_data <- import_local_fm("path/to/fall_membership_2024.xlsx", 2024)
#' }
import_local_fm <- function(file_path, end_year) {

  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }

  # Determine file type
  file_ext <- tolower(tools::file_ext(file_path))

  if (file_ext %in% c("xlsx", "xls")) {
    # Read Excel file
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files. Install with: install.packages('readxl')")
    }

    # Get sheet names
    sheets <- readxl::excel_sheets(file_path)

    # Look for school and division sheets
    school_sheet <- sheets[grep("school|campus", sheets, ignore.case = TRUE)]
    division_sheet <- sheets[grep("division|district|lea", sheets, ignore.case = TRUE)]

    # Read sheets
    school_data <- if (length(school_sheet) > 0) {
      readxl::read_excel(file_path, sheet = school_sheet[1])
    } else {
      # Try first sheet if no specific sheet found
      readxl::read_excel(file_path, sheet = 1)
    }

    division_data <- if (length(division_sheet) > 0) {
      readxl::read_excel(file_path, sheet = division_sheet[1])
    } else {
      NULL
    }

    # Convert to character to match download format
    school_data <- as.data.frame(lapply(school_data, as.character))
    if (!is.null(division_data)) {
      division_data <- as.data.frame(lapply(division_data, as.character))
    }

  } else if (file_ext == "csv") {
    # Read CSV file
    school_data <- readr::read_csv(
      file_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
    division_data <- NULL

  } else {
    stop(paste("Unsupported file type:", file_ext, ". Use .xlsx, .xls, or .csv"))
  }

  # Add end_year if not present
  if (!"end_year" %in% names(school_data)) {
    school_data$end_year <- as.character(end_year)
  }
  if (!is.null(division_data) && !"end_year" %in% names(division_data)) {
    division_data$end_year <- as.character(end_year)
  }

  list(
    school = school_data,
    division = division_data
  )
}
