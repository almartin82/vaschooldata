# ==============================================================================
# Utility Functions
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
#' through the Urban Institute's Education Data Portal.
#'
#' @return Integer vector of available years (1987-2023)
#' @export
#' @examples
#' get_available_years()
get_available_years <- function() {
  # CCD data available 1986-2023, but Virginia data starts 1987
  1987L:2023L
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

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "-2", "-9", "<5", "N/A", "NA", "", "PS")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Build API URL for Urban Institute Education Data Portal
#'
#' Constructs the URL for querying the Education Data Portal API.
#'
#' @param level Data level ("schools" or "school-districts")
#' @param source Data source ("ccd")
#' @param topic Data topic ("enrollment" or "directory")
#' @param year School year end (e.g., 2023 for 2022-23)
#' @param filters List of filter parameters
#' @param by Optional disaggregation (e.g., "race", "sex", "grade")
#' @return Character string URL
#' @keywords internal
build_api_url <- function(level, source, topic, year = NULL, filters = NULL, by = NULL) {

  base_url <- "https://educationdata.urban.org/api/v1"

  # Build path components
  path_parts <- c(level, source, topic)

  if (!is.null(year)) {
    path_parts <- c(path_parts, year)
  }

  if (!is.null(by)) {
    path_parts <- c(path_parts, by)
  }

  url <- paste(c(base_url, path_parts), collapse = "/")

  # Add query parameters
  if (!is.null(filters) && length(filters) > 0) {
    query_parts <- sapply(names(filters), function(name) {
      paste0(name, "=", filters[[name]])
    })
    url <- paste0(url, "/?", paste(query_parts, collapse = "&"))
  }

  url
}


#' Make API request with pagination handling
#'
#' Fetches all pages of results from the Education Data Portal API.
#'
#' @param url Base URL for the API request
#' @param timeout Request timeout in seconds
#' @return Data frame with all results
#' @keywords internal
fetch_api_data <- function(url, timeout = 300) {

  all_results <- list()
  page <- 1
  has_more <- TRUE

  while (has_more) {
    # Add page parameter
    page_url <- if (grepl("\\?", url)) {
      paste0(url, "&page=", page)
    } else {
      paste0(url, "?page=", page)
    }

    message(paste("  Fetching page", page, "..."))

    response <- tryCatch({
      httr::GET(
        page_url,
        httr::timeout(timeout),
        httr::add_headers(Accept = "application/json")
      )
    }, error = function(e) {
      stop(paste("Network error:", e$message))
    })

    if (httr::http_error(response)) {
      stop(paste("API error:", httr::status_code(response),
                 httr::content(response, "text", encoding = "UTF-8")))
    }

    content <- httr::content(response, "text", encoding = "UTF-8")
    parsed <- jsonlite::fromJSON(content, flatten = TRUE)

    # Check if we have results
    if (is.null(parsed$results) || length(parsed$results) == 0) {
      has_more <- FALSE
    } else {
      all_results[[page]] <- parsed$results

      # Check for next page
      has_more <- !is.null(parsed$`next`) && parsed$`next` != ""
      page <- page + 1

      # Safety limit
      if (page > 100) {
        warning("Reached page limit (100). Some data may be missing.")
        has_more <- FALSE
      }
    }
  }

  if (length(all_results) == 0) {
    return(data.frame())
  }

  dplyr::bind_rows(all_results)
}


#' Map race codes to standard names
#'
#' Converts Urban Institute race codes to standardized names.
#'
#' @param race_code Integer race code from API
#' @return Character race name
#' @keywords internal
map_race_code <- function(race_code) {
  race_map <- c(
    "1" = "white",
    "2" = "black",
    "3" = "hispanic",
    "4" = "asian",
    "5" = "native_american",
    "6" = "pacific_islander",
    "7" = "multiracial",
    "8" = "unknown",
    "9" = "total",
    "20" = "other",
    "99" = "total"
  )

  as.character(race_code) %>%
    sapply(function(x) {
      if (x %in% names(race_map)) race_map[x] else "unknown"
    })
}


#' Map grade codes to standard format
#'
#' Converts Urban Institute grade codes to standardized format.
#'
#' @param grade_code Integer or character grade code from API
#' @return Character grade in standard format (PK, K, 01-12)
#' @keywords internal
map_grade_code <- function(grade_code) {
  grade_map <- c(
    "-1" = "PK",
    "-2" = "PK",  # Pre-K special
    "0" = "K",
    "1" = "01", "2" = "02", "3" = "03", "4" = "04",
    "5" = "05", "6" = "06", "7" = "07", "8" = "08",
    "9" = "09", "10" = "10", "11" = "11", "12" = "12",
    "13" = "UG",  # Ungraded
    "14" = "AE",  # Adult education
    "15" = "UG",  # Ungraded secondary
    "99" = "TOTAL"
  )

  as.character(grade_code) %>%
    sapply(function(x) {
      if (x %in% names(grade_map)) grade_map[x] else x
    })
}
