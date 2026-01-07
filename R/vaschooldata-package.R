#' vaschooldata: Fetch and Process Virginia School Data
#'
#' Downloads and processes school enrollment data from the Virginia Department
#' of Education (VDOE). Provides functions for fetching Fall Membership
#' enrollment data directly from VDOE and transforming it into tidy format
#' for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Fetch enrollment data for a school year}
#'   \item{\code{\link{fetch_enr_multi}}}{Fetch enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Transform wide data to tidy (long) format}
#'   \item{\code{\link{id_enr_aggs}}}{Add aggregation level flags}
#'   \item{\code{\link{enr_grade_aggs}}}{Create grade-level aggregations}
#'   \item{\code{\link{get_available_years}}}{Get list of available data years}
#' }
#'
#' @section Cache functions:
#' \describe{
#'   \item{\code{\link{cache_status}}}{View cached data files}
#'   \item{\code{\link{clear_cache}}}{Remove cached data files}
#' }
#'
#' @section ID System:
#' Virginia uses a hierarchical ID system:
#' \itemize{
#'   \item Division IDs: 7 characters (e.g., 5100180 = Alexandria City)
#'   \item School IDs: 12 characters (division ID + 5-digit school number)
#' }
#'
#' Virginia has 132 school divisions (equivalent to districts in other states).
#'
#' @section Data Sources:
#' Data is sourced exclusively from:
#' \itemize{
#'   \item VDOE Fall Membership: \url{https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics}
#' }
#'
#' Note: This package does NOT use federal data sources (NCES, Urban Institute, etc.).
#'
#' @section Data Availability:
#' \itemize{
#'   \item Years: 1987-2023 (37 years)
#'   \item Aggregation levels: Division (District), School
#'   \item Demographics: Race/ethnicity (varies by era), Sex
#'   \item Grade levels: PK through 12, plus ungraded
#' }
#'
#' @section Format Eras:
#' \describe{
#'   \item{Pre-1998 (1987-1997)}{Limited demographics; 5 race categories}
#'   \item{5-Race Era (1998-2010)}{White, Black, Hispanic, Asian/Pacific Islander, American Indian}
#'   \item{7-Race Era (2011-2023)}{Added Pacific Islander (separate), Two or More Races}
#' }
#'
#' @docType package
#' @name vaschooldata-package
#' @aliases vaschooldata
#' @keywords internal
"_PACKAGE"

# Import pipe operator for use in internal functions
#' @importFrom magrittr `%>%`
NULL

# Global variables for NSE (non-standard evaluation)
# These are used in dplyr/tidyr operations
utils::globalVariables(c(
  ".data",
  "all_of",
  "cohort_size",
  "completion_rate",
  "diploma_count",
  "diploma_type",
  "division_name",
  "division_number",
  "dropout_rate",
  "dropouts",
  "end_year",
  "graduation_rate",
  "is_district",
  "is_school",
  "is_state",
  "level",
  "long_term_absence",
  "rate_type",
  "school_name",
  "school_number",
  "still_enrolled",
  "total_graduates"
))

