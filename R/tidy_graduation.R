# ==============================================================================
# Graduation Rate Data Tidy Transformation
# ==============================================================================
#
# This file contains functions for converting processed graduation data to
# long (tidy) format.
#
# Transforms wide-format diploma counts into long format with diploma_type
# and diploma_count columns.
#
# ==============================================================================

#' Tidy graduation rate data
#'
#' Converts processed graduation data from wide to long (tidy) format.
#' Pivots diploma type columns into rows.
#'
#' @param processed_data Processed data from process_graduation()
#' @return Long-format tibble with diploma_type column
#' @keywords internal
tidy_graduation <- function(processed_data) {

  # Define diploma type columns to pivot
  diploma_cols <- c("advanced_studies", "ib_diploma", "standard_diploma",
                    "other_diplomas", "applied_studies", "ged", "isaep",
                    "certificate")

  # Check which diploma columns exist
  existing_diploma_cols <- intersect(diploma_cols, names(processed_data))

  # Pivot longer
  if (length(existing_diploma_cols) > 0) {
    tidy <- processed_data %>%
      tidyr::pivot_longer(
        cols = dplyr::all_of(existing_diploma_cols),
        names_to = "diploma_type",
        values_to = "diploma_count",
        values_drop_na = FALSE
      ) %>%
      dplyr::mutate(
        # Clean up diploma_type names (remove _diploma suffix)
        diploma_type = gsub("_diploma$", "", diploma_type)
      )
  } else {
    # No diploma columns, add empty columns
    tidy <- processed_data
    tidy$diploma_type <- NA_character_
    tidy$diploma_count <- NA_integer_
  }

  # Ensure is_state, is_district, is_school are logical
  tidy$is_state <- as.logical(tidy$is_state)
  tidy$is_district <- as.logical(tidy$is_district)
  tidy$is_school <- as.logical(tidy$is_school)

  # Ensure end_year is integer
  tidy$end_year <- as.integer(tidy$end_year)

  # Remove level column (not needed in tidy format)
  tidy$level <- NULL

  # Remove era column (not needed in tidy format)
  tidy$era <- NULL

  # Ensure consistent column order
  col_order <- c(
    "end_year",
    "division_number",
    "division_name",
    "school_number",
    "school_name",
    "rate_type",
    "cohort_size",
    "total_graduates",
    "graduation_rate",
    "completion_rate",
    "dropout_rate",
    "diploma_type",
    "diploma_count",
    "dropouts",
    "still_enrolled",
    "long_term_absence",
    "is_state",
    "is_district",
    "is_school"
  )

  # Keep only columns that exist
  existing_cols <- intersect(col_order, names(tidy))

  tidy[, existing_cols]
}


#' Calculate state aggregates (for v1 data)
#'
#' For v1 era (2019-2022), calculates state-level aggregates by summing
#' all school-level records.
#'
#' @param tidy_data Tidy graduation data
#' @return Data frame with added state aggregate records
#' @keywords internal
calc_state_aggregates <- function(tidy_data) {

  # Check if state records already exist
  if (any(tidy_data$is_state)) {
    return(tidy_data)
  }

  # Calculate state totals
  # First, get one row per entity with aggregate values
  entity_totals <- tidy_data %>%
    dplyr::filter(is_school) %>%
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
      long_term_absence
    )

  # Now sum to state level
  # We need to aggregate for each diploma_type
  diploma_types <- unique(tidy_data$diploma_type)

  state_list <- lapply(diploma_types, function(dt) {
    entity_totals %>%
      dplyr::mutate(diploma_type = dt, diploma_count = NA_integer_)
  })

  state_totals <- dplyr::bind_rows(state_list) %>%
    dplyr::group_by(end_year, rate_type, diploma_type) %>%
    dplyr::summarise(
      cohort_size = sum(cohort_size, na.rm = TRUE),
      total_graduates = sum(total_graduates, na.rm = TRUE),
      dropouts = sum(dropouts, na.rm = TRUE),
      still_enrolled = sum(still_enrolled, na.rm = TRUE),
      long_term_absence = sum(long_term_absence, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Calculate rates from aggregates
      graduation_rate = total_graduates / cohort_size,
      completion_rate = graduation_rate,  # Use same rate (v1 doesn't have separate completion rate in source)
      dropout_rate = dropouts / cohort_size,
      # Set entity indicators
      division_number = NA_character_,
      division_name = NA_character_,
      school_number = NA_character_,
      school_name = "Virginia",
      is_state = TRUE,
      is_school = FALSE,
      is_district = FALSE
    )

  # Combine with original data
  dplyr::bind_rows(tidy_data, state_totals)
}
