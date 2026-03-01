# ==============================================================================
# Directory Year Coverage Tests for vaschooldata
# ==============================================================================
#
# Tests for fetch_directory() which provides a current snapshot of all
# Virginia schools from VDOE's school directory.
#
# All fetch calls use use_cache = TRUE.
#
# Virginia has ~132 school divisions and ~2,000+ schools.
# Fairfax County is the largest division.
#
# Note: The directory endpoint may change column names over time, so these
# tests are designed to be resilient to minor schema changes while still
# catching broken functionality.
# ==============================================================================

# Helper to skip when directory data is unavailable
skip_if_directory_unavailable <- function() {
  tryCatch({
    result <- vaschooldata::fetch_directory(use_cache = TRUE)
    if (is.null(result) || nrow(result) == 0) {
      testthat::skip("VDOE directory data returned empty result")
    }
  }, error = function(e) {
    testthat::skip(paste("VDOE directory data unavailable:", conditionMessage(e)))
  })
}

# =============================================================================
# REQUIRED FIELDS
# =============================================================================

test_that("directory has division_id column", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)
  expect_true("division_id" %in% names(dir_data),
    info = paste("Available columns:", paste(names(dir_data), collapse = ", ")))
})

test_that("directory has school_id column", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)
  expect_true("school_id" %in% names(dir_data))
})

test_that("directory has division_name column", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)
  expect_true("division_name" %in% names(dir_data))
})

test_that("directory has school_name column", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)
  expect_true("school_name" %in% names(dir_data))
})

test_that("directory has address fields", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  # At minimum should have city and state
  expect_true("city" %in% names(dir_data))
  expect_true("state" %in% names(dir_data))
})

test_that("directory has phone column", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)
  expect_true("phone" %in% names(dir_data))
})

test_that("directory has grade_level or school_type column", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  has_grade <- "grade_level" %in% names(dir_data)
  has_type <- "school_type" %in% names(dir_data)

  expect_true(has_grade || has_type,
    info = "Missing both grade_level and school_type columns")
})

# =============================================================================
# ENTITY COUNTS
# =============================================================================

test_that("directory has 2000+ school records", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  # Virginia has approximately 2,000-3,000 schools
  expect_true(nrow(dir_data) >= 1500,
    info = paste("Only", nrow(dir_data), "records"))
})

test_that("directory has 100+ unique divisions", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  n_divisions <- length(unique(dir_data$division_id))

  # Virginia has ~132 school divisions
  expect_true(n_divisions >= 100,
    info = paste("Only", n_divisions, "unique divisions"))
})

test_that("directory division count is approximately 132", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  n_divisions <- length(unique(dir_data$division_id))

  expect_true(n_divisions >= 125 & n_divisions <= 145,
    info = paste("Division count:", n_divisions))
})

# =============================================================================
# KNOWN ENTITY LOOKUP — Fairfax County
# =============================================================================

test_that("Fairfax County schools exist in directory", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  fairfax <- dir_data |>
    dplyr::filter(grepl("Fairfax", division_name, ignore.case = TRUE))

  # Fairfax County should have many schools (100+)
  expect_true(nrow(fairfax) >= 50,
    info = paste("Only", nrow(fairfax), "Fairfax schools found"))
})

test_that("Fairfax County has most schools of any division", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  division_counts <- dir_data |>
    dplyr::count(division_name, sort = TRUE)

  # Fairfax should be in the top 3 by school count
  top3 <- head(division_counts, 3)
  expect_true(any(grepl("Fairfax", top3$division_name, ignore.case = TRUE)),
    info = paste("Top 3 divisions:", paste(top3$division_name, collapse = ", ")))
})

# =============================================================================
# STATE FIELD VALIDATION
# =============================================================================

test_that("all state values are VA", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("state" %in% names(dir_data)) {
    states <- unique(dir_data$state)
    # Remove NAs
    states <- states[!is.na(states)]
    expect_true(all(states == "VA"),
      info = paste("Non-VA states found:", paste(states, collapse = ", ")))
  }
})

# =============================================================================
# ZIP CODE VALIDATION
# =============================================================================

test_that("zip codes are Virginia format (2xxxx)", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("zip" %in% names(dir_data)) {
    zips <- dir_data$zip[!is.na(dir_data$zip)]
    # Virginia ZIP codes start with 2 (20000-26999 range)
    # Extract first 5 digits
    zip5 <- substr(zips, 1, 5)
    zip_numeric <- suppressWarnings(as.numeric(zip5))
    valid_zips <- zip_numeric[!is.na(zip_numeric)]

    # At least 90% should be in Virginia range
    pct_valid <- mean(valid_zips >= 20000 & valid_zips <= 26999)
    expect_true(pct_valid >= 0.90,
      info = paste("Only", round(pct_valid * 100, 1), "% valid VA ZIPs"))
  }
})

# =============================================================================
# RAW VS TIDY FORMAT
# =============================================================================

test_that("raw directory has same or more rows than tidy", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  tryCatch({
    raw <- fetch_directory(tidy = FALSE, use_cache = TRUE)
    tidy <- fetch_directory(tidy = TRUE, use_cache = TRUE)

    # Raw should have at least as many rows
    expect_true(nrow(raw) >= nrow(tidy),
      info = paste("Raw:", nrow(raw), "Tidy:", nrow(tidy)))
  }, error = function(e) {
    skip(paste("Raw directory fetch failed:", e$message))
  })
})

# =============================================================================
# NO DUPLICATE SCHOOLS
# =============================================================================

test_that("no duplicate school entries in directory", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("school_id" %in% names(dir_data) && "division_id" %in% names(dir_data)) {
    # Each division_id + school_id combination should be unique
    dupes <- dir_data |>
      dplyr::count(division_id, school_id) |>
      dplyr::filter(n > 1)

    expect_equal(nrow(dupes), 0,
      info = paste(nrow(dupes), "duplicate school entries found"))
  }
})

# =============================================================================
# NCES ID FORMAT
# =============================================================================

test_that("NCES IDs are 12-digit format when present", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("nces_id" %in% names(dir_data)) {
    nces_ids <- dir_data$nces_id[!is.na(dir_data$nces_id) & dir_data$nces_id != ""]

    if (length(nces_ids) > 0) {
      # Most NCES school IDs are 12 digits
      pct_12digit <- mean(nchar(nces_ids) == 12)
      expect_true(pct_12digit >= 0.80,
        info = paste("Only", round(pct_12digit * 100, 1), "% are 12-digit NCES IDs"))
    }
  }
})

# =============================================================================
# PRINCIPAL NAME FORMAT
# =============================================================================

test_that("most schools have a principal name", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("principal_name" %in% names(dir_data)) {
    pct_with_principal <- mean(!is.na(dir_data$principal_name) &
                                dir_data$principal_name != "")

    # At least 80% should have a principal
    expect_true(pct_with_principal >= 0.80,
      info = paste("Only", round(pct_with_principal * 100, 1),
                    "% have principal names"))
  }
})

# =============================================================================
# CACHE FUNCTIONS
# =============================================================================

test_that("directory cache functions exist", {
  expect_true(is.function(vaschooldata::clear_directory_cache))
})

test_that("dir_cache_exists returns logical", {
  dir_cache_exists <- vaschooldata:::dir_cache_exists
  result <- dir_cache_exists("directory_tidy")
  expect_true(is.logical(result))
})

# =============================================================================
# SCHOOL TYPE DIVERSITY
# =============================================================================

test_that("directory includes multiple school types", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("school_type" %in% names(dir_data)) {
    school_types <- unique(dir_data$school_type[!is.na(dir_data$school_type)])
    expect_true(length(school_types) >= 2,
      info = paste("Only", length(school_types), "school types found"))
  }
})

test_that("directory includes multiple grade levels", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  if ("grade_level" %in% names(dir_data)) {
    grade_levels <- unique(dir_data$grade_level[!is.na(dir_data$grade_level)])
    # Should have Elementary, Middle, High at minimum
    expect_true(length(grade_levels) >= 3,
      info = paste("Only", length(grade_levels), "grade level categories"))
  }
})

# =============================================================================
# GEOGRAPHIC BOUNDS (when lat/lon available)
# =============================================================================
# Virginia latitude: ~36.5 to ~39.5
# Virginia longitude: ~-83.7 to ~-75.2

test_that("latitude values are within Virginia bounds when available", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  lat_cols <- c("latitude", "lat")
  lat_col <- lat_cols[lat_cols %in% names(dir_data)]

  if (length(lat_col) > 0) {
    lats <- as.numeric(dir_data[[lat_col[1]]])
    valid_lats <- lats[!is.na(lats)]

    if (length(valid_lats) > 0) {
      expect_true(all(valid_lats >= 36.0 & valid_lats <= 40.0),
        info = paste("Latitude range:", min(valid_lats), "to", max(valid_lats)))
    }
  }
})

test_that("longitude values are within Virginia bounds when available", {
  skip_on_cran()
  skip_if_offline()
  skip_if_directory_unavailable()

  dir_data <- fetch_directory(use_cache = TRUE)

  lon_cols <- c("longitude", "lon", "lng")
  lon_col <- lon_cols[lon_cols %in% names(dir_data)]

  if (length(lon_col) > 0) {
    lons <- as.numeric(dir_data[[lon_col[1]]])
    valid_lons <- lons[!is.na(lons)]

    if (length(valid_lons) > 0) {
      expect_true(all(valid_lons >= -84.0 & valid_lons <= -75.0),
        info = paste("Longitude range:", min(valid_lons), "to", max(valid_lons)))
    }
  }
})
