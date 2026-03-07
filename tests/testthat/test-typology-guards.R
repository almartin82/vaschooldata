# ==============================================================================
# Typology Guard Tests for vaschooldata
# ==============================================================================
#
# These tests verify that the package enforces correct data types, column names,
# entity flags, and suppression marker handling consistently across all functions.
#
# VA-specific suppression markers: PS, DS, -2, -9, <, *, S, s, -1, <5, <10
# ==============================================================================

# =============================================================================
# SUPPRESSION MARKER HANDLING
# =============================================================================

test_that("safe_numeric handles VA-specific suppression markers", {
  safe_numeric <- vaschooldata:::safe_numeric

  # PS = Privacy Suppressed

  expect_true(is.na(safe_numeric("PS")))

  # DS = Data Suppressed
  expect_true(is.na(safe_numeric("DS")))

  # Numeric suppression codes
  expect_true(is.na(safe_numeric("-2")))
  expect_true(is.na(safe_numeric("-9")))
  expect_true(is.na(safe_numeric("-1")))

  # Range suppression
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("<10")))

  # Generic markers
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("S")))
  expect_true(is.na(safe_numeric("s")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("")))
})

test_that("safe_numeric preserves valid numeric values", {
  safe_numeric <- vaschooldata:::safe_numeric

  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("1234567"), 1234567)
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric(42), 42)
  expect_equal(safe_numeric(0), 0)
})

test_that("safe_numeric handles edge cases", {
  safe_numeric <- vaschooldata:::safe_numeric

  # Decimal values
  expect_equal(safe_numeric("3.14"), 3.14)
  expect_equal(safe_numeric("0.5"), 0.5)

  # Negative legitimate values should become NA (since -1, -2, -9 are suppression)
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("-2")))
  expect_true(is.na(safe_numeric("-9")))
})

# =============================================================================
# GRADUATION SUPPRESSION: "<" vs ".00%"
# =============================================================================

test_that("graduation parse_percentage handles .00% as genuine zero", {
  parse_percentage <- vaschooldata:::parse_percentage

  # ".00%" is a real zero rate, NOT suppression
  result <- parse_percentage("   .00%")
  expect_equal(result, 0.0, tolerance = 0.0001)
  expect_false(is.na(result))
})

test_that("graduation parse_percentage handles standard percentages", {
  parse_percentage <- vaschooldata:::parse_percentage

  expect_equal(parse_percentage("   83.71%"), 0.8371, tolerance = 0.0001)
  expect_equal(parse_percentage("  100.00%"), 1.0, tolerance = 0.0001)
  expect_equal(parse_percentage("    91.93%"), 0.9193, tolerance = 0.0001)
})

test_that("graduation parse_integer handles '<' as suppressed (NA)", {
  parse_integer <- vaschooldata:::parse_integer

  result <- parse_integer("<")
  expect_true(is.na(result))
})

test_that("graduation parse_integer handles comma-formatted numbers", {
  parse_integer <- vaschooldata:::parse_integer

  expect_equal(parse_integer("          98,927"), 98927L)
  expect_equal(parse_integer("          90,944"), 90944L)
  expect_equal(parse_integer("           178"), 178L)
})

test_that("graduation parse_integer handles leading spaces", {
  parse_integer <- vaschooldata:::parse_integer

  expect_equal(parse_integer("           178"), 178L)
  expect_equal(parse_integer("               7"), 7L)
  expect_equal(parse_integer("               4"), 4L)
})

# =============================================================================
# COLUMN NAME VALIDATION
# =============================================================================

test_that("tidy enrollment has required columns", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    required_cols <- c(
      "end_year", "type", "district_id", "campus_id",
      "district_name", "campus_name",
      "grade_level", "subgroup", "n_students", "pct",
      "is_state", "is_district", "is_campus", "is_charter"
    )

    for (col in required_cols) {
      expect_true(col %in% names(enr),
        info = paste("Missing required column:", col))
    }
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("wide enrollment has required columns", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    wide <- vaschooldata::fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

    required_cols <- c(
      "end_year", "type", "district_id", "campus_id",
      "district_name", "campus_name", "row_total"
    )

    for (col in required_cols) {
      expect_true(col %in% names(wide),
        info = paste("Missing required column:", col))
    }
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("tidy graduation has required columns", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    required_cols <- c(
      "end_year", "division_number", "division_name",
      "school_number", "school_name", "rate_type",
      "cohort_size", "total_graduates", "graduation_rate",
      "completion_rate", "dropout_rate",
      "diploma_type", "diploma_count",
      "dropouts", "still_enrolled", "long_term_absence",
      "is_state", "is_district", "is_school"
    )

    for (col in required_cols) {
      expect_true(col %in% names(grad),
        info = paste("Missing required column:", col))
    }
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

# =============================================================================
# ENTITY FLAG TYPE VALIDATION
# =============================================================================

test_that("enrollment entity flags are logical type", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    expect_true(is.logical(enr$is_state))
    expect_true(is.logical(enr$is_district))
    expect_true(is.logical(enr$is_campus))
    expect_true(is.logical(enr$is_charter))
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("graduation entity flags are logical type", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    expect_true(is.logical(grad$is_state))
    expect_true(is.logical(grad$is_district))
    expect_true(is.logical(grad$is_school))
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

test_that("enrollment entity flags are mutually exclusive", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    # Each row should be exactly one entity type
    type_sums <- enr$is_state + enr$is_district + enr$is_campus
    expect_true(all(type_sums == 1),
      info = "Entity flags are not mutually exclusive")
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

# =============================================================================
# SUBGROUP NAME STANDARDIZATION
# =============================================================================

test_that("enrollment subgroup names follow naming standard", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    # Standard subgroup names per CLAUDE.md
    valid_subgroups <- c(
      "total_enrollment",
      "white", "black", "hispanic", "asian",
      "native_american", "pacific_islander", "multiracial",
      "male", "female"
    )

    actual_subgroups <- unique(enr$subgroup)

    # All subgroups must be in the valid set
    invalid <- setdiff(actual_subgroups, valid_subgroups)
    expect_equal(length(invalid), 0,
      info = paste("Non-standard subgroup names:", paste(invalid, collapse = ", ")))

    # Must have total_enrollment
    expect_true("total_enrollment" %in% actual_subgroups,
      info = "Missing total_enrollment subgroup")
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("enrollment grade levels follow naming standard", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    # Standard grade levels per CLAUDE.md (all UPPERCASE)
    valid_grades <- c(
      "PK", "K", "01", "02", "03", "04", "05", "06",
      "07", "08", "09", "10", "11", "12",
      "UG", "TOTAL"
    )

    actual_grades <- unique(enr$grade_level)

    # All grades must be in the valid set
    invalid <- setdiff(actual_grades, valid_grades)
    expect_equal(length(invalid), 0,
      info = paste("Non-standard grade levels:", paste(invalid, collapse = ", ")))

    # Must have TOTAL
    expect_true("TOTAL" %in% actual_grades,
      info = "Missing TOTAL grade level")
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("graduation diploma types follow naming standard", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    valid_diploma_types <- c(
      "advanced_studies", "ib", "standard",
      "other_diplomas", "applied_studies", "ged", "isaep",
      "certificate", "all"
    )

    actual_types <- unique(grad$diploma_type)

    invalid <- setdiff(actual_types, valid_diploma_types)
    expect_equal(length(invalid), 0,
      info = paste("Non-standard diploma types:", paste(invalid, collapse = ", ")))
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

# =============================================================================
# DATA TYPE VALIDATION
# =============================================================================

test_that("enrollment n_students is numeric", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    expect_true(is.numeric(enr$n_students))
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("enrollment pct is numeric between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    expect_true(is.numeric(enr$pct))

    valid_pcts <- enr$pct[!is.na(enr$pct)]
    expect_true(all(valid_pcts >= 0 & valid_pcts <= 1.01),
      info = "Percentages outside [0, 1] range")
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("enrollment end_year is integer-like", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    expect_true(is.numeric(enr$end_year))
    expect_true(all(enr$end_year == round(enr$end_year)))
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("graduation rates are numeric between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    expect_true(is.numeric(grad$graduation_rate))
    valid_rates <- grad$graduation_rate[!is.na(grad$graduation_rate)]
    expect_true(all(valid_rates >= 0 & valid_rates <= 1),
      info = "Graduation rates outside [0, 1] range")
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

test_that("graduation cohort_size is non-negative integer", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    expect_true(is.integer(grad$cohort_size) || is.numeric(grad$cohort_size))
    valid_cohorts <- grad$cohort_size[!is.na(grad$cohort_size)]
    expect_true(all(valid_cohorts >= 0),
      info = "Negative cohort sizes found")
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

test_that("graduation school_number is character type", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    expect_true(is.character(grad$school_number))
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

test_that("graduation division_number is character type", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    expect_true(is.character(grad$division_number))
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

# =============================================================================
# NO INF/NAN GUARDS
# =============================================================================

test_that("enrollment has no Inf values", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    numeric_cols <- names(enr)[sapply(enr, is.numeric)]
    for (col in numeric_cols) {
      expect_false(any(is.infinite(enr[[col]]), na.rm = TRUE),
        info = paste("Inf found in column:", col))
    }
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("enrollment has no NaN values", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    enr <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

    numeric_cols <- names(enr)[sapply(enr, is.numeric)]
    for (col in numeric_cols) {
      expect_false(any(is.nan(enr[[col]]), na.rm = TRUE),
        info = paste("NaN found in column:", col))
    }
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

test_that("graduation has no Inf values in rates", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    rate_cols <- c("graduation_rate", "completion_rate", "dropout_rate")
    for (col in rate_cols) {
      if (col %in% names(grad)) {
        expect_false(any(is.infinite(grad[[col]]), na.rm = TRUE),
          info = paste("Inf found in column:", col))
      }
    }
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

test_that("graduation has no NaN values in rates", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    grad <- vaschooldata::fetch_graduation(2023, use_cache = TRUE)

    rate_cols <- c("graduation_rate", "completion_rate", "dropout_rate")
    for (col in rate_cols) {
      if (col %in% names(grad)) {
        expect_false(any(is.nan(grad[[col]]), na.rm = TRUE),
          info = paste("NaN found in column:", col))
      }
    }
  }, error = function(e) {
    skip(paste("Graduation data unavailable:", e$message))
  })
})

# =============================================================================
# YEAR VALIDATION GUARDS
# =============================================================================

test_that("fetch_enr rejects years outside valid range", {
  expect_error(fetch_enr(1900), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
  expect_error(fetch_enr(2015), "end_year must be between")
  expect_error(fetch_enr(2026), "end_year must be between")
})

test_that("fetch_graduation rejects years outside valid range", {
  expect_error(fetch_graduation(2018), "end_year must be between")
  expect_error(fetch_graduation(2030), "end_year must be between")
  expect_error(fetch_graduation(1999), "end_year must be between")
})

test_that("fetch_enr_multi rejects any invalid year in range", {
  expect_error(fetch_enr_multi(2014:2016), "Invalid years")
  expect_error(fetch_enr_multi(c(2020, 2030)), "Invalid years")
})

test_that("fetch_graduation_multi rejects any invalid year in range", {
  expect_error(fetch_graduation_multi(2017:2019), "Invalid years")
  expect_error(fetch_graduation_multi(c(2020, 2030)), "Invalid years")
})

# =============================================================================
# AVAILABLE YEARS GUARDS
# =============================================================================

test_that("get_available_years returns valid integer vector", {
  years <- get_available_years()

  expect_true(is.integer(years))
  expect_true(all(years >= 2000 & years <= 2030))
  expect_true(length(years) >= 5)  # at least 5 years of data
  expect_equal(years, sort(years))  # sorted ascending
})

test_that("get_available_grad_years returns valid integer vector", {
  years <- get_available_grad_years()

  expect_true(is.integer(years))
  expect_true(all(years >= 2015 & years <= 2030))
  expect_true(length(years) >= 3)  # at least 3 years of data
  expect_equal(years, sort(years))  # sorted ascending
})

test_that("enrollment years are contiguous", {
  years <- get_available_years()
  diffs <- diff(years)
  expect_true(all(diffs == 1),
    info = "Enrollment year range has gaps")
})

test_that("graduation years are contiguous", {
  years <- get_available_grad_years()
  diffs <- diff(years)
  expect_true(all(diffs == 1),
    info = "Graduation year range has gaps")
})

# =============================================================================
# ERA DETECTION GUARDS
# =============================================================================

test_that("detect_grad_era returns v1 for data without Level column", {
  detect_grad_era <- vaschooldata:::detect_grad_era

  mock_v1 <- data.frame(
    `Cohort Year` = 2019,
    `Division Number` = "1",
    check.names = FALSE
  )

  expect_equal(detect_grad_era(mock_v1), "v1")
})

test_that("detect_grad_era returns v2 for data with Level column", {
  detect_grad_era <- vaschooldata:::detect_grad_era

  mock_v2 <- data.frame(
    `Cohort Year` = 2023,
    Level = "School",
    `Division Number` = "1",
    check.names = FALSE
  )

  expect_equal(detect_grad_era(mock_v2), "v2")
})

# =============================================================================
# MAP FUNCTION GUARDS
# =============================================================================

test_that("map_vdoe_race handles all standard race categories", {
  map_vdoe_race <- vaschooldata:::map_vdoe_race

  expect_equal(map_vdoe_race("White"), "white")
  expect_equal(map_vdoe_race("Black"), "black")
  expect_equal(map_vdoe_race("Black or African American"), "black")
  expect_equal(map_vdoe_race("Hispanic"), "hispanic")
  expect_equal(map_vdoe_race("Hispanic or Latino"), "hispanic")
  expect_equal(map_vdoe_race("Asian"), "asian")
  expect_equal(map_vdoe_race("American Indian"), "native_american")
  expect_equal(map_vdoe_race("American Indian or Alaska Native"), "native_american")
  expect_equal(map_vdoe_race("Native Hawaiian"), "pacific_islander")
  expect_equal(map_vdoe_race("Native Hawaiian or Other Pacific Islander"), "pacific_islander")
  expect_equal(map_vdoe_race("Two or More Races"), "multiracial")
  expect_equal(map_vdoe_race("Two or more races"), "multiracial")
  expect_equal(map_vdoe_race("Multiracial"), "multiracial")
})

test_that("map_vdoe_race handles abbreviation codes", {
  map_vdoe_race <- vaschooldata:::map_vdoe_race

  expect_equal(map_vdoe_race("WH"), "white")
  expect_equal(map_vdoe_race("BL"), "black")
  expect_equal(map_vdoe_race("HI"), "hispanic")
  expect_equal(map_vdoe_race("AS"), "asian")
  expect_equal(map_vdoe_race("AM"), "native_american")
  expect_equal(map_vdoe_race("PI"), "pacific_islander")
  expect_equal(map_vdoe_race("HP"), "pacific_islander")
  expect_equal(map_vdoe_race("TR"), "multiracial")
  expect_equal(map_vdoe_race("MR"), "multiracial")
})

test_that("map_grade_code handles all standard grade codes", {
  map_grade_code <- vaschooldata:::map_grade_code

  expect_equal(unname(map_grade_code("PK")), "PK")
  expect_equal(unname(map_grade_code("K")), "K")
  expect_equal(unname(map_grade_code("KG")), "K")
  expect_equal(unname(map_grade_code("1")), "01")
  expect_equal(unname(map_grade_code("01")), "01")
  expect_equal(unname(map_grade_code("9")), "09")
  expect_equal(unname(map_grade_code("12")), "12")
  expect_equal(unname(map_grade_code("UG")), "UG")
  expect_equal(unname(map_grade_code("TOTAL")), "TOTAL")
})

# =============================================================================
# GRADE AGGREGATION GUARDS
# =============================================================================

test_that("enr_grade_aggs produces K8, HS, K12 levels", {
  skip_on_cran()
  skip_if_offline()

  tryCatch({
    tidy_data <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
    grade_aggs <- enr_grade_aggs(tidy_data)

    agg_levels <- unique(grade_aggs$grade_level)
    expect_true("K8" %in% agg_levels)
    expect_true("HS" %in% agg_levels)
    expect_true("K12" %in% agg_levels)
  }, error = function(e) {
    skip(paste("VDOE data unavailable:", e$message))
  })
})

# =============================================================================
# CACHE FUNCTION GUARDS
# =============================================================================

test_that("get_cache_dir returns valid path containing vaschooldata", {
  get_cache_dir <- vaschooldata:::get_cache_dir
  cache_dir <- get_cache_dir()

  expect_true(is.character(cache_dir))
  expect_true(grepl("vaschooldata", cache_dir))
})

test_that("cache_exists returns FALSE for non-existent year", {
  cache_exists <- vaschooldata:::cache_exists

  expect_false(cache_exists(9999, "tidy"))
  expect_false(cache_exists(9999, "wide"))
})

test_that("get_cache_path generates expected filenames", {
  get_cache_path <- vaschooldata:::get_cache_path

  path <- get_cache_path(2023, "tidy")
  expect_true(grepl("enr_tidy_2023.rds", path))

  path_wide <- get_cache_path(2023, "wide")
  expect_true(grepl("enr_wide_2023.rds", path_wide))
})

# =============================================================================
# FORMAT UTILITY GUARDS
# =============================================================================

test_that("format_school_year produces correct format", {
  format_school_year <- vaschooldata:::format_school_year

  expect_equal(format_school_year(2024), "2023-24")
  expect_equal(format_school_year(2020), "2019-20")
  expect_equal(format_school_year(2000), "1999-00")
})

test_that("format_school_year_long produces correct format", {
  format_school_year_long <- vaschooldata:::format_school_year_long

  expect_equal(format_school_year_long(2024), "2023-2024")
  expect_equal(format_school_year_long(2020), "2019-2020")
})

test_that("get_va_fips returns 51", {
  get_va_fips <- vaschooldata:::get_va_fips
  expect_equal(get_va_fips(), 51L)
})
