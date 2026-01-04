# Tests for enrollment functions
# Note: Most tests are marked as skip_on_cran since they require network access

test_that("safe_numeric handles various inputs", {
  # Use internal function with ::: operator
  safe_numeric <- vaschooldata:::safe_numeric

  # Normal numbers
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)

  # Suppressed values
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("-2")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("PS")))

  # Whitespace handling
  expect_equal(safe_numeric("  100  "), 100)

  # Already numeric
  expect_equal(safe_numeric(100), 100)
})

test_that("get_available_years returns expected range", {
  years <- get_available_years()

  expect_true(is.integer(years))
  expect_equal(min(years), 2016L)
  expect_equal(max(years), 2024L)
  expect_true(length(years) == 9)  # 2016-2024 inclusive
})

test_that("get_va_fips returns Virginia FIPS code", {
  get_va_fips <- vaschooldata:::get_va_fips
  expect_equal(get_va_fips(), 51L)
})

test_that("fetch_enr validates year parameter", {
  expect_error(fetch_enr(1900), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
  expect_error(fetch_enr(2015), "end_year must be between")
})

# Note: build_api_url tests removed - package now uses VDOE data sources only

test_that("map_vdoe_race converts race names correctly", {
  map_vdoe_race <- vaschooldata:::map_vdoe_race
  expect_equal(map_vdoe_race("White"), "white")
  expect_equal(map_vdoe_race("Black"), "black")
  expect_equal(map_vdoe_race("Hispanic"), "hispanic")
  expect_equal(map_vdoe_race("Asian"), "asian")
  expect_equal(map_vdoe_race("American Indian"), "native_american")
  expect_equal(map_vdoe_race("Native Hawaiian"), "pacific_islander")
  expect_equal(map_vdoe_race("Two or More Races"), "multiracial")
})

test_that("map_grade_code converts codes correctly", {
  map_grade_code <- vaschooldata:::map_grade_code
  expect_equal(unname(map_grade_code("PK")), "PK")
  expect_equal(unname(map_grade_code("K")), "K")
  expect_equal(unname(map_grade_code("KG")), "K")
  expect_equal(unname(map_grade_code("1")), "01")
  expect_equal(unname(map_grade_code("12")), "12")
  expect_equal(unname(map_grade_code("TOTAL")), "TOTAL")
})

test_that("get_cache_dir returns valid path", {
  get_cache_dir <- vaschooldata:::get_cache_dir
  cache_dir <- get_cache_dir()
  expect_true(is.character(cache_dir))
  expect_true(grepl("vaschooldata", cache_dir))
})

test_that("cache functions work correctly", {
  get_cache_path <- vaschooldata:::get_cache_path
  cache_exists <- vaschooldata:::cache_exists

  # Test cache path generation
  path <- get_cache_path(2023, "tidy")
  expect_true(grepl("enr_tidy_2023.rds", path))

  # Test cache_exists returns FALSE for non-existent cache
  expect_false(cache_exists(9999, "tidy"))
})

# Integration tests (require network access)
test_that("fetch_enr downloads and processes data", {
  skip_on_cran()
  skip_if_offline()
  skip_if_vdoe_unavailable()

  # Use a recent year
  result <- fetch_enr(2022, tidy = FALSE, use_cache = FALSE)

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("district_id" %in% names(result))
  expect_true("campus_id" %in% names(result))
  expect_true("row_total" %in% names(result))
  expect_true("type" %in% names(result))

  # Check we have all levels
  expect_true("State" %in% result$type)
  expect_true("District" %in% result$type)
  expect_true("Campus" %in% result$type)

  # Check Virginia has about 132 divisions
  n_districts <- sum(result$type == "District")
  expect_true(n_districts >= 100 && n_districts <= 150)

  # Check Virginia has about 2000+ schools
  n_campuses <- sum(result$type == "Campus")
  expect_true(n_campuses >= 1500)
})

test_that("tidy_enr produces correct long format", {
  skip_on_cran()
  skip_if_offline()
  skip_if_vdoe_unavailable()

  # Get wide data
  wide <- fetch_enr(2022, tidy = FALSE, use_cache = TRUE)

  # Tidy it
  tidy_result <- tidy_enr(wide)

  # Check structure
  expect_true("grade_level" %in% names(tidy_result))
  expect_true("subgroup" %in% names(tidy_result))
  expect_true("n_students" %in% names(tidy_result))
  expect_true("pct" %in% names(tidy_result))

  # Check subgroups include expected values
  subgroups <- unique(tidy_result$subgroup)
  expect_true("total_enrollment" %in% subgroups)
})

test_that("id_enr_aggs adds correct flags", {
  skip_on_cran()
  skip_if_offline()
  skip_if_vdoe_unavailable()

  # Get tidy data with aggregation flags
  result <- fetch_enr(2022, tidy = TRUE, use_cache = TRUE)

  # Check flags exist
  expect_true("is_state" %in% names(result))
  expect_true("is_district" %in% names(result))
  expect_true("is_campus" %in% names(result))
  expect_true("is_charter" %in% names(result))

  # Check flags are boolean
  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))
  expect_true(is.logical(result$is_charter))

  # Check mutual exclusivity (each row is only one type)
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))
})

test_that("fetch_enr works for different eras", {
  skip_on_cran()
  skip_if_offline()
  skip_if_vdoe_unavailable()

  # Test 7-race era (2011+)
  result_2020 <- fetch_enr(2020, tidy = FALSE, use_cache = TRUE)
  expect_true("pacific_islander" %in% names(result_2020) ||
              "multiracial" %in% names(result_2020))

  # Test early era (pre-2000)
  result_1995 <- fetch_enr(1995, tidy = FALSE, use_cache = TRUE)
  expect_true(is.data.frame(result_1995))
  expect_true(nrow(result_1995) > 0)
})

test_that("fetch_enr_multi works for multiple years", {
  skip_on_cran()
  skip_if_offline()
  skip_if_vdoe_unavailable()

  result <- fetch_enr_multi(2020:2022, tidy = TRUE, use_cache = TRUE)

  # Should have all 3 years
  years_present <- unique(result$end_year)
  expect_equal(sort(years_present), 2020:2022)

  # Should have state totals for each year
  state_totals <- result %>%
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(state_totals), 3)
})

test_that("enr_grade_aggs creates correct aggregations", {
  skip_on_cran()
  skip_if_offline()
  skip_if_vdoe_unavailable()

  tidy_data <- fetch_enr(2022, tidy = TRUE, use_cache = TRUE)
  grade_aggs <- enr_grade_aggs(tidy_data)

  # Check expected grade levels created
  grade_levels <- unique(grade_aggs$grade_level)
  expect_true("K8" %in% grade_levels)
  expect_true("HS" %in% grade_levels)
  expect_true("K12" %in% grade_levels)
})
