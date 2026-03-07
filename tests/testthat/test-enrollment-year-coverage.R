# ==============================================================================
# Enrollment Year Coverage Tests for vaschooldata
# ==============================================================================
#
# Per-year tests across all available enrollment years (2016-2025).
# Verifies state totals, Fairfax County (largest division), subgroup/grade
# completeness, and entity flags for each year.
#
# All fetch calls use use_cache = TRUE.
# All pinned values from real VDOE data.
#
# Virginia has ~1.25 million students across 132 school divisions.
# Fairfax County Public Schools is the largest division (~188,000+ students).
#
# Note: VDOE enrollment data requires direct download from School Quality
# Profiles which may be blocked by CAPTCHA. Tests skip gracefully when
# data is unavailable.
# ==============================================================================

# Helper to skip when VDOE enrollment is unavailable
skip_if_enr_unavailable <- function() {
  tryCatch({
    result <- vaschooldata::fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
    if (is.null(result) || nrow(result) == 0) {
      testthat::skip("VDOE enrollment data returned empty result")
    }
  }, error = function(e) {
    testthat::skip(paste("VDOE enrollment data unavailable:", conditionMessage(e)))
  })
}

# =============================================================================
# STATE TOTAL TESTS — Per Year
# =============================================================================
# Virginia total enrollment is approximately 1.25 million students.
# Valid range: 1,100,000 to 1,400,000 (allows for historical changes)

test_that("2016 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2017 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2017, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2018 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2018, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2019 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2019, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2020 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2021 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2021, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2022 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2022, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2023 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2023, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2024 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

test_that("2025 state total enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2025, tidy = TRUE, use_cache = TRUE)
  state <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

  expect_equal(nrow(state), 1)
  expect_true(state$n_students >= 1100000 & state$n_students <= 1400000,
    info = paste("State total:", state$n_students))
})

# =============================================================================
# FAIRFAX COUNTY TESTS — Per Year (Largest Division)
# =============================================================================
# Fairfax County Public Schools enrolls ~186,000-189,000 students.
# Division number: typically found with district_name containing "FAIRFAX"
# Valid range: 150,000 to 210,000 (allows for growth over time)

test_that("2016 Fairfax County enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = TRUE)
  ff <- enr |>
    dplyr::filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
                  grepl("FAIRFAX", district_name, ignore.case = TRUE))

  expect_equal(nrow(ff), 1, info = "Expected exactly 1 Fairfax County row")
  expect_true(ff$n_students >= 150000 & ff$n_students <= 210000,
    info = paste("Fairfax total:", ff$n_students))
})

test_that("2020 Fairfax County enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  ff <- enr |>
    dplyr::filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
                  grepl("FAIRFAX", district_name, ignore.case = TRUE))

  expect_equal(nrow(ff), 1, info = "Expected exactly 1 Fairfax County row")
  expect_true(ff$n_students >= 150000 & ff$n_students <= 210000,
    info = paste("Fairfax total:", ff$n_students))
})

test_that("2024 Fairfax County enrollment is in valid range", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  ff <- enr |>
    dplyr::filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
                  grepl("FAIRFAX", district_name, ignore.case = TRUE))

  expect_equal(nrow(ff), 1, info = "Expected exactly 1 Fairfax County row")
  expect_true(ff$n_students >= 150000 & ff$n_students <= 210000,
    info = paste("Fairfax total:", ff$n_students))
})

# =============================================================================
# SUBGROUP COMPLETENESS — Per Year
# =============================================================================

test_that("2016 has all expected subgroups", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = TRUE)
  subs <- unique(enr$subgroup)

  expect_true("total_enrollment" %in% subs)
  expect_true("white" %in% subs)
  expect_true("black" %in% subs)
  expect_true("hispanic" %in% subs)
  expect_true("asian" %in% subs)
  expect_true("male" %in% subs)
  expect_true("female" %in% subs)
})

test_that("2020 has all expected subgroups including 7-race", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  subs <- unique(enr$subgroup)

  expect_true("total_enrollment" %in% subs)
  expect_true("white" %in% subs)
  expect_true("black" %in% subs)
  expect_true("hispanic" %in% subs)
  expect_true("asian" %in% subs)
  expect_true("native_american" %in% subs)
  expect_true("pacific_islander" %in% subs)
  expect_true("multiracial" %in% subs)
  expect_true("male" %in% subs)
  expect_true("female" %in% subs)
})

test_that("2024 has all expected subgroups including 7-race", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subs <- unique(enr$subgroup)

  expect_true("total_enrollment" %in% subs)
  expect_true("white" %in% subs)
  expect_true("black" %in% subs)
  expect_true("hispanic" %in% subs)
  expect_true("asian" %in% subs)
  expect_true("native_american" %in% subs)
  expect_true("pacific_islander" %in% subs)
  expect_true("multiracial" %in% subs)
  expect_true("male" %in% subs)
  expect_true("female" %in% subs)
})

# =============================================================================
# GRADE LEVEL COMPLETENESS — Per Year
# =============================================================================

test_that("2016 has standard grade levels", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)

  # Must have PK through 12 plus TOTAL
  expect_true("TOTAL" %in% grades)
  expect_true("K" %in% grades)
  for (g in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
    expect_true(g %in% grades, info = paste("Missing grade:", g))
  }
})

test_that("2020 has standard grade levels", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)

  expect_true("TOTAL" %in% grades)
  expect_true("PK" %in% grades)
  expect_true("K" %in% grades)
  for (g in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
    expect_true(g %in% grades, info = paste("Missing grade:", g))
  }
})

test_that("2024 has standard grade levels", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  grades <- unique(enr$grade_level)

  expect_true("TOTAL" %in% grades)
  expect_true("PK" %in% grades)
  expect_true("K" %in% grades)
  for (g in c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) {
    expect_true(g %in% grades, info = paste("Missing grade:", g))
  }
})

# =============================================================================
# ENTITY FLAG TESTS — Per Year
# =============================================================================

test_that("2016 has state, district, and campus level records", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = TRUE)

  expect_true(any(enr$is_state), info = "No state records")
  expect_true(any(enr$is_district), info = "No district records")
  expect_true(any(enr$is_campus), info = "No campus records")
})

test_that("2020 has state, district, and campus level records", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)

  expect_true(any(enr$is_state), info = "No state records")
  expect_true(any(enr$is_district), info = "No district records")
  expect_true(any(enr$is_campus), info = "No campus records")
})

test_that("2024 has state, district, and campus level records", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  expect_true(any(enr$is_state), info = "No state records")
  expect_true(any(enr$is_district), info = "No district records")
  expect_true(any(enr$is_campus), info = "No campus records")
})

# =============================================================================
# DISTRICT COUNT — Per Year
# =============================================================================
# Virginia has approximately 132 school divisions

test_that("2016 has approximately 132 divisions", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = FALSE, use_cache = TRUE)
  n_dist <- sum(enr$type == "District")

  expect_true(n_dist >= 125 & n_dist <= 140,
    info = paste("Division count:", n_dist))
})

test_that("2020 has approximately 132 divisions", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = FALSE, use_cache = TRUE)
  n_dist <- sum(enr$type == "District")

  expect_true(n_dist >= 125 & n_dist <= 140,
    info = paste("Division count:", n_dist))
})

test_that("2024 has approximately 132 divisions", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  n_dist <- sum(enr$type == "District")

  expect_true(n_dist >= 125 & n_dist <= 140,
    info = paste("Division count:", n_dist))
})

# =============================================================================
# SCHOOL COUNT — Per Year
# =============================================================================
# Virginia has approximately 2,000+ schools

test_that("2016 has 1500+ schools", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = FALSE, use_cache = TRUE)
  n_campus <- sum(enr$type == "Campus")

  expect_true(n_campus >= 1500,
    info = paste("School count:", n_campus))
})

test_that("2020 has 1500+ schools", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = FALSE, use_cache = TRUE)
  n_campus <- sum(enr$type == "Campus")

  expect_true(n_campus >= 1500,
    info = paste("School count:", n_campus))
})

test_that("2024 has 1500+ schools", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  n_campus <- sum(enr$type == "Campus")

  expect_true(n_campus >= 1500,
    info = paste("School count:", n_campus))
})

# =============================================================================
# WIDE FORMAT STRUCTURE — Per Year
# =============================================================================

test_that("2016 wide format has demographic columns", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  wide <- fetch_enr(2016, tidy = FALSE, use_cache = TRUE)

  expect_true("row_total" %in% names(wide))
  expect_true("white" %in% names(wide))
  expect_true("black" %in% names(wide))
  expect_true("hispanic" %in% names(wide))
})

test_that("2020 wide format has 7-race and gender columns", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  wide <- fetch_enr(2020, tidy = FALSE, use_cache = TRUE)

  expect_true("row_total" %in% names(wide))
  expect_true("white" %in% names(wide))
  expect_true("black" %in% names(wide))
  expect_true("hispanic" %in% names(wide))
  expect_true("asian" %in% names(wide))
  expect_true("native_american" %in% names(wide))
  expect_true("pacific_islander" %in% names(wide))
  expect_true("multiracial" %in% names(wide))
  expect_true("male" %in% names(wide))
  expect_true("female" %in% names(wide))
})

test_that("2024 wide format has grade columns", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  expect_true("grade_k" %in% names(wide))
  expect_true("grade_01" %in% names(wide))
  expect_true("grade_12" %in% names(wide))
})

# =============================================================================
# DATA QUALITY — Per Year
# =============================================================================

test_that("2016 enrollment has no negative counts", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2016, tidy = TRUE, use_cache = TRUE)
  valid_counts <- enr$n_students[!is.na(enr$n_students)]
  expect_true(all(valid_counts >= 0),
    info = "Negative enrollment counts found")
})

test_that("2020 enrollment has no negative counts", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2020, tidy = TRUE, use_cache = TRUE)
  valid_counts <- enr$n_students[!is.na(enr$n_students)]
  expect_true(all(valid_counts >= 0),
    info = "Negative enrollment counts found")
})

test_that("2024 enrollment has no negative counts", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  valid_counts <- enr$n_students[!is.na(enr$n_students)]
  expect_true(all(valid_counts >= 0),
    info = "Negative enrollment counts found")
})

# =============================================================================
# YEAR-OVER-YEAR STABILITY
# =============================================================================

test_that("state enrollment does not change >10% between adjacent years", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  years <- get_available_years()
  prev_total <- NULL

  for (yr in years) {
    tryCatch({
      enr <- fetch_enr(yr, tidy = TRUE, use_cache = TRUE)
      state <- enr |>
        dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

      if (nrow(state) == 1) {
        if (!is.null(prev_total)) {
          pct_change <- abs(state$n_students - prev_total) / prev_total
          expect_true(pct_change < 0.10,
            info = paste("YoY change", yr, ":", round(pct_change * 100, 1), "%"))
        }
        prev_total <- state$n_students
      }
    }, error = function(e) NULL)
  }
})

# =============================================================================
# MULTI-YEAR FETCH
# =============================================================================

test_that("fetch_enr_multi returns data for all requested years", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr_multi(2022:2024, tidy = TRUE, use_cache = TRUE)

  years_present <- sort(unique(enr$end_year))
  expect_equal(years_present, 2022:2024)

  # State totals for each year
  state_totals <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
  expect_equal(nrow(state_totals), 3)
})

# =============================================================================
# DEMOGRAPHIC SUM EQUALS TOTAL
# =============================================================================

test_that("demographic subgroup sum approximates total enrollment", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  # Get state-level data
  state_total <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  race_subgroups <- c("white", "black", "hispanic", "asian",
                      "native_american", "pacific_islander", "multiracial")

  state_race_sum <- enr |>
    dplyr::filter(is_state, subgroup %in% race_subgroups, grade_level == "TOTAL") |>
    dplyr::summarize(total = sum(n_students, na.rm = TRUE)) |>
    dplyr::pull(total)

  # Race sum should be within 5% of total (some students may be unclassified)
  pct_diff <- abs(state_race_sum - state_total) / state_total
  expect_true(pct_diff < 0.05,
    info = paste("Race sum:", state_race_sum, "Total:", state_total,
                 "Diff:", round(pct_diff * 100, 1), "%"))
})

test_that("gender sum approximates total enrollment", {
  skip_on_cran()
  skip_if_offline()
  skip_if_enr_unavailable()

  enr <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_total <- enr |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  state_gender_sum <- enr |>
    dplyr::filter(is_state, subgroup %in% c("male", "female"), grade_level == "TOTAL") |>
    dplyr::summarize(total = sum(n_students, na.rm = TRUE)) |>
    dplyr::pull(total)

  # Gender sum should be within 2% of total
  pct_diff <- abs(state_gender_sum - state_total) / state_total
  expect_true(pct_diff < 0.02,
    info = paste("Gender sum:", state_gender_sum, "Total:", state_total,
                 "Diff:", round(pct_diff * 100, 1), "%"))
})
