# ==============================================================================
# Graduation Year Coverage Tests for vaschooldata
# ==============================================================================
#
# Per-year tests across all available graduation years (2019-2023).
# Verifies state rates, cohort sizes, school counts, diploma type
# completeness, suppression handling, and pinned school-level values.
#
# All fetch calls use use_cache = TRUE.
# All pinned values from real VDOE data verified against cached CSVs.
#
# Virginia graduates ~90,000-91,000 seniors per year from a cohort
# of ~97,000-99,000. State graduation rate: 91-93%.
#
# ==============================================================================

# =============================================================================
# STATE-LEVEL PINNED VALUES — Per Year
# =============================================================================
# All values verified from VDOE Open Data Portal CSV files.
# Column name is graduation_rate (not grad_rate).

test_that("2019 state graduation rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(nrow(state), 1, info = "Expected exactly 1 state-level 'all' record")
  expect_equal(state$graduation_rate, 0.9160, tolerance = 0.002)
})

test_that("2019 state cohort size matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$cohort_size, 98241, tolerance = 100)
})

test_that("2019 state total graduates matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$total_graduates, 89991, tolerance = 100)
})

test_that("2019 state dropout count matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropouts, 5410, tolerance = 50)
})

test_that("2020 state graduation rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(nrow(state), 1)
  expect_equal(state$graduation_rate, 0.9252, tolerance = 0.002)
})

test_that("2020 state cohort size matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$cohort_size, 98327, tolerance = 100)
})

test_that("2020 state total graduates matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$total_graduates, 90971, tolerance = 100)
})

test_that("2020 state dropout count matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropouts, 5008, tolerance = 50)
})

test_that("2021 state graduation rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(nrow(state), 1)
  expect_equal(state$graduation_rate, 0.9303, tolerance = 0.002)
})

test_that("2021 state cohort size matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$cohort_size, 97096, tolerance = 100)
})

test_that("2021 state total graduates matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$total_graduates, 90325, tolerance = 100)
})

test_that("2021 state dropout count matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropouts, 4129, tolerance = 50)
})

test_that("2022 state graduation rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(nrow(state), 1)
  expect_equal(state$graduation_rate, 0.9219, tolerance = 0.002)
})

test_that("2022 state cohort size matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$cohort_size, 98281, tolerance = 100)
})

test_that("2022 state total graduates matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$total_graduates, 90603, tolerance = 100)
})

test_that("2022 state dropout count matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropouts, 5061, tolerance = 50)
})

test_that("2023 state graduation rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(nrow(state), 1)
  expect_equal(state$graduation_rate, 0.9193, tolerance = 0.002)
})

test_that("2023 state cohort size matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$cohort_size, 98927, tolerance = 100)
})

test_that("2023 state total graduates matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$total_graduates, 90944, tolerance = 100)
})

test_that("2023 state completion rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$completion_rate, 0.9293, tolerance = 0.002)
})

test_that("2023 state dropout rate matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropout_rate, 0.0538, tolerance = 0.002)
})

test_that("2023 state dropout count matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropouts, 5319, tolerance = 50)
})

test_that("2023 state still_enrolled matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$still_enrolled, 1330, tolerance = 50)
})

test_that("2023 state long_term_absence matches VDOE source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state <- data |>
    dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$long_term_absence, 346, tolerance = 20)
})

# =============================================================================
# STATE-LEVEL DIPLOMA BREAKDOWN — 2023
# =============================================================================

test_that("2023 state Advanced Studies diploma count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 50175, tolerance = 100)
})

test_that("2023 state Standard diploma count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "standard") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 37883, tolerance = 100)
})

test_that("2023 state IB diploma count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "ib") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 766, tolerance = 10)
})

test_that("2023 state GED count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "ged") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 145, tolerance = 10)
})

test_that("2023 state ISAEP count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "isaep") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 700, tolerance = 10)
})

test_that("2023 state certificate count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "certificate") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 143, tolerance = 10)
})

test_that("2023 state applied_studies count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 2117, tolerance = 20)
})

test_that("2023 state other_diplomas is suppressed (NA)", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(is_state, diploma_type == "other_diplomas") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(count),
    info = paste("Expected NA for other_diplomas, got:", count))
})

# =============================================================================
# SCHOOL COUNT — Per Year
# =============================================================================
# Virginia has 316-334 high schools reporting graduation data

test_that("2019 has 300-400 schools with graduation data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school, diploma_type == "all") |>
    nrow()

  expect_true(school_count >= 300 & school_count <= 400,
    info = paste("School count:", school_count))
})

test_that("2020 has 300-400 schools with graduation data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school, diploma_type == "all") |>
    nrow()

  expect_true(school_count >= 300 & school_count <= 400,
    info = paste("School count:", school_count))
})

test_that("2021 has 300-400 schools with graduation data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school, diploma_type == "all") |>
    nrow()

  expect_true(school_count >= 300 & school_count <= 400,
    info = paste("School count:", school_count))
})

test_that("2022 has 300-400 schools with graduation data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school, diploma_type == "all") |>
    nrow()

  expect_true(school_count >= 300 & school_count <= 400,
    info = paste("School count:", school_count))
})

test_that("2023 has 300-400 schools with graduation data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school, diploma_type == "all") |>
    nrow()

  expect_true(school_count >= 300 & school_count <= 400,
    info = paste("School count:", school_count))
})

# =============================================================================
# DIPLOMA TYPE COMPLETENESS — Per Year
# =============================================================================

test_that("2019 has all expected diploma types", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)
  dtypes <- unique(data$diploma_type)

  expect_true("all" %in% dtypes)
  expect_true("advanced_studies" %in% dtypes)
  expect_true("standard" %in% dtypes)
  expect_true("ib" %in% dtypes)
  expect_true("ged" %in% dtypes)
  expect_true("applied_studies" %in% dtypes)
  expect_true("certificate" %in% dtypes)
  expect_true("isaep" %in% dtypes)
})

test_that("2023 has all expected diploma types", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)
  dtypes <- unique(data$diploma_type)

  expect_true("all" %in% dtypes)
  expect_true("advanced_studies" %in% dtypes)
  expect_true("standard" %in% dtypes)
  expect_true("ib" %in% dtypes)
  expect_true("ged" %in% dtypes)
  expect_true("applied_studies" %in% dtypes)
  expect_true("certificate" %in% dtypes)
  expect_true("isaep" %in% dtypes)
  expect_true("other_diplomas" %in% dtypes)
})

# =============================================================================
# RATE RANGE TESTS — Per Year
# =============================================================================

test_that("2019 graduation rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)
  valid_rates <- data$graduation_rate[!is.na(data$graduation_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1),
    info = "Rates outside [0, 1] range")
})

test_that("2020 graduation rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)
  valid_rates <- data$graduation_rate[!is.na(data$graduation_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1))
})

test_that("2021 graduation rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)
  valid_rates <- data$graduation_rate[!is.na(data$graduation_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1))
})

test_that("2022 graduation rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)
  valid_rates <- data$graduation_rate[!is.na(data$graduation_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1))
})

test_that("2023 graduation rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)
  valid_rates <- data$graduation_rate[!is.na(data$graduation_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1))
})

test_that("2023 dropout rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)
  valid_rates <- data$dropout_rate[!is.na(data$dropout_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1))
})

test_that("2023 completion rates are between 0 and 1", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)
  valid_rates <- data$completion_rate[!is.na(data$completion_rate)]

  expect_true(all(valid_rates >= 0 & valid_rates <= 1))
})

# =============================================================================
# SUPPRESSION HANDLING — Across Years
# =============================================================================

test_that("'<' suppressed values become NA in diploma counts", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High Applied Studies is "<" in source CSV
  arcadia_app <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(arcadia_app),
    info = "Suppressed '<' not converted to NA")
})

test_that(".00% zero rates are NOT treated as suppressed", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Tabb High (Div 98, School 0230) has 0% dropout rate
  tabb_drop <- data |>
    dplyr::filter(division_number == "98", school_number == "0230",
                  diploma_type == "all") |>
    dplyr::pull(dropout_rate)

  expect_equal(tabb_drop, 0.0, tolerance = 0.0001)
  expect_false(is.na(tabb_drop))
})

test_that("100% graduation rate is valid (not capped)", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Tabb High has 100% graduation rate
  tabb_rate <- data |>
    dplyr::filter(division_number == "98", school_number == "0230",
                  diploma_type == "all") |>
    dplyr::pull(graduation_rate)

  expect_equal(tabb_rate, 1.0, tolerance = 0.0001)
})

# =============================================================================
# PINNED SCHOOL-LEVEL VALUES — Arcadia High
# =============================================================================

test_that("2023 Arcadia High graduation rate matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  rate <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "all") |>
    dplyr::pull(graduation_rate)

  expect_equal(rate, 0.8371, tolerance = 0.001)
})

test_that("2023 Arcadia High cohort size matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  cohort <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(cohort, 178, tolerance = 1)
})

test_that("2023 Arcadia High total graduates matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  grads <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(grads, 149, tolerance = 1)
})

test_that("2023 Arcadia High Advanced Studies count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 52, tolerance = 1)
})

test_that("2023 Arcadia High Standard count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "standard") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 96, tolerance = 1)
})

test_that("2019 Arcadia High graduation rate matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  rate <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "all") |>
    dplyr::pull(graduation_rate)

  expect_equal(rate, 0.8874, tolerance = 0.001)
})

test_that("2019 Arcadia High cohort size matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  cohort <- data |>
    dplyr::filter(division_number == "1", school_number == "0540",
                  diploma_type == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(cohort, 151, tolerance = 1)
})

# =============================================================================
# PINNED SCHOOL-LEVEL VALUES — Yorktown High
# =============================================================================

test_that("2023 Yorktown High graduation rate matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  rate <- data |>
    dplyr::filter(division_number == "7", school_number == "0330",
                  diploma_type == "all") |>
    dplyr::pull(graduation_rate)

  expect_equal(rate, 0.9794, tolerance = 0.001)
})

test_that("2023 Yorktown High cohort size matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  cohort <- data |>
    dplyr::filter(division_number == "7", school_number == "0330",
                  diploma_type == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(cohort, 678, tolerance = 1)
})

test_that("2023 Yorktown High Advanced Studies count matches source", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  count <- data |>
    dplyr::filter(division_number == "7", school_number == "0330",
                  diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(count, 507, tolerance = 1)
})

# =============================================================================
# PINNED SCHOOL-LEVEL VALUES — Alexandria City High
# =============================================================================

test_that("2023 Alexandria City High cohort size handles commas", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  cohort <- data |>
    dplyr::filter(division_number == "101", school_number == "0210",
                  diploma_type == "all") |>
    dplyr::pull(cohort_size)

  # Source CSV has "1,138" with comma formatting
  expect_equal(cohort, 1138, tolerance = 1)
})

# =============================================================================
# YEAR-OVER-YEAR CONSISTENCY
# =============================================================================

test_that("state graduation rate does not change >5pp between years", {
  skip_on_cran()
  skip_if_offline()

  prev_rate <- NULL

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)
    rate <- data |>
      dplyr::filter(is_state, diploma_type == "all") |>
      dplyr::pull(graduation_rate)

    if (!is.null(prev_rate)) {
      change <- abs(rate - prev_rate)
      expect_true(change < 0.05,
        info = paste("YoY change", yr, ":", round(change * 100, 1), "pp"))
    }
    prev_rate <- rate
  }
})

test_that("state cohort size does not change >10% between years", {
  skip_on_cran()
  skip_if_offline()

  prev_cohort <- NULL

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)
    cohort <- data |>
      dplyr::filter(is_state, diploma_type == "all") |>
      dplyr::pull(cohort_size)

    if (!is.null(prev_cohort)) {
      pct_change <- abs(cohort - prev_cohort) / prev_cohort
      expect_true(pct_change < 0.10,
        info = paste("YoY change", yr, ":", round(pct_change * 100, 1), "%"))
    }
    prev_cohort <- cohort
  }
})

test_that("Arcadia High appears in all 5 years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)

    arcadia <- data |>
      dplyr::filter(division_number == "1", school_number == "0540",
                    diploma_type == "all")

    expect_true(nrow(arcadia) > 0,
      info = paste("Arcadia High missing in", yr))
  }
})

# =============================================================================
# MULTI-YEAR FETCH
# =============================================================================

test_that("fetch_graduation_multi returns data for all requested years", {
  skip_on_cran()
  skip_if_offline()

  grad <- fetch_graduation_multi(2019:2023, use_cache = TRUE)

  years_present <- sort(unique(grad$end_year))
  expect_equal(years_present, 2019:2023)

  # State totals for each year
  state_totals <- grad |>
    dplyr::filter(is_state, diploma_type == "all")
  expect_equal(nrow(state_totals), 5)
})

# =============================================================================
# RATE TYPE CONSISTENCY
# =============================================================================

test_that("all years use consistent rate_type", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)
    rate_types <- unique(data$rate_type)

    # All should be "4 yr rate" or similar
    expect_equal(length(rate_types), 1,
      info = paste("Multiple rate types in", yr))
  }
})

# =============================================================================
# SCHEMA CONSISTENCY ACROSS ERAS
# =============================================================================

test_that("tidy column names are consistent across v1 and v2 eras", {
  skip_on_cran()
  skip_if_offline()

  data_2019 <- fetch_graduation(2019, use_cache = TRUE)
  data_2023 <- fetch_graduation(2023, use_cache = TRUE)

  expect_equal(sort(names(data_2019)), sort(names(data_2023)))
})

test_that("2019-2022 (v1) all have state records from aggregation", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2022) {
    data <- fetch_graduation(yr, use_cache = TRUE)
    state_rows <- data |> dplyr::filter(is_state, diploma_type == "all")
    expect_equal(nrow(state_rows), 1,
      info = paste("Missing state aggregate for", yr))
  }
})

test_that("2023 (v2) has state record from source data", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)
  state_rows <- data |> dplyr::filter(is_state, diploma_type == "all")
  expect_equal(nrow(state_rows), 1)
})

# =============================================================================
# DATA QUALITY — NO INF/NAN
# =============================================================================

test_that("no Inf in graduation rates across all years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)

    expect_false(any(is.infinite(data$graduation_rate), na.rm = TRUE),
      info = paste("Inf found in graduation_rate for", yr))
  }
})

test_that("no NaN in graduation rates across all years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)

    expect_false(any(is.nan(data$graduation_rate), na.rm = TRUE),
      info = paste("NaN found in graduation_rate for", yr))
  }
})

test_that("all cohort sizes are non-negative across all years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)
    valid_cohorts <- data$cohort_size[!is.na(data$cohort_size)]

    expect_true(all(valid_cohorts >= 0),
      info = paste("Negative cohort size found in", yr))
  }
})

# =============================================================================
# ID FORMAT PRESERVATION
# =============================================================================

test_that("school numbers preserve leading zeros across all years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)

    # Arcadia High is "0540" not "540"
    arcadia <- data |>
      dplyr::filter(division_number == "1", school_number == "0540")

    expect_true(nrow(arcadia) > 0,
      info = paste("Cannot find Arcadia High by school_number '0540' in", yr))
  }
})

test_that("school numbers are 4 digits in 2023", {
  skip_on_cran()
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  school_ids <- unique(data$school_number[data$is_school])
  expect_true(all(nchar(school_ids) == 4),
    info = "Not all school numbers are 4 digits")
})

test_that("division numbers are character type across all years", {
  skip_on_cran()
  skip_if_offline()

  for (yr in 2019:2023) {
    data <- fetch_graduation(yr, use_cache = TRUE)
    expect_true(is.character(data$division_number),
      info = paste("division_number not character in", yr))
  }
})
