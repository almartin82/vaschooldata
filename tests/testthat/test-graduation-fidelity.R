# Raw Data Fidelity Tests for VA Graduation Rate Data
# These tests verify that fetch_graduation() returns values that match the raw source
# All values are verified against actual CSV files downloaded from VA Open Data Portal
#
# VA-specific test values documented in VA-GRADUATION-RESEARCH.md

skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# =============================================================================
# STATE-LEVEL TESTS (5 years)
# =============================================================================

test_that("2023 state graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # From 2023 state record: 91.93%
  state_rate <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(state_rate, 0.9193, tolerance = 0.0001)
})

test_that("2023 state cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # From 2023 state record: "98,927" -> 98927
  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(state_cohort, 98927, tolerance = 1)
})

test_that("2023 state total graduates matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # From 2023 state record: "90,944" -> 90944
  state_grads <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(state_grads, 90944, tolerance = 1)
})

test_that("2023 state completion rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # From 2023 state record: 92.93%
  state_completion <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(completion_rate)

  expect_equal(state_completion, 0.9293, tolerance = 0.0001)
})

test_that("2023 state dropout rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # From 2023 state record: 5.38%
  state_dropout <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(dropout_rate)

  expect_equal(state_dropout, 0.0538, tolerance = 0.0001)
})

test_that("2021 calculated state graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  # From 2021 school-level aggregation: 93.03%
  state_rate <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(state_rate, 0.9303, tolerance = 0.0001)
})

test_that("2021 state cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  # From 2021 school-level aggregation: 97,096
  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(state_cohort, 97096, tolerance = 1)
})

test_that("2021 state total graduates matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  # From 2021 school-level aggregation: 90,325
  state_grads <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(state_grads, 90325, tolerance = 1)
})

test_that("2019 calculated state graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  # From 2019 school-level aggregation: 91.60%
  state_rate <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(state_rate, 0.9160, tolerance = 0.0001)
})

test_that("2019 state cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  # From 2019 school-level aggregation: 98,241
  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(state_cohort, 98241, tolerance = 1)
})

test_that("2019 state total graduates matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  # From 2019 school-level aggregation: 89,991
  state_grads <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(state_grads, 89991, tolerance = 1)
})

test_that("2020 state graduation rate is in expected range", {
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  # From 2020 school-level aggregation: 92.70%
  state_rate <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(state_rate, 0.9270, tolerance = 0.0001)
})

test_that("2020 state cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2020, use_cache = TRUE)

  # From 2020 school-level aggregation: 97,262
  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(state_cohort, 97262, tolerance = 1)
})

test_that("2022 state graduation rate is in expected range", {
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  # From 2022 school-level aggregation: 92.19%
  state_rate <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(state_rate, 0.9219, tolerance = 0.0001)
})

test_that("2022 state cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2022, use_cache = TRUE)

  # From 2022 school-level aggregation: 98,281
  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(state_cohort, 98281, tolerance = 1)
})

# =============================================================================
# SCHOOL-LEVEL TESTS (2023 Arcadia High - Detailed)
# =============================================================================

test_that("2023 Arcadia High graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High (Div 1, School 0540): 83.71%
  arcadia_rate <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(arcadia_rate, 0.8371, tolerance = 0.0001)
})

test_that("2023 Arcadia High cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 178
  arcadia_cohort <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(arcadia_cohort, 178, tolerance = 1)
})

test_that("2023 Arcadia High total graduates matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 149
  arcadia_grads <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(arcadia_grads, 149, tolerance = 1)
})

test_that("2023 Arcadia High Advanced Studies diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High Advanced Studies: 52
  arcadia_adv <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(arcadia_adv, 52, tolerance = 1)
})

test_that("2023 Arcadia High Standard diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High Standard: 96
  arcadia_std <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  diploma_type == "standard") |>
    dplyr::pull(diploma_count)

  expect_equal(arcadia_std, 96, tolerance = 1)
})

test_that("2023 Arcadia High Applied Studies is suppressed (NA)", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High Applied Studies: < (suppressed) -> NA
  arcadia_app <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(arcadia_app))
})

test_that("2023 Arcadia High completion rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 84.83%
  arcadia_comp <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(completion_rate)

  expect_equal(arcadia_comp, 0.8483, tolerance = 0.0001)
})

test_that("2023 Arcadia High dropout rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 3.93%
  arcadia_drop <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(dropout_rate)

  expect_equal(arcadia_drop, 0.0393, tolerance = 0.0001)
})

test_that("2023 Arcadia High dropout count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 7 dropouts
  arcadia_drop_count <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(dropouts)

  expect_equal(arcadia_drop_count, 7, tolerance = 1)
})

test_that("2023 Arcadia High still enrolled matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 4 still enrolled
  arcadia_still <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(still_enrolled)

  expect_equal(arcadia_still, 4, tolerance = 1)
})

test_that("2023 Arcadia High long-term absence matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High: 16 long-term absence
  arcadia_lta <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(long_term_absence)

  expect_equal(arcadia_lta, 16, tolerance = 1)
})

# =============================================================================
# SCHOOL-LEVEL TESTS (2019 Arcadia High - Era Comparison)
# =============================================================================

test_that("2019 Arcadia High graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  # Arcadia High 2019: 88.74%
  arcadia_rate <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(arcadia_rate, 0.8874, tolerance = 0.0001)
})

test_that("2019 Arcadia High cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  # Arcadia High 2019: 151
  arcadia_cohort <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(arcadia_cohort, 151, tolerance = 1)
})

test_that("2019 Arcadia High total graduates matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  # Arcadia High 2019: 134
  arcadia_grads <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(arcadia_grads, 134, tolerance = 1)
})

# =============================================================================
# SCHOOL-LEVEL TESTS (2021 Arcadia High - Era Comparison)
# =============================================================================

test_that("2021 Arcadia High graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  # Arcadia High 2021: 88.28%
  arcadia_rate <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(arcadia_rate, 0.8828, tolerance = 0.0001)
})

test_that("2021 Arcadia High cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  # Arcadia High 2021: 145
  arcadia_cohort <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(arcadia_cohort, 145, tolerance = 1)
})

test_that("2021 Arcadia High total graduates matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  # Arcadia High 2021: 128
  arcadia_grads <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  subgroup == "all") |>
    dplyr::pull(total_graduates)

  expect_equal(arcadia_grads, 128, tolerance = 1)
})

# =============================================================================
# SCHOOL-LEVEL TESTS (Additional Schools for Diversity)
# =============================================================================

test_that("2023 Yorktown High graduation rate matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Yorktown High (Div 7, School 0330): 97.94%
  yorktown_rate <- data |>
    dplyr::filter(division_number == "7",
                  school_number == "0330",
                  subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(yorktown_rate, 0.9794, tolerance = 0.0001)
})

test_that("2023 Yorktown High cohort size matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Yorktown High: 678
  yorktown_cohort <- data |>
    dplyr::filter(division_number == "7",
                  school_number == "0330",
                  subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(yorktown_cohort, 678, tolerance = 1)
})

test_that("2023 T.C. Williams (Alexandria City High) has commas in cohort", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Alexandria City High (Div 101, School 0210): "1,138" -> 1138
  alex_cohort <- data |>
    dplyr::filter(division_number == "101",
                  school_number == "0210",
                  subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_equal(alex_cohort, 1138, tolerance = 1)
})

test_that("2023 Tabb High has 100% graduation rate", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Tabb High (Div 98, School 0230): 100.00%
  tabb_rate <- data |>
    dplyr::filter(division_number == "98",
                  school_number == "0230",
                  subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_equal(tabb_rate, 1.0, tolerance = 0.0001)
})

test_that("2023 Tabb High has zero dropout rate", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Tabb High: .00% -> 0.0 (NOT suppressed)
  tabb_drop <- data |>
    dplyr::filter(division_number == "98",
                  school_number == "0230",
                  subgroup == "all") |>
    dplyr::pull(dropout_rate)

  expect_equal(tabb_drop, 0.0, tolerance = 0.0001)
})

# =============================================================================
# DIPLOMA TYPE TESTS (State Level - 2023)
# =============================================================================

test_that("2023 state Advanced Studies diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State Advanced Studies: "50,175" -> 50175
  state_adv <- data |>
    dplyr::filter(is_state,
                  diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(state_adv, 50175, tolerance = 1)
})

test_that("2023 state IB diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State IB: 766
  state_ib <- data |>
    dplyr::filter(is_state,
                  diploma_type == "ib") |>
    dplyr::pull(diploma_count)

  expect_equal(state_ib, 766, tolerance = 1)
})

test_that("2023 state Standard diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State Standard: "37,883" -> 37883
  state_std <- data |>
    dplyr::filter(is_state,
                  diploma_type == "standard") |>
    dplyr::pull(diploma_count)

  expect_equal(state_std, 37883, tolerance = 1)
})

test_that("2023 state Applied Studies is suppressed (NA)", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State Applied Studies: < (suppressed) -> NA
  state_app <- data |>
    dplyr::filter(is_state,
                  diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(state_app))
})

test_that("2023 state GED diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State GED: "2,117" -> 2117
  state_ged <- data |>
    dplyr::filter(is_state,
                  diploma_type == "ged") |>
    dplyr::pull(diploma_count)

  expect_equal(state_ged, 2117, tolerance = 1)
})

test_that("2023 state Certificate count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State Certificate: 145
  state_cert <- data |>
    dplyr::filter(is_state,
                  diploma_type == "certificate") |>
    dplyr::pull(diploma_count)

  expect_equal(state_cert, 145, tolerance = 1)
})

# =============================================================================
# DIPLOMA TYPE TESTS (School Level)
# =============================================================================

test_that("2023 Yorktown High Advanced Studies count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Yorktown High Advanced Studies: 507
  yorktown_adv <- data |>
    dplyr::filter(division_number == "7",
                  school_number == "0330",
                  diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(yorktown_adv, 507, tolerance = 1)
})

test_that("2023 Yorktown High IB diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Yorktown High IB: 95
  yorktown_ib <- data |>
    dplyr::filter(division_number == "7",
                  school_number == "0330",
                  diploma_type == "ib") |>
    dplyr::pull(diploma_count)

  expect_equal(yorktown_ib, 95, tolerance = 1)
})

test_that("2023 Alexandria City High Standard diploma count matches raw source", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Alexandria City High Standard: 500
  alex_std <- data |>
    dplyr::filter(division_number == "101",
                  school_number == "0210",
                  diploma_type == "standard") |>
    dplyr::pull(diploma_count)

  expect_equal(alex_std, 500, tolerance = 1)
})

test_that("2023 Wakefield High Applied Studies is suppressed", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Wakefield High (Div 7, School 0450): Applied Studies is < (suppressed)
  wakefield_app <- data |>
    dplyr::filter(division_number == "7",
                  school_number == "0450",
                  diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(wakefield_app))
})

# =============================================================================
# DATA QUALITY TESTS (No Inf/NaN, Valid Ranges)
# =============================================================================

test_that("No Inf in 2023 graduation rates", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  expect_false(any(is.infinite(data$grad_rate)), info = "grad_rate has Inf")
})

test_that("No NaN in 2023 graduation rates", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  expect_false(any(is.nan(data$grad_rate)), info = "grad_rate has NaN")
})

test_that("All 2023 graduation rates are between 0 and 1", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  expect_true(all(data$grad_rate >= 0 & data$grad_rate <= 1, na.rm = TRUE),
             info = "grad_rate not in [0,1]")
})

test_that("All 2023 cohort sizes are non-negative", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  expect_true(all(data$cohort_size >= 0, na.rm = TRUE),
             info = "cohort_size has negative values")
})

test_that("All 2023 dropout rates are between 0 and 1", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  expect_true(all(data$dropout_rate >= 0 & data$dropout_rate <= 1, na.rm = TRUE),
             info = "dropout_rate not in [0,1]")
})

test_that("No Inf in 2021 graduation rates", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  expect_false(any(is.infinite(data$grad_rate)), info = "grad_rate has Inf")
})

test_that("No NaN in 2021 graduation rates", {
  skip_if_offline()

  data <- fetch_graduation(2021, use_cache = TRUE)

  expect_false(any(is.nan(data$grad_rate)), info = "grad_rate has NaN")
})

test_that("State graduation rate is in expected range (88-96%)", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state_rate <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  expect_true(state_rate >= 0.88 & state_rate <= 0.96,
             info = "State rate outside expected 88-96% range")
})

test_that("Cohort size is in expected range (90,000-110,000)", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_true(state_cohort >= 90000 & state_cohort <= 110000,
             info = "State cohort outside expected range")
})

# =============================================================================
# ERA DETECTION TESTS (v1 vs v2)
# =============================================================================

test_that("2019 data is detected as v1 era (no Level field)", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE, tidy = FALSE)

  # v1 should NOT have level column in raw format
  # This test verifies the era detection works
  expect_true("era" %in% names(data) || !("level" %in% names(data)))
})

test_that("2023 data is detected as v2 era (has Level field)", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE, tidy = FALSE)

  # v2 SHOULD have level column in raw format
  expect_true("level" %in% tolower(names(data)))
})

test_that("2019-2022 all use v1 schema", {
  skip_if_offline()

  for (year in c(2019, 2020, 2021, 2022)) {
    data <- fetch_graduation(year, use_cache = TRUE, tidy = FALSE)
    # All v1 years should have same structure
    expect_equal(ncol(data), 23)  # v1 has 23 columns
  }
})

test_that("2023 uses v2 schema with Level field", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE, tidy = FALSE)

  # v2 has 24 columns (includes Level)
  expect_equal(ncol(data), 24)
})

test_that("Tidy format is consistent across eras", {
  skip_if_offline()

  data_2019 <- fetch_graduation(2019, use_cache = TRUE)
  data_2023 <- fetch_graduation(2023, use_cache = TRUE)

  # Both should have same tidy columns
  expect_equal(names(data_2019), names(data_2023))
})

# =============================================================================
# SUPPRESSED VALUE HANDLING TESTS
# =============================================================================

test_that("Suppressed diploma counts become NA in tidy output", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High Applied Studies is suppressed
  suppressed <- data |>
    dplyr::filter(division_number == "1",
                  school_number == "0540",
                  diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(suppressed))
})

test_that("Zero dropout rates are NOT treated as suppressed", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Tabb High has 0% dropout (real zero, not suppression)
  zero_rate <- data |>
    dplyr::filter(school_number == "0230",
                  subgroup == "all") |>
    dplyr::pull(dropout_rate)

  expect_equal(zero_rate, 0.0, tolerance = 0.0001)
  expect_false(is.na(zero_rate))
})

test_that("Multiple suppressed values are handled correctly", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Wakefield High has multiple suppressed diploma types
  wakefield <- data |>
    dplyr::filter(school_number == "0450") |>
    dplyr::select(diploma_type, diploma_count) |>
    tidyr::drop_na(diploma_count)

  # Some diploma types should be present (not all NA)
  expect_gt(nrow(wakefield), 0)
})

test_that("State-level suppressed Applied Studies is NA", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  state_app <- data |>
    dplyr::filter(is_state,
                  diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(state_app))
})

test_that("Suppressed values don't break aggregation", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # State totals should calculate correctly even with some suppressed values
  state_cohort <- data |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  expect_gt(state_cohort, 90000)  # Should be ~98,927
})

# =============================================================================
# SCHOOL COUNT TESTS (330-340 schools per year)
# =============================================================================

test_that("2019 has expected school count", {
  skip_if_offline()

  data <- fetch_graduation(2019, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school) |>
    dplyr::distinct(school_number) |>
    nrow()

  expect_gt(school_count, 300)
  expect_lt(school_count, 400)
})

test_that("2023 has expected school count", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  school_count <- data |>
    dplyr::filter(is_school) |>
    dplyr::distinct(school_number) |>
    nrow()

  expect_gt(school_count, 300)
  expect_lt(school_count, 400)
})

test_that("School count is stable across years", {
  skip_if_offline()

  data_2019 <- fetch_graduation(2019, use_cache = TRUE)
  data_2023 <- fetch_graduation(2023, use_cache = TRUE)

  schools_2019 <- data_2019 |>
    dplyr::filter(is_school) |>
    dplyr::distinct(school_number) |>
    nrow()

  schools_2023 <- data_2023 |>
    dplyr::filter(is_school) |>
    dplyr::distinct(school_number) |>
    nrow()

  # School count shouldn't vary by more than 20 year-over-year
  expect_true(abs(schools_2019 - schools_2023) < 20)
})

# =============================================================================
# YEAR-OVER-YEAR CONSISTENCY TESTS
# =============================================================================

test_that("State graduation rate doesn't change more than 5% YoY", {
  skip_if_offline()

  data_2019 <- fetch_graduation(2019, use_cache = TRUE)
  data_2023 <- fetch_graduation(2023, use_cache = TRUE)

  rate_2019 <- data_2019 |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  rate_2023 <- data_2023 |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(grad_rate)

  yoy_change <- abs(rate_2023 - rate_2019)

  expect_true(yoy_change < 0.05, info = "YoY change > 5 percentage points")
})

test_that("State cohort size doesn't change more than 10% YoY", {
  skip_if_offline()

  data_2019 <- fetch_graduation(2019, use_cache = TRUE)
  data_2023 <- fetch_graduation(2023, use_cache = TRUE)

  cohort_2019 <- data_2019 |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  cohort_2023 <- data_2023 |>
    dplyr::filter(is_state, subgroup == "all") |>
    dplyr::pull(cohort_size)

  yoy_change <- abs(cohort_2023 - cohort_2019) / cohort_2019

  expect_true(yoy_change < 0.10, info = "YoY cohort change > 10%")
})

test_that("Arcadia High appears in all years", {
  skip_if_offline()

  for (year in c(2019, 2020, 2021, 2022, 2023)) {
    data <- fetch_graduation(year, use_cache = TRUE)

    arcadia <- data |>
      dplyr::filter(division_number == "1",
                    school_number == "0540")

    expect_gt(nrow(arcadia), 0, info = "Arcadia High missing in ", year)
  }
})

# =============================================================================
# ID FORMAT TESTS (Leading Zeros Preserved)
# =============================================================================

test_that("School numbers preserve leading zeros", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High should be "0540" not "540"
  school_ids <- data |>
    dplyr::filter(school_number == "0540") |>
    dplyr::pull(school_number)

  expect_equal(school_ids[1], "0540")
})

test_that("School numbers are 4 digits", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # All school numbers should be 4 characters (with leading zeros)
  unique_schools <- unique(data$school_number[data$is_school])

  expect_true(all(nchar(unique_schools) == 4))
})

test_that("Division numbers are character type", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Division numbers should be character (to match enrollment data)
  expect_true(is.character(data$division_number[1]))
})

# =============================================================================
# TIDY FORMAT SCHEMA TESTS
# =============================================================================

test_that("Tidy output has expected columns", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  expected_cols <- c("end_year", "division_number", "division_name",
                     "school_number", "school_name", "rate_type",
                     "cohort_size", "total_graduates", "grad_rate",
                     "completion_rate", "dropout_rate", "diploma_type",
                     "diploma_count", "dropouts", "still_enrolled",
                     "long_term_absence", "is_state", "is_district", "is_school")

  expect_true(all(expected_cols %in% names(data)))
})

test_that("Tidy output has is_state, is_district, is_school flags", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Should have type flag columns
  expect_true("is_state" %in% names(data))
  expect_true("is_district" %in% names(data))
  expect_true("is_school" %in% names(data))

  # At least one state record (2023 only)
  expect_true(any(data$is_state))

  # Should have school records
  expect_true(any(data$is_school))
})

test_that("Tidy output has diploma_type column", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # Should have diploma type for long format
  expect_true("diploma_type" %in% names(data))

  # Should have multiple diploma types
  diploma_types <- unique(data$diploma_type)
  expect_gt(length(diploma_types), 1)
})

test_that("rate_type is consistently 'on_time_4yr'", {
  skip_if_offline()

  data <- fetch_graduation(2023, use_cache = TRUE)

  # VA only reports 4-year on-time rates
  expect_true(all(unique(data$rate_type) == "on_time_4yr" | is.na(data$rate_type)))
})
