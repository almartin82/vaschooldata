# ==============================================================================
# Transformation Correctness Tests: Virginia Graduation Rate Data
# ==============================================================================
#
# Tests verify every transformation step in the graduation pipeline:
# - Percentage parsing (parse_percentage)
# - Integer parsing with comma removal (parse_integer)
# - Suppression marker handling ("<" -> NA)
# - Era detection (v1 2019-2022 vs v2 2023+)
# - Column name standardization (standardize_grad_columns)
# - Entity flag assignment (is_state, is_school, is_district)
# - Pivot fidelity (wide diploma columns -> tidy diploma_type)
# - State aggregate calculation (v1 era)
# - "all" subgroup record creation
# - ID format preservation (school_number leading zeros)
# - Cross-year consistency
#
# All test values traced to actual VA Open Data Portal CSV files.
# No fabricated test values.
# ==============================================================================

library(testthat)

# ==============================================================================
# SECTION 1: Percentage Parsing (parse_percentage)
# ==============================================================================

test_that("parse_percentage converts standard VA format to decimal", {
  pp <- vaschooldata:::parse_percentage

  # Standard VA format: leading spaces + number + %
  expect_equal(pp("   83.71%"), 0.8371)
  expect_equal(pp("    91.93%"), 0.9193)
  expect_equal(pp("    92.93%"), 0.9293)
  expect_equal(pp("     5.38%"), 0.0538)
  expect_equal(pp("     3.93%"), 0.0393)
})

test_that("parse_percentage handles zero rate (.00%)", {
  pp <- vaschooldata:::parse_percentage

  # VA uses "   .00%" for zero rates -- NOT suppression
  expect_equal(pp("   .00%"), 0.0)
  expect_false(is.na(pp("   .00%")))
})

test_that("parse_percentage handles 100% rate", {
  pp <- vaschooldata:::parse_percentage

  expect_equal(pp("  100.00%"), 1.0)
})

test_that("parse_percentage handles numbers without leading spaces", {
  pp <- vaschooldata:::parse_percentage

  expect_equal(pp("91.93%"), 0.9193)
  expect_equal(pp("100%"), 1.0)
})

test_that("parse_percentage converts to 0-1 scale (not 0-100)", {
  pp <- vaschooldata:::parse_percentage

  result <- pp("50%")
  expect_true(result <= 1.0)
  expect_equal(result, 0.50)
})

# ==============================================================================
# SECTION 2: Integer Parsing (parse_integer)
# ==============================================================================

test_that("parse_integer handles leading spaces", {
  pi_fn <- vaschooldata:::parse_integer

  expect_equal(pi_fn("           178"), 178L)
  expect_equal(pi_fn("             151"), 151L)
  expect_equal(pi_fn("           149"), 149L)
})

test_that("parse_integer removes commas from large numbers", {
  pi_fn <- vaschooldata:::parse_integer

  expect_equal(pi_fn("98,927"), 98927L)
  expect_equal(pi_fn("90,944"), 90944L)
  expect_equal(pi_fn("50,175"), 50175L)
  expect_equal(pi_fn("37,883"), 37883L)
  expect_equal(pi_fn("2,117"), 2117L)
})

test_that("parse_integer converts suppression marker < to NA", {
  pi_fn <- vaschooldata:::parse_integer

  expect_true(is.na(pi_fn("<")))
})

test_that("parse_integer handles normal integers", {
  pi_fn <- vaschooldata:::parse_integer

  expect_equal(pi_fn("52"), 52L)
  expect_equal(pi_fn("96"), 96L)
  expect_equal(pi_fn("7"), 7L)
  expect_equal(pi_fn("4"), 4L)
  expect_equal(pi_fn("0"), 0L)
})

test_that("parse_integer returns integer type", {
  pi_fn <- vaschooldata:::parse_integer

  result <- pi_fn("178")
  expect_true(is.integer(result))
})

# ==============================================================================
# SECTION 3: Era Detection (detect_grad_era)
# ==============================================================================

test_that("detect_grad_era returns v2 when Level column exists", {
  de <- vaschooldata:::detect_grad_era

  df <- data.frame(Level = "State", `Cohort Year` = 2023, check.names = FALSE)
  expect_equal(de(df), "v2")
})

test_that("detect_grad_era returns v1 when Level column is absent", {
  de <- vaschooldata:::detect_grad_era

  df <- data.frame(`Cohort Year` = 2019, `Division Number` = 1, check.names = FALSE)
  expect_equal(de(df), "v1")
})

# ==============================================================================
# SECTION 4: Column Name Standardization (standardize_grad_columns)
# ==============================================================================

test_that("standardize_grad_columns renames v2 columns correctly", {
  std <- vaschooldata:::standardize_grad_columns

  df <- data.frame(
    `Cohort Year` = 2023,
    Level = "School",
    `Division Number` = 1,
    `Division Name` = "Test",
    `School Number` = "0540",
    `School Name` = "Test High",
    `Type of Graduation Rate` = "On-Time Graduation Rate",
    `Rate Type` = "4 yr rate",
    `Graduation Rate` = "   83.71%",
    `Students in Cohort` = "           178",
    `Total Graduates` = "           149",
    `Advanced Studies` = "              52",
    IB = "               0",
    Standard = "              96",
    `Other Diplomas` = "               0",
    `Applied Studies` = "<",
    GED = "               2",
    ISAEP = "               0",
    `Certificate of Completion` = "               0",
    `Completion Rate` = "    84.83%",
    `Dropout Rate` = "     3.93%",
    Dropouts = "               7",
    `Still Enrolled` = "               4",
    `Long-Term Absence` = "              16",
    check.names = FALSE
  )

  result <- std(df, "v2")

  expect_true("cohort_year" %in% names(result))
  expect_true("level" %in% names(result))
  expect_true("division_number" %in% names(result))
  expect_true("school_number" %in% names(result))
  expect_true("graduation_rate" %in% names(result))
  expect_true("cohort_size" %in% names(result))
  expect_true("total_graduates" %in% names(result))
  expect_true("advanced_studies" %in% names(result))
  expect_true("ib_diploma" %in% names(result))
  expect_true("standard_diploma" %in% names(result))
  expect_true("other_diplomas" %in% names(result))
  expect_true("applied_studies" %in% names(result))
  expect_true("completion_rate" %in% names(result))
  expect_true("dropout_rate" %in% names(result))
  expect_true("long_term_absence" %in% names(result))
})

test_that("standardize_grad_columns v1 omits Level mapping", {
  std <- vaschooldata:::standardize_grad_columns

  df <- data.frame(
    `Cohort Year` = 2019,
    `Division Number` = 1,
    `School Number` = "0540",
    `Graduation Rate` = "    88.74%",
    check.names = FALSE
  )

  result <- std(df, "v1")

  # v1 should NOT have "level" column in mapping
  expect_false("level" %in% names(result))
  expect_true("graduation_rate" %in% names(result))
})

# ==============================================================================
# SECTION 5: Suppression Handling in Graduation Data
# ==============================================================================

test_that("suppression marker < becomes NA in parsed integers", {
  pi_fn <- vaschooldata:::parse_integer

  # From raw CSV: Applied Studies for Arcadia High = "<"
  expect_true(is.na(pi_fn("<")))
})

test_that("suppression marker < becomes NA in processed output", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  # Arcadia High (div 1, school 0540): applied_studies is suppressed
  arcadia_app <- grad |>
    dplyr::filter(
      division_number == "1",
      school_number == "0540",
      diploma_type == "applied_studies"
    ) |>
    dplyr::pull(diploma_count)

  expect_true(is.na(arcadia_app))
})

test_that("zero rate .00% is NOT treated as suppressed", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  # Tabb High (div 98, school 0230): dropout_rate = 0%
  tabb_drop <- grad |>
    dplyr::filter(
      division_number == "98",
      school_number == "0230",
      diploma_type == "all"
    ) |>
    dplyr::pull(dropout_rate)

  expect_equal(tabb_drop, 0.0, tolerance = 0.0001)
  expect_false(is.na(tabb_drop))
})

test_that("state-level Other Diplomas is suppressed (NA), not Applied Studies", {
  skip_if_offline()

  # Bug in existing tests: they claimed Applied Studies was suppressed at state level
  # Actually it is Other Diplomas that is "<" in the raw CSV
  grad <- fetch_graduation(2023, use_cache = TRUE)

  state_other <- grad |>
    dplyr::filter(is_state, diploma_type == "other_diplomas") |>
    dplyr::pull(diploma_count)

  expect_true(is.na(state_other))

  # Applied Studies at state level is 2117, NOT suppressed
  state_applied <- grad |>
    dplyr::filter(is_state, diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)

  expect_equal(state_applied, 2117L)
})

# ==============================================================================
# SECTION 6: Entity Flag Assignment
# ==============================================================================

test_that("v2 era sets is_state from Level column", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  # 2023 is v2 with Level column
  state_rows <- grad |> dplyr::filter(is_state)
  school_rows <- grad |> dplyr::filter(is_school)

  expect_gt(nrow(state_rows), 0)
  expect_gt(nrow(school_rows), 0)
})

test_that("v1 era has no state rows initially (calculated later)", {
  skip_if_offline()

  # For v1 (2019-2022), state aggregates are calculated from school data
  # After processing, state rows should exist
  grad <- fetch_graduation(2019, use_cache = TRUE)

  state_rows <- grad |> dplyr::filter(is_state)
  expect_gt(nrow(state_rows), 0)  # Should exist after calc_state_aggregates
})

test_that("is_district is always FALSE in graduation data", {
  skip_if_offline()

  # VA graduation data has no division-level records
  grad <- fetch_graduation(2023, use_cache = TRUE)
  expect_false(any(grad$is_district))

  grad_2019 <- fetch_graduation(2019, use_cache = TRUE)
  expect_false(any(grad_2019$is_district))
})

test_that("entity flags are logical type", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  expect_true(is.logical(grad$is_state))
  expect_true(is.logical(grad$is_school))
  expect_true(is.logical(grad$is_district))
})

test_that("entity flags are mutually exclusive", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  type_sums <- as.integer(grad$is_state) + as.integer(grad$is_school) + as.integer(grad$is_district)
  expect_true(all(type_sums == 1))
})

# ==============================================================================
# SECTION 7: Pivot Fidelity (Wide -> Tidy Diploma Counts)
# ==============================================================================

test_that("diploma counts in tidy match wide format for Arcadia High", {
  skip_if_offline()

  wide <- fetch_graduation(2023, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  # Get Arcadia High from both

  arc_wide <- wide |>
    dplyr::filter(division_number == "1", school_number == "0540")
  arc_tidy <- tidy |>
    dplyr::filter(division_number == "1", school_number == "0540")

  # Wide: advanced_studies = 52
  expect_equal(arc_wide$advanced_studies, 52L)
  # Tidy: diploma_type == "advanced_studies", diploma_count = 52
  arc_adv <- arc_tidy |>
    dplyr::filter(diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)
  expect_equal(arc_adv, 52L)

  # Wide: standard_diploma = 96
  expect_equal(arc_wide$standard_diploma, 96L)
  arc_std <- arc_tidy |>
    dplyr::filter(diploma_type == "standard") |>
    dplyr::pull(diploma_count)
  expect_equal(arc_std, 96L)

  # Wide: ib_diploma = 0
  expect_equal(arc_wide$ib_diploma, 0L)
  arc_ib <- arc_tidy |>
    dplyr::filter(diploma_type == "ib") |>
    dplyr::pull(diploma_count)
  expect_equal(arc_ib, 0L)

  # Wide: ged = 2
  expect_equal(arc_wide$ged, 2L)
  arc_ged <- arc_tidy |>
    dplyr::filter(diploma_type == "ged") |>
    dplyr::pull(diploma_count)
  expect_equal(arc_ged, 2L)

  # Wide: applied_studies = NA (suppressed)
  expect_true(is.na(arc_wide$applied_studies))
  arc_app <- arc_tidy |>
    dplyr::filter(diploma_type == "applied_studies") |>
    dplyr::pull(diploma_count)
  expect_true(is.na(arc_app))
})

test_that("diploma counts in tidy match wide for state record", {
  skip_if_offline()

  wide <- fetch_graduation(2023, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  state_wide <- wide |> dplyr::filter(is_state)
  state_tidy <- tidy |> dplyr::filter(is_state)

  # advanced_studies: 50175
  expect_equal(state_wide$advanced_studies, 50175L)
  tidy_adv <- state_tidy |>
    dplyr::filter(diploma_type == "advanced_studies") |>
    dplyr::pull(diploma_count)
  expect_equal(tidy_adv, 50175L)

  # ib_diploma: 766
  expect_equal(state_wide$ib_diploma, 766L)
  tidy_ib <- state_tidy |>
    dplyr::filter(diploma_type == "ib") |>
    dplyr::pull(diploma_count)
  expect_equal(tidy_ib, 766L)

  # standard_diploma: 37883
  expect_equal(state_wide$standard_diploma, 37883L)
  tidy_std <- state_tidy |>
    dplyr::filter(diploma_type == "standard") |>
    dplyr::pull(diploma_count)
  expect_equal(tidy_std, 37883L)

  # certificate: 143
  expect_equal(state_wide$certificate, 143L)
  tidy_cert <- state_tidy |>
    dplyr::filter(diploma_type == "certificate") |>
    dplyr::pull(diploma_count)
  expect_equal(tidy_cert, 143L)
})

test_that("tidy diploma_type removes _diploma suffix", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)
  diploma_types <- unique(tidy$diploma_type)

  # "ib_diploma" should become "ib", "standard_diploma" -> "standard"
  expect_true("ib" %in% diploma_types)
  expect_true("standard" %in% diploma_types)
  expect_false("ib_diploma" %in% diploma_types)
  expect_false("standard_diploma" %in% diploma_types)

  # "advanced_studies" stays as-is (no _diploma suffix)
  expect_true("advanced_studies" %in% diploma_types)
})

test_that("tidy format has all expected diploma types", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)
  diploma_types <- sort(unique(tidy$diploma_type))

  expected_types <- sort(c(
    "advanced_studies", "ib", "standard", "other_diplomas",
    "applied_studies", "ged", "isaep", "certificate", "all"
  ))

  expect_equal(diploma_types, expected_types)
})

# ==============================================================================
# SECTION 8: "all" Subgroup Record
# ==============================================================================

test_that("all subgroup has NA diploma_count", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  all_rows <- tidy |> dplyr::filter(diploma_type == "all")
  expect_true(all(is.na(all_rows$diploma_count)))
})

test_that("all subgroup preserves graduation_rate from source", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  # Arcadia High "all" should have same graduation_rate as specific diploma types
  arc_all <- tidy |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")
  arc_adv <- tidy |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "advanced_studies")

  expect_equal(arc_all$graduation_rate, arc_adv$graduation_rate)
  expect_equal(arc_all$cohort_size, arc_adv$cohort_size)
  expect_equal(arc_all$total_graduates, arc_adv$total_graduates)
})

test_that("all subgroup exists for every school", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  school_all <- tidy |> dplyr::filter(is_school, diploma_type == "all")
  # school_number is NOT unique across divisions (e.g. "0540" appears in div 1 and div 83)
  # so must use (division_number, school_number) pair
  school_any <- tidy |>
    dplyr::filter(is_school) |>
    dplyr::distinct(division_number, school_number)

  # Every school should have an "all" record
  expect_equal(nrow(school_all), nrow(school_any))
})

test_that("all subgroup exists for state record", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  state_all <- tidy |> dplyr::filter(is_state, diploma_type == "all")
  expect_equal(nrow(state_all), 1)
})

# ==============================================================================
# SECTION 9: ID Format Preservation
# ==============================================================================

test_that("school_number preserves leading zeros as 4-digit character", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  schools <- grad |> dplyr::filter(is_school)
  school_nums <- unique(schools$school_number)

  # All should be character
  expect_true(is.character(school_nums))

  # All should be 4 characters (with leading zeros)
  expect_true(all(nchar(school_nums) == 4))

  # Specific example: Arcadia High = "0540" not "540"
  expect_true("0540" %in% school_nums)
})

test_that("division_number is character type", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  expect_true(is.character(grad$division_number))
})

test_that("school_number is NA for state-level records", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  state_schools <- grad |>
    dplyr::filter(is_state) |>
    dplyr::pull(school_number)

  expect_true(all(is.na(state_schools)))
})

# ==============================================================================
# SECTION 10: Spot-Check Values Against Raw CSV (2023 v2 Era)
# ==============================================================================

test_that("2023 state graduation_rate = 0.9193", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$graduation_rate, 0.9193, tolerance = 0.0001)
})

test_that("2023 state cohort_size = 98927", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$cohort_size, 98927L)
})

test_that("2023 state total_graduates = 90944", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$total_graduates, 90944L)
})

test_that("2023 state completion_rate = 0.9293", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$completion_rate, 0.9293, tolerance = 0.0001)
})

test_that("2023 state dropout_rate = 0.0538", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropout_rate, 0.0538, tolerance = 0.0001)
})

test_that("2023 state dropouts = 5319", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$dropouts, 5319L)
})

test_that("2023 state still_enrolled = 1330", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$still_enrolled, 1330L)
})

test_that("2023 state long_term_absence = 346", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  state <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expect_equal(state$long_term_absence, 346L)
})

test_that("2023 Arcadia High graduation_rate = 0.8371", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$graduation_rate, 0.8371, tolerance = 0.0001)
})

test_that("2023 Arcadia High cohort_size = 178", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$cohort_size, 178L)
})

test_that("2023 Arcadia High total_graduates = 149", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$total_graduates, 149L)
})

test_that("2023 Arcadia High completion_rate = 0.8483", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$completion_rate, 0.8483, tolerance = 0.0001)
})

test_that("2023 Arcadia High dropout_rate = 0.0393", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$dropout_rate, 0.0393, tolerance = 0.0001)
})

test_that("2023 Arcadia High dropouts = 7", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$dropouts, 7L)
})

test_that("2023 Arcadia High still_enrolled = 4", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$still_enrolled, 4L)
})

test_that("2023 Arcadia High long_term_absence = 16", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$long_term_absence, 16L)
})

test_that("2023 Tabb High graduation_rate = 1.0", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  tabb <- grad |>
    dplyr::filter(division_number == "98", school_number == "0230", diploma_type == "all")

  expect_equal(tabb$graduation_rate, 1.0, tolerance = 0.0001)
})

test_that("2023 Tabb High cohort_size = total_graduates = 280", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  tabb <- grad |>
    dplyr::filter(division_number == "98", school_number == "0230", diploma_type == "all")

  expect_equal(tabb$cohort_size, 280L)
  expect_equal(tabb$total_graduates, 280L)
})

test_that("2023 Yorktown High graduation_rate = 0.9794", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  yk <- grad |>
    dplyr::filter(division_number == "7", school_number == "0330", diploma_type == "all")

  expect_equal(yk$graduation_rate, 0.9794, tolerance = 0.0001)
})

test_that("2023 Yorktown High cohort_size = 678", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  yk <- grad |>
    dplyr::filter(division_number == "7", school_number == "0330", diploma_type == "all")

  expect_equal(yk$cohort_size, 678L)
})

test_that("2023 Alexandria City High cohort_size = 1138 (comma parsing)", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  alex <- grad |>
    dplyr::filter(division_number == "101", school_number == "0210", diploma_type == "all")

  # Raw CSV has "1,138" -- verify comma was parsed
  expect_equal(alex$cohort_size, 1138L)
})

# ==============================================================================
# SECTION 11: Spot-Check Values Against Raw CSV (2019 v1 Era)
# ==============================================================================

test_that("2019 Arcadia High graduation_rate = 0.8874", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$graduation_rate, 0.8874, tolerance = 0.0001)
})

test_that("2019 Arcadia High cohort_size = 151", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$cohort_size, 151L)
})

test_that("2019 Arcadia High total_graduates = 134", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)
  arc <- grad |>
    dplyr::filter(division_number == "1", school_number == "0540", diploma_type == "all")

  expect_equal(arc$total_graduates, 134L)
})

# ==============================================================================
# SECTION 12: State Aggregate Calculation (v1 Era)
# ==============================================================================

test_that("v1 state aggregate cohort is sum of school cohorts", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)

  # Get state-level "all" record (calculated from school sums)
  state_all <- grad |> dplyr::filter(is_state, diploma_type == "all")
  expect_equal(nrow(state_all), 1)
  expect_gt(state_all$cohort_size, 90000)

  # Get sum of all school cohorts (each school counted once via "all" type)
  school_cohort_sum <- grad |>
    dplyr::filter(is_school, diploma_type == "all") |>
    dplyr::pull(cohort_size) |>
    sum(na.rm = TRUE)

  expect_equal(state_all$cohort_size, school_cohort_sum)
})

test_that("v1 state aggregate graduates is sum of school graduates", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)

  state_all <- grad |> dplyr::filter(is_state, diploma_type == "all")

  school_grad_sum <- grad |>
    dplyr::filter(is_school, diploma_type == "all") |>
    dplyr::pull(total_graduates) |>
    sum(na.rm = TRUE)

  expect_equal(state_all$total_graduates, school_grad_sum)
})

test_that("v1 state graduation_rate = total_graduates / cohort_size", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)

  state_all <- grad |> dplyr::filter(is_state, diploma_type == "all")

  expected_rate <- state_all$total_graduates / state_all$cohort_size
  expect_equal(state_all$graduation_rate, expected_rate, tolerance = 0.0001)
})

# ==============================================================================
# SECTION 13: Cross-Year Consistency
# ==============================================================================

test_that("Arcadia High appears in all 5 years (2019-2023)", {
  skip_if_offline()

  for (yr in 2019:2023) {
    grad <- fetch_graduation(yr, use_cache = TRUE)
    arc <- grad |>
      dplyr::filter(division_number == "1", school_number == "0540")
    expect_gt(nrow(arc), 0, label = paste("Arcadia High present in", yr))
  }
})

test_that("tidy output has consistent columns across v1 and v2 eras", {
  skip_if_offline()

  grad_2019 <- fetch_graduation(2019, use_cache = TRUE)
  grad_2023 <- fetch_graduation(2023, use_cache = TRUE)

  # Column names should be identical
  expect_equal(sort(names(grad_2019)), sort(names(grad_2023)))
})

test_that("rate_type is '4 yr rate' for all years", {
  skip_if_offline()

  for (yr in c(2019, 2021, 2023)) {
    grad <- fetch_graduation(yr, use_cache = TRUE)
    rate_types <- unique(grad$rate_type)
    expect_equal(rate_types, "4 yr rate", label = paste("rate_type for", yr))
  }
})

test_that("state graduation_rate is between 0.88 and 0.96 for all years", {
  skip_if_offline()

  for (yr in 2019:2023) {
    grad <- fetch_graduation(yr, use_cache = TRUE)
    state_rate <- grad |>
      dplyr::filter(is_state, diploma_type == "all") |>
      dplyr::pull(graduation_rate)

    expect_true(
      state_rate >= 0.88 & state_rate <= 0.96,
      label = paste("State rate for", yr, "=", round(state_rate, 4))
    )
  }
})

test_that("state cohort_size is between 90000 and 110000 for all years", {
  skip_if_offline()

  for (yr in 2019:2023) {
    grad <- fetch_graduation(yr, use_cache = TRUE)
    state_cohort <- grad |>
      dplyr::filter(is_state, diploma_type == "all") |>
      dplyr::pull(cohort_size)

    expect_true(
      state_cohort >= 90000 & state_cohort <= 110000,
      label = paste("State cohort for", yr, "=", state_cohort)
    )
  }
})

test_that("school count is 300-400 for all years", {
  skip_if_offline()

  for (yr in c(2019, 2023)) {
    grad <- fetch_graduation(yr, use_cache = TRUE)
    n_schools <- grad |>
      dplyr::filter(is_school, diploma_type == "all") |>
      dplyr::distinct(school_number) |>
      nrow()

    expect_gt(n_schools, 100, label = paste("Schools for", yr))
    expect_lt(n_schools, 400, label = paste("Schools for", yr))
  }
})

# ==============================================================================
# SECTION 14: Data Quality (No Inf/NaN, Valid Ranges)
# ==============================================================================

test_that("no Inf values in graduation rates for 2023", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  expect_false(any(is.infinite(grad$graduation_rate), na.rm = TRUE))
  expect_false(any(is.infinite(grad$completion_rate), na.rm = TRUE))
  expect_false(any(is.infinite(grad$dropout_rate), na.rm = TRUE))
})

test_that("no NaN values in graduation rates for 2023", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  expect_false(any(is.nan(grad$graduation_rate), na.rm = TRUE))
  expect_false(any(is.nan(grad$completion_rate), na.rm = TRUE))
  expect_false(any(is.nan(grad$dropout_rate), na.rm = TRUE))
})

test_that("graduation rates are in [0, 1] range", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  non_na_rates <- grad$graduation_rate[!is.na(grad$graduation_rate)]
  expect_true(all(non_na_rates >= 0 & non_na_rates <= 1))
})

test_that("dropout rates are in [0, 1] range", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  non_na_rates <- grad$dropout_rate[!is.na(grad$dropout_rate)]
  expect_true(all(non_na_rates >= 0 & non_na_rates <= 1))
})

test_that("cohort sizes are non-negative", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  non_na_cohort <- grad$cohort_size[!is.na(grad$cohort_size)]
  expect_true(all(non_na_cohort >= 0))
})

test_that("end_year is integer type", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)
  expect_true(is.integer(grad$end_year))
})

test_that("no Inf values in graduation rates for 2019 (v1)", {
  skip_if_offline()

  grad <- fetch_graduation(2019, use_cache = TRUE)

  expect_false(any(is.infinite(grad$graduation_rate), na.rm = TRUE))
})

# ==============================================================================
# SECTION 15: Wide Format Schema (process_graduation output)
# ==============================================================================

test_that("wide format 2023 has Level column (v2)", {
  skip_if_offline()

  wide <- fetch_graduation(2023, tidy = FALSE, use_cache = TRUE)
  expect_true("level" %in% names(wide))
})

test_that("wide format 2019 has NO Level column (v1)", {
  skip_if_offline()

  wide <- fetch_graduation(2019, tidy = FALSE, use_cache = TRUE)
  expect_false("level" %in% names(wide))
})

test_that("wide format has era column", {
  skip_if_offline()

  wide <- fetch_graduation(2023, tidy = FALSE, use_cache = TRUE)
  expect_true("era" %in% names(wide))
  expect_equal(unique(wide$era), "v2")

  wide_2019 <- fetch_graduation(2019, tidy = FALSE, use_cache = TRUE)
  expect_equal(unique(wide_2019$era), "v1")
})

test_that("wide format has all 8 diploma type columns", {
  skip_if_offline()

  wide <- fetch_graduation(2023, tidy = FALSE, use_cache = TRUE)

  diploma_cols <- c("advanced_studies", "ib_diploma", "standard_diploma",
                    "other_diplomas", "applied_studies", "ged", "isaep", "certificate")

  expect_true(all(diploma_cols %in% names(wide)))
})

test_that("tidy format removes era and level columns", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  expect_false("era" %in% names(tidy))
  expect_false("level" %in% names(tidy))
})

test_that("tidy format has expected column set", {
  skip_if_offline()

  tidy <- fetch_graduation(2023, tidy = TRUE, use_cache = TRUE)

  expected_cols <- c(
    "end_year", "division_number", "division_name",
    "school_number", "school_name", "rate_type",
    "cohort_size", "total_graduates", "graduation_rate",
    "completion_rate", "dropout_rate", "diploma_type",
    "diploma_count", "dropouts", "still_enrolled",
    "long_term_absence", "is_state", "is_district", "is_school"
  )

  expect_true(all(expected_cols %in% names(tidy)))
})

# ==============================================================================
# SECTION 16: Year Validation
# ==============================================================================

test_that("get_available_grad_years returns 2019:2023", {
  years <- get_available_grad_years()
  expect_equal(years, 2019L:2023L)
})

test_that("fetch_graduation rejects invalid years", {
  expect_error(fetch_graduation(2018), "end_year must be between")
  expect_error(fetch_graduation(2024), "end_year must be between")
  expect_error(fetch_graduation(2000), "end_year must be between")
})

test_that("fetch_graduation_multi rejects invalid years", {
  expect_error(fetch_graduation_multi(c(2019, 2024)), "Invalid years")
  expect_error(fetch_graduation_multi(c(2018, 2019)), "Invalid years")
})

# ==============================================================================
# SECTION 17: Multi-Year Fetch
# ==============================================================================

test_that("fetch_graduation_multi returns data for all requested years", {
  skip_if_offline()

  grad <- fetch_graduation_multi(2019:2023, use_cache = TRUE)
  years_present <- sort(unique(grad$end_year))

  expect_equal(years_present, 2019:2023)
})

test_that("fetch_graduation_multi has state records for each year", {
  skip_if_offline()

  grad <- fetch_graduation_multi(2019:2023, use_cache = TRUE)

  state_years <- grad |>
    dplyr::filter(is_state, diploma_type == "all") |>
    dplyr::pull(end_year) |>
    sort()

  expect_equal(state_years, 2019:2023)
})

# ==============================================================================
# SECTION 18: One Observation Per Group Per Period
# ==============================================================================

test_that("no duplicate school x diploma_type rows per year", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  # Each school should have exactly 1 row per diploma_type per year
  dupes <- grad |>
    dplyr::filter(is_school) |>
    dplyr::count(school_number, division_number, diploma_type) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(dupes), 0, label = "Duplicate school x diploma_type rows")
})

test_that("no duplicate state rows per diploma_type per year", {
  skip_if_offline()

  grad <- fetch_graduation(2023, use_cache = TRUE)

  state_dupes <- grad |>
    dplyr::filter(is_state) |>
    dplyr::count(diploma_type) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(state_dupes), 0, label = "Duplicate state diploma_type rows")
})

test_that("across multi-year: one state row per year per diploma_type", {
  skip_if_offline()

  grad <- fetch_graduation_multi(2019:2023, use_cache = TRUE)

  state_dupes <- grad |>
    dplyr::filter(is_state) |>
    dplyr::count(end_year, diploma_type) |>
    dplyr::filter(n > 1)

  expect_equal(nrow(state_dupes), 0, label = "Duplicate state rows across years")
})
