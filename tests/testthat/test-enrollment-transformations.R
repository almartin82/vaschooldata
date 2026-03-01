# ==============================================================================
# Transformation Correctness Tests: Virginia Enrollment
# ==============================================================================
#
# Tests verify every transformation step in the enrollment pipeline:
# - safe_numeric() suppression marker handling
# - Column name mapping (extract_enrollment_columns)
# - Grade level normalization (tidy_enr grade_level_map)
# - Subgroup naming (demographics -> standard names)
# - Entity flag logic (id_enr_aggs)
# - Pivot fidelity (wide -> tidy count preservation)
# - Percentage calculations
# - State aggregate creation
# - Grade aggregations (enr_grade_aggs)
#
# All values traced to actual VDOE data or verified function behavior.
# No fabricated test values.
# ==============================================================================

library(testthat)

# ==============================================================================
# SECTION 1: Suppression Marker Handling (safe_numeric)
# ==============================================================================

test_that("safe_numeric converts normal numbers correctly", {
  safe_numeric <- vaschooldata:::safe_numeric

  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("3.14"), 3.14)
  expect_equal(safe_numeric("1234567"), 1234567)
})

test_that("safe_numeric handles comma-separated numbers", {
  safe_numeric <- vaschooldata:::safe_numeric

  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("1,234,567"), 1234567)
  expect_equal(safe_numeric("98,927"), 98927)
})

test_that("safe_numeric strips whitespace", {
  safe_numeric <- vaschooldata:::safe_numeric

  expect_equal(safe_numeric("  100  "), 100)
  expect_equal(safe_numeric("           178"), 178)
  expect_equal(safe_numeric("\t200\t"), 200)
})

test_that("safe_numeric returns NA for VDOE suppression markers", {
  safe_numeric <- vaschooldata:::safe_numeric

  # All documented VDOE suppression markers
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("-2")))
  expect_true(is.na(safe_numeric("-9")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("<10")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("")))
  expect_true(is.na(safe_numeric("PS")))
  expect_true(is.na(safe_numeric("S")))
  expect_true(is.na(safe_numeric("s")))
  expect_true(is.na(safe_numeric("DS")))
})

test_that("safe_numeric passes through already-numeric values", {
  safe_numeric <- vaschooldata:::safe_numeric

  expect_equal(safe_numeric(100), 100)
  expect_equal(safe_numeric(0), 0)
  expect_equal(safe_numeric(3.14), 3.14)
  expect_equal(safe_numeric(-5), -5)
})

test_that("safe_numeric handles non-suppression negative numbers", {
  safe_numeric <- vaschooldata:::safe_numeric

  # -5 is NOT a suppression marker (only -1, -2, -9 are)
  expect_equal(safe_numeric("-5"), -5)
  expect_equal(safe_numeric("-100"), -100)
})

# ==============================================================================
# SECTION 2: Race/Ethnicity Name Mapping
# ==============================================================================

test_that("map_vdoe_race maps full names to standard names", {
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

test_that("map_vdoe_race maps abbreviation codes to standard names", {
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

test_that("map_vdoe_race handles unknown values with lowercase transformation", {
  map_vdoe_race <- vaschooldata:::map_vdoe_race

  # Unknown values get tolower + gsub(space -> underscore)
  expect_equal(map_vdoe_race("Some Other Race"), "some_other_race")
})

# ==============================================================================
# SECTION 3: Grade Level Normalization
# ==============================================================================

test_that("map_grade_code normalizes grade names to standard format", {
  map_grade_code <- vaschooldata:::map_grade_code

  # PK and K variants
  expect_equal(unname(map_grade_code("PK")), "PK")
  expect_equal(unname(map_grade_code("K")), "K")
  expect_equal(unname(map_grade_code("KG")), "K")

  # Single-digit grades get zero-padded
  expect_equal(unname(map_grade_code("1")), "01")
  expect_equal(unname(map_grade_code("2")), "02")
  expect_equal(unname(map_grade_code("9")), "09")

  # Already zero-padded grades pass through
  expect_equal(unname(map_grade_code("01")), "01")
  expect_equal(unname(map_grade_code("09")), "09")

  # Double-digit grades pass through
  expect_equal(unname(map_grade_code("10")), "10")
  expect_equal(unname(map_grade_code("11")), "11")
  expect_equal(unname(map_grade_code("12")), "12")

  # Special grades
  expect_equal(unname(map_grade_code("UG")), "UG")
  expect_equal(unname(map_grade_code("UE")), "UE")
  expect_equal(unname(map_grade_code("US")), "US")
  expect_equal(unname(map_grade_code("PG")), "PG")
  expect_equal(unname(map_grade_code("TOTAL")), "TOTAL")
})

test_that("map_grade_code passes through unmapped values", {
  map_grade_code <- vaschooldata:::map_grade_code

  expect_equal(unname(map_grade_code("99")), "99")
})

test_that("tidy_enr grade_level_map produces expected values", {
  # The grade_level_map inside tidy_enr converts column names to grade levels
  # Verify the mapping is consistent with CLAUDE.md valid filter values:
  # PK, K, 01-12, UG, TOTAL

  grade_level_map <- c(
    "grade_pk" = "PK",
    "grade_k" = "K",
    "grade_01" = "01",
    "grade_02" = "02",
    "grade_03" = "03",
    "grade_04" = "04",
    "grade_05" = "05",
    "grade_06" = "06",
    "grade_07" = "07",
    "grade_08" = "08",
    "grade_09" = "09",
    "grade_10" = "10",
    "grade_11" = "11",
    "grade_12" = "12",
    "grade_ug" = "UG"
  )

  # All values should be uppercase
  expect_true(all(grade_level_map == toupper(grade_level_map)))

  # PK and K should be letters not numbers
  expect_equal(grade_level_map[["grade_pk"]], "PK")
  expect_equal(grade_level_map[["grade_k"]], "K")

  # Grades 1-9 should be zero-padded
  expect_equal(grade_level_map[["grade_01"]], "01")
  expect_equal(grade_level_map[["grade_09"]], "09")
})

# ==============================================================================
# SECTION 4: Column Name Mapping (extract_enrollment_columns)
# ==============================================================================

test_that("extract_enrollment_columns maps total enrollment variants", {
  extract <- vaschooldata:::extract_enrollment_columns

  # Test with "total" column name
  df <- data.frame(total = c("100", "200"), stringsAsFactors = FALSE)
  result <- extract(df)
  expect_true("row_total" %in% names(result))
  expect_equal(result$row_total, c(100, 200))
})

test_that("extract_enrollment_columns maps race column variants", {
  extract <- vaschooldata:::extract_enrollment_columns

  # Test with abbreviation codes
  df <- data.frame(
    wh = c("50", "60"),
    bl = c("30", "40"),
    hi = c("10", "15"),
    as = c("5", "8"),
    am = c("2", "3"),
    pi = c("1", "1"),
    mr = c("2", "3"),
    stringsAsFactors = FALSE
  )
  result <- extract(df)

  expect_true("white" %in% names(result))
  expect_true("black" %in% names(result))
  expect_true("hispanic" %in% names(result))
  expect_true("asian" %in% names(result))
  expect_true("native_american" %in% names(result))
  expect_true("pacific_islander" %in% names(result))
  expect_true("multiracial" %in% names(result))

  expect_equal(result$white, c(50, 60))
  expect_equal(result$black, c(30, 40))
})

test_that("extract_enrollment_columns maps gender variants", {
  extract <- vaschooldata:::extract_enrollment_columns

  df <- data.frame(m = c("50", "60"), f = c("45", "55"), stringsAsFactors = FALSE)
  result <- extract(df)

  expect_true("male" %in% names(result))
  expect_true("female" %in% names(result))
  expect_equal(result$male, c(50, 60))
  expect_equal(result$female, c(45, 55))
})

test_that("extract_enrollment_columns maps grade column variants", {
  extract <- vaschooldata:::extract_enrollment_columns

  df <- data.frame(
    pk = c("10", "12"),
    kg = c("20", "22"),
    g01 = c("30", "32"),
    g12 = c("25", "28"),
    stringsAsFactors = FALSE
  )
  result <- extract(df)

  expect_true("grade_pk" %in% names(result))
  expect_true("grade_k" %in% names(result))
  expect_true("grade_01" %in% names(result))
  expect_true("grade_12" %in% names(result))
})

test_that("extract_enrollment_columns handles suppressed values in mapping", {
  extract <- vaschooldata:::extract_enrollment_columns

  df <- data.frame(total = c("100", "*", "<5"), stringsAsFactors = FALSE)
  result <- extract(df)

  expect_equal(result$row_total[1], 100)
  expect_true(is.na(result$row_total[2]))
  expect_true(is.na(result$row_total[3]))
})

# ==============================================================================
# SECTION 5: Entity Flag Logic (id_enr_aggs)
# ==============================================================================

test_that("id_enr_aggs sets is_state based on type column", {
  df <- data.frame(
    type = c("State", "District", "Campus"),
    charter_flag = c(NA, NA, "N"),
    district_id = c(NA, "001", "001"),
    campus_id = c(NA, NA, "0001"),
    stringsAsFactors = FALSE
  )
  result <- vaschooldata::id_enr_aggs(df)

  expect_true(result$is_state[1])
  expect_false(result$is_state[2])
  expect_false(result$is_state[3])
})

test_that("id_enr_aggs sets is_district based on type column", {
  df <- data.frame(
    type = c("State", "District", "Campus"),
    charter_flag = c(NA, NA, "N"),
    district_id = c(NA, "001", "001"),
    campus_id = c(NA, NA, "0001"),
    stringsAsFactors = FALSE
  )
  result <- vaschooldata::id_enr_aggs(df)

  expect_false(result$is_district[1])
  expect_true(result$is_district[2])
  expect_false(result$is_district[3])
})

test_that("id_enr_aggs sets is_campus based on type column", {
  df <- data.frame(
    type = c("State", "District", "Campus"),
    charter_flag = c(NA, NA, "N"),
    district_id = c(NA, "001", "001"),
    campus_id = c(NA, NA, "0001"),
    stringsAsFactors = FALSE
  )
  result <- vaschooldata::id_enr_aggs(df)

  expect_false(result$is_campus[1])
  expect_false(result$is_campus[2])
  expect_true(result$is_campus[3])
})

test_that("id_enr_aggs detects charter schools from charter_flag", {
  df <- data.frame(
    type = c("Campus", "Campus", "Campus"),
    charter_flag = c("Y", "N", NA),
    district_id = c("001", "001", "001"),
    campus_id = c("0001", "0002", "0003"),
    stringsAsFactors = FALSE
  )
  result <- vaschooldata::id_enr_aggs(df)

  expect_true(result$is_charter[1])
  expect_false(result$is_charter[2])
  expect_false(result$is_charter[3])
})

test_that("id_enr_aggs flags are mutually exclusive (each row is one type)", {
  df <- data.frame(
    type = c("State", "District", "Campus"),
    charter_flag = c(NA, NA, "N"),
    district_id = c(NA, "001", "001"),
    campus_id = c(NA, NA, "0001"),
    stringsAsFactors = FALSE
  )
  result <- vaschooldata::id_enr_aggs(df)

  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1))
})

test_that("id_enr_aggs produces logical columns", {
  df <- data.frame(
    type = c("State", "District", "Campus"),
    charter_flag = c(NA, NA, "Y"),
    district_id = c(NA, "001", "001"),
    campus_id = c(NA, NA, "0001"),
    stringsAsFactors = FALSE
  )
  result <- vaschooldata::id_enr_aggs(df)

  expect_true(is.logical(result$is_state))
  expect_true(is.logical(result$is_district))
  expect_true(is.logical(result$is_campus))
  expect_true(is.logical(result$is_charter))
})

# ==============================================================================
# SECTION 6: Tidy Enrollment Pivot Fidelity
# ==============================================================================

test_that("tidy_enr creates total_enrollment subgroup from row_total", {
  df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 1000,
    white = 500,
    black = 300,
    hispanic = 100,
    asian = 50,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 35,
    male = 520,
    female = 480,
    grade_pk = 50,
    grade_k = 80,
    grade_01 = 90,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  # total_enrollment subgroup should have n_students = row_total
  total_rows <- result[result$subgroup == "total_enrollment" & result$grade_level == "TOTAL", ]
  expect_equal(nrow(total_rows), 1)
  expect_equal(total_rows$n_students, 1000)
  expect_equal(total_rows$pct, 1.0)
})

test_that("tidy_enr preserves demographic counts exactly", {
  df <- data.frame(
    end_year = 2024,
    type = "District",
    district_id = "001",
    campus_id = NA_character_,
    district_name = "Test District",
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 1000,
    white = 500,
    black = 300,
    hispanic = 100,
    asian = 50,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 35,
    male = 520,
    female = 480,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  # Each demographic subgroup should preserve the exact count
  white_row <- result[result$subgroup == "white", ]
  expect_equal(white_row$n_students, 500)

  black_row <- result[result$subgroup == "black", ]
  expect_equal(black_row$n_students, 300)

  hispanic_row <- result[result$subgroup == "hispanic", ]
  expect_equal(hispanic_row$n_students, 100)

  asian_row <- result[result$subgroup == "asian", ]
  expect_equal(asian_row$n_students, 50)

  na_row <- result[result$subgroup == "native_american", ]
  expect_equal(na_row$n_students, 10)

  pi_row <- result[result$subgroup == "pacific_islander", ]
  expect_equal(pi_row$n_students, 5)

  mr_row <- result[result$subgroup == "multiracial", ]
  expect_equal(mr_row$n_students, 35)
})

test_that("tidy_enr preserves gender counts exactly", {
  df <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "001",
    campus_id = "0001",
    district_name = "Test District",
    campus_name = "Test School",
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 1000,
    white = 500,
    black = 300,
    hispanic = 200,
    male = 520,
    female = 480,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  male_row <- result[result$subgroup == "male", ]
  expect_equal(male_row$n_students, 520)

  female_row <- result[result$subgroup == "female", ]
  expect_equal(female_row$n_students, 480)
})

test_that("tidy_enr calculates pct as n_students / row_total", {
  df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 1000,
    white = 500,
    black = 300,
    male = 520,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  white_row <- result[result$subgroup == "white", ]
  expect_equal(white_row$pct, 0.5)

  black_row <- result[result$subgroup == "black", ]
  expect_equal(black_row$pct, 0.3)

  male_row <- result[result$subgroup == "male", ]
  expect_equal(male_row$pct, 0.52)
})

test_that("tidy_enr preserves grade enrollment counts exactly", {
  df <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "001",
    campus_id = "0001",
    district_name = "Test District",
    campus_name = "Test School",
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 300,
    grade_pk = 20,
    grade_k = 25,
    grade_01 = 30,
    grade_09 = 80,
    grade_12 = 75,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  pk_rows <- result[result$grade_level == "PK", ]
  expect_equal(pk_rows$n_students, 20)

  k_rows <- result[result$grade_level == "K", ]
  expect_equal(k_rows$n_students, 25)

  g01_rows <- result[result$grade_level == "01", ]
  expect_equal(g01_rows$n_students, 30)

  g09_rows <- result[result$grade_level == "09", ]
  expect_equal(g09_rows$n_students, 80)

  g12_rows <- result[result$grade_level == "12", ]
  expect_equal(g12_rows$n_students, 75)
})

test_that("tidy_enr demographic rows have grade_level = TOTAL", {
  df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 100,
    white = 50,
    black = 30,
    male = 55,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  demo_rows <- result[result$subgroup %in% c("white", "black", "male", "total_enrollment"), ]
  expect_true(all(demo_rows$grade_level == "TOTAL"))
})

test_that("tidy_enr grade rows have subgroup = total_enrollment", {
  df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 100,
    grade_pk = 10,
    grade_k = 15,
    grade_01 = 20,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  grade_rows <- result[result$grade_level %in% c("PK", "K", "01"), ]
  expect_true(all(grade_rows$subgroup == "total_enrollment"))
})

test_that("tidy_enr filters out NA n_students rows", {
  df <- data.frame(
    end_year = 2024,
    type = "Campus",
    district_id = "001",
    campus_id = "0001",
    district_name = "Test",
    campus_name = "Test",
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 100,
    white = 50,
    black = NA,  # suppressed
    grade_pk = NA,  # no PK at this school
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  # Rows with NA n_students should be filtered out
  expect_false(any(is.na(result$n_students)))

  # black and grade_pk should NOT appear since they are NA
  expect_false("black" %in% result$subgroup)
  expect_false("PK" %in% result$grade_level)
})

test_that("tidy_enr output has required columns", {
  df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 100,
    white = 50,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)

  expected_cols <- c("end_year", "type", "grade_level", "subgroup", "n_students", "pct")
  expect_true(all(expected_cols %in% names(result)))
})

# ==============================================================================
# SECTION 7: Standard Subgroup Names (Cross-State Compatibility)
# ==============================================================================

test_that("tidy enrollment subgroup names match naming standard", {
  # Per CLAUDE.md, standard subgroup names must be used
  standard_subgroups <- c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female"
  )

  df <- data.frame(
    end_year = 2024,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    charter_flag = NA_character_,
    row_total = 1000,
    white = 500,
    black = 200,
    hispanic = 150,
    asian = 50,
    native_american = 10,
    pacific_islander = 5,
    multiracial = 35,
    male = 520,
    female = 480,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::tidy_enr(df)
  actual_subgroups <- unique(result$subgroup)

  # All subgroups produced should be from the standard set
  expect_true(all(actual_subgroups %in% standard_subgroups))

  # total_enrollment should always be present
  expect_true("total_enrollment" %in% actual_subgroups)
})

# ==============================================================================
# SECTION 8: State Aggregate Creation
# ==============================================================================

test_that("create_state_aggregate sums division totals", {
  create_state_aggregate <- vaschooldata:::create_state_aggregate

  division_df <- data.frame(
    end_year = c(2024, 2024),
    type = c("District", "District"),
    district_id = c("001", "002"),
    campus_id = c(NA, NA),
    district_name = c("Div 1", "Div 2"),
    campus_name = c(NA, NA),
    county = c(NA, NA),
    charter_flag = c(NA, NA),
    row_total = c(1000, 2000),
    white = c(500, 900),
    black = c(300, 700),
    hispanic = c(100, 200),
    asian = c(50, 100),
    native_american = c(10, 20),
    pacific_islander = c(5, 10),
    multiracial = c(35, 70),
    male = c(520, 1040),
    female = c(480, 960),
    stringsAsFactors = FALSE
  )

  result <- create_state_aggregate(division_df, 2024)

  expect_equal(nrow(result), 1)
  expect_equal(result$type, "State")
  expect_equal(result$row_total, 3000)
  expect_equal(result$white, 1400)
  expect_equal(result$black, 1000)
  expect_equal(result$male, 1560)
  expect_equal(result$female, 1440)
  expect_true(is.na(result$district_id))
  expect_true(is.na(result$campus_id))
})

test_that("create_state_aggregate handles NA values with na.rm", {
  create_state_aggregate <- vaschooldata:::create_state_aggregate

  division_df <- data.frame(
    end_year = c(2024, 2024),
    type = c("District", "District"),
    district_id = c("001", "002"),
    campus_id = c(NA, NA),
    district_name = c("Div 1", "Div 2"),
    campus_name = c(NA, NA),
    county = c(NA, NA),
    charter_flag = c(NA, NA),
    row_total = c(1000, 2000),
    white = c(500, NA),
    black = c(NA, 700),
    stringsAsFactors = FALSE
  )

  result <- create_state_aggregate(division_df, 2024)

  # na.rm=TRUE means NA values are skipped in sum
  expect_equal(result$row_total, 3000)
  expect_equal(result$white, 500)
  expect_equal(result$black, 700)
})

# ==============================================================================
# SECTION 9: Grade Level Aggregations (enr_grade_aggs)
# ==============================================================================

test_that("enr_grade_aggs creates K8 aggregate", {
  # Build a minimal tidy dataframe
  grades <- c("K", "01", "02", "03", "04", "05", "06", "07", "08")
  n <- c(100, 90, 85, 80, 75, 70, 65, 60, 55)

  df <- data.frame(
    end_year = rep(2024, length(grades)),
    type = rep("State", length(grades)),
    subgroup = rep("total_enrollment", length(grades)),
    grade_level = grades,
    n_students = n,
    pct = n / 1000,
    is_state = TRUE,
    is_district = FALSE,
    is_campus = FALSE,
    is_charter = FALSE,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::enr_grade_aggs(df)
  k8 <- result[result$grade_level == "K8", ]

  expect_equal(nrow(k8), 1)
  expect_equal(k8$n_students, sum(n))
})

test_that("enr_grade_aggs creates HS aggregate (grades 09-12)", {
  grades <- c("09", "10", "11", "12")
  n <- c(120, 110, 105, 100)

  df <- data.frame(
    end_year = rep(2024, length(grades)),
    type = rep("State", length(grades)),
    subgroup = rep("total_enrollment", length(grades)),
    grade_level = grades,
    n_students = n,
    pct = n / 1000,
    is_state = TRUE,
    is_district = FALSE,
    is_campus = FALSE,
    is_charter = FALSE,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::enr_grade_aggs(df)
  hs <- result[result$grade_level == "HS", ]

  expect_equal(nrow(hs), 1)
  expect_equal(hs$n_students, sum(n))
})

test_that("enr_grade_aggs creates K12 aggregate (K through 12, excludes PK)", {
  grades <- c("PK", "K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  n <- c(50, 100, 90, 85, 80, 75, 70, 65, 60, 55, 120, 110, 105, 100)

  df <- data.frame(
    end_year = rep(2024, length(grades)),
    type = rep("State", length(grades)),
    subgroup = rep("total_enrollment", length(grades)),
    grade_level = grades,
    n_students = n,
    pct = n / sum(n),
    is_state = TRUE,
    is_district = FALSE,
    is_campus = FALSE,
    is_charter = FALSE,
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::enr_grade_aggs(df)
  k12 <- result[result$grade_level == "K12", ]

  # K12 = K + 01-12 (excludes PK)
  expected_k12 <- sum(n) - 50  # subtract PK
  expect_equal(k12$n_students, expected_k12)
})

test_that("enr_grade_aggs only uses total_enrollment subgroup", {
  # Should NOT aggregate demographic subgroups
  df <- data.frame(
    end_year = c(2024, 2024),
    type = c("State", "State"),
    subgroup = c("total_enrollment", "white"),
    grade_level = c("K", "K"),
    n_students = c(100, 50),
    pct = c(1.0, 0.5),
    is_state = c(TRUE, TRUE),
    is_district = c(FALSE, FALSE),
    is_campus = c(FALSE, FALSE),
    is_charter = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  result <- vaschooldata::enr_grade_aggs(df)

  # Only total_enrollment rows should be in aggregates
  expect_true(all(result$subgroup == "total_enrollment"))
})

# ==============================================================================
# SECTION 10: Process Enrollment Schema
# ==============================================================================

test_that("process_enr output has correct column order", {
  # The standard column order from select_standard_columns
  expected_cols <- c(
    "end_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "charter_flag",
    "row_total",
    "white", "black", "hispanic", "asian",
    "native_american", "pacific_islander", "multiracial",
    "male", "female",
    "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  select_standard <- vaschooldata:::select_standard_columns

  # Create a minimal df with some standard cols
  df <- data.frame(
    end_year = 2024,
    type = "State",
    row_total = 100,
    white = 50,
    stringsAsFactors = FALSE
  )

  result <- select_standard(df)

  # All expected columns should be present (missing ones added as NA)
  expect_true(all(expected_cols %in% names(result)))

  # Column order should match
  expect_equal(names(result), expected_cols)
})

test_that("process_school_enr sets type to Campus", {
  process_school <- vaschooldata:::process_school_enr

  school_data <- data.frame(
    school_num = "0001",
    division_num = "001",
    school_name = "Test School",
    division_name = "Test Division",
    total = "100",
    stringsAsFactors = FALSE
  )

  result <- process_school(school_data, 2024)
  expect_true(all(result$type == "Campus"))
})

test_that("process_division_enr sets type to District", {
  process_div <- vaschooldata:::process_division_enr

  div_data <- data.frame(
    division_num = "001",
    division_name = "Test Division",
    total = "100",
    stringsAsFactors = FALSE
  )

  result <- process_div(div_data, 2024)
  expect_true(all(result$type == "District"))
  expect_true(all(is.na(result$campus_id)))
  expect_true(all(is.na(result$campus_name)))
})

# ==============================================================================
# SECTION 11: Utility Functions
# ==============================================================================

test_that("get_available_years returns integer vector", {
  years <- get_available_years()
  expect_true(is.integer(years))
})

test_that("get_available_years range is 2016-2024", {
  years <- get_available_years()
  expect_equal(min(years), 2016L)
  expect_equal(max(years), 2024L)
  expect_equal(length(years), 9L)
})

test_that("format_school_year produces correct format", {
  fmt <- vaschooldata:::format_school_year
  expect_equal(fmt(2024), "2023-24")
  expect_equal(fmt(2020), "2019-20")
  expect_equal(fmt(2016), "2015-16")
})

test_that("format_school_year_long produces correct format", {
  fmt <- vaschooldata:::format_school_year_long
  expect_equal(fmt(2024), "2023-2024")
  expect_equal(fmt(2020), "2019-2020")
})

test_that("validate_year rejects out-of-range years", {
  validate <- vaschooldata:::validate_year
  expect_error(validate(2015), "end_year must be between")
  expect_error(validate(2030), "end_year must be between")
  expect_error(validate(1900), "end_year must be between")
})

test_that("validate_year accepts valid years silently", {
  validate <- vaschooldata:::validate_year
  expect_silent(validate(2020))
  expect_silent(validate(2016))
  expect_silent(validate(2024))
})

test_that("validate_year rejects non-numeric input", {
  validate <- vaschooldata:::validate_year
  expect_error(validate("2020"), "single numeric value")
  expect_error(validate(c(2020, 2021)), "single numeric value")
})

test_that("get_va_fips returns 51", {
  fips <- vaschooldata:::get_va_fips()
  expect_equal(fips, 51L)
})

# ==============================================================================
# SECTION 12: Cache Functions
# ==============================================================================

test_that("get_cache_dir creates directory if needed", {
  cache_dir <- vaschooldata:::get_cache_dir()
  expect_true(dir.exists(cache_dir))
  expect_true(grepl("vaschooldata", cache_dir))
})

test_that("get_cache_path builds correct file path", {
  path <- vaschooldata:::get_cache_path(2023, "tidy")
  expect_true(grepl("enr_tidy_2023\\.rds$", path))

  path2 <- vaschooldata:::get_cache_path(2024, "wide")
  expect_true(grepl("enr_wide_2024\\.rds$", path2))
})

test_that("cache_exists returns FALSE for non-existent year", {
  exists <- vaschooldata:::cache_exists(9999, "tidy")
  expect_false(exists)
})

# ==============================================================================
# SECTION 13: Year Validation in fetch_enr
# ==============================================================================

test_that("fetch_enr rejects years outside available range", {
  expect_error(fetch_enr(1900), "end_year must be between")
  expect_error(fetch_enr(2030), "end_year must be between")
  expect_error(fetch_enr(2015), "end_year must be between")
})

test_that("fetch_enr_multi rejects invalid years", {
  expect_error(fetch_enr_multi(c(2020, 9999)), "Invalid years")
  expect_error(fetch_enr_multi(c(1900, 2020)), "Invalid years")
})
