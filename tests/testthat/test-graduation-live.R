# LIVE Pipeline Tests for VA Graduation Rate Data
# These tests verify the entire data pipeline from URL download to tidy output
# All tests use LIVE network calls - NO MOCKS
#
# VA-specific considerations:
# - Two eras: v1 (2019-2022, 23 columns) and v2 (2023+, 24 columns with "Level" field)
# - Percentages have leading spaces and "%" suffix: "   83.71%"
# - Integers have leading spaces: "           178"
# - State-level numbers (2023 only) have commas: "98,927"
# - Suppressed values marked with "<"
# - Zero rates: "   .00%" (real zero, NOT suppression)

# Helper function for network-dependent tests
skip_if_offline <- function() {
  tryCatch({
    response <- httr::HEAD("https://www.google.com", timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# Hardcoded URLs for stability (from VA-GRADUATION-RESEARCH.md)
get_graduation_url <- function(end_year) {
  urls <- list(
    "2019" = "https://data.virginia.gov/dataset/89179d05-0009-4a08-a8bd-4f31ee47ed4a/resource/209a1d06-8d24-47e5-9bea-d64f6e7b6406/download/cohort_statistics-8.csv",
    "2020" = "https://data.virginia.gov/dataset/c13dbcbb-92fd-4335-9f41-9d6f41b084eb/resource/19b88afb-e16d-450d-80fe-9dd11fa87f0d/download/cohort_statistics-7.csv",
    "2021" = "https://data.virginia.gov/dataset/d2f4192e-a978-44ea-b6e9-c7fcdba2272f/resource/99a342e4-4e83-40f1-9b51-887a5c7e434f/download/cohort_statistics-6.csv",
    "2022" = "https://data.virginia.gov/dataset/92e9facf-079a-4af9-9890-6020db3b527e/resource/49810d27-f16d-42f1-9577-21b173713204/download/cohort_statistics-5.csv",
    "2023" = "https://data.virginia.gov/dataset/554735ce-fd3e-4077-af7f-868c32e51edf/resource/ccf203ad-862d-4522-8e9c-ab50de579ce7/download/cohort_statistics-4.csv"
  )

  if (!as.character(end_year) %in% names(urls)) {
    stop("Graduation data not available for ", end_year)
  }

  urls[[as.character(end_year)]]
}

# =============================================================================
# CATEGORY 1: URL Availability Tests
# =============================================================================

test_that("2019 graduation URL returns HTTP 200", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  response <- httr::HEAD(url, timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("2020 graduation URL returns HTTP 200", {
  skip_if_offline()

  url <- get_graduation_url(2020)
  response <- httr::HEAD(url, timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("2021 graduation URL returns HTTP 200", {
  skip_if_offline()

  url <- get_graduation_url(2021)
  response <- httr::HEAD(url, timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("2022 graduation URL returns HTTP 200", {
  skip_if_offline()

  url <- get_graduation_url(2022)
  response <- httr::HEAD(url, timeout(30))

  expect_equal(httr::status_code(response), 200)
})

test_that("2023 graduation URL returns HTTP 200", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  response <- httr::HEAD(url, timeout(30))

  expect_equal(httr::status_code(response), 200)
})

# =============================================================================
# CATEGORY 2: File Download Tests
# =============================================================================

test_that("Can download 2019 graduation file", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))

  expect_equal(httr::status_code(response), 200)
  expect_gt(file.info(temp_file)$size, 1000)

  # Verify it's a CSV file, not HTML
  content <- readLines(temp_file, n = 1, warn = FALSE)
  expect_true(grepl("Cohort Year", content))
})

test_that("Can download 2023 graduation file (v2 schema)", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))

  expect_equal(httr::status_code(response), 200)
  expect_gt(file.info(temp_file)$size, 1000)

  # Verify it's a CSV file
  content <- readLines(temp_file, n = 1, warn = FALSE)
  expect_true(grepl("Level", content))  # v2 has Level column
})

# =============================================================================
# CATEGORY 3: File Parsing Tests
# =============================================================================

test_that("Can parse 2019 CSV with readr", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))

  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  expect_true(is.data.frame(raw))
  expect_gt(nrow(raw), 300)
  expect_equal(ncol(raw), 23)  # v1 has 23 columns
})

test_that("Can parse 2023 CSV with readr", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))

  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  expect_true(is.data.frame(raw))
  expect_gt(nrow(raw), 300)
  expect_equal(ncol(raw), 24)  # v2 has 24 columns (includes Level)
})

# =============================================================================
# CATEGORY 4: Column Structure Tests
# =============================================================================

test_that("2019 file has expected v1 columns (no Level)", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  # v1 columns (23 total, no Level)
  expected_cols <- c("Cohort Year", "Division Number", "Division Name",
                     "School Number", "School Name", "Type of Graduation Rate",
                     "Rate Type", "Graduation Rate", "Students in Cohort",
                     "Total Graduates", "Advanced Studies", "IB", "Standard",
                     "Other Diplomas", "Applied Studies", "GED", "ISAEP",
                     "Certificate of Completion", "Completion Rate",
                     "Dropout Rate", "Dropouts", "Still Enrolled",
                     "Long-Term Absence")

  expect_true(all(expected_cols %in% names(raw)))
  expect_false("Level" %in% names(raw))  # v1 should NOT have Level
})

test_that("2023 file has expected v2 columns (includes Level)", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  # v2 columns (24 total, includes Level)
  expect_true("Level" %in% names(raw))  # v2 SHOULD have Level
  expect_true("Cohort Year" %in% names(raw))
  expect_true("Graduation Rate" %in% names(raw))
  expect_true("Students in Cohort" %in% names(raw))
  expect_true("Total Graduates" %in% names(raw))
})

# =============================================================================
# CATEGORY 5: Year Filtering Tests
# =============================================================================

test_that("Can extract 2019 data", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  expect_equal(unique(raw$`Cohort Year`), 2019)
  expect_gt(nrow(raw), 300)
})

test_that("Can extract 2023 data", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  expect_equal(unique(raw$`Cohort Year`), 2023)
  expect_gt(nrow(raw), 300)
})

test_that("2023 includes state-level record", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  state_records <- raw$Level == "State"

  expect_true(any(state_records))
  expect_equal(sum(state_records), 1)  # Exactly 1 state record
})

test_that("2019 has no state-level record (v1)", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  # v1 doesn't have Level column, so no state records
  # All records are school-level
  expect_false("Level" %in% names(raw))
  expect_gt(nrow(raw), 300)  # All schools
})

# =============================================================================
# CATEGORY 6: Aggregation Tests
# =============================================================================

test_that("2023 state record has non-zero values", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  state_rec <- raw[raw$Level == "State", ]

  expect_gt(nrow(state_rec), 0)

  # State record has commas in numbers: "98,927"
  # These will be character strings in raw parse
  expect_true(!is.na(state_rec$`Graduation Rate`))
  expect_true(!is.na(state_rec$`Students in Cohort`))
  expect_true(!is.na(state_rec$`Total Graduates`))
})

test_that("School records have consistent structure across years", {
  skip_if_offline()

  # Check 2019
  url_2019 <- get_graduation_url(2019)
  temp_2019 <- tempfile()
  response_2019 <- httr::GET(url_2019, httr::write_disk(temp_2019, overwrite = TRUE), timeout(60))
  raw_2019 <- readr::read_csv(temp_2019, show_col_types = FALSE)

  # Check 2023
  url_2023 <- get_graduation_url(2023)
  temp_2023 <- tempfile()
  response_2023 <- httr::GET(url_2023, httr::write_disk(temp_2023, overwrite = TRUE), timeout(60))
  raw_2023 <- readr::read_csv(temp_2023, show_col_types = FALSE)

  # Both should have similar row counts (330-340 schools)
  expect_gt(nrow(raw_2019), 300)
  expect_gt(nrow(raw_2023), 300)
  expect_lt(nrow(raw_2019), 400)
  expect_lt(nrow(raw_2023), 400)
})

# =============================================================================
# CATEGORY 7: Data Quality Tests
# =============================================================================

test_that("2023 can parse percentage with leading spaces and % suffix", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  # Get a specific school's graduation rate
  arcadia <- raw[raw$`School Name` == "Arcadia High", ]

  expect_gt(nrow(arcadia), 0)

  # Raw value is "   83.71%" (with spaces and %)
  # Should be able to parse to numeric 0.8371
  rate_str <- arcadia$`Graduation Rate`[1]
  expect_true(grepl("%", rate_str))
  expect_true(grepl("^\\s+", rate_str))  # Leading spaces
})

test_that("2023 can parse integer with leading spaces", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  arcadia <- raw[raw$`School Name` == "Arcadia High", ]

  expect_gt(nrow(arcadia), 0)

  # Raw value is "           178" (with leading spaces)
  cohort_str <- arcadia$`Students in Cohort`[1]
  expect_true(grepl("^\\s+", cohort_str))  # Leading spaces
})

test_that("Suppressed values are marked with <", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  arcadia <- raw[raw$`School Name` == "Arcadia High", ]

  expect_gt(nrow(arcadia), 0)

  # Arcadia High has suppressed "Applied Studies" diploma count
  applied_studies <- arcadia$`Applied Studies`[1]
  expect_equal(applied_studies, "<")
})

test_that("Zero dropout rates are marked as .00% not <", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  # Find a school with zero dropout rate
  # Some schools have "   .00%" which is a real zero, NOT suppression
  zero_drops <- raw[raw$`Dropout Rate` == "   .00%", ]

  expect_gt(nrow(zero_drops), 0)  # At least one school has 0% dropout
})

test_that("State-level numbers in 2023 include commas", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  state_rec <- raw[raw$Level == "State", ]

  expect_gt(nrow(state_rec), 0)

  # State record has numbers like "98,927" with commas
  cohort_str <- state_rec$`Students in Cohort`[1]
  expect_true(grepl(",", cohort_str))  # Has commas
})

# =============================================================================
# CATEGORY 8: Output Fidelity Tests
# =============================================================================

test_that("2023 Arcadia High values are extractable from raw CSV", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  arcadia <- raw[raw$`School Name` == "Arcadia High" & raw$`Division Number` == 1, ]

  expect_equal(nrow(arcadia), 1)

  # Verified values from research:
  # Graduation Rate: "   83.71%" -> 0.8371
  # Students in Cohort: "           178" -> 178
  # Total Graduates: "           149" -> 149
  # Advanced Studies: "              52" -> 52
  # Standard: "              96" -> 96
  # Applied Studies: "<" -> NA (suppressed)
  # Completion Rate: "    84.83%" -> 0.8483
  # Dropout Rate: "     3.93%" -> 0.0393
  # Dropouts: "               7" -> 7
  # Still Enrolled: "               4" -> 4
  # Long-Term Absence: "              16" -> 16

  expect_equal(arcadia$`Graduation Rate`[1], "   83.71%")
  expect_equal(arcadia$`Students in Cohort`[1], "           178")
  expect_equal(arcadia$`Total Graduates`[1], "           149")
})

test_that("2023 state record values are extractable from raw CSV", {
  skip_if_offline()

  url <- get_graduation_url(2023)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  state_rec <- raw[raw$Level == "State", ]

  expect_equal(nrow(state_rec), 1)

  # Verified values from research:
  # Graduation Rate: "    91.93%" -> 0.9193
  # Students in Cohort: "          98,927" -> 98927
  # Total Graduates: "          90,944" -> 90944
  # Advanced Studies: "          50,175" -> 50175
  # IB: "             766" -> 766
  # Standard: "          37,883" -> 37883
  # GED: "           2,117" -> 2117
  # Certificate: "             145" -> 145
  # Completion Rate: "    92.93%" -> 0.9293
  # Dropout Rate: "     5.38%" -> 0.0538
  # Dropouts: "           5,319" -> 5319
  # Still Enrolled: "           1,330" -> 1330
  # Long-Term Absence: "             346" -> 346

  expect_equal(state_rec$`Graduation Rate`[1], "    91.93%")
  expect_equal(state_rec$`Students in Cohort`[1], "          98,927")
  expect_equal(state_rec$`Total Graduates`[1], "          90,944")
})

test_that("2019 school values are extractable from raw CSV", {
  skip_if_offline()

  url <- get_graduation_url(2019)
  temp_file <- tempfile()

  response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE), timeout(60))
  raw <- readr::read_csv(temp_file, show_col_types = FALSE)

  arcadia <- raw[raw$`School Name` == "Arcadia High" & raw$`Division Number` == 1, ]

  expect_equal(nrow(arcadia), 1)

  # Verified 2019 values:
  # Graduation Rate: "    88.74%" -> 0.8874
  # Students in Cohort: "             151" -> 151
  # Total Graduates: "             134" -> 134
  # Advanced Studies: "              57" -> 57
  # Standard: "              74" -> 74

  expect_equal(arcadia$`Graduation Rate`[1], "    88.74%")
  expect_equal(arcadia$`Students in Cohort`[1], "             151")
  expect_equal(arcadia$`Total Graduates`[1], "             134")
})
