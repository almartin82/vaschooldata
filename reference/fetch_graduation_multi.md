# Fetch graduation rate data for multiple years

Downloads and combines graduation rate data for multiple school years.

## Usage

``` r
fetch_graduation_multi(end_years, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_years:

  Vector of school year ends (e.g., c(2019, 2020, 2021))

- tidy:

  If TRUE (default), returns data in long (tidy) format.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Combined data frame with graduation rate data for all requested years

## Examples

``` r
if (FALSE) { # \dontrun{
# Get 5 years of data
grad_multi <- fetch_graduation_multi(2019:2023)

# Track graduation rate trends
grad_multi |>
  dplyr::filter(is_state, diploma_type == "all") |>
  dplyr::select(end_year, graduation_rate, cohort_size)

# Compare specific school across years
grad_multi |>
  dplyr::filter(school_number == "0540", diploma_type == "all") |>
  dplyr::select(end_year, graduation_rate)
} # }
```
