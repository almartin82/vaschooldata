# Fetch enrollment using local file

Processes a locally downloaded Fall Membership file from VDOE. Use this
function when automatic downloads fail due to CAPTCHA or other
restrictions.

## Usage

``` r
fetch_enr_local(file_path, end_year, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- file_path:

  Path to the downloaded Excel or CSV file

- end_year:

  School year end (e.g., 2024 for 2023-24)

- tidy:

  If TRUE (default), returns data in long (tidy) format.

- use_cache:

  If TRUE (default), caches the processed data.

## Value

Data frame with enrollment data

## Examples

``` r
if (FALSE) { # \dontrun{
# Download Fall Membership file manually from:
# https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics

# Then process it:
enr_2024 <- fetch_enr_local("path/to/fall_membership_2024.xlsx", 2024)
} # }
```
