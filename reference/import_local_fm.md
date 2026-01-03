# Import local Fall Membership file

Imports enrollment data from a locally downloaded Fall Membership file.
Use this function if automatic download fails due to CAPTCHA or other
restrictions.

## Usage

``` r
import_local_fm(file_path, end_year)
```

## Arguments

- file_path:

  Path to the downloaded Excel or CSV file

- end_year:

  School year end (e.g., 2024 for 2023-24)

## Value

List with school and division data frames

## Examples

``` r
if (FALSE) { # \dontrun{
# Download the Fall Membership file manually from:
# https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics
# Then import it:
raw_data <- import_local_fm("path/to/fall_membership_2024.xlsx", 2024)
} # }
```
