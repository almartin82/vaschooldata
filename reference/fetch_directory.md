# Fetch Virginia school directory data

Downloads and processes school directory data from the Virginia
Department of Education (VDOE). Data includes school names, addresses,
phone numbers, and principal information.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  Not currently used. VDOE provides a single current snapshot. Included
  for API consistency with other fetch functions.

- tidy:

  If TRUE (default), returns data in a clean format with standardized
  column names. If FALSE, returns data closer to the original source
  format.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from VDOE.

## Value

Data frame with school directory information including:

- division_id:

  3-digit division (district) identifier

- school_id:

  4-digit school identifier

- nces_id:

  NCES school identifier (12-digit)

- division_name:

  Name of the school division

- school_name:

  Name of the school

- principal_name:

  Name of the school principal

- address:

  Street address line 1

- address2:

  Street address line 2 (if any)

- city:

  City

- state:

  State (VA)

- zip:

  ZIP code

- phone:

  Phone number

- low_grade:

  Lowest grade served

- high_grade:

  Highest grade served

- school_type:

  School type (e.g., "Public school - Regular")

- grade_level:

  Grade level category (Elementary, Middle, High, etc.)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get school directory
directory <- fetch_directory()

# Get raw format
directory_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
directory_fresh <- fetch_directory(use_cache = FALSE)

# Filter to specific division
fairfax <- directory |>
  dplyr::filter(division_name == "Fairfax County Public Schools")
} # }
```
