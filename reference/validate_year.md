# Validate year parameter

Checks if the provided year is within the valid range.

## Usage

``` r
validate_year(end_year, min_year = NULL, max_year = NULL)
```

## Arguments

- end_year:

  School year end to validate

- min_year:

  Minimum valid year (default from get_available_years())

- max_year:

  Maximum valid year (default from get_available_years())

## Value

NULL (throws error if invalid)
