# Get available years of Virginia enrollment data

Returns a vector of years for which enrollment data is available from
the Virginia Department of Education (VDOE).

## Usage

``` r
get_available_years()
```

## Value

Integer vector of available years (2016-2024)

## Details

VDOE provides Fall Membership enrollment data through the School Quality
Profiles website. Historical data is available from approximately 2016
to the present.

Note: VDOE typically releases Fall Membership data in late fall/early
winter following the school year start. For example, 2024-25
(end_year=2025) data is usually available by December 2024.

## Examples

``` r
get_available_years()
#> [1] 2016 2017 2018 2019 2020 2021 2022 2023 2024

# Check the current range
range(get_available_years())
#> [1] 2016 2024
```
