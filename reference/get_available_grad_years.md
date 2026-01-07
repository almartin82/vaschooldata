# Get available years of graduation data

Returns a vector of years for which graduation rate data is available
from the Virginia Department of Education (VDOE).

## Usage

``` r
get_available_grad_years()
```

## Value

Integer vector of available years (2019-2023)

## Details

VDOE provides Cohort Graduation and Dropout Report data through the Open
Data Portal. Historical data is available from 2019 to present.

## Examples

``` r
get_available_grad_years()
#> [1] 2019 2020 2021 2022 2023

# Check the current range
range(get_available_grad_years())
#> [1] 2019 2023
```
