# Download raw graduation rate data

Downloads graduation rate CSV from VDOE Open Data Portal and returns it
as a data frame with minimal processing.

## Usage

``` r
get_raw_graduation(end_year, cache_dir = get_cache_dir())
```

## Arguments

- end_year:

  School year end (e.g., 2023 for 2022-23 school year)

- cache_dir:

  Directory to cache downloaded files

## Value

Data frame with raw graduation data as provided by VDOE
