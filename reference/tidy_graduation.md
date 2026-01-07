# Tidy graduation rate data

Converts processed graduation data from wide to long (tidy) format.
Pivots diploma type columns into rows.

## Usage

``` r
tidy_graduation(processed_data)
```

## Arguments

- processed_data:

  Processed data from process_graduation()

## Value

Long-format tibble with diploma_type column
