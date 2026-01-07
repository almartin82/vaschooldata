# Detect graduation data era

Determines whether the data is v1 (2019-2022, 23 columns, no Level
field) or v2 (2023+, 24 columns, has Level field).

## Usage

``` r
detect_grad_era(raw_data)
```

## Arguments

- raw_data:

  Raw data frame from get_raw_graduation()

## Value

Character "v1" or "v2"
