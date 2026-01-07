# Process raw graduation data

Converts raw VDOE graduation data to standardized format. Handles era
detection, column renaming, and value parsing.

## Usage

``` r
process_graduation(raw_data, end_year)
```

## Arguments

- raw_data:

  Raw data from get_raw_graduation()

- end_year:

  School year end

## Value

Data frame with standardized schema
