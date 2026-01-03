# Process raw enrollment data

Transforms raw VDOE data into a standardized schema combining school and
division data.

## Usage

``` r
process_enr(raw_data, end_year)
```

## Arguments

- raw_data:

  List containing school and division data from get_raw_enr

- end_year:

  School year end

## Value

Processed data frame with standardized columns
