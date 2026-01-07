# Parse percentage string to numeric

Converts VA percentage format (" 95.3%") to decimal (0.953). Handles
leading spaces, % suffix, and commas.

## Usage

``` r
parse_percentage(x)
```

## Arguments

- x:

  Character vector of percentages

## Value

Numeric vector on 0-1 scale
