# Get VDOE document ID for Fall Membership files

Returns the document ID for VDOE's published Excel files. These IDs may
need to be updated as VDOE changes their CMS.

## Usage

``` r
get_vdoe_doc_id(end_year, level)
```

## Arguments

- end_year:

  School year end

- level:

  "school" or "division"

## Value

Character string document ID, or NA if not found
