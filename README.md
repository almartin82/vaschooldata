# vaschooldata

An R package for fetching, processing, and analyzing school enrollment data from Virginia's Department of Education (VDOE). It provides a programmatic interface to public school data, enabling researchers, analysts, and education policy professionals to easily access Virginia public school data across the **full historical record** (1987-2023).

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("almartin82/vaschooldata")
```

## Quick Start

```r
library(vaschooldata)

# Get 2023 enrollment data (2022-23 school year)
enr_2023 <- fetch_enr(2023)

# View state totals
enr_2023 %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# Get wide format (one row per school/district)
enr_wide <- fetch_enr(2023, tidy = FALSE)

# Get multiple years at once
enr_multi <- fetch_enr_multi(2019:2023)
```

## Data Availability

### Years Available

**Full historical coverage: 1987-2023 (37 years)**

Data is sourced from the [Urban Institute's Education Data Portal](https://educationdata.urban.org/), which aggregates NCES Common Core of Data (CCD) enrollment figures.

### Aggregation Levels

| Level | Description | Approximate Count |
|-------|-------------|-------------------|
| State | Virginia statewide totals | 1 |
| District | School divisions | ~132 |
| Campus | Individual schools | ~2,100 |

Virginia uses the term "school division" rather than "school district," but this package uses "district" for consistency with other state packages.

### Demographics

Demographic categories vary by era:

| Era | Years | Race/Ethnicity Categories |
|-----|-------|---------------------------|
| Pre-1998 | 1987-1997 | Limited - 5 categories (White, Black, Hispanic, Asian/Pacific Islander, American Indian) |
| 5-Race Era | 1998-2010 | White, Black, Hispanic, Asian/Pacific Islander, American Indian |
| 7-Race Era | 2011-2023 | White, Black, Hispanic, Asian, Pacific Islander (separate), American Indian, Two or More Races |

### Grade Levels

- Pre-Kindergarten (PK)
- Kindergarten (K)
- Grades 1-12
- Ungraded (UG) - for schools with non-traditional grade structures

### Other Available Fields

- **Sex**: Male, Female (available all years)
- **Charter status**: Charter school indicator (recent years)
- **County**: County name for each school/division

## Data Source

This package retrieves data from the [Urban Institute's Education Data Portal](https://educationdata.urban.org/), which provides a comprehensive API for education data from multiple federal sources.

The underlying data comes from:
- **NCES Common Core of Data (CCD)**: The CCD is a comprehensive, annual, national database of all public elementary and secondary schools and school districts.

### Original Source

The Virginia Department of Education (VDOE) collects Fall Membership data annually:
- [VDOE Enrollment & Demographics](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics)
- [Fall Membership Build-A-Table](https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304)
- [Virginia School Quality Profiles](https://schoolquality.virginia.gov/)

## ID System

Virginia uses NCES identifier codes:

| ID Type | Format | Example | Description |
|---------|--------|---------|-------------|
| Division ID (leaid) | 7 characters | 5100180 | NCES Local Education Agency ID |
| School ID (ncessch) | 12 characters | 510018000123 | Division ID + 5-digit school number |

### Notable Divisions

| Division Name | LEAID | Notes |
|--------------|-------|-------|
| Fairfax County Public Schools | 5100390 | Largest division (~180,000 students) |
| Virginia Beach City Public Schools | 5101410 | 2nd largest |
| Prince William County Schools | 5101170 | 3rd largest |
| Loudoun County Public Schools | 5100690 | 4th largest |

## What's NOT Available

- **Special populations by school**: Economically disadvantaged, English Learners, and Special Education counts are available at the district level in some years but not consistently at the school level through this API
- **Pre-1987 data**: CCD data begins in 1986, but Virginia data is available from 1987
- **Private school enrollment**: VDOE does not collect private school data

## Known Caveats

1. **Pre-2011 race data**: Asian and Pacific Islander were combined into a single category before 2011
2. **Two or More Races**: This category was not collected before 2011
3. **Charter schools**: Virginia has relatively few charter schools compared to other states; charter data may be limited
4. **Suppression**: Small cell sizes may be suppressed in source data
5. **Fall Membership timing**: All counts represent enrollment as of September 30 of each school year

## Functions

### Main Functions

| Function | Description |
|----------|-------------|
| `fetch_enr(end_year)` | Fetch enrollment data for a single year |
| `fetch_enr_multi(end_years)` | Fetch enrollment data for multiple years |
| `tidy_enr(df)` | Transform wide data to tidy (long) format |
| `id_enr_aggs(df)` | Add aggregation level flags |
| `enr_grade_aggs(df)` | Create grade-level aggregations (K-8, HS, K-12) |
| `get_available_years()` | Get list of available data years |

### Cache Functions

| Function | Description |
|----------|-------------|
| `cache_status()` | View cached data files |
| `clear_cache()` | Remove cached data files |

## Output Schema

### Wide Format (`tidy = FALSE`)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end (2023 = 2022-23) |
| type | character | "State", "District", or "Campus" |
| district_id | character | Division/district NCES ID |
| campus_id | character | School NCES ID (NA for districts) |
| district_name | character | Division name |
| campus_name | character | School name (NA for districts) |
| county | character | County name |
| charter_flag | character | "Y" or "N" |
| row_total | integer | Total enrollment |
| white, black, hispanic, etc. | integer | Enrollment by race |
| male, female | integer | Enrollment by sex |
| grade_pk, grade_k, grade_01-12 | integer | Enrollment by grade |

### Tidy Format (`tidy = TRUE`)

| Column | Type | Description |
|--------|------|-------------|
| end_year | integer | School year end |
| type | character | Aggregation level |
| district_id, campus_id | character | IDs |
| district_name, campus_name | character | Names |
| grade_level | character | "TOTAL", "PK", "K", "01"-"12" |
| subgroup | character | "total_enrollment", "white", "black", etc. |
| n_students | integer | Student count |
| pct | numeric | Percentage of total (0-1 scale) |
| is_state, is_district, is_campus | logical | Aggregation flags |
| is_charter | logical | Charter school indicator |

## Examples

### State Enrollment Over Time

```r
library(vaschooldata)
library(dplyr)
library(ggplot2)

# Get 10 years of data
enr <- fetch_enr_multi(2014:2023)

# Plot state total enrollment
enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  ggplot(aes(x = end_year, y = n_students)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Virginia Public School Enrollment",
    x = "School Year",
    y = "Total Students"
  )
```

### Division Comparison

```r
# Compare enrollment in top 5 divisions
enr_2023 <- fetch_enr(2023)

top_divisions <- enr_2023 %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  arrange(desc(n_students)) %>%
  head(5)

print(top_divisions)
```

### Demographic Breakdown

```r
# State demographic percentages
enr_2023 %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(subgroup, n_students, pct)
```

## License

MIT

## Related Packages

This package is part of a family of state education data packages:
- [txschooldata](https://github.com/almartin82/txschooldata) - Texas
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Illinois
- [caschooldata](https://github.com/almartin82/caschooldata) - California
- [paschooldata](https://github.com/almartin82/paschooldata) - Pennsylvania
- [ohschooldata](https://github.com/almartin82/ohschooldata) - Ohio
- [nyschooldata](https://github.com/almartin82/nyschooldata) - New York
