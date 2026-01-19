# vaschooldata: Fetch and Process Virginia School Data

Downloads and processes school enrollment data from the Virginia
Department of Education (VDOE). Provides functions for fetching Fall
Membership enrollment data directly from VDOE and transforming it into
tidy format for analysis.

## Main functions

- [`fetch_enr`](https://almartin82.github.io/vaschooldata/reference/fetch_enr.md):

  Fetch enrollment data for a school year

- [`fetch_enr_multi`](https://almartin82.github.io/vaschooldata/reference/fetch_enr_multi.md):

  Fetch enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/vaschooldata/reference/tidy_enr.md):

  Transform wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/vaschooldata/reference/id_enr_aggs.md):

  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/vaschooldata/reference/enr_grade_aggs.md):

  Create grade-level aggregations

- [`get_available_years`](https://almartin82.github.io/vaschooldata/reference/get_available_years.md):

  Get list of available data years

## Cache functions

- [`cache_status`](https://almartin82.github.io/vaschooldata/reference/cache_status.md):

  View cached data files

- [`clear_cache`](https://almartin82.github.io/vaschooldata/reference/clear_cache.md):

  Remove cached data files

## ID System

Virginia uses a hierarchical ID system:

- Division IDs: 7 characters (e.g., 5100180 = Alexandria City)

- School IDs: 12 characters (division ID + 5-digit school number)

Virginia has 132 school divisions (equivalent to districts in other
states).

## Data Sources

Data is sourced exclusively from:

- VDOE Fall Membership:
  <https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics>

Note: This package does NOT use federal data sources (NCES, Urban
Institute, etc.).

## Data Availability

- Years: 2016-2024 (9 years from VDOE School Quality Profiles)

- Aggregation levels: State, Division (District), School

- Demographics: Race/ethnicity (7 categories), Sex, Economically
  Disadvantaged, etc.

- Grade levels: PK through 12, plus ungraded and TOTAL

## Demographics

VDOE provides enrollment data broken down by:

- Race/ethnicity: White, Black, Hispanic, Asian, Native American,
  Pacific Islander, Multiracial

- Gender: Male, Female

- Special populations: Economically Disadvantaged, Students with
  Disabilities, English Learners

## See also

Useful links:

- <https://almartin82.github.io/vaschooldata>

- <https://github.com/almartin82/vaschooldata>

- Report bugs at <https://github.com/almartin82/vaschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
