# vaschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/vaschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/vaschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/vaschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/vaschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/vaschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/vaschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**[Documentation](https://almartin82.github.io/vaschooldata/)** | [GitHub](https://github.com/almartin82/vaschooldata)

Fetch and analyze Virginia school enrollment data from the Virginia Department of Education (VDOE) in R or Python. Data for every school and division in Virginia.

## Part of the State Schooldata Project

This package is part of a collection of state education data packages inspired by [njschooldata](https://github.com/almartin82/njschooldata), the original R package for accessing New Jersey school data. The goal is to provide a simple, consistent interface for accessing state-published school data across all 50 states.

**All state packages:** [github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## What can you find with vaschooldata?

Virginia educates **1.25 million students** across 132 school divisions, from the coalfields of Appalachia to the suburbs of Northern Virginia. Here are fifteen stories hiding in the data:

---

### 1. The Northern Virginia Boom

While most of Virginia holds steady, **Loudoun County** has exploded. From 52,000 students in 2016 to over 82,000 today, it is now Virginia's 4th-largest division.

```r
library(vaschooldata)
library(dplyr)

# Loudoun's explosive growth
fetch_enr_multi(2016:2023, use_cache = TRUE) |>
  filter(is_district, grepl("Loudoun", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students)
```

![Loudoun County enrollment growth](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/loudoun-chart-1.png)

---

### 2. The Fairfax Giant

**Fairfax County Public Schools** alone enrolls more students than 42 states' entire charter sectors. At 180,000 students, it is the 10th-largest district in America.

```r
fetch_enr(2023, use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
```

![Virginia's largest divisions](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png)

---

### 3. The Rural Decline

While Northern Virginia grows, **Southwest Virginia is vanishing**. Lee County has lost 45% of its students since 2016. Dickenson County: down 40%+.

```r
fetch_enr_multi(c(2016, 2023), use_cache = TRUE) |>
  filter(is_district, grepl("Lee County|Dickenson", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students) |>
  tidyr::pivot_wider(names_from = end_year, values_from = n_students)
```

![Rural decline chart](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/rural-decline-chart-1.png)

---

### 4. Virginia's Demographic Transformation

Virginia is becoming increasingly diverse. Hispanic students are the fastest-growing demographic group.

```r
fetch_enr_multi(c(2016, 2023), use_cache = TRUE) |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) |>
  select(end_year, subgroup, n_students, pct)
```

![Demographics chart](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/demographics-chart-1.png)

---

### 5. The COVID Dip

Virginia lost students between 2019 and 2021. Enrollment has partially recovered but remains below pre-pandemic levels.

```r
fetch_enr_multi(2019:2024, use_cache = TRUE) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

![COVID enrollment impact](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/covid-chart-1.png)

---

### 6. The Charter Desert

Unlike many states, Virginia has **fewer than 10 charter schools** statewide. The legislature has historically been restrictive.

```r
charter_data <- fetch_enr(2023, use_cache = TRUE) |>
  filter(grepl("charter", tolower(campus_name)) | grepl("charter", tolower(district_name)),
         subgroup == "total_enrollment", grade_level == "TOTAL")

if (nrow(charter_data) > 0) {
  charter_data |>
    summarize(n_charter_schools = n(), total_students = sum(n_students, na.rm = TRUE))
} else {
  data.frame(n_charter_schools = 0, total_students = 0)
}
```

---

### 7. The Hampton Roads Plateau

Virginia Beach, Norfolk, and Newport News, once growing military hubs, have **flatlined for two decades**.

```r
fetch_enr_multi(c(2016, 2024), use_cache = TRUE) |>
  filter(is_district, grepl("Virginia Beach|Norfolk|Newport News", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students)
```

![Hampton Roads trends](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/hampton-roads-chart-1.png)

---

### 8. Kindergarten as Crystal Ball

Kindergarten enrollment predicts the future. **Richmond City** kindergarten has dropped, signaling continued enrollment pressure.

```r
fetch_enr_multi(2019:2023, use_cache = TRUE) |>
  filter(is_district, grepl("Richmond City", district_name),
         subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students)
```

![Kindergarten trends](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/kindergarten-chart-1.png)

---

### 9. The Wealthiest Division in Virginia

**Falls Church City**, a tiny independent city in Northern Virginia, is 100% in Fairfax County but operates its own schools. Median income: $150,000+.

```r
fetch_enr(2023, use_cache = TRUE) |>
  filter(is_district, grepl("Falls Church", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(district_name, n_students)
```

---

### 10. 9 Years of VDOE Data

This package provides **9 years** of Virginia enrollment data from the VDOE School Quality Profiles (2016-2024).

```r
# Years available
get_available_years()
```

```r
# Count divisions over time
fetch_enr_multi(c(2016, 2020, 2024), use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_divisions = n(), total_students = sum(n_students, na.rm = TRUE))
```

---

### 11. Richmond City's Enrollment Pressure

**Richmond City Public Schools** has lost students steadily, with enrollment dropping from 24,000 to under 22,000 since 2016.

```r
fetch_enr_multi(2016:2024, use_cache = TRUE) |>
  filter(is_district, grepl("Richmond City", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

![Richmond City enrollment trends](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/richmond-chart-1.png)

---

### 12. Hampton Roads Military Corridor

Virginia Beach, Norfolk, Newport News, and Hampton together educate **180,000+ students**, but enrollment has plateaued despite the large military presence.

```r
fetch_enr_multi(c(2016, 2024), use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Virginia Beach|Norfolk|Newport News|Hampton", district_name)) |>
  group_by(end_year) |>
  summarize(total = sum(n_students, na.rm = TRUE))
```

---

### 13. Elementary vs. High School Shifts

COVID's kindergarten dip is now visible in elementary grades, while high school enrollment remains stable from pre-pandemic cohorts.

```r
fetch_enr(2024, use_cache = TRUE) |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "09", "12")) |>
  select(grade_level, n_students)
```

![Grade band enrollment trends](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/grade-distribution-chart-1.png)

---

### 14. Southwest Virginia Coalfield Decline

The **coalfield counties** (Buchanan, Dickenson, Wise, Lee, Tazewell) have lost 20%+ of students since 2016 as the coal economy continues to decline.

```r
fetch_enr_multi(c(2016, 2024), use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Buchanan|Dickenson|Wise|Lee|Tazewell", district_name)) |>
  group_by(end_year) |>
  summarize(total = sum(n_students, na.rm = TRUE))
```

![Coalfield enrollment decline](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/coalfield-chart-1.png)

---

### 15. The Two Virginias

Northern Virginia (Fairfax, Loudoun, Prince William, Arlington) has **grown** since 2016 while Southwest Virginia has **declined**. These diverging trajectories represent two different futures.

```r
# Northern Virginia vs Southwest Virginia indexed to 2016
# See vignette for full code
```

![Two Virginias diverging trajectories](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/two-virginias-chart-1.png)

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("almartin82/vaschooldata")
```

## R Quick Start

```r
library(vaschooldata)
library(dplyr)

# Get 2023 enrollment data (2022-23 school year)
enr <- fetch_enr(2023, use_cache = TRUE)

# Statewide total
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)

# Top 5 divisions
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
```

## Python Quick Start

```python
import pyvaschooldata as va

# Fetch 2023 data (2022-23 school year)
enr = va.fetch_enr(2023)

# Statewide total
total = enr[(enr['is_state'] == True) & (enr['subgroup'] == 'total_enrollment') & (enr['grade_level'] == 'TOTAL')]['n_students'].sum()
print(f"{total:,} students")

# Get multiple years
enr_multi = va.fetch_enr_multi([2020, 2021, 2022, 2023])

# Check available years
years = va.get_available_years()
print(f"Data available: {min(years)}-{max(years)}")
```

## Graduation Rate Data

```r
library(vaschooldata)
library(dplyr)

# Get 2023 graduation rates (2022-23 school year)
grad <- fetch_graduation(2023)

# Statewide graduation rate
grad |>
  filter(is_state, diploma_type == "all") |>
  select(graduation_rate, cohort_size)

# Compare schools
grad |>
  filter(is_school, diploma_type == "all") |>
  arrange(desc(graduation_rate)) |>
  select(school_name, graduation_rate, cohort_size) |>
  head(5)
```

**Years available:** 2019-2023 (5 years)
**Data source:** VDOE Cohort Graduation and Dropout Report
**Rate types:** 4-year, 5-year graduation rates
**Diploma types:** Advanced Studies, Standard, IB, GED, Certificate, Applied Studies

## Data Format

`fetch_enr()` returns tidy (long) format by default:

| Column | Description |
|--------|-------------|
| `end_year` | School year end (e.g., 2023 for 2022-23) |
| `district_id` | LEA ID (7 characters) |
| `campus_id` | School ID (12 characters) |
| `type` | "State", "District", or "Campus" |
| `district_name`, `campus_name` | Names |
| `county` | County name |
| `grade_level` | "TOTAL", "PK", "K", "01"..."12" |
| `subgroup` | Demographic group |
| `n_students` | Enrollment count |
| `pct` | Percentage of total |

## Data Notes

**Data source:** Virginia Department of Education (VDOE) School Quality Profiles

**Available years:** 2016-2024 (9 years)

**Entities:** State, 132 school divisions, ~2,100 schools

**Subgroups:** Total enrollment, race/ethnicity (white, black, Hispanic, Asian, multiracial, Native American, Pacific Islander), gender, economically disadvantaged, students with disabilities, English learners

**Suppression:** Small counts may be suppressed for student privacy

**Census Day:** Fall membership count (typically late September/early October)

## Enrollment Visualizations

<img src="https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/loudoun-chart-1.png" alt="Virginia statewide enrollment trends" width="600">

<img src="https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/top-districts-chart-1.png" alt="Top Virginia districts" width="600">

See the [full vignette](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html) for all 15 insights with interactive charts.

## Author

Andy Martin (almartin@gmail.com)
[github.com/almartin82](https://github.com/almartin82)

## License

MIT
