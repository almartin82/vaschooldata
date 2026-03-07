# vaschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/vaschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/vaschooldata/actions/workflows/R-CMD-check.yaml)
[![Python Tests](https://github.com/almartin82/vaschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/vaschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/vaschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/vaschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Virginia educates nearly **1.25 million students** across 132 school divisions and graduates over **90,000 seniors** each year. This package provides 10 years of enrollment data and 5 years of graduation data directly from the Virginia Department of Education.

Part of the [njschooldata](https://github.com/almartin82/njschooldata) family.

**[Full documentation](https://almartin82.github.io/vaschooldata/)** -- all 15 stories with interactive charts, getting-started guide, and complete function reference.

## Highlights

```r
library(vaschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))

all_grad <- bind_rows(lapply(2019:2023, function(yr) {
  fetch_graduation(yr, use_cache = TRUE)
}))
```

### 1. More than half of Virginia graduates earn Advanced Studies diplomas

In 2023, 50,175 graduates (54.6% of all diploma recipients) earned the Advanced Studies diploma, Virginia's most rigorous option requiring additional math, science, and foreign language credits. Only 38.3% earned the Standard diploma.

```r
diplomas <- all_grad |>
  filter(is_state, end_year == 2023, diploma_type != "all", !is.na(diploma_count)) |>
  select(diploma_type, diploma_count) |>
  mutate(pct = round(diploma_count / sum(diploma_count) * 100, 1)) |>
  arrange(desc(diploma_count))

diplomas
```
```
#> # A tibble: 7 x 3
#>   diploma_type    diploma_count   pct
#>   <chr>                   <int> <dbl>
#> 1 advanced_studies        50175  54.6
#> 2 standard               37883  38.3
#> 3 applied_studies         2117   2.3
#> 4 ib                       766   0.8
#> 5 isaep                    700   0.8
#> 6 ged                      145   0.2
#> 7 certificate              143   0.2
```

![Diploma type breakdown](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/diploma-breakdown-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#more-than-half-of-virginia-graduates-earn-advanced-studies-diplomas)

### 2. Southwest Virginia coalfield schools defy decline narratives

Despite losing population, Southwest Virginia's rural schools maintain strong graduation rates. Wise County graduates 98.3% of its students, higher than Fairfax County (93.4%) or any other NoVA division.

```r
swva_divisions <- c("Lee County", "Dickenson County", "Buchanan County",
                     "Wise County", "Tazewell County")

swva_grad <- all_grad |>
  filter(is_school, diploma_type == "all",
         division_name %in% swva_divisions) |>
  group_by(end_year, division_name) |>
  summarize(
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  )

swva_grad |>
  filter(end_year == 2023) |>
  arrange(desc(grad_rate)) |>
  mutate(grad_pct = round(grad_rate * 100, 1))
```
```
#> # A tibble: 5 x 6
#>   end_year division_name    cohort graduates grad_rate grad_pct
#>      <int> <chr>             <int>     <int>     <dbl>    <dbl>
#> 1     2023 Wise County         405       398     0.983     98.3
#> 2     2023 Tazewell County     425       401     0.944     94.4
#> 3     2023 Dickenson County    160       144     0.900     90.0
#> 4     2023 Buchanan County     188       169     0.899     89.9
#> 5     2023 Lee County          199       168     0.844     84.4
```

![Southwest Virginia graduation rates](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/swva-grad-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#southwest-virginia-coalfield-schools-defy-decline-narratives)

### 3. Division-level graduation gap: 72% to 100%

Virginia's 130 school divisions span a 28-point graduation rate gap. Grayson County and Clarke County graduate nearly 100% of students, while Richmond City and Danville graduate fewer than 75%.

```r
div_grad <- all_grad |>
  filter(is_school, diploma_type == "all", end_year == 2023) |>
  group_by(division_name) |>
  summarize(
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  ) |>
  filter(cohort >= 50)

cat("Top 5 divisions:\n")
div_grad |> arrange(desc(grad_rate)) |>
  head(5) |>
  mutate(grad_pct = round(grad_rate * 100, 1))
```
```
#> Top 5 divisions:
#> # A tibble: 5 x 5
#>   division_name    cohort graduates grad_rate grad_pct
#>   <chr>             <int>     <int>     <dbl>    <dbl>
#> 1 Grayson County      116       116     1         100
#> 2 Clarke County       146       145     0.993      99.3
#> 3 Falls Church City   200       197     0.985      98.5
#> 4 Wise County         405       398     0.983      98.3
#> 5 Essex County        105       103     0.981      98.1
```

```r
cat("\nBottom 5 divisions:\n")
div_grad |> arrange(grad_rate) |>
  head(5) |>
  mutate(grad_pct = round(grad_rate * 100, 1))
```
```
#> Bottom 5 divisions:
#> # A tibble: 5 x 5
#>   division_name        cohort graduates grad_rate grad_pct
#>   <chr>                 <int>     <int>     <dbl>    <dbl>
#> 1 Richmond City          1483      1073     0.724     72.4
#> 2 Danville City           393       288     0.733     73.3
#> 3 Prince Edward County    157       126     0.803     80.3
#> 4 Fredericksburg City     270       219     0.811     81.1
#> 5 Norfolk City           1800      1475     0.819     81.9
```

![Division graduation rate extremes](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/division-gap-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#division-level-graduation-gap-72-to-100)

## Data Taxonomy

| Category | Years | Function | Details |
|----------|-------|----------|---------|
| **Enrollment** | 2016-2025 | `fetch_enr()` / `fetch_enr_multi()` | State, division, school. Race, gender |
| Assessments | -- | -- | Not yet available |
| **Graduation** | 2019-2023 | `fetch_graduation()` / `fetch_graduation_multi()` | State, school. Diploma types, dropout rate |
| **Directory** | current | `fetch_directory()` | School names, addresses, principals, grade levels |
| Per-Pupil Spending | -- | -- | Not yet available |
| Accountability | -- | -- | Not yet available |
| Chronic Absence | -- | -- | Not yet available |
| EL Progress | -- | -- | Not yet available |
| Special Ed | -- | -- | Not yet available |

> See the full [data category taxonomy](DATA-CATEGORY-TAXONOMY.md) for what each category covers.

## Quick Start

### R

```r
# install.packages("devtools")
devtools::install_github("almartin82/vaschooldata")
```

```r
library(vaschooldata)
library(dplyr)

# Get 2023 graduation rates
grad <- fetch_graduation(2023, use_cache = TRUE)

# Statewide graduation rate
grad |>
  filter(is_state, diploma_type == "all") |>
  select(graduation_rate, cohort_size)

# Top 5 schools by cohort
grad |>
  filter(is_school, diploma_type == "all") |>
  arrange(desc(cohort_size)) |>
  select(school_name, division_name, graduation_rate, cohort_size) |>
  head(5)

# Enrollment data (10 years: 2016-2025)
enr <- fetch_enr(2024, use_cache = TRUE)

# Available years
get_available_years()       # Enrollment: 2016-2025
get_available_grad_years()  # Graduation: 2019-2023
```

### Python

```python
import pyvaschooldata as va

# Fetch 2023 graduation data
grad = va.fetch_graduation(2023)

# Statewide rate
state = grad[(grad['is_state'] == True) & (grad['diploma_type'] == 'all')]
print(f"Graduation rate: {state['graduation_rate'].values[0]:.1%}")

# Fetch enrollment data
enr = va.fetch_enr(2024)

# Check available years
print(f"Enrollment years: {va.get_available_years()}")
print(f"Graduation years: {va.get_available_grad_years()}")
```

## Explore More

Full analysis with 15 stories:
- [15 Stories from Virginia School Data](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html) -- 15 stories covering graduation rates, diploma types, regional comparisons, and dropout trends
- [Function reference](https://almartin82.github.io/vaschooldata/reference/)

## Data Notes

**Enrollment data source**: [Virginia Department of Education (VDOE) School Quality Profiles](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics)

**Graduation data source**: [VDOE Open Data Portal](https://data.virginia.gov) (Cohort Graduation and Dropout Report)

**Available years**: Enrollment 2016-2025 (10 years); Graduation 2019-2023 (5 years)

**Entities**: State, 132 school divisions (Virginia's term for districts), ~2,100 schools (enrollment), ~320 high schools (graduation)

**Enrollment subgroups**: Total enrollment, race/ethnicity (white, black, Hispanic, Asian, multiracial, Native American, Pacific Islander), gender

**Graduation data**: 4-year cohort graduation rate, dropout rate, completion rate, diploma types (Advanced Studies, Standard, IB, Applied Studies, GED, ISAEP, Certificate)

**Suppression**: Small counts suppressed for student privacy (marked as `<` in raw data, `NA` in processed data)

**Census Day**: Fall membership count (typically late September/early October)

**CAPTCHA note**: VDOE's School Quality Profiles site occasionally requires CAPTCHA verification for enrollment data downloads. When this happens, use `use_cache = TRUE` (default) to rely on locally cached data. If no cache exists, download files manually from the [VDOE enrollment page](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/enrollment-demographics) and use `fetch_enr_local()` to import them. Graduation data is always available via the VDOE Open Data Portal.

## Deeper Dive

### 4. Virginia graduates 92% of its students

Virginia's four-year graduation rate has held steady between 91.6% and 93.0% over five years, placing it well above the national average of roughly 87%.

```r
state_grad <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, graduation_rate, cohort_size, total_graduates, dropout_rate)

state_grad |>
  mutate(grad_pct = round(graduation_rate * 100, 2),
         dropout_pct = round(dropout_rate * 100, 2))
```
```
#> # A tibble: 5 x 7
#>   end_year graduation_rate cohort_size total_graduates dropout_rate grad_pct dropout_pct
#>      <int>           <dbl>       <int>           <int>        <dbl>    <dbl>       <dbl>
#> 1     2019           0.916       98241           89991       0.0551     91.6        5.51
#> 2     2020           0.925       98327           90971       0.0509     92.5        5.09
#> 3     2021           0.930       97096           90325       0.0425     93.0        4.25
#> 4     2022           0.922       98281           90603       0.0515     92.2        5.15
#> 5     2023           0.919       98927           90944       0.0538     91.9        5.38
```

![Virginia graduation rate trend](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/grad-trend-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#virginia-graduates-92-of-its-students)

### 5. Richmond City's graduation crisis

Richmond City Public Schools graduates just 72% of its students, the lowest rate among Virginia's 130 school divisions. Its dropout rate of 24% is more than four times the state average of 5.4%.

```r
div_grad_23 <- all_grad |>
  filter(is_school, diploma_type == "all", end_year == 2023) |>
  group_by(division_name) |>
  summarize(
    n_schools = n(),
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    dropouts = sum(dropouts, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  )

richmond_trend <- all_grad |>
  filter(is_school, diploma_type == "all", division_name == "Richmond City") |>
  group_by(end_year) |>
  summarize(
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    dropouts = sum(dropouts, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  )

richmond_trend |>
  mutate(grad_pct = round(grad_rate * 100, 1))
```
```
#> # A tibble: 5 x 5
#>   end_year cohort graduates dropouts grad_pct
#>      <int>  <int>     <int>    <int>    <dbl>
#> 1     2019   1514      1073      364     70.9
#> 2     2020   1495      1066      350     71.3
#> 3     2021   1472      1155      222     78.5
#> 4     2022   1360      1008      272     74.1
#> 5     2023   1483      1073      353     72.4
```

![Richmond City graduation trend](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/richmond-grad-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#richmond-citys-graduation-crisis)

### 6. Loudoun County leads Northern Virginia in graduation rates

Among NoVA's four largest divisions, Loudoun County consistently graduates the highest share of students at 96.7%, followed by Arlington (93.5%), Fairfax (93.4%), and Prince William (91.7%).

```r
nova_grad <- all_grad |>
  filter(is_school, diploma_type == "all",
         division_name %in% c("Fairfax County", "Loudoun County",
                               "Prince William County", "Arlington County")) |>
  group_by(end_year, division_name) |>
  summarize(
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  )

nova_grad |>
  filter(end_year == 2023) |>
  arrange(desc(grad_rate)) |>
  mutate(grad_pct = round(grad_rate * 100, 1))
```
```
#> # A tibble: 4 x 6
#>   end_year division_name         cohort graduates grad_rate grad_pct
#>      <int> <chr>                  <int>     <int>     <dbl>    <dbl>
#> 1     2023 Loudoun County          6688      6468     0.967     96.7
#> 2     2023 Arlington County        1991      1862     0.935     93.5
#> 3     2023 Fairfax County         14907     13923     0.934     93.4
#> 4     2023 Prince William County   7161      6566     0.917     91.7
```

![Northern Virginia graduation rates](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/nova-grad-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#loudoun-county-leads-northern-virginia-in-graduation-rates)

### 7. COVID drove dropout rates to a five-year low

Virginia's dropout rate fell from 5.5% in 2019 to 4.3% in 2021, possibly reflecting emergency pandemic policies that kept students enrolled. By 2023, dropout rates had rebounded to 5.4%.

```r
dropout_trend <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, graduation_rate, dropout_rate) |>
  mutate(
    grad_pct = round(graduation_rate * 100, 2),
    dropout_pct = round(dropout_rate * 100, 2)
  )

dropout_trend
```
```
#> # A tibble: 5 x 4
#>   end_year graduation_rate dropout_rate grad_pct dropout_pct
#>      <int>           <dbl>        <dbl>    <dbl>       <dbl>
#> 1     2019           0.916       0.0551     91.6        5.51
#> 2     2020           0.925       0.0509     92.5        5.09
#> 3     2021           0.930       0.0425     93.0        4.25
#> 4     2022           0.922       0.0515     92.2        5.15
#> 5     2023           0.919       0.0538     91.9        5.38
```

![Dropout rate trend](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/covid-dropout-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#covid-drove-dropout-rates-to-a-five-year-low)

### 8. Thomas Jefferson: Virginia's 100% graduation factory

Thomas Jefferson High School for Science and Technology (TJHSST) in Fairfax County has graduated 100% of its cohort for four consecutive years (2020-2023). With 459 students in its 2023 cohort, it is the largest school in Virginia with a perfect graduation rate.

```r
tj <- all_grad |>
  filter(is_school, diploma_type == "all",
         grepl("Thomas Jefferson High for Science", school_name)) |>
  select(end_year, school_name, graduation_rate, cohort_size)

tj
```
```
#> # A tibble: 5 x 4
#>   end_year school_name                                      graduation_rate cohort_size
#>      <int> <chr>                                                      <dbl>       <int>
#> 1     2019 Thomas Jefferson High for Science and Technology           0.998         427
#> 2     2020 Thomas Jefferson High for Science and Technology           1             443
#> 3     2021 Thomas Jefferson High for Science and Technology           1             436
#> 4     2022 Thomas Jefferson High for Science and Technology           1             452
#> 5     2023 Thomas Jefferson High for Science and Technology           1             459
```

```r
perfect <- all_grad |>
  filter(is_school, diploma_type == "all", end_year == 2023,
         graduation_rate == 1.0, cohort_size >= 30) |>
  select(school_name, division_name, cohort_size) |>
  arrange(desc(cohort_size))

perfect
```
```
#> # A tibble: 7 x 3
#>   school_name                                      division_name      cohort_size
#>   <chr>                                            <chr>                    <int>
#> 1 Thomas Jefferson High for Science and Technology Fairfax County             459
#> 2 Tabb High                                        York County                280
#> 3 Grayson County High                              Grayson County             116
#> 4 Green Run Collegiate                             Virginia Beach City         72
#> 5 Achievable Dream Middle/High                     Newport News City           47
#> 6 Open High                                        Richmond City               46
#> 7 Richmond Community High                          Richmond City               40
```

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#thomas-jefferson-virginias-100-graduation-factory)

### 9. Hampton Roads: Norfolk is the outlier

Among Hampton Roads' five largest divisions, Norfolk City graduates just 82% of its students, lagging 10+ points behind neighbors like Hampton City (96.4%) and Virginia Beach (95.3%).

```r
hr_divisions <- c("Virginia Beach City", "Norfolk City",
                   "Newport News City", "Hampton City", "Chesapeake City")

hr_grad <- all_grad |>
  filter(is_school, diploma_type == "all",
         division_name %in% hr_divisions) |>
  group_by(end_year, division_name) |>
  summarize(
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  )

hr_grad |>
  filter(end_year == 2023) |>
  arrange(desc(grad_rate)) |>
  mutate(grad_pct = round(grad_rate * 100, 1))
```
```
#> # A tibble: 5 x 6
#>   end_year division_name       cohort graduates grad_rate grad_pct
#>      <int> <chr>                <int>     <int>     <dbl>    <dbl>
#> 1     2023 Hampton City          1461      1408     0.964     96.4
#> 2     2023 Virginia Beach City   5111      4873     0.953     95.3
#> 3     2023 Newport News City     1745      1645     0.943     94.3
#> 4     2023 Chesapeake City       3294      3037     0.922     92.2
#> 5     2023 Norfolk City          1800      1475     0.819     81.9
```

![Hampton Roads graduation rates](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/hampton-roads-grad-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#hampton-roads-norfolk-is-the-outlier)

### 10. Virginia's largest high schools are all in Northern Virginia

Alexandria City High School leads the state with 1,138 seniors in its 2023 cohort. Eight of the ten largest high schools are in Fairfax, Prince William, or Arlington counties.

```r
big_schools <- all_grad |>
  filter(is_school, diploma_type == "all", end_year == 2023) |>
  arrange(desc(cohort_size)) |>
  select(school_name, division_name, cohort_size, graduation_rate) |>
  head(10) |>
  mutate(grad_pct = round(graduation_rate * 100, 1))

big_schools
```
```
#> # A tibble: 10 x 5
#>    school_name                 division_name         cohort_size graduation_rate grad_pct
#>    <chr>                       <chr>                       <int>           <dbl>    <dbl>
#>  1 Alexandria City High School Alexandria City               1138           0.831     83.1
#>  2 Lake Braddock Secondary     Fairfax County                 722           0.983     98.3
#>  3 Charles J. Colgan Sr. High  Prince William County          709           0.982     98.2
#>  4 Chantilly High              Fairfax County                 703           0.970     97.0
#>  5 West Potomac High           Fairfax County                 694           0.955     95.5
#>  6 Oakton High                 Fairfax County                 690           0.971     97.1
#>  7 Washington-Liberty High     Arlington County               682           0.922     92.2
#>  8 Yorktown High               Arlington County               678           0.979     97.9
#>  9 Osbourn Park High           Prince William County          672           0.921     92.1
#> 10 Unity Reed High             Prince William County          666           0.793     79.3
```

![Largest Virginia high schools](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/largest-schools-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#virginias-largest-high-schools-are-all-in-northern-virginia)

### 11. Virginia loses 5,300 students to dropout every year

Over the 2019-2023 period, Virginia averaged roughly 5,000 dropouts per year. The 2021 pandemic-policy dip to 4,129 was temporary; 2023 saw 5,319 dropouts.

```r
dropout_counts <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, cohort_size, total_graduates, dropouts, still_enrolled) |>
  mutate(
    dropout_pct = round(dropouts / cohort_size * 100, 1),
    still_enrolled_pct = round(still_enrolled / cohort_size * 100, 1)
  )

dropout_counts
```
```
#> # A tibble: 5 x 7
#>   end_year cohort_size total_graduates dropouts still_enrolled dropout_pct still_enrolled_pct
#>      <int>       <int>           <int>    <int>          <int>       <dbl>              <dbl>
#> 1     2019       98241           89991     5410           2840         5.5                2.9
#> 2     2020       98327           90971     5008           2348         5.1                2.4
#> 3     2021       97096           90325     4129           2642         4.3                2.7
#> 4     2022       98281           90603     5061           2617         5.2                2.7
#> 5     2023       98927           90944     5319           2664         5.4                2.7
```

![Non-graduates by outcome](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/dropout-count-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#virginia-loses-5300-students-to-dropout-every-year)

### 12. Alexandria City High: one school, 1,138 seniors, 83% graduation

Alexandria City High School is Virginia's single largest high school by cohort, yet its 83.1% graduation rate trails the state average by nearly 9 points. As the only public high school in Alexandria City, every student funnels through one building.

```r
alex <- all_grad |>
  filter(is_school, diploma_type == "all",
         grepl("Alexandria City High", school_name)) |>
  select(end_year, school_name, cohort_size, graduation_rate, dropout_rate) |>
  mutate(grad_pct = round(graduation_rate * 100, 1))

alex
```
```
#> # A tibble: 5 x 6
#>   end_year school_name                 cohort_size graduation_rate dropout_rate grad_pct
#>      <int> <chr>                             <int>           <dbl>        <dbl>    <dbl>
#> 1     2019 Alexandria City High School        1031           0.849       0.0981     84.9
#> 2     2020 Alexandria City High School        1075           0.853       0.0949     85.3
#> 3     2021 Alexandria City High School        1143           0.876       0.0665     87.6
#> 4     2022 Alexandria City High School        1115           0.861       0.0825     86.1
#> 5     2023 Alexandria City High School        1138           0.831       0.128      83.1
```

![Alexandria City High School trends](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/alexandria-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#alexandria-city-high-one-school-1138-seniors-83-graduation)

### 13. Fairfax County alone graduates 14,000 students per year

With 14,907 seniors in its 2023 cohort across 30 high schools, Fairfax County produces more graduates than many states. Its graduation rate has climbed from 91.3% in 2019 to 93.4% in 2023.

```r
fairfax <- all_grad |>
  filter(is_school, diploma_type == "all",
         division_name == "Fairfax County") |>
  group_by(end_year) |>
  summarize(
    n_schools = n(),
    cohort = sum(cohort_size, na.rm = TRUE),
    graduates = sum(total_graduates, na.rm = TRUE),
    grad_rate = graduates / cohort,
    .groups = "drop"
  ) |>
  mutate(grad_pct = round(grad_rate * 100, 1))

fairfax
```
```
#> # A tibble: 5 x 6
#>   end_year n_schools cohort graduates grad_rate grad_pct
#>      <int>     <int>  <int>     <int>     <dbl>    <dbl>
#> 1     2019        30  14936     13636     0.913     91.3
#> 2     2020        30  14793     13762     0.930     93.0
#> 3     2021        30  14641     13859     0.947     94.7
#> 4     2022        30  14801     13921     0.941     94.1
#> 5     2023        30  14907     13923     0.934     93.4
```

![Fairfax County graduation trend](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/fairfax-grad-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#fairfax-county-alone-graduates-14000-students-per-year)

### 14. The biggest school-level improvements since 2019

Lancaster High School improved its graduation rate by 14 percentage points between 2019 and 2023 (from 83.8% to 97.8%). Armstrong High in Richmond jumped nearly 12 points.

```r
improvement <- all_grad |>
  filter(is_school, diploma_type == "all", end_year %in% c(2019, 2023)) |>
  select(end_year, school_name, division_name, graduation_rate, cohort_size) |>
  pivot_wider(names_from = end_year, values_from = c(graduation_rate, cohort_size)) |>
  filter(!is.na(graduation_rate_2019), !is.na(graduation_rate_2023),
         cohort_size_2023 >= 50) |>
  mutate(change = round((graduation_rate_2023 - graduation_rate_2019) * 100, 1)) |>
  arrange(desc(change)) |>
  head(10)

improvement |>
  select(school_name, division_name, graduation_rate_2019, graduation_rate_2023,
         change, cohort_size_2023) |>
  mutate(
    rate_2019 = round(graduation_rate_2019 * 100, 1),
    rate_2023 = round(graduation_rate_2023 * 100, 1)
  )
```
```
#> # A tibble: 10 x 8
#>    school_name          division_name     graduation_rate_2019 graduation_rate_2023 change cohort_size_2023 rate_2019 rate_2023
#>    <chr>                <chr>                            <dbl>                <dbl>  <dbl>            <int>     <dbl>     <dbl>
#>  1 Lancaster High       Lancaster County                 0.838                0.978   13.9               90      83.8      97.8
#>  2 Armstrong High       Richmond City                    0.652                0.770   11.8              174      65.2      77.0
#>  3 Osbourn High         Manassas City                    0.777                0.895   11.8              562      77.7      89.5
#>  4 Amherst County High  Amherst County                   0.862                0.960    9.8              299      86.2      96.0
#>  5 Huguenot High        Richmond City                    0.682                0.780    9.8              355      68.2      78.0
#>  6 Rustburg High        Campbell County                  0.901                0.995    9.4              195      90.1      99.5
#>  7 Fairfax County Adult High Fairfax County              0.069                0.153    8.4              203       6.9      15.3
#>  8 Phoebus High         Hampton City                     0.899                0.982    8.4              283      89.9      98.2
#>  9 West Potomac High    Fairfax County                   0.872                0.955    8.3              694      87.2      95.5
#> 10 Brunswick High       Brunswick County                 0.800                0.881    8.1              109      80.0      88.1
```

![Biggest graduation rate improvements](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/improvements-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#the-biggest-school-level-improvements-since-2019)

### 15. Five years, 490,000 graduates

From 2019 to 2023, Virginia graduated 452,834 students across five cohorts totaling 490,872 seniors. The state maintains roughly 320 high schools.

```r
summary_stats <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  summarize(
    years = n(),
    total_cohort = sum(cohort_size, na.rm = TRUE),
    total_graduates = sum(total_graduates, na.rm = TRUE),
    total_dropouts = sum(dropouts, na.rm = TRUE),
    avg_grad_rate = round(mean(graduation_rate) * 100, 1)
  )

summary_stats
```
```
#> # A tibble: 1 x 5
#>   years total_cohort total_graduates total_dropouts avg_grad_rate
#>   <int>        <int>           <int>          <int>         <dbl>
#> 1     5       490872          451834          24927          92.2
```

```r
school_counts <- all_grad |>
  filter(is_school, diploma_type == "all") |>
  group_by(end_year) |>
  summarize(n_schools = n_distinct(school_name), .groups = "drop")

school_counts
```
```
#> # A tibble: 5 x 2
#>   end_year n_schools
#>      <int>     <int>
#> 1     2019       316
#> 2     2020       318
#> 3     2021       317
#> 4     2022       316
#> 5     2023       320
```

![Cohort size vs graduates](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks_files/figure-html/five-year-summary-chart-1.png)

[(source)](https://almartin82.github.io/vaschooldata/articles/enrollment_hooks.html#five-years-490000-graduates)
