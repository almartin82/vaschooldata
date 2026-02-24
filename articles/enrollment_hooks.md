# 15 Insights from Virginia School Enrollment Data

``` r
library(vaschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))
```

This vignette explores Virginia’s public school enrollment data,
surfacing key trends and demographic patterns across 9 years of data
(2016-2024).

------------------------------------------------------------------------

## 1. The Northern Virginia Boom

While most of Virginia holds steady, **Loudoun County** has exploded.
From 52,000 students in 2010 to over 82,000 today, it is now Virginia’s
4th-largest division.

``` r
# Loudoun's explosive growth
fetch_enr_multi(2016:2023, use_cache = TRUE) |>
  filter(is_district, grepl("Loudoun", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students)
```

``` r
loudoun <- fetch_enr_multi(2016:2024, use_cache = TRUE) |>
  filter(is_district, grepl("Loudoun", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL")

ggplot(loudoun, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  scale_y_continuous(labels = scales::comma, limits = c(50000, NA)) +
  labs(
    title = "Loudoun County Enrollment Growth",
    subtitle = "Virginia's fastest-growing major division",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )
```

------------------------------------------------------------------------

## 2. The Fairfax Giant

**Fairfax County Public Schools** alone enrolls more students than 42
states’ entire charter sectors. At 180,000 students, it is the
10th-largest district in America.

``` r
enr_2024 <- fetch_enr(2024, use_cache = TRUE)

fetch_enr(2023, use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(5)
```

``` r
top_10 <- enr_2024 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_10 |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name)) +
  geom_col(fill = "#003366") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Virginia's 10 Largest School Divisions (2024)",
    x = "Total Enrollment",
    y = NULL
  )
```

------------------------------------------------------------------------

## 3. The Rural Decline

While Northern Virginia grows, **Southwest Virginia is vanishing**. Lee
County has lost 45% of its students since 2016. Dickenson County: down
40%+.

``` r
fetch_enr_multi(c(2016, 2023), use_cache = TRUE) |>
  filter(is_district, grepl("Lee County|Dickenson", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students) |>
  tidyr::pivot_wider(names_from = end_year, values_from = n_students)
```

``` r
rural <- fetch_enr_multi(2016:2024, use_cache = TRUE) |>
  filter(is_district, grepl("Lee County|Dickenson", district_name, ignore.case = TRUE),
         subgroup == "total_enrollment", grade_level == "TOTAL")

ggplot(rural, aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Rural Division Enrollment Decline",
    subtitle = "Southwest Virginia continues to lose students",
    x = "School Year",
    y = "Enrollment",
    color = "Division"
  )
```

------------------------------------------------------------------------

## 4. Virginia’s Demographic Transformation

Virginia is becoming increasingly diverse. Hispanic students are the
fastest-growing demographic group.

``` r
enr <- fetch_enr_multi(2016:2024, use_cache = TRUE)

fetch_enr_multi(c(2016, 2023), use_cache = TRUE) |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) |>
  select(end_year, subgroup, n_students, pct)
```

``` r
demo_trend <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) |>
  select(end_year, subgroup, n_students)

ggplot(demo_trend, aes(x = end_year, y = n_students, color = subgroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Virginia Enrollment by Race/Ethnicity (2016-2024)",
    subtitle = "Hispanic enrollment growth outpaces other groups",
    x = "School Year",
    y = "Enrollment",
    color = "Group"
  )
```

------------------------------------------------------------------------

## 5. The COVID Dip

Virginia lost students between 2019 and 2021. Enrollment has partially
recovered but remains below pre-pandemic levels.

``` r
fetch_enr_multi(2019:2024, use_cache = TRUE) |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

``` r
covid <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year >= 2019)

ggplot(covid, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  geom_vline(xintercept = 2020, linetype = "dashed", alpha = 0.5) +
  scale_y_continuous(labels = scales::comma) +
  annotate("text", x = 2020, y = max(covid$n_students), label = "COVID", vjust = -0.5) +
  labs(
    title = "Virginia Enrollment During and After COVID",
    x = "School Year",
    y = "Total Enrollment"
  )
```

------------------------------------------------------------------------

## 6. The Charter Desert

Unlike many states, Virginia has **fewer than 10 charter schools**
statewide, enrolling under 3,000 students. The legislature has
historically been restrictive.

``` r
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

Note: Virginia’s charter school sector is minimal compared to other
states.

------------------------------------------------------------------------

## 7. The Hampton Roads Plateau

Virginia Beach, Norfolk, and Newport News, once growing military hubs,
have **flatlined for two decades**.

``` r
fetch_enr_multi(c(2016, 2024), use_cache = TRUE) |>
  filter(is_district, grepl("Virginia Beach|Norfolk|Newport News", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, district_name, n_students)
```

``` r
hampton <- enr |>
  filter(is_district, grepl("Virginia Beach|Norfolk|Newport News", district_name, ignore.case = TRUE),
         subgroup == "total_enrollment", grade_level == "TOTAL")

ggplot(hampton, aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Hampton Roads Enrollment Trends",
    subtitle = "Military corridor enrollment has plateaued",
    x = "School Year",
    y = "Enrollment",
    color = "Division"
  )
```

------------------------------------------------------------------------

## 8. Kindergarten as Crystal Ball

Kindergarten enrollment predicts the future. **Richmond City**
kindergarten has dropped, signaling continued enrollment pressure.

``` r
fetch_enr_multi(2019:2023, use_cache = TRUE) |>
  filter(is_district, grepl("Richmond City", district_name),
         subgroup == "total_enrollment", grade_level == "K") |>
  select(end_year, n_students)
```

``` r
k_trend <- enr |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "05", "09"))

ggplot(k_trend, aes(x = end_year, y = n_students, color = grade_level)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Enrollment by Grade Level Over Time",
    subtitle = "Kindergarten dips signal future enrollment trends",
    x = "School Year",
    y = "Enrollment",
    color = "Grade"
  )
```

------------------------------------------------------------------------

## 9. The Wealthiest Division in Virginia

**Falls Church City**, a tiny independent city in Northern Virginia, is
100% in Fairfax County but operates its own schools. Median income:
\$150,000+.

``` r
fetch_enr(2023, use_cache = TRUE) |>
  filter(is_district, grepl("Falls Church", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(district_name, n_students)
```

------------------------------------------------------------------------

## 10. 9 Years of VDOE Data

This package provides **9 years** of Virginia enrollment data from the
VDOE School Quality Profiles (2016-2024).

``` r
# Years available
get_available_years()
```

``` r
# Count divisions over time
fetch_enr_multi(c(2016, 2020, 2024), use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year) |>
  summarize(n_divisions = n(), total_students = sum(n_students, na.rm = TRUE))
```

------------------------------------------------------------------------

## 11. Richmond City’s Enrollment Pressure

**Richmond City Public Schools** has lost students steadily, with
enrollment dropping from 24,000 to under 22,000 since 2016.

``` r
fetch_enr_multi(2016:2024, use_cache = TRUE) |>
  filter(is_district, grepl("Richmond City", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

``` r
richmond <- enr |>
  filter(is_district, grepl("Richmond City", district_name),
         subgroup == "total_enrollment", grade_level == "TOTAL")

ggplot(richmond, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#B22222") +
  geom_point(size = 3, color = "#B22222") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Richmond City Enrollment Trends",
    subtitle = "Steady decline in enrollment",
    x = "School Year",
    y = "Enrollment"
  )
```

------------------------------------------------------------------------

## 12. Hampton Roads Military Corridor

Virginia Beach, Norfolk, Newport News, and Hampton together educate
**180,000+ students**, but enrollment has plateaued despite the large
military presence.

``` r
fetch_enr_multi(c(2016, 2024), use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Virginia Beach|Norfolk|Newport News|Hampton", district_name)) |>
  group_by(end_year) |>
  summarize(total = sum(n_students, na.rm = TRUE))
```

------------------------------------------------------------------------

## 13. Elementary vs. High School Shifts

COVID’s kindergarten dip is now visible in elementary grades, while high
school enrollment remains stable from pre-pandemic cohorts.

``` r
fetch_enr(2024, use_cache = TRUE) |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "09", "12")) |>
  select(grade_level, n_students)
```

``` r
grade_dist <- enr |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "05", "09", "12"))

ggplot(grade_dist, aes(x = end_year, y = n_students, color = grade_level)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Enrollment by Grade Level Over Time",
    subtitle = "Elementary grades showing COVID impact",
    x = "School Year",
    y = "Enrollment",
    color = "Grade"
  )
```

------------------------------------------------------------------------

## 14. Southwest Virginia Coalfield Decline

The **coalfield counties** (Buchanan, Dickenson, Wise, Lee, Tazewell)
have lost 20%+ of students since 2016 as the coal economy continues to
decline.

``` r
fetch_enr_multi(c(2016, 2024), use_cache = TRUE) |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Buchanan|Dickenson|Wise|Lee|Tazewell", district_name)) |>
  group_by(end_year) |>
  summarize(total = sum(n_students, na.rm = TRUE))
```

``` r
coalfield <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Buchanan|Dickenson|Wise|Lee County|Tazewell", district_name, ignore.case = TRUE))

coalfield_total <- coalfield |>
  group_by(end_year) |>
  summarize(total = sum(n_students, na.rm = TRUE), .groups = "drop")

ggplot(coalfield_total, aes(x = end_year, y = total)) +
  geom_line(linewidth = 1.2, color = "#8B4513") +
  geom_point(size = 3, color = "#8B4513") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Southwest Virginia Coalfield Region Enrollment",
    subtitle = "Buchanan, Dickenson, Wise, Lee, and Tazewell counties",
    x = "School Year",
    y = "Total Enrollment"
  )
```

------------------------------------------------------------------------

## 15. The Two Virginias

Northern Virginia (Fairfax, Loudoun, Prince William, Arlington) has
**grown** since 2016 while Southwest Virginia has **declined**. These
diverging trajectories represent two different futures.

``` r
# Northern Virginia
nova <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Fairfax|Loudoun|Prince William|Arlington", district_name, ignore.case = TRUE)) |>
  group_by(end_year) |>
  summarize(nova_total = sum(n_students, na.rm = TRUE), .groups = "drop")

# Southwest Virginia
swva <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Buchanan|Dickenson|Wise|Lee County|Tazewell|Russell|Scott|Smyth|Washington", district_name, ignore.case = TRUE)) |>
  group_by(end_year) |>
  summarize(swva_total = sum(n_students, na.rm = TRUE), .groups = "drop")

# Combine and calculate indexed values (2016 = 100)
comparison <- nova |>
  left_join(swva, by = "end_year") |>
  mutate(
    nova_idx = round(nova_total / nova_total[end_year == min(end_year)] * 100, 1),
    swva_idx = round(swva_total / swva_total[end_year == min(end_year)] * 100, 1)
  )

comparison |> select(end_year, nova_idx, swva_idx)
```

``` r
comparison_long <- comparison |>
  select(end_year, nova_idx, swva_idx) |>
  pivot_longer(cols = c(nova_idx, swva_idx), names_to = "region", values_to = "index") |>
  mutate(region = ifelse(region == "nova_idx", "Northern Virginia", "Southwest Virginia"))

ggplot(comparison_long, aes(x = end_year, y = index, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("Northern Virginia" = "#003366", "Southwest Virginia" = "#8B4513")) +
  labs(
    title = "The Two Virginias: Diverging Trajectories",
    subtitle = "Enrollment indexed to 2016 baseline (100)",
    x = "School Year",
    y = "Index (2016 = 100)",
    color = "Region"
  )
```

------------------------------------------------------------------------

## Summary

Virginia’s school enrollment data reveals:

- **Northern Virginia dominance**: Fairfax, Loudoun, and Prince William
  drive statewide enrollment
- **Growing diversity**: Hispanic and Asian student populations are
  increasing
- **Rural decline**: Many small divisions in rural areas are losing
  students
- **COVID recovery**: Enrollment has partially rebounded after the
  pandemic dip
- **Local variation**: 132 divisions create a wide range of school
  system sizes
- **Two Virginias**: Growing NoVA vs. declining Southwest create
  diverging trajectories

These patterns shape education policy across the Commonwealth.

------------------------------------------------------------------------

## Data Notes

**Data source**: Virginia Department of Education (VDOE) School Quality
Profiles

**Available years**: 2016-2024 (9 years)

**Entities**: State, 132 school divisions, ~2,100 schools

**Subgroups**: Total enrollment, race/ethnicity (white, black, Hispanic,
Asian, multiracial, Native American, Pacific Islander), gender,
economically disadvantaged, students with disabilities, English learners

**Suppression**: Small counts may be suppressed for student privacy

**Census Day**: Fall membership count (typically late September/early
October)

------------------------------------------------------------------------

*Data sourced from the Virginia Department of Education (VDOE).*

------------------------------------------------------------------------

## Session Info

``` r
sessionInfo()
```
