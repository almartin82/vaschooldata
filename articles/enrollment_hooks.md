# 10 Insights from Virginia School Enrollment Data

``` r
library(vaschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))
```

This vignette explores Virginia’s public school enrollment data,
surfacing key trends and demographic patterns across 10 years of data
(2016-2025).

------------------------------------------------------------------------

## 1. Virginia serves over 1.2 million students

Virginia’s public schools educate more than 1.2 million students across
132 school divisions (Virginia’s term for districts).

``` r
enr <- fetch_enr_multi(2016:2025)

state_totals <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students),
         pct_change = round(change / lag(n_students) * 100, 2))

state_totals
```

``` r
ggplot(state_totals, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Virginia Public School Enrollment (2016-2025)",
    subtitle = "The Commonwealth educates over 1.2 million students",
    x = "School Year (ending)",
    y = "Total Enrollment"
  )
```

------------------------------------------------------------------------

## 2. Fairfax County is larger than many states

Fairfax County Public Schools, with 180,000+ students, is one of the
largest school systems in America and rivals the total enrollment of
several states.

``` r
enr_2025 <- fetch_enr(2025)

top_10 <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  head(10) |>
  select(district_name, n_students)

top_10
```

``` r
top_10 |>
  mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
  ggplot(aes(x = n_students, y = district_name)) +
  geom_col(fill = "#003366") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Virginia's 10 Largest School Divisions (2025)",
    x = "Total Enrollment",
    y = NULL
  )
```

------------------------------------------------------------------------

## 3. Northern Virginia dominates enrollment

The Northern Virginia suburbs of Washington, D.C. – Fairfax, Loudoun,
and Prince William counties – together educate nearly 400,000 students.

``` r
nova <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         grepl("Fairfax|Loudoun|Prince William", district_name, ignore.case = TRUE)) |>
  select(district_name, n_students) |>
  arrange(desc(n_students))

nova

nova_total <- sum(nova$n_students, na.rm = TRUE)
state_total <- enr_2025 |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  pull(n_students)

nova_pct <- round(nova_total / state_total * 100, 1)
```

Northern Virginia represents approximately 30% of statewide enrollment.

------------------------------------------------------------------------

## 4. Virginia is increasingly diverse

Hispanic and Asian student populations have grown substantially, while
white student enrollment has declined.

``` r
demographics <- enr_2025 |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) |>
  mutate(pct = round(pct * 100, 1)) |>
  select(subgroup, n_students, pct) |>
  arrange(desc(n_students))

demographics
```

``` r
demographics |>
  mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
  ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Virginia Student Demographics (2025)",
    subtitle = "White students remain the plurality, but diversity is growing",
    x = "Number of Students",
    y = NULL
  )
```

------------------------------------------------------------------------

## 5. Loudoun County is Virginia’s growth engine

Loudoun County has been among the fastest-growing school divisions in
the state, driven by data center development and suburban expansion.

``` r
growth_divisions <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(2016, 2025)) |>
  group_by(district_name) |>
  filter(n() == 2) |>
  summarize(
    y2016 = n_students[end_year == 2016],
    y2025 = n_students[end_year == 2025],
    change = y2025 - y2016,
    pct_change = round((y2025 / y2016 - 1) * 100, 1),
    .groups = "drop"
  ) |>
  filter(y2016 > 10000) |>
  arrange(desc(pct_change)) |>
  head(10)

growth_divisions |>
  mutate(district_name = forcats::fct_reorder(district_name, pct_change)) |>
  ggplot(aes(x = pct_change, y = district_name)) +
  geom_col(fill = "#4CAF50") +
  geom_text(aes(label = paste0(pct_change, "%")), hjust = -0.1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Fastest-Growing Virginia School Divisions (2016-2025)",
    subtitle = "Among divisions with 10,000+ students in 2016",
    x = "Percent Change",
    y = NULL
  )
```

------------------------------------------------------------------------

## 6. COVID caused a temporary dip

Virginia enrollment dipped during the 2020-21 school year but has
largely recovered.

``` r
covid <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year >= 2019) |>
  select(end_year, n_students) |>
  mutate(change = n_students - lag(n_students))

covid
```

------------------------------------------------------------------------

## 7. Hispanic enrollment has surged

Hispanic students are the fastest-growing demographic group in Virginia
schools.

``` r
hispanic_trend <- enr |>
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("hispanic", "white", "black", "asian")) |>
  select(end_year, subgroup, n_students)

hispanic_trend |>
  ggplot(aes(x = end_year, y = n_students, color = subgroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Virginia Enrollment by Race/Ethnicity (2016-2025)",
    subtitle = "Hispanic enrollment growth outpaces other groups",
    x = "School Year",
    y = "Enrollment",
    color = "Group"
  )
```

------------------------------------------------------------------------

## 8. Some rural divisions are declining

While Northern Virginia grows, many rural divisions in Southwest
Virginia and the Southside have seen enrollment declines.

``` r
declining <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         end_year %in% c(2016, 2025)) |>
  group_by(district_name) |>
  filter(n() == 2) |>
  summarize(
    y2016 = n_students[end_year == 2016],
    y2025 = n_students[end_year == 2025],
    pct_change = round((y2025 / y2016 - 1) * 100, 1),
    .groups = "drop"
  ) |>
  filter(y2016 > 1000) |>
  arrange(pct_change) |>
  head(10)

declining
```

------------------------------------------------------------------------

## 9. Kindergarten enrollment signals future trends

Kindergarten enrollment serves as a leading indicator of future student
populations.

``` r
k_trend <- enr |>
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("K", "01", "05", "09"),
         end_year >= 2019) |>
  select(end_year, grade_level, n_students) |>
  pivot_wider(names_from = grade_level, values_from = n_students)

k_trend
```

------------------------------------------------------------------------

## 10. Virginia’s 132 divisions create local variation

Virginia’s school division structure – based on cities and counties –
creates significant variation in size and demographics.

``` r
division_stats <- enr_2025 |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  summarize(
    n_divisions = n_distinct(district_name),
    total_students = sum(n_students, na.rm = TRUE),
    median_size = median(n_students, na.rm = TRUE),
    min_size = min(n_students, na.rm = TRUE),
    max_size = max(n_students, na.rm = TRUE)
  )

division_stats
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
- **COVID recovery**: Enrollment has rebounded after the pandemic dip
- **Local variation**: 132 divisions create a wide range of school
  system sizes

These patterns shape education policy across the Commonwealth.

------------------------------------------------------------------------

*Data sourced from the Virginia Department of Education (VDOE).*
