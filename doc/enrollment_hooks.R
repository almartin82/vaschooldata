## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5,
  eval = FALSE
)

## ----load-packages------------------------------------------------------------
# library(vaschooldata)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# 
# theme_set(theme_minimal(base_size = 14))

## ----statewide-trend----------------------------------------------------------
# enr <- fetch_enr_multi(2016:2025)
# 
# state_totals <- enr |>
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   select(end_year, n_students) |>
#   mutate(change = n_students - lag(n_students),
#          pct_change = round(change / lag(n_students) * 100, 2))
# 
# state_totals

## ----statewide-chart----------------------------------------------------------
# ggplot(state_totals, aes(x = end_year, y = n_students)) +
#   geom_line(linewidth = 1.2, color = "#003366") +
#   geom_point(size = 3, color = "#003366") +
#   scale_y_continuous(labels = scales::comma) +
#   labs(
#     title = "Virginia Public School Enrollment (2016-2025)",
#     subtitle = "The Commonwealth educates over 1.2 million students",
#     x = "School Year (ending)",
#     y = "Total Enrollment"
#   )

## ----top-districts------------------------------------------------------------
# enr_2025 <- fetch_enr(2025)
# 
# top_10 <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   arrange(desc(n_students)) |>
#   head(10) |>
#   select(district_name, n_students)
# 
# top_10

## ----top-districts-chart------------------------------------------------------
# top_10 |>
#   mutate(district_name = forcats::fct_reorder(district_name, n_students)) |>
#   ggplot(aes(x = n_students, y = district_name)) +
#   geom_col(fill = "#003366") +
#   scale_x_continuous(labels = scales::comma) +
#   labs(
#     title = "Virginia's 10 Largest School Divisions (2025)",
#     x = "Total Enrollment",
#     y = NULL
#   )

## ----nova---------------------------------------------------------------------
# nova <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          grepl("Fairfax|Loudoun|Prince William", district_name, ignore.case = TRUE)) |>
#   select(district_name, n_students) |>
#   arrange(desc(n_students))
# 
# nova
# 
# nova_total <- sum(nova$n_students, na.rm = TRUE)
# state_total <- enr_2025 |>
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   pull(n_students)
# 
# nova_pct <- round(nova_total / state_total * 100, 1)

## ----demographics-------------------------------------------------------------
# demographics <- enr_2025 |>
#   filter(is_state, grade_level == "TOTAL",
#          subgroup %in% c("hispanic", "white", "black", "asian", "multiracial")) |>
#   mutate(pct = round(pct * 100, 1)) |>
#   select(subgroup, n_students, pct) |>
#   arrange(desc(n_students))
# 
# demographics

## ----demographics-chart-------------------------------------------------------
# demographics |>
#   mutate(subgroup = forcats::fct_reorder(subgroup, n_students)) |>
#   ggplot(aes(x = n_students, y = subgroup, fill = subgroup)) +
#   geom_col(show.legend = FALSE) +
#   geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
#   scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title = "Virginia Student Demographics (2025)",
#     subtitle = "White students remain the plurality, but diversity is growing",
#     x = "Number of Students",
#     y = NULL
#   )

## ----growth-chart-------------------------------------------------------------
# growth_divisions <- enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          end_year %in% c(2016, 2025)) |>
#   group_by(district_name) |>
#   filter(n() == 2) |>
#   summarize(
#     y2016 = n_students[end_year == 2016],
#     y2025 = n_students[end_year == 2025],
#     change = y2025 - y2016,
#     pct_change = round((y2025 / y2016 - 1) * 100, 1),
#     .groups = "drop"
#   ) |>
#   filter(y2016 > 10000) |>
#   arrange(desc(pct_change)) |>
#   head(10)
# 
# growth_divisions |>
#   mutate(district_name = forcats::fct_reorder(district_name, pct_change)) |>
#   ggplot(aes(x = pct_change, y = district_name)) +
#   geom_col(fill = "#4CAF50") +
#   geom_text(aes(label = paste0(pct_change, "%")), hjust = -0.1) +
#   scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
#   labs(
#     title = "Fastest-Growing Virginia School Divisions (2016-2025)",
#     subtitle = "Among divisions with 10,000+ students in 2016",
#     x = "Percent Change",
#     y = NULL
#   )

## ----covid-impact-------------------------------------------------------------
# covid <- enr |>
#   filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL",
#          end_year >= 2019) |>
#   select(end_year, n_students) |>
#   mutate(change = n_students - lag(n_students))
# 
# covid

## ----regional-chart-----------------------------------------------------------
# hispanic_trend <- enr |>
#   filter(is_state, grade_level == "TOTAL",
#          subgroup %in% c("hispanic", "white", "black", "asian")) |>
#   select(end_year, subgroup, n_students)
# 
# hispanic_trend |>
#   ggplot(aes(x = end_year, y = n_students, color = subgroup)) +
#   geom_line(linewidth = 1.2) +
#   geom_point(size = 2) +
#   scale_y_continuous(labels = scales::comma) +
#   scale_color_brewer(palette = "Set1") +
#   labs(
#     title = "Virginia Enrollment by Race/Ethnicity (2016-2025)",
#     subtitle = "Hispanic enrollment growth outpaces other groups",
#     x = "School Year",
#     y = "Enrollment",
#     color = "Group"
#   )

## ----rural-decline------------------------------------------------------------
# declining <- enr |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
#          end_year %in% c(2016, 2025)) |>
#   group_by(district_name) |>
#   filter(n() == 2) |>
#   summarize(
#     y2016 = n_students[end_year == 2016],
#     y2025 = n_students[end_year == 2025],
#     pct_change = round((y2025 / y2016 - 1) * 100, 1),
#     .groups = "drop"
#   ) |>
#   filter(y2016 > 1000) |>
#   arrange(pct_change) |>
#   head(10)
# 
# declining

## ----kindergarten-------------------------------------------------------------
# k_trend <- enr |>
#   filter(is_state, subgroup == "total_enrollment",
#          grade_level %in% c("K", "01", "05", "09"),
#          end_year >= 2019) |>
#   select(end_year, grade_level, n_students) |>
#   pivot_wider(names_from = grade_level, values_from = n_students)
# 
# k_trend

## ----division-count-----------------------------------------------------------
# division_stats <- enr_2025 |>
#   filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#   summarize(
#     n_divisions = n_distinct(district_name),
#     total_students = sum(n_students, na.rm = TRUE),
#     median_size = median(n_students, na.rm = TRUE),
#     min_size = min(n_students, na.rm = TRUE),
#     max_size = max(n_students, na.rm = TRUE)
#   )
# 
# division_stats

