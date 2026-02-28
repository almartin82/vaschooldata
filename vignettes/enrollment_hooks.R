## ----setup, include=FALSE-----------------------------------------------------
# Check if VDOE enrollment data is fetchable (CAPTCHA may block)
can_fetch_enr <- tryCatch({
  test_data <- vaschooldata::fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  is.data.frame(test_data) && nrow(test_data) > 0
}, error = function(e) FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5,
  cache = TRUE
)


## ----load-packages, eval=TRUE-------------------------------------------------
library(vaschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)

theme_set(theme_minimal(base_size = 14))


## ----grad-trend---------------------------------------------------------------
all_grad <- bind_rows(lapply(2019:2023, function(yr) {
  fetch_graduation(yr, use_cache = TRUE)
}))

state_grad <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, graduation_rate, cohort_size, total_graduates, dropout_rate)

state_grad |>
  mutate(grad_pct = round(graduation_rate * 100, 2),
         dropout_pct = round(dropout_rate * 100, 2))


## ----grad-trend-chart---------------------------------------------------------
stopifnot(nrow(state_grad) > 0)

ggplot(state_grad, aes(x = end_year, y = graduation_rate)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  scale_y_continuous(labels = scales::percent, limits = c(0.88, 0.96)) +
  labs(
    title = "Virginia 4-Year Graduation Rate (2019-2023)",
    subtitle = "Consistently above 91%, peaking in 2021",
    x = "Cohort Year",
    y = "Graduation Rate"
  )


## ----diploma-breakdown--------------------------------------------------------
diplomas <- all_grad |>
  filter(is_state, end_year == 2023, diploma_type != "all", !is.na(diploma_count)) |>
  select(diploma_type, diploma_count) |>
  mutate(pct = round(diploma_count / sum(diploma_count) * 100, 1)) |>
  arrange(desc(diploma_count))

diplomas


## ----diploma-breakdown-chart--------------------------------------------------
stopifnot(nrow(diplomas) > 0)

diplomas |>
  mutate(diploma_type = forcats::fct_reorder(diploma_type, diploma_count)) |>
  ggplot(aes(x = diploma_count, y = diploma_type)) +
  geom_col(fill = "#003366") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Virginia Diplomas by Type (2023)",
    subtitle = "Advanced Studies is the most common diploma",
    x = "Number of Graduates",
    y = NULL
  )


## ----richmond-grad------------------------------------------------------------
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


## ----richmond-grad-chart------------------------------------------------------
stopifnot(nrow(richmond_trend) > 0)

ggplot(richmond_trend, aes(x = end_year, y = grad_rate)) +
  geom_line(linewidth = 1.2, color = "#B22222") +
  geom_point(size = 3, color = "#B22222") +
  geom_hline(yintercept = 0.9193, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 2022, y = 0.93, label = "State average (91.9%)", size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0.65, 0.96)) +
  labs(
    title = "Richmond City Graduation Rate (2019-2023)",
    subtitle = "Persistently below state average, with a brief COVID-era bump",
    x = "Cohort Year",
    y = "Graduation Rate"
  )


## ----nova-grad----------------------------------------------------------------
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


## ----nova-grad-chart----------------------------------------------------------
stopifnot(nrow(nova_grad) > 0)

ggplot(nova_grad, aes(x = end_year, y = grad_rate, color = division_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0.88, 1.0)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Northern Virginia Graduation Rates (2019-2023)",
    subtitle = "Loudoun County leads the region",
    x = "Cohort Year",
    y = "Graduation Rate",
    color = "Division"
  )


## ----covid-dropout------------------------------------------------------------
dropout_trend <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, graduation_rate, dropout_rate) |>
  mutate(
    grad_pct = round(graduation_rate * 100, 2),
    dropout_pct = round(dropout_rate * 100, 2)
  )

dropout_trend


## ----covid-dropout-chart------------------------------------------------------
stopifnot(nrow(dropout_trend) > 0)

ggplot(dropout_trend, aes(x = end_year, y = dropout_rate)) +
  geom_line(linewidth = 1.2, color = "#D32F2F") +
  geom_point(size = 3, color = "#D32F2F") +
  geom_vline(xintercept = 2021, linetype = "dashed", alpha = 0.5) +
  annotate("text", x = 2021.3, y = 0.052, label = "2021\npandemic\npolicies", size = 3) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Virginia Statewide Dropout Rate (2019-2023)",
    subtitle = "Pandemic-era policies temporarily reduced dropouts",
    x = "Cohort Year",
    y = "Dropout Rate"
  )


## ----tj-trend-----------------------------------------------------------------
tj <- all_grad |>
  filter(is_school, diploma_type == "all",
         grepl("Thomas Jefferson High for Science", school_name)) |>
  select(end_year, school_name, graduation_rate, cohort_size)

tj


## ----perfect-schools----------------------------------------------------------
perfect <- all_grad |>
  filter(is_school, diploma_type == "all", end_year == 2023,
         graduation_rate == 1.0, cohort_size >= 30) |>
  select(school_name, division_name, cohort_size) |>
  arrange(desc(cohort_size))

perfect


## ----hampton-roads-grad-------------------------------------------------------
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


## ----hampton-roads-grad-chart-------------------------------------------------
stopifnot(nrow(hr_grad) > 0)

ggplot(hr_grad, aes(x = end_year, y = grad_rate, color = division_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0.75, 1.0)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Hampton Roads Graduation Rates (2019-2023)",
    subtitle = "Norfolk City trails the region by a wide margin",
    x = "Cohort Year",
    y = "Graduation Rate",
    color = "Division"
  )


## ----swva-grad----------------------------------------------------------------
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


## ----swva-grad-chart----------------------------------------------------------
stopifnot(nrow(swva_grad) > 0)

ggplot(swva_grad, aes(x = end_year, y = grad_rate, color = division_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0.75, 1.0)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Southwest Virginia Coalfield Graduation Rates (2019-2023)",
    subtitle = "Small coalfield divisions graduate at high rates despite population loss",
    x = "Cohort Year",
    y = "Graduation Rate",
    color = "Division"
  )


## ----largest-schools----------------------------------------------------------
big_schools <- all_grad |>
  filter(is_school, diploma_type == "all", end_year == 2023) |>
  arrange(desc(cohort_size)) |>
  select(school_name, division_name, cohort_size, graduation_rate) |>
  head(10) |>
  mutate(grad_pct = round(graduation_rate * 100, 1))

big_schools


## ----largest-schools-chart----------------------------------------------------
stopifnot(nrow(big_schools) > 0)

big_schools |>
  mutate(school_label = paste0(school_name, " (", division_name, ")"),
         school_label = forcats::fct_reorder(school_label, cohort_size)) |>
  ggplot(aes(x = cohort_size, y = school_label)) +
  geom_col(fill = "#003366") +
  geom_text(aes(label = paste0(grad_pct, "%")), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Virginia's 10 Largest High Schools by Cohort (2023)",
    subtitle = "Labels show graduation rate",
    x = "Cohort Size",
    y = NULL
  )


## ----dropout-count------------------------------------------------------------
dropout_counts <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, cohort_size, total_graduates, dropouts, still_enrolled) |>
  mutate(
    dropout_pct = round(dropouts / cohort_size * 100, 1),
    still_enrolled_pct = round(still_enrolled / cohort_size * 100, 1)
  )

dropout_counts


## ----dropout-count-chart------------------------------------------------------
stopifnot(nrow(dropout_counts) > 0)

dropout_long <- dropout_counts |>
  select(end_year, Dropouts = dropouts, `Still Enrolled` = still_enrolled) |>
  pivot_longer(cols = -end_year, names_to = "outcome", values_to = "count")

ggplot(dropout_long, aes(x = end_year, y = count, fill = outcome)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Dropouts" = "#D32F2F", "Still Enrolled" = "#FF9800")) +
  labs(
    title = "Virginia Non-Graduates by Outcome (2019-2023)",
    subtitle = "Students who left high school without graduating",
    x = "Cohort Year",
    y = "Number of Students",
    fill = NULL
  )


## ----alexandria---------------------------------------------------------------
alex <- all_grad |>
  filter(is_school, diploma_type == "all",
         grepl("Alexandria City High", school_name)) |>
  select(end_year, school_name, cohort_size, graduation_rate, dropout_rate) |>
  mutate(grad_pct = round(graduation_rate * 100, 1))

alex


## ----alexandria-chart---------------------------------------------------------
stopifnot(nrow(alex) > 0)

ggplot(alex, aes(x = end_year)) +
  geom_col(aes(y = cohort_size), fill = "#B0BEC5", alpha = 0.6) +
  geom_line(aes(y = graduation_rate * 1200), color = "#003366", linewidth = 1.2) +
  geom_point(aes(y = graduation_rate * 1200), color = "#003366", size = 3) +
  scale_y_continuous(
    name = "Cohort Size",
    labels = scales::comma,
    sec.axis = sec_axis(~ . / 1200, name = "Graduation Rate", labels = scales::percent)
  ) +
  labs(
    title = "Alexandria City High School (2019-2023)",
    subtitle = "Virginia's largest high school by cohort size",
    x = "Cohort Year"
  )


## ----fairfax-grad-------------------------------------------------------------
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


## ----fairfax-grad-chart-------------------------------------------------------
stopifnot(nrow(fairfax) > 0)

ggplot(fairfax, aes(x = end_year, y = grad_rate)) +
  geom_line(linewidth = 1.2, color = "#2E7D32") +
  geom_point(size = 3, color = "#2E7D32") +
  scale_y_continuous(labels = scales::percent, limits = c(0.88, 0.96)) +
  labs(
    title = "Fairfax County Graduation Rate (2019-2023)",
    subtitle = "30 high schools, 14,900+ seniors per year",
    x = "Cohort Year",
    y = "Graduation Rate"
  )


## ----improvements-------------------------------------------------------------
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


## ----improvements-chart-------------------------------------------------------
stopifnot(nrow(improvement) > 0)

improvement |>
  mutate(school_label = paste0(school_name, " (", division_name, ")"),
         school_label = forcats::fct_reorder(school_label, change)) |>
  ggplot(aes(x = change, y = school_label)) +
  geom_col(fill = "#4CAF50") +
  geom_text(aes(label = paste0("+", change, " pp")), hjust = -0.1, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Biggest Graduation Rate Improvements (2019 to 2023)",
    subtitle = "Schools with 50+ seniors in 2023 cohort",
    x = "Percentage Point Change",
    y = NULL
  )


## ----division-gap-------------------------------------------------------------
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

cat("\nBottom 5 divisions:\n")
div_grad |> arrange(grad_rate) |>
  head(5) |>
  mutate(grad_pct = round(grad_rate * 100, 1))


## ----division-gap-chart-------------------------------------------------------
stopifnot(nrow(div_grad) > 0)

extremes <- bind_rows(
  div_grad |> arrange(desc(grad_rate)) |> head(5) |> mutate(group = "Top 5"),
  div_grad |> arrange(grad_rate) |> head(5) |> mutate(group = "Bottom 5")
)

extremes |>
  mutate(division_name = forcats::fct_reorder(division_name, grad_rate)) |>
  ggplot(aes(x = grad_rate, y = division_name, fill = group)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1.05)) +
  scale_fill_manual(values = c("Top 5" = "#2E7D32", "Bottom 5" = "#D32F2F")) +
  labs(
    title = "Virginia Division Graduation Rates: Top and Bottom 5 (2023)",
    subtitle = "Among divisions with 50+ seniors",
    x = "Graduation Rate",
    y = NULL,
    fill = NULL
  )


## ----five-year-summary--------------------------------------------------------
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

school_counts <- all_grad |>
  filter(is_school, diploma_type == "all") |>
  group_by(end_year) |>
  summarize(n_schools = n_distinct(school_name), .groups = "drop")

school_counts


## ----five-year-summary-chart--------------------------------------------------
yearly_summary <- all_grad |>
  filter(is_state, diploma_type == "all") |>
  select(end_year, cohort_size, total_graduates)

stopifnot(nrow(yearly_summary) > 0)

yearly_long <- yearly_summary |>
  pivot_longer(cols = c(cohort_size, total_graduates),
               names_to = "measure", values_to = "count") |>
  mutate(measure = ifelse(measure == "cohort_size", "Cohort", "Graduates"))

ggplot(yearly_long, aes(x = end_year, y = count, fill = measure)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Cohort" = "#B0BEC5", "Graduates" = "#003366")) +
  labs(
    title = "Virginia Cohort Size vs. Graduates (2019-2023)",
    subtitle = "490,000+ seniors over five years",
    x = "Cohort Year",
    y = "Number of Students",
    fill = NULL
  )


## ----session-info, eval=TRUE--------------------------------------------------
sessionInfo()

