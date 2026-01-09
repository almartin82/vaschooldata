### CONCURRENT TASK LIMIT
- **Maximum 5 background tasks running simultaneously**
- When launching multiple agents (e.g., for mass audits), batch them in groups of 5
- Wait for the current batch to complete before launching the next batch

---

## CRITICAL DATA SOURCE RULES

**NEVER use Urban Institute API, NCES CCD, or ANY federal data source** — the entire point of these packages is to provide STATE-LEVEL data directly from state DOEs. Federal sources aggregate/transform data differently and lose state-specific details. If a state DOE source is broken, FIX IT or find an alternative STATE source — do not fall back to federal data.

---


# Claude Code Instructions

## Git Commits and PRs
- NEVER reference Claude, Claude Code, or AI assistance in commit messages
- NEVER reference Claude, Claude Code, or AI assistance in PR descriptions
- NEVER add Co-Authored-By lines mentioning Claude or Anthropic
- Keep commit messages focused on what changed, not how it was written

---

## Local Testing Before PRs (REQUIRED)

**PRs will not be merged until CI passes.** Run these checks locally BEFORE opening a PR:

### CI Checks That Must Pass

| Check | Local Command | What It Tests |
|-------|---------------|---------------|
| R-CMD-check | `devtools::check()` | Package builds, tests pass, no errors/warnings |
| Python tests | `pytest tests/test_pyvaschooldata.py -v` | Python wrapper works correctly |
| pkgdown | `pkgdown::build_site()` | Documentation and vignettes render |

### Quick Commands

```r
# R package check (required)
devtools::check()

# Python tests (required)
system("pip install -e ./pyvaschooldata && pytest tests/test_pyvaschooldata.py -v")

# pkgdown build (required)
pkgdown::build_site()
```

### Pre-PR Checklist

Before opening a PR, verify:
- [ ] `devtools::check()` — 0 errors, 0 warnings
- [ ] `pytest tests/test_pyvaschooldata.py` — all tests pass
- [ ] `pkgdown::build_site()` — builds without errors
- [ ] Vignettes render (no `eval=FALSE` hacks)

---

## LIVE Pipeline Testing

This package includes `tests/testthat/test-pipeline-live.R` with LIVE network tests.

### Test Categories:
1. URL Availability - HTTP 200 checks
2. File Download - Verify actual file (not HTML error)
3. File Parsing - readxl/readr succeeds
4. Column Structure - Expected columns exist
5. get_raw_enr() - Raw data function works
6. Data Quality - No Inf/NaN, non-negative counts
7. Aggregation - State total > 0
8. Output Fidelity - tidy=TRUE matches raw

### Running Tests:
```r
devtools::test(filter = "pipeline-live")
```

See `state-schooldata/CLAUDE.md` for complete testing framework documentation.


---

## README Images from Vignettes (REQUIRED)

**NEVER use `man/figures/` or `generate_readme_figs.R` for README images.**

README images MUST come from pkgdown-generated vignette output so they auto-update on merge:

```markdown
![Chart name](https://almartin82.github.io/{package}/articles/{vignette}_files/figure-html/{chunk-name}-1.png)
```

**Why:** Vignette figures regenerate automatically when pkgdown builds. Manual `man/figures/` requires running a separate script and is easy to forget, causing stale/broken images.

---

## Graduation Rate Data (Stage 1 Research Complete)

**Status:** Ready for Stage 2 (TDD Implementation)
**Research Date:** 2026-01-07
**Full Research Report:** `/Users/almartin/Documents/state-schooldata/docs/VA-GRADUATION-RESEARCH.md`

### Data Source

Virginia graduation rate data is available via the **Virginia Open Data Portal** (CKAN API):

- **Portal:** https://data.virginia.gov
- **API:** https://data.virginia.gov/api/3/action/package_search
- **Organization:** Department of Education
- **Contact:** Susan.M.Williams@doe.virginia.gov
- **Authentication:** None required (public)
- **Data Format:** CSV

### Available Years

**5 years available (2019-2023):**

| Year | Dataset | Resource ID | URL | Status |
|------|---------|-------------|-----|--------|
| 2019 | Cohort Graduation and Dropout Report 2019 | 209a1d06-8d24-47e5-9bea-d64f6e7b6406 | [Download](https://data.virginia.gov/dataset/89179d05-0009-4a08-a8bd-4f31ee47ed4a/resource/209a1d06-8d24-47e5-9bea-d64f6e7b6406/download/cohort_statistics-8.csv) | HTTP 200 |
| 2020 | Cohort Graduation and Dropout Report 2020 | 19b88afb-e16d-450d-80fe-9dd11fa87f0d | [Download](https://data.virginia.gov/dataset/c13dbcbb-92fd-4335-9f41-9d6f41b084eb/resource/19b88afb-e16d-450d-80fe-9dd11fa87f0d/download/cohort_statistics-7.csv) | HTTP 200 |
| 2021 | Cohort Graduation and Dropout Report 2021 | 99a342e4-4e83-40f1-9b51-887a5c7e434f | [Download](https://data.virginia.gov/dataset/d2f4192e-a978-44ea-b6e9-c7fcdba2272f/resource/99a342e4-4e83-40f1-9b51-887a5c7e434f/download/cohort_statistics-6.csv) | HTTP 200 |
| 2022 | Cohort Graduation and Dropout Report 2022 | 49810d27-f16d-42f1-9577-21b173713204 | [Download](https://data.virginia.gov/dataset/92e9facf-079a-4af9-6020db3b527e/resource/49810d27-f16d-42f1-9577-21b173713204/download/cohort_statistics-5.csv) | HTTP 200 |
| 2023 | Cohort Graduation and Dropout Report 2023 | ccf203ad-862d-4522-8e9c-ab50de579ce7 | [Download](https://data.virginia.gov/dataset/554735ce-fd3e-4077-af7f-868c32e51edf/resource/ccf203ad-862d-4522-8e9c-ab50de579ce7/download/cohort_statistics-4.csv) | HTTP 200 |

All URLs verified working (2026-01-07).

### Schema Structure

**Two eras identified:**

| Version | Years | Columns | Level Field | State Aggregates |
|---------|-------|---------|-------------|------------------|
| v1 | 2019-2022 | 23 | NO | NO (calculate from schools) |
| v2 | 2023+ | 24 | YES | YES (provided in file) |

**Schema change detection:**
```r
detect_grad_era <- function(raw_data) {
  if ("Level" %in% names(raw_data)) {
    return("v2")  # 2023+
  } else {
    return("v1")  # 2019-2022
  }
}
```

### Column Names (v2 - 2023+)

1. Cohort Year
2. Level (NEW in v2 - "School" or "State")
3. Division Number
4. Division Name
5. School Number
6. School Name
7. Type of Graduation Rate
8. Rate Type
9. Graduation Rate
10. Students in Cohort
11. Total Graduates
12-19. Diploma Types (Advanced Studies, IB, Standard, Other, Applied Studies, GED, ISAEP, Certificate)
20. Completion Rate
21. Dropout Rate
22. Dropouts
23. Still Enrolled
24. Long-Term Absence

**v1 (2019-2022):** Same as v2 but NO "Level" column (all columns shifted left by 1).

### Data Format Details

- **File format:** CSV (comma-separated)
- **Encoding:** UTF-8
- **Row count:** ~330-340 schools per year
- **2023 includes:** 1 state-level record + school records
- **2019-2022:** School records only (calculate state totals manually)

### Value Formatting

**Percentages:** `"   83.71%"` (leading spaces + % suffix)
- Strip spaces, remove %, convert to numeric, divide by 100

**Integers:** `"           178"` (leading spaces)
- Strip spaces, convert to integer

**State-level numbers (2023 only):** `"98,927"` (commas)
- Remove commas before converting

**Suppressed values:** `"<"`
- Replace with NA
- Appears in diploma type counts and some rates

**Zero rates:** `"   .00%"`
- This is a real zero, NOT suppression
- Convert to `0.00`

### ID Formats

**Division Numbers:**
- Format: Integer, 1-3 digits (no leading zeros)
- Range: 1-218 (132 active divisions)
- Storage: Character (to match enrollment data)

**School Numbers:**
- Format: 4-digit numeric with leading zeros
- Examples: "0540", "0070", "0332"
- **CRITICAL:** Store as character to preserve leading zeros

### Verified State Totals

| Year | Cohort Size | Graduates | Graduation Rate | Source |
|------|-------------|-----------|-----------------|--------|
| 2019 | 98,241 | 89,991 | 91.60% | Calculated from schools |
| 2020 | 97,262 | 90,162 | 92.70% | Calculated from schools |
| 2021 | 97,096 | 90,325 | 93.03% | Calculated from schools |
| 2022 | 98,281 | 90,603 | 92.19% | Calculated from schools |
| 2023 | 98,927 | 90,944 | 91.93% | Provided in state record |

**Time series heuristics:**
- State graduation rate: 91-93% (expect values in this range)
- Cohort size: 97,000-99,000 students
- YoY rate change: typically < 1.5 percentage points
- School count: 330-340 high schools

**Red flags:**
- State rate < 88% or > 96% (data error likely)
- Cohort size < 90,000 or > 110,000
- School count < 300 or > 400

### Fidelity Test Values (Verified)

**2023 state-level (from state record):**
```
Graduation Rate: 91.93%
Cohort Size: 98,927
Total Graduates: 90,944
Completion Rate: 92.93%
Dropout Rate: 5.38%
```

**2023 Arcadia High (Accomack County, Div 1, School 0540):**
```
Graduation Rate: 83.71%
Cohort Size: 178
Total Graduates: 149
Advanced Studies: 52
Standard Diploma: 96
Applied Studies: < (suppressed)
Completion Rate: 84.83%
Dropout Rate: 3.93%
Dropouts: 7
Still Enrolled: 4
```

**2019 calculated state total:**
```
Total Cohort: 98,241
Total Graduates: 89,991
Graduation Rate: 91.60%
```

### Required R Packages

**No new dependencies needed!** All required packages already in DESCRIPTION:
- `readr` - Parse CSV files
- `httr` - Download files
- `stringr` - String manipulation (remove %, commas, spaces)
- `dplyr` - Data manipulation
- `tidyr` - Pivot to long format
- `tibble` - tibble data frames

### Implementation Status

**Stage 1 (Research):** COMPLETE
- [x] All 5 URLs verified working
- [x] Sample files downloaded (3+ years examined)
- [x] Schema changes documented (v1 vs v2)
- [x] Column names mapped
- [x] Data structure analyzed
- [x] Suppression markers identified
- [x] State totals verified
- [x] Fidelity test values documented

**Stage 2 (TDD):** READY TO START
- [ ] Create `tests/testthat/test-graduation-live.R` (8 LIVE pipeline tests)
- [ ] Create `tests/testthat/test-graduation-fidelity.R` (3+ fidelity tests)
- [ ] Run tests (should fail - no implementation yet)

**Stage 3 (Implementation):** PENDING
- [ ] Create `R/get_raw_graduation.R` (download + parse CSV)
- [ ] Create `R/process_graduation.R` (era detection + column standardization)
- [ ] Create `R/tidy_graduation.R` (pivot to long format)
- [ ] Create `R/fetch_graduation.R` (user-facing main function)
- [ ] Create `R/fetch_graduation_multi.R` (multi-year wrapper)
- [ ] Run `devtools::check()` - fix until 0 errors, 0 warnings

**Stage 4 (Documentation):** PENDING
- [ ] Update README.md with graduation rate availability
- [ ] Add roxygen2 documentation to all functions
- [ ] Update pkgdown reference (if needed)
- [ ] Create vignette example (if package has vignettes)

### CKAN API Access

**Discover all graduation datasets:**
```r
api_url <- "https://data.virginia.gov/api/3/action/package_search"
params <- list(q = "cohort graduation dropout", rows = 50, sort = "metadata_modified desc")
response <- httr::GET(api_url, query = params)
data <- httr::content(response, as = "parsed")
datasets <- data$result$results
```

**Hardcoded URLs (recommended for stability):**
```r
get_graduation_url <- function(end_year) {
  urls <- list(
    "2019" = "https://data.virginia.gov/dataset/89179d05-0009-4a08-a8bd-4f31ee47ed4a/resource/209a1d06-8d24-47e5-9bea-d64f6e7b6406/download/cohort_statistics-8.csv",
    "2020" = "https://data.virginia.gov/dataset/c13dbcbb-92fd-4335-9f41-9d6f41b084eb/resource/19b88afb-e16d-450d-80fe-9dd11fa87f0d/download/cohort_statistics-7.csv",
    "2021" = "https://data.virginia.gov/dataset/d2f4192e-a978-44ea-b6e9-c7fcdba2272f/resource/99a342e4-4e83-40f1-9b51-887a5c7e434f/download/cohort_statistics-6.csv",
    "2022" = "https://data.virginia.gov/dataset/92e9facf-079a-4af9-6020db3b527e/resource/49810d27-f16d-42f1-9577-21b173713204/download/cohort_statistics-5.csv",
    "2023" = "https://data.virginia.gov/dataset/554735ce-fd3e-4077-af7f-868c32e51edf/resource/ccf203ad-862d-4522-8e9c-ab50de579ce7/download/cohort_statistics-4.csv"
  )
  if (!as.character(end_year) %in% names(urls)) {
    stop("Graduation data not available for ", end_year)
  }
  urls[[as.character(end_year)]]
}
```

### Additional Data on Portal

The Virginia Open Data Portal also provides:
- Graduates by Diploma Type (2019-2023)
- Graduates by Continuing Education Plans (2019-2023)
- Graduate and Completer Totals by School (2019-2023)
- Historical Reports of Graduates (1996-2006)

Could be added later as separate functions if needed.

### Complexity Assessment

**Tier: LOW (Tier 1)**
- Direct CSV download
- Stable CKAN API
- Only 2 schema eras
- No authentication required
- No new dependencies

**Estimated implementation time:** 3-4 hours (per graduation rate implementation plan)

**Ready for Phase 1 proof of concept implementation.**

---

## README and Vignette Code Matching (REQUIRED)

**CRITICAL RULE (as of 2026-01-08):** ALL code blocks in the README MUST match code in a vignette EXACTLY (1:1 correspondence).

### Why This Matters

The Idaho fix revealed critical bugs when README code didn't match vignettes:
- Wrong district names (lowercase vs ALL CAPS)
- Text claims that contradicted actual data  
- Missing data output in examples

### README Story Structure (REQUIRED)

Every story/section in the README MUST follow this structure:

1. **Claim**: A factual statement about the data
2. **Explication**: Brief explanation of why this matters
3. **Code**: R code that fetches and analyzes the data (MUST exist in a vignette)
4. **Code Output**: Data table/print statement showing actual values (REQUIRED)
5. **Visualization**: Chart from vignette (auto-generated from pkgdown)

### Enforcement

The `state-deploy` skill verifies this before deployment:
- Extracts all README code blocks
- Searches vignettes for EXACT matches
- Fails deployment if code not found in vignettes
- Randomly audits packages for claim accuracy

### What This Prevents

- ❌ Wrong district/entity names (case sensitivity, typos)
- ❌ Text claims that contradict data
- ❌ Broken code that fails silently
- ❌ Missing data output
- ✅ Verified, accurate, reproducible examples

### Example

```markdown
### 1. State enrollment grew 28% since 2002

State added 68,000 students from 2002 to 2026, bucking national trends.

```r
library(arschooldata)
library(dplyr)

enr <- fetch_enr_multi(2002:2026)

enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  filter(end_year %in% c(2002, 2026)) %>%
  mutate(change = n_students - lag(n_students),
         pct_change = round((n_students / lag(n_students) - 1) * 100, 1))
# Prints: 2002=XXX, 2026=YYY, change=ZZZ, pct=PP.P%
```

![Chart](https://almartin82.github.io/arschooldata/articles/...)
```
