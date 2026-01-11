# Virginia Assessment Data Expansion Research

## Package Status

**Current Scope:** Enrollment data only

---

## Assessment Data Availability

### Data Source
**Virginia Department of Education (VDOE)**
- **SOL Test Pass Rates & Other Results:** https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results
- **Virginia SOL Assessment Program:** https://www.doe.virginia.gov/teaching-learning-assessment/student-assessment/virginia-sol-assessment-program

### Assessment System: Standards of Learning (SOL)

**Current Assessments:**
- **Grades:** 3-8 and End-of-Course
- **Subjects:**
  - Reading
  - Mathematics
  - Science
  - History/Social Science
  - Writing
- **EOC Assessments:** Algebra I, Geometry, Algebra II, Earth Science, Biology, Chemistry, Virginia/US History, World History I & II

**Historical Timeline:**
- **1990s:** SOL assessments established as part of Virginia's Standards of Accreditation
- **Early 2000s:** SOL tests became graduation requirements
- **2010s:** Continued SOL administration with periodic standard settings
- **2020s:** SOL assessments continue, added Growth Assessments in some grades

---

## Data Access Analysis

### VDOE Data Reports Page

**URL:** https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results

**What It Provides:**
- SOL pass rates by school, district, and state
- Historical data files (Excel/CSV format typically)
- Disaggregated data by student groups
- Multiple years of results

**File Formats:**
- Typically Excel spreadsheets (.xlsx)
- May include CSV files
- Organized by year and subject

**Years Available:**
- VDOE maintains extensive historical data
- Likely 2000s-present (exact range requires verification)
- Annual updates

---

## Implementation Complexity

### Complexity Level: **MEDIUM**

**Advantages:**
1. **Centralized Data Source:** Single VDOE data reports page
2. **Standardized Format:** Excel files with consistent structure
3. **Long Time Series:** 20+ years of SOL data
4. **Official Source:** Direct from VDOE (no federal aggregation)

**Challenges:**
1. **File Organization:** Multiple files per year (by subject)
2. **Schema Evolution:** May have column changes over 20+ years
3. **EOC vs Grade-Level:** Different structures for different tests
4. **Pass Rates vs Raw Scores:** Primarily pass percentages, may need raw scores

---

## Implementation Recommendations

### Phase 1: File Structure Investigation

**Tasks:**
1. Download 3 sample files from different eras (e.g., 2010, 2015, 2023)
2. Document column names and structures
3. Identify changes over time
4. Map to standard schema

**Time Estimate:** 2-3 hours

### Phase 2: Implementation

**Functions to Create:**
1. `get_raw_sol(year, subject, level)` - Download SOL data
2. `process_sol(raw_data)` - Process and standardize
3. `fetch_sol(year, subject, tidy = TRUE)` - User-facing function

**Estimated Effort:** 12-15 hours

---

## Data Quality Considerations

### Known Issues

1. **Pandemic Impact:**
   - 2019-20: SOL assessments waived/modified
   - 2020-21: Participation may have varied
   - Data quality issues in pandemic years

2. **Standard Setting Changes:**
   - VDOE periodically resets cut scores
   - Not directly comparable across all years
   - Document standard setting years

3. **Small School Suppression:**
   - Small schools may have suppressed data
   - FERPA compliance requirements

---

## Next Steps

1. **Investigate VDOE data files** - Download and analyze sample files
2. **Document schema evolution** - Identify column changes over time
3. **Test URL patterns** - Verify predictable download URLs
4. **Implement data functions** - Build fetch/process/tidy pipeline

---

## References

- [VDOE SOL Test Pass Rates & Other Results](https://www.doe.virginia.gov/data-policy-funding/data-reports/statistics-reports/sol-test-pass-rates-other-results)
- [Virginia SOL Assessment Program](https://www.doe.virginia.gov/teaching-learning-assessment/student-assessment/virginia-sol-assessment-program)
- [SOL Test Scoring & Performance Reports](https://www.doe.virginia.gov/teaching-learning-assessment/student-assessment/virginia-sol-assessment-program/sol-test-scoring-performance-reports)

---

**Last Updated:** 2025-01-11
**Research Status:** Complete - Requires file investigation
**Recommended Next Phase:** Download and analyze VDOE SOL data files (2-3 hours)
