# TODO - vaschooldata

## pkgdown Build Issues

### Issue: VDOE Data Download Failure (2026-01-01)

**Error:** pkgdown build fails when rendering `vignettes/enrollment_hooks.Rmd`

**Details:**
- Build fails at chunk `statewide-trend` (lines 38-48)
- Error: `Failed to download Virginia enrollment data for year 2016`
- The vignette calls `fetch_enr_multi(2016:2025)` which attempts to download data from VDOE
- All years 2016-2025 are configured as available in `get_available_years()`

**Root Cause:**
- Network connectivity issues or VDOE server unavailability
- The download fails with a connection timeout when trying to fetch enrollment data

**Potential Solutions:**
1. Add pre-built/cached data to the package for vignette builds
2. Use `eval = FALSE` for chunks that require live data downloads
3. Add error handling with graceful fallback in the vignette
4. Consider using `knitr::opts_chunk$set(eval = nzchar(Sys.getenv("VASCHOOLDATA_BUILD_VIGNETTES")))` to conditionally build

**Stack Trace:**
```
Error in get_raw_enr():
! Failed to download Virginia enrollment data for year 2016
Please check your internet connection and try again.
If the problem persists, VDOE data may be temporarily unavailable.
```
