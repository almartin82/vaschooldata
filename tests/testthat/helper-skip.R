# Helper functions for skipping tests when VDOE data is unavailable

#' Skip test if VDOE data cannot be fetched
#'
#' VDOE sometimes requires CAPTCHA or has temporary outages.
#' This function actually tries to fetch data to verify access.
skip_if_vdoe_unavailable <- function() {
  tryCatch({
    # Try to fetch a single year - if CAPTCHA is active, this will fail
    result <- vaschooldata::fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
    if (is.null(result) || nrow(result) == 0) {
      testthat::skip("VDOE data fetch returned empty result")
    }
  }, error = function(e) {
    testthat::skip(paste("VDOE data unavailable:", conditionMessage(e)))
  })
}
