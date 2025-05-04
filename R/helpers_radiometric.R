#' Radiometric Correction Between Rasters
#'
#' Harmonizes radiometric properties between before/after rasters
#'
#' @keywords internal
#' @noRd
#' @importFrom terra values app global
#' @importFrom stats ecdf
radiometric_correction <- function(before, after, method = "histogram", verbose = TRUE) {
  if (method == "histogram") {
    if (verbose) message("Applying histogram matching...")

    # Safely handle NA values
    before_vals <- terra::values(before) |> na.omit()
    after_vals <- terra::values(after) |> na.omit()

    if (length(before_vals) == 0 || length(after_vals) == 0) {
      stop("Cannot perform histogram matching - raster contains only NA values")
    }

    ecdf_before <- stats::ecdf(before_vals)
    after <- terra::app(after, function(x) ecdf_before(x))

  } else if (method == "scaling") {
    if (verbose) message("Applying z-score normalization...")

    # Calculate global stats with NA handling
    after_mean <- terra::global(after, "mean", na.rm = TRUE)[[1]]
    after_sd <- terra::global(after, "sd", na.rm = TRUE)[[1]]

    # Prevent division by zero
    if (after_sd < .Machine$double.eps) {
      warning("Zero standard deviation detected in scaling - returning original")
      return(list(before = before, after = after))
    }

    after <- (after - after_mean) / after_sd

  } else if (method != "none") {
    stop("Supported methods: 'histogram', 'scaling', or 'none'")
  }

  list(before = before, after = after)
}
