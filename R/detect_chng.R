#' Detect Changes via Thresholding
#'
#' Detects changes in a difference raster using various thresholding methods. The result is a binary raster
#' indicating areas of change (1) and no change (0).
#'
#' @param diff_raster A `SpatRaster` object representing the difference raster (e.g., from the `calc_diff()` function).
#' @param method Character. The thresholding method to use. One of `"otsu"`, `"kmeans"`, `"sd"` (mean + 2σ), or `"percentile"`.
#' @param threshold Numeric. A user-defined threshold value. Required if `method = "fixed"`.
#'
#' @return A binary `SpatRaster` object, where pixels with change are labeled as `1` and pixels with no change are labeled as `0`.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' before <- rast(system.file("extdata/before.tif", package = "changTrackR"))
#' after <- rast(system.file("extdata/after.tif", package = "changTrackR"))
#'
#' # Compute difference
#' diff_raster <- calc_diff(before, after, method = "simple")
#'
#' # Detect changes using Otsu thresholding
#' change_mask_otsu <- detect_chng(diff_raster, method = "otsu")
#' plot(change_mask_otsu)
#'
#' # Detect changes using k-means thresholding
#' change_mask_kmeans <- detect_chng(diff_raster, method = "kmeans")
#' plot(change_mask_kmeans)
#'
#' # Detect changes using standard deviation thresholding (mean + 2σ)
#' change_mask_sd <- detect_chng(diff_raster, method = "sd")
#' plot(change_mask_sd)
#'
#' # Detect changes using a user-defined percentile threshold
#' change_mask_percentile <- detect_chng(diff_raster, method = "percentile")
#' plot(change_mask_percentile)
#' }
#'
#' @importFrom terra classify
#' @importFrom stats ecdf kmeans
#' @export
detect_change <- function(diff_raster,
                          method = c("otsu", "kmeans", "sd", "percentile", "fixed"),
                          threshold = NULL) {

  method <- match.arg(method)

  # Validate input raster
  if (!inherits(diff_raster, "SpatRaster")) {
    stop("Input must be a SpatRaster")
  }

  if (!is.null(threshold)) {
    method <- "fixed"
    warning("Using custom threshold; method argument ignored")
  }

  if (method != "fixed" && is.null(threshold)) {
    threshold <- calc_thresh(diff_raster, method)
  }

  # Correct classification matrix (from, to, becomes)
  rcl <- matrix(
    c(-Inf, threshold, 0,
      threshold, Inf, 1),
    ncol = 3, byrow = TRUE
  )

  change_mask <- terra::classify(diff_raster, rcl)

  return(change_mask)
}
