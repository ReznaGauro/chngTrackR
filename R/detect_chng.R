#' Detect Significant Changes Using Thresholding
#'
#' @param diff_raster RasterLayer of differences
#' @param threshold Numeric threshold value
#' @param method Threshold method ("fixed", "otsu", "kmeans")
#' @return Binary RasterLayer (1 = change, 0 = no change)
#' @importFrom EBImage otsu
#' @export
detect_change <- function(diff_raster, threshold = NULL,
                          method = c("fixed", "otsu", "kmeans")) {

  method <- match.arg(method)

  # Calculate threshold if not provided or method is not fixed
  if (method != "fixed" && is.null(threshold)) {
    threshold <- calculate_threshold(diff_raster, method)
  }

  # Create binary mask where change is considered if difference > threshold
  change_mask <- diff_raster > threshold

  # Replace NA values with 0 (no change)
  change_mask[is.na(change_mask)] <- 0

  # Convert to a binary raster (1 = change, 0 = no change)
  change_mask <- raster::calc(change_mask, fun = function(x) ifelse(x > 0, 1, 0))

  return(change_mask)
}
