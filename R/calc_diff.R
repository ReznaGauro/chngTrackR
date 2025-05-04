#' Calculate Differences Between Rasters
#'
#' Computes the difference between two `SpatRaster` objects using various methods,
#' including simple subtraction, absolute difference, normalized difference, or index-based difference (e.g., NDVI).
#'
#' @param before A `SpatRaster` object representing the "before" image.
#' @param after A `SpatRaster` object representing the "after" image.
#' @param method Character. Method to calculate differences. One of `"simple"`, `"absolute"`, `"normalized"`, or `"index"`. Default is `"simple"`.
#' @param index Character. Required if `method = "index"`. The name of the index to compute (e.g., `"NDVI"`, `"NDBI"`).
#'
#' @return A `SpatRaster` object representing the difference map.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' before <- rast(system.file("extdata/before.tif", package = "changTrackR"))
#' after <- rast(system.file("extdata/after.tif", package = "changTrackR"))
#'
#' # Simple difference
#' diff_simple <- calc_diff(before, after, method = "simple")
#' plot(diff_simple)
#'
#' # Absolute difference
#' diff_abs <- calc_diff(before, after, method = "absolute")
#' plot(diff_abs)
#'
#' # Normalized difference
#' diff_norm <- calc_diff(before, after, method = "normalized")
#' plot(diff_norm)
#'
#' # Index-based difference (e.g., NDVI)
#' ndvi_diff <- calc_diff(before, after, method = "index", index = "NDVI")
#' plot(ndvi_diff)
#' }
#'
#' @import terra
#' @importFrom stats sd
#' @importFrom terra head
#' @importFrom terra nlyr
#' @importFrom methods is
#' @importFrom magick image_read
#' @importFrom grDevices dev.off
#' @export
calc_diff <- function(before, after,
                      method = c("simple", "absolute", "normalized", "index"),
                      index = NULL) {
  method <- match.arg(method)

  # Validate raster compatibility
  validate_rasters(before, after)  # Ensures same CRS, resolution, etc.

  if (method == "index") {
    if (is.null(index)) stop("Index required for method='index'")
    index <- toupper(index)
    if (!index %in% c("NDVI", "NDBI", "NDWI")) {
      stop("Unsupported index. Use: NDVI, NDBI, NDWI")
    }
    before <- compute_index(before, index)
    after <- compute_index(after, index)
  }

  # Ensure matching number of layers after index calculation
  if (terra::nlyr(before) != terra::nlyr(after)) {
    stop("Rasters must have same number of layers after index calculation")
  }

  diff_rast <- switch(
    method,
    simple = after - before,
    absolute = abs(after - before),
    normalized = normalize_diff(before, after),
    index = after - before
  )

  return(diff_rast)
}
