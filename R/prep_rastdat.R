#' Preprocess Raster Data for Change Detection Analysis
#'
#' Aligns and preprocesses two rasters (before/after) for change detection.
#' Handles resampling, NA values, and ensures spatial compatibility.
#'
#' @param before_raster RasterLayer representing the "before" state.
#' @param after_raster RasterLayer representing the "after" state.
#' @param target_resolution Numeric vector of length 2 (xres, yres) for output
#'                          resolution. If NULL, keeps resolution of
#'                          `before_raster`.
#' @param resample_method Character. Resampling method: "bilinear" (for continuous
#'                        data) or "ngb" (nearest neighbor for categorical data).
#' @param na_action How to handle NA values: "replace" (fill with `na_value`) or
#'                  "ignore" (leave as NA).
#' @param na_value Value to replace NAs with if `na_action = "replace"`.
#' @param verbose Logical. Print processing messages? (Default: TRUE).
#'
#' @return A list with two elements:
#'   - `before`: Processed "before" raster.
#'   - `after`: Processed "after" raster.
#' @export
#'
#' @examples
#' \dontrun{
#' library(raster)
#' r1 <- raster(nrows = 100, ncols = 100, vals = runif(10000))
#' r2 <- raster(nrows = 150, ncols = 150, vals = runif(22500))
#' processed <- prep_rastdat(r1, r2, target_resolution = c(0.5, 0.5))
#' }
prep_rastdat <- function(before_raster, after_raster,
                         target_resolution = NULL,
                         resample_method = "bilinear",
                         na_action = "replace",
                         na_value = 0,
                         verbose = TRUE) {

  # Validate inputs
  if (!inherits(before_raster, "RasterLayer") ||
      !inherits(after_raster, "RasterLayer")) {
    stop("Both inputs must be RasterLayer objects")
  }

  if (!na_action %in% c("replace", "ignore")) {
    stop('na_action must be either "replace" or "ignore"')
  }

  if (!is.null(target_resolution)) {
    if (!is.numeric(target_resolution) || length(target_resolution) != 2) {
      stop("target_resolution must be a numeric vector of length 2 (xres, yres)")
    }
  }

  if (verbose) message("1/4 - Checking CRS...")
  if (is.na(raster::crs(before_raster)) || is.na(raster::crs(after_raster))) {
    warning("One or both rasters lack CRS definition")
  }

  # CRS alignment
  if (!raster::compareCRS(before_raster, after_raster)) {
    if (verbose) message("Reprojecting after_raster to match before_raster CRS...")
    after_raster <- raster::projectRaster(after_raster, before_raster)
  }

  if (verbose) message("2/4 - Aligning extents...")
  common_extent <- raster::intersect(raster::extent(before_raster),
                                     raster::extent(after_raster))
  if (is.null(common_extent)) {
    stop("Input rasters don't overlap spatially")
  }
  before_raster <- raster::crop(before_raster, common_extent)
  after_raster <- raster::crop(after_raster, common_extent)

  # Resolution handling
  if (!is.null(target_resolution)) {
    if (verbose) message("3/4 - Resampling to target resolution...")
    template <- raster::raster(common_extent, res = target_resolution)
    before_raster <- raster::resample(before_raster, template,
                                      method = resample_method)
    after_raster <- raster::resample(after_raster, template,
                                     method = resample_method)
  }

  # NA handling
  if (na_action == "replace") {
    if (verbose) message("4/4 - Handling NA values...")
    before_raster[is.na(before_raster)] <- na_value
    after_raster[is.na(after_raster)] <- na_value
  }

  return(list(before = before_raster, after = after_raster))
}
