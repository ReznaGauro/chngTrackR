#' Preprocess Raster Data for Change Detection Analysis
#'
#' Aligns and preprocesses two rasters (before/after) for change detection.
#' Handles resampling, NA values, and ensures spatial compatibility.
#'
#' @return A list with processed "before" and "after" rasters
#' @importFrom raster crop extent intersect projectRaster resample crs
#' @importFrom terra rast crs res project ext
#' @export
prep_rastdat <- function(before_raster, after_raster,
                         target_resolution = NULL,
                         resample_method = "bilinear",
                         na_action = "replace",
                         na_value = 0,
                         verbose = TRUE) {

  # Validate inputs using helper
  validate_raster_inputs(before_raster, after_raster, na_action, target_resolution)

  # CRS alignment using helper
  if (verbose) message("1/4 - Checking CRS...")
  after_raster <- align_crs(before_raster, after_raster, verbose)

  # Spatial alignment using helper
  if (verbose) message("2/4 - Aligning extents...")
  aligned_rasters <- align_rasters(before_raster, after_raster, verbose)

  # Resolution handling using helper
  if (verbose) message("3/4 - Adjusting resolution...")
  processed_rasters <- handle_resolution(
    aligned_rasters$before,
    aligned_rasters$after,
    target_resolution,
    resample_method,
    verbose
  )

  # NA handling (kept simple in main function)
  if (verbose) message("4/4 - Handling NA values...")
  processed_rasters <- handle_na_values(
    processed_rasters$before,
    processed_rasters$after,
    na_action,
    na_value
  )

  return(processed_rasters)
}
