#' Preprocess Raster Data for Change Detection
#'
#' @param before_raster, after_raster RasterLayer objects
#' @param after_raster A RasterLayer or SpatRaster representing the "after" time period.
#' @param target_resolution Numeric vector (xres, yres) or NULL
#' @param resample_method "bilinear" or "ngb"
#' @param na_action "replace" or "ignore"
#' @param na_value Replacement value for NAs
#' @param verbose Print progress messages?
#' @return List of processed rasters
#' @export
#' @importFrom raster crop extent intersect projectRaster resample
prep_rastdat <- function(before_raster, after_raster,
                         target_resolution = NULL,
                         resample_method = "bilinear",
                         na_action = "replace",
                         na_value = 0,
                         verbose = TRUE) {

  # Validate inputs using helper
  validate_raster_inputs(before_raster, after_raster, na_action, target_resolution)

  # CRS alignment using helper
  after_raster <- align_crs(before_raster, after_raster, verbose)

  # Spatial alignment using helper
  aligned <- align_rasters(before_raster, after_raster, verbose)

  # Resolution handling using helper
  processed <- handle_resolution(aligned$before, aligned$after,
                                 target_resolution, resample_method, verbose)

  # Handle NA values (kept simple in main function)
  processed <- handle_na_values(processed$before, processed$after, na_action, na_value)

  # Return processed rasters
  return(processed)
}
