# Internal Helper Functions for changTrackR Package
#
# These functions support the main package functions but are not exported for user access.
# Use @keywords internal to prevent public documentation.

#' Validate Raster Inputs
#'
#' Internal function to validate inputs for raster processing functions.
#'
#' @param r1 First raster (RasterLayer)
#' @param r2 Second raster (RasterLayer)
#' @param na_action NA handling method ("replace" or "ignore")
#' @param target_res Target resolution vector (numeric length 2 or NULL)
#'
#' @return NULL. Throws errors for invalid inputs.
#' @keywords internal
validate_raster_inputs <- function(r1, r2, na_action, target_res) {
  if (!inherits(r1, "RasterLayer") || !inherits(r2, "RasterLayer")) {
    stop("Both inputs must be RasterLayer objects")
  }
  if (!na_action %in% c("replace", "ignore")) {
    stop('na_action must be either "replace" or "ignore"')
  }
  if (!is.null(target_res) && (!is.numeric(target_res) || length(target_res) != 2)) {
    stop("target_resolution must be numeric length 2 (xres, yres) or NULL")
  }
}

#' Align Coordinate Reference Systems
#'
#' Ensures two rasters share the same CRS by reprojecting if needed.
#'
#' @param ref_raster Reference raster (target CRS)
#' @param target_raster Raster to align (will be reprojected)
#' @param verbose Logical. Print progress messages?
#'
#' @return RasterLayer with matching CRS
#' @keywords internal
align_crs <- function(ref_raster, target_raster, verbose = TRUE) {
  if (!raster::compareCRS(ref_raster, target_raster)) {
    if (verbose) message("Reprojecting to match reference CRS...")
    target_raster <- raster::projectRaster(target_raster, ref_raster)
  }
  target_raster
}

#' Align Raster Extents
#'
#' Crops rasters to their common spatial extent.
#'
#' @param r1 First raster (RasterLayer)
#' @param r2 Second raster (RasterLayer)
#' @param verbose Logical. Print progress messages?
#'
#' @return List with two cropped rasters (named 'before' and 'after')
#' @keywords internal
align_rasters <- function(r1, r2, verbose = TRUE) {
  if (verbose) message("Aligning raster extents...")
  common_extent <- raster::intersect(raster::extent(r1), raster::extent(r2))
  if (is.null(common_extent)) stop("Input rasters don't overlap")

  list(
    before = raster::crop(r1, common_extent),
    after = raster::crop(r2, common_extent)
  )
}

#' Handle Raster Resolution
#'
#' Resamples rasters to target resolution if specified.
#'
#' @param r1 First raster (RasterLayer)
#' @param r2 Second raster (RasterLayer)
#' @param target_res Target resolution (numeric length 2 or NULL)
#' @param method Resampling method ("bilinear" or "ngb")
#' @param verbose Logical. Print progress messages?
#'
#' @return List with two resampled rasters (named 'before' and 'after')
#' @keywords internal
handle_resolution <- function(r1, r2, target_res, method = "bilinear", verbose = TRUE) {
  if (!is.null(target_res)) {
    if (verbose) message("Resampling to target resolution...")
    template <- raster::raster(raster::extent(r1), res = target_res)
    list(
      before = raster::resample(r1, template, method = method),
      after = raster::resample(r2, template, method = method)
    )
  } else {
    list(before = r1, after = r2)
  }
}

#' Handle NA Values in Rasters
#'
#' Replaces NA values according to specified method.
#'
#' @param r1 First raster (RasterLayer)
#' @param r2 Second raster (RasterLayer)
#' @param action NA handling method ("replace" or "ignore")
#' @param value Replacement value when action = "replace"
#'
#' @return List with two processed rasters (named 'before' and 'after')
#' @keywords internal
handle_na_values <- function(r1, r2, action = "replace", value = 0) {
  if (action == "replace") {
    r1[is.na(r1)] <- value
    r2[is.na(r2)] <- value
  }
  list(before = r1, after = r2)
}
