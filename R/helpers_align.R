#' Validate Input Raster Objects
#'
#' Checks whether both inputs are valid `SpatRaster` objects from the `terra` package.
#'
#' @param r1 First raster object to validate.
#' @param r2 Second raster object to validate.
#'
#' @return Invisible `NULL` if both inputs are valid. Throws an error otherwise.
#' @keywords internal
#' @noRd
validate_rasters <- function(r1, r2) {
  if (!inherits(r1, "SpatRaster") || !inherits(r2, "SpatRaster")) {
    stop("Both inputs must be terra SpatRaster objects")
  }
}


#' Align Coordinate Reference Systems
#'
#' Reprojects the target raster to match the coordinate reference system (CRS) of a reference raster, if necessary.
#'
#' @param ref A `SpatRaster` object used as the CRS reference.
#' @param target A `SpatRaster` object to be reprojected.
#' @param verbose Logical. If `TRUE`, a message will be printed when reprojection occurs.
#'
#' @return A `SpatRaster` object reprojected to the CRS of `ref` if needed.
#' @keywords internal
#' @noRd
#' @importFrom terra crs project
align_crs <- function(ref, target, verbose = TRUE) {
  if (terra::crs(ref) != terra::crs(target)) {
    if (verbose) message("Reprojecting to match reference CRS...")
    target <- terra::project(target, terra::crs(ref))
  }
  return(target)
}


#' Align Extents of Two Rasters
#'
#' Crops both input rasters to a shared extent defined by their intersection.
#'
#' @param before A `SpatRaster` object representing the "before" raster.
#' @param after A `SpatRaster` object representing the "after" raster.
#' @param verbose Logical. If `TRUE`, prints a message when alignment is performed.
#'
#' @return A list containing the cropped `before` and `after` rasters.
#' @keywords internal
#' @noRd
#' @importFrom terra ext intersect crop
align_extents <- function(before, after, verbose = TRUE) {
  common_ext <- terra::intersect(terra::ext(before), terra::ext(after))
  if (verbose) message("Cropping rasters to common extent...")
  list(
    before = terra::crop(before, common_ext),
    after = terra::crop(after, common_ext)
  )
}


#' Handle Resolution Alignment Between Rasters
#'
#' Ensures both input rasters have the same resolution and extent by resampling them to a shared template.
#'
#' @param before A `SpatRaster` representing the "before" image.
#' @param after A `SpatRaster` representing the "after" image.
#' @param target_res Optional numeric vector of length 2 indicating the desired resolution (`c(xres, yres)`). If `NULL`, the resolution of `before` is used.
#' @param resample_method Character. The resampling method to use (`"bilinear"`, `"near"`, etc.).
#' @param verbose Logical. If `TRUE`, messages will be printed during processing.
#'
#' @return A list containing the resampled `before` and `after` rasters.
#' @keywords internal
#' @noRd
#' @importFrom terra res resample rast ext crs compareGeom
handle_resolution <- function(before, after, target_res = NULL,
                              resample_method = "bilinear", verbose = TRUE) {

  # Set target resolution
  if (is.null(target_res)) {
    target_res <- terra::res(before)
    if (verbose) message("\nUsing 'before' raster resolution: ",
                         paste(round(target_res, 2), collapse = " x "))
  }

  # Create template raster with target specs
  template <- terra::rast(
    extent = terra::ext(before),
    resolution = target_res,
    crs = terra::crs(before)
  )

  # Resample if needed
  if (!terra::compareGeom(before, template)) {
    if (verbose) message("\nResampling 'before' raster...")
    before <- terra::resample(before, template, method = resample_method)
  }

  if (!terra::compareGeom(after, template)) {
    if (verbose) message("\nResampling 'after' raster...")
    after <- terra::resample(after, template, method = resample_method)
  }

  list(before = before, after = after)
}


#' Handle Missing Values in Raster Data
#'
#' Replaces `NA` values in each raster layer with the layer's mean. If a layer is entirely `NA`, it is filled with `0`.
#'
#' @param before A `SpatRaster` representing the "before" image.
#' @param after A `SpatRaster` representing the "after" image.
#' @param verbose Logical. If `TRUE`, messages will be printed during processing.
#'
#' @return A list of `SpatRaster` objects with `NA` values replaced: `before` and `after`.
#' @keywords internal
#' @noRd
#' @importFrom terra nlyr global classify
handle_na <- function(before, after, verbose = TRUE) {
  if (verbose) message("\nHandling missing values...")

  # Define NA replacement function with safety checks
  replace_na <- function(r) {
    for (i in 1:terra::nlyr(r)) {
      layer_mean <- terra::global(r[[i]], "mean", na.rm = TRUE)[[1]]

      # Handle fully NA layers
      if (!is.finite(layer_mean)) {
        layer_mean <- 0
        if (verbose) message("Layer ", names(r)[i], " is entirely NA - using 0 for replacement")
      }

      # Create classification matrix
      rcl <- matrix(c(NA, layer_mean), ncol = 2)

      # Apply replacement
      r[[i]] <- terra::classify(r[[i]], rcl, right = FALSE)
    }
    return(r)
  }

  return(list(
    before = replace_na(before),
    after = replace_na(after)
  ))
}
