#' Preprocess Raster Data for Change Detection
#'
#' This function prepares two `SpatRaster` objects (before and after images) for change detection analysis by aligning their coordinate reference systems, extents, and resolutions, applying optional radiometric correction, and handling missing values.
#'
#' @param before A `SpatRaster` object representing the "before" image.
#' @param after A `SpatRaster` object representing the "after" image.
#' @param target_res Optional. A numeric vector of length 2 specifying the target resolution in the form `c(xres, yres)`. If `NULL`, resolution of `before` raster is used.
#' @param resample_method Character. Resampling method to apply when aligning resolutions. Either `"bilinear"` or `"near"`. Default is `"bilinear"`.
#' @param radiometric_method Character. Radiometric correction method to apply. Options are `"histogram"`, `"scaling"`, or `"none"`. Default is `"histogram"`.
#' @param verbose Logical. If `TRUE`, prints progress messages during preprocessing. Default is `TRUE`.
#'
#' @return A named list containing preprocessed `SpatRaster` objects with elements:
#' \describe{
#'   \item{before}{The processed "before" raster.}
#'   \item{after}{The processed "after" raster.}
#' }
#'
#' @examples
#' \dontrun{
#' library(terra)
#' before <- rast(system.file("extdata/before.tif", package = "changTrackR"))
#' after <- rast(system.file("extdata/after.tif", package = "changTrackR"))
#'
#' processed <- prep_rastdat(before, after,
#'                           target_res = c(30, 30),
#'                           resample_method = "bilinear",
#'                           radiometric_method = "histogram",
#'                           verbose = TRUE)
#'
#' plot(processed$before)
#' plot(processed$after)
#' }
#' @import terra
#' @export
prep_rastdat <- function(before, after,
                         target_res = NULL,
                         resample_method = "bilinear",
                         radiometric_method = "histogram",
                         verbose = TRUE) {

  # Validate inputs
  validate_rasters(before, after)

  # Align CRS (returns modified 'after' raster)
  after <- align_crs(ref = before, target = after, verbose = verbose)

  # Align extents using CRS-aligned rasters
  aligned <- align_extents(before = before, after = after, verbose = verbose)

  # Resample resolution
  resampled <- handle_resolution(
    before = aligned$before,
    after = aligned$after,
    target_res = target_res,
    resample_method = resample_method,
    verbose = verbose
  )

  # Radiometric correction
  corrected <- radiometric_correction(
    before = resampled$before,
    after = resampled$after,
    method = radiometric_method,
    verbose = verbose
  )

  # Handle NA values
  final <- handle_na(
    before = corrected$before,
    after = corrected$after,
    verbose = verbose
  )

  return(final)
}
