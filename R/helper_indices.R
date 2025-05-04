#' Compute Spectral Indices
#'
#' Calculate common remote sensing indices
#'
#' @keywords internal
#' @noRd
#' @importFrom terra subset
compute_index <- function(raster, index) {
  index <- toupper(index)

  switch(index,
         NDVI = {
           # (NIR - RED) / (NIR + RED)
           b4 <- terra::subset(raster, 4)
           b3 <- terra::subset(raster, 3)
           (b4 - b3) / (b4 + b3)
         },
         NDBI = {
           # (SWIR - NIR) / (SWIR + NIR)
           b5 <- terra::subset(raster, 5)
           b4 <- terra::subset(raster, 4)
           (b5 - b4) / (b5 + b4)
         },
         NDWI = {
           # (GREEN - NIR) / (GREEN + NIR)
           b2 <- terra::subset(raster, 2)
           b4 <- terra::subset(raster, 4)
           (b2 - b4) / (b2 + b4)
         },
         stop("Unsupported index. Available: NDVI, NDBI, NDWI")
  )
}

#' Normalized Difference Calculator
#'
#' Generic normalized difference (A - B)/(A + B)
#'
#' @keywords internal
#' @noRd
normalize_diff <- function(a, b) {
  # Add small epsilon to prevent division by zero
  eps <- .Machine$double.eps
  (a - b) / (a + b + eps)
}
