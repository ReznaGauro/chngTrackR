# Analysis Helpers for changTrackR
#
# Functions supporting change detection calculations.
# @keywords internal

#' Calculate normalized difference
#' @param r1,r2 RasterLayers
#' @return Normalized difference raster
normalize_difference <- function(r1, r2) {
  diff <- r2 - r1
  (diff - min(diff[], na.rm=TRUE)) /
    (max(diff[], na.rm=TRUE) - min(diff[], na.rm=TRUE))
}

#' Automatic threshold calculation
#' @param r RasterLayer
#' @param method Threshold method
#' @importFrom stats kmeans na.omit
calculate_threshold <- function(r, method = c("otsu", "kmeans")) {
  method <- match.arg(method)
  vals <- raster::values(r)

  switch(method,
         otsu = EBImage::otsu(vals),
         kmeans = {
           km <- kmeans(na.omit(vals), centers = 2)
           max(km$centers)
         }
  )
}
