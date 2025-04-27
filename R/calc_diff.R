#' Calculate Pixel-wise Differences
#'
#' @param before_raster,after_raster RasterLayer objects
#' @param method "simple", "absolute", or "normalized"
#' @return Difference raster
#' @importFrom raster overlay
#' @export
calc_diff <- function(before_raster, after_raster,
                                 method = c("simple", "absolute", "normalized")) {
  method <- match.arg(method)

  # Calculate the difference based on the chosen method
  diff_raster <- switch(method,
                        simple = after_raster - before_raster,
                        absolute = abs(after_raster - before_raster),
                        normalized = normalize_difference(before_raster, after_raster)
  )

  return(diff_raster)  # Return the result
}

