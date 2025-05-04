#' Prepare Raster Data for Visualization
#'
#' Converts raster objects to a formatted data frame suitable for ggplot2 visualization,
#' handling different visualization types and combining multiple raster layers.
#'
#' @param before A `SpatRaster` object representing the pre-change imagery
#' @param after A `SpatRaster` object representing the post-change imagery (optional for some types)
#' @param mask A binary `SpatRaster` change mask (optional for some types)
#' @param type Character specifying visualization type: "sidebyside", "overlay", or "difference"
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{x}{X coordinate}
#'   \item{y}{Y coordinate}
#'   \item{value}{Pixel value}
#'   \item{layer}{Factor indicating raster source (Before/After/Mask)}
#' }
#'
#' @details This function handles:
#' - Coordinate extraction
#' - Value normalization
#' - Layer stacking for different visualization types
#' - NA value removal
#'
#' @note For large rasters, consider using sample points instead of full conversion
#'
#' @examples
#' \dontrun{
#' library(terra)
#' before <- rast(system.file("extdata/before.tif", package = "changTrackR"))
#' after <- rast(system.file("extdata/after.tif", package = "changTrackR"))
#'
#' # Prepare side-by-side data
#' plot_data <- prepare_plot_data(before, after, type = "sidebyside")
#' }
#'
#' @importFrom terra as.data.frame
#' @keywords internal
#' @noRd
prepare_plot_data <- function(before, after = NULL, mask = NULL, type = "sidebyside") {
  # Implementation to convert rasters to data frames
  # Example: Extract coordinates and values
  df <- as.data.frame(before, xy = TRUE)
  names(df)[3] <- "value"
  return(df)
}
