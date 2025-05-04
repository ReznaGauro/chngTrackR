#' Plot Change Detection Results
#'
#' This function visualizes the results of change detection by plotting the before and after rasters,
#' along with the binary change mask. Different plot types are available, including side-by-side comparison,
#' overlay of the change mask on the before raster, and difference visualization.
#'
#' @param before A `SpatRaster` object representing the raster data before change detection.
#' @param after A `SpatRaster` object representing the raster data after change detection (optional; required for certain plot types).
#' @param change_mask A `SpatRaster` object representing the binary change mask (optional; required for certain plot types).
#' @param type A character string specifying the plot type. Options include:
#'   \itemize{
#'     \item "sidebyside" : Display before and after rasters side by side.
#'     \item "overlay" : Overlay the change mask on top of the before raster.
#'     \item "difference" : Show the difference between the before and after rasters.
#'   }
#' @param fast A logical value indicating whether to use terra's native plotting (faster for large rasters) or ggplot2 (default).
#'             If `TRUE`, native terra plotting is used for better performance.
#'
#' @return A `ggplot` object if `fast` is `FALSE`, or a `terra` plot if `fast` is `TRUE`.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # Load example rasters
#' before <- rast(matrix(runif(100), nrow = 10))
#' after <- rast(matrix(runif(100), nrow = 10))
#' change_mask <- rast(matrix(sample(0:1, 100, replace = TRUE), nrow = 10))
#'
#' # Visualize change detection results
#' vis_chng(before, after, change_mask, type = "overlay", fast = TRUE)
#' vis_chng(before, after, change_mask, type = "sidebyside", fast = FALSE)
#' }
#'
#' @importFrom viridis viridis
#' @importFrom ggplot2 aes
#' @importFrom terra plot
#' @importFrom ggplot2 ggplot geom_raster scale_fill_viridis_c
#' @export
vis_chng <- function(before, after = NULL, change_mask = NULL,
                     type = c("sidebyside", "overlay", "difference"),
                     fast = FALSE) {
  type <- match.arg(type)

  if (fast) {
    terra::plot(before, main = "Before")
    if (type == "sidebyside") {
      terra::plot(after, main = "After")
    } else if (type == "overlay") {
      terra::plot(change_mask, add = TRUE, col = c(NA, "red"), alpha = 0.5)
    } else if (type == "difference") {
      diff <- after - before
      terra::plot(diff, col = viridis::viridis(100))
    }
  } else {
    df <- prepare_plot_data(before, after, change_mask, type)
    ggplot(df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      scale_fill_viridis_c()
  }
}


