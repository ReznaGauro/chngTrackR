#' Visualize Change Detection Results
#'
#' This function visualizes the change detection results between two raster images,
#' either as a side-by-side comparison, an overlay of changes, or a difference map.
#'
#' @param before_raster A RasterLayer object representing the original (before) raster.
#' @param after_raster A RasterLayer object representing the modified (after) raster.
#' @param change_mask A RasterLayer object representing the binary change mask.
#' @param type A character string specifying the type of visualization. Possible values are:
#'   - "sidebyside": Displays the before and after rasters side by side.
#'   - "overlay": Displays the change mask overlayed on the original raster.
#'   - "difference": Displays the difference between the before and after rasters.
#'
#' @return A ggplot object containing the visualization of the change detection results.
#' @importFrom ggplot2 ggplot aes aes_string geom_raster scale_fill_viridis_c
#' @importFrom raster as.data.frame
#' @export
vis_chng <- function(before_raster, after_raster = NULL,
                     change_mask = NULL,
                     type = c("sidebyside", "overlay", "difference")) {

  # Match the provided type to ensure it is valid
  type <- match.arg(type)

  # Prepare the data for plotting using the helper function
  plot_data <- prepare_plot_data(before_raster, after_raster, change_mask, type)

  # Create the plot based on the type of visualization
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_raster(ggplot2::aes_string(x = "x", y = "y", fill = "value")) +
    ggplot2::scale_fill_viridis_c()  # Using a color scale for better visualization

  return(p)  # Return the ggplot object
}
