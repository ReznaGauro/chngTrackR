#' Visualize Change Detection Results
#'
#' This function visualizes the results of change detection by plotting the before and after rasters,
#' along with an optional binary change mask. Different plot types are available, including side-by-side comparison,
#' overlay of the change mask on the before raster, and difference visualization.
#'
#' @param before A `SpatRaster` object representing the raster data before change detection.
#' @param after A `SpatRaster` object representing the raster data after change detection. Required for `"sidebyside"` and `"difference"` plot types.
#' @param change_mask A `SpatRaster` object representing the binary change mask (1 = change, 0 = no change). Required for `"overlay"` plot type.
#' @param type A character string specifying the plot type. Options include:
#'   \itemize{
#'     \item `"sidebyside"`: Display before and after rasters side by side.
#'     \item `"overlay"`: Overlay the change mask on top of the before raster.
#'     \item `"difference"`: Show the difference between the before and after rasters.
#'   }
#' @param fast A logical value. If `TRUE`, uses `terra::plot()` for faster visualization (no `ggplot`). If `FALSE` (default), uses `ggplot2` for more customizable output.
#'
#' @return A `ggplot` object if `fast = FALSE`, or a `terra` plot if `fast = TRUE`.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' before <- rast(matrix(runif(100), nrow = 10))
#' after <- rast(matrix(runif(100), nrow = 10))
#' change_mask <- rast(matrix(sample(0:1, 100, replace = TRUE), nrow = 10))
#'
#' vis_chng(before, after, change_mask, type = "overlay", fast = TRUE)
#' vis_chng(before, after, change_mask, type = "sidebyside", fast = FALSE)
#' }
#'
#' @importFrom terra plot
#' @importFrom ggplot2 ggplot aes geom_raster coord_fixed facet_wrap scale_fill_manual scale_fill_viridis_c
#' @importFrom viridis viridis
#' @importFrom ggnewscale new_scale_fill
#' @importFrom ggnewscale new_scale_fill
#' @importFrom viridis viridis
#' @importFrom dplyr filter
#' @keywords visualization change-detection raster
#' @export
vis_chng <- function(before, after = NULL, change_mask = NULL,
                     type = c("sidebyside", "overlay", "difference"),
                     fast = FALSE) {
  type <- match.arg(type)

  if(fast) {
    # ... (terra native plotting)
  } else {
    df <- prepare_plot_data(before, after, change_mask, type)

    base_plot <- ggplot(df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      coord_fixed()

    if(type == "sidebyside") {
      base_plot +
        facet_wrap(~layer, ncol = 2) +
        scale_fill_manual(values = class_colors)

    } else if(type == "overlay") {
      base_plot +
        scale_fill_manual(
          name = "Land Cover Class",
          values = class_colors,
          labels = class_labels,
          na.value = NA
        ) +
        new_scale_fill() +
        geom_raster(
          data = filter(df, layer == "Change Mask"),
          aes(fill = factor(value)),
          alpha = 0.6
        ) +
        scale_fill_manual(
          name = "Change Status",
          values = c("0" = NA, "1" = "red"),
          labels = c("No Change", "Change"),
          na.value = NA
        )

    } else if(type == "difference") {
      base_plot +
        scale_fill_viridis_c(
          option = "inferno",
          na.value = NA,
          name = "Change Magnitude"
        )
    }
  }
}

