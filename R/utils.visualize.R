#' Prepare Raster Data for Plotting
#'
#' This helper function prepares raster data for visualization by converting raster objects
#' into a tidy data frame that can be plotted using `ggplot2`. It handles different types
#' of visualizations such as side-by-side comparisons, overlays, and differences.
#'
#' @param before_raster A RasterLayer object representing the original (before) raster.
#' @param after_raster A RasterLayer object representing the modified (after) raster.
#' @param change_mask A RasterLayer object representing the binary change mask.
#' @param type A character string specifying the type of visualization. Possible values are:
#'   - "sidebyside": Converts the before and after rasters into separate columns for comparison.
#'   - "overlay": Merges the before raster with the change mask to show detected changes.
#'   - "difference": Calculates the difference between the before and after rasters.
#'
#' @return A data frame with columns: `x` (longitude), `y` (latitude), and `value` (raster value).
#' @keywords internal
#' @importFrom raster as.data.frame
prepare_plot_data <- function(before_raster, after_raster = NULL,
                              change_mask = NULL, type = c("sidebyside", "overlay", "difference")) {
  # Match the provided type to ensure it is valid
  type <- match.arg(type)

  # Convert the 'before' raster to a data frame
  df_before <- raster::as.data.frame(before_raster, xy = TRUE)
  colnames(df_before)[3] <- "value_before"

  # Handle different types of visualizations
  if (type == "sidebyside") {
    # Convert the 'after' raster to a data frame if side-by-side view
    df_after <- raster::as.data.frame(after_raster, xy = TRUE)
    colnames(df_after)[3] <- "value_after"

    # Merge the before and after data frames by their x and y coordinates
    df <- merge(df_before, df_after, by = c("x", "y"))
    df$value <- df$value_before  # Default visualization for side-by-side (before raster)

  } else if (type == "overlay") {
    # Merge with the change mask for overlay visualization
    df_change <- raster::as.data.frame(change_mask, xy = TRUE)
    colnames(df_change)[3] <- "change_mask"

    # Merge before raster with change mask
    df <- merge(df_before, df_change, by = c("x", "y"))
    df$value <- df$change_mask  # Use the change mask for the overlay visualization

  } else if (type == "difference") {
    # For the difference view, calculate the difference between before and after rasters
    df_after <- raster::as.data.frame(after_raster, xy = TRUE)
    colnames(df_after)[3] <- "value_after"

    # Merge before and after data frames
    df <- merge(df_before, df_after, by = c("x", "y"))
    df$value <- df$value_after - df$value_before  # Calculate the difference

  }

  return(df)  # Return the processed data frame for ggplot2
}
