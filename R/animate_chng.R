#' Create Change Detection Animation with Pixel Change Graph
#'
#' This function creates an animated GIF that shows change detection over a series
#' of images, along with a dynamic graph of the percentage of changed pixels over time.
#'
#' @param image_dir Directory containing the time-series images (JPEG or PNG).
#' @param output_file Output filename for the animation (e.g., `"change_animation.gif"`).
#' @param frame_rate Speed of animation playback in frames per second.
#' @param threshold Change detection threshold (between 0 and 1) for pixel difference.
#' @param col_pal Color palette used for visualizing detected changes (default: reversed `heat.colors(10)`).
#' @param plot_title Title displayed on the change detection plots.
#' @param width Width of the output animation frames in pixels.
#' @param height Height of the output animation frames in pixels.
#' @param change_mask Optional raster layer used to limit change detection to a specific region.
#'
#' @return Saves an animated GIF showing detected changes and corresponding pixel change graph.
#' @importFrom raster brick mask plotRGB
#' @importFrom magick image_read image_write image_append image_join image_animate
#' @importFrom ggplot2 ggplot geom_line geom_point ylim labs theme_minimal last_plot
#' @importFrom utils tail
#' @importFrom grDevices dev.off png heat.colors
#' @export
animate_chng <- function(image_dir, output_file = "change_animation.gif",
                         frame_rate = 2, threshold = 0.1,
                         col_pal = rev(heat.colors(10)),
                         plot_title = "Change Detection Tracking",
                         width = 800, height = 600,
                         change_mask = NULL) {

  # Get image file paths
  img_files <- list.files(image_dir, pattern = "\\.(jpg|png)$", full.names = TRUE)
  if (length(img_files) < 2) {
    stop("Need at least 2 images for comparison.")
  }

  # Initialize vectors
  changed_pixels <- numeric(length(img_files) - 1)
  dates <- character(length(img_files) - 1)

  # Create a temporary directory for intermediate frames
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Process each image pair
  for (i in seq_len(length(img_files) - 1)) {
    img1 <- brick(img_files[i])
    img2 <- brick(img_files[i + 1])

    if (!is.null(change_mask)) {
      img1 <- mask(img1, change_mask)
      img2 <- mask(img2, change_mask)
    }

    diff_img <- abs(img1 - img2)
    diff_matrix <- as.matrix(diff_img)

    changed_pixels[i] <- sum(diff_matrix > threshold, na.rm = TRUE) / length(na.omit(diff_matrix))
    dates[i] <- sub(".*?(\\d{8}).*", "\\1", basename(img_files[i]))

    # Save the difference visualization
    diff_plot_path <- file.path(temp_dir, sprintf("diff_plot_%03d.png", i))
    png(diff_plot_path, width = width, height = height / 2)
    plotRGB(diff_img * 5, stretch = "lin",
            main = paste(plot_title, dates[i]),
            col = col_pal, margins = TRUE)
    dev.off()

    # Save the graph visualization
    graph_plot_path <- file.path(temp_dir, sprintf("graph_plot_%03d.png", i))
    png(graph_plot_path, width = width, height = height / 2)
    ggplot(data.frame(Time = seq_along(changed_pixels[1:i]),
                      ChangedPixels = changed_pixels[1:i])) +
      geom_line(color = "red") +
      geom_point() +
      ylim(0, 1) +
      labs(title = "Percentage of Changed Pixels Over Time",
           x = "Time Step", y = "Changed Pixels (%)") +
      theme_minimal()
    print(last_plot())
    dev.off()

    # Read both images and stack vertically
    img_diff <- image_read(diff_plot_path)
    img_graph <- image_read(graph_plot_path)

    frame <- image_append(c(img_diff, img_graph), stack = TRUE)

    image_write(frame, path = file.path(temp_dir, sprintf("frame_%03d.png", i)))
  }

  # Create final animation
  frame_files <- list.files(temp_dir, pattern = "frame_\\d+\\.png$", full.names = TRUE)
  frames <- image_read(frame_files)

  animation <- image_animate(frames, fps = frame_rate, dispose = "previous")

  image_write(animation, path = output_file)

  # Clean up temporary files
  unlink(temp_dir, recursive = TRUE)

  message("Animation created: ", output_file)
}
