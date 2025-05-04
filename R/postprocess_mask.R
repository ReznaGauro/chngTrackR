#' Post-process Change Mask
#'
#' This function applies morphological operations such as closing, opening, or clumping to a binary change mask.
#' These operations can be useful for cleaning up the change mask and removing small artifacts or grouping adjacent pixels.
#'
#' @param mask A `SpatRaster` object representing a binary change mask (1 for change, 0 for no change).
#' @param operation A character string specifying the morphological operation to be applied.
#'                  Options are "closing", "opening", or "clump".
#' @param kernel_size An integer defining the size of the matrix (e.g., 3 for a 3x3 matrix) to be used in the morphological operations.
#'                    Default is 3.
#'
#' @return A `SpatRaster` object with the processed change mask.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # Create a sample binary mask
#' mask <- rast(matrix(c(0, 1, 0, 1, 0, 0, 1, 0, 0), nrow = 3))
#'
#' # Apply closing operation
#' processed_mask <- postprocess_mask(mask, operation = "closing", kernel_size = 3)
#'
#' # Apply clumping operation
#' clumped_mask <- postprocess_mask(mask, operation = "clump")
#' }
#'
#' @importFrom terra focal patches
#' @importFrom terra focal
#' @export
postprocess_mask <- function(mask,
                             operation = c("closing", "opening", "clump"),
                             kernel_size = 3) {

  operation <- match.arg(operation)

  # Validate binary mask
  if (!all(terra::values(mask) %in% c(0, 1, NA))) {
    stop("Mask must be binary (0/1)")
  }

  if (operation %in% c("closing", "opening")) {
    if (kernel_size %% 2 == 0) {
      stop("Kernel size must be odd")
    }
    kernel <- matrix(1, kernel_size, kernel_size)
  }

  processed <- switch(
    operation,
    closing = terra::focal(mask, kernel, fun = max) |> terra::focal(kernel, fun = min),
    opening = terra::focal(mask, kernel, fun = min) |> terra::focal(kernel, fun = max),
    clump = terra::patches(mask, directions = 8)
  )

  return(processed)
}
