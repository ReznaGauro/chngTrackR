#' Validate Change Detection Results
#'
#' This function compares the predicted change mask with the ground truth mask and calculates accuracy metrics
#' including overall accuracy and Kappa statistic using a confusion matrix.
#'
#' @param pred_mask A `SpatRaster` object representing the predicted changes (binary: 1 for change, 0 for no change).
#' @param truth_mask A `SpatRaster` object representing the ground truth mask (binary: 1 for actual change, 0 for no change).
#'
#' @return A list containing:
#'   \item{confusion_matrix}{The confusion matrix showing true positives, false positives, true negatives, and false negatives.}
#'   \item{overall_accuracy}{The overall accuracy of the prediction (proportion of correct predictions).}
#'   \item{kappa}{The Kappa statistic, measuring agreement between predicted and true labels, accounting for chance.}
#'
#' @examples
#' \dontrun{
#' library(terra)
#' # Create example predicted and truth masks
#' pred_mask <- rast(matrix(c(0, 1, 0, 1, 1, 0), nrow = 2))
#' truth_mask <- rast(matrix(c(0, 1, 0, 1, 0, 1), nrow = 2))
#'
#' # Validate the results
#' results <- validate_results(pred_mask, truth_mask)
#' print(results)
#' }
#'
#' @importFrom caret confusionMatrix
#' @importFrom terra values
#' @export
validate_results <- function(pred_mask, truth_mask) {
  # Remove NA values
  pred_vec <- terra::values(pred_mask) |> na.omit()
  truth_vec <- terra::values(truth_mask) |> na.omit()

  # Ensure binary values
  if (!all(pred_vec %in% c(0, 1)) || !all(truth_vec %in% c(0, 1))) {
    stop("Masks must be strictly binary (0/1)")
  }

  cm <- caret::confusionMatrix(
    data = factor(pred_vec, levels = c(0, 1)),
    reference = factor(truth_vec, levels = c(0, 1))
  )

  return(list(
    confusion_matrix = cm$table,
    overall_accuracy = cm$overall["Accuracy"],
    kappa = cm$overall["Kappa"]
  ))
}
