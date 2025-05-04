#' Calculate Change Detection Threshold
#'
#' Determine optimal threshold using multiple methods
#'
#' @keywords internal
#' @noRd
#' @importFrom terra values
#' @importFrom stats kmeans sd
#' @importFrom autothresholdr auto_thresh
calc_thresh <- function(r, method = "otsu") {
  vals <- terra::values(r) |> na.omit()

  if (length(vals) == 0) {
    stop("Cannot calculate threshold - raster contains only NA values")
  }

  switch(method,
         otsu = autothresholdr::auto_thresh(vals, "Otsu"),
         kmeans = {
           km <- stats::kmeans(vals, centers = 2, nstart = 10)
           max(km$centers)
         },
         sd = mean(vals) + 2 * stats::sd(vals),
         percentile = stats::quantile(vals, probs = 0.95, names = FALSE),
         fixed = {
           if (is.null(threshold)) stop("Threshold required for 'fixed' method")
           threshold
         },
         stop("Invalid method. Use: otsu, kmeans, sd, percentile, or fixed")
  )
}
