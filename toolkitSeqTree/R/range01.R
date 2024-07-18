#' Re-scale to range 0-1
#'
#' Re-scale a numeric vector such that the minimum number is 0 and maximum is 1.
#'
#' @param x Data frame describing tree structure
#'
#' @return Re-scaled numeric vector
#' @export
#'
range01 <- function(x = NA, ...) {
  (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}
