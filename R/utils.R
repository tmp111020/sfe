#' Get the population standard deviation of a numeric vector
#'
#' The built in function "sd" only does sample standard deviation (with n-1 in
#' the denominator); computing population standard deviation is not difficult,
#' but it's easier to have a function for this
#' @usage sd_pop(x)
#' @param x numeric vector
#' @param na.rm logical indicating whether or not NA values should be removed
#' @return the population standard deviation
#' @keywords statistics
#' @export
#' @examples
#' sd_pop(c(3, 4, 5))
#'
sd_pop <- function(x, na.rm = TRUE) {
  if (na.rm == TRUE) {
    x <- na.omit(x)
  }
  sqrt(sum((x - mean(x))^2)/length(x))
}
