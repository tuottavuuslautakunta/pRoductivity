#' Rebase (or base) index
#'
#'
#' @param x a numeric vector. An index to rebase
#' @param time a time variable in a Date format.
#' @param baseyear a year or vector of years.
#'
#' @export
rebase <- function(x, time, baseyear) {
  time_year <- if (lubridate::is.Date(time)) lubridate::year(time) else time
  y <- 100 * x / mean(x[time_year %in% baseyear])
  y
}
