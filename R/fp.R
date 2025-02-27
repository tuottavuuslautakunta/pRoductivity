
#' @export
#' @examples
#' fp(cp = c(NA, 94, 96, 98), pp = c(NA, NA, 95, 98), time = c(1994:1997), year = 1997)
#'
#'

# fp <- function (cp, pp, time, year)
# {
#
#   non_na_ind <- cumsum(!is.na(cp)) != 0
#   y_len <- length(cp)
#   if (all(!non_na_ind)) return(rep_len(NA, y_len))
#   cp <- cp[non_na_ind]
#   pp <- pp[non_na_ind]
#   ind <- pp/lag(cp)
#   ind[1] <- 1
#   ind <- cumprod(ind)
#   if (is.na(ind[time == year]) | length(ind[time == year]) == 0) return(rep_len(NA, y_len))
#   time <- time[non_na_ind]
#   y0 <- cp[time == year] * ind/ind[time == year]
#   y <- rep_len(NA, length(non_na_ind))
#   y[non_na_ind] <- y0
#   y
# }


fp <- function (cp, pp, time, year)
{

  # non_na_ind <- cumsum(!is.na(cp)) != 0
  # y_len <- length(cp)
  # if (all(!non_na_ind)) return(rep_len(NA, y_len))
  # cp <- cp[non_na_ind]
  # pp <- pp[non_na_ind]
  ind <- pp/lag(cp)
  start <- max(which(is.na(ind[1:which(time == year)])))
  ind[1:start] <- 1
  ind <- cumprod(ind)
  # if (is.na(ind[time == year]) | length(ind[time == year]) == 0) return(rep_len(NA, y_len))
  # time <- time[non_na_ind]
  y <- cp[time == year] * ind/ind[time == year]
  # y <- rep_len(NA, length(non_na_ind))
  # y[non_na_ind] <- y0
  if (start > 1) y[1:(start - 1)] <- NA
  y
}
