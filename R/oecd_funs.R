#' Create an OECD Filter String
#'
#' This function takes a list of filtering arguments and constructs a filter string for an OECD API call.
#'
#' @param x A list of strings representing different filter criteria for the OECD API.
#'
#' @return A single string with the filter criteria concatenated using '+' and '.' as separators.
#'
#' @export
#'
#' @examples
#' make_oecd_filter(list("A", "sna_geo", "", "", "sna6a_transact", "", "sna_activity", "", "", "sna_measures", "", ""))
make_oecd_filter <- function(x) {
  x |>
    purrr::map(~paste(.x, collapse = "+")) |>
    paste(collapse = ".")
}
