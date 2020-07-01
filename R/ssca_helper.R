#' Loop SSCA estimations
#'
#' @param value_var A string vector of names of the variable to estimate over.
#' @param data A data.frame
#' @param ... Further arguments passed to \link{\code[ssca_est]}.
#'
#' @export
#' @import dplyr


ssca_est_loop <- function(.value_var, .data, ...){
  .data %>%
    nest(data_nace = -nace0) %>%
    mutate(
      model = purrr::map(
        .x =data_nace,
        .f = function(x) ssca_est(x, value_var = .value_var, ...)
        ),
      model = set_names(model, nace0)) %>%
    select(-data_nace)
}


