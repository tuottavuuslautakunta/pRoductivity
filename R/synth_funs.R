#' Loop SSCA estimations
#'
#' @param value_var A string vector of names of the variable to estimate over.
#' @param data A data.frame
#' @param ... Further arguments passed to \link{\code[ssca_est]}.
#'
#' @export
#' @import dplyr


synth_est_loop <- function(.value_var, .data, i_time){
  .data %>%
    nest(data_nace = -nace0) %>%
    mutate(
      model = purrr::map(
        .x = data_nace,
        .f = function(x) sc_mean(x, value_var = .value_var, i_time = i_time)
        ),
      model = set_names(model, nace0)) %>%
    select(-data_nace)
}


#' Synthetic with mean
#'
#' @examples
#' sc_mean(filter(synth_dat10, nace0 == "private"), value_var = "lp_ind", 2008)

sc_mean <- function(.data, value_var, i_time){
  .data %>%
    synthetic_control(
      outcome = !!sym(value_var),
      unit = geo,
      time = time,
      i_unit = "FI",
      i_time = i_time,
      generate_placebos = T
    ) %>%
    generate_predictor(
      time_window = NULL,
      mean_lp_ind = mean(lp_ind)
    ) %>%
    # generate_predictor(time_window = 2005,
    #                    lp_ind_2005 = lp_ind) %>%
    # generate_predictor(time_window = 1995,
    #                    lp_ind_1995 = lp_ind) %>%
    generate_weights(
      optimization_window = NULL
    ) %>%
    generate_control()
}


sc_95_05 <- function(dat){
  dat %>%
    synthetic_control(
      outcome = lp_ind,
      unit = geo,
      time = time,
      i_unit = "FI",
      i_time = 2008,
      generate_placebos = T
    ) %>%
    # generate_predictor(
    #   time_window = NULL,
    #   mean_lp_ind = mean(lp_ind)
    # ) %>%
    generate_predictor(time_window = 2005,
                       lp_ind_2005 = lp_ind) %>%
    generate_predictor(time_window = 1995,
                       lp_ind_1995 = lp_ind) %>%
    generate_weights(
      optimization_window = NULL
    ) %>%
    generate_control()
}


sc_15 <- function(dat){
  dat %>%
    synthetic_control(
      outcome = lp_ind,
      unit = geo,
      time = time,
      i_unit = "FI",
      i_time = 2015,
      generate_placebos = T
    ) %>%
    # generate_predictor(
    #   time_window = NULL,
    #   mean_lp_ind = mean(lp_ind)
    # ) %>%
    generate_predictor(time_window = 2005,
                       lp_ind_2005 = lp_ind) %>%
    generate_predictor(time_window = 1995,
                       lp_ind_1995 = lp_ind) %>%
    generate_predictor(time_window = 2015,
                       lp_ind_2015 = lp_ind) %>%
    generate_weights(
      optimization_window = NULL
    ) %>%
    generate_control()
}
