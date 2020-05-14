#' Make SSCA table
#'
#' @param ssca_obj SSCA object
#'
#' @export
#'
#' @import dplyr
#'
#'
ssca_table <- function(ssca_obj){
  W_star <- ssca_obj$w_list$W_star
  z_list <- ssca_obj$z_list


  w_star_short <- round(W_star,2)[which(round(W_star,2) != 0)]

  table_dat <- tibble(
    maa = suppressWarnings(fct_recode(names(w_star_short), !!!countries)),
    paino = w_star_short
  )
  ### Normalized RMSE Loss ###
  rmse <- round(comp.rmse(z_list, W_star), 2)


  gt::gt(table_dat) %>%
    gt::tab_header(
      subtitle = paste0("Normalisoitu keskineliövirheen neliöjuuri (RMSE): ", rmse),
      title = "Synteettisen kontrollin estimointi")

}
