#' Productivity index plot
#'
#' @param data a data to plot
#'
#' @export
#'
prod_ind_plot <- function(data){
  ggplot(data = data, aes(time, lp_ind, colour = geo_name, size = geo_name == high_country)) +
    geom_line() +
    scale_size_manual(values = c(1.5,2.5), guide = "none") +
    guides(colour = guide_legend(reverse = TRUE)) +
    the_title_blank(c("x", "l")) +
    labs(y = glue("Indeksi, {base_year} = 100"))
}
