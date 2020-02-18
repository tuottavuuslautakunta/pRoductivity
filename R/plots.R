#' Productivity index plot
#'
#' @param data a data to plot
#'
#' @import ggplot
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

#' Productivity index plot with highlighting
#'
#' @param data a data to plot
#'
#' @export
#' @import dplyr, ggplot
#'
prod_ind_plot_high <- function(data){
  mutate(data, high_names = fct_other(geo_name, keep = c(high_country, high_countries), other_level = "muut"),
         high_names = fct_relevel(high_names, c(high_country, high_countries), after = 0),
         geo_name = fct_relevel(geo_name, c(high_countries, high_country), after = Inf)) %>%
    ggplot(aes(time, lp_ind, group = geo_name, colour = high_names, size = geo_name == high_country)) +
    geom_line(alpha = 0.9) +
    scale_size_manual(values = c(1.5,2.5), guide = "none") +
    scale_colour_manual(values = tula_pal(7)) +
    guides(colour = guide_legend()) +
    the_title_blank(c("x", "l")) +
    labs(y = glue("Indeksi, {base_year} = 100"))
}
