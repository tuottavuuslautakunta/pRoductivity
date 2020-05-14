#' Productivity index plot
#'
#' @param data a data to plot
#'
#' @import ggplot2
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
#' @param data A data to plot.
#' @param high_country A country highlighted by line size.
#' @param high_countries Countries highlighted by line colour.
#'
#' @export
#' @import dplyr ggplot2
#'
prod_ind_plot_high <- function(data, high_country, high_countries){
  mutate(data,
         high_names = fct_other(geo_name, keep = c(high_country, high_countries), other_level = "muut"),
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


#' Plot triptych
#'
#' One large level plot and two ssca plots
#'
#' @param ssca_obj a ssca_obj.
#' @param high_country A country highlighted by line size.
#' @param high_countries Countries highlighted by line colour.
#'
#' @export
#' @import dplyr ggplot2 patchwork

trip_plot <- function(ssca_obj, high_country, high_countries){

  p1 <-
    ssca_obj$z_list$theCall$longdata %>%
    filter(time >= 2000) %>%
    prod_ind_plot_high(high_country, high_countries) +
    ggtitle("a. Suomi ja vertailumaat") +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

  ssca_pdata <- ssca_plot_data(ssca_obj) %>%
    filter(year >= 2000)

  p2 <- ssca_plot_level(ssca_pdata) + guides(colour = "none")
  p3 <- ssca_plot_diff(ssca_pdata)


  p1 / ((p2 | p3) + plot_layout(guides = "collect")) + plot_layout(heights = c(3, 2))
}
