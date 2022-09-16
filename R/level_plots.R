#' Plot lp levels
#'
#' @param data
#' @param  geos
#' @param  nace
#'
#'
#' @export
#'
plot_lp_level <- function(data, geos, nace, y = lp_level05){

  size_values <-
   c(2.5,1.5,1.5,1.5,1.5,1.5,0.5,0.5)[1:length(geos)]

  colour_values <- tula_pal(length(geos))

  data |>
  filter(geo %in% geos,
         nace0 == nace) |>
    droplevels() |>
    mutate(geo = fct_relevel(geo, geo_level)) |>
    mutate(geo_name = fct_recode(geo, !!!countries)) |>
    ggplot(aes(time, {{y}}, colour = geo_name, size = geo_name)) +
    geom_line() +
    scale_size_manual(name = "legend", values = size_values)+
    scale_y_log10(breaks = seq(0, 200, 10)) +
    scale_colour_manual(name = "legend", values = colour_values) +
    guides(colour = guide_legend(nrow = 2)) +
    the_legend_bot() +
    the_title_blank("xl") +
    labs(y = "Indeksi, Yhdysvallat 2005 = 100, logaritminen asteikko")
}

#' @describeIn plot_lp_level
#' @export
plot_lp_level_rel <- function(data, geos, nace){

  size_values <-
    c(2.5,1.5,1.5,1.5,1.5,1.5,0.5,0.5)[1:length(geos)]

  colour_values <- tula_pal(length(geos))

  data |>
    filter(geo %in% geos,
           nace0 == nace) |>
    droplevels() |>
    complete(time, nesting(geo, geo_name), nace0) |>
    group_by(time, nace0) |>
    mutate(lp_level05 = 100 * lp_level05 / lp_level05[geo == "US"]) |>
    ungroup() |>
    mutate(geo = fct_relevel(geo, geo_level)) |>
    mutate(geo_name = fct_recode(geo, !!!countries)) |>
    ggplot(aes(time, lp_level05, colour = geo_name, size = geo_name)) +
    geom_line() +
    scale_size_manual(name = "legend", values = size_values)+
    scale_y_continuous(breaks = seq(0, 200, 10)) +
    scale_colour_manual(name = "legend", values = colour_values) +
    guides(colour = guide_legend(nrow = 2)) +
    the_legend_bot() +
    the_title_blank("xl") +
    labs(y = "Indeksi, Yhdysvallat = 100")
}
