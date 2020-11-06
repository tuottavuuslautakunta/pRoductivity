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
    scale_color_discrete(palette = tula_pal) +
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
prod_ind_plot_high <- function(data, plot_var, base_year, high_country, high_countries){
  data %>%
  rename(plot_var = plot_var) %>%
  mutate(high_names = fct_other(geo_name, keep = c(high_country, high_countries), other_level = "muut"),
         high_names = fct_relevel(high_names, c(high_country, high_countries), after = 0),
         high_size = fct_other(high_names, keep = c(high_country, "muut"), other_level = "muut high"),
         geo_name = fct_relevel(geo_name, c(high_countries, high_country), after = Inf)) %>%
    ggplot(aes(time, plot_var, group = geo_name, colour = high_names, size = high_size)) +
    # geom_line(alpha = 0.7) +
    geom_line() +
    scale_size_manual(values = c(2.5, 1, 1.5), guide = "none") +
    scale_colour_manual(values = c(tula_pal(length(c(high_country, high_countries))), "grey75")) +
    guides(colour = guide_legend()) +
    the_title_blank(c("x", "l")) +
    labs(y = glue("Indeksi, {base_year} = 100"))
}


#' Plot triptych
#'
#' One large level plot and two ssca plots
#'
#' @param ssca_obj a ssca_obj.
#' @param weight_data Data with weighted variables.
#' @param nace nace classification to use.
#' @param plot_var A variable to plot.
#' @param high_country A country highlighted by line size.
#' @param high_countries Countries highlighted by line colour.
#'
#' @export
#' @import dplyr ggplot2 patchwork

trip_plot <- function(ssca_obj, weight_data, nace, plot_var, base_year, high_country, high_countries){

  p1 <-
    ssca_obj[[plot_var]][["model"]][[nace]]$z_list$theCall$longdata %>%
    filter(time >= plot_start_year) %>%
    prod_ind_plot_high(plot_var, base_year, high_country, high_countries) +
    ggtitle("a. Suomi ja vertailumaat") +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))

  ssca_pdata <- ssca_plot_data(ssca_obj[[plot_var]][["model"]][[nace]]) %>%
    filter(year >= plot_start_year)


  p2 <- ssca_plot_diff_100(ssca_pdata, base_year)
  p3 <- rel_one_plot(weight_data, nace = nace,
                     title = "c. Suomi suhteessa euroalueeseen ja\nEurooppalaisiin maihin kauppapainoin")


  p1 / ((p2 | p3) ) + plot_layout(heights = c(3, 2))
}

#' Plot syntetic control figures
#'
#' two ssca plots
#'
#' @param ssca_obj A ssca_obj.
#' @param nace A nace classification to use.
#' @param plot_var A variable to plot.
#' @param high_country A country highlighted by line size.
#' @param high_countries Countries highlighted by line colour.
#'
#' @export
#' @import dplyr ggplot2 patchwork

synt_plot <- function(ssca_obj, nace, plot_var, base_year, high_country, high_countries){


  ssca_pdata <- ssca_plot_data(ssca_obj[[plot_var]][["model"]][[nace]]) %>%
    filter(year >= plot_start_year)

  p2 <- ssca_plot_level(ssca_pdata, base_year) + guides(colour = "none")
  p3 <- ssca_plot_diff(ssca_pdata, base_year)


 ((p2 | p3) + plot_layout(guides = "collect")) + plot_layout(heights = c(3, 2))
}

#' Weighted plot of variables
#'
#' @param .data A data to plot.
#' @param nace A industry classification to plot
#'
#' @export
#' @import dplyr ggplot2
#'
rel_one_plot <- function(.data, nace,
                         w_plot_var = c("Työn tuottavuus" = "lp_ind"),
                         title = "Suomi suhteessa euroalueeseen ja\nEurooppalaisiin maihin kauppapainoin"){

  rel_vars <- c("euroalue-12" = "ea",
                "kauppakumppanit" = "weight")

  .data %>%
    filter(nace0 == nace, geo == "FI",
           time >= plot_start_year) %>%
    pivot_longer(contains("_rel_"), names_to = c("vars", "rel"), names_sep = "_rel_",
                 values_to = "values") %>%
    filter(vars %in% w_plot_var) %>%
    mutate(vars = fct_recode(vars, !!!w_plot_var),
           rel = fct_recode(rel, !!!rel_vars)) %>%
    ggplot(aes(time, values, colour = rel)) +
    # geom_line(alpha = 0.7) +
    geom_line() +
    scale_colour_discrete(palette = tula_pal) +
    the_title_blank(c("x", "l")) +
    labs(y = glue("Indeksi, {base_year} = 100")) +
    labs(title = title)
}

#' Weighted plot of three variables
#'
#' @param .data A data to plot.
#' @param nace A industry classification to plot
#'
#' @export
#' @import dplyr
#'
rel_plot <- function(.data, nace,
                        w_plot_vars = c("Työn tuottavuus" = "lp_ind_rel_ecfin37",
                                         "Arvonlisäys" = "va_ind_rel_ecfin37",
                                         "Työtunnit" = "h_ind_rel_ecfin37")){

  .data %>%
    filter(nace0 == nace, geo == "FI",
           time >= plot_start_year) %>%
    pivot_longer(all_of(w_plot_vars), names_to = "vars", values_to = "values") %>%
    mutate(vars = fct_recode(vars, !!!w_plot_vars)) %>%
    prod_ind_plot_high("values", base_year, high_country, high_countries) +
    facet_wrap(~ vars) +
    guides(colour = "none") +
    labs(title = "Suomi suhteessa Eurooppalaisiin maihin kauppapainoin")
}

rel_plot2 <- function(.data, nace,
                     w_plot_vars = c("Työn tuottavuus" = "lp_ind",
                                     "Arvonlisäys" = "va_ind",
                                     "Työtunnit" = "h_ind")){
  rel_vars <- c("euroalue-12" = "ea",
                "kauppakumppanit" = "ecfin37")

  .data %>%
    filter(nace0 == nace, geo == "FI",
           time >= plot_start_year) %>%
    pivot_longer(contains("_rel_"), names_to = c("vars", "rel"), names_sep = "_rel_",
                 values_to = "values") %>%
    mutate(vars = fct_recode(vars, !!!w_plot_vars),
           rel = fct_recode(rel, !!!rel_vars)) %>%
    ggplot(aes(time, values, colour = rel)) +
    geom_line() +
    facet_wrap(~ vars) +
    scale_colour_manual(values = tula_pal(2)) +
    the_title_blank(c("x", "l")) +
    labs(y = glue("Indeksi, {base_year} = 100")) +
    labs(title = "Suomi suhteessa euroalueeseen ja\nEurooppalaisiin maihin kauppapainoin")
}
