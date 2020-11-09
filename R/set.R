#' A colour palette for TULA
#'
#' @param n a number of colours
#'
#' @export

tula_pal <- function(n) c("#0072B2", "#fc7d0b", "#109618", "grey25", "#c85200", "grey75", "#1170aa", "#6495ED", "#5fa2ce", "#EE7600", "#8B3E2F", "#8968CD", "#f03b20")[1:n]
# tula_pal <- ggthemes::tableau_color_pal(palette = "Color Blind")


#' Set Competetive Board theme
#'
#' @param base_size A font base size.
#'
#' @export
#'

set_board_theme <- function(base_size = 11){
  if (!("prod_sets" %in% search())) {
    e <- new.env()
    attach(e, name = "prod_sets", warn.conflicts = FALSE)
  }
  attach(as.environment("prod_sets"), pos = 2L, name = "prod_sets")
  old_theme <- ggplot2::theme_set(theme)
  assign("old_theme", old_theme, pos = "prod_sets")

  pal <- tula_pal

  scale_colour_discrete <- function(...) discrete_scale("colour",
                                                        "tula", pal, ...)
  scale_fill_discrete <- function(...) discrete_scale("fill",
                                                      "tula", pal, ...)
  assign("scale_colour_discrete", scale_colour_discrete,
         pos = "prod_sets")
  assign("scale_color_discrete", scale_colour_discrete,
         pos = "prod_sets")
  assign("scale_fill_discrete", scale_fill_discrete,
         pos = "prod_sets")

 theme_set(theme_ptt(base_size, "sans"))
  ggplot2::theme_update(
    plot.title = element_text(size = base_size + 0.5),
    legend.text = ggplot2::element_text(size = base_size - 1.5),
    axis.title = ggplot2::element_text(colour = "grey20", size = base_size - 2),
    plot.subtitle = ggplot2::element_text(colour = "grey40"),
    plot.caption = ggplot2::element_text(size = base_size-4, face = "plain", colour = "grey40"),
    plot.margin = ggplot2::margin(t = 10, r = 8, b = 4, l = 5, unit = "pt"))
}
