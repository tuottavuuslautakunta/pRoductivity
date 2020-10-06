#' Save figures for report
#'
#' Uses ggsave.
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to last plot displayed.
#' @param width Plot size
#' @param height Plot size
#'
#' @export

save_figs <- function(filename,
                     plot = last_plot(),
                     width = 13.5,
                     height = 13.5){
  plot <- plot
  ggplot2::ggsave(here::here("figures", paste0(filename, ".pdf")), width = width, height = height, units = "cm")
  ggplot2::ggsave(here::here("figures", paste0(filename, ".png")), width = width, height = height, units = "cm")
}
