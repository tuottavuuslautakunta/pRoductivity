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
                     height = 13.5,
                     en = TRUE){
  plot1 <- plot +
    ggptt::the_title_blank(c("x", "t", "l")) +
    theme(plot.caption = element_blank())

  plot_slide <- plot +
    theme(text = element_text(size=15))

  plot_slide_half <- plot +
    ggptt::the_title_blank(c("x", "l")) +
    theme(text = element_text(size=11))

  ggplot2::ggsave(here::here("figures/pdf", paste0(filename, ".pdf")),
                  plot = plot1,
                  width = width, height = height, units = "cm")

  ggplot2::ggsave(here::here("figures/png", paste0(filename, ".png")),
                  plot = plot1,  width = width, height = height, units = "cm")
  ggplot2::ggsave(here::here("figures", paste0(filename, "_esitys.png")),
                  plot = plot_slide, width = 26, height = 12, units = "cm")
  ggplot2::ggsave(here::here("figures", paste0(filename, "_esitys_puolet.png")),
                  plot = plot_slide_half, width = 16, height = 12, units = "cm")

  if (en){
  ggplot2::ggsave(here::here("figures/pdf_en", paste0(filename, "_en.pdf")),
                  plot = ggptt::translate_plot(plot1, fig_translation),
                  width = width, height = height, units = "cm")}


}

#' @describeIn save_fig
#' @export
#'
#'

save_figs2 <- function(filename,
                      plot = last_plot(),
                      width = 13.5,
                      height = 13.5,
                      en = TRUE){
  plot1 <- plot +
    ggptt::the_title_blank(c("t", "l")) +
    theme(plot.caption = element_blank())

  plot_slide <- plot +
    theme(text = element_text(size=15))

  plot_slide_half <- plot +
    ggptt::the_title_blank(c("l")) +
    theme(text = element_text(size=11))

  ggplot2::ggsave(here::here("figures/pdf", paste0(filename, ".pdf")),
                  plot = plot1,
                  width = width, height = height, units = "cm")

  ggplot2::ggsave(here::here("figures/png", paste0(filename, ".png")),
                  plot = plot1,  width = width, height = height, units = "cm")
  ggplot2::ggsave(here::here("figures", paste0(filename, "_esitys.png")),
                  plot = plot_slide, width = 26, height = 12, units = "cm")
  ggplot2::ggsave(here::here("figures", paste0(filename, "_esitys_puolet.png")),
                  plot = plot_slide_half, width = 16, height = 12, units = "cm")

  if (en){
    ggplot2::ggsave(here::here("figures/pdf_en", paste0(filename, "_en.pdf")),
                    plot = ggptt::translate_plot(plot1, fig_translation),
                    width = width, height = height, units = "cm")}


}


#' Save data from triplot to csv-files
save_trip_plot_data <- function(filename,
                                plot = last_plot()){

  plot <- ggplot2::ggplot_build(plot)

  dat1 <- plot$plot[[1]]$data
  dat2 <- plot$plot[[2]][[1]]$data
  dat3 <- plot$plot[[2]][[2]]$data

  write.csv2(dat1, here::here("figures", paste0(filename, "_data1.csv")), row.names = FALSE)
  write.csv2(dat2, here::here("figures", paste0(filename, "_data2.csv")), row.names = FALSE)
  write.csv2(dat3, here::here("figures", paste0(filename, "_data3.csv")), row.names = FALSE)

}
