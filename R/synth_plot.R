# Plot functions for ssca


#' Get plot data from synth_obj
#'
#' @param synth_obj a synth_obj.
#' @param plot_var A name of variable to plot.
#' @param nace A name of the nace0 classification to plot.
#'
#' @export
#' @import dplyr
#'
#' @examples
#'   synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private")

synth_plot_data <- function(synth_obj, plot_var = NULL, nace = NULL){
  if (".placebo" %in% names(synth_obj)){
    obj <- synth_obj
  } else {
    obj <- synth_obj[[plot_var]][["model"]][[nace]]
  }
  pdat <- tidysynth::grab_synthetic_control(obj) |>
    dplyr::rename(year = time_unit) |>
    tidyr::gather(vars, values, -year)

  pdat
}


#' Plot synth object
#'
#'
#' @param title
#' @param synth_obj
#' @param legend_arg
#'
#' @export
#' @import dplyr ggplot2 forcats ggptt
#'
#' @examples
#'   synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private") |>
#'     synth_plot("Testi", "indeksi, 100 = 2007")
#'
#'   synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private") |>
#'     synth_plot_level(2007, "Testi")
#'
#'   synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private") |>
#'     synth_plot_diff(2007, "Testi")


synth_plot <- function(pdat_synt,
                       title, y_lab,
                       legend_arg = c("Suomi", "Synteettinen kontrolli")){
  pdat <- pdat_synt %>%
    mutate(
      var_names = recode_factor(vars,
                                     real_y = legend_arg[1],
                                     synth_y = legend_arg[2]
                                     ))
  pdat %>%
    ggplot(aes(year, values, group = vars, colour = var_names, size = var_names)) +
    geom_line() +
    scale_colour_manual(values = c(tula_pal(2))) +
    scale_size_manual(values = c(2,2), guide = "none") +
    the_legend_bot() +
    the_title_blank(c("l", "x")) +
    labs(title = title, y = y_lab)
}


#' @describeIn synth_plot

synth_plot_level <- function(synth_pdat, base_year,
                            title = "a. Suomi ja synteettinen kontrolli",
                            ylab = paste0("Indeksi, ", base_year, " = 100"),
                            legend_arg = c("Suomi", "Synteettinen kontrolli")){
  pdat <- synth_pdat %>%
    mutate(
      vars = fct_relevel(vars, "synth_y", "real_y", after = Inf),
      # var_names = fct_other(vars, keep = c("Z1", "real_full", "syn_fit"), other_level = legend_arg[3]),
      var_names = recode_factor(vars,
                                real_y = legend_arg[1],
                                synth_y = legend_arg[2])
      )

  pdat %>%
    droplevels() %>%
    ggplot(aes(year, values, group = vars, colour = var_names, size = var_names)) +
    geom_line() +
    scale_colour_manual(values = c(tula_pal(1), "grey50")) +
    scale_size_manual(values = c(2,1.5), guide = "none") +
    # the_legend_bot() +
    the_title_blank(c("l", "x")) +
    labs(title = title ,y = ylab) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    geom_vline(xintercept = base_year, size = 0.5, linetype = "dashed")
}


#' @describeIn synth_plot

synth_plot_diff <- function(synth_pdat, base_year,
                           title = "b. Ero synteettiseen kontrolliin",
                           ylab = "%",
                           legend_arg = c("Suomi", "Synteettinen kontrolli")){
  synth_pdat %>%
    group_by(year) %>%
    mutate(values = 100 * (values[vars == "real_y"]  / values -1)) %>%
    ungroup() %>%
    # mutate(vars = forcats::fct_recode(vars, real_full = "synth_y", syn_fit = "real_y")) %>%
    synth_plot_level(base_year = base_year, title = title, ylab = ylab, legend_arg = legend_arg)

}

#' @describeIn synth_plot

synth_plot_diff_100 <- function(synth_pdat, base_year,
                           title = "b. Suomi suhteessa\nsynteettiseen kontrolliin",
                           ylab = "Indeksi, synteettinen kontrolli = 100",
                           legend_arg = c("Suomi", "Synteettinen kontrolli", "Vaihtoehtoiset")){
  synth_pdat %>%
    group_by(year) %>%
    mutate(values = 100 * (values[vars == "real_y"]  / values)) %>%
    ungroup() %>%
    # mutate(vars = forcats::fct_recode(vars, real_full = "syn_fit", syn_fit = "real_full")) %>%
    mutate(
      # vars = fct_relevel(vars, "syn_fit", "real_full", after = Inf),
      # var_names = fct_other(vars, keep = c("Z1", "real_full", "syn_fit"), other_level = legend_arg[3]),
      var_names = recode_factor(vars,
                                real_y = legend_arg[1],
                                synth_y = legend_arg[2])
    )  %>%
    filter(vars != "real_y") %>%
    droplevels() %>%
    ggplot(aes(year, values, group = vars, colour = var_names, size = var_names)) +
    geom_line() +
    scale_colour_manual(values = c(tula_pal(1), "grey75")) +
    scale_size_manual(values = c(2,1), guide = "none") +
    # the_legend_bot() +
    geom_hline(yintercept = 100) +
    the_title_blank(c("l", "x")) +
    labs(title = title ,y = ylab) +
    theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.position = "none") +
    geom_vline(xintercept = base_year, size = 0.5, linetype = "dashed")

}


#
#
#
# plot.fit <-
#   function(z.list,
#            W_star,
#            start_date,
#            title = "",
#            intervention_date,
#            LOO = NULL,
#            looline = FALSE,
#            legend_arg = NULL,
#            write_csv = NULL,
#            write_pdf = NULL) {
#     Z1 <- z.list$Z1
#     Z0 <- z.list$Z0
#     Z1_post <- z.list$Z1_post
#     Z0_post <- z.list$Z0_post
#     real_full <- ts(c(Z1, Z1_post), start = start_date, frequency = 1)
#
#     syn_fit1 <- ts(Z0 %*% W_star, start = start_date, frequency = 1)
#     syn_fit2 <-
#       ts(c(syn_fit1, Z0_post %*% W_star),
#          start = start_date,
#          frequency = 1)
#     syn_fit3 <- syn_fit2
#     syn_fit2[1:(length(syn_fit1) - 1)] <- NA
#
#     ymax <- max(na.omit(c(real_full, syn_fit1, syn_fit2))) + 2
#
#     if (!is.null(write_pdf) &
#         looline == FALSE)
#       pdf(write_pdf,
#           width = 9,
#           height = 6,
#           paper = 'special')
#
#     if (looline == TRUE) {
#       lines(syn_fit1,
#             lwd = 1,
#             lty = 2,
#             col = "grey")
#       lines(syn_fit2,
#             lwd = 1,
#             lty = 2,
#             col = "grey")
#     } else {
#       plot(
#         real_full,
#         lwd = 2,
#         main = title,
#         ylab = "Indeksi",
#         xlab = "",
#         ylim = c(100, ymax)
#       )
#       lines(syn_fit1, lwd = 2, col = "tomato")
#       lines(syn_fit2,
#             lwd = 2,
#             lty = 2,
#             col = "tomato")
#       abline(
#         v = intervention_date,
#         col = "grey",
#         lwd = 2,
#         lty = 2
#       )
#
#       timevec <-
#         attributes(real_full)$tsp[1]:(attributes(real_full)$tsp[1] + length(real_full) -
#                                         1)
#       mat <- cbind(timevec, real_full, syn_fit3)
#       colnames(mat) <- c("year", "real", "sc")
#
#       if (!is.null(LOO)) {
#         for (j in 1:length(LOO)) {
#           plot.fit(LOO[[j]][["z.list"]],
#                    LOO[[j]][["W_star"]],
#                    start_date,
#                    "",
#                    intervention_date,
#                    looline = TRUE)
#           syn_fit1_loo <-
#             ts(LOO[[j]][["z.list"]][["Z0"]] %*% LOO[[j]][["W_star"]],
#                start = start_date,
#                frequency = 1)
#           syn_fit2_loo <-
#             ts(c(syn_fit1, LOO[[j]][["z.list"]][["Z0_post"]] %*% LOO[[j]][["W_star"]]),
#                start = start_date,
#                frequency = 1)
#           mat <- cbind(mat, syn_fit2_loo)
#         }
#         allData <- z.list$theCall$longdata
#         countries <-
#           levels(as.factor(allData$geo))[-which(levels(as.factor(allData$geo)) == "FI")]
#         colnames(mat) <-
#           c("year", "real", "sc", paste0("exc", countries))
#         lines(real_full,
#               col = "black",
#               lwd = 2,
#               lty = 1)
#         lines(syn_fit1, lwd = 2, col = "tomato")
#         lines(syn_fit2,
#               lwd = 2,
#               lty = 2,
#               col = "tomato")
#         if (is.null(legend_arg)) {
#           legend(
#             "topleft",
#             legend = c(
#               "Suomi",
#               "Synteettinen kontrolli",
#               "Yksi maa jätetty pois maajoukosta"
#             ),
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#         } else {
#           legend(
#             "topleft",
#             legend = legend_arg,
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#         }
#         axis(1, 2007)
#         axis(1)
#       } else {
#         if (is.null(legend_arg)) {
#           legend(
#             "topleft",
#             legend = c(
#               "Suomi",
#               "Synteettinen kontrolli",
#               "Yksi maa jätetty pois maajoukosta"
#             ),
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#         } else {
#           legend(
#             "topleft",
#             legend = legend_arg,
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#         }
#       }
#
#       if (!is.null(write_pdf))
#         dev.off()
#
#       if (!is.null(write_csv)) {
#         write.csv(mat, write_csv, row.names = FALSE)
#       }
#     }
#
#   }
#
# plot.diff <-
#   function(z.list,
#            W_star,
#            intervention,
#            title = "",
#            history = FALSE,
#            LOO = NULL,
#            scale = NULL,
#            legend_arg = NULL,
#            looline = FALSE,
#            write_csv = NULL,
#            write_pdf = NULL) {
#     Z1 <- z.list$Z1
#     Z0 <- z.list$Z0
#     Z1_post <- z.list$Z1_post
#     Z0_post <- z.list$Z0_post
#
#     the_diff <-
#       100 * (Z1_post - ts(Z0_post %*% W_star, start = intervention + 1, frequency = 1)) /
#       Z1_post
#
#     if (history == TRUE) {
#       the_diff_past <-
#         100 * (Z1 - ts(
#           Z0 %*% W_star,
#           start = attributes(Z1)$tsp[1],
#           frequency = 1
#         )) / Z1
#       the_diff <-
#         ts(c(the_diff_past, the_diff),
#            start = attributes(Z1)$tsp[1],
#            frequency = 1)
#       zero <- the_diff_past
#       zero[] <- 0
#     }
#
#     if (!is.null(write_pdf) &
#         looline == FALSE)
#       pdf(write_pdf,
#           width = 9,
#           height = 6,
#           paper = 'special')
#
#     if (looline == TRUE) {
#       lines(the_diff,
#             col = "grey",
#             lwd = 1,
#             lty = 2)
#     } else {
#       title <- paste0(title)
#       if (is.null(scale))
#         scale <- max(abs(the_diff))
#       plot(
#         the_diff,
#         col = "black",
#         lwd = 2,
#         lty = 1,
#         ylim = c(-scale, scale),
#         main = title,
#         ylab = "%",
#         xlab = ""
#       )
#       timevec <-
#         attributes(the_diff)$tsp[1]:(attributes(the_diff)$tsp[1] + length(the_diff) -
#                                        1)
#       mat <- cbind(timevec, the_diff)
#       colnames(mat) <- c("year", "difference")
#
#       if (history == TRUE) {
#         abline(
#           v = intervention,
#           lty = 2,
#           lwd = 2,
#           col = "grey"
#         )
#         abline(
#           h = 0,
#           lwd = 2,
#           lty = 2,
#           col = "tomato"
#         )
#         lines(zero,
#               lwd = 2,
#               lty = 1,
#               col = "tomato")
#         if (!is.null(LOO)) {
#           for (j in 1:length(LOO)) {
#             plot.diff(LOO[[j]][["z.list"]],
#                       LOO[[j]][["W_star"]],
#                       intervention,
#                       history = TRUE,
#                       looline = TRUE)
#             the_diff_loo <-
#               100 * (LOO[[j]][["z.list"]][["Z1_post"]] - ts(
#                 LOO[[j]][["z.list"]][["Z0_post"]] %*% LOO[[j]][["W_star"]],
#                 start = intervention + 1,
#                 frequency = 1
#               )) / LOO[[j]][["z.list"]][["Z1_post"]]
#             the_diff_past_loo <-
#               100 * (LOO[[j]][["z.list"]][["Z1"]] - ts(
#                 LOO[[j]][["z.list"]][["Z0"]] %*% LOO[[j]][["W_star"]],
#                 start = attributes(Z1)$tsp[1],
#                 frequency = 1
#               )) / Z1
#             the_diff_loo <-
#               ts(
#                 c(the_diff_past_loo, the_diff_loo),
#                 start = attributes(Z1)$tsp[1],
#                 frequency = 1
#               )
#             mat <- cbind(mat, the_diff_loo)
#           }
#           allData <- z.list$theCall$longdata
#           countries <-
#             levels(as.factor(allData$geo))[-which(levels(as.factor(allData$geo)) == "FI")]
#           colnames(mat) <-
#             c("year", "difference", paste0("exc", countries))
#           lines(the_diff,
#                 col = "black",
#                 lwd = 2,
#                 lty = 1)
#           legend(
#             "topleft",
#             legend = c(
#               "Suomi",
#               "Synteettinen kontrolli",
#               "Yksi maa jätetty pois maajoukosta"
#             ),
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#           axis(1, 2007)
#           axis(1)
#         } else {
#           if (is.null(legend_arg)) {
#             legend(
#               "topleft",
#               legend = c(
#                 "Suomi",
#                 "Synteettinen kontrolli",
#                 "Yksi maa jätetty pois maajoukosta"
#               ),
#               col = c("black", "tomato", "lightgrey"),
#               lwd = c(2, 2, 1),
#               lty = c(1, 2, 2)
#             )
#           } else {
#             legend(
#               "topleft",
#               legend = legend_arg,
#               col = c("black", "tomato", "lightgrey"),
#               lwd = c(2, 2, 1),
#               lty = c(1, 2, 2)
#             )
#           }
#         }
#
#       } else {
#         abline(
#           h = 0,
#           lwd = 2,
#           lty = 2,
#           col = "tomato"
#         )
#         if (is.null(legend_arg)) {
#           legend(
#             "topleft",
#             legend = c(
#               "Suomi",
#               "Synteettinen kontrolli",
#               "Yksi maa jätetty pois maajoukosta"
#             ),
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#         } else {
#           legend(
#             "topleft",
#             legend = legend_arg,
#             col = c("black", "tomato", "lightgrey"),
#             lwd = c(2, 2, 1),
#             lty = c(1, 2, 2)
#           )
#         }
#       }
#
#       if (!is.null(write_pdf))
#         dev.off()
#
#       if (!is.null(write_csv)) {
#         write.csv(mat, write_csv, row.names = FALSE)
#       }
#     }
#
#   }
