% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/synth_plot.R
\name{synth_plot}
\alias{synth_plot}
\title{Plot synth object}
\usage{
synth_plot(
  pdat_synt,
  title,
  y_lab,
  legend_arg = c("Suomi", "Synteettinen kontrolli")
)
}
\arguments{
\item{legend_arg}{}
}
\description{
Plot synth object
}
\examples{
  synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private") |>
    synth_plot("Testi", "indeksi, 100 = 2007")

  synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private") |>
    synth_plot_level(2007, "Testi")

  synth_plot_data(synth_obj = synth10_est_results, plot_var = "lp_ind", nace = "private") |>
    synth_plot_diff(2007, "Testi")
}
