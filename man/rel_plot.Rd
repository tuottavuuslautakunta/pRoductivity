% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{rel_plot}
\alias{rel_plot}
\title{Weighted plot of three variables}
\usage{
rel_plot(
  .data,
  nace,
  w_plot_vars = c(`Työn tuottavuus` = "lp_ind_rel_ecfin37", Arvonlisäys =
    "va_ind_rel_ecfin37", Työtunnit = "h_ind_rel_ecfin37")
)
}
\arguments{
\item{.data}{A data to plot.}

\item{nace}{A industry classification to plot}
}
\description{
Weighted plot of three variables
}
