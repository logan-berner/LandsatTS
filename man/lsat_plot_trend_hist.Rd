% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsat_plot_trend_hist.R
\name{lsat_plot_trend_hist}
\alias{lsat_plot_trend_hist}
\title{Create a histogram summarizing relative temporal changes in a spectral index
across all sample sites.}
\usage{
lsat_plot_trend_hist(dt, xlim = c(-30, 30))
}
\arguments{
\item{dt}{A data.table output from lsat_calc_trend()}

\item{xlim}{Numeric vector specifying the minimum and maximum values for the
histogram x-axis.}
}
\value{
A histogram generated by ggplot2
}
\description{
Create a histogram summarizing relative temporal changes in a spectral index
across all sample sites.
}
\examples{
data(lsat.example.dt)
lsat.dt <- lsat_format_data(lsat.example.dt)
lsat.dt <- lsat_clean_data(lsat.dt)
lsat.dt <- lsat_calc_spectral_index(lsat.dt, 'ndvi')
# lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', write.output = F)
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi') 
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi')
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', yrs = 2000:2020)
lsat_plot_trend_hist(lsat.trend.dt)
}
