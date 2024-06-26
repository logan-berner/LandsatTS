% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsat_fit_phenological_curves.R
\name{lsat_fit_phenological_curves}
\alias{lsat_fit_phenological_curves}
\title{Characterize land surface phenology using spectral vegetation index time series}
\usage{
lsat_fit_phenological_curves(
  dt,
  si,
  window.yrs = 7,
  window.min.obs = 20,
  si.min = 0.15,
  spar = 0.78,
  pcnt.dif.thresh = c(-30, 30),
  weight = TRUE,
  spl.fit.outfile = FALSE,
  progress = TRUE,
  test.run = FALSE,
  plot.title = NA
)
}
\arguments{
\item{dt}{Data.table with a multi-year time series a vegetation index.}

\item{si}{Character string specifying the spectral index (e.g., NDVI) to use for
determining surface phenology. This must correspond to an existing column
in the data.table.}

\item{window.yrs}{Number specifying the focal window width in years that is used when
pooling data to fit cubic splines (use odd numbers).}

\item{window.min.obs}{Minimum number of focal window observations necessary to fit
a cubic spline.}

\item{si.min}{Minimum value of spectral index necessary for observation to be used
when fitting cubic splines. Defaults to 0.15 which for NDVI is about when plants
are present. Note that si.min must be >= 0 because the underlying spline fitting
function will error out if provided negative values.}

\item{spar}{Smoothing parameter typically around 0.70 - 0.80 for this application.
A higher value means a less flexible spline. Defaults to 0.78.}

\item{pcnt.dif.thresh}{Vector with two numbers that specify the allowable
negative and positive percent difference between individual
observations and fitted cubic spline. Observations that differ by more than
these thresholds are filtered out and the cubic spline is iteratively refit.
Defaults to -30\% and 30\%.}

\item{weight}{When fitting the cubic splines, should individual observations be
weighted by their year of acquisition relative to the focal year?
If so, each observation is weighted by exp(-0.25*n.yrs.from.focal) when fitting the cubic splines.}

\item{spl.fit.outfile}{(Optional) Name of output csv file containing the fitted
cubic splines for each sample site. Useful for subsequent visualization.}

\item{progress}{(TRUE/FALSE) Print a progress report?}

\item{test.run}{(TRUE/FALSE) If TRUE, then algorithm is run using a small random
subset of data and only a figure is output. This is used for model parameterization.}

\item{plot.title}{(Optional) Custom title for output figure.}
}
\value{
Data.table that provides, for each observation, information on the phenological
conditions for that specific day of year during the focal period.
These data can then be used to estimate annual maximum spectral index
and other growing season metrics using lsat_summarize_growing_season().
A figure is also generated that shows observation points and phenological
curves for nine random sample locations.
}
\description{
This function characterizes seasonal land surface phenology at each sample site using
flexible cubic splines that are iteratively fit to time series of spectral vegetation indices
(e.g., NDVI). This function facilitates estimating annual maximum NDVI and other
spectral vegetation indices with lsat_summarize_growing_seasons(). For each site,
cubic splines are iteratively fit to measurements pooled over years within a moving
window that has a user-specified width. Each cubic spline is iteratively fit, with
each iteration checking if there are outliers and, if so, excluding outliers and refitting.
The function returns information about typical phenology at a sample site and about
the relative phenological timing of each individual measuremenent. This function was
designed for situations where the seasonal phenology is hump-shaped. If you are using
a spectral index that is typically negative (e.g., Normalized Difference Water Index)
then multiply the index by -1 before running this function, then back-transform
your index after running the lsat_summarize_growing_seasons() function.
}
\examples{
data(lsat.example.dt)
lsat.dt <- lsat_format_data(lsat.example.dt)
lsat.dt <- lsat_clean_data(lsat.dt)
lsat.dt <- lsat_calc_spectral_index(lsat.dt, 'ndvi')
lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', 
write.output = FALSE, train.with.highlat.data = TRUE)
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi')
lsat.pheno.dt
}
