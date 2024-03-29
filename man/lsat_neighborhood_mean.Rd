% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsat_neighborhood_mean.R
\name{lsat_neighborhood_mean}
\alias{lsat_neighborhood_mean}
\title{Compute Neighborhood Average Landsat Surface Reflectance}
\usage{
lsat_neighborhood_mean(dt)
}
\arguments{
\item{dt}{A data.table containing coincident surface reflectance measurements
for multiple Landsat pixels at each sample site.}
}
\value{
A data.table with average surface reflectance
}
\description{
For each band, this function computes average surface reflectance
across neighboring voxels at a sample site. Use this function when working with
Landsat data extracted for buffered points. Also, make sure to have previously
cleaning the individual observations using lsat_clean_data().
}
