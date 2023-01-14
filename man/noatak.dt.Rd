\name{noatak.dt}
\alias{noatak.dt}
\docType{data}
\title{Example Landsat data for the Noatak National Preserve}
\description{
This dataset provides Landsat time series data for 100 random sample locations
within the Noatak National Preserve, USA. These data were extracted from Google 
Earth Engine using the lsat_export_ts() function.
}
\usage{data("noatak.dt")}
\format{
  A data frame with 99600 observations on the following 23 variables.
  \describe{
    \item{\samp{system:index}}{a character vector}
    \item{\code{CLOUD_COVER}}{a numeric vector}
    \item{\code{COLLECTION_NUMBER}}{a numeric vector}
    \item{\code{DATE_ACQUIRED}}{a IDate}
    \item{\code{GEOMETRIC_RMSE_MODEL}}{a numeric vector}
    \item{\code{LANDSAT_PRODUCT_ID}}{a character vector}
    \item{\code{LANDSAT_SCENE_ID}}{a character vector}
    \item{\code{PROCESSING_LEVEL}}{a character vector}
    \item{\code{QA_PIXEL}}{a numeric vector}
    \item{\code{QA_RADSAT}}{a numeric vector}
    \item{\code{SPACECRAFT_ID}}{a character vector}
    \item{\code{SR_B1}}{a numeric vector}
    \item{\code{SR_B2}}{a numeric vector}
    \item{\code{SR_B3}}{a numeric vector}
    \item{\code{SR_B4}}{a numeric vector}
    \item{\code{SR_B5}}{a numeric vector}
    \item{\code{SR_B6}}{a numeric vector}
    \item{\code{SR_B7}}{a numeric vector}
    \item{\code{SUN_ELEVATION}}{a numeric vector}
    \item{\code{chunk_id}}{a character vector}
    \item{\code{max_extent}}{a numeric vector}
    \item{\code{sample_id}}{a character vector}
    \item{\code{.geo}}{a character vector}
  }
}
\source{
Generate as example data for Berner et al. (2023) 
}
\references{
Landsat data are provided by the United States Geological Survey
}
\keyword{datasets}
