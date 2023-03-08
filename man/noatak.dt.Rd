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
    \item{\samp{system:index}}{GEE index}
    \item{\code{CLOUD_COVER}}{Percent cloud cover of scene}
    \item{\code{COLLECTION_NUMBER}}{a numeric vector}
    \item{\code{DATE_ACQUIRED}}{Acquisition date in yyyy-mm-dd format}
    \item{\code{GEOMETRIC_RMSE_MODEL}}{a numeric vector}
    \item{\code{LANDSAT_PRODUCT_ID}}{Landsat product identifier}
    \item{\code{LANDSAT_SCENE_ID}}{Landsat scene identifier}
    \item{\code{PROCESSING_LEVEL}}{Processing level}
    \item{\code{QA_PIXEL}}{CFMASK pixel quality attribute bitmask}
    \item{\code{QA_RADSAT}}{Radiometric saturation bitmask}
    \item{\code{SPACECRAFT_ID}}{Name of the spacecraft}
    \item{\code{SR_B1}}{Band 1 surface reflectance}
    \item{\code{SR_B2}}{Band 2 surface reflectance}
    \item{\code{SR_B3}}{Band 3 surface reflectance}
    \item{\code{SR_B4}}{Band 4 surface reflectance}
    \item{\code{SR_B5}}{Band 5 surface reflectance}
    \item{\code{SR_B6}}{Band 6 surface reflectance}
    \item{\code{SR_B7}}{Band 7 surface reflectance}
    \item{\code{SUN_ELEVATION}}{Sun elevation angle in degrees for the image center location at the image center acquisition time}
    \item{\code{chunk_id}}{Download chunk identifier}
    \item{\code{max_extent}}{JRC maximum water extent flag }
    \item{\code{sample_id}}{User specificed sample location identifier}
    \item{\code{.geo}}{Sample location coordinates}
  }
}
\source{
Generated as example data for Berner et al. (2023) 
}
\references{
Landsat data are provided by the United States Geological Survey
}
\keyword{datasets}
