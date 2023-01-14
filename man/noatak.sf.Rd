\name{noatak.sf}
\alias{noatak.sf}
\docType{data}
\title{Noatak National Preserve Simple Feature}
\description{
  This dataset provides the spatial boundary of the Noatak National Preserve, USA,
  as a multipolygon simple feature.
}
\usage{data("noatak.sf")}
\format{
  A simple feature data frame with 1 observations on the following 20 variables.
  \describe{
    \item{\code{OBJECTID}}{a numeric vector}
    \item{\code{UNIT_CODE}}{a character vector}
    \item{\code{GIS_Notes}}{a character vector}
    \item{\code{UNIT_NAME}}{a character vector}
    \item{\code{DATE_EDIT}}{a character vector}
    \item{\code{STATE}}{a character vector}
    \item{\code{REGION}}{a character vector}
    \item{\code{GNIS_ID}}{a character vector}
    \item{\code{UNIT_TYPE}}{a character vector}
    \item{\code{CREATED_BY}}{a character vector}
    \item{\code{METADATA}}{a character vector}
    \item{\code{PARKNAME}}{a character vector}
    \item{\code{CreationDa}}{a character vector}
    \item{\code{Creator}}{a character vector}
    \item{\code{EditDate}}{a character vector}
    \item{\code{Editor}}{a character vector}
    \item{\code{GlobalID}}{a character vector}
    \item{\code{Shape_Leng}}{a numeric vector}
    \item{\code{Shape_Area}}{a numeric vector}
    \item{\code{geometry}}{a sfc_MULTIPOLYGON}
  }
}
\source{
Downloaded from https://irma.nps.gov/DataStore/Reference/Profile/2296705
}
\references{
US National Park Service. 2022. Administrative Boundaries of National Park System Units - National Geospatial Data Asset (NGDA) NPS National Parks Dataset. NPS - Land Resources Division. https://irma.nps.gov/DataStore/Reference/Profile/2296705
}
\examples{
data(noatak.sf)
plot(noatak.sf)
}
\keyword{datasets}
