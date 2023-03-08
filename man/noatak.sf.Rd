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
  A simple feature data frame with 1 observations on the following 1 variable.
  \describe{
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
