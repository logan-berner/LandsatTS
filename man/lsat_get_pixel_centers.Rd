% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lsat_get_pixel_centers.R
\name{lsat_get_pixel_centers}
\alias{lsat_get_pixel_centers}
\title{Get Landsat 8 pixel centers for a polygon or a buffered point}
\usage{
lsat_get_pixel_centers(
  polygon_sf,
  pixel_prefix = "pixel",
  pixel_prefix_from = NULL,
  buffer = 15,
  plot_map = F,
  lsat_WRS2_scene_bounds = NULL
)
}
\arguments{
\item{polygon_sf}{Simple feature with a simple feature collection of type
"sfc_POLYGON" containing a single polygon geometry. Alternatively, a simple
feature containing a simple feature collection of type 'sfc_POINT' with a
single point.}

\item{pixel_prefix}{Prefix for the generated pixel identifiers (output column
"sample_id"). Defaults to "pixel".}

\item{pixel_prefix_from}{Optional, a column name in the simple feature to specify
the pixel_prefix. Overrides the "pixel_prefix" argument.}

\item{buffer}{Buffer surrounding the geometry to be included. Specified in m.
Defaults to 15 m - the nominal half-width of a Landsat pixel.}

\item{plot_map}{Optional, default is FALSE. If TRUE the retrieved pixel
centers and the polygon are plotted on a summer Landsat 8 image
(grey-scale red band) using mapview. If a character is supplied an
additional output to a file is generated (png, pdf, and jpg supported, see
mapview::mapshot). Note: Both slow down the execution of this function
notably, especially for large polygons! Only use in interactive R sessions.}

\item{lsat_WRS2_scene_bounds}{File path to the Landsat WRS2 path row scene
boundaries. If not specified the boundaries are downloaded to a temporary
file when the function is executed for the first time during a session. To
avoid future downloads, the file may be downloaded manually and it's file
path specified using this argument.
The file can be found here:
https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/ \cr
atoms/files/WRS-2_bound_world_0.kml
See also:
https://www.usgs.gov/core-science-systems/nli/landsat/ \cr
landsat-shapefiles-and-kml-files}
}
\value{
sf object of point geometries for Landsat 8 pixel centers within the
polygon or the buffer around the point coordinate specified. For use in
lsat_export_ts().
}
\description{
A convenience helper function that determines the Landsat 8 grid (pixel)
centers within a polygon plus an optional buffer. It can also be applied to a
single point to retrieve all pixels within a buffer. \cr\cr
Does not work for large polygons. The default maximum number of pixels set by
EE is 10000000 this should not be exceeded. Consider whether extraction for a
large polygon is a good idea, if yes split the polygon into manageable chunks.
\cr\cr
For the unlikely case that a polygon exceeds the boundaries of the Landsat
tile closest to the polygon's center, the polygon is clipped at the
boundaries of the Landsat tile and a warning is issued. Again, if this is the
case, consider processing smaller polygons instead. \cr\cr
Please note: The approximation of the tile overlap with the polygon generates
a warning by the sf package that the coordinates are assumed to be planar.
This can be ignored.
}
\examples{
# Only run example if "rgee" is installed
if (requireNamespace("rgee", quietly = TRUE)) { 

# Using sf, dplyr, rgee and purr
library(sf)
library(dplyr)
library(rgee)
library(purrr)

# Initialize EE
ee_Initialize()

# Specify a region to retrieve pixel centers for
test_poly_sf <- list(matrix(c(-138.90125, 69.58413,
              -138.88988, 69.58358,
              -138.89147, 69.58095,
              -138.90298, 69.57986,
              -138.90125, 69.58413),
            ncol = 2, byrow = TRUE)) \%>\%
           st_polygon() \%>\%
           st_sfc(crs = 4326) \%>\%
           st_sf()

# Retrieve pixel centers and plot to mapview
pixels <- lsat_get_pixel_centers(test_poly_sf, plot_map = TRUE)


## Ge pixel centers for multiple regions
# Create multi-polygon sf
ellesmere <- st_polygon(list(matrix(c(-75.78526, 78.86973,
                                      -75.78526, 78.87246,
                                      -75.77116, 78.87246,
                                      -75.77116, 78.86973,
                                      -75.78526, 78.86973),
                                      ncol = 2, byrow = TRUE)))
zackenberg <- st_polygon(list(matrix(c(-20.56254, 74.47469,
                                    -20.56254, 74.47740,
                                    -20.55242, 74.47740,
                                    -20.55242, 74.47469,
                                    -20.56254, 74.47469),
                                  ncol = 2, byrow = TRUE)))
toolik <- st_polygon(list(matrix(c(-149.60686, 68.62364,
                                   -149.60686, 68.62644,
                                   -149.59918, 68.62644,
                                   -149.59918, 68.62364,
                                   -149.60686, 68.62364),
                                   ncol = 2, byrow = TRUE)))
test_regions_sf <- st_sfc(ellesmere, zackenberg, toolik, crs = 4326) \%>\%
  st_sf() \%>\%
  mutate(region = c("ellesmere", "zackenberg", "toolik"))

# Split and map lsat_get_pixel_centers using dplyr and purrr
pixel_list <- test_regions_sf \%>\%
   split(.$region) \%>\%
   map(lsat_get_pixel_centers,
       pixel_prefix_from = "region") \%>\%
   bind_rows()

# Closing bracket for the "rgee" check   
}
}
\author{
Jakob J. Assmann
}
