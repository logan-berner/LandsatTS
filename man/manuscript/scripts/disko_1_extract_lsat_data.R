# DESCRIPTION --------------------------------------------------------------------------------------
# This R script extracts Landsat surface reflectance time series data for all pixels in an example 
# study area on Disko Island using the lsatTS package to query Google Earth Engine via the rgee package.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-18
# URL: https://github.com/logan-berner/lsatTS
#---------------------------------------------------------------------------------------------------

# Clear workspace
rm(list=ls())

# Load required R packages
require(sf)
require(lsatTS)
require(rgee)

# Set working directory
setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')

# Initialize Earth Engine
ee_Initialize()

# Create a spatial polygon that demarcates the study area
aoi.proj <- st_crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

aoi.poly <- st_polygon(list(matrix(
  c(-332950,-2243300,
    -334950,-2243300,
    -334950,-2245300,
    -332950,-2245300,
    -332950,-2243300),
  ncol = 2, byrow = T))) %>%
  st_sfc(crs = aoi.proj)

# Get the central coordinates for each Landsat pixels in study area
aoi.pts <- lsat_get_pixel_centers(aoi.poly)

# Extract a time-series of Landsat surface reflectance measurements for each Landsat pixel
task_list <- lsat_export_ts(pixel_coords_sf = aoi.pts, startJulian = 142, endJulian = 243,
                            file_prefix = 'disko', drive_export_dir = 'earth_engine')

# END SCRIPT ----------------------------------------------------------------------------------------