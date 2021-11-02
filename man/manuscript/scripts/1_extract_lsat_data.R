# This R script creates a random sample within a specificed area of interest and then extracts 
# Landsat time series for each sample point by querying Google Earth Engine. 
# Author: Logan Berner, NAU
# Date: 2021-10-19

# SET UP -------------------------------------------------------------------------------------------
rm(list=ls())
require(rgdal)
require(sf)
require(dplyr)
require(lsatTS)
require(rgee)
require(dplyr)

setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')
# ak.aea.proj <- '+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'

# load polygon of study area
aoi.sf <- st_read('data/aoi_extent.kml')

# project?
# aoi.sf <- st_transform(aoi.sf, ak.aea.proj)
# plot(aoi.sf)

# sample study area
pts <- st_sample(aoi.sf, size = 100) %>% st_cast('POINT')
plot(pts, col = 'blue')

# add unique ID to each sample point
pts <- pts %>% st_sf %>% mutate(sample_id = paste0('pt_', 1:nrow(st_coordinates(pts))), region = c("disco"))

# extract Landsat time-series for sample points
ee_Initialize()
task_list <- lsat_export_ts(pixel_coords_sf = pts, site_from = 'sample_id', file_prefix = 'disco',
                            drive_export_dir = 'earth_engine', startJulian = 142, endJulian = 243)

# END SCRIPT ----------------------------------------------------------------------------------------