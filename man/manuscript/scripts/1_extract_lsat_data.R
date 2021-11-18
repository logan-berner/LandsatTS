# DESCRIPTION --------------------------------------------------------------------------------------
# This R script creates a random sample within a specificed area of interest and then extracts 
# Landsat time series for each sample point by querying Google Earth Engine. 
# Author: Logan Berner, NAU
# Date: 2021-10-19

# SET UP -------------------------------------------------------------------------------------------
rm(list=ls())
require(sf)
require(dplyr)
require(lsatTS)
require(rgee)
setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')
ee_Initialize()

# CREATE SAMPLE POINTS AND EXTRACT LANDSAT DATA ---------------------------------------------------

# Read in a spatial polygon that demarcates the study area
aoi.poly <- st_read('data/aoi_extent.kml')

# Get the central coordinates of each Landsat pixel in study area
aoi.pts <- lsat_get_pixel_centers(aoi.poly)

# Extract a time-series of Landsat surface reflectance measurements for each Landsat pixel
task_list <- lsat_export_ts(pixel_coords_sf = aoi.pts, startJulian = 142, endJulian = 243,
                            file_prefix = 'disko', drive_export_dir = 'earth_engine')

# END SCRIPT ----------------------------------------------------------------------------------------