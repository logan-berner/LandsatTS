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
require(dplyr)

sf::sf_use_s2(FALSE)

# Set working directory
setwd('C:/Users/lb968/My Drive/research/code/lsatTS/man/manuscript/')

# lsat_export_ts_biome_sample()

# download RESOLVE ecoregions
download.file('https://wri-public-data.s3.amazonaws.com/resourcewatch/bio_042_biome_ecoregion.zip', destfile = 'data/resolve_ecoregions.zip')
unzip(zipfile = 'data/resolve_ecoregions.zip', exdir = 'data/resolve_ecoregions')
unlink('data/resolve_ecoregions.zip')

# read in ecoregions shapefile
ecoreg.sf <- st_read('data/resolve_ecoregions/Ecoregions2017.shp')

# subset Tundra, Boreal Forests/Taiga and dissolve
n.ecoreg.sf <- ecoreg.sf %>% filter((BIOME_NAME == 'Tundra' | BIOME_NAME == 'Boreal Forests/Taiga') & (REALM == 'Nearctic' | REALM == 'Palearctic'))

# reproject
laea <- st_crs("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
n.ecoreg.sf <- st_transform(n.ecoreg.sf, laea)

# dissolve polygons
n.ecoreg.sf <- st_union(n.ecoreg.sf)
plot(n.ecoreg.sf)

# sample high-latitude study area
sample.pts <- st_sample(n.ecoreg.sf, size = 10000) %>% st_cast('POINT')
sample.pts <- sample.pts %>% st_sf %>% mutate(sample_id = paste0('pt', 1:nrow(st_coordinates(sample.pts))))
plot(sample.pts, col = 'blue', add=T)

# cluster sample points to speed data extraction from GEE
sample.pts.latlon <- st_transform(sample.pts, crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords <- st_coordinates(sample.pts.latlon)
dist <- distm(coords) # distance matrix
clusters <- hclust(as.dist(dist), method="complete")
sample.pts$cluster <- cutree(clusters, h=1000000)

# Extract a time-series of Landsat surface reflectance measurements for each Landsat pixel
ee_Initialize()
task_list <- lsat_export_ts(pixel_coords_sf = sample.pts, chunks_from = 'cluster', startJulian = 122, endJulian = 274,
                            file_prefix = 'highlat', drive_export_dir = 'earth_engine')

# END SCRIPT ----------------------------------------------------------------------------------------