# DESCRIPTION --------------------------------------------------------------------------------------
# This R script extracts Landsat surface reflectance time series data for all pixels in an example 
# study area on Disko Island using the lsatTS package to query Google Earth Engine via the rgee package.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-18
# URL: https://github.com/logan-berner/lsatTS 
#---------------------------------------------------------------------------------------------------

# Load required R packages
require(sf)
require(LandsatTS)
require(rgee)
require(leaflet)

# Create a spatial polygon that demarcates the study area
aoi.sf <- st_polygon(list(matrix(
  c(-332950,-2243300,
    -334950,-2243300,
    -334950,-2245300,
    -332950,-2245300,
    -332950,-2243300),
  ncol = 2, byrow = T)))

aoi.sf <- aoi.sf %>% st_sfc(crs = 3413) %>% st_transform(crs = 4326) %>% st_as_sf()

# Create 100 random sample points within study area polygon 
n.pts <- 100
pts.sf <- st_sample(x = aoi.poly, size = n.pts) %>% st_sf()

# Add unique identifier to each point
pts.sf$sample_id = paste0('S_', 1:n.pts)

# Simple map of site locations 
leaflet(pts.sf) %>%
  addTiles() %>% 
  addMarkers()

# Initialize Earth Engine
ee_Initialize()

# Extract a time-series of Landsat surface reflectance measurements for each Landsat pixel
task_list <- lsat_export_ts(pixel_coords_sf = pts.sf,
                            start_doy = 152, 
                            end_doy = 273,
                            file_prefix = 'disko', 
                            drive_export_dir = 'earth_engine')

# END SCRIPT ----------------------------------------------------------------------------------------