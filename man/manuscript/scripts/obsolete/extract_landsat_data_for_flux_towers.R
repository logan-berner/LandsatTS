# DESCRIPTION --------------------------------------------------------------------------------------
# This R script extracts Landsat surface reflectance time series data for all pixels in an example 
# study area on Disko Island using the lsatTS package to query Google Earth Engine via the rgee package.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-18
# URL: https://github.com/logan-berner/lsatTS 
#---------------------------------------------------------------------------------------------------

# Load required R packages
require(LandsatTS)
require(sf)
require(rgee)
require(dplyr)

# Create simple feature collection of points using flux tower coordinates 
# from https://fluxnet.org/sites/site-list-and-pages/

flux_towers_sf <- st_sfc(sf::st_point(c(-121.2992, 61.3079)),
                         sf::st_point(c(-121.6224, 44.4992)),
                         sf::st_point(c(161.3414, 68.6130)),
                         crs = 4326) %>%
  st_sf() %>%
  mutate(sample_id = c("CA-SCC",
                       "US-Me4",
                       "RU-Che"))


# Initialize Earth Engine
ee_Initialize()

# Extract a time-series of Landsat surface reflectance measurements for each Landsat pixel
lsat_export_ts(pixel_coords_sf = flux_towers_sf, startJulian = 152, endJulian = 273,
               file_prefix = 'flux_tower', drive_export_dir = 'gee_export')

lsat_export_ts(pixel_coords_sf = flux_towers_sf, 
               start_date = "1985-06-01", 
               end_date = "2021-09-30",
               startJulian = 152, 
               endJulian = 273, 
               file_prefix = 'flux_tower', 
               drive_export_dir = 'gee_export', 
               BUFFER_DIST = 45)

# END SCRIPT ----------------------------------------------------------------------------------------