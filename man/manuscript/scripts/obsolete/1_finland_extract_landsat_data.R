# DESCRIPTION --------------------------------------------------------------------------------------
# This R script extracts Landsat surface reflectance time series data for ITEX phenology
# monitoring sites using the LandsatTS package to query Google Earth Engine via the rgee package.  
# Author: Logan Berner, Jakob Assman.
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2022-12-05
# URL: https://github.com/logan-berner/LandsatTS 
#---------------------------------------------------------------------------------------------------

# Load required R packages
require(LandsatTS)
require(sf)
require(rgee)
require(dplyr)
require(leaflet)
require(data.table)
require(rnaturalearth)

# Load data set with ITEX site coordinates (Prevéy et al. 2021)
sites.dt <- fread('A:/tmp/finland_site_biomass.csv')
setnames(sites.dt, 'site_id','sample_id')
sites.dt[, sample_id := paste0('S', sample_id)]

# data(sites.dt)

# Create simple feature collection of points using ITEX site coordinates 
sites.sf <- sites.dt %>% 
  data.frame() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
  st_cast('POINT')

# Simple map of site locations 
leaflet(sites.sf) %>%
  addTiles() %>% 
  addMarkers()

ggplot() +
  geom_sf(data = ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(data = sites.sf, color = 'black', fill = 'red', pch = 21, size = 1) + 
  coord_sf(crs = 4326, xlim = c(-180, 180), ylim = c(-90, 90))

# ggsave('man/manuscript/figures/figure_2_itex_site_locations.jpg', width = 9, height = 6, units = "cm", dpi = 400)

# randomly subset three sites
# sites.sf <- sites.sf[sample(1:nrow(sites.sf), 3),]

# Initialize Earth Engine
ee_Initialize()

# Extract Landsat surface reflectance time-series for each ITEX site
lsat_export_ts(pixel_coords_sf = sites.sf, 
               start_date = "1985-06-01", 
               end_date = "2022-09-30",
               start_doy = 152, 
               end_doy = 273, 
               buffer_dist = 0, 
               file_prefix = 'finland', 
               drive_export_dir = 'gee_export')

# END SCRIPT ----------------------------------------------------------------------------------------