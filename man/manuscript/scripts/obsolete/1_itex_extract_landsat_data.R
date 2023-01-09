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
require(rnaturalearth)

# Load data set with ITEX site coordinates (Prevéy et al. 2021)
data(itex.sites.dt)

# Create simple feature collection of points using ITEX site coordinates 
itex.sites.sf <- itex.sites.dt %>% 
  data.frame() %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant") %>% 
  st_cast('POINT')

# Simple map of site locations 
ggplot() +
  geom_sf(data = ne_countries(scale = "medium", returnclass = "sf")) +
  geom_sf(data = itex.sites.sf, color = 'black', fill = 'red', pch = 21, size = 1) + 
  coord_sf(crs = 4326, xlim = c(-180, 180), ylim = c(-90, 90))

# ggsave('man/manuscript/figures/figure_2_itex_site_locations.jpg', width = 9, height = 6, units = "cm", dpi = 400)

# randomly subset three sites
itex.sites.sf <- itex.sites.sf[sample(1:nrow(itex.sites.sf), 3),]

# Initialize Earth Engine
ee_Initialize()

# Extract Landsat surface reflectance time-series for each ITEX site
lsat_export_ts(pixel_coords_sf = itex.sites.sf, 
               start_date = "1985-06-01", 
               end_date = "2021-09-30",
               startJulian = 152, 
               endJulian = 273, 
               file_prefix = 'itex', 
               drive_export_dir = 'gee_export')

# END SCRIPT ----------------------------------------------------------------------------------------