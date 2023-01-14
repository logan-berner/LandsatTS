# DESCRIPTION --------------------------------------------------------------------------------------
# This R script extracts Landsat surface reflectance time series data for a sample
# of pixels in the Noatak National Preserve in northern Alaska using the LandsatTS
# package to query Google Earth Engine. 
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-18
#-------------------------------------------------------------------------------

# Load required R packages
require(LandsatTS)
require(sf)
require(rgee)
require(tidyverse)
require(leaflet)
require(mapview)

# Load the Noatak National Preserve simple feature polygon
data(noatak.sf)

# Create n random sample points within the Noatak National Preserve 
n.pts <- 100
noatak.pts.sf <- st_sample(x = noatak.sf, size = n.pts) %>% st_sf()

# Add unique identifier to each point
noatak.pts.sf$sample_id <- paste0('S_', 1:n.pts)

# Make basic interactive map showing Noatak National Preserve and sample points 
leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>% 
  addCircleMarkers(data = noatak.pts.sf, 
                   color = 'white', 
                   opacity = 0.9,
                   fillColor = 'fuchsia', 
                   fillOpacity = 0.75, 
                   weight = 1, 
                   radius = 5) %>% 
  addPolygons(data = noatak.sf, 
              color = 'white', 
              weight = 3) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))


# Initialize Earth Engine
ee_Initialize()

# Extract a time-series of surface reflectance measurements for each Landsat pixel
task_list <- lsat_export_ts(pixel_coords_sf = noatak.pts.sf,
                            start_date = "1985-06-01", 
                            end_date = "2022-09-30",
                            start_doy = 152, 
                            end_doy = 273,
                            file_prefix = 'noatak', 
                            drive_export_dir = 'earth_engine')

# END SCRIPT -------------------------------------------------------------------

# Simple map of site locations 
map <- leaflet() %>%
  addProviderTiles('Esri.WorldImagery') %>% 
  addCircleMarkers(data = noatak.pts.sf, 
                   color = 'white',
                   opacity = 0.9,
                   fillColor = 'fuchsia',
                   fillOpacity = 0.75,
                   weight = 1,
                   radius = 4) %>% 
  addPolygons(data = noatak.sf, color = 'white', weight = 3) %>%
  setView(lng = -155, lat = 66, zoom = 5) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))

map 

mapshot(map, 
        file = 'man/manuscript/figures/figure_2_noatak_map.jpeg',
        remove_controls = NULL)
