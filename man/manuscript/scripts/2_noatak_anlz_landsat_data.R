# DESCRIPTION ---------------------------------------------------------------------------------
# This R script generates and analyzes time series of annual maximum vegetation greenness 
# for pixels in the Noatak National Preserve in northern Alaska.
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2023-01-05
# URL: https://github.com/logan-berner/LandsatTS
# --------------------------------------------------------------------------------------------

# Load required R packages
require(LandsatTS)
require(data.table)
require(tidyverse)
require(sf)
require(leaflet)
require(mapview)

# Load Landsat data for Noatak sites, or read in file using data.table::fread(). 
data(noatak.dt)

# Format the exported data
noatak.dt <- lsat_format_data(noatak.dt)

# Clean the data by filtering out clouds, snow, water, etc.
noatak.dt <- lsat_clean_data(noatak.dt)

# Summarize the availability of Landsat data for each pixel
lsat_summarize_data(noatak.dt)

# ggsave('man/manuscript/figures/figure_3_noatak_landsat_observations.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute the Normalized Difference Vegetation Index (ndvi)
noatak.dt <- lsat_calc_spectral_index(noatak.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using polynomial regression
noatak.dt <- lsat_calibrate_poly(noatak.dt, 
                                 band.or.si = 'ndvi', 
                                 train.with.highlat.data = T, 
                                 overwrite.col = T)

ggsave('man/manuscript/figures/figure_4_noatak_landsat_calibration.jpg',
       width = 8.0, height = 7.5, units = 'in', dpi = 400)

# Fit phenological models (cubic splines) to each time series
noatak.pheno.dt <- lsat_fit_phenological_curves(noatak.dt, si = 'ndvi', window.min.obs = 15, window.yrs = 7)

# ggsave('man/manuscript/figures/figure_5_noatak_phenological_curves.jpg', width = 9, height = 7, units = 'in', dpi = 400)

# Summarize growing season characteristics
noatak.gs.dt <- lsat_summarize_growing_seasons(noatak.pheno.dt, si = 'ndvi', min.frac.of.max = 0.5)

# # Evaluate estimates of annual maximum ndvi
# noatak.gs.eval.dt <- lsat_evaluate_phenological_max(noatak.pheno.dt, si = 'ndvi')
# 
# ggsave('man/manuscript/figures/figure_6_noatak_ndvi_max_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)


# Compute temporal trend in annual ndvimax for each field site
noatak.trend.dt <- lsat_calc_trend(noatak.gs.dt,
                                 si = 'ndvi.max',
                                 yr.tolerance = 2,
                                 yrs = 2000:2022)

# fwrite(noatak.trend.dt, 'man/manuscript/output/noatak_ndvi_trends.csv')

# Plots histogram of trends across sample points
lsat_plot_trend_hist(noatak.trend.dt, xlim = c(-21,21))
# ggsave('man/manuscript/figures/figure_7_noatak_ndvi_trend_hist.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Create an interactive map showing NDVI trends
colors.dt <- data.table(trend.cat = c("greening","no_trend","browning"), 
                        trend.color = c("springgreen","white","orange"))
#                        trend.color = c("#5ab4ac","white","#d8b365"))
#trend.color = c("darkgreen","white","darkgoldenrod"))
                        # trend.color = c("springgreen","white","darkgoldenrod"))

noatak.trend.dt <- noatak.trend.dt[colors.dt, on = 'trend.cat']

noatak.trend.sf <- st_as_sf(noatak.trend.dt, 
                            coords = c("longitude", "latitude"), 
                            crs = 4326)

leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(data = noatak.sf, color = 'white', weight = 3) %>% 
  addCircleMarkers(data = noatak.trend.sf, 
                   color = 'white',
                   weight = 1,
                   opacity = 0.9,
                   fillColor = ~trend.color,
                   fillOpacity = 0.5,
                   radius = ~sqrt(abs(total.change.pcnt))*3) %>%
  setView(-160, 68, zoom = 7) %>%
  addLegend('bottomright', 
            colors = colors.dt$trend.color, 
            labels = colors.dt$trend.cat,
            title = 'NDVImax trend',
            opacity = 1) %>% 
  addScaleBar(options = scaleBarOptions(imperial = F))


# END OF CODE EXAMPLES =========================================================

noatak.trend.map <- leaflet() %>% 
  addProviderTiles('Esri.WorldImagery') %>%
  addPolylines(data = noatak.sf, color = 'white', weight = 3) %>% 
  addCircleMarkers(data = noatak.trend.sf, 
                   color = 'white',
                   weight = 1,
                   opacity = 0.9,
                   fillColor = ~trend.color,
                   fillOpacity = 0.5,
                   radius = ~sqrt(abs(total.change.pcnt))*3) %>%
  setView(-160, 68, zoom = 7) %>%
  addLegend('bottomright', 
            colors = colors.dt$trend.color, 
            labels = colors.dt$trend.cat,
            title = 'NDVImax trend',
            opacity = 1) %>%
  addScaleBar(options = scaleBarOptions(imperial = F))

noatak.trend.map

mapshot(noatak.trend.map, 
        file = 'man/manuscript/figures/figure_8_noatak_trend_map.jpeg', 
        remove_controls = NULL)
