# DESCRIPTION ---------------------------------------------------------------------------------
# This R script analyzes time series of annual maximum vegetation greenness 
# for pixels in the study area on Disko Island using Landsat surface reflectance.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-23
# URL: https://github.com/logan-berner/lsatTS
# --------------------------------------------------------------------------------------------

# Clean workspace
rm(list=ls())

# Load required R packages
require(data.table)
require(lsatTS)
require(ggplot2)
require(raster)
require(RStoolbox)

# ANALYZE LANDSAT DATA ------------------------------------------------------------------------

# Read in data files
lsat.gs.dt <- fread('data/lsat_annual_growing_season_summaries.csv')

# Compute temporal trends in NDVImax
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', yrs = 2000:2020)
ggsave('figures/Disko_NDVImax_trends.jpg', width = 6, height = 3, units = 'in', dpi = 400)

# Convert to simple feature and write out shapefile
lsat.trend.sf <- lsat.gs.trend.dt %>% st_as_sf(coords=c('longitude', 'latitude'), crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
lsat.trend.sf <- lsat.trend.sf %>% st_transform(crs = st_crs(img.stk))
st_write(lsat.trend.sf, dsn = 'data/lsat_ndvimax_trends.shp')


## 4) QC: Data visualisation ----
ndvi.fig <- ggplot(lsat.gs.dt, aes(year, ndvi.max)) + 
  ylim(0.25,0.85) + 
  labs(y='Landsat NDVImax', x='Year') + 
  geom_line(data = lsat.gs.dt, 
            mapping = aes(year, ndvi.max, group = sample.id, color = sample.id), 
            alpha = 0.25, 
            size=0.5) + 
  #scale_color_gradientn(colours = rainbow(1000)) +
  theme_bw() +
  theme(legend.position="none", 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=14),
        plot.title=element_text(hjust = 0.5))
ndvi.fig



# Write out data.table with growing season summaries
# fwrite(lsat.gs.dt, 'tests/lsat_TS_test_run/lsat_annual_growing_season_summaries.csv')

# END SCRIPT #--------------------------------------------------------------------------------------------------------


 
# img.stk <- stack('data/disko_s2_crop.tif')
# 
# img.stk <- stack('data/disko_wv_2019_crop.tif')
# img.stk <- raster::subset(img.stk, c(2,3,5))
# img.stk <- raster::shift(img.stk, dx = 100, dy = 30)
# 
# ggRGB(img.stk, r = 3, g = 2, b = 1, stretch = 'lin', ggObj = T, coord_equal = F)
# 
# ggRGB(img.stk, r = 3, g = 2, b = 1, stretch = 'lin', ggObj = T, coord_equal = F) + 
#   geom_sf(data = lsat.trend.sf, aes(color = total.change.pcnt)) + 
#   scale_color_gradient2(low="darkorange4", mid='white', high="darkgreen", limits = c(-50,50), midpoint = 0, name = 'Total change (%)')
# 
# ggsave('figures/Disko_NDVImax_trends_landscape.jpg', width = 6, height = 6, units = 'in', dpi = 400)
# 
# ggRGB(img.stk, r = 5, g = 3, b = 2, stretch = 'lin', ggObj = T)
# 
# ggplot() + geom_sf(data = lsat.trend.sf, aes(color = total.change.pcnt)) + 
#   scale_color_gradient2(low="darkorange4", mid='white', high="darkgreen", limits = c(-50,50), midpoint = 0)
# 
# geom_point(data = lsat.trend.sf, aes(longitude, latitude, color = total.change.pcnt))
# ggplot(pt_buff)  + theme_light() +
#   geom_point(data = pts, aes(x = x, y = y), size = 1) +
#   geom_sf(col = "blue", size = 1.2, fill = "transparent") +