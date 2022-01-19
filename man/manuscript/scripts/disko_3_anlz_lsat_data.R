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
require(sf)
setwd('C:/Users/lb968/My Drive/research/code/lsatTS/man/manuscript/')


# ANALYZE LANDSAT DATA ------------------------------------------------------------------------

# Read in data files
lsat.gs.dt <- fread('output/lsat_annual_growing_season_summaries.csv')
lsat.gs.dt <- lsat.gs.dt[year >= 2000]

# Compute temporal trends in NDVImax
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', yrs = 2000:2020, legend.position = c(0.65,0.9))
ggsave('figures/figure 7 disko ndvi max trend distribution.jpg', width = 6, height = 8, units = 'in', dpi = 400)
# ggsave('figures/figure 7 disko ndvi max trend distribution.jpg', width = 6, height = 3, units = 'in', dpi = 400)

# Convert to simple feature and write out shapefile
lsat.trend.sf <- lsat.trend.dt %>% st_as_sf(coords=c('longitude', 'latitude'), crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
lsat.trend.sf <- lsat.trend.sf %>% st_transform(crs = 3413)
st_write(lsat.trend.sf, dsn = 'data/lsat_ndvimax_trends.shp')

# Create time series figure for each trend class
lsat.gs.dt <- lsat.gs.dt[lsat.trend.dt, on = 'sample.id']

lsat.trend.cls.yrly.dt <- lsat.gs.dt[, .(ndvi.max.avg = mean(ndvi.max,na.rm = T), ndvi.max.sd = sd(ndvi.max, na.rm = T),
                                         n = .N), by = c('trend.cat','year')]

lsat.trend.cls.yrly.dt[, ndvi.max.se := ndvi.max.sd/sqrt(n)]


lsat.trend.cls.yrly.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'), 
                                             labels = c('browning','no trend','greening'))]
trend.cols <- c('darkgoldenrod4','ivory3','darkgreen')
ggplot(lsat.trend.cls.yrly.dt[year != 2003], aes(year, ndvi.max.avg, group = trend.cat, color = trend.cat)) + 
  ylim(0.40,0.65) + 
  labs(y='Landsat NDVImax', x='Year') + 
  geom_ribbon(aes(ymin=ndvi.max.avg-ndvi.max.se,ymax=ndvi.max.avg+ndvi.max.se, fill=trend.cat),alpha=0.3, linetype=0)+
  geom_line(aes(color = trend.cat), alpha = 1, size=1) + 
  scale_fill_manual(values = trend.cols, name = 'Trend class') + 
  scale_color_manual(values = trend.cols, name = 'Trend class')+
  theme_bw() +
  theme(legend.position=c(0.8, 0.2), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=14),
        plot.title=element_text(hjust = 0.5))

ggsave('figures/figure 8 disko ndvi max time series.jpg', width = 6, height = 4, units = 'in', dpi = 400)


# END SCRIPT #--------------------------------------------------------------------------------------------------------