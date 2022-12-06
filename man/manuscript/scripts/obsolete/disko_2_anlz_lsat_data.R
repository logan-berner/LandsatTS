# DESCRIPTION ---------------------------------------------------------------------------------
# This R script generates and analyzes time series of annual maximum vegetation greenness 
# for pixels in the study area on Disko Island using Landsat surface reflectance.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-18
# URL: https://github.com/logan-berner/lsatTS
# --------------------------------------------------------------------------------------------

# Clean workspace
rm(list=ls())

# Load required R packages
require(data.table)
require(lsatTS)
require(ggplot2)
require(purrr)
require(R.utils)

setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')

# PROCESS LANDSAT DATA ------------------------------------------------------------------------

# Create a list of data files exported from GEE and then read them in to R as a data.table object 
data.files <- list.files('output/lsat_exports/', full.names = T, pattern = 'disko')
lsat.dt <- do.call("rbind", lapply(data.files, fread))

# Format the exported data
lsat.dt <- lsat_general_prep(lsat.dt)

# Clean the data by filtering out clouds, snow, and water, as well as radiometric and geometric errors
lsat.dt <- lsat_clean_data(lsat.dt)

# Summarize the availability of Landsat data for each pixel
lsat_summarize_data_avail(lsat.dt)
ggsave('figures/figure_3_disko_observation_density.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute the Normalized Difference Vegetation Index (NDVI)
lsat.dt <- lsat_calc_spec_index(lsat.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using random forest models and overwrite data in the NDVI column  
lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', doy.rng = 151:239, train.with.highlat.data = T, outdir = 'output/ndvi_xcal_smry/', overwrite.col = T)

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi', test.run = F)
ggsave('figures/figure_5_disko_phenological_curves.jpg', width = 9, height = 7, units = 'in', dpi = 400)

# Summarize vegetation index for the "growing season", including estimating annual max vegetation index
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.75)

# Evaluate estimates of annual maximum NDVI
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi', min.obs = 5, reps = 2, min.frac.of.max = 0.75)
ggsave('figures/figure_6_disko_ndvi_max_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Write out data.table with growing season summaries
fwrite(lsat.gs.dt, 'output/lsat_annual_growing_season_summaries.csv')

# Compute temporal trends in NDVImax
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', yrs = 2000:2020, legend.position = c(0.66,0.93))
ggsave('figures/figure_7_disko_ndvi_max_trend_distribution.jpg', width = 6, height = 8, units = 'in', dpi = 400)

# Convert trend data table to simple feature and write out shapefile
lsat.trend.sf <- lsat.trend.dt %>% st_as_sf(coords=c('longitude', 'latitude'), crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
lsat.trend.sf <- lsat.trend.sf %>% st_transform(crs = 3413)
st_write(lsat.trend.sf, dsn = 'data/lsat_ndvimax_trends.shp')

# END SCRIPT #--------------------------------------------------------------------------------------------------------