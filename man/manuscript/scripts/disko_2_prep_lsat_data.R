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

# Set working directory
# setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')
setwd('C:/Users/lb968/My Drive/research/code/lsatTS/man/manuscript/')

# PROCESS LANDSAT DATA ------------------------------------------------------------------------

# Create a list of data files exported from GEE and then read them in to R as a data.table object 
# data.files <- list.files('C:/Users/Logan/My Drive/earth_engine/', full.names = T, pattern = 'disko')
data.files <- list.files('C:/Users/lb968/My Drive/earth_engine/', full.names = T, pattern = 'disko')
lsat.dt <- do.call("rbind", lapply(data.files, fread))

# Format the exported data
lsat.dt <- lsat_general_prep(lsat.dt)

# Clean the data by filtering out clouds, snow, and water, as well as radiometric and geometric errors
lsat.dt <- lsat_clean_data(lsat.dt)

# Summarize the availability of Landsat data for each pixel
lsat_summarize_data_avail(lsat.dt)
ggsave('figures/Disko_observation_density.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute the Normalized Difference Vegetation Index (NDVI)
lsat.dt <- lsat_calc_spec_index(lsat.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using random forest models and overwrite data in the NDVI column  
lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', outdir = 'C:/tmp/test_xcal/', overwrite.col = T)

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi', window.yrs = 5, window.min.obs = 10)

# Summarize vegetation index for the "growing season", including estimating annual max vegetation index
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.75)

# Evaluate estimates of annual maximum NDVI
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi', min.obs = 5, reps = 10, min.frac.of.max = 0.75)
ggsave('figures/Disko_NDVImax_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute temporal trends in NDVImax
lsat.gs.trend.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', yrs = 2000:2020)
ggsave('figures/Disko_NDVImax_trends.jpg', width = 6, height = 3, units = 'in', dpi = 400)


# Write out data.table with growing season summaries
# fwrite(lsat.gs.dt, 'tests/lsat_TS_test_run/lsat_annual_growing_season_summaries.csv')

# END SCRIPT #--------------------------------------------------------------------------------------------------------