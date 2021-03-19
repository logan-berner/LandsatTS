# This R script 
# Author: Logan Berner, NAU
# Date: 2021-03-19
rm(list=ls())
require(data.table)
setwd('C:/Users/Logan/Google Drive/research/code/lsatTS/')

# source R functions in lsatTS package
lsatTS.fun.files = list.files('C:/Users/Logan/Google Drive/research/code/lsatTS/R/', full.names = T)
sapply(lsatTS.fun.files, source)

# Read in and combine files containing Landsat data extracted from GEE using lsat_download_ts()
lsat.extract.files <- list.files('tests/lsat_TS_test_run/lsat_extract/', full.names = T)
lsat.dt <- do.call("rbind", lapply(lsat.extract.files, fread))
setnames(lsat.dt, 'pixel_id','site') # all lsatTS function depend on there being a column called "site" that uniquely identifies each location

# Parse data, filter to clear-sky observations, compute mean surface reflectance among pxls w/in each window around a site
lsat.dt <- lsat_general_prep(lsat.dt)

# Clean the data, filtering out clouds, snow, water, radiometric and geometric errors
lsat.dt <- lsat_clean_data(lsat.dt, geom.max = 15, cloud.max = 80, sza.max = 60, filter.snow = T, filter.water = T)

# Optional: 
# lsat.dt <- lsat_ngb_mean(lsat.dt)

# Compute NDVI
lsat.dt <- lsat_calc_spec_index(lsat.dt, 'ndvi')

# Cross-calibrate NDVI among sensors using RF models
lsat.dt <- lsat_calibrate_rf(lsat.dt, band = 'ndvi', doy.rng = 151:242, min.obs = 2, frac.train = 0.80, outdir = 'tests/lsat_TS_test_run/ndvi_xcal/')

# Drop column with uncalibrated data and remain column with calibrated data
lsat.dt[, c('ndvi') := NULL]
setnames(lsat.dt, 'ndvi.xcal', 'ndvi')

# Optional: Summarize availability of Landsat data by site
data.summary.dt <- lsat_summarize_data_avail(lsat.dt)
data.summary.dt

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, vi = 'ndvi', window.yrs = 5, window.min.obs = 10, vi.min = 0, spl.fit.outfile = F, progress = T)

# Summarize vegetation index for the "growing season", including estimating annual max vegetation index
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, vi = 'ndvi', min.frac.of.max = 0.75)

# Optional: Evaluate how raw and phenologically-modeled estimates of annual maximum VI are affected by the number of available observations 
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, vi = 'ndvi', min.obs = 6, reps = 20, min.frac.of.max = 0.75, outdir = 'tests/lsat_TS_test_run/max_eval/')

# Write out data.table with growing season summaries
# fwrite(lsat.gs.dt, 'tests/lsat_TS_test_run/lsat_annual_growing_season_summaries.csv')

# END SCRIPT #--------------------------------------------------------------------------------------------------------