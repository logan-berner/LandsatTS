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
require(googledrive)
require(purrr)
require(R.utils)
setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')

# # DOWNLOAD EXPORTED LANDSAT DATA FROM GOOGLE DRIVE ----------------------------------------------
# folder.url <- "https://drive.google.com/drive/u/0/folders/1m5j1CdLIX8Tb3LgoxUDz-MDgR_tpvA0E"
# folder <- drive_get(as_id(folder.url))
# files <- drive_ls(folder, type = "csv")
# mkdirs('output/lsat_exports/')
# for (i in 1:nrow(files)){
#   drive_download(file = as_id(files$id[i]), path = paste0('output/lsat_exports/',files$name[i]))
# }

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
ggsave('figures/figure 3 disko observation density.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute the Normalized Difference Vegetation Index (NDVI)
lsat.dt <- lsat_calc_spec_index(lsat.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using random forest models and overwrite data in the NDVI column  
lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', doy.rng = 151:239, train.with.highlat.data = T, outdir = 'output/ndvi_xcal_smry/', overwrite.col = T)

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi')
ggsave('figures/figure 5 disko phenological curves.jpg', width = 8, height = 6, units = 'in', dpi = 400)

# Summarize vegetation index for the "growing season", including estimating annual max vegetation index
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.75)

# Evaluate estimates of annual maximum NDVI
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi', min.obs = 5, reps = 10, min.frac.of.max = 0.75)
ggsave('figures/figure 6 Disko NDVImax evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Write out data.table with growing season summaries
fwrite(lsat.gs.dt, 'output/lsat_annual_growing_season_summaries.csv')

# END SCRIPT #--------------------------------------------------------------------------------------------------------