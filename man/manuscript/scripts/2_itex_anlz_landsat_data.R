# DESCRIPTION ---------------------------------------------------------------------------------
# This R script generates and analyzes time series of annual maximum vegetation greenness 
# for pixels in the study area on Disko Island using Landsat surface reflectance.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2022-12-05
# URL: https://github.com/logan-berner/LandsatTS
# --------------------------------------------------------------------------------------------

# Load required R packages
require(LandsatTS)
require(data.table)
require(ggplot2)
require(purrr)
require(R.utils)

# Load data set with Landsat data for ITEX sites. Alternatively, load a data set 
# using data.table::fread(). 
data(itex.lsat.dt)

# Format the exported data
lsat.dt <- lsat_format_data(itex.lsat.dt)

# Clean the data by filtering out clouds, snow, and water, as well as radiometric and geometric errors
lsat.dt <- lsat_clean_data(lsat.dt)

# Summarize the availability of Landsat data for each pixel
lsat_summarize_data(lsat.dt)
ggsave('man/manuscript/figures/figure_3_itex_landsat_observations.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute the Normalized Difference Vegetation Index (NDVI)
lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'ndvi')

# Cross-calibrate NDVI among sensors using polynomial regression
lsat.dt <- lsat_calibrate_poly(lsat.dt, 
                             band.or.si = 'ndvi', 
                             train.with.highlat.data = T, 
                             overwrite.col = T)

ggsave('man/manuscript/figures/figure_4_itex_landsat_calibration.jpg', 
       width = 8.0, height = 7.5, units = 'in', dpi = 400)

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi')

ggsave('man/manuscript/figures/figure_5_itex_phenological_curves.jpg', width = 9, height = 7, units = 'in', dpi = 400)

# Summarize growing season characteristics
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi')

# Evaluate estimates of annual maximum NDVI
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi')

ggsave('man/manuscript/figures/figure_6_itex_ndvi_max_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)


# Compute temporal trend in annual NDVImax for each field site
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, 
                                 si = 'ndvi.max', 
                                 yrs = 2000:2021)

lsat.trend.dt


