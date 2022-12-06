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

# Cross-calibrate NDVI among sensors using polynomial regression and overwrite data in the NDVI column  
lsat.dt <- lsat_calibrate_poly(lsat.dt, 
                             band.or.si = 'ndvi', 
                             train.with.highlat.data = T, 
                             overwrite.col = T,
                             write.output = T,
                             outdir = 'man/manuscript/output/ndvi_xcal_smry/')

ggsave('man/manuscript/figures/figure_4_itex_landsat_calibration.jpg', 
       width = 8.0, height = 7.5, units = 'in', dpi = 400)


# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, 
                                              si = 'ndvi', 
                                              spar = 0.70)

ggsave('man/manuscript/figures/figure_5_itex_phenological_curves.jpg', width = 9, height = 7, units = 'in', dpi = 400)

# Summarize vegetation index for the "growing season", including estimating annual max vegetation index
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.75)

# Evaluate estimates of annual maximum NDVI
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, 
                                                  si = 'ndvi', 
                                                  min.obs = 5, 
                                                  reps = 2, 
                                                  min.frac.of.max = 0.75)

ggsave('figures/figure_6_itex_ndvi_max_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute temporal trends in NDVImax
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, 
                                 si = 'ndvi.max', 
                                 yrs = 2000:2021, 
                                 legend.position = c(0.66,0.93))

ggsave('figures/figure_7_itex_ndvi_max_trend_distribution.jpg', width = 6, height = 8, units = 'in', dpi = 400)

# Convert trend data table to simple feature and write out shapefile
lsat.trend.sf <- lsat.trend.dt %>% st_as_sf(coords=c('longitude', 'latitude'), crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
lsat.trend.sf <- lsat.trend.sf %>% st_transform(crs = 3413)
st_write(lsat.trend.sf, dsn = 'data/lsat_ndvimax_trends.shp')


smry <- lsat_summarize_data(lsat.dt)
mean(smry$n.obs.tot)
sd(smry$n.obs.tot)

mean(smry$n.yrs.with.obs)
sd(smry$n.yrs.with.obs)

mean(smry$first.yr)
sd(smry$first.yr)
