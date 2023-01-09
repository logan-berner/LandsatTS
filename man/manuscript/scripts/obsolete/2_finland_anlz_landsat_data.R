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

# Load data set with Landsat data for finland sites. Alternatively, load a data set 
# using data.table::fread(). 
# data(finland.lsat.dt)
sites.dt <- fread('A:/tmp/finland_site_biomass.csv')
setnames(sites.dt, 'site_id','sample.id')
sites.dt[, sample.id := paste0('S', sample.id)]

finland.lsat.dt <- fread('C:/Users/Logan/My Drive/gee_export/finland_chunk_1.csv')

# Format the exported data
lsat.dt <- lsat_format_data(finland.lsat.dt)

# Clean the data by filtering out clouds, snow, and water, as well as radiometric and geometric errors
lsat.dt <- lsat_clean_data(lsat.dt)

lsat.dt <- lsat_neighborhood_mean(lsat.dt)

# # Summarize the availability of Landsat data for each pixel
# lsat_summarize_data(lsat.dt)
# ggsave('man/manuscript/figures/figure_3_finland_landsat_observations.jpg', width = 6, height = 4, units = 'in', dpi = 400)

# Compute the Normalized Difference Vegetation Index (ndvi)
#lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'ndvi')
lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'nbr')
setnames(lsat.dt, 'nbr', 'ndvi')
# setnames(lsat.dt, 'nirv', 'ndvi')

# Cross-calibrate ndvi among sensors using polynomial regression
lsat.dt <- lsat_calibrate_poly(lsat.dt, 
                             band.or.si = 'ndvi', 
                             train.with.highlat.data = T, 
                             overwrite.col = T)

# ggsave('man/manuscript/figures/figure_4_finland_landsat_calibration.jpg', 
#        width = 8.0, height = 7.5, units = 'in', dpi = 400)

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi', window.min.obs = 15, window.yrs = 5)

# ggsave('man/manuscript/figures/figure_5_finland_phenological_curves.jpg', width = 9, height = 7, units = 'in', dpi = 400)

# Summarize growing season characteristics
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.5)

# # Evaluate estimates of annual maximum ndvi
# lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi')
# 
# ggsave('man/manuscript/figures/figure_6_finland_ndvi_max_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)


# # Compute temporal trend in annual ndvimax for each field site
# lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, 
#                                  si = 'ndvi.max', 
#                                  yr.tolerance = 2,
#                                  yrs = 2000:2021)
# 
# lsat.trend.dt <- setorder(lsat.trend.dt, total.change.pcnt)
# lsat.trend.dt
# 
# fwrite(lsat.trend.dt, 'man/manuscript/output/finland_ndvi_trend_summary.csv')

# # Plots histogram of trends across sites
# lsat_plot_trend_hist(lsat.trend.dt)
# 
# ggsave('man/manuscript/figures/figure_7_finland_ndvi_trend_hist.jpg', width = 6, height = 4, units = 'in', dpi = 400)
# 
# 
# lsat.dt[doy <= 152, season := 'es']
# lsat.dt[doy > 152, season := 'ls']
# lsat.dt <- lsat.dt[, c('doy','satellite','latitude','longitude') := NULL]
# 
# lsat.avg.dt <- lsat.dt[year >= 2020, ]


lsat.gs.2022.dt <- lsat.gs.dt[year == 2022]

sites.dt <- sites.dt[lsat.gs.2022.dt, on = 'sample.id']
sites.dt <- sites.dt[biomass_density_gm2_avg < 3000]

plot(ndvi.max ~ biomass_density_gm2_avg, sites.dt)
plot(ndvi.gs.med ~ biomass_density_gm2_avg, sites.dt)
plot(ndvi.gs.avg ~ biomass_density_gm2_avg, sites.dt)

plot(ndvi.max ~ log10(biomass_density_gm2_avg), sites.dt)
plot(ndvi.gs.med ~ log10(biomass_density_gm2_avg), sites.dt)
plot(ndvi.gs.avg ~ log10(biomass_density_gm2_avg), sites.dt)

cor.test(sites.dt$ndvi.max, log10(sites.dt$biomass_density_gm2_avg))
cor.test(sites.dt$ndvi.gs.avg, log10(sites.dt$biomass_density_gm2_avg))
cor.test(sites.dt$ndvi.gs.med, log10(sites.dt$biomass_density_gm2_avg))

summary(lm(log10(sites.dt$biomass_density_gm2_avg) ~ ndvi.gs.avg, sites.dt))
summary(lm(log10(sites.dt$biomass_density_gm2_avg) ~ ndvi.max, sites.dt))



lsat.dt[satellite == 'LANDSAT_8']
