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

# Compute the Normalized Difference Vegetation Index (ndvi)
lsat.dt <- lsat_calc_spectral_index(lsat.dt, si = 'ndvi')

# Cross-calibrate ndvi among sensors using polynomial regression
lsat.dt <- lsat_calibrate_poly(lsat.dt, 
                             band.or.si = 'ndvi', 
                             train.with.highlat.data = T, 
                             overwrite.col = T)

ggsave('man/manuscript/figures/figure_4_itex_landsat_calibration.jpg', 
       width = 8.0, height = 7.5, units = 'in', dpi = 400)

# Fit phenological models (cubic splines) to each time series
lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi', window.min.obs = 15, window.yrs = 11)

ggsave('man/manuscript/figures/figure_5_itex_phenological_curves.jpg', width = 9, height = 7, units = 'in', dpi = 400)

# Summarize growing season characteristics
lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi', min.frac.of.max = 0.5)

# Evaluate estimates of annual maximum ndvi
lsat.gs.eval.dt <- lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi')

ggsave('man/manuscript/figures/figure_6_itex_ndvi_max_evaluation.jpg', width = 6, height = 4, units = 'in', dpi = 400)


# Compute temporal trend in annual ndvimax for each field site
lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, 
                                 si = 'ndvi.max', 
                                 yr.tolerance = 2,
                                 yrs = 2000:2021)

lsat.trend.dt <- setorder(lsat.trend.dt, total.change.pcnt)
lsat.trend.dt

fwrite(lsat.trend.dt, 'man/manuscript/output/itex_ndvi_trend_summary.csv')

# Plots histogram of trends across sites
lsat_plot_trend_hist(lsat.trend.dt)

ggsave('man/manuscript/figures/figure_7_itex_ndvi_trend_hist.jpg', width = 6, height = 4, units = 'in', dpi = 400)


# sites 
cleaned.sites <- unique(lsat.dt$sample.id)
pheno.sites <- unique(lsat.pheno.dt$sample.id)
gs.sites <- unique(lsat.gs.dt$sample.id)
trend.sites <- unique(lsat.trend.dt$sample.id)

length(cleaned.sites) # cleaned
length(pheno.sites)
length(gs.sites)
length(trend.sites)

# sites dropped when modeling phenology  
dropped.pheno.sites <- cleaned.sites[!(cleaned.sites %in% pheno.sites)]
lsat.dt[sample.id %in% dropped.pheno.sites]
lsat.dt[sample.id %in% dropped.pheno.sites, .N, by = sample.id]

lsat.dt[sample.id == 'Val Bercla']
# sites dropped when computing trends
dropped.trend.sites <- pheno.sites[!(pheno.sites %in% trend.sites)]
lsat.dt[sample.id %in% dropped.trend.sites]
lsat.dt[sample.id %in% dropped.trend.sites, .N, by = sample.id]

lsat.dt[sample.id == 'Adventdalen']
lsat.dt[sample.id == 'Tanquary Fiord']
lsat.dt[sample.id == 'Val Bercla']

mean(lsat.dt[sample.id == 'Adventdalen']$ndvi)
mean(lsat.dt[sample.id == 'Tanquary Fiord']$ndvi)
mean(lsat.dt[sample.id == 'Val Bercla']$ndvi)

fivenum(lsat.trend.dt$total.change.pcnt)

quantile(lsat.trend.dt$total.change.pcnt, 0.5)
quantile(lsat.trend.dt$total.change.pcnt, 0.025)
quantile(lsat.trend.dt$total.change.pcnt, 0.975)

setorder(lsat.trend.dt, total.change.pcnt)
lsat.trend.dt


plot(ndvi.max ~ year, lsat.gs.dt[sample.id == 'Niwot Ridge'])
