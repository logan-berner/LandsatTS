# LandsatTS changelog

## 1.2.2 (released 2024-04-23)
- lsat_calc_trend(): Added option to calculate trends using linear regression, in addition to the default method that uses Mann-Kendall tests and Theil-Sen slopes.  

## 1.2.1 (released 2024-04-10)
- lsat_summarize_growing_seasons(): Removed z-score based outlier filtering that caused years with one observation to be accidentally removed since a standard deviation can not be computed. Outlier detection and removal is less important in this function now that lsat_fit_phenological_curves() includes enhanced outlier detection and filtering as of v. 1.1.0. 

## 1.2.0 (released: 2024-02-28)
- lsat_fit_phenological_curves(): Fixed error that caused all observations from a focal period to be returned instead of only observations from a specific focal year. Subsequent testing showed strong correlation (mean r = 0.95, n = 100) between NDVImax time series before and after the fix.
- lsat_calibrate_poly(): Updated help file

## 1.1.1 (released: 2023-04-18)
- Fixed NAMESPACE issues
- Compressed RDA files to reduce package size
- Made more function calls explicit
- Defined a set of global variables

## 1.1.0 (released: 2022-12-07)
- Changed package name from lsatTS to LandsatTS
- Added function lsat_calibrate_poly()
- Added function lsat_plot_trend_hist()
- lsat_fit_phenological_curves(): Revised function to improve outlier screening
- lsat_calc_trend(): Revised function so it no longer creates figures

## 1.0.1 (released: 2022-6-2)
- lsat_get_pixel_centers(): Bug fix to address error caused by change in the URL for the WRS2 geometries hosted by USGS. See Issue #35: https://github.com/logan-berner/lsatTS/issues/35

## 1.0.0 (released: 2022-4-12)
- First public release of lsatTS.
