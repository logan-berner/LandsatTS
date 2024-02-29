# LandsatTS changelog

## 1.2.0 (released: 2024-02-28)
- Fixed error in lsat_fit_phenological_curves() that caused all observations from a focal period to be returned instead of only observations from a specific focal year. Subsequent testing showed strong correlation (mean r = 0.95, n = 100) between NDVImax time series before and after the fix.
- Updated help file for lsat_calibrate_poly() 

## 1.1.1 (released: 2023-04-18)
- Fixed NAMESPACE issues
- Compressed RDA files to reduce package size
- Made more function calls explicit
- Defined a set of global variables

## 1.1.0 (released: 2022-12-07)
- Changed package name from lsatTS to LandsatTS
- Added function lsat_calibrate_poly()
- Added function lsat_plot_trend_hist()
- Revised function lsat_fit_phenological_curves() with improve outlier screening
- Revised function lsat_calc_trend() so it no longer creates figures

## 1.0.1 (released: 2022-6-2)
- Bug fix to address error in lsat_get_pixel_centers() caused by change in the URL for the WRS2 geometries hosted by USGS. See Issue #35: https://github.com/logan-berner/lsatTS/issues/35

## 1.0.0 (released: 2022-4-12)
- First public release of lsatTS.
