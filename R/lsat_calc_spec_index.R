#' Calculate Spectral Indices
#'
#' @description This function computes some widely used spetral indices.
#' Only one index can be computed at a time. Current indices include the:
#' Normalized Difference Vegetation Index (NDVI; Rouse et al. 1974),
#' kernel NDVI (kNDVI; Camp-Valls et al. 2020)
#' Enhanced Vegetationd Index (EVI; Huete et al. 2002),
#' 2-band EVI (EVI2; Jiang et al. 2008), 
#' Near Infrared Vegetation Index (NIRv; Badgley et al. 2017),
#' Moisture Stress Index (MSI; Rock et al. 1986),
#' Normalized Burn Ratio (NBR, Key and Benson 1999),
#' Normalized Difference Infrared Index (NDII; Hardisky et al. 1983)
#' Normalized Difference Water Index (NDWI; McFeeters 1996),
#' Plant Senescence Reflectance Index (PSRI; Merzlyak et al. 1999),
#' and the Soil-adjusted Total Vegetation Index (SATVI; Marsett et al. 2006). 
#' 
#' @param dt Data.table containing surface reflectance data.
#' @param si Character string specifying abbreviation of the desired spetral index.
#'
#' @return The input data.table with an appended colunm containing the spectral index
#' @import data.table
#' @export lsat_calc_spec_index
#'
#' @examples # my.dt <- lsat_calc_spec_index(my.dt, 'ndvi')

lsat_calc_spec_index <- function(dt, si){
  dt <- data.table::data.table(dt)
  si <- tolower(si)
  if (si == 'evi'){dt[, evi := 2.5  * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)]}
  if (si == 'evi2'){dt[, evi2 := 2.5  * (nir - red) / (nir + 2.5 * red + 1)]}
  if (si == 'kndvi'){dt[, kndvi := tanh(((nir - red) / (nir + red))^2)]}
  if (si == 'msi'){dt[, msi := swir1 / nir]}
  if (si == 'nbr'){dt[, nbr := (swir1 - swir2) / (swir1 + swir2)]} 
  if (si == 'nirv'){dt[, nirv := (nir * (nir - red)) / (nir + red)]}
  if (si == 'ndii'){dt[, ndii := (nir - swir1) / (nir + swir1)]}
  if (si == 'ndvi'){dt[, ndvi := (nir - red) / (nir + red)]}
  if (si == 'ndwi'){dt[, ndwi := (green - nir) / (green + nir)]}
  if (si == 'psri'){dt[, psri := (red - blue) / nir]}
  if (si == 'satvi'){dt[, satvi := 1.5 * (swir1 - red) / (swir1 + red + 0.5) - swir2/2]}
  dt
}
