#' Calculate spectral indices
#'
#' @description This function computes some widely used spectral vegetation indices.
#' Only one index can be computed at a time. Current indices include the:
#' Normalized Difference Vegetation Index (NDVI; Rouse et al. 1974),
#' kernel NDVI (kNDVI; Camp-Valls et al. 2020),
#' Green NDVI (gNDVI; Gitelson and Merzlyak 1998),
#' Soil Adjusted Vegetation Index (SAVI; Huete 1998),
#' Wide Dynamic Range Vegetation Index (WDRVI; Gitelson 2004),
#' Enhanced Vegetation Index (EVI; Huete et al. 2002),
#' 2-band EVI (EVI2; Jiang et al. 2008),
#' Near Infrared Vegetation Index (NIRv; Badgley et al. 2017),
#' Moisture Stress Index (MSI; Rock et al. 1986),
#' Normalized Difference Water Index (NDWI; McFeeters 1996),
#' Normalized Difference Moisture Index (NDMI; Gao 1996),
#' Normalized Burn Ratio (NBR, Key and Benson 1999),
#' Normalized Difference Infrared Index (NDII; Hardisky et al. 1983),
#' Plant Senescence Reflectance Index (PSRI; Merzlyak et al. 1999),
#' and the Soil-Adjusted Total Vegetation Index (SATVI; Marsett et al. 2006).
#'
#' @param dt Data.table containing surface reflectance data.
#' @param si Character string specifying abbreviation of the desired spectral index.
#'
#' @return The input data.table with an appended column containing the spectral index.
#' @import data.table
#' @export lsat_calc_spec_index
#'
#' @examples
#' data(lsat.example.dt)
#' lsat.dt <- lsat_format_data(lsat.example.dt)
#' lsat.dt <- lsat_clean_data(lsat.dt)
#' lsat.dt <- lsat_calc_spec_index(lsat.dt, 'ndvi')
#' lsat.dt

lsat_calc_spec_index <- function(dt, 
                                 si){
  dt <- data.table::data.table(dt)
  si <- tolower(si)
  avail.si <- c('evi','evi2','gndvi','kndvi','msi','nbr','nirv','ndii',
                'ndmi','ndvi','ndvsi','ndwi','psri','satvi','savi','wdrvi')

  if (si %in% avail.si){
    if (si == 'evi'){dt[, evi := 2.5  * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)]}
    if (si == 'evi2'){dt[, evi2 := 2.5  * (nir - red) / (nir + 2.5 * red + 1)]}
    if (si == 'gndvi'){dt[, gndvi := (nir - green) / (nir + green)]}
    if (si == 'kndvi'){dt[, kndvi := tanh(((nir - red) / (nir + red))^2)]}
    if (si == 'msi'){dt[, msi := swir1 / nir]}
    if (si == 'nbr'){dt[, nbr := (nir - swir2) / (nir + swir2)]}
    if (si == 'ndii'){dt[, ndii := (nir - swir1) / (nir + swir1)]}
    if (si == 'ndmi'){dt[, ndmi := (nir - swir1)/(nir + swir1)]}
    if (si == 'ndvi'){dt[, ndvi := (nir - red) / (nir + red)]}
    if (si == 'ndvsi'){dt[, ndvsi := (swir1 - red) / (swir1 + red)]}
    if (si == 'ndwi'){dt[, ndwi := (green - nir) / (green + nir)]}
    if (si == 'nirv'){dt[, nirv := (nir * (nir - red)) / (nir + red)]}
    if (si == 'psri'){dt[, psri := (red - blue) / nir]}
    if (si == 'satvi'){dt[, satvi := 1.5 * ((swir1 - red) / (swir1 + red + 0.5)) - swir2/2]}
    if (si == 'savi'){dt[, sati := (1.5 * (nir - red)) / (nir + red + 0.5)]}
    if (si == 'wdrvi'){dt[, wdrvi := (0.2*nir - red) / (0.2*nir + red)]}
    dt
  } else {
    print('The requested SI is not currently available...')
    dt
  }
}
