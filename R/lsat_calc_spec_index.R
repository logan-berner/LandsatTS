#' Compute Spectral Indices
#' 
#' @description This function computes some widely used spetral indices.
#' These include the Normalized Difference Vegetation Index (NDVI),
#' the Enhanced Vegetationd Index (EVI), and 10 others (EVI2, kNDVI, MSI, NBR, NIRv, NDII, NDVSI, NDWI, PSRI, SATVI).
#' Only one index can be computed at a time.
#'   
#' @param dt Data.table containing surface reflectance data.
#' @param si Character string specifying the desired spetral index.
#' 
#' @return The input data.table with an appended colunm containing the spectral index
#' @export lsat_calc_spec_index
#' 
#' @examples my.dt <- lsat_spec_index(my.dt, 'ndvi')

lsat_calc_spec_index <- function(dt, si){
  require(data.table)
  dt <- data.table(dt)
  si <- tolower(si)
  if (si == 'evi'){dt[, evi := 2.5  * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)]} # Huete et al. (2002) Remote Sensing of Environment
  if (si == 'evi2'){dt[, evi2 := 2.5  * (nir - red) / (nir + 2.5 * red + 1)]} # Jiang et al. (2008) Remote Sensing of Environment
  if (si == 'kndvi'){dt[, kndvi := tanh(((nir - red) / (nir + red))^2)]} # Camp-Valls et al. (2020) Science Advances
  if (si == 'msi'){dt[, msi := swir1 / nir]} # Rock et al. (1986) BioScience
  if (si == 'nbr'){dt[, nbr := (swir1 - swir2) / (swir1 + swir2)]} # Key and Benson (1999) USGS
  if (si == 'nirv'){dt[, nirv := (nir * (nir - red)) / (nir + red)]} # Badgley et al. (2017) Sciences Advances
  if (si == 'ndii'){dt[, ndii := (nir - swir1) / (nir + swir1)]} # Hardisky et al. (1983) Photogram. Engineering and Remote Sensing
  if (si == 'ndvi'){dt[, ndvi := (nir - red) / (nir + red)]} # Rouse et al. (1974) NASA publication  
  if (si == 'ndvsi'){dt[, ndvsi := (swir1 - red) / (swir1 + red)]} # Qi et al (2002) EOS
  if (si == 'ndwi'){dt[, ndwi := (green - nir) / (green + nir)]} # McFeeters (1996) Int. J. Remote Sensing
  if (si == 'psri'){dt[, psri := (red - blue) / nir]} # Merzlyak et al. (1999) Physiologia Plantarum
  if (si == 'satvi'){dt[, satvi := 1.5 * (swir1 - red) / (swir1 + red + 0.5) - swir2/2]} # Marsett et al. (2006) Rangeland Eco. and Management
  dt
}