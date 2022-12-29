#' Compute Neighborhood Average Landsat Surface Reflectance
#'
#' @description For each band, this function computes average surface reflectance
#' across neighboring voxels at a sample site. Use this function when working with 
#' Landsat data extracted for buffered points. Also, make sure to have previously 
#' cleaning the individual observations using lsat_clean_data().
#' @param dt A data.table containing coincident surface reflectance measurements 
#'     for multiple Landsat pixels at each sample site.
#' @return A data.table with average surface reflectance
#' @import data.table
#' @export lsat_neighborhood_mean
lsat_neighborhood_mean <- function(dt){
  dt <- data.table::data.table(dt)
  dt[, sample.id := unlist(data.table::transpose(strsplit(sample.id, split = '_'))[[1]])]
  dt <- dt[, .(latitude = mean(latitude, na.rm=T),
               longitude = mean(longitude, na.rm=T),
               blue = mean(blue, na.rm=T),
               green = mean(green, na.rm=T),
               red = mean(red, na.rm=T),
               nir = mean(nir, na.rm=T),
               swir1 = mean(swir1, na.rm=T),
               swir2 = mean(swir2, na.rm=T)),
           by = c('sample.id','year','doy','satellite')]
  dt
}
