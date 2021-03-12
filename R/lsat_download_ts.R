#' Download Surface Reflectance Time-Series from the whole Landsat record using rgee
#'
#' This function grabs the surface reflectance time series from the whole Landsat
#' record using the Google Earth Engine (account required). These data can then be
#' processed using the remainder of the lsatTS package workflow.
#'
#' Plese note: unlike the other functions in this package, this function does NOT
#' return the time-series as an object, instead these are exported as a CSV to a
#' GEE users' google drive. This export allows for more flexible scheduling and does
#' not require the RSession to continue to run in the background while the requests
#' are processed on the Earth Engine.
#'
#' @param site_coords sfc object containing the site point coordinates for which the time-series should be extracted
#' @param export_dir folder path for the export objects (on google drive)
#'
#' @return Execution status
#' @export lsat_download_ts
#'
#' @examples
#'
#'
lsat_download_ts <- function(site_coords, export_dir){
  # @JAKOB INSERT CODE FROM GEE:/users/HiLDEN/HiLDEN/getLS_by_site HERE!
  # & adapt for rgee use
  return(NULL)
}
