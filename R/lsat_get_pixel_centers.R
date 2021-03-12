
#' Landsat 8 pixels centers within a polygon
#'
#' A convenience helper function that determines the Landsat 8 grid (pixel)
#' centers within a polygon and a surrounding buffer.
#'
#' Will fail gracefully for large polygons or polygons extending across multiple
#' Landsat WRS tiles. Errors/warnings will be issues in this case.
#'
#' @param polygon_sf Simple feature collection (or simple feature) with the polygon's geometry.
#' @param buffer Buffer surrounding the geometry to be included. Specified in m.
#'
#' @return sfc of point geometries for Landsat 8 pixel centers within the polygon for use in lsat_download_ts
#' @export lsat_get_pixel_centers
#'
#' @examples
#'
#' my_polgyon <-
#'
lsat_get_pixel_centers <- function(polygon_sf, buffer){
  # @JAKOB INSERT CODE FROM HiLDEN's "make_landsat8_grid.R" HERE
  #  & adapt for generality!
  return(NULL)
}
