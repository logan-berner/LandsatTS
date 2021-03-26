
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
lsat_get_pixel_centers <- function(polygon_sfc, buffer = 15,
                                   lsat_WRS2_scene_bounds = NULL){
  # Test polygon used for function building
  polygon_sfc <- st_polygon(x = list(matrix(c(69.58630431239703, -138.90079471017262,
                                              69.58352270419293, -138.86266117080206,
                                              69.57759763297376, -138.86730488907006,
                                              69.57739454194817, -138.9068533804639,
                                              69.58630431239703, -138.90079471017262), ncol = 2, byrow = T)[,c(2,1)])) %>%
    st_sfc(crs = 4326)


  # @JAKOB INSERT CODE FROM HiLDEN's "make_landsat8_grid.R" HERE
  #  & adapt for generality!

  ## Preparations
  # confirm rgee is installed and initialized
  tryCatch(ee_user_info(quiet = T), error = function(e) stop("rgee not initialized!\nPlease install and intialize rgee. See: https://r-spatial.github.io/rgee/index.html"))

  # Check arguments
  # confirm polygon_sfc is an sfc
  if(class(polygon_sfc)[1] != "sfc_POLYGON") stop("Invalid argument supplied for polygon_sfc!\nPlease supply an object of type 'sfc_POLYGON'.")
  # confirm buffer is a number
  if(!is.numeric(buffer)) stop("Invalid argument supplied for buffer.\nPlease supplay an object of type 'numeric'.")

  # confirm landsat_wrs2_scene_bounds
  # if not NULL check path
  # if NULL download file from USGS
  if(!is.null(lsat_WRS2_scene_bounds)) {
    if(!is.character(lsat_WRS2_scene_bounds)) stop("Invalid file path for landsat_wrs2_scene_bounds!")
    if(!file.exists(lsat_WRS2_scene_bounds)) stop("landsat_wrs2_scene_bounds file does not exist!")
  } else {
    cat("lsat_WRS2_scene_bounds not specified. Downloading WRS2 scene boundaries from USGS...\n")
    lsat_WRS2_scene_bounds <- tempfile(pattern = "lsat_WRS2_scene_bounds_", fileext = ".kml")
    wrs2_usgs_path <- "https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/WRS-2_bound_world_0.kml"
    download.file(wrs2_usgs_path, lsat_WRS2_scene_bounds)
    if(!file.exists(lsat_WRS2_scene_bounds))stop("Could not download WRS2 boundaries from USGS, please specify file path!")
    warning(paste0("lsat_WRS2_scene_bounds was not specified! WRS2 scene boundaries have been downloaded to temp file.\nConsider downloading and specifying the scene boundaries for future use (see help)."))
    # Note add this to documentation: https://www.usgs.gov/core-science-systems/nli/landsat/landsat-shapefiles-and-kml-files
  }

  # Load WRS2 scene bounds
  ls8_scene_footprints <- read_sf(lsat_WRS2_scene_bounds)

  # Calculate distance to tile-centres
  distance_to_ls8_tile_centres <- st_distance(st_centroid(site_sf_polar),
                                              st_centroid(site_ls8_tiles))
  return(NULL)
}

