
#' Landsat 8 pixels centers within a polygon
#'
#' A convenience helper function that determines the Landsat 8 grid (pixel)
#' centers within a polygon and a surrounding buffer.
#'
#' Does not work for large polygons. The default maximum number of pixels set by the GEE is 10000000.
#' Consider whether extractions for a large polygon is a good idea.
#' Split the polygon into manageable chuncks. For the unlikely case that a polygon
#' exceeds the boundaries of the Landsat tile closest to the polygon's centre,
#' the polygon is clipped ot the boundaries of the Landsat tile and a warning is issued.
#' Again, consider processing smaller polygons.
#'
#'
#' @param polygon_sfc Polygon geometry of the sfc_POLYGON.
#' @param buffer Buffer surrounding the geometry to be included. Specified in m.
#' @param pixel_prefix Optional prefix for the generated pixel ids. Defaults to "pixel".
#' @param plot_map Optional. If TRUE the retrieved pixel centers and the polygon are plotted on a mid-season Landsat 8 image (grey-scale red band) in the mapview. If a character is supplied an addtional output to a file is generated (png, pdf, and jpg supported, see mapview::mapshot). Both slow down the execution of this funciton dramatically, especially for large polygons.
#' @param lsat_WRS2_scene_bounds File path to the Landsat WRS2 path row scene boundaries. If not specified these are downloaded to a temporary file. To speed up this function consider downloading the file manually and specifiying the file path in this argument. The file can be found here: https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/WRS-2_bound_world_0.kml
#'
#' @return sfc of point geometries for Landsat 8 pixel centers within the polygon for use in lsat_download_ts
#' @export lsat_get_pixel_centers
#'
#' @examples
#'
#' # Specify a region to retrieve pixel centers for
#' test_poly <- st_polygon(
#' list(matrix(c(69.58413326844578, -138.90125985326782,
#'               69.58358009075835, -138.88988547014793,
#'               69.5809560122973, -138.89147182732913,
#'               69.57986505201093, -138.90298010997816,
#'               69.58413326844578, -138.90125985326782),
#'             ncol = 2, byrow = T)[,c(2,1)]))
#' test_poly2_sfc <- st_sfc(test_poly2, crs = 4326)
#'
#' # Retrieve pixel centers and plot to mapview
#' pixels <- lsat_get_pixel_centers(test_poly2_sfc, plot_map = T)
#'
lsat_get_pixel_centers <- function(polygon_sfc,
                                   buffer = 15,
                                   region_name = "pixel",
                                   plot_map = F,
                                   lsat_WRS2_scene_bounds = NULL){
  ## Preparations

  # confirm rgee is initialized
  tryCatch(ee_user_info(quiet = T), error = function(e) stop("rgee not initialized!\nPlease install and intialize rgee. See: https://r-spatial.github.io/rgee/index.html"))

  # Check function arguments
  # confirm polygon_sfc is an sfc
  if(class(polygon_sfc)[1] != "sfc_POLYGON") stop("Invalid argument supplied for polygon_sfc!\nPlease supply an object of type 'sfc_POLYGON'.")
  # confirm buffer is a number
  if(!is.numeric(buffer)) stop("Invalid argument supplied for buffer.\nPlease supplay an object of type 'numeric'.")

  # Confirm landsat_wrs2_scene_bounds
  # if not NULL check path
  # if NULL download file from USGS
  if(!is.null(lsat_WRS2_scene_bounds)) {
    if(!is.character(lsat_WRS2_scene_bounds)) stop("Invalid file path for landsat_wrs2_scene_bounds!")
    if(!file.exists(lsat_WRS2_scene_bounds)) stop("landsat_wrs2_scene_bounds file does not exist!")
  } else {
    cat(paste0("Argument 'lsat_WRS2_scene_bounds' was not specified!\n",
        "Downloading WRS2 scene boundaries from USGS...\n"))
    lsat_WRS2_scene_bounds <- tempfile(pattern = "lsat_WRS2_scene_bounds_", fileext = ".kml")
    wrs2_usgs_path <- "https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/WRS-2_bound_world_0.kml"
    download.file(wrs2_usgs_path, lsat_WRS2_scene_bounds)
    if(!file.exists(lsat_WRS2_scene_bounds))stop("Could not download WRS2 boundaries from USGS, please specify file path!")
    warning(paste0("Argument 'lsat_WRS2_scene_bounds' was not specified! WRS2 scene boundaries were downloaded to a temp file. Consider downloading the scene boundaries for future use (see help)."))
    # Note add this to documentation: https://www.usgs.gov/core-science-systems/nli/landsat/landsat-shapefiles-and-kml-files
  }

  # Load WRS2 scene bounds
  lsat_scene_footprints <- read_sf(lsat_WRS2_scene_bounds)

  # Status
  cat(paste0("Determining Landsat WRS tile closest to the polygon centre...\n"))

  # Identify overlapping tiles with polygon
  lsat_overlapping_tiles <- lsat_scene_footprints[st_intersects(polygon_sfc, lsat_scene_footprints, quiet = T)[[1]],]

  # Identify EPSG for UTM zone of centroid (southern hemisphere landsat tiles use northern hemispher UTM )
  polygon_centroid <- suppressWarnings(st_centroid(polygon_sfc, silent = T))
  polygon_centroid_utm_crs <- floor((st_coordinates(polygon_centroid)[1] + 180) / 6) + 1 + 32600

  # Transform geometries to local UTM
  polygon_sfc_utm <-  st_transform(polygon_sfc, crs = polygon_centroid_utm_crs)
  lsat_overlapping_tiles_utm <- st_transform(lsat_overlapping_tiles, crs = polygon_centroid_utm_crs)

  # Identify closest landsat tile to site centroid
  distance_to_tiles <- suppressWarnings(
    st_distance(st_centroid(polygon_sfc_utm),
                st_centroid(lsat_overlapping_tiles_utm)))
  min_distance_tile_index <- which(distance_to_tiles == min(distance_to_tiles))
  wrs_tile_id <- lsat_overlapping_tiles_utm[min_distance_tile_index,]$Name

  # Split wrs_tile_id into path and row
  wrs_path <- as.numeric(gsub("([0-9]*)_[0-9]*", "\\1", wrs_tile_id))
  wrs_row <- as.numeric(gsub("[0-9]*_([0-9]*)", "\\1", wrs_tile_id))

  # Access landsat8 image collection
  ls8IC <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")

  # Retrieve tile crs from GEE
  lsat_tile_crs <- ls8IC$
    filterMetadata("WRS_PATH", "equals", as.integer(wrs_path))$
    filterMetadata("WRS_ROW", 'equals', as.integer(wrs_row))$
    first()$
    projection()$
    crs()$
    getInfo() %>%
    gsub("EPSG:(.*)", "\\1", .) %>%
    as.numeric()

  # Compare with previously determined crs and if different change projection of polygon
  if(polygon_centroid_utm_crs != lsat_tile_crs) polygon_sfc_utm <-  st_transform(polygon_sfc, crs = lsat_tile_crs)

  # Check whether polygon is fully covered by tile
  is_covered <- as.numeric(st_covers(lsat_overlapping_tiles_utm[min_distance_tile_index,], polygon_sfc_utm))
  if(is.na(is_covered)){
    warning("Polygon exceeds boundaries of closest Landsat WRS tile!\n",
            "Clipping polygon to footprint of Landsat WRS tile ", wrs_tile_id, ". ",
            "Any regions outwidth the tile will be ignored. ",
            "Consider splliting polygon into smaller chuncks!")
    polygon_sfc_utm <- st_intersection(polygon_sfc_utm, lsat_overlapping_tiles_utm[min_distance_tile_index,])
  }

  # Status
  cat(paste0("Retrieving pixel centres based on WRS tile '", wrs_tile_id, "'...\n"))

  # Add buffer to sf if specified and transform to lat long
  polygon_sfc_buffered <- polygon_sfc_utm %>% st_buffer(buffer) %>% st_transform(4326)

  # Retrieve first landsat 8 tile for summer 2019 from GEE
  ls8_image <- ls8IC$
    filterMetadata("WRS_PATH", "equals", as.integer(wrs_path))$
    filterMetadata("WRS_ROW", 'equals', as.integer(wrs_row))$
    filterDate("2019-06-01", "2019-09-30")$
    first()

  # Extract pixel coordinates
  ls8_pixels <- ls8_image$pixelLonLat()$
    select(c("longitude", "latitude"))$
    reduceRegion(reducer = ee$Reducer$toList(),
                 geometry = sf_as_ee(polygon_sfc_buffered),
                 crs = ls8_image$projection(),
                 scale = 30L)

  # Conver to sf
  ls8_pixels_sf <- data.frame(
    latitude = ls8_pixels$getInfo()$latitude,
    longitude = ls8_pixels$getInfo()$longitude) %>%
    st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326)

  # Add pixel and site ID
  ls8_pixels_sf$pixel_id <- paste0(region_name, "_",
                                   1:nrow(ls8_pixels_sf))

  # Visusalise grid using the mapview if requested
  if(plot_map == T | is.character(plot_map)){
    # Status update
    cat("Plotting grid...\n")
    # make map
    region_map <- Map$addLayer(ls8_image$select("B4")) +
      Map$addLayer(sf_as_ee(polygon_sfc_utm), list(color = "darkred")) +
      Map$addLayer(sf_as_ee(ls8_pixels_sf), list(color = "black"))
    # center view
    Map$centerObject(sf_as_ee(polygon_sfc_utm))

    # display map
    print(region_map)

    # Export to snapshot to file if requested
    if(is.character(plot_map)){
      try(
        mapshot(
          region_map,
          file = plot_map,
          remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar",
                              "drawToolbar", "easyButton")))
    }
  }

  # Status
  cat("Done.\n")
  # Return pixel centers as sfc
  return(ls8_pixels_sf)
}
