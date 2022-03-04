#' Get Landsat 8 pixels centers within a polygon or around a point coordinate
#' with a buffer
#'
#' A convenience helper function that determines the Landsat 8 grid (pixel)
#' centers within a polygon and an optional buffer. It can also be applied to
#' a single point to retrieve all pixels within a surrounding buffer.
#'
#' Does not work for large polygons. The default maximum number of pixels set by
#' the GEE is 10000000. Consider whether extraction for a large polygon is a
#' good idea, if yes split the polygon into manageable chunks.
#'
#' For the unlikely case that a polygon exceeds the boundaries of the Landsat
#' tile closest to the polygon's center, the polygon is clipped at the
#' boundaries of the Landsat tile and a warning is issued. Again, if this is the
#' case, consider processing smaller polygons instead.
#'
#' Please note that the approximation of tile overlap with polygon generates a
#' warning by sf that the coordinates are assumed to be planar. This can be
#' ignored.
#'
#' @param polygon_sf Simple feature with a simple feature collection of type
#'  "sfc_POLYGON" containing a single polygon geometry. Alternatively, a simple
#'  feature containing a simple feature collection of type 'sfc_POINT' with a
#'  single point.
#' @param pixel_prefix Prefix for the generated pixel ids. Defaults to
#'  "pixel".
#' @param pixel_prefix_from Optional, column name in simple feature to specify
#'  pixel_prefix. Overrides "pixel_prefix" argument.
#' @param buffer Buffer surrounding the geometry to be included. Specified in m.
#'   Defaults to 15 m, the nominal Landsat pixel size.
#' @param plot_map Optional, default is FALSE. If TRUE the retrieved pixel
#'  centers and the polygon are plotted on a summer Landsat 8 image
#'  (grey-scale red band) using mapview. If a character is supplied an
#'  additional output to a file is generated (png, pdf, and jpg supported, see
#'  mapview::mapshot). Note: Both slow down the execution of this function
#'  dramatically, especially for large polygons. Only useful in interactive
#'  sessions.
#' @param lsat_WRS2_scene_bounds File path to the Landsat WRS2 path row scene
#'   boundaries. If not specified the boundaries are downloaded to a temporary
#'   file when the function is executed the first time during a session. To
#'   avoid future downloads, the file may be downloaded manually and it's file
#'   pathe specified using this argument.
#'   The file can be found here:
#'   https://prd-wret.s3.us-west-2.amazonaws.com/assets/palladium/production/atoms/files/WRS-2_bound_world_0.kml
#'   See also:
#'   https://www.usgs.gov/core-science-systems/nli/landsat/landsat-shapefiles-and-kml-files
#'
#' @return sfc of point geometries for Landsat 8 pixel centers within the
#' polygon or the buffer around the point coordinate. For use in
#' lsat_export_ts().
#'
#' @author Jakob J. Assmann
#'
#' @export lsat_get_pixel_centers
#'
#' @examples
#' # Using sf, dplyr, rgee and purr
#' library(sf)
#' library(dplyr)
#' library(rgee)
#' library(purrr)
#'
#' # Initialize EE
#' ee_Initialize()
#'
#' # Specify a region to retrieve pixel centers for
#' test_poly_sf <- list(matrix(c(-138.90125, 69.58413,
#'               -138.88988, 69.58358,
#'               -138.89147, 69.58095,
#'               -138.90298, 69.57986,
#'               -138.90125, 69.58413),
#'             ncol = 2, byrow = TRUE)) %>%
#'            st_polygon() %>%
#'            st_sfc(crs = 4326) %>%
#'            st_sf()
#'
#' # Retrieve pixel centers and plot to mapview
#' pixels <- lsat_get_pixel_centers(test_poly_sf, plot_map = TRUE)
#'
#'
#' ## Ge pixel centers for multiple regions
#' # Create multi-polygon sf
#' ellesmere <- st_polygon(list(matrix(c(-75.78526, 78.86973,
#'                                       -75.78526, 78.87246,
#'                                       -75.77116, 78.87246,
#'                                       -75.77116, 78.86973,
#'                                       -75.78526, 78.86973),
#'                                       ncol = 2, byrow = TRUE)))
#' zackenberg <- st_polygon(list(matrix(c(-20.56254, 74.47469,
#'                                     -20.56254, 74.47740,
#'                                     -20.55242, 74.47740,
#'                                     -20.55242, 74.47469,
#'                                     -20.56254, 74.47469),
#'                                   ncol = 2, byrow = TRUE)))
#' toolik <- st_polygon(list(matrix(c(-149.60686, 68.62364,
#'                                    -149.60686, 68.62644,
#'                                    -149.59918, 68.62644,
#'                                    -149.59918, 68.62364,
#'                                    -149.60686, 68.62364),
#'                                    ncol = 2, byrow = TRUE)))
#' test_regions_sf <- st_sfc(ellesmere, zackenberg, toolik, crs = 4326) %>%
#'   st_sf() %>%
#'   mutate(region = c("ellesmere", "zackenberg", "toolik"))
#'
#' # Split and map lsat_get_pixel_centers using dplyr and purrr
#' pixel_list <- test_regions_sf %>%
#'    split(.$region) %>%
#'    map(lsat_get_pixel_centers,
#'        pixel_prefix_from = "region") %>%
#'    bind_rows()
#'
lsat_get_pixel_centers <- function(polygon_sf,
                                   pixel_prefix = "pixel",
                                   pixel_prefix_from = NULL,
                                   buffer = 15,
                                   plot_map = F,
                                   lsat_WRS2_scene_bounds = NULL){
  ## Preparations

  # confirm rgee is initialized
  tryCatch(rgee::ee_user_info(quiet = T), error = function(e){
    stop("rgee not initialized!\nPlease intialize rgee. ",
         "See: https://r-spatial.github.io/rgee/index.html")
    })

  # Turn off use of S2 in sf package if version is > sf 1.0
  try(sf::sf_use_s2(FALSE))

  # Check function arguments
  # confirm polygon_sf is sfc_POLYGON or sfc_POINT
  if(!("sfc_POINT" %in% class(sf::st_geometry(polygon_sf)) |
       ("sfc_POLYGON" %in% class(sf::st_geometry(polygon_sf))) |
     (length(sf::st_geometry(polygon_sf)) != 1))) {
    stop("Invalid argument supplied for polygon_sf!\n",
         "Please supply an object with 'sfc_POLYGON' or 'sfc_POINT' geometries.")
    }
  # confirm pixel prefix is a valid character
  if(!is.character(pixel_prefix)) {
    stop("Invalid argument supplied for pixel_prefix, please supplly a",
         "character or do not specify.")
    }
  # confirm whether pixel_prefix_from was specified and if assign
  if(!is.null(pixel_prefix_from)){
    if(pixel_prefix_from %in% colnames(polygon_sf)) pixel_prefix <-
        sf::st_drop_geometry(polygon_sf)[, pixel_prefix_from][1]
    else stop("Invalid column name specified for pixel_prefix_from.")
  }
  # confirm buffer is a number
  if(!is.numeric(buffer)) stop("Invalid argument supplied for buffer.\n",
                               "Please supply an object of type 'numeric'.")

  # Confirm landsat_wrs2_scene_bounds
  # if not NULL check path
  # if NULL download file from USGS
  if(!is.null(lsat_WRS2_scene_bounds)) {
    if(!is.character(lsat_WRS2_scene_bounds)) {
      stop("file path for landsat_wrs2_scene_bounds is not a character vector!")
    }
    if(!file.exists(lsat_WRS2_scene_bounds)){
      stop("landsat_wrs2_scene_bounds file does not exist!")
      }
  } else {
    # Status update
    cat("Argument 'lsat_WRS2_scene_bounds' was not specified!\n")

    # Check whether file was downloaded previously in current session,
    # if not download.
    lsat_WRS2_scene_bounds <- R.utils::getOption("lsat_WRS2_scene_bounds")
    if(is.null(lsat_WRS2_scene_bounds)){
      cat("Downloading WRS2 scene boundaries from USGS...",
          "(required only once per session)\n")
      lsat_WRS2_scene_bounds <- tempfile(pattern = "lsat_WRS2_scene_bounds_",
                                         fileext = ".kml")
      R.utils::setOption("lsat_WRS2_scene_bounds", lsat_WRS2_scene_bounds)
      wrs2_usgs_path <- paste0("https://prd-wret.s3.us-west-2.amazonaws.com/",
                               "assets/palladium/production/atoms/files/",
                               "WRS-2_bound_world_0.kml")
      utils::download.file(wrs2_usgs_path, lsat_WRS2_scene_bounds)
      if(!file.exists(lsat_WRS2_scene_bounds)) {
        stop("Could not download WRS2 boundaries from USGS, ",
             "please specify file path!")
        }
      warning("Argument 'lsat_WRS2_scene_bounds' was not specified!",
                     " To avoid download in future, save file locally and",
                     " specify argument (see help).")
    }
  }

  # Load WRS2 scene bounds
  lsat_scene_footprints <- sf::read_sf(lsat_WRS2_scene_bounds)

  # Status
  cat(paste0("Determining Landsat WRS tile closest to the polygon centre...\n"))

  # Approximate overlapping tiles with polygon
  cat(crayon::green("Approximating tiles overlapping with polygon, the warning",
                    "by st_intersects can be ignored!\n"))
  lsat_overlapping_tiles <- lsat_scene_footprints[
    sf::st_intersects(polygon_sf, lsat_scene_footprints)[[1]],]

  # Identify EPSG for UTM zone of centroid (southern hemisphere landsat tiles
  # use northern hemisphere UTM )
  polygon_centroid <- suppressWarnings(sf::st_centroid(polygon_sf, silent = T))
  polygon_centroid_utm_crs <-
    floor((sf::st_coordinates(polygon_centroid)[1] + 180) / 6) + 1 + 32600

  # Transform geometries to local UTM
  polygon_sf_utm <-  sf::st_transform(polygon_sf,
                                      crs = polygon_centroid_utm_crs)
  lsat_overlapping_tiles_utm <- sf::st_transform(lsat_overlapping_tiles,
                                                 crs = polygon_centroid_utm_crs)

  # Identify closest Landsat tile to site centroid
  distance_to_tiles <- suppressWarnings(
    sf::st_distance(sf::st_centroid(polygon_sf_utm),
                    sf::st_centroid(lsat_overlapping_tiles_utm)))
  min_distance_tile_index <- which(distance_to_tiles == min(distance_to_tiles))
  wrs_tile_id <- lsat_overlapping_tiles_utm[min_distance_tile_index,]$Name

  # Split wrs_tile_id into path and row
  wrs_path <- as.numeric(gsub("([0-9]*)_[0-9]*", "\\1", wrs_tile_id))
  wrs_row <- as.numeric(gsub("[0-9]*_([0-9]*)", "\\1", wrs_tile_id))

  # Access landsat8 image collection
  ls8IC <- rgee::ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")

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

  # Compare with previously determined crs, if different transform polygon
  if(polygon_centroid_utm_crs != lsat_tile_crs) polygon_sf_utm <-
    sf::st_transform(polygon_sf, crs = lsat_tile_crs)

  # Check whether polygon is fully covered by tile
  is_covered <- lsat_overlapping_tiles_utm[min_distance_tile_index,] %>%
    sf::st_transform(crs = lsat_tile_crs) %>%
    sf::st_covers(polygon_sf_utm) %>%
    as.numeric()
  # If not, throw warning!
  if(is.na(is_covered)){
    warning("Polygon exceeds boundaries of closest Landsat WRS tile!\n",
            "Clipping polygon to footprint of Landsat WRS tile ", wrs_tile_id,
            ". Any regions outwidth the tile will be ignored. ",
            "Consider splliting polygon into smaller chuncks!")
    polygon_sf_utm <- polygon_sf_utm %>%
      sf::st_intersection(lsat_overlapping_tiles_utm[min_distance_tile_index,])
  }

  # Status
  cat(paste0("Retrieving pixel centres based on WRS tile '",
             wrs_tile_id,
             "'...\n"))

  # Add buffer to sf if specified and transform to lat long
  if("sfc_POLYGON" %in% class(sf::st_geometry(polygon_sf))) {
    polygon_sf_buffered <- polygon_sf_utm %>% sf::st_buffer(buffer) %>%
      sf::st_transform(4326)
  } else if("sfc_POINT" %in% class(sf::st_geometry(polygon_sf))) {
    polygon_sf_buffered <- polygon_sf_utm %>%
      sf::st_buffer(buffer, endCapStyle = "SQUARE") %>% sf::st_transform(4326)
  }
  # Retrieve first landsat 8 tile for summer 2019 from GEE
  ls8_image <- ls8IC$
    filterMetadata("WRS_PATH", "equals", as.integer(wrs_path))$
    filterMetadata("WRS_ROW", 'equals', as.integer(wrs_row))$
    filterDate("2019-06-01", "2019-09-30")$
    first()

  # Extract pixel coordinates
  ls8_pixels <- ls8_image$pixelLonLat()$
    select(c("longitude", "latitude"))$
    reduceRegion(reducer = rgee::ee$Reducer$toList(),
                 geometry = rgee::sf_as_ee(polygon_sf_buffered),
                 crs = ls8_image$projection(),
                 scale = 30L)

  # Conver to sf
  ls8_pixels_sf <- data.frame(
    latitude = ls8_pixels$getInfo()$latitude,
    longitude = ls8_pixels$getInfo()$longitude) %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326)

  # Add sample_id ID
  ls8_pixels_sf$sample_id <- paste0(pixel_prefix, "_",
                                   1:nrow(ls8_pixels_sf))

  # Visusalise grid using the mapview if requested
  if(plot_map == T | is.character(plot_map)){
    # Status update
    cat("Plotting grid...\n")

    # center map view
    rgee::Map$centerObject(rgee::sf_as_ee(polygon_sf_utm))

    # make map
    region_map <- rgee::Map$addLayer(ls8_image$select("B4")) +
      rgee::Map$addLayer(rgee::sf_as_ee(polygon_sf_buffered),
                         list(color = "blue")) +
      rgee::Map$addLayer(rgee::sf_as_ee(polygon_sf_utm),
                         list(color = "darkred")) +
      rgee::Map$addLayer(rgee::sf_as_ee(ls8_pixels_sf),
                         list(color = "black"))

    # display map
    print(region_map)

    # Export to snapshot to file if requested
    if(is.character(plot_map)){
      try(
        mapview::mapshot(
          region_map,
          file = plot_map,
          remove_controls = c("zoomControl",
                              "layersControl",
                              "homeButton",
                              "scaleBar",
                              "drawToolbar",
                              "easyButton")))
    }
  }

  # Status
  cat("Done.\n")
  # Return pixel centers as sfc
  return(ls8_pixels_sf)
}
