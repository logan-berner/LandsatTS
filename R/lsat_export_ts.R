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
#' @export lsat_export_ts
#'
#' @examples
#'
#'
lsat_export_ts <- function(pixel_coords_sf,
                             pixel_id_from = "pixel_id",
                             chunks_from = NULL,
                             max_chunk_size = 250,
                             drive_export_dir = "lsatTS_export",
                             file_prefix = "lsatTS_export",
                             startJulian = 152,
                             endJulian = 243,
                             start_date = "1984-01-01",
                             end_date = "2021-01-01",
                             BUFFER_DIST = 0,
                             SCALE = 30,
                             NULL_VALUE = 0,
                             MASK_VALUE = 0
                             ){
  # @JAKOB INSERT CODE FROM GEE:/users/HiLDEN/HiLDEN/getLS_by_site HERE!
  # & adapt for rgee use

  # Extract Landsat Time-Series for HiLDEN sites
  # written by Jakob Assmann (j.assmann@bio.au.dk)
  # based on a Python EE script by Richard Massey (Richard.Massey@nau.edu)
  # 16 October 2020

  # Declare global variables

  # Prep Landsat Time series
  bands <- list("B1", "B2", "B3", "B4", "B5", "B6", "B7", "pixel_qa", "radsat_qa")
  BAND_LIST <- ee$List(bands)

  # addon asset and bands
  ADDON <- ee$Image('JRC/GSW1_0/GlobalSurfaceWater')$float()$unmask(MASK_VALUE);
  ADDON_BANDLIST <- ee$List(list("max_extent"));

  # Landsat Surface Reflectance collections
  ls5_1 <- ee$ImageCollection("LANDSAT/LT05/C01/T1_SR");
  ls5_2 <- ee$ImageCollection("LANDSAT/LT05/C01/T2_SR");
  ls7_1 <- ee$ImageCollection("LANDSAT/LE07/C01/T1_SR");
  ls7_2 <- ee$ImageCollection("LANDSAT/LE07/C01/T2_SR");
  ls8_1 <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR");
  ls8_2 <- ee$ImageCollection("LANDSAT/LC08/C01/T2_SR");

  ALL_BANDS <- BAND_LIST$cat(ADDON_BANDLIST);

  # merge all collections in one with a global empty image at the beginning
  LS_COLL <- ls5_1$
    merge(ls7_1$
         merge(ls8_1$
                merge(ls5_2$
                       merge(ls7_2$
                              merge(ls8_2)))))$
    filterDate(start_date, end_date)$
    filter(ee$Filter$calendarRange(startJulian, endJulian, "day_of_year"))$
    #.filterBounds(table)
    map(function(image) {return(image$addBands(ADDON, ADDON_BANDLIST))})$
    select(ALL_BANDS)$
    map(function(image){ return(image$float())} )


  # Check if chunks_from was specified, if not determine chunks
  if(!is.null(chunks_from)){
    if(!(chunks_from %in% colnames(pixel_coords_sf))) stop("Invalid columname specified for chunkls_from")
    chunk_list <- pixel_coords_sf %>% split(.[,chunks_from])
  } else {
    n_chunks <- floor(nrow(pixel_coords_sf) / max_chunk_size) + 1
    pixel_coords_sf$chunk_id <- paste0("chunk_", sort(rep(1:n_chunks, max_chunk_size)))[1:nrow(pixel_coords_sf)]
    chunks_from <- "chunk_id"
  }

  # Status:
  cat(paste0("Exporting time-series for ", nrow(pixel_coords_sf), " pixels",
             " in ", length(unique(st_drop_geometry(pixel_coords_sf)[,chunks_from])), " chunks.\n"))

  # Retrieve time-series by chunk
  pixel_coords_sf %>% split(., st_drop_geometry(.)[,chunks_from]) %>%
    map(function(chunk){
      # Status
      cat(paste0("Submitting task to EE for chunk_id: ", st_drop_geometry(chunk)[,chunks_from][1], ".\n"))
      # Upload chunk to sf to reduce size keep only necessary columns
      ee_chunk <- sf_as_ee(chunk[,c("geometry", pixel_id_from, chunks_from)])
      # Retrieve Landsat time-series
      ee_chunk_export <- ee_chunk$map(function(feature){
          return(
            # Create FC containing a single empty image
            # This will ensure all bands are present in the export
            ee$ImageCollection$fromImages(
            list(ee$Image(list(0,0,0,0,0,0,0,0,0,0))$
                   select(list(0,1,2,3,4,5,6,7,8,9), ALL_BANDS)$
                   copyProperties(ls8_1$first())))$
              # Merge with extraction of time-series form whole Landsat collection
              merge(LS_COLL$filterBounds(feature$geometry()))$
                # For each image in the collection ....
                map(function(image){
                  # Create a feature
                return(ee$Feature(feature$geometry(),
                                  # fill it with the point value extracted with
                                  # reduceRegion and the first() reducer at the set SCALE
                                  image$reduceRegion(ee$Reducer$first(),
                                                     feature$geometry(),
                                                     SCALE))$
                         # copy the image properties to the feature (incl. date and image metadata)
                         copyProperties(image)$
                         # assign a pixel and chunk id columns for identification
                         set(pixel_id_from, feature$get(pixel_id_from))$
                         set(chunks_from, feature$get(chunks_from)))
              }))
        })$flatten()
      # Prepare export task
      chunk_task <- ee_table_to_drive(
        collection = ee_chunk_export,
        description = paste0("lsatTS_export_", st_drop_geometry(chunk)[,chunks_from][1]),
        folder = drive_export_dir,
        fileNamePrefix = paste0(file_prefix, "_", st_drop_geometry(chunk)[,chunks_from][1]),
        timePrefix = F,
        fileFormat = "csv")
      # Submit export task
      chunk_task$start()

      # Return nothing
      invisible(NULL)
    })
  # Status update
  cat(green("Done!\n"))
  cat("You can monitor the export progrees for each task using rgee's ee_monitoring() or the GEE WebAPI.\n")
}

test_poly <- st_polygon(
list(matrix(c(69.58413326844578, -138.90125985326782,
               69.58358009075835, -138.88988547014793,
               69.5809560122973, -138.89147182732913,
               69.57986505201093, -138.90298010997816,
               69.58413326844578, -138.90125985326782),
             ncol = 2, byrow = T)))
test_poly2_sfc <- st_sfc(test_poly2, crs = 4326)

library(purrr)
pixel_list <- test_poly_sf %>%
  split(.$region) %>% map(lsat_get_pixel_centers,
                          pixel_prefix_from = "region",
                          lsat_WRS2_scene_bounds = "tests/WRS-2_bound_world_0.kml") %>% bind_rows()
class(test_poly_sf$geometry[1][1])
test <- test_poly_sf %>%
  split(.$region)
test_sub <- pixel_list[sample(1:nrow(pixel_list), 5),]
lsat_download_ts(pixel_list)
