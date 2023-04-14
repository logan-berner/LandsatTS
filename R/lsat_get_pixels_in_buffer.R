#' Landsat 8 pixel centers within a buffer around point coordinates
#' **This function is currently in development and not intended for use!!!**
#' Helper function that uses lsat_get_pixel_centers() to determine all Landsat 8
#' pixel centre coordinates within a buffer around a set of point coordinates.
#'
#' @param point_coord_sf simple feature collection of points with a column that
#'   indicates the sample_id (speciefied with option sample_id_from).
#' @param buffer_dist buffer distance to be applied to point coordinates. Note:
#'   a distance of 15 m is automatically added. So when buffer distance = 0 the
#'   nearest pixel centre to the coordinate(s) is identified. If buffer distance =
#'   30, then the centre coordiantes of the pixels in the 3 x 3 neigbourhood
#'   that is centred on the coordinate(s) os returned.
#' @param sample_id_from column that provides the unique sample identifier.
#'   Defaults to "sample_id". Pixels centers within the buffer are returned by
#'   appending a counter "_x" to this column.
#' @param n_threads the number of threads to be used if parallel processing is
#'   desired. Recommended for large datasets.
#'
#' @return Simple feature collection of points that denote pixel centres within
#'   buffer around input coordinates. Unique identifiers are provided by
#'   appending a numerical counter "_x" to original identifiers in column
#'   specified in the sample_id argument.
# #' @export lsat_get_pixels_in_buffer
#' @author Jakob J. Assmann
#' @keywords internal
#'
#' @examples
#'
#' # Using sf, dplyr and rgee
#' library(sf)
#' library(dplyr)
#' library(rgee)
#'
#' # Initialize rgee
#' ee_Initialize()
#'
#' # Generate test points
#' test_points_sf <- st_sfc(st_point(c(-149.6026, 68.62574)),
#'                          st_point(c(-149.6003, 68.62524)),
#'                          st_point(c(-75.78057, 78.87038)),
#'                          st_point(c(-75.77098, 78.87256)),
#'                          st_point(c(-20.56182, 74.47670)),
#'                          st_point(c(-20.55376, 74.47749)), crs = 4326) %>%
#'   st_sf() %>%
#'   mutate(sample_id = c("toolik_1",
#'                       "toolik_2",
#'                       "ellesmere_1",
#'                       "ellesmere_1",
#'                       "zackenberg_1",
#'                       "zackenberg_2"),
#'          region = c("toolik", "toolik",
#'                     "ellesmere", "ellesmere",
#'                     "zackenberg", "zackenberg"))
#'
#' # Retrieve pixel centres in 3 x 3 neighbourhood using parallel processing
#' # and 4 threads
#'
# #' lsat_get_pixels_in_buffer(test_points_sf, buffer_dist = 30, n_threads = 4)

lsat_get_pixels_in_buffer <- function(point_coord_sf,
                                  buffer_dist = 0,
                                  sample_id_from = "sample_id",
                                  n_threads = 1){
  # Status update
  cat("Retrieving Landsat 8 pixel centers within",
      buffer_dist, "m for",
      nrow(point_coord_sf),
      "point coordinates.\n")

  # Check if multiprocessing was requested and prepare cluster
  # (forking not possible due to rgee)
  if(n_threads > 1){
    cat("Multiprocessing requested with", n_threads, "threads.\n")
    cat("Preparing parallel cluster ...")
    cl <- parallel::makeCluster(n_threads)
    parallel::clusterEvalQ(cl, {
      library(rgee)
      ee_Initialize()
    })
    cat(" cluster prepared.\n")
  } else { # If no parallel processing was requested set cluster to NULL
    cl = NULL
  }

  # Execute buffering using lsat_get_pixel_centers
  cat("Buffering coordinates ... \n")

  point_list <- point_coord_sf %>%
    split(pull(point_coord_sf, sample_id_from))
  pixel_coords_sf_buffered <-  parallel::parLapply(point_list,
                      lsat_get_pixel_centers,
                      buffer = buffer_dist + 15,
                      pixel_prefix_from = sample_id_from,
                      cl = cl) %>%
    dplyr::bind_rows()

  # Stop cluster if needed
  if(class(cl)[1] == "SOCKcluster") parallel::stopCluster(cl)

  # Status update
  cat("Done.")

  # Return buffered pixels
  return(pixel_coords_sf_buffered)
}

