test_that("lsat_export_ts works", {
  # Initialize rgee
  rgee::ee_Initialize()

  # Set test points
  test_points_sf <- sf::st_sfc(sf::st_point(c(-149.6026, 68.62574)),
                               sf::st_point(c(-149.6003, 68.62524)),
                               sf::st_point(c(-75.78057, 78.87038)),
                               sf::st_point(c(-75.77098, 78.87256)),
                               sf::st_point(c(-20.56182, 74.47670)),
                               sf::st_point(c(-20.55376, 74.47749)),
                               crs = 4326)
  test_points_sf <- sf::st_sf(geometry = test_points_sf)
  test_points_sf$site <- c("toolik_1",
                           "toolik_2",
                           "ellesmere_1",
                           "ellesmere_1",
                           "zackenberg_1",
                           "zackenberg_2")

  test_points_sf$region <- c("toolik", "toolik",
                             "ellesmere", "ellesmere",
                             "zackenberg", "zackenberg")

  # Export time-series using lsat_export_ts()
  task_list <- lsat_export_ts(test_points_sf)

  # Monitor task completion
  cat("Waiting for export from EE to finish...")
  task_status <- rgee::ee_monitoring(task_list[[1]])
  task_status <- rgee::ee_monitoring()
  task_status <- gsub(".*\\((.*)\\).*", "\\1", task_status)
  testthat::expect_equal(task_status, "COMPLETED")

  # the following code does not work as ee_drive_to_local can only be used
  # in and interactive session
  # # Copy output from google drive to tempfile
  # output <- rgee::ee_drive_to_local(task_list[[1]])
  #
  # # Load tempfile and control data
  # output_file <- readr::read_csv(output)
  # control_file <- readr::read_csv("../data/lsatTS_export_chunk_1.csv")
  #
  # # Check for equivalence
  # testthat::expect_equal(output_file, control_file)
})
