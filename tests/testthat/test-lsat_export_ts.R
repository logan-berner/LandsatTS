test_that("lsat_export_ts basic functionality works", {
  # Initialize rgee
  rgee::ee_Initialize()

  cat("Generating test points ...\n")

  # Set test points
  test_points_sf <- matrix(c(-149.6026, 68.62574,
                             -149.6003, 68.62524,
                             -75.78057, 78.87038,
                             -75.77098, 78.87256,
                             -20.56182, 74.47670,
                             -20.55376, 74.47749),
                           byrow = T, ncol = 2,
                           dimnames = list(NULL, c("long", "lat"))) %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
  test_points_sf$sample_id <- c("toolik_1",
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

  # Confirm that this worked
  task_list %>% lapply(function(x) x$active()) %>% unlist() %>% unique() %>%
    expect_equal(TRUE)

  # Cancel tasks
  task_list %>% lapply(function(x) x$cancel())

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

test_that("lsat_export_ts chunk division works", {
  # Initialize rgee
  rgee::ee_Initialize()

  cat("Generating test points ...\n")

  # Set test points
  test_points_sf <- matrix(c(-149.6026, 68.62574,
                           -149.6003, 68.62524,
                           -75.78057, 78.87038,
                           -75.77098, 78.87256,
                           -20.56182, 74.47670,
                           -20.55376, 74.47749),
                           byrow = T, ncol = 2,
                           dimnames = list(NULL, c("long", "lat"))) %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
  test_points_sf$sample_id <- c("toolik_1",
                           "toolik_2",
                           "ellesmere_1",
                           "ellesmere_1",
                           "zackenberg_1",
                           "zackenberg_2")

  test_points_sf$region <- c("toolik", "toolik",
                             "ellesmere", "ellesmere",
                             "zackenberg", "zackenberg")

  # Export time-series by region using lsat_export_ts()
  cat("Testing export with default settings:\n")
  task_list <- lsat_export_ts(test_points_sf)

  # Confirm that this worked
  task_list %>% lapply(function(x) x$active()) %>% unlist() %>% unique() %>%
    expect_equal(TRUE)

  # Cancel tasks
  task_list %>% lapply(function(x) x$cancel())

  cat("Cancelling tasks...\n")
})

test_that("lsat_export_ts chunk division works", {
  # Initialize rgee
  rgee::ee_Initialize()

  cat("Generating test points ...\n")

  # Set test points
  test_points_sf <- matrix(c(-149.6026, 68.62574,
                             -149.6003, 68.62524,
                             -75.78057, 78.87038,
                             -75.77098, 78.87256,
                             -20.56182, 74.47670,
                             -20.55376, 74.47749),
                           byrow = T, ncol = 2,
                           dimnames = list(NULL, c("long", "lat"))) %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
  test_points_sf$sample_id <- c("toolik_1",
                                "toolik_2",
                                "ellesmere_1",
                                "ellesmere_1",
                                "zackenberg_1",
                                "zackenberg_2")

  test_points_sf$region <- c("toolik", "toolik",
                             "ellesmere", "ellesmere",
                             "zackenberg", "zackenberg")

  ## Export time-series by region using lsat_export_ts()
  cat("Testing export by region:\n")
  task_list <- lsat_export_ts(test_points_sf,
                              chunks_from = "region")
  # Confirm that this worked
  task_list %>% lapply(function(x) x$active()) %>% unlist() %>% unique() %>%
    expect_equal(TRUE)

  # Cancel tasks
  cat("Cancelling tasks...\n")
  task_list %>% lapply(function(x) x$cancel())

  ## Export time-series for one chunk by region using lsat_export_ts()
  cat("Testing export by region for one chunk only:\n")
  task_list <- lsat_export_ts(test_points_sf,
                              chunks_from = "region",
                              this_chunk_only = "ellesmere")
  # Confirm that this worked
  task_list %>% lapply(function(x) x$active()) %>% unlist() %>% unique() %>%
    expect_equal(TRUE)
  task_list %>% length() %>% expect_equal(1)

  # Cancel tasks
  cat("Cancelling tasks...\n")
  task_list %>% lapply(function(x) x$cancel())

  ## Export time-series by chunk size using lsat_export_ts()
  cat("Testing export by setting chunmax_chunk_size to 2:\n")
  task_list <- lsat_export_ts(test_points_sf,
                              max_chunk_size = 2)
  # Confirm that this worked
  task_list %>% lapply(function(x) x$active()) %>% unlist() %>% unique() %>%
    expect_equal(TRUE)

  # Cancel tasks
  cat("Cancelling tasks...\n")
  task_list %>% lapply(function(x) x$cancel())

  # Test expected errors
  cat("Testing error messages\n")

  # Main argument not an sf object
  expect_error(lsat_export_ts("ASFASDF"))
  expect_error(lsat_export_ts(test_points_sf,
                              chunks_from = "not_a_column"))
  expect_error(lsat_export_ts(test_points_sf,
                              chunks_from = "region",
                              this_chunk_only = "not_a_chunk"))

})
