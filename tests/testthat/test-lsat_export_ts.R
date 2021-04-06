test_that("lsat_export_ts works", {
  # Initialize rgee
  rgee::ee_Initialize()

  # Load control data
  load("../data/lsat_export_ts.Rda")

  # Export time-series using lsat_export_ts()
  task_list <- lsat_export_ts(test_points_sf, site_from = "pixel_id")

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
