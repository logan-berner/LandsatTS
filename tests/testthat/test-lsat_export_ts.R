test_that("lsat_export_ts works", {
  # Intialize rgee
  rgee::ee_Initialize()

  save(test_points_sf, file = "tests/data/lsat_export_ts.Rda")
  load("tests/data/lsat_export_ts.Rda")

  # Export time-series using lsat_export_ts()
  task_list <- lsat_export_ts(test_points_sf)

  # Monitor task completion
  cat("Waiting for export from EE to finish...")
  rgee::ee_monitoring(task_list[[1]])

  # Copy output from google drive to tempfile
  output <- ee_drive_to_local(task_list[[1]])

  # Load tempfile and control data
  output_file <- readr::read_csv(output)
  control_file <- readr::read_csv("tests/data/lsatTS_export_chunk_1.csv")

  # Check for equivalence
  expect_equal(output_file, control_file)
})
