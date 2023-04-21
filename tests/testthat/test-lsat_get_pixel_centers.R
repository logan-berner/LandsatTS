test_that("lsat_get_pixel_centers basic functionality works", {
  # Skip test if rgee is not installed (requested by reviewer)
  skip_if_not_installed("rgee")
  
  # Initialize rgee
  rgee::ee_Initialize()

  # Set test polygon coordinates
  test_poly_sf <- list(matrix(c(-138.90125, 69.58413,
                  -138.88988, 69.58358,
                  -138.89147, 69.58095,
                  -138.90298, 69.57986,
                  -138.90125, 69.58413),
                ncol = 2, byrow = T)) %>%
    sf::st_polygon() %>%
    sf::st_sfc(crs = 4326) %>%
    sf::st_sf()

  # Set test point coordinates
  test_point_sf <- c(-138.90125, 69.58413) %>%
    sf::st_point() %>%
    sf::st_sfc(crs = 4326) %>%
    sf::st_sf()

  # Get pixel coordinates, suppressing first warning that the WRS2 grid file was not supplied
  suppressWarnings(test_pixels_poly <- lsat_get_pixel_centers(test_poly_sf))
  test_pixels_point_buffered <- lsat_get_pixel_centers(test_point_sf, buffer = 45)

  # # Cache control data
  # test_pixels_poly_control <- test_pixels_poly
  # save(test_pixels_poly_control,
  #   file = test_path("cache/lsat_get_pixel_centers_test_pixels_poly.Rda"))
  # test_pixels_point_buffered_control <- test_pixels_point_buffered
  # save(test_pixels_point_buffered_control,
  #   file = test_path("cache/lsat_get_pixel_centers_test_pixels_point_buffered.Rda"))

  # Load control data
  load(test_path("cache/lsat_get_pixel_centers_test_pixels_poly.Rda"))
  load(test_path("cache/lsat_get_pixel_centers_test_pixels_point_buffered.Rda"))

  # Test for equivalence
  testthat::expect_equal(test_pixels_poly, test_pixels_poly_control)
  testthat::expect_equal(test_pixels_point_buffered,
                         test_pixels_point_buffered_control)
})
