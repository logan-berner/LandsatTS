test_that("lsat_get_pixel_centers works", {
  # Intialise rgee
  rgee::ee_Initialize()
  # Set test polygon coordinates
  test_poly <- sf::st_polygon(
    list(matrix(c(-138.90125, 69.58413,
                  -138.88988, 69.58358,
                  -138.89147, 69.58095,
                  -138.90298, 69.57986,
                  -138.90125, 69.58413),
                ncol = 2, byrow = T)))
  test_poly_sf <- sf::st_sf(sf::st_sfc(test_poly, crs = 4326))

  # Get pixel coordinates
  test_pixels <- lsat_get_pixel_centers(test_poly_sf)

  # test_pixels_control <- test_pixels
  # save(test_pixels_control, file = "tests/data/lsat_get_pixel_centers.Rda")

  # Load control data
  load("../data/lsat_get_pixel_centers.Rda")

  # Test for equivalence
  testthat::expect_equal(test_pixels, test_pixels_control)
})
