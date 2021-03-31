# Package set up script following https://r-pkgs.org/whole-game.html
# Jakob Assmann j.assmann@bio.au.dk 12 March 2021

# DO NOT RE RUN THIS SCRIPT!
# Only execute specific commands if required.

# Dependencies
library(devtools)
library(tidyverse)
library(fs)

# Create package skeleton
create_package("~/Repositories/lsatTS")

# Add first functions
use_r("lsat_retrieve_pixels")
use_r("lsat_download_ts")

# Load all functions
load_all()

# Generate documentation
document()

# Check whether package load would succeed?
check()

# Use testthat
use_testthat()

# Initialize test for functions
use_test("lsat_get_pixel_centers")
use_test("lsat_export_ts")

# add required packages for import
# USE: use_package("package_name")

# Add R markdown readme for github documentation
use_readme_rmd()
