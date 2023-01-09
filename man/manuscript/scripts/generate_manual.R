# Quick script to generate the manual form the Roxygen documentation
require(devtools)
devtools::document()
devtools::build_manual(path = "man/manuscript/")
