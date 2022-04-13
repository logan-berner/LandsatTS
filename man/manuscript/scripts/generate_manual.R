# Quick script to generate the manual form the Roxygen documentation
devtools::document()
devtools::build_manual(path = "man/manuscript/")
