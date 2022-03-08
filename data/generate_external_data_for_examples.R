# DESCRIPTION ========================================================================
# This R script extracts Landsat data for several Arctic research stations to use for example

# SET UP =============================================================================
rm(list=ls()) 
library(lsatTS)
library(sf)
library(dplyr)
library(rgee)
library(data.table)
library(R.utils)

# GENERATE TEST POINTS ===============================================================
test_points_sf <- st_sfc(st_point(c(-149.6026, 68.62574)),
                         st_point(c(-149.6003, 68.62524)),
                         st_point(c(-75.78057, 78.87038)),
                         st_point(c(-75.77098, 78.87256)),
                         st_point(c(-20.56182, 74.47670)),
                         st_point(c(-20.55376, 74.47749)), crs = 4326) %>%
  st_sf() %>%
  mutate(sample_id = c("toolik_1",
                      "toolik_2",
                      "ellesmere_1",
                      "ellesmere_1",
                      "zackenberg_1",
                      "zackenberg_2"),
         region = c("toolik", "toolik",
                    "ellesmere", "ellesmere",
                    "zackenberg", "zackenberg"))

# EXPORT TIME SERIES ==============================================================
ee_Initialize()
task_list <- lsat_export_ts(test_points_sf)

# GRAB DATA FROM GOOGLE DRIVE AND MOVE TO LOCAL DIRECTORY =========================
lsat.data <- fread('C:/tmp/lsatTS_export_chunk_1.csv')
mkdirs('data')  
save(lsat.data, file="data/lsat_example_data.RData")
