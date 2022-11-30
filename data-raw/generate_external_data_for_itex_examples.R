# # DESCRIPTION ================================================================
# # This R script extracts Landsat data for ITEX tundra phenology sites
# # Data set: https://www.polardata.ca/pdcsearch/PDCSearch.jsp?doi_id=13215

# SET UP =======================================================================
rm(list=ls())
library(lsatTS)
library(sf)
library(dplyr)
library(rgee)
library(data.table)
library(R.utils)
library(tidyverse)
library(rnaturalearth) # For the country boundaries

# COORDINATES FOR ITEX SITES ===================================================
# # DESCRIPTION ========================================================================
# # This R script extracts Landsat data for several Arctic research stations to use for example
#
# SET UP =============================================================================
rm(list=ls())
library(lsatTS)
library(sf)
library(dplyr)
library(rgee)
library(data.table)
library(R.utils)

# GENERATE TEST POINTS ===============================================================
itex.sites.sf <- st_sfc(st_point(c(-80.00, 73.13)),
                         st_point(c(-150.00, 69.00)),
                         st_point(c(-50.72, 67.02)),
                         st_point(c(-76.00, 79.00)),
                         st_point(c(-139.08, 69.58)), crs = 4326) %>%
  st_sf() %>%
  mutate(sample_id = c("Bylot Island",
                       "Toolik Lake",
                       "Kangerlussuaq",
                       "Alexandra Fiord",
                       "Qikiqtaruk"))

# EXPORT TIME SERIES ==============================================================
ee_Initialize()
task_list <- lsat_export_ts(test_points_sf)

# GRAB DATA FROM GOOGLE DRIVE AND MOVE TO LOCAL DIRECTORY =========================
mkdirs('data')
lsat.example.dt <- fread('C:/tmp/lsatTS_export_chunk_1.csv')
save(lsat.example.dt, file="data/lsat.example.dt.RData")



itex.sites.df <- read_csv("data-raw/itex_tundra_phenology_database.csv")

itex.sites.df <- itex.sites.df %>% rename(sample_id = study_area) %>%
  select(sample_id, lat, long, year) %>%
  distinct() %>%
  group_by(sample_id, lat, long) %>%
  top_n(n = 1) %>%
  arrange(year)

itex.sites.sf <- itex.sites.df %>% st_as_sf(coords = c("long", "lat"), crs = 4326)


itex.sites.sf <- read_csv("data-raw/itex_tundra_phenology_database.csv") %>%
  rename(sample_id = study_area) %>%
  select(sample_id, lat, long, year) %>%
  distinct() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)




itex.sites.sf <- read_csv("data-raw/itex_tundra_phenology_database.csv") %>%
  rename(sample_id = study_area) %>%
  select(sample_id, lat, long) %>%
  distinct() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

plot(st_geometry(ne_countries(returnclass = "sf")))
plot(st_geometry(itex.sites.sf), col = "red", add = T)

save(itex.sites.sf, file="data/itex.sites.sf.RData")


# EXPORT TIME SERIES ==============================================================
ee_Initialize()
task_list <- lsat_export_ts(itex.sites.sf)

# GRAB DATA FROM GOOGLE DRIVE AND MOVE TO LOCAL DIRECTORY =========================
lsat.example.dt <- fread('C:/tmp/lsatTS_export_chunk_1.csv')
save(lsat.example.dt, file="data/lsat.example.dt.RData")
