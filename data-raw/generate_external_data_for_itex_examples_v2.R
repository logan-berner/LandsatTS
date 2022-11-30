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
library(rnaturalearth) # For the country boundaries

# GENERATE TEST POINTS =========================================================
flux.sites.sf <- st_sfc(st_point(c(161.3414, 68.6130)),
                         st_point(c(-20.5503, 74.4733)),
                         st_point(c(-155.7503, 68.4865)),
                        crs = 4326) %>%
  st_sf() %>%
  mutate(sample_id = c("Cherski",
                       "Zackenberg Heath",
                       "Ivotuk"))

plot(st_geometry(ne_countries(returnclass = "sf")))
plot(st_geometry(itex.sites.sf), col = "red", add = T)

# EXPORT TIME SERIES ===========================================================
ee_Initialize()
task_list <- lsat_export_ts(flux.sites.sf)

# SAVE EXTRACTED LANDSAT DATA FOR INTERNAL USE =========================
lsat.example.dt <- fread('C:/tmp/lsatTS_export_chunk_1.csv')
save(lsat.example.dt, file="data/lsat.example.dt.RData")

