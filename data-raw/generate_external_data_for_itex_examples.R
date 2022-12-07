# DESCRIPTION ================================================================
# This R script extracts Landsat data for ITEX tundra phenology sites
# Data set: https://www.polardata.ca/pdcsearch/PDCSearch.jsp?doi_id=13215
# CSV files: https://www.polardata.ca/pdcsearch/PDC_Metadata_Data_Download.ccin?action=downloadPDCData&ccin_ref_number=13215&fileLoc=/pdc/ipy/13215/CCIN13215_20220120_tundra_phenology_database.csv
# SET UP =======================================================================
rm(list=ls())
library(LandsatTS)
library(sf)
library(dplyr)
library(rgee)
library(rnaturalearth) # For the country boundaries

# COORDINATES FOR ITEX SITES ===================================================
itex.dt <- fread("data-raw/itex_tundra_phenology_database.csv")

itex.sites.dt <- itex.dt %>% rename(sample_id = study_area) %>%
  select(sample_id, lat, long) %>%
  distinct() %>%
  group_by(sample_id) %>%
  top_n(n = 1) %>% as.data.table()

# exclude the Bogong site located in Australia
itex.sites.dt <- itex.sites.dt[sample_id != 'Bogong']
save(itex.sites.dt, file="data/itex.sites.dt.RData")

# itex.sites.sf <- itex.sites.dt %>% as.data.frame() %>% st_as_sf(coords = c("long", "lat"), crs = 4326)
# save(itex.sites.sf, file="data/itex.sites.sf.RData")

# jpeg('man/manuscript/figures/itex_site_map.jpg', width = 7, height = 5, units = 'in', res = 300)
# plot(st_geometry(ne_countries(returnclass = "sf")))
# plot(st_geometry(itex.sites.sf), col = "black", bg = 'red', add = T, pch = 21)
# dev.off()


# EXPORT TIME SERIES ==============================================================
ee_Initialize()
task_list <- lsat_export_ts(pixel_coords_sf = itex.sites.sf, 
               start_date = "1985-06-01", 
               end_date = "2021-09-30",
               startJulian = 152, 
               endJulian = 273, 
               file_prefix = 'itex_sites', 
               drive_export_dir = 'gee_export')


# GRAB DATA FROM GOOGLE DRIVE AND MOVE TO LOCAL DIRECTORY =========================
itex.lsat.dt <- fread('C:/Users/Logan/My Drive/gee_export/itex_sites__chunk_1.csv')
itex.lsat.dt <- itex.lsat.dt[sample_id != "Bogong"]
save(itex.lsat.dt, file="data/itex.lsat.dt.RData")