# DESCRIPTION ================================================================
# This R script extracts Landsat data for ITEX tundra phenology sites

# SET UP =======================================================================
rm(list=ls())
library(LandsatTS)
library(sf)
library(dplyr)
library(rgee)
library(R.utils)

# DOWNLOAD NATIONAL PARK BOUNDARY SHAPEFILE ====================================
mkdirs('data-raw/tmp')
download.file(url = 'https://irma.nps.gov/DataStore/DownloadFile/680778', 
              destfile = 'data-raw/tmp/nps_boundary.zip',
              mode='wb', cacheOK=FALSE)

unzip(zipfile = 'data-raw/tmp/nps_boundary.zip', exdir = 'data-raw/tmp/')


# EXTRACT NOATAK NATIONAL PRESERVE =============================================
parks.sf <- st_read('data-raw/tmp/nps_boundary.shp')
noatak.sf <- parks.sf %>% filter(UNIT_NAME == 'Noatak National Preserve')
noatak.sf <- noatak.sf %>% st_transform(crs = 4326)
noatak.sf <- noatak.sf %>% select()

# Save shapefile so it's accessible within package
save(noatak.sf, file="data/noatak.sf.RData")


# CREATE RANDOM SAMPLE POINTS WITHIN NOATAK AND EXTRACT LANDSAT DATA ==========
n.pts <- 100
pts.sf <- st_sample(x = noatak.sf, size = n.pts) %>% st_sf()

# Add unique identifier to each point
pts.sf$sample_id = paste0('S_', 1:n.pts)

# Extract data from GEE
ee_Initialize()

task_list <- lsat_export_ts(pixel_coords_sf = pts.sf,
                            start_date = "1985-06-01", 
                            end_date = "2022-09-30",
                            start_doy = 152, 
                            end_doy = 273,
                            file_prefix = 'noatak', 
                            drive_export_dir = 'earth_engine')


# GRAB DATA FROM GOOGLE DRIVE AND MOVE TO LOCAL DIRECTORY ======================
noatak.dt <- fread('C:/Users/Logan/My Drive/earth_engine/noatak_chunk_1.csv')
save(noatak.dt, file="data/noatak.dt.RData")

# END SCRIPT ===================================================================