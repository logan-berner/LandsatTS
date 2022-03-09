# DESCRIPTION =============================================================================\
# This R script prepares Landsat data for internal use by the lsat_calibrate_rf() function. 
# Specifically, the script extracts and processes Landsat surface reflectance time series data
# for 10,000 random sample locations in the Arctic-Boreal domain. It then prepares the data 
# for optional use in the cross-calibration funciot.n 
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2022-03-27
# URL: https://github.com/logan-berner/lsatTS
# =========================================================================================

# Clear workspace
rm(list=ls())

# Load required R packages
require(sf)
require(lsatTS)
require(rgee)
require(dplyr)
require(R.utils)
sf::sf_use_s2(FALSE)
mkdirs('data-raw/tmp/')

# DOWNLOAD AND PREPARE ECOREGION DATASET ================================================

# download RESOLVE ecoregions
download.file('https://wri-public-data.s3.amazonaws.com/resourcewatch/bio_042_biome_ecoregion.zip', 
              destfile = 'data-raw/tmp/resolve_ecoregions.zip')

unzip(zipfile = 'data-raw/tmp/resolve_ecoregions.zip', exdir = 'data-raw/tmp/resolve_ecoregions')
unlink('data-raw/tmp/resolve_ecoregions.zip')

# read in ecoregions shapefile
ecoreg.sf <- st_read('data-raw/tmp/resolve_ecoregions/Ecoregions2017.shp')

# subset Tundra, Boreal Forests/Taiga and dissolve
n.ecoreg.sf <- ecoreg.sf %>% filter((BIOME_NAME == 'Tundra' | BIOME_NAME == 'Boreal Forests/Taiga') & (REALM == 'Nearctic' | REALM == 'Palearctic'))

# reproject
laea <- st_crs("+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
n.ecoreg.sf <- st_transform(n.ecoreg.sf, laea)

# dissolve polygons
n.ecoreg.sf <- st_union(n.ecoreg.sf)
plot(n.ecoreg.sf)


# CREATE SAMPLE POINTS =========================================================================================

# sample high-latitude study area
sample.pts <- st_sample(n.ecoreg.sf, size = 10000) %>% st_cast('POINT')
sample.pts <- sample.pts %>% st_sf %>% mutate(sample_id = paste0('pt', 1:nrow(st_coordinates(sample.pts))))
plot(sample.pts, col = 'blue', add=T)

# cluster sample points to speed data extraction from GEE
sample.pts.latlon <- st_transform(sample.pts, crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
coords <- st_coordinates(sample.pts.latlon)
dist <- distm(coords) # distance matrix
clusters <- hclust(as.dist(dist), method="complete")
sample.pts$cluster <- cutree(clusters, h=1000000)


# EXTRACT LANDSAT DATA FOR SAMPLE POINTS =========================================================================
ee_Initialize()
task_list <- lsat_export_ts(pixel_coords_sf = sample.pts, chunks_from = 'cluster', startJulian = 122, endJulian = 274,
                            file_prefix = 'highlat', drive_export_dir = 'earth_engine')


# WAIT FOR EXTRACTION TO FINISH THEN GRAB FILES ==================================================================
# Create a list of data files exported from GEE and then read them in to R as a data.table object 
data.files <- list.files('data-raw/tmp/earth_engine_export/', full.names = T, pattern = 'highlat')
lsat.dt <- do.call("rbind", lapply(data.files, fread))


# PROCESS LANDSAT DATA ===========================================================================================
lsat.dt <- lsat_general_prep(lsat.dt)
lsat.dt <- lsat_clean_data(lsat.dt)


# PREPARE LANDSAT DATA FOR USE IN LSAT_CALIBRATE_RF() ============================================================

# For Landsat 5/8, identify and subset years when scenes were also collected by Landsat 7
xcal.list <- list()
sats <- dt[,unique(satellite)]
sats <- sats[-which(sats %in% 'LANDSAT_7')]

for (i in sats){
  xcal.dt <- lsat.dt[satellite == i | satellite == 'LANDSAT_7'] # get obs for specific satellites
  sample.yr.dt <- xcal.dt[,.(year = unique(year)), by = .(sample.id, satellite)]
  sample.yr.dt <- data.table::dcast.data.table(sample.yr.dt, sample.id + year ~ satellite, value.var = 'year')
  sample.yr.dt <- na.omit(sample.yr.dt)
  sample.yr.dt <- data.table::melt.data.table(sample.yr.dt, id = "sample.id", measure = c("LANDSAT_7",i), variable.name = "satellite", value.name = "year")
  xcal.dt <- xcal.dt[sample.yr.dt, on = c('sample.id','year','satellite')]
  xcal.list[[i]] <- xcal.dt
}

lsat.xcal.dt <- rbindlist(xcal.list)

# subset columns to keep
keep.cols <- c('sample.id','latitude','longitude','satellite','year','doy','blue','green','red','nir','swir1','swir2')
lsat.xcal.dt  <- lsat.xcal.dt[, keep.cols, with = F]


# WRITE OUT FOR INTERNAL USE =========================================================================================
usethis::use_data(lsat.xcal.dt, internal = TRUE)

# clean up
unlink('data-raw/tmp', recursive = T)

# END SCRIPT ========================================================================================================