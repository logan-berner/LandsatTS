# DESCRIPTION ---------------------------------------------------------------------------------
# This R script generates and analyzes time series of annual maximum vegetation greenness 
# for pixels in the study area on Disko Island using Landsat surface reflectance.  
# Author: Logan Berner 
# Institution: Northern Arizona University, School of Informatics, Computing, and Cyber Systems
# Date: 2021-11-18
# URL: https://github.com/logan-berner/lsatTS
# --------------------------------------------------------------------------------------------

# Clean workspace
rm(list=ls())

# Load required R packages
require(data.table)
require(lsatTS)
require(ggplot2)

# Set working directory
setwd('C:/Users/Logan/My Drive/research/code/lsatTS/man/manuscript/')
# setwd('C:/Users/lb968/My Drive/research/code/lsatTS/man/manuscript/')

# PROCESS LANDSAT DATA ------------------------------------------------------------------------

# Create a list of data files exported from GEE and then read them in to R as a data.table object 
data.files <- list.files('C:/tmp/earth_engine_export/', full.names = T, pattern = 'highlat')
lsat.dt <- do.call("rbind", lapply(data.files, fread))

# Format the exported data
lsat.dt <- lsat_general_prep(lsat.dt)

# Clean the data by filtering out clouds, snow, and water, as well as radiometric and geometric errors
lsat.dt <- lsat_clean_data(lsat.dt)

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

usethis::use_data(lsat.xcal.dt, internal = TRUE)

# END SCRIPT #--------------------------------------------------------------------------------------------------------