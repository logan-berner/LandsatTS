#' Prepare Landsat data for analysis
#'
#' @description This function parses sample site coordinates and time period of each measurement, 
#'     scales band values, and formats column names as needed for subsequent analysis using the 
#'     lsatTS package.
#' @param dt Data.table with Landsat data exported from Google Earth Engine using lsat_export_ts().
#' @return Data.table with formatted and scaled values.
#' @import data.table
#' @export lsat_general_prep
#'
#' @examples 
#' data(lsat.example.dt)
#' lsat.dt <- lsat_general_prep(lsat.example.dt)
#' lsat.dt

data.files <- list.files('C:/Users/Logan/My Drive/earth_engine/', full.names = T, pattern = 'serdp')
dt <- do.call("rbind", lapply(data.files, fread))


lsat_general_prep <- function(dt){

  # type cast
  dt <- data.table::data.table(dt)
  
  # change sample_id to sample.id
  setnames(dt, 'sample_id','sample.id')

  # change colunm names to lower case with conjoined words seperated by '.'
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub("_", ".", colnames(dt))

  # identify satellite
  setnames(dt, 'spacecraft.id','satellite')

  # parse year and day of year
  dates <- as.POSIXlt(dt$date.acquired, format = '%Y%m%d')
  dt$year <- dates$year+1900
  dt$doy <- dates$yday

  # parse coords
  coords <- stringr::str_extract(string = dt$.geo, pattern = "(?<=\\[).*(?=\\])")
  coords <- matrix(unlist(strsplit(coords, ',')), ncol = 2, byrow = T)
  dt$latitude <- as.numeric(coords[,2])
  dt$longitude <- as.numeric(coords[,1])

  # rename bands seperatly lsat 5/7 and lsat 8
  setkey(dt, 'satellite')
  lsat57.dt <- dt[c("LANDSAT_5", "LANDSAT_7")] # landsat 5 or 7
  lsat57.bands <- c('blue','green','red','nir','swir1','swir2')
  colnames(lsat57.dt)[which(colnames(lsat57.dt) %in% paste('sr.b',c(1:5,7),sep=''))] <- lsat57.bands
  lsat57.dt <- lsat57.dt[, sr.b6 := NULL]
  
  lsat8.dt <- dt["LANDSAT_8"] # landsat 8
  lsat8.bands <- c('ublue','blue','green','red','nir','swir1','swir2')
  colnames(lsat8.dt)[which(colnames(lsat8.dt) %in% paste('sr.b',c(1:7),sep=''))] <- lsat8.bands

  # merge back together
  dt <- rbind(lsat57.dt, lsat8.dt, fill=T)
  
  # select bands to keep / drop 
  keep.bands <- c('blue','green','red','nir','swir1','swir2')
  dt[, 'ublue' := NULL]  
  
  # rescale band values
  scaled.bands.dt <- dt[, ..keep.bands]*0.0000275-0.2
  dt[, (keep.bands) := scaled.bands.dt]

  # rename jrc water extent
  dt <- data.table::setnames(dt, "max.extent", "jrc.water")
  
  # select and reorder cols to keep
  keep.cols <- c('sample.id','latitude','longitude','jrc.water','satellite','year','doy','collection.number','landsat.scene.id',
                 'processing.level','sun.elevation','qa.pixel','qa.radsat','cloud.cover', 'geometric.rmse.model',
                 'blue','green','red','nir','swir1','swir2')

  dt <- dt[, keep.cols, with=F]
  dt <- dt[order(sample.id, year, doy, satellite)]
  dt
}