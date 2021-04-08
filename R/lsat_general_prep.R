#' Prepare Landsat Data
#'
#' @description This function parses the (1) Landsat satellite; (2) Landsat collection; (3) year; and (4) day of year from the Landsat ID.
#' The function also renames and rescales bands 1 - 7, taking into account differences between Landsat 5 / 7 and 8
#' @param dt Data.table containing Landsat data extracted from Google Earth Engine.
#' @return Data.table with formatted and scaled values.
#' @export lsat_general_prep
#'
#' @examples # parsed.dt <- lsat_general_prep(gee.dt)

lsat_general_prep <- function(dt){

  # type cast
  dt <- data.table::data.table(dt)

  # change colunm names to lower case with conjoined words seperated by '.'
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub("_", ".", colnames(dt))

  # parse satellite and collection
  dt$satellite <- substr(dt$landsat.id, 1,4)
  dt$collection <- substr(dt$landsat.id, nchar(as.character(dt$landsat.id))-1, nchar(as.character(dt$landsat.id)))

  # parse year and day of year
  dates <- as.POSIXlt(substr(dt$landsat.id, 18, 25), format = '%Y%m%d')
  dt$year <- dates$year+1900
  dt$doy <- dates$yday
  # dt[,c("landsat.id","id","time"):=NULL] # remove several unnecessary colunms

  # parse coords
  coords <- stringr::str_extract(string = dt$.geo, pattern = "(?<=\\[).*(?=\\])")
  coords <- matrix(unlist(strsplit(coords, ',')), ncol = 2, byrow = T)
  dt$latitude <- as.numeric(coords[,2])
  dt$longitude <- as.numeric(coords[,1])

  # seperate lsat 5 and 7 from lsat 8
  setkey(dt, 'satellite')
  lsat57.dt <- dt[c("LT05", "LE07")] # landsat 5 or 7
  lsat57.bands <- c('blue','green','red','nir','swir1','tir','swir2')
  colnames(lsat57.dt)[which(colnames(lsat57.dt) %in% paste('b',1:7,sep=''))] <- lsat57.bands

  lsat8.dt <- dt["LC08"] # landsat 8
  lsat8.bands <- c('ublue','blue','green','red','nir','swir1','swir2')
  colnames(lsat8.dt)[which(colnames(lsat8.dt) %in% paste('b',1:7,sep=''))] <- lsat8.bands

  # rescale band values
  lsat57.scale.coef <- matrix(rep(c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.1, 0.0001), nrow(lsat57.dt)), nrow = nrow(lsat57.dt), ncol = 7, byrow=T)
  lsat57.band.cols <- colnames(lsat57.dt)[colnames(lsat57.dt) %in% lsat57.bands]
  lsat57.scaled.bands.dt <- lsat57.dt[,..lsat57.band.cols] * lsat57.scale.coef
  lsat57.dt <- cbind(lsat57.scaled.bands.dt, lsat57.dt[,(lsat57.band.cols) := NULL]) # there must be a better way!

  lsat8.scale.coef <- matrix(rep(c(0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001), nrow(lsat8.dt)), nrow = nrow(lsat8.dt), ncol = 7, byrow=T)
  lsat8.band.cols <- colnames(lsat8.dt)[colnames(lsat8.dt) %in% lsat8.bands]
  lsat8.scaled.bands.dt <- lsat8.dt[,..lsat8.band.cols] * lsat8.scale.coef
  lsat8.dt <- cbind(lsat8.scaled.bands.dt, lsat8.dt[,(lsat8.band.cols) := NULL]) # there must be a better way!

  # merge back together
  dt <- rbind(lsat57.dt, lsat8.dt, fill=T)

  # select and reorder cols to keep
  dt <- data.table::setnames(dt, "max.extent", "jrc.water")
  keep.cols <- c('site','latitude','longitude','jrc.water','satellite','year','doy','collection','solar.zenith.angle','pixel.qa','radsat.qa','cloud.cover',
                 'geometric.rmse.model','ublue','blue','green','red','nir','swir1','swir2','tir')

  dt <- dt[, keep.cols, with=F]
  dt <- dt[order(site, year, doy, satellite)]
  dt
}
