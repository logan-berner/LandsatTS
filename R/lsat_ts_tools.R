# ABOUT THIS SCRIT ================================================================================
# This R script contains a set of custom functions for working with site-level Landsat data extracted from Google Earth Engine.
# The functions handle QAQC, derivation of veg indices, sensor cross-calibration, and estimation of max summer veg indices.
# Author: Logan Berner, Northern Arizona University
# Date: 2021-03-09


# LANDSAT PARSE METADATA ================================================================================ 
# This function parses the (1) satellite; (2) landsat collection; (3) year; and (4) day of year
# It also renames and rescales bands 1 - 7, taking into account differences between Landsat 5 / 7 and 8 

lsat_general_prep <- function(dt){
  require(data.table)
  require(dplyr)
  
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
  
  # order cols and rows, then return data table
  dt <- setnames(dt, "max.extent", "jrc.water")
  dt <- setcolorder(dt, c('site','latitude','longitude','jrc.water','satellite','year','doy','collection','solar.zenith.angle','pixel.qa','radsat.qa','cloud.cover',
                          'geometric.rmse.model','ublue','blue','green','red','nir','swir1','swir2','tir'))
  dt <- dt[order(site, year, doy, satellite)]
  dt
}


# LANDSAT QAQC FLAGS ================================================================================
# This function filters out voxels that exhibit:(1) clouds / water / snow; (2) band saturation; 
# (3) scene cloud cover above a user-defined threshold; (4) geometric uncertainty exceeding a user-defined threshold
# https://landsat.usgs.gov/collectionqualityband

# LANDSAT CLEAR SKY BIT VALUES (returns 1 if clear and 0 if not clear)
clear_value = function(x) {
  bit_str = paste(as.integer(intToBits(x)), collapse="") # reverse order of bits, left to right
  # conditions
  fill_val = substr(bit_str, 1, 1) == '1'
  clear = substr(bit_str, 2, 2) == '0'
  cloud_shadow = substr(bit_str, 4, 4) == '1'
  cloud = substr(bit_str, 6, 6) == '1'
  cloud_conf = substr(bit_str, 7, 8) == '11' | substr(bit_str, 7, 8) == '01'
  cirrus_conf = substr(bit_str, 9, 10) == '11' | substr(bit_str, 9, 10) == '01'
  terrain_occ = substr(bit_str, 11, 11) == '1'
  
  if(fill_val | cloud_shadow | cloud | cloud_conf | cirrus_conf | terrain_occ){
    return(0)
  }  else{
    return(1)
  }
}

# filter snow
snow_flag = function(x) {
  # reverse order of bits, left to right
  bit_str = paste(as.integer(intToBits(x)), collapse="")
  snow = substr(bit_str, 5, 5) == '1'
  if(snow){return(1)}  else{return(0)}
}

# filter water
water_flag = function(x) {
  # reverse order of bits, left to right
  bit_str = paste(as.integer(intToBits(x)), collapse="")
  water = substr(bit_str, 3, 3) == '1'
  if(water){return(1)}  else{return(0)}
}

lsat_qaqc_flags <- function(dt, cloud.max=80, geom.max=30, sza.max=60, filter.snow = T, filter.water = T){
  require(data.table)
  n.orig <- nrow(dt)
  
  # pixel flags for clear sky
  dt$clear <- apply(X = data.table(pixel.qa=dt$pixel.qa), MARGIN = 1, FUN = clear_value)
  dt <- dt[clear == 1]
  
  # pixel flags for snow 
  if (filter.snow == T){
    dt$snow <- apply(X = data.table(pixel.qa=dt$pixel.qa), MARGIN = 1, FUN = snow_flag)
    dt <- dt[snow == 0]
  }
  
  # pixel flags for water and JRC Max Water Extent 
  if (filter.water == T){
    dt$water <- apply(X = data.table(pixel.qa=dt$pixel.qa), MARGIN = 1, FUN = water_flag)
    dt <- dt[water == 0]
    
    dt$jrc.water <- as.numeric(dt$jrc.water)
    dt <- dt[jrc.water == 0]
    
  }
  
  # scene flags
  dt <- dt[cloud.cover <= cloud.max & geometric.rmse.model <= geom.max & solar.zenith.angle <= sza.max & radsat.qa == 0]
  
  # filter out unrealistic band values
  dt <- dt[blue > 0.005][green > 0.005][red > 0.005][nir > 0.005]
  dt <- dt[blue < 1][green < 1][red < 1][nir < 1]
  
  n.final <- nrow(dt)
  n.removed <- n.orig - n.final
  print(paste('removed', n.removed, 'of', n.orig, 'observations (', round(n.removed / n.orig * 100, 2), '%)', sep=' '))
  dt
}


# LANDSAT CLEAN NEIGHBORHOOD OF ANOMALIES =============================================================================
# This function assesses whether there are anomalously high or low values among neighboring voxels
# within a site. It then filters out these anomalies using a user-defined z-score threshold. 

zscore <- function(x){(x - mean(x, na.rm=T)) / sd(x, na.rm=T)}

lsat_ngb_qaqc <- function(dt, zscore.lim=2){
  n.orig <- nrow(dt)
  dt[, blue_z:=abs(zscore(blue)), by = c('site', 'year', 'doy', 'satellite')]
  dt[, green_z:=abs(zscore(green)), by = c('site', 'year', 'doy', 'satellite')]  
  dt[, red_z:=abs(zscore(red)), by = c('site', 'year', 'doy', 'satellite')]  
  dt[, nir_z:=abs(zscore(nir)), by = c('site', 'year', 'doy', 'satellite')]  
  dt <- dt[blue_z <= zscore.lim][green_z <= zscore.lim][red_z <= zscore.lim][nir_z <= zscore.lim]
  dt[,c("blue_z","green_z","red_z","nir_z"):=NULL]
  n.final <- nrow(dt)
  n.removed = n.orig - n.final
  print(paste('removed', n.removed, 'of', n.orig, 'observations (', round(n.removed / n.orig * 100, 2), '%)', sep=' '))
  dt
}


# LANDSAT AGGREGATE NEIGHBORHOOD  =============================================================================
# This function computes average reflectance for each band among neighboring voxels 
lsat_ngb_mean <- function(dt){
  dt <- dt[, .(latitude = mean(latitude, na.rm=T),
               longitude = mean(longitude, na.rm=T),
               ublue = mean(ublue, na.rm=T),
               blue = mean(blue, na.rm=T),
               green = mean(green, na.rm=T),
               red = mean(red, na.rm=T),
               nir = mean(nir, na.rm=T),
               swir1 = mean(swir1, na.rm=T),
               swir2 = mean(swir2, na.rm=T),
               tir = mean(tir, na.rm=T)), by = c('site','year','doy','satellite')]
  dt
}


# LANDSAT SPECTRAL INDICES  =============================================================================
# This function computes and adds several common spectral indices

lsat_spec_index <- function(dt, si){
  if (si == 'ndvi'){dt[, ndvi := (nir - red) / (nir + red)]} # Rouse et al. (1974) NASA publication
  if (si == 'nirv'){dt[, nirv := (nir * (nir - red)) / (nir + red)]} # Badgley et al. (2017) Sciences Advances
  if (si == 'evi'){dt[, evi := 2.5  * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)]} # Huete et al. (2002) Remote Sensing of Environment
  if (si == 'evi2'){dt[, evi2 := 2.5  * (nir - red) / (nir + 2.5 * red + 1)]} # Jiang et al. (2008) Remote Sensing of Environment
  if (si == 'nbr'){dt[, nbr := (swir1 - swir2) / (swir1 + swir2)]} # Key and Benson (1999) USGS
  if (si == 'ndwi'){dt[, ndwi := (green - nir) / (green + nir)]} # McFeeters (1996) Int. J. Remote Sensing
  if (si == 'psri'){dt[, psri := (red - blue) / nir]} # Merzlyak et al. (1999) Physiologia Plantarum
  if (si == 'msi'){dt[, msi := swir1 / nir]} # Rock et al. (1986) BioScience
  if (si == 'ndii'){dt[, ndii := (nir - swir1) / (nir + swir1)]} # Hardisky et al. (1983) Photogram. Engineering and Remote Sensing
  if (si == 'ndvsi'){dt[, ndvsi := (swir1 - red) / (swir1 + red)]} # Qi et al (2002) EOS
  if (si == 'satvi'){dt[, satvi := 1.5 * (swir1 - red) / (swir1 + red + 0.5) - swir2/2]} # Marsett et al. (2006) Rangeland Eco. and Management
  if (si == 'kndvi'){dt[, kndvi := tanh(((nir - red) / (nir + red))^2)]} # Camp-Valls et al. (2020) Science Advances
  dt
}
# LANDSAT CROSS CALIBRATION  =============================================================================
# This function cross-calibrates Landsat 5 and 8 to match Landsat 7. The cross-calibration can be performed on any band or spectral metric.

# dt = lsat.dt
# band = 'ndvi'
# doy.rng = 152:243
# min.obs = 5
# frac.eval = 0.33
# i='LC08'
# outdir ='data/lsat_site_data/sensor_xcal/ndvi/'
# outfile.prefix <- paste(band, 'rep',i, sep='_')

lsat_xcal_rf <- function(dt, band, doy.rng, min.obs, frac.eval = 0.33, outfile.prefix, outdir){
  require(data.table)
  require(ggplot2)
  require(ggpubr)
  require(ranger)
  require(R.utils)
  require(zoo)
  require(dplyr)
  mkdirs(outdir)
  dt <- data.table(dt)
  sats <- dt[,unique(satellite)] # which satellites are in the data set? 
  sats <- sats[-which(sats %in% 'LE07')]
  dt$xcal <- numeric() # populate this field later in script
  model.lst <- list()  # list to store random forest models 
  fig.lst <- list() # list to store output figures
  model.eval.df <- data.frame(matrix(data = NA, nrow = length(sats), ncol = 9))
  colnames(model.eval.df) <- c('band','sat','rf.r2','rf.rmse','rf.n','xval.r2','xval.rmse','xval.n', 'xval.bias')
  model.eval.df$sat <- sats 
  
  for (i in sats){
    xcal.dt <- dt[doy %in% doy.rng] # get obs from specified time of year
    xcal.dt <- xcal.dt[satellite == i | satellite == 'LE07'] # get obs for specific satellites
    
    # identify and subset years for which scenes are available from both sensors at each site
    site.yr.dt <- xcal.dt[,.(year = unique(year)), by = .(site, satellite)]
    site.yr.dt <- dcast.data.table(site.yr.dt, site + year ~ satellite, value.var = 'year')
    site.yr.dt <- na.omit(site.yr.dt)
    site.yr.dt <- melt.data.table(site.yr.dt, id = "site", measure = c("LE07",i), variable.name = "satellite", value.name = "year")
    xcal.dt <- xcal.dt[site.yr.dt, on = c('site','year','satellite')]
    
    # identify and subset random 15-day seasonal windows for which obs are available from both sensors at each site
    site.doy.dt <- xcal.dt[, .(n.obs=.N), by = c('site','satellite','doy')]
    full.fac <- data.table(expand.grid('site' = unique(xcal.dt$site), 'satellite' = unique(xcal.dt$satellite), 'doy' = doy.rng))
    site.doy.dt <- site.doy.dt[full.fac, on = c('site','satellite','doy')] # expand to include all possible summer DOYs for each site x satellite 
    site.doy.dt <- setorderv(site.doy.dt, c('site', 'satellite', 'doy'))    
    site.doy.dt <- site.doy.dt[is.na(n.obs), n.obs := 0]
    site.doy.dt <- site.doy.dt[, n.obs.15days := rollapplyr(n.obs, FUN='sum', align = 'center', width=15, partial = T), by = c('site','satellite')] # compute N obs w/in +-7 days of each DOY
    site.doy.dt <- dcast.data.table(site.doy.dt, site + doy ~ satellite, value.var = 'n.obs.15days')
    site.doy.dt <- site.doy.dt[LE07 >= min.obs][get(i) >= min.obs]
    site.doy.dt <- site.doy.dt[, .SD[sample(.N, 1)], by = 'site']  # randomly select one observational period per site 
    site.doy.dt <- site.doy.dt[, c('LE07',i):= NULL]
    site.doy.dt <- setnames(site.doy.dt, 'doy','focal.doy')
    site.doy.win.dt <- data.table(site = site.doy.dt$site, matrix(unlist(lapply(site.doy.dt$focal.doy, function(x){x-seq(-7,7,1)})), ncol = 15, byrow = T)) # create dt with DOYs of 15-day windows for each site
    site.doy.win.dt <- melt.data.table(site.doy.win.dt, id.vars = 'site', value.name = 'doy')
    site.doy.win.dt <- site.doy.win.dt[,variable := NULL]
    xcal.dt <- site.doy.win.dt[xcal.dt, on = c('site','doy'), nomatch=0]
    xcal.dt <- xcal.dt[site.doy.dt, on = 'site']
    
    # compute median band / VI value for the 15-day seasonal window at each site
    coord.dt <- xcal.dt[, .(latitude = mean(latitude, na.rm=T), longitude = mean(longitude, na.rm=T)), by = 'site']
    rf.dat <- xcal.dt[, .(mov.med=median(get(band), na.rm=T)), by = c('site','satellite','focal.doy')]
    rf.dat <- setnames(rf.dat, 'focal.doy','doy')
    rf.dat <- dcast.data.table(rf.dat, site + doy ~ satellite, value.var = 'mov.med')
    rf.dat <- setnames(rf.dat, c('LE07',i), c(paste('LE07',band,sep='.'), band))
    rf.dat <- rf.dat[coord.dt, on = 'site']
    
    # subset training and evaluation data
    sites <- unique(rf.dat$site)
    n.sites <- length(sites)
    sites.train <- sample(sites, round(n.sites * (1-frac.eval)), replace = F)
    sites.eval <- sites[sites %in% sites.train == F]
    rf.dat.train <- rf.dat[site %in% sites.train]
    rf.dat.eval <- rf.dat[site %in% sites.eval]
    
    # fit random forest
    form.rhs <- paste(eval(band), 'doy', 'latitude', 'longitude', sep = ' + ')
    form.lhs <- paste('LE07.', band, ' ~ ', sep='')
    rf.form <- formula(paste(form.lhs, form.rhs, sep=''))
    rf.xcal <- ranger(rf.form, rf.dat.train, importance = 'impurity')
    
    # apply random forest to cross-calibrate satellites
    sat.dt <- dt[satellite == i]
    rf.pred <- predict(rf.xcal, sat.dt)
    dt <- dt[satellite == i, xcal := rf.pred$predictions]
    
    # save model to disk
    outname <- paste(outdir,'/', outfile.prefix, '_', i, '_xcal_rf.RData', sep='')
    saveRDS(rf.xcal, outname)
    
    # evaluate model using cross-validation and internal rf metrics, saving summaries to data table
    rf.dat.eval[, eval(paste("LE07", band, 'pred', sep='.')) := predict(rf.xcal, rf.dat.eval)$predictions]
    file.name <- paste(outdir, '/', outfile.prefix,'_', i, '_xcal_rf_eval_data.csv', sep='')
    fwrite(rf.dat.eval, file.name)
    
    lm.form <- formula(paste("LE07.", band, ' ~ LE07.', band,'.pred', sep=''))
    xval.lm.smry <- summary(lm(lm.form, rf.dat.eval))
    xval.rmse <- as.numeric(rf.dat.eval[, .(rmse = round(sqrt(mean((get(paste('LE07.', band, sep='')) - get(paste('LE07.', band, '.pred', sep='')))^2)),4))])
    xval.bias <- round(sum(rf.dat.eval[[paste('LE07.',band,sep='')]] - rf.dat.eval[[paste('LE07.',band,'.pred', sep='')]]) / nrow(rf.dat.eval),5)
    
    model.eval.df$band <- band
    model.eval.df$rf.r2[model.eval.df$sat == i] <- round(rf.xcal$r.squared,3)
    model.eval.df$rf.rmse[model.eval.df$sat == i] <- round(sqrt(rf.xcal$prediction.error),5)
    model.eval.df$rf.n[model.eval.df$sat == i] <- rf.xcal$num.samples
    model.eval.df$xval.r2[model.eval.df$sat == i] <- round(xval.lm.smry$r.squared,3)
    model.eval.df$xval.rmse[model.eval.df$sat == i] <- xval.rmse
    model.eval.df$xval.bias[model.eval.df$sat == i] <- xval.bias
    model.eval.df$xval.n[model.eval.df$sat == i] <- nrow(rf.dat.eval)
    
    # plot obs vs obs and obs vs predicted band values 
    axis.min <- rf.dat.eval[, .(min1 = min(get(paste('LE07.', band, sep=''))), min2 = min(get(paste('LE07.', band, '.pred', sep=''))))]
    axis.min <- min(as.numeric(axis.min))
    axis.max <- rf.dat.eval[, .(max1 = max(get(paste('LE07.', band, sep=''))), max2 = max(get(paste('LE07.', band, '.pred', sep=''))))]
    axis.max <- max(as.numeric(axis.max))
    axis.lim <- c(axis.min, axis.max)
    
    if(i == 'LT05'){
      uncal.xlab <- bquote('Landsat 5 '~.(toupper(band))~'(Raw)')
      cal.xlab <- bquote('Landsat 5 '~.(toupper(band))~'(Cross-Calibrated)')
    } else if (i == 'LC08'){
      uncal.xlab <- bquote('Landsat 8 '~.(toupper(band))~'(Raw)')
      cal.xlab <- bquote('Landsat 8 '~.(toupper(band))~'(Cross-Calibrated)')
    }
    
    lsat7.ylab <- bquote('Landsat 7 '~.(toupper(band)))
    pred <- paste('LE07',band,sep='.')
    obs <- paste('LE07',band,'pred',sep='.')
    
    # raw figure
    fig.raw <- ggplot(rf.dat.eval, aes_string(x = band, y = obs))
    fig.raw <- fig.raw + geom_bin2d(binwidth=c(0.01,0.01)) + geom_abline(color='orange') + scale_fill_viridis_c()
    fig.raw <- fig.raw + theme_bw() + labs(y=lsat7.ylab, x=uncal.xlab) + coord_cartesian(ylim=c(axis.min, axis.max), xlim=c(axis.min, axis.max))
    fig.raw <- fig.raw + theme(legend.position="right", 
                               axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
    
    # cal figure
    fig.cal <- ggplot(rf.dat.eval, aes_string(x = pred, y = obs))
    fig.cal <- fig.cal + geom_bin2d(binwidth=c(0.01,0.01)) + geom_abline(color='orange') + scale_fill_viridis_c()
    fig.cal <- fig.cal + theme_bw() + labs(y=lsat7.ylab, x=cal.xlab) + coord_cartesian(ylim=c(axis.min, axis.max), xlim=c(axis.min, axis.max))
    fig.cal <- fig.cal + theme(legend.position="right", 
                               axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
    
    # combine figures
    fig <- ggarrange(fig.raw, fig.cal, ncol = 2)
    
    # write out fig or save to list to write later if calibrating more than one satellite
    if (length(sats) == 1){
      fig.name <- paste(outdir, '/', outfile.prefix, '_', i, '_xval_pred_vs_obs.jpg', sep='')
      jpeg(fig.name, 4.5, 4, units = 'in', res=400)
      print(fig)
      dev.off()
    } else {
      fig.lst[[i]] <- fig
    }
  }
  
  # write out composite figure
  if (length(sats) == 2){
    fig <- ggarrange(fig.lst[[1]], fig.lst[[2]], ncol = 1, nrow = 2, labels = c('(a)','(b)'), vjust=0.9)
    fig.name <- paste(outdir, '/', outfile.prefix,'_xval_pred_vs_obs.jpg', sep='')
    jpeg(fig.name, 9, 7.5, units = 'in', res=400)
    print(fig)
    dev.off()
  } else {
    print('modify to accommodate plotting more satellites!')
  }
  
  
  # save model evaluation summary table
  write.table(model.eval.df, paste(outdir, '/', outfile.prefix, '_xcal_rf_eval.csv', sep=''), sep = ',', row.names = F, col.names = T)
  
  # output rf models and updated data table
  dt[satellite == 'LE07', xcal:= get(band)]
  setnames(dt, 'xcal', eval(paste(band, 'xcal', sep='.')))
  dt
}


# LANDSAT PHENOLOGY  =============================================================================
# This function fits seasonal curves to NDVI (or other veg index) data to characterize typical land surface phenology at a site.
# The function returns information about typical phenology at a site and about the timing of an observation relative to this phenology.

# dt = lsat.dt
# vi = 'ndvi'
# window.yrs=7
# window.min.obs=15
# spar=0.7
# spl.fit.outfile=NA

lsat_pheno = function(dt, vi, window.yrs=15, window.min.obs=30, spar=0.7, pcnt.dif.thresh=100, spl.fit.outfile=F, progress=T){
  # LOAD LIBRARIES
  require(data.table)
  
  # GET SITE, DOY, YEAR, AND VEG INDEX FROM INPUT DATA FRAME
  dt <- dt[, eval(c('site','latitude','longitude','year','doy',vi)), with=F]
  dt <- setnames(dt, vi, 'vi')
  dt <- dt[order(site,doy)]
  
  # IDENTIFY TIME PERIODS 
  all.yrs <- sort(unique(dt$year))
  focal.yrs <- all.yrs[-c(1:(round(window.yrs/2)), (length(all.yrs)-round(window.yrs/2)+1):length(all.yrs))]
  n.focal.yrs <- length(focal.yrs)
  
  # CREATE LISTS FOR TEMPORARY STORAGE
  data.list <- list()
  splines.list <- list()
  
  # LOOP THROUGH FOCAL YEARS, FITTING SPLINE FOR EACH SITE
  for (i in 1:n.focal.yrs){
    # SUBSET OBS FROM FOCAL PERIOD    
    focal.yr <- focal.yrs[i]
    focal.win <- seq(focal.yr-round(window.yrs/2), focal.yr+round(window.yrs/2))
    focal.dt <- dt[year %in% focal.win]
    focal.dt <- focal.dt[order(site,doy)]
    
    # COMPUTE NUMBER OF OBS DURING FOCAL PERIOD FOR EACH SITE AND EXCLUDE SITES WITH FEWER THAN SOME USE-SPECIFIC THRESHOLD
    focal.dt <- focal.dt[, n.obs.focal.win := .N, by = 'site']
    focal.dt <- focal.dt[n.obs.focal.win >= window.min.obs]
    
    # if (nrow(focal.dt) == 0){next()}
    
    doy.rng <- min(focal.dt$doy):max(focal.dt$doy)
    
    # FIT SPLINE TO SEASONAL TIME SERIES AT EACH SITE
    splines.dt <- focal.dt[, .(spl.fit = list(smooth.spline(doy, vi, spar = spar))), by = 'site']
    spline.fits.dt <- splines.dt[, .(spl.fit = unlist(Map(function(mod,doy){predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'site']
    spline.fits.dt <- spline.fits.dt[, doy := doy.rng, by = 'site']
    
    # ITERATIVE QUALITY CHECK: LOOK FOR LARGE DIFFS BETWEEN OBS AND FITTED VALUES, DROP OBS WITH TOO LARGE A DIFF, REFIT SPLINES, RECHECK
    refitting = 1
    # ii=1
    while(refitting == 1){
      focal.dt <- spline.fits.dt[focal.dt, on = c('site','doy')] 
      focal.dt <- focal.dt[, abs.pcnt.dif := abs((vi - spl.fit)/((vi+spl.fit)/2)*100)] # calc abs % dif
      refit.sites <- unique(focal.dt[abs.pcnt.dif > pcnt.dif.thresh]$site)
      focal.dt <- focal.dt[abs.pcnt.dif <= pcnt.dif.thresh]
      focal.dt <- focal.dt[, c('spl.fit', 'abs.pcnt.dif'):= NULL]
      refit.dt <- focal.dt[site %in% refit.sites]
      
      # REFIT SPLINES AT SITES THAT HAD LARGE DIFFS BETWEEN OBS AND FITTED VALUES
      if (length(refit.sites) > 0){
        # set aside the splines that don't need to be refit
        spline.fits.dt <- spline.fits.dt[site %in% refit.sites == F]
        
        # check the number of obs per site and filter sites out those with too few obs
        refit.dt <- refit.dt[, n.obs.focal.win := .N, by = 'site']
        refit.dt <- refit.dt[n.obs.focal.win >= window.min.obs]
        
        # refit
        spline.refits.dt <- refit.dt[, .(spl.fit = list(smooth.spline(doy, vi, spar = spar))), by = 'site']
        spline.refits.dt <- spline.refits.dt[, .(spl.fit = unlist(Map(function(mod,doy){predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'site']
        spline.refits.dt <- spline.refits.dt[, doy := doy.rng, by = 'site']
        spline.fits.dt <- rbind(spline.fits.dt, spline.refits.dt)
      } else {
        refitting = 0
      }
    } # end of refitting
    
    # CALCULATE SEVERAL PHENOLOGY METRICS FOR EACH SITE
    site.doy.smry <- focal.dt[, .(min.doy = min(doy), max.doy = max(doy)), by = 'site'] # identify DOY range for each site
    spline.fits.dt <- spline.fits.dt[site.doy.smry, on = 'site'] 
    spline.fits.dt <- spline.fits.dt[doy >= min.doy][doy <= max.doy] # limit spline fit to DOY range at each site
    spline.fits.dt <- spline.fits.dt[, spl.fit.max := max(spl.fit), by = 'site'] # compute max vi typically observed at a site
    spline.fits.dt <- spline.fits.dt[, spl.fit.max.doy := doy[which.max(spl.fit)], by = 'site'] # calculate typcial day of peak greenness
    spline.fits.dt <- spline.fits.dt[, spl.frac.max := spl.fit / spl.fit.max]
    spline.fits.dt <- spline.fits.dt[, vi.adjustment := abs(spl.fit - spl.fit.max)] # compute adjustment factor
    spline.fits.dt <- spline.fits.dt[, focal.yr := focal.yr]
    spline.fits.dt <- spline.fits.dt[, c('min.doy','max.doy'):= NULL]
    splines.list[[i]] <- spline.fits.dt
    
    # ADD PHENOLOGY METRICS TO FOCAL DATA
    focal.dt <- spline.fits.dt[focal.dt, on = c('site','doy')]
    focal.dt <- focal.dt[, vi.max.pred := vi + vi.adjustment]
    focal.dt <- focal.dt[, c('focal.yr') := NULL]
    setnames(focal.dt, c('n.obs.focal.win'),c('spl.fit.n.obs'))
    
    # ADD PHENOLOGY DATA TO MAIN DATA TABLE 
    if (i == 1){
      yr.win <- c((focal.yr-(round(window.yrs/2))):focal.yr)
      data.list[[i]] <- focal.dt[year %in% yr.win]
    } else if (i == n.focal.yrs) {
      yr.win <- c(focal.yr:(focal.yr+round(window.yrs/2)))
      data.list[[i]] <- focal.dt[year %in% yr.win]
    } else {
      data.list[[i]] <- focal.dt[year == focal.yr]
    }
    
    # PRINT STATUS (if requested)
    if (progress == T){print(paste('focal year: ', focal.yr, '(', round(i/n.focal.yrs,2)*100, '% finished)', sep=''))}
    
  } # end focal year loop
  
  # WRITE OUT FILE WITH SPLINE FITS (OPTIONAL)
  if (spl.fit.outfile != F){
    spline.dt <- data.table(rbindlist(splines.list))
    fwrite(spline.dt, spl.fit.outfile)
  }
  
  # OUTPUT DATA TABLE
  dt <- data.table(rbindlist(data.list))
  dt <- dt[order(site,year,doy)]
  setcolorder(dt, c('site','latitude','longitude','year','doy','spl.fit.n.obs','spl.fit','spl.frac.max','spl.fit.max','spl.fit.max.doy','vi.adjustment','vi','vi.max.pred'))
  colnames(dt) <- gsub('vi',vi,colnames(dt))
  dt
}

# LANDSAT PHENOLOGICAL MAXIMUM ========================================================================
# This function estimates annual maximum NDVI using individual observations 
# and info an typcial land surface at the site

lsat_pheno_max = function(dt, vi, min.frac.of.max = 0.75, zscore.thresh = 3){
  require(data.table)
  colnames(dt) <- gsub(vi, 'vi', colnames(dt))
  
  # take obervations from the 'growing season' identified as the period when vi typically exceedes a specified fraction of the typical max vi 
  dt <- dt[spl.frac.max >= min.frac.of.max]
  
  # identify and filter out obs-level predictions of max VI that are anomalously high or low relative to other obs from that site x year
  dt <- dt[, ':='(avg = mean(vi.max.pred), sd = sd(vi.max.pred), n=.N), by = c('site','year')]
  dt <- dt[, abs.zscore := abs((vi.max.pred - avg )/sd)]
  dt <- dt[abs.zscore <= zscore.thresh]
  
  #  estimate max summer VI
  dt.smry <- dt[,.(latitude = first(latitude), longitude = first(longitude), n.obs = .N, 
                   vi.gs.med = median(vi), vi.gs.q90 = quantile(vi, 0.9),
                   vi.max = median(vi.max.pred),
                   vi.max.lwr = min(vi.max.pred), vi.max.upr = max(vi.max.pred),
                   pos.doy = mean(spl.fit.max.doy)), 
                by = c('site','year')]
  
  # predicted max can't be lower than observed 90th pecentile
  dt.smry[vi.max < vi.gs.q90, vi.max := vi.gs.q90]
  
  #output
  colnames(dt.smry) <- gsub("vi", vi, colnames(dt.smry))
  dt.smry
}



# EVALUATE ESTIMATES OF PHENOLOGICAL MAXIMUM ===========================================================
# Assess how scene count affects estimates of peak summer NDVI derived from raw observations and 
# observations that were 'phenologically corrected'

lsat_pheno_max_eval <- function(dt, vi, min.frac.of.max = 0.75, min.obs = 10, reps = 10, outdir = 'output/pheno_max_eval/', outfile.suffix){  
  require(data.table)
  require(ggplot2)
  require(R.utils)
  
  colnames(dt) <- gsub(vi, 'vi', colnames(dt))
  
  # if desired, only use observations from the growing season
  dt <- dt[spl.frac.max >= min.frac.of.max]
  
  # get site x years with atleast the min number of observations specificed
  dt <- dt[, n.obs.gs := .N, by = c('site','year')]
  dt <- dt[n.obs.gs > min.obs]
  
  # identify and filter out obs-level predictions of max VI that are anomalously high or low relative to other obs from that site x year 
  dt <- dt[, ':='(avg = mean(vi.max.pred), sd = sd(vi.max.pred), n=.N), by = c('site','year')]
  dt <- dt[, abs.zscore := abs((vi.max.pred - avg )/sd)]
  dt <- dt[abs.zscore < 2]
  
  # compute max observed VI (actually 90% percentile to avoid spuriously high values)
  dt <- dt[, vi.max.obs := quantile(vi, 0.90), by = c('site','year')]
  
  # iteratively loop through sample size and reps  
  out.list <- list()
  cnt=1
  for (i in 1:(min.obs-1)){
    for (j in 1:reps){
      rep.dt <- dt[,.SD[sample(.N, i)], by=c('site','year')]
      rep.dt <- rep.dt[,':='(n.obs=i, rep=j)]             
      rep.dt <- rep.dt[, .(vi.max.obs = first(vi.max.obs), vi.max.uncor = quantile(vi,0.9), vi.max.cor = median(vi.max.pred)), by = c('n.obs','rep','site','year')]
      rep.dt <- rep.dt[vi.max.cor < vi.max.uncor, vi.max.cor := vi.max.uncor]
      rep.dt[, vi.uncor.pcntdif := (vi.max.uncor - vi.max.obs)/vi.max.obs*100]
      rep.dt[, vi.cor.pcntdif := (vi.max.cor - vi.max.obs)/vi.max.obs*100]
      out.list[[cnt]] <- rep.dt
      cnt = cnt + 1 
    }
  }
  
  eval.dt <- rbindlist(out.list)
  eval.dt <- na.omit(eval.dt)
  
  # summarize across iterations
  eval.smry.dt <- eval.dt[,.(vi.uncor.pcntdif.med = median(vi.uncor.pcntdif, na.rm=T), vi.cor.pcntdif.med = median(vi.cor.pcntdif, na.rm=T)), by = c('site','year','n.obs')]
  eval.smry.dt <- melt.data.table(eval.smry.dt, id.vars=c('site','year','n.obs'), value.name='pcnt.dif', variable.name='correction')
  eval.smry.dt$n.obs.fac <- as.factor(eval.smry.dt$n.obs)
  eval.smry.dt$correction <- factor(eval.smry.dt$correction, labels = c('Raw','Corrected'))
  
  # EVALUATION PLOT
  ylab.pcntdif <- bquote("Difference from observed "~.(toupper(gsub('.xcal','',vi)))['max']~" (%)")
  fig <- ggplot(eval.smry.dt, aes(n.obs.fac, pcnt.dif, fill=correction)) + geom_boxplot(outlier.size=0.7, outlier.color='gray')
  fig <- fig + theme_bw() + labs(y=ylab.pcntdif, x='Number of observations')
  fig <- fig + theme(legend.position="right", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) 
  
  # output
  colnames(eval.smry.dt) <- gsub('vi', vi, colnames(eval.smry.dt))
  mkdirs(outdir)
  
  fig.outname <- paste0(outdir,'pheno_max_eval_',outfile.suffix,'.jpg')
  jpeg(fig.outname, width = 6, height = 4, res = 400, units = 'in')
  print(fig)
  dev.off()
  
  data.outname <- paste0(outdir,'pheno_max_eval_',outfile.suffix,'.csv')
  fwrite(eval.smry.dt, data.outname)
  
  eval.smry.dt
}

# END SCRIPT ===========================================================================
