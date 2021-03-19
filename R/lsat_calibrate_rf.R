#' Cross-Calibrate Landsat Sensors using Random Forests Models
#' 
#' @description 
#' There are systematic differences in spectral indices (e.g., NDVI) among Landsat 5, 7, and 8 (Landsat Collection 1). 
#' It is important to address these differences before assessing temporal trends in spectral data. 
#' Failure to address these differences can, for instance, introduce artificial positive trends into NDVI time-series that are 
#' based on measurements from multiple Landsat sensors (Ju and Masek 2016, Roy et al. 2016, Berner et al. 2020). 
#'
#' This function cross-calibrates individual bands or spectral indices from Landsat 5/8 to match Landsat 7.
#' Landsat 7 is used as a benchmark because it temporally overlaps with the other two sensors.
#' Cross-calibration can only be performed on one band or spectral index at a time.
#' The approach involves determining the typical reflectance at a site during a portion of the growing season using 
#' Landsat 7 and Landsat 5/8 data that were collected the same years. 
#' A random forest model is then trained to predict Landsat 7 reflectance from Landsat 5/8 reflectance.
#' To account for potential seasonal and regional differences between sensors, the random forest models also
#' include as covariates the midpoint of each 15-day period (day of year) and the spatial coordinates of each sampling site. 
#' This approach can handle non-linear relationships, but is most suitable when working with data from 100s to preferably 1000s of sampling sites. 
#'  
#' The specific steps to cross-calibrating sensors include:
#' (1)	Identify the years when both Landsat 7 and Landsat 5/8 measured surface reflectance at a sampling site.
#' (2)	Pool the reflectance measurements across those years and compute 15-day moving median reflectance over the course of the growing season for each sensor and sampling site.
#' (3)	Exclude 15-day periods with fewer than a specified number of measurements from both sets of sensors and then randomly select one remaining 15-day period from each sampling site. 
#' (4)	Split the data into sets for model training and evaluation.
#' (5)	Train Random Forest models that predict Landsat 7 reflectance based on Landsat 5/8 reflectance. The models also account for potential seasonal and regional differences between sensors by including as covariates the midpoint of each 15-day period (day of year) and the spatial coordinates of each sampling site. 
#' (6)	Fit the random forest models using the ranger package. 
#' See Berner et al. (2020) for a full description of the approach. 
#'  
#' @param dt Data.table containing the band or spectral index to cross calibrate.
#' @param band Character string matching the column name of the band or spectral index to cross-calibrate.
#' @param doy.rng Sequence of numbers specifying the Days of Year (Julian Days) to use for model development.
#' @param min.obs Minimum number of paired, seasonally-matched observations from Landsat 7 and Landsat 5/8 required to include a sampling site.   
#' @param frac.train Fraction of sites to use for training the random forest models. The remaining sites are used for model cross-validation.
#' @param outfile.id Identifier used when naming output files. Defaults to the input band, but can be specified if needed such as when performing Monte Carlo simulations.
#' @param outdir Output directory (created if necessary) to which multiple files will be written. 
#' The files include the (1) fitted random forest models as an R object, (2) evaluation data in a csv file, (3) summary of 
#' model cross-validation in a csv file, and (4) multi-panel scatter plot comparing sensors pre- and post-calibration in jpeg format.
#' If cross-calibrating both Landsat 5 and 8, then the function returns files for both sensors.   

#' @return The input data.table with an appended column titled band.xcal, where "band" is your specified band or spectral index 
#' @export lsat_calibrate_rf
#' @examples lsat.dt <- lsat_xcal_rf(lsat.dt, band = 'ndvi', doy.rng = 152:243, min.obs = 5, frac.train = 0.75, outfile.id = 'ndvi', outdir ='data/lsat_site_data/sensor_xcal/ndvi/')

lsat_calibrate_rf <- function(dt, band, doy.rng, min.obs, frac.train = 0.75, outfile.id=band, outdir){
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
    sites.train <- sample(sites, round(n.sites * frac.train), replace = F)
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
    outname <- paste(outdir,'/', outfile.id, '_', i, '_xcal_rf.RData', sep='')
    saveRDS(rf.xcal, outname)
    
    # evaluate model using cross-validation and internal rf metrics, saving summaries to data table
    rf.dat.eval[, eval(paste("LE07", band, 'pred', sep='.')) := predict(rf.xcal, rf.dat.eval)$predictions]
    file.name <- paste(outdir, '/', outfile.id,'_', i, '_xcal_rf_eval_data.csv', sep='')
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
      fig.name <- paste(outdir, '/', outfile.id, '_', i, '_xval_pred_vs_obs.jpg', sep='')
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
    fig.name <- paste(outdir, '/', outfile.id,'_xval_pred_vs_obs.jpg', sep='')
    jpeg(fig.name, 9, 7.5, units = 'in', res=400)
    print(fig)
    dev.off()
  } else {
    print('modify to accommodate plotting more satellites!')
  }
  
  
  # save model evaluation summary table
  write.table(model.eval.df, paste(outdir, '/', outfile.id, '_xcal_rf_eval.csv', sep=''), sep = ',', row.names = F, col.names = T)
  
  # output rf models and updated data table
  dt[satellite == 'LE07', xcal:= get(band)]
  setnames(dt, 'xcal', eval(paste(band, 'xcal', sep='.')))
  dt
}