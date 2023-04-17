#' Cross-calibrate Landsat sensors using polynomial regression
#'
#' @description
#' There are systematic differences in spectral indices (e.g., NDVI) among Landsat 5, 7, and 8 
#' (Landsat Collection 2). It is important to address these differences before assessing temporal
#' trends in spectral data. Failure to address these differences can, for instance, introduce
#' artificial positive trends into NDVI time-series that are based on measurements from multiple
#' Landsat sensors (Ju and Masek 2016, Roy et al. 2016, Berner et al. 2020). This function
#' cross-calibrates individual bands or spectral indices from Landsat 5/8 to match Landsat 7.
#' Landsat 7 is used as a benchmark because it temporally overlaps with the other two sensors.
#' Cross-calibration can only be performed on one band or spectral index at a time. The approach
#' involves determining the typical reflectance at a sample during a portion of the growing
#' season site using Landsat 7 and Landsat 5/8 data that were collected the same years. 
#' Polynomial regression models from first to third order are trained to predict Landsat 7 reflectance 
#' from Landsat 5/8 reflectance, the most parsimonious model is selected
#' using BIC, and then that model is used to cross-calibrate the data.  
#' This approach is most suitable when working with data from 100s to preferably 1000s
#' of samples.
#'
#' The specific steps to cross-calibrating sensors include:
#' (1) Identify the years when both Landsat 7 and Landsat 5/8 measured surface reflectance
#'       at a sample.
#' (2) Pool the reflectance measurements across those years and compute 15-day moving median
#'       reflectance over the course of the growing season for each sensor and sampling sample.
#' (3) Exclude 15-day periods with fewer than a specified number of measurements from both
#'       sets of sensors and then randomly select one remaining 15-day period from each 
#'       sample.
#' (4) Split the data into sets for model training and evaluation.
#' (5) Train polynomial regression models that predict Landsat 7 reflectance based on Landsat 5/8 
#'       reflectance. Model order (1st to 3rd) is selected using BIC.  
#' (6) Apply the polynomial regression models to cross-calibrate measurements.
#' 
#' See Berner et al. (2020) for a full description of the approach.
#'
#' @param dt Data.table containing the band or spectral index to cross-calibrate.
#' @param band.or.si Character string matching the column name of the band or spectral 
#'    index to cross-calibrate.
#' @param doy.rng Sequence of numbers specifying the Days of Year 
#'    to use for model development.
#' @param min.obs Minimum number of paired, seasonally-matched observations 
#'    from Landsat 7 and Landsat 5/8 required to include a sampling sample.
#' @param train.with.highlat.data (True/False) Should the RF models be trained using an 
#'    internal high-latitude dataset that sampled the Arctic and Boreal biomes? 
#' @param frac.train Fraction of samples to use for training the random forest models.
#'    The remaining samples are used for model cross-validation.
#' @param trim (True/False) If true, then for each sample site the percent difference 
#'    in spectral indices between satellites is determined. The lowest 2.5 and highest 97.5 
#'    percentiles are then trimmed. This is meant to reduce potential differences that 
#'    are not directly attributable to the sensors, but rather to exogenous factors.    
#' @param overwrite.col (True/False) Overwrite existing column or (by default)
#'    append cross-calibrated data as a new column?
#' @param write.output (True/False) Should RF models and evaluation content be written to disk?
#'    Either way, evaluation table and figure are printed in the console.  
#' @param outfile.id Identifier used when naming output files. Defaults to the input band,
#'    but can be specified if needed such as when performing Monte Carlo simulations.
#' @param outdir Output directory (created if necessary) to which multiple files will be written.
#'    The files include:
#'    (1) fitted random forest models as R objects,
#'    (2) evaluation data in a csv file,
#'    (3) summary of model cross-validation in a csv file, and 
#'    (4) multi-panel scatter plot comparing sensors pre- and post-calibration in jpeg format.
#' If cross-calibrating both Landsat 5 and 8, then the function returns files for both sensors.

#' @return The input data.table with an appended column titled band.xcal, where "band" 
#'    is your specified band or spectral index.
#' @import data.table
#' @export lsat_calibrate_poly
#' @examples
#' data(lsat.example.dt)
#' lsat.dt <- lsat_format_data(lsat.example.dt)
#' lsat.dt <- lsat_clean_data(lsat.dt)
#' lsat.dt <- lsat_calc_spectral_index(lsat.dt, 'ndvi')
#' lsat.dt <- lsat_calibrate_poly(lsat.dt, band.or.si = 'ndvi', 
#'                              train.with.highlat.data = TRUE, 
#'                              write.output = FALSE)
#' lsat.dt

lsat_calibrate_poly <- function(dt,
                                band.or.si,
                                doy.rng = 152:243,
                                min.obs = 5,
                                train.with.highlat.data = F,
                                frac.train = 0.75,
                                trim = T,
                                overwrite.col = F,
                                write.output = F,
                                outfile.id=band.or.si,
                                outdir = NA){
  
  # IDENTIFY SATELLITES, BUILD OUTPUT LISTS AND DATA FRAMES, ETC ==============
  dt <- data.table::data.table(dt)
  sats <- dt[,unique(satellite)] # which satellites are in the data set?
  sats <- sats[-which(sats %in% 'LANDSAT_7')]
  dt$xcal <- numeric() # populate this field later in script
  model.lst <- list()  # list to store nonlinear models
  fig.lst <- list() # list to store output figures
  
  # create data.frame to store polynomial model coefficients
  model.coef.df <- data.frame(matrix(data = NA, nrow = length(sats), ncol = 10))
  colnames(model.coef.df) <- c('satellite','band.or.si','B0','B1','B2','B3',
                               'B0.se','B1.se','B2.se','B3.se')
  model.coef.df$satellite <- sats
  
  # create data.frame to store model evaluation metrics
  model.eval.df <- data.frame(matrix(data = NA, nrow = length(sats), ncol = 11))
  colnames(model.eval.df) <- c('satellite','band.or.si','train.n.sites','eval.n.sites','r2',
                               'uncal.rmse','uncal.bias','uncal.bias.pcnt',
                               'xcal.rmse','xcal.bias','xcal.bias.pcnt')
  model.eval.df$satellite <- sats
  
  # create output directory if writing output
  if (write.output == T){
    R.utils::mkdirs(outdir)
  }
  
  # BEGIN CROSS-CALIBRATION ==================================================
  # i = 'LANDSAT_5'
  for (i in sats){
    if (train.with.highlat.data == T){
      # if true, fit random forest models using internal data
      xcal.dt <- lsat.xcal.dt[satellite == i | satellite == 'LANDSAT_7']
      xcal.dt <- xcal.dt[doy %in% doy.rng] # get obs from specified time of year
      
    } else {
      # if false, use user's sample for model fitting
      xcal.dt <- dt[satellite == i | satellite == 'LANDSAT_7'] # get obs for specific satellites
      xcal.dt <- xcal.dt[doy %in% doy.rng] # get obs from specified time of year
      
      # identify and subset years for which scenes are available from both sensors at each sample
      sample.yr.dt <- xcal.dt[,.(year = unique(year)), by = .(sample.id, satellite)]
      sample.yr.dt <- data.table::dcast.data.table(sample.yr.dt, 
                                                   sample.id + year ~ satellite, 
                                                   value.var = 'year')
      sample.yr.dt <- na.omit(sample.yr.dt)
      sample.yr.dt <- data.table::melt.data.table(sample.yr.dt, 
                                                  id = "sample.id", 
                                                  measure = c("LANDSAT_7",i),
                                                  variable.name = "satellite",
                                                  value.name = "year")
      
      xcal.dt <- xcal.dt[sample.yr.dt, on = c('sample.id','year','satellite')]
      
      # check whether there are an adequate number of samples; if not, then stop or warn.
      n.sites <- length(unique(xcal.dt$sample.id))
      if (n.sites == 0){
        stop.msg <- strwrap(paste0('Your dataset does not have any sample sites with enough temporally 
                             overlapping measurements from LANDSAT_7 and ', i,'. Therefore, there 
                             is not enough data to cross-calibrate the sensors. Either set min.obs 
                             lower, set train.with.highlat.data = T if working in the Arctic
                             or Boreal biome, or extract Landsat data from more sample sites... 
                             The function will now stop.'),
                            prefix = ' ', initial = '')
        
        dt
        stop(stop.msg)
      }
      
      if (n.sites < 100){
        warning.msg <- strwrap(paste0('Your dataset has ', n.sites, ' sample sites with enough temporally 
                             overlapping measurements from LANDSAT_7 and ', i,'. This might not be
                             enough data to rigorously train and evaluate the model. Please carefully 
                             inspect the model evaluation output to ensure it is adequate. If not
                             adequate, then set min.obs lower, set train.with.highlat.data = T if 
                             working in the Arctic or Boreal biome, or extract Landsat data from 
                             more sample sites...'),
                               prefix = ' ', initial = '')
        
        warning(warning.msg)
      }
      
    }
    
    # identify and subset random 15-day seasonal windows for which obs are available 
    # from both sensors at each sample
    sample.doy.dt <- xcal.dt[, .(n.obs=.N), by = c('sample.id','satellite','doy')]
    full.fac <- data.table::data.table(expand.grid('sample.id' = unique(xcal.dt$sample.id),
                                                   'satellite' = unique(xcal.dt$satellite),
                                                   'doy' = doy.rng))
    sample.doy.dt <- sample.doy.dt[full.fac, on = c('sample.id','satellite','doy')]
    sample.doy.dt <- data.table::setorderv(sample.doy.dt, c('sample.id', 'satellite', 'doy'))
    sample.doy.dt <- sample.doy.dt[is.na(n.obs), n.obs := 0]
    
    # compute N obs w/in +-7 days of each DOY
    sample.doy.dt <- sample.doy.dt[, n.obs.15days := zoo::rollapplyr(n.obs, 
                                                                     FUN='sum',
                                                                     align = 'center',
                                                                     width=15,
                                                                     partial = T),
                                   by = c('sample.id','satellite')]
    
    sample.doy.dt <- data.table::dcast.data.table(sample.doy.dt, 
                                                  sample.id + doy ~ satellite,
                                                  value.var = 'n.obs.15days')
    
    sample.doy.dt <- sample.doy.dt[LANDSAT_7 >= min.obs][get(i) >= min.obs]
    
    # randomly select one observational period per sample
    sample.doy.dt <- sample.doy.dt[, .SD[sample(.N, 1)], by = 'sample.id']
    sample.doy.dt <- sample.doy.dt[, c('LANDSAT_7',i):= NULL]
    sample.doy.dt <- data.table::setnames(sample.doy.dt, 'doy','focal.doy')
    
    # create dt with DOYs of 15-day windows for each sample
    sample.doy.win.dt <- data.table::data.table(sample.id = sample.doy.dt$sample.id, 
                                                matrix(unlist(lapply(sample.doy.dt$focal.doy, 
                                                                     function(x){x-seq(-7,7,1)})), 
                                                       ncol = 15, byrow = T))
    
    sample.doy.win.dt <- data.table::melt.data.table(sample.doy.win.dt,
                                                     id.vars = 'sample.id',
                                                     value.name = 'doy')
    
    sample.doy.win.dt <- sample.doy.win.dt[,variable := NULL]
    xcal.dt <- sample.doy.win.dt[xcal.dt, on = c('sample.id','doy'), nomatch=0]
    xcal.dt <- xcal.dt[sample.doy.dt, on = 'sample.id']
    
    # if using internal data to calibrate a spectral index, 
    # then derive the required spectral index
    if (train.with.highlat.data == T){
      if (band.or.si %in% c('blue','green','red','nir','swir1','swir2') == F){
        xcal.dt <- lsat_calc_spectral_index(xcal.dt, band.or.si)
      } 
    }
    
    # compute median band.or.si / VI value for the 15-day seasonal window at each sample
    nlm.data.dt <- xcal.dt[, .(mov.med=median(get(band.or.si), na.rm=T)),
                           by = c('sample.id','satellite','focal.doy')]
    nlm.data.dt <- data.table::setnames(nlm.data.dt, 'focal.doy','doy')
    nlm.data.dt <- data.table::dcast.data.table(nlm.data.dt,
                                                sample.id + doy ~ satellite,
                                                value.var = 'mov.med')
    nlm.data.dt <- data.table::setnames(nlm.data.dt,
                                        c('LANDSAT_7',i),
                                        c(paste('LANDSAT_7', band.or.si,sep='.'), band.or.si))
    
    # screen out values with greater than a user-specified percent difference
    if (trim == T){
      nlm.data.dt[, dif.pcnt := get(band.or.si) - get(paste('LANDSAT_7',band.or.si,sep='.'))]
      nlm.data.dt[, dif.pcnt.rank := dplyr::percent_rank(dif.pcnt)]
      nlm.data.dt <- nlm.data.dt[dif.pcnt.rank > 0.025 & dif.pcnt.rank < 0.975]
      nlm.data.dt <- nlm.data.dt[, dif.pcnt := NULL]
      nlm.data.dt <- nlm.data.dt[, dif.pcnt.rank := NULL]
    }
    
    # subset samples for training and evaluation data
    samples <- unique(nlm.data.dt$sample.id)
    n.samples <- length(samples)
    
    samples.train <- sample(samples, round(n.samples * frac.train), replace = F)
    samples.eval <- samples[samples %in% samples.train == F]
    
    train.dt <- nlm.data.dt[sample.id %in% samples.train]
    eval.dt <- nlm.data.dt[sample.id %in% samples.eval]
    
    # fit polynomial regression models using BIC to most parsimonious model up to 3rd order
    form.lhs <- paste('LANDSAT_7.', band.or.si, ' ~ ', sep='')
    poly.lm.lst <- list()
    aic.lst <- list()
    for (ii in 1:3){
      # form.rhs <- paste('poly(',eval(band.or.si),',', ii,')', sep = '')
      form.rhs <- paste('poly(',eval(band.or.si),',', ii,', raw = T)', sep = '')
      model.form <- stats::formula(paste(form.lhs, form.rhs, sep=''))
      poly.lm <- lm(model.form, data = train.dt)
      aic.lst[[ii]] <- BIC(poly.lm)
      poly.lm.lst[[ii]] <- poly.lm
    }
    poly.lm <- poly.lm.lst[[which.min(aic.lst)]]
    poly.coef <- broom::tidy(poly.lm)
    poly.coef[,2:3] <- round(poly.coef[,2:3],4)
    
    # store model coefficients
    model.coef.df$band.or.si <- band.or.si
    model.coef.df$B0[model.coef.df$satellite == i] <- poly.coef$estimate[1]
    model.coef.df$B1[model.coef.df$satellite == i] <- poly.coef$estimate[2]
    model.coef.df$B2[model.coef.df$satellite == i] <- poly.coef$estimate[3]
    model.coef.df$B3[model.coef.df$satellite == i] <- poly.coef$estimate[4]
    model.coef.df$B0.se[model.coef.df$satellite == i] <- poly.coef$std.error[1]
    model.coef.df$B1.se[model.coef.df$satellite == i] <- poly.coef$std.error[2]
    model.coef.df$B2.se[model.coef.df$satellite == i] <- poly.coef$std.error[3]
    model.coef.df$B3.se[model.coef.df$satellite == i] <- poly.coef$std.error[4]
    
    # apply regression model to cross-calibrate satellites
    sat.dt <- dt[satellite == i]
    poly.pred <- stats::predict(poly.lm, sat.dt)
    dt <- dt[satellite == i, xcal := poly.pred]
    
    # evaluate model using cross-validation, saving summaries to data table
    eval.dt[, eval(paste("LANDSAT_7", band.or.si, 'pred', sep='.')) := 
              stats::predict(poly.lm, eval.dt)]
    
    target.vals <- eval.dt[[paste('LANDSAT_7.',band.or.si,sep='')]]
    uncal.vals <- eval.dt[[band.or.si]]
    xcal.vals <- eval.dt[[paste('LANDSAT_7.', band.or.si, '.pred', sep='')]]
    
    uncal.rmse <- round(sqrt(mean((target.vals - uncal.vals)^2)),3)
    uncal.bias <- round(median(uncal.vals - target.vals), 3)
    uncal.bias.pcnt <- round(median((uncal.vals - target.vals) / target.vals * 100), 1)
    
    lm.form <- stats::formula(paste("LANDSAT_7.", band.or.si, ' ~ LANDSAT_7.',
                                    band.or.si,'.pred', sep=''))
    xcal.lm.smry <- summary(stats::lm(lm.form, eval.dt))
    
    xcal.rmse <- round(sqrt(mean((target.vals - xcal.vals)^2)),3)
    xcal.bias <- round(median(xcal.vals - target.vals), 3)
    xcal.bias.pcnt <- round(median((xcal.vals - target.vals) / target.vals * 100), 1)
    
    model.eval.df$band.or.si[model.eval.df$satellite == i] <- band.or.si
    model.eval.df$train.n.sites[model.eval.df$satellite == i] <- nrow(train.dt)
    model.eval.df$eval.n.sites[model.eval.df$satellite == i] <- nrow(eval.dt)
    model.eval.df$r2[model.eval.df$satellite == i] <- round(xcal.lm.smry$r.squared,3)
    model.eval.df$uncal.rmse[model.eval.df$satellite == i] <- uncal.rmse
    model.eval.df$uncal.bias[model.eval.df$satellite == i] <- uncal.bias
    model.eval.df$uncal.bias.pcnt[model.eval.df$satellite == i] <- uncal.bias.pcnt 
    model.eval.df$xcal.rmse[model.eval.df$satellite == i] <- xcal.rmse
    model.eval.df$xcal.bias[model.eval.df$satellite == i] <- xcal.bias
    model.eval.df$xcal.bias.pcnt[model.eval.df$satellite == i] <- xcal.bias.pcnt
    
    
    # plot obs vs obs and obs vs predicted band.or.si values
    axis.min <- eval.dt[, .(min1 = min(get(paste('LANDSAT_7.', band.or.si, sep=''))), 
                            min2 = min(get(paste('LANDSAT_7.', band.or.si, '.pred', sep=''))))]
    axis.min <- min(as.numeric(axis.min))
    axis.max <- eval.dt[, .(max1 = max(get(paste('LANDSAT_7.', band.or.si, sep=''))),
                            max2 = max(get(paste('LANDSAT_7.', band.or.si, '.pred', sep=''))))]
    axis.max <- max(as.numeric(axis.max))
    axis.lim <- c(axis.min, axis.max)
    
    if(i == 'LANDSAT_5'){
      uncal.xlab <- bquote('Landsat 5'~.(toupper(band.or.si))~'(Uncalibrated)')
      cal.xlab <- bquote('Landsat 5'~.(toupper(band.or.si))~'(Cross-Calibrated)')
    } else if (i == 'LANDSAT_8'){
      uncal.xlab <- bquote('Landsat 8'~.(toupper(band.or.si))~'(Uncalibrated)')
      cal.xlab <- bquote('Landsat 8'~.(toupper(band.or.si))~'(Cross-Calibrated)')
    }
    
    lsat7.ylab <- bquote('Landsat 7'~.(toupper(band.or.si)))
    obs <- paste('LANDSAT_7',band.or.si,sep='.')
    pred <- paste('LANDSAT_7',band.or.si,'pred',sep='.')
    
    # raw figure
    fig.raw <- ggplot2::ggplot(eval.dt, ggplot2::aes_string(x = band.or.si, y = obs)) + 
      ggplot2::geom_bin2d(binwidth=c(0.005,0.005)) + 
      ggplot2::geom_abline(color='orange', size=1.5, alpha=0.5) + 
      ggplot2::scale_fill_viridis_c() + 
      ggplot2::labs(y=lsat7.ylab, x=uncal.xlab) + 
      ggplot2::coord_cartesian(ylim=c(axis.min, axis.max), xlim=c(axis.min, axis.max)) +
      ggplot2::theme_bw() + 
      ggplot2::theme(legend.position = c(0.15, 0.70),
                     legend.title=ggplot2::element_text(size=12), 
                     legend.text=ggplot2::element_text(size=12),
                     axis.text=ggplot2::element_text(size=12), 
                     axis.title=ggplot2::element_text(size=14,face="bold"))
    
    
    # cal figure
    fig.cal <- ggplot2::ggplot(eval.dt, ggplot2::aes_string(x = pred, y = obs)) + 
      ggplot2::geom_bin2d(binwidth=c(0.005,0.005)) + 
      ggplot2::geom_abline(color='orange', size=1.5, alpha=0.5) + 
      ggplot2::scale_fill_viridis_c() + 
      ggplot2::theme_bw() + 
      ggplot2::labs(y=lsat7.ylab, x=cal.xlab) + 
      ggplot2::coord_cartesian(ylim=c(axis.min, axis.max), xlim=c(axis.min, axis.max)) +
      ggplot2::theme(legend.position = c(0.15, 0.70), 
                     legend.title=ggplot2::element_text(size=12),
                     legend.text=ggplot2::element_text(size=12),
                     axis.text=ggplot2::element_text(size=12), 
                     axis.title=ggplot2::element_text(size=14,face="bold"))
    
    # combine figures and store in list
    fig <- ggpubr::ggarrange(fig.raw, fig.cal, ncol = 2)
    fig.lst[[i]] <- fig
  }
  
  # write out fig or save to list to write later if calibrating more than one satellite
  if (length(sats) == 1){
    fig <- fig.list[[1]]
    if (write.output == T){
      fig.name <- paste(outdir, '/', outfile.id, '_', i, '_xcal_pred_vs_obs.jpg', sep='')
      grDevices::jpeg(fig.name, 4, 4, units = 'in', res=400)
      print(fig)
      grDevices::dev.off()
    }
  } else if (length(sats) == 2){
    fig <- ggpubr::ggarrange(fig.lst[[1]], fig.lst[[2]], 
                             ncol = 1, nrow = 2, labels = c('(a)','(b)'), vjust=0.9)
    if (write.output == T){
      fig.name <- paste(outdir, '/', outfile.id,'_xcal_pred_vs_obs.jpg', sep='')
      grDevices::jpeg(fig.name, 8.0, 7.5, units = 'in', res=400)
      print(fig)
      grDevices::dev.off()
    }
  } else {
    print('Modify to accommodate plotting more satellites!')
  }
  
  # (optional) save model coefficients and evaluation summary tables
  if (write.output == T){
    outfile <- paste(outdir, '/', outfile.id, '_xcal_regression_coefs.csv', sep='')
    write.table(model.coef.df, outfile, sep = ',', row.names = F, col.names = T)
    
    outfile <- paste(outdir, '/', outfile.id, '_xcal_evaluation.csv', sep='')
    write.table(model.eval.df, outfile, sep = ',', row.names = F, col.names = T)
  }
  
  # fill in xcal column values for Landsat 7 using uncalibrated values
  dt[satellite == 'LANDSAT_7', xcal:= get(band.or.si)]
  
  # (optional) overwrite uncalibrated column with cross-calibrated data or return new column
  if (overwrite.col == T){
    dt[, eval(band.or.si) := NULL]
    data.table::setnames(dt, 'xcal', eval(band.or.si))
  } else {
    data.table::setnames(dt, 'xcal', eval(paste(band.or.si, 'xcal', sep='.')))
  }
  
  # stuff to return to the console
  print(fig)
  print(model.coef.df)
  print(model.eval.df)
  dt
}