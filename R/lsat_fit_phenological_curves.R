#' Characterize Land Surface Phenology using Vegetation Index Time Series
#'
#' @description
#' This function characterizes seasonal land surface phenology at each sample site using time series of spectral vegetation indices (e.g., NDVI).
#' The underlying algorithm was constructed to facilitate estimating annual maximum vegetation greenness (spectral index)
#' The function returns information about typical phenology at a sample site and about the timing of an individual observation relative.
#' Please note that this function was designed for situations where the seasonal phenology is hump shaped. If you are using a spectral index that is 
#' typically negative (e.g., Normalized Difference Water Index) then multiply the index by -1 before running this function, then back-transform
#' your index after running the lsat_summarize_growing_seasons() function.   
#' @param dt Data.table with a multi-year time series a vegetation index
#' @param si Character string specifying the spectral index (e.g., NDVI) to use for determining surface phenology. This must correspond
#' to an existing column in the data.table.
#' @param window.yrs Number specifying the focal window width in years that is used when pooling data to fit cubic splines (use odd numbers).
#' @param window.min.obs Minimum number of focal window observations necessary to fit a cubic spline.
#' @param si.min Minimum value of spectral index necessary for observation to be used when fitting cubic splines. Defaults to 0.15 which for NDVI is about when plants are present.
#' @param spar Smoothing parameter passed to smooth.spline(), typically around 0.65 - 0.75 for this application.
#' @param pcnt.dif.thresh Allowable percent difference (0-100) between individual observations and fitted cubic spline.
#' Observations that differ by more than this threshold are filtered out and the cubic spline is iteratively refit.
#' @param weight When fitting the cubic splines, should individual observations be weighted by their year of acquisition relative to the focal year? 
#' If so, each observation is weighted by exp(-0.25*n.yrs.from.focal) when fitting the cubic splines. 
#' @param spl.fit.outfile (Optional) Name of output csv file containing the fitted cubic splines for each sample site.
#' Useful for subsequent visualization
#' @param progress (TRUE/FALSE) Print a progress report?
#' @param test.run (TRUE/FALSE) If TRUE, then algorithm is run using a small random subset of data and only a figure is output. This is used for model parameterization.
#' @return Data.table that provides, for each observation, information on the phenological conditions for that specific day of year during the focal period. 
#' These data can then be used to estimate annual maximum spectral index and other growing season metrics using lsat_summarize_growing_season().
#' A figure is also generated that shows observation points and phenological curves for nine random sample locations. 
#' @import data.table
#' @export lsat_fit_phenological_curves
#'
#' @examples # To come...


lsat_fit_phenological_curves = function(dt, si, window.yrs=11, window.min.obs=20, si.min=0.15, spar=0.73,
                                        pcnt.dif.thresh=30, weight=T, spl.fit.outfile=F, progress=T, test.run=F){
  dt <- data.table::data.table(dt)
  
  if (test.run == T){
    dt <- dt[sample.id %in% sample(unique(dt$sample.id), 9)]
    }
  # GET SAMPLE sample, DOY, YEAR, AND SPECTRAL INDEX FROM INPUT DATA TABLE
  dt <- dt[, eval(c('sample.id','latitude','longitude','year','doy',si)), with=F]
  dt <- data.table::setnames(dt, si, 'si')
  dt <- dt[order(sample.id,doy)]
  
  # FILTER OUT LOW VALUES
  dt <- dt[si >= si.min]
  
  # IDENTIFY TIME PERIODS
  all.yrs <- sort(unique(dt$year))
  rng.yrs <- min(all.yrs):max(all.yrs)
  focal.yrs <- rng.yrs[-c(1:(round(window.yrs/2)), (length(rng.yrs)-round(window.yrs/2)+1):length(rng.yrs))]
  n.focal.yrs <- length(focal.yrs)
  
  # CREATE LISTS FOR TEMPORARY STORAGE
  data.list <- list()
  splines.list <- list()
  
  # LOOP THROUGH FOCAL YEARS, FITTING SPLINE FOR EACH SAMPLE SITE
  all.yrs <- min(dt$year):max(dt$year)
  for (i in all.yrs){
    focal.yr <- i
    
    # DETERMINE FOCAL WINDOW
    half.win.yrs <- round(window.yrs/2)
    if (i - half.win.yrs < min(all.yrs)){ # start of record
      focal.win <- min(all.yrs):(min(all.yrs)+window.yrs-1)
    } else if (i + half.win.yrs > max(all.yrs)){ # end of record
      focal.win <- (max(all.yrs)-window.yrs+1):max(all.yrs)
    } else{ # middle of record
      focal.win <- (i - half.win.yrs+1):(i + half.win.yrs-1)
    }
    
    # SUBSET OBS FROM FOCAL PERIOD
    focal.dt <- dt[year %in% focal.win]
    focal.dt <- focal.dt[order(sample.id,doy)]
    
    # COMPUTE NUMBER OF OBS DURING FOCAL PERIOD FOR EACH SAMPLE SITE AND EXCLUDE SITES WITH FEWER THAN SOME USE-SPECIFIC THRESHOLD
    focal.dt <- focal.dt[, n.obs.focal.win := .N, by = 'sample.id']
    focal.dt <- focal.dt[n.obs.focal.win >= window.min.obs]
    
    # IF NO SITES MEET THE OBSERVATION CRITERIA, THEN SKIP TO THE NEXT YEAR
    if (nrow(focal.dt) == 0){
      if (progress == T){print(paste0('skipping ', focal.yr,' because too little data'))}
      next()
    }
    
    # FIT SPLINE TO SEASONAL TIME SERIES AT EACH SAMPLE SITE
    if (weight == T){
      focal.dt[, n.yrs.from.focal := abs(year - focal.yr)]
      focal.dt[, weight := exp(-0.25*n.yrs.from.focal)]
      splines.dt <- focal.dt[, .(spl.fit = list(stats::smooth.spline(doy, si, w = weight, spar = spar))), by = 'sample.id']
    } else {
      splines.dt <- focal.dt[, .(spl.fit = list(stats::smooth.spline(doy, si, spar = spar))), by = 'sample.id']
    }
    
    doy.rng <- min(focal.dt$doy):max(focal.dt$doy)
    spline.fits.dt <- splines.dt[, .(spl.fit = unlist(Map(function(mod,doy){stats::predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'sample.id']
    spline.fits.dt <- spline.fits.dt[, doy := doy.rng, by = 'sample.id']
    
    # ITERATIVE QUALITY CHECK: LOOK FOR LARGE DIFFS BETWEEN OBS AND FITTED VALUES, DROP OBS WITH TOO LARGE A DIFF, REFIT SPLINES, RECHECK
    refitting = 1
    # ii=1
    while(refitting == 1){
      focal.dt <- spline.fits.dt[focal.dt, on = c('sample.id','doy')]
      focal.dt <- focal.dt[, abs.pcnt.dif := abs((si - spl.fit)/((si+spl.fit)/2)*100)] # calc abs % dif
      refit.sites <- unique(focal.dt[abs.pcnt.dif > pcnt.dif.thresh]$sample.id)
      focal.dt <- focal.dt[abs.pcnt.dif <= pcnt.dif.thresh]
      focal.dt <- focal.dt[, c('spl.fit', 'abs.pcnt.dif'):= NULL]
      refit.dt <- focal.dt[sample.id %in% refit.sites]
      
      # REFIT SPLINES AT SITES THAT HAD LARGE DIFFS BETWEEN OBS AND FITTED VALUES
      if (length(refit.sites) > 0){
        # set aside the splines that don't need to be refit
        spline.fits.dt <- spline.fits.dt[sample.id %in% refit.sites == F]
        
        # check the number of obs per site and filter sites out those with too few obs
        refit.dt <- refit.dt[, n.obs.focal.win := .N, by = 'sample.id']
        refit.dt <- refit.dt[n.obs.focal.win >= window.min.obs]
        
        # if no data are left, then stop refitting...
        if (nrow(refit.dt) == 0){
          refitting = 0
        } else{
          # refit
          if (weight == T){
            spline.refits.dt <- refit.dt[, .(spl.fit = list(stats::smooth.spline(doy, si, w = weight, spar = spar))), by = 'sample.id']
          } else {
            spline.refits.dt <- refit.dt[, .(spl.fit = list(stats::smooth.spline(doy, si, spar = spar))), by = 'sample.id']
          }
          spline.refits.dt <- spline.refits.dt[, .(spl.fit = unlist(Map(function(mod,doy){stats::predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'sample.id']
          spline.refits.dt <- spline.refits.dt[, doy := doy.rng, by = 'sample.id']
          spline.fits.dt <- rbind(spline.fits.dt, spline.refits.dt)
        }
        
      } else {
        refitting = 0
      }
    } # end of refitting
    
    # DROP WEIGHTING COLUMNS
    if (weight == T){
      focal.dt <- focal.dt[, c('n.yrs.from.focal','weight') := NULL]
    }
    
    # CALCULATE SEVERAL PHENOLOGY METRICS FOR EACH SAMPLE SITE
    sample.doy.smry <- focal.dt[, .(min.doy = min(doy), max.doy = max(doy)), by = 'sample.id'] # identify DOY range for each site
    spline.fits.dt <- spline.fits.dt[sample.doy.smry, on = 'sample.id']
    spline.fits.dt <- spline.fits.dt[doy >= min.doy][doy <= max.doy] # limit spline fit to DOY range at each site
    spline.fits.dt <- spline.fits.dt[, spl.fit.max := max(spl.fit), by = 'sample.id'] # compute max si typically observed at a site
    spline.fits.dt <- spline.fits.dt[, spl.fit.max.doy := doy[which.max(spl.fit)], by = 'sample.id'] # calculate typcial day of peak greenness
    spline.fits.dt <- spline.fits.dt[, spl.frac.max := spl.fit / spl.fit.max]
    spline.fits.dt <- spline.fits.dt[, si.adjustment := abs(spl.fit - spl.fit.max)] # compute adjustment factor
    spline.fits.dt <- spline.fits.dt[, focal.yr := focal.yr]
    spline.fits.dt <- spline.fits.dt[, c('min.doy','max.doy'):= NULL]
    splines.list[[i]] <- spline.fits.dt
    
    # ADD PHENOLOGY METRICS TO FOCAL DATA
    focal.dt <- spline.fits.dt[focal.dt, on = c('sample.id','doy')]
    focal.dt <- focal.dt[, si.max.pred := si + si.adjustment]
    focal.dt <- focal.dt[, c('focal.yr') := NULL]
    setnames(focal.dt, c('n.obs.focal.win'),c('spl.n.obs'))
    
    # ADD PHENOLOGY DATA TO MAIN DATA TABLE
    data.list[[i]] <- focal.dt
    
    # PRINT STATUS (if requested)
    if (progress == T){print(paste0('finished ', focal.yr))}
    
  } # end focal year loop
  
  # WRITE OUT FILE WITH SPLINE FITS (OPTIONAL)
  spline.dt <- data.table::data.table(data.table::rbindlist(splines.list))
  if (spl.fit.outfile != F){
    data.table::fwrite(spline.dt, spl.fit.outfile)
  }
  
  # OUTPUT FIGURE
  example.ids <- sample(unique(dt$sample.id), 9)
  example.obs.dt <- dt[sample.id %in% example.ids]
  example.curves.dt <- spline.dt[sample.id %in% example.ids]
  
  fig <- ggplot2::ggplot(example.obs.dt, aes(doy, si)) + 
    ggplot2::labs(y=paste0('Landsat ', toupper(si)), x='Day of Year') + 
    ggplot2::ggtitle('Nine random sample locations') + 
    ggplot2::facet_wrap(~sample.id, nrow = 3, ncol = 3, scales = 'free_y') + 
    ggplot2::geom_point(aes(fill = year), pch=21, color = 'black', size = 2) + 
    ggplot2::scale_fill_gradientn(name = 'Observation', colours = c('blue','red','gold')) + 
    ggplot2::geom_line(data = example.curves.dt, mapping = ggplot2::aes(doy, spl.fit, group = focal.yr, color = focal.yr), alpha = 0.75) + 
    ggplot2::scale_color_gradientn(name = 'Curve',  colours = c('blue','red','gold')) + 
    ggplot2::theme_bw() + ggplot2::theme(legend.position = 'right', 
                                         legend.text=ggplot2::element_text(size=12), legend.title=ggplot2::element_text(size=12), 
                                         axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14),
                                         plot.title=ggplot2::element_text(hjust = 0.5),
                                         strip.background = ggplot2::element_rect(fill="black"),
                                         strip.text = ggplot2::element_text(color = "white", size = 12)) + 
    ggplot2::guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))
  
  print(fig)
  
  # OUTPUT DATA TABLE
  if (test.run == F){
    dt <- data.table::data.table(rbindlist(data.list))
    dt <- dt[order(sample.id,year,doy)]
    data.table::setcolorder(dt, c('sample.id','latitude','longitude','year','doy','si','spl.n.obs','spl.fit','spl.frac.max','spl.fit.max','spl.fit.max.doy','si.adjustment','si.max.pred'))
    colnames(dt) <- gsub('si',si,colnames(dt))
    dt
  }
  
}
