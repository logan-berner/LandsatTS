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
#' @param si.min Minimum value of spectral index necessary for observation to be used when fitting cubic splines
#' @param spar Smoothing parameter passed to smooth.spline(), typically around 0.65 - 0.75 for this application.
#' @param pcnt.dif.thresh Allowable percent difference (0-100) between individual observations and fitted cubic spline.
#' Observations that differ by more than this threshold are filtered out and the cubic spline is iteratively refit.
#' @param spl.fit.outfile (Optional) Name of output csv file containing the fitted cubic splines for each sample site.
#' Useful for subsequent visualization
#' @param progress (TRUE/FALSE) Print a progress report?
#' @return Data.table that provides, for each observation, information on the phenological conditions for that specific day of year during the focal period.
#' These data can then be used to estimate annual maximum spectral index and other growing season metrics using lsat_summarize_growing_season().
#' @import data.table
#' @export lsat_fit_phenological_curves
#'
#' @examples # To come...
#'
lsat_fit_phenological_curves = function(dt, si, window.yrs=5, window.min.obs=10, si.min = 0, spar=0.7, pcnt.dif.thresh=100, spl.fit.outfile=F, progress=T){
  dt <- data.table::data.table(dt)

  # GET SAMPLE sample, DOY, YEAR, AND SPECTRAL INDEX FROM INPUT DATA TABLE
  dt <- dt[, eval(c('sample.id','latitude','longitude','year','doy',si)), with=F]
  dt <- data.table::setnames(dt, si, 'si')
  dt <- dt[order(sample.id,doy)]

  # FILTER OUT LOW VALUES
  dt <- dt[si >= si.min]

  # IDENTIFY TIME PERIODS
  all.yrs <- sort(unique(dt$year))
  focal.yrs <- all.yrs[-c(1:(round(window.yrs/2)), (length(all.yrs)-round(window.yrs/2)+1):length(all.yrs))]
  n.focal.yrs <- length(focal.yrs)

  # CREATE LISTS FOR TEMPORARY STORAGE
  data.list <- list()
  splines.list <- list()

  # LOOP THROUGH FOCAL YEARS, FITTING SPLINE FOR EACH SAMPLE SITE
  for (i in 1:n.focal.yrs){
    # SUBSET OBS FROM FOCAL PERIOD
    focal.yr <- focal.yrs[i]
    focal.win <- seq(focal.yr-round(window.yrs/2), focal.yr+round(window.yrs/2))
    focal.dt <- dt[year %in% focal.win]
    focal.dt <- focal.dt[order(sample.id,doy)]

    # COMPUTE NUMBER OF OBS DURING FOCAL PERIOD FOR EACH SAMPLE SITE AND EXCLUDE SITES WITH FEWER THAN SOME USE-SPECIFIC THRESHOLD
    focal.dt <- focal.dt[, n.obs.focal.win := .N, by = 'sample.id']
    focal.dt <- focal.dt[n.obs.focal.win >= window.min.obs]

    if (nrow(focal.dt) == 0){next()}

    doy.rng <- min(focal.dt$doy):max(focal.dt$doy)

    # FIT SPLINE TO SEASONAL TIME SERIES AT EACH SAMPLE SITE
    splines.dt <- focal.dt[, .(spl.fit = list(stats::smooth.spline(doy, si, spar = spar))), by = 'sample.id']
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
          spline.refits.dt <- refit.dt[, .(spl.fit = list(stats::smooth.spline(doy, si, spar = spar))), by = 'sample.id']
          spline.refits.dt <- spline.refits.dt[, .(spl.fit = unlist(Map(function(mod,doy){stats::predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'sample.id']
          spline.refits.dt <- spline.refits.dt[, doy := doy.rng, by = 'sample.id']
          spline.fits.dt <- rbind(spline.fits.dt, spline.refits.dt)
        }
        
      } else {
        refitting = 0
      }
    } # end of refitting

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
    if (progress == T){print(paste('focal year: ', focal.yr, ' (', round(i/n.focal.yrs,2)*100, '% finished)', sep=''))}

  } # end focal year loop

  # WRITE OUT FILE WITH SPLINE FITS (OPTIONAL)
  if (spl.fit.outfile != F){
    spline.dt <- data.table::data.table(data.table::rbindlist(splines.list))
    data.table::fwrite(spline.dt, spl.fit.outfile)
  }

  # OUTPUT DATA TABLE
  dt <- data.table::data.table(rbindlist(data.list))
  dt <- dt[order(sample.id,year,doy)]
  data.table::setcolorder(dt, c('sample.id','latitude','longitude','year','doy','spl.n.obs','spl.fit','spl.frac.max','spl.fit.max','spl.fit.max.doy','si.adjustment','si','si.max.pred'))
  colnames(dt) <- gsub('si',si,colnames(dt))
  dt
}
