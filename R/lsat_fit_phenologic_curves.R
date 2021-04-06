#' Characterize Land Surface Phenology using Vegetation Index Time Series
#'
#' @description
#' This function characterizes seasonal land surface phenology at each sampling site using time series of spectral vegetation indices (e.g., NDVI).
#' The underlying algorithm was construted to facilitate estimating annual maximum vegetation greenness (vegetation index)
#' The function returns information about typical phenology at a site and about the timing of an individual observation relative
#' @param dt Forthcoming...
#' @param vi Character string specifying the vegetation index (e.g., NDVI) to use for determining surface phenology. This must correspond
#' to an existing colunm in the data.table.
#' @param window.yrs Number specifying the focal window width in years that is used when pooling data to fit cubic splines (use odd numbers).
#' @param window.min.obs Minimum number of focal window observations necessary to fit a cubic spline.
#' @param vi.min Minimum value of vegetation index necessary for observation to be used when fitting cubic splines
#' @param spar Smoothing paramater passed to smooth.spline(), typically around 0.65 - 0.75 for this application.
#' @param pcnt.dif.thresh Allowable percent difference (0-100) between individual observations and fitted cubic spline.
#' Observations that differ by more than this threshold are filtered out and the cubic spline is iteratively re-reft.
#' @param spl.fit.outfile (Optional) Name of output csv file containing the fitted cubic splines for each site.
#' Useful for subsequent visualization
#' @param progress (TRUE/FALSE) Print a progress report?
#' @return Data.table that provides, for each observation, information on the phenological conditions for that specific day of year during the focal period.
#' These data can then be used to estimate annual maximum vegegation index and other growing season metrics using lsat_summarize_growing_season().
#'
#' @export lsat_fit_phenological_curves
#'
#' @examples # To come...
#'
lsat_fit_phenological_curves = function(dt, vi, window.yrs=5, window.min.obs=10, vi.min = 0, spar=0.7, pcnt.dif.thresh=100, spl.fit.outfile=F, progress=T){
  # LOAD LIBRARIES
  dt <- data.table::data.table(dt)

  # GET SITE, DOY, YEAR, AND VEG INDEX FROM INPUT DATA TABLE
  dt <- dt[, eval(c('site','latitude','longitude','year','doy',vi)), with=F]
  dt <- data.table::setnames(dt, vi, 'vi')
  dt <- dt[order(site,doy)]

  # FILTER OUT LOW VALUES
  dt <- dt[vi >= vi.min]

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
    splines.dt <- focal.dt[, .(spl.fit = list(stats::smooth.spline(doy, vi, spar = spar))), by = 'site']
    spline.fits.dt <- splines.dt[, .(spl.fit = unlist(Map(function(mod,doy){stats::predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'site']
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
        spline.refits.dt <- refit.dt[, .(spl.fit = list(stats::smooth.spline(doy, vi, spar = spar))), by = 'site']
        spline.refits.dt <- spline.refits.dt[, .(spl.fit = unlist(Map(function(mod,doy){stats::predict(mod, data.frame(doy=doy.rng))$y}, spl.fit))), by = 'site']
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
    if (progress == T){print(paste('focal year: ', focal.yr, '(', round(i/n.focal.yrs,2)*100, '% finished)', sep=''))}

  } # end focal year loop

  # WRITE OUT FILE WITH SPLINE FITS (OPTIONAL)
  if (spl.fit.outfile != F){
    spline.dt <- data.table::data.table(data.table::rbindlist(splines.list))
    data.table::fwrite(spline.dt, spl.fit.outfile)
  }

  # OUTPUT DATA TABLE
  dt <- data.table::data.table(rbindlist(data.list))
  dt <- dt[order(site,year,doy)]
  data.table::setcolorder(dt, c('site','latitude','longitude','year','doy','spl.n.obs','spl.fit','spl.frac.max','spl.fit.max','spl.fit.max.doy','vi.adjustment','vi','vi.max.pred'))
  colnames(dt) <- gsub('vi',vi,colnames(dt))
  dt
}
