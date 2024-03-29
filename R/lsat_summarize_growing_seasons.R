#' Summarize growing season characteristics using spectral vegetation indices

#' @description This function not only computes mean, median, and 90th percentile 
#'     of a spectral index (SI) using observations for a user-specified 
#'     "growing season," but also estimates the annual maximum SI and associated day of year
#'     using phenology modeling and growing season observations.
#' @param dt Data.table generated by the function lsat_fit_phenological_curves().
#' @param si Character string specifying the spectral vegetation index to summarize (e.g., NDVI).
#' @param min.frac.of.max Numeric threshold (0-1) that defines the "growing season" as 
#'     the seasonal window when the phenological curves indicate the SI is within a 
#'     specified fraction of the maximum SI. In other words, an observation
#'     is considered to be from the "growing season" when the SI is within a 
#'     user-specified fraction of the curve-fit growing season maximum SI.
#' @param zscore.thresh Numeric threshold specifying the Z-score value beyond 
#'     which individual observations are filtered before summarizing growing season SI.

#' @return Data.table summarizing annual growing season conditions based on a spectral index.
#' @import data.table
#' @export lsat_summarize_growing_seasons
#'
#' @examples
#' data(lsat.example.dt)
#' lsat.dt <- lsat_format_data(lsat.example.dt)
#' lsat.dt <- lsat_clean_data(lsat.dt)
#' lsat.dt <- lsat_calc_spectral_index(lsat.dt, 'ndvi')
#' # lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', write.output = F)
#' lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi') 
#' lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi')
#' lsat.gs.dt

lsat_summarize_growing_seasons = function(dt, si, min.frac.of.max = 0.75, zscore.thresh = 3){
  dt <- data.table::data.table(dt)
  colnames(dt) <- gsub(si, 'si', colnames(dt))

  # take observations from the 'growing season' identified as the period when si typically exceeds a specified fraction of the typical max si
  dt <- dt[spl.frac.max >= min.frac.of.max]

  # identify and filter out obs-level predictions of max si that are anomalously high or low relative to other obs from that site x year
  dt <- dt[, ':='(avg = mean(si.max.pred), std = stats::sd(si.max.pred), n=.N), by = c('sample.id','year')]
  dt <- dt[, abs.zscore := abs((si.max.pred - avg )/std)]
  dt <- dt[abs.zscore <= zscore.thresh]

  #  estimate max summer si
  dt.smry <- dt[,.(latitude = data.table::first(latitude), longitude = data.table::first(longitude), n.obs = .N,
                   si.gs.avg = mean(si),
                   si.gs.med = stats::median(si),
                   si.gs.q90 = stats::quantile(si, 0.9),
                   si.max = stats::median(si.max.pred),
                   si.max.lwr = min(si.max.pred), si.max.upr = max(si.max.pred),
                   si.max.doy = round(mean(spl.fit.max.doy))),
                by = c('sample.id','year')]

  # predicted max can't be lower than observed 90th pecentile
  dt.smry[si.max < si.gs.q90, si.max := si.gs.q90]

  #output
  colnames(dt.smry) <- gsub("si", si, colnames(dt.smry))
  dt.smry
}

