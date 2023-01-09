#' Calculate non-parametric vegetation greenness trends
#'
#' @description This function evaluates and summarizes interannual trends in 
#' vegetation greenness for sample sites over a user-specified time period. 
#' Potential interannual trends in vegetation greenness are assessed using 
#' Mann-Kendall trend tests and Theil-Sen slope indicators after prewhitening
#' each time series. This trend assessment relies on the zyp.yuepilon() function 
#' from the zyp package, which provides further details. 
#' @param dt Data.table with columns including site, year, and the vegetation index of interest.
#' @param si Spectral index for which to assess trend (e.g., NDVI).
#' @param yrs A sequence of years over which to assess trends (e.g., 2000:2020).
#' @param yr.tolerance The number of years that a site's first/last years of 
#'    observations can differ from the start/end of the user-specified 
#'    time period ('yrs') for a trend to be computed.
#' @param nyr.min.frac Fraction of years within the time period for which observations 
#'     must be available if a trend is to be computed.
#' @param sig A p-value significance cutoff used to categories trends (e.g., 0.10)
#' @return A list that includes: 
#'     (1) a summary message about the mean relative change across sample sites;
#'     (2) a data.table summarizing the number and percentage of sites that
#'     fall into each trend category;
#'     (3) a data.table with trend statistics for each sample site.
#' 
#' @export lsat_calc_trend
#' @import data.table
#' @examples 
#' data(lsat.example.dt)
#' lsat.dt <- lsat_format_data(lsat.example.dt)
#' lsat.dt <- lsat_clean_data(lsat.dt)
#' lsat.dt <- lsat_calc_spectral_index(lsat.dt, 'ndvi')
#' # lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', write.output = F)
#' lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi') 
#' lsat.gs.dt <- lsat_summarize_growing_seasons(lsat.pheno.dt, si = 'ndvi')
#' lsat.trend.dt <- lsat_calc_trend(lsat.gs.dt, si = 'ndvi.max', yrs = 2000:2020)
#' lsat.trend.dt

lsat_calc_trend <- function(dt, 
                            si, 
                            yrs, 
                            yr.tolerance = 1, 
                            nyr.min.frac = 0.66, 
                            sig = 0.10){
  
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, si, 'si')
  
  # subset to years of interest
  dt <- dt[year %in% yrs]
  
  # summarize spatial and temporal data by site
  site.smry <- dt[, .(first.yr = min(year), last.yr = max(year), n.yr.obs = .N), 
                  by = c('sample.id','latitude','longitude')]
  
  site.smry[, trend.period := paste0(min(yrs),'to',max(yrs))]
  
  # note which si is being used
  site.smry[, si := si]
  
  # identify sites with observations within the specified tolerance of first and last years
  site.smry[, ':='(first.yr.abs.dif = abs(first.yr - min(yrs)),
                   last.yr.abs.dif = abs(last.yr - max(yrs)))]
  site.smry <- site.smry[first.yr.abs.dif <= yr.tolerance][, first.yr.abs.dif := NULL]
  site.smry <- site.smry[last.yr.abs.dif <= yr.tolerance][, last.yr.abs.dif := NULL]
  
  
  # identify sites with observations from atleast a
  # user-specific number of years during the time period
  site.smry <- site.smry[n.yr.obs >= round(length(yrs)*nyr.min.frac)]
  
  # subset data to sites that meet observation criteria
  dt <- dt[sample.id %in% site.smry$sample.id]
  
  # rescale year so that the regression intercept is the 
  # first year of the analysis time period
  dt[, year.rescale := year - min(yrs), by = sample.id]
  
  # fit regression models
  trend.dt <- dt[, as.list(calc.trends(year.rescale, si)), by = sample.id]
  
  # combine spatial / temporal and trend details into one data table
  trend.dt <- site.smry[trend.dt, on = 'sample.id']
  
  # compute total change (absolute and percent change)
  trend.dt[, total.change := round(slope * length(yrs),3)]
  trend.dt[, total.change.pcnt := round(total.change / intercept * 100,1)]
  
  # categorize trends
  trend.dt[, trend.cat := character()]
  trend.dt[pval <= sig & slope > 0, trend.cat := 'greening']
  trend.dt[pval <= sig & slope < 0, trend.cat := 'browning']
  trend.dt[pval > sig, trend.cat := 'no_trend']
  
  # sort by sample id
  setorder(trend.dt, 'sample.id')
  
  # create output message about average relative change 
  avg <- round(mean(trend.dt$total.change.pcnt),2)
  std <- round(sd(trend.dt$total.change.pcnt),2)
  print.avg.msg <- paste0("Mean (SD) relative change of ", avg,
                          " (", std, ") % across the ", nrow(trend.dt), 
                          ' sample sites')
  
  
  trend.freqs.dt <- trend.dt[, .(n = .N), trend.cat]
  trend.freqs.dt[, N := sum(n)][, pcnt := round(n/N * 100)]
  trend.freqs.dt <- trend.freqs.dt[, N := NULL]
  names(trend.freqs.dt) <- c('Trend category','Number of sites','Percent of sites')
  
  # output
  output.message <- list()
  output.message$message <- print.avg.msg
  output.message$trend.frequencies <- trend.freqs.dt
  print(output.message)
  trend.dt
}


calc.trends <- function(x,y){
  xx <- zyp::zyp.yuepilon(y,x) ## note the order of x and y are switched in this call!!!
  return(data.table(slope=round(xx['trend'],5), 
                    intercept=round(xx['intercept'],4), 
                    tau=round(xx['tau'],3), 
                    pval=round(xx['sig'],4)))
}
