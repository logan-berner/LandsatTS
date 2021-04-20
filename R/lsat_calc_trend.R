#' Calculate non-parametric vegetation greenness trends
#'
#' @description This function computes a temporal trend in annual time series of vegetation greenness for each sampling site
#' over a user-specificed time period. This is a wrapper for the zyp.yuepilon() function from the zyp package.
#' This function will iteratively pre-whiten a time series (i.e., remove temporal autocorrelation) and then compute
#' Mann-Kendall trend tests and Theil-Sen slope indicators.
#' @param dt Data.table with columns including site, year, and the vegetation index of interest
#' @param vi Vegetation index (e.g., NDVI) for which to assess trend
#' @param yrs A sequence of years (time period) over which to assess trends (e.g., 2000:2020)
#' @param yr.tolerance The number of years that a site's first/last years of observations can differ from the start/end of the user-specified time period ('yrs') for a trend to be computed
#' @param nyr.min.frac Fraction of years within the time period for which observations must be available if a trend is to be computed
#' @param sig A p-value significance cutoff used to categories trends (e.g., 0.10)
#'
#' @return Data.table with summary of temporal trend by site
#' @export lsat_calc_trend
#' @import data.table
#' @examples # Forthcoming...

lsat_calc_trend <- function(dt, vi, yrs, yr.tolerance = 1, nyr.min.frac = 0.7, sig = 0.10){
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, vi, 'vi')

  # subset to years of interest
  dt <- dt[year %in% yrs]

  # summarize spatial and temporal data by site
  site.smry <- dt[, .(first.yr = min(year), last.yr = max(year), n.yr.obs = .N), 
                  by = c('site','latitude','longitude')]
  
  site.smry[, trend.period := paste0(min(yrs),'to',max(yrs))]

  # note which vi is being used
  site.smry[, vi.name := vi]
  
  # identify sites with observations within the specified tolerance of first and last years
  site.smry[, ':='(first.yr.abs.dif = abs(first.yr - min(yrs)),
                   last.yr.abs.dif = abs(last.yr - max(yrs)))]
  site.smry <- site.smry[first.yr.abs.dif <= yr.tolerance][, first.yr.abs.dif := NULL]
  site.smry <- site.smry[last.yr.abs.dif <= yr.tolerance][, last.yr.abs.dif := NULL]
  
  # identify sites with observations from atleast a user-specific number of years during the time period
  site.smry <- site.smry[n.yr.obs >= round(length(yrs)*nyr.min.frac)]

  # subset data to sites that meet observation criteria
  dt <- dt[site %in% site.smry$site]
  
  # rescale year so that the regression intercept is the first year of the analysis time period
  dt[, year.rescale := year - min(yrs), by = site]
  
  # fit regression models
  trnd.dt <- dt %>% dplyr::group_by(site) %>%
    dplyr::do(out=calc.trends(x=.$year.rescale, y=.$vi)) %>%
    tidyr::unnest(cols=c(out)) %>%
    data.table::data.table()

  # combine spatial / temporal and trend details into one data table
  trnd.dt <- site.smry[trnd.dt, on = 'site']

  # compute total change (absolute and percent change)
  trnd.dt[, total.change := slope * length(yrs)]
  trnd.dt[, total.change.pcnt := total.change / intercept * 100]

  # categorize trends
  trnd.dt[, trend.cat := character()]
  trnd.dt[pval <= sig & slope > 0, trend.cat := 'greening']
  trnd.dt[pval <= sig & slope < 0, trend.cat := 'browning']
  trnd.dt[pval > sig, trend.cat := 'no trend']
  
  # density plot of slope
  ggplot2::ggplot(trnd.dt, ggplot2::aes(total.change.pcnt)) +
    ggplot2::geom_density(fill='lightblue') +
    ggplot2::labs(y='Density', x=paste0("Total % change in ", toupper(vi), ' from ', min(yrs), ' to ', max(yrs)))

  # output
  trnd.dt
}

calc.trends <- function(x,y){
  xx <- zyp::zyp.yuepilon(y,x) ## note the order of x and y are switched in this call!!!
  return(data.table(slope=round(xx['trend'],5), intercept=round(xx['intercept'],5), tau=round(xx['tau'],3), pval=round(xx['sig'],4)))
}
