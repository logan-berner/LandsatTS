#' Calculate non-parametric vegetation greenness trends
#' 
#' @description This function computes a temporal trend in annual time series of vegetation greenness for each sampling site
#' over a user-specificed time period. This is a wrapper for the zyp.yuepilon() function from the zyp package.
#' This function will iteratively pre-whiten a time series (i.e., remove temporal autocorrelation) and then compute  
#' Mann-Kendall trend tests and Theil-Sen slope indicators.
#' @param dt Data.table with columns including site, year, and the vegetation index of interest 
#' @param vi Vegetation index (e.g., NDVI) for which to assess trend
#' @param yrs A sequence of years over which to evaluate a trend (e.g., 2000:2020)
#' @param sig A p-value significance cutoff used to categories trends (e.g., 0.10)
#'
#' @return Data.table with summary of temporal trend by site
#' @export lsat_calc_trend
#' @import data.table
#' @examples Forthcoming...

lsat_calc_trend <- function(dt, vi, yrs, sig = 0.10){
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, vi, 'vi')
  
  # subset to years of interest
  dt <- dt[year %in% yrs]
  
  # summarize spatial and temporal data by site
  site.smry <- dt[, .(latitude = first(latitude), longitude = first(longitude),
                      first.yr = min(year), last.year = max(year)), by = site]
  site.smry[, n.yr.rng := length(first.yr:last.year), by = site]
  
  # rescale year
  dt[, year.rescale := year - min(year), by = site]
  
  # fit regression models
  trnd.dt <- dt %>% dplyr::group_by(site) %>% 
    dplyr::do(out=calc.trends(x=.$year.rescale, y=.$vi)) %>% 
    tidyr::unnest(cols=c(out)) %>% 
    data.table::data.table()

  # drop sites for which there was not enough data to model trend
  trnd.dt <- trnd.dt[is.na(slope)==F]
  
  # add spatial details to trends
  trnd.dt <- site.smry[trnd.dt, on = 'site']
  
  # compute total change (absolute and percent change)
  trnd.dt[, total.change := slope * n.yr.rng]
  trnd.dt[, total.change.pcnt := total.change / int * 100]
  
  # categorize trends
  trnd.dt[, trend.cat := character()]
  trnd.dt[pval <= sig & slope > 0, trend.cat := 'greening']
  trnd.dt[pval <= sig & slope < 0, trend.cat := 'browning']
  trnd.dt[pval > sig, trend.cat := 'no trend']
  
  # density plot of slope
  ggplot2::ggplot(trnd.dt, aes(total.change.pcnt)) + 
    ggplot2::geom_density(fill='lightblue') + 
    ggplot2::labs(y='Density', x=paste0("Total % change in ", toupper(vi), ' from ', min(yrs), ' to ', max(yrs)))
  
  # output
  trnd.dt
}

calc.trends <- function(x,y){
  xx <- zyp::zyp.yuepilon(y,x) ## note the order of x and y are switched in this call!!!
  return(data.table(slope=round(xx['trend'],5), int=round(xx['intercept'],5), tau=round(xx['tau'],3), pval=round(xx['sig'],4)))
}
