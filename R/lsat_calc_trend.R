#' Calculate non-parametric vegetation greenness trends
#'
#' @description This function computes a temporal trend in annual time series of vegetation greenness for each sampling site
#' over a user-specificed time period. This is a wrapper for the zyp.yuepilon() function from the zyp package.
#' This function will iteratively pre-whiten a time series (i.e., remove temporal autocorrelation) and then compute
#' Mann-Kendall trend tests and Theil-Sen slope indicators.
#' @param dt Data.table with columns including site, year, and the vegetation index of interest
#' @param si Spectral index (e.g., NDVI) for which to assess trend
#' @param yrs A sequence of years (time period) over which to assess trends (e.g., 2000:2020)
#' @param yr.tolerance The number of years that a site's first/last years of observations can differ from the start/end of the user-specified time period ('yrs') for a trend to be computed
#' @param nyr.min.frac Fraction of years within the time period for which observations must be available if a trend is to be computed
#' @param sig A p-value significance cutoff used to categories trends (e.g., 0.10)
#'
#' @return Data.table with summary of temporal trends by site and a histogram showing summarizing relative changes in vegetation greenness 
#' @export lsat_calc_trend
#' @import data.table
#' @examples # Forthcoming...

lsat_calc_trend <- function(dt, si, yrs, yr.tolerance = 1, nyr.min.frac = 0.66, sig = 0.10, legend.position=c(0.8, 0.2), legend.dir = 'horizontal'){
  dt <- data.table::data.table(dt)
  data.table::setnames(dt, si, 'si')
  
  # subset to years of interest
  dt <- dt[year %in% yrs]
  
  # summarize spatial and temporal data by site
  site.smry <- dt[, .(first.yr = min(year), last.yr = max(year), n.yr.obs = .N), 
                  by = c('sample.id','latitude','longitude')]
  
  site.smry[, trend.period := paste0(min(yrs),'to',max(yrs))]
  
  # note which si is being used
  site.smry[, si.name := si]
  
  # identify sites with observations within the specified tolerance of first and last years
  site.smry[, ':='(first.yr.abs.dif = abs(first.yr - min(yrs)),
                   last.yr.abs.dif = abs(last.yr - max(yrs)))]
  site.smry <- site.smry[first.yr.abs.dif <= yr.tolerance][, first.yr.abs.dif := NULL]
  site.smry <- site.smry[last.yr.abs.dif <= yr.tolerance][, last.yr.abs.dif := NULL]
  
  
  # identify sites with observations from atleast a user-specific number of years during the time period
  site.smry <- site.smry[n.yr.obs >= round(length(yrs)*nyr.min.frac)]
  
  # subset data to sites that meet observation criteria
  dt <- dt[sample.id %in% site.smry$sample.id]
  
  # rescale year so that the regression intercept is the first year of the analysis time period
  dt[, year.rescale := year - min(yrs), by = sample.id]
  
  # fit regression models
  trend.dt <- dt[, as.list(calc.trends(year.rescale, si)), by = sample.id]
  
  # combine spatial / temporal and trend details into one data table
  trend.dt <- site.smry[trend.dt, on = 'sample.id']
  
  # compute total change (absolute and percent change)
  trend.dt[, total.change := slope * length(yrs)]
  trend.dt[, total.change.pcnt := total.change / intercept * 100]
  
  # categorize trends
  trend.dt[, trend.cat := character()]
  trend.dt[pval <= sig & slope > 0, trend.cat := 'greening']
  trend.dt[pval <= sig & slope < 0, trend.cat := 'browning']
  trend.dt[pval > sig, trend.cat := 'no_trend']
  
  # create output message
  avg <- round(mean(trend.dt$total.change.pcnt),2)
  std <- round(sd(trend.dt$total.change.pcnt),2)
  pcnts <- round(prop.table(table(trend.dt$trend.cat)),3)*100
  msg <- paste0("Mean (SD) relative change of ", avg, " (", std,") % with browning, greening, and no trend at ", pcnts[1], ", ", pcnts[2], ", and ", pcnts[3], " % of sample sites")
  
  # histogram of vegetation greenness trends
  fig1 <- ggplot2::ggplot(trend.dt, ggplot2::aes(total.change.pcnt, fill=..x..)) +
    ggplot2::geom_histogram(bins = 50, size = 0.25, color = 'gray20') +
    ggplot2::scale_fill_gradient2(low="darkgoldenrod4", mid='white', high="darkgreen", limits = c(-50,50), midpoint = 0) +
    ggplot2::labs(y = 'Number of sample sites', x = paste0("Relative change in Landsat ", gsub('.MAX', 'max', toupper(si)), ' from ', min(yrs), ' to ', max(yrs), ' (%)')) +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = 'none', axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14)) + 
    ggplot2::xlim(-50, 50)
  
  # Create time series figure for each trend class
  dt <- dt[trend.dt, on = 'sample.id']
  
  trend.cls.yrly.dt <- dt[, .(si.avg = mean(si, na.rm = T), si.sd = sd(si, na.rm = T),
                              n = .N), by = c('trend.cat','year')]
  
  trend.cls.yrly.dt[, si.se := si.sd/sqrt(n)]
  
  trend.cls.yrly.dt[, trend.cat := factor(trend.cat, levels = c('browning','no_trend','greening'), 
                                          labels = c('browning','no trend','greening'))]
  
  trend.cols <- c('darkgoldenrod4','ivory3','darkgreen')
  
  fig2 <- ggplot2::ggplot(trend.cls.yrly.dt, ggplot2::aes(year, si.avg, group = trend.cat, color = trend.cat)) + 
    ggplot2::labs(y=paste0('Mean Landsat ', gsub('.MAX', 'max', toupper(si))), x='Year') + 
    ggplot2::geom_ribbon(aes(ymin = si.avg-si.se, ymax = si.avg + si.se, fill=trend.cat),alpha=0.3, linetype=0)+
    ggplot2::geom_line(aes(color = trend.cat), alpha = 1, size=1) + 
    ggplot2::scale_fill_manual(values = trend.cols, name = 'Trend class') + 
    ggplot2::scale_color_manual(values = trend.cols, name = 'Trend class')+
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position=legend.position, legend.direction=legend.dir,  
                   axis.text=ggplot2::element_text(size=12), 
                   axis.title=ggplot2::element_text(size=14),
                   plot.title=ggplot2::element_text(hjust = 0.5))
  
  # combo figure
  fig <- ggpubr::ggarrange(fig1, fig2, ncol = 1, nrow = 2, labels = c('(a)','(b)'), vjust=0.9, hjust = -0.1)
  
  # output
  print(fig)
  print(msg)
  trend.dt
}

calc.trends <- function(x,y){
  xx <- zyp::zyp.yuepilon(y,x) ## note the order of x and y are switched in this call!!!
  return(data.table(slope=round(xx['trend'],5), intercept=round(xx['intercept'],5), tau=round(xx['tau'],3), pval=round(xx['sig'],4)))
}
