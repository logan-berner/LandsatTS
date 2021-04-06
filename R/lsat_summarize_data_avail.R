#' Summarize Availability of Landsat data by Site
#' @description This little function summarizes the temporal period and availability of observations at each site
#' @param dt Data.table with columns named "site" and "year"
#' @return Data.table summarizing for each site the first, last, and number of years with observations,
#' the minimum and maximum number of observations in a year, the total number of observations across years.
#' @export lsat_summarize_data_avail
#'
#' @examples # summary.dt <- lsat_summarize_data_avail(dt)

lsat_summarize_data_avail <- function(dt){
  yr.dt <- dt[, .(n.obs = .N), by = c('site','year')]
  smry.dt <- yr.dt[, .(first.yr = data.table::first(year), last.yr = data.table::last(year), n.yrs = length(unique(year)),
         n.obs.min = min(n.obs), n.obs.max = max(n.obs), n.obs.tot = sum(n.obs)), by = site]
  smry.dt
}
