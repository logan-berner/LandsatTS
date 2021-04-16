#' Summarize Availability of Landsat data by Site
#' @description This little function summarizes the temporal period and availability of observations at each site
#' @param dt Data.table with columns named "site" and "year"
#' @return Data.table summarizing for each site the first, last, and number of years with observations,
#' the minimum and maximum number of observations in a year, the total number of observations across years.
#' @import data.table
#' @export lsat_summarize_data_avail
#'
#' @examples # summary.dt <- lsat_summarize_data_avail(dt)

lsat_summarize_data_avail <- function(dt){
  output.lst <- list()

  # summarize data by site
  yr.dt <- dt[, .(n.obs = .N), by = c('site','year')]
  smry.dt <- yr.dt[, .(first.yr = min(year), last.yr = max(year), n.yrs = length(unique(year)),
         n.obs.min = min(n.obs), n.obs.max = max(n.obs), n.obs.tot = sum(n.obs)), by = site]
  output.lst[['data.smry']] <- smry.dt

  # density plot of observations across years
  output.lst[['fig']] <- ggplot2::ggplot(dt, ggplot2::aes(year)) + ggplot2::geom_density(fill='lightblue') + ggplot2::labs(y='Density of observations', x='Year')

  output.lst
}
