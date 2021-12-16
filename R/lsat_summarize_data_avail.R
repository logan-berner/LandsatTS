#' Summarize Availability of Landsat for each Sample Site
#' @description This little function summarizes the temporal period and availability of observations at each sample site
#' @param dt Data.table with columns named "sample.id" and "year"
#' @return Data.table summarizing for each site the first, last, and number of years with observations,
#' the minimum and maximum number of observations in a year, the total number of observations across years.
#' @import data.table
#' @export lsat_summarize_data_avail
#'
#' @examples # summary.dt <- lsat_summarize_data_avail(dt)

lsat_summarize_data_avail <- function(dt){
  # summarize data by site
  yr.dt <- dt[, .(n.obs = .N), by = c('sample.id','year')]
  smry.dt <- yr.dt[, .(first.yr = min(year), last.yr = max(year), n.yrs = length(unique(year)),
         n.obs.yrly.min = min(n.obs), n.obs.yrly.max = max(n.obs), n.obs.tot = sum(n.obs)), by = sample.id]

  # density plot of observations across years
  fig <- ggplot2::ggplot(dt, ggplot2::aes(year)) + 
    ggplot2::geom_density(fill='lightblue') + 
    ggplot2::labs(y='Density of observations across all locations', x='Year') + 
    ggplot2::theme_bw()
  
  # return
  smry.msg <- paste0("Total of ", nrow(smry.dt), " sample locations with ", sum(smry.dt$n.obs.tot), " multi-band observations from ",
                     min(smry.dt$first.yr), " to ", max(smry.dt$last.yr))
  print(smry.msg)
  print(fig)
  smry.dt
}
