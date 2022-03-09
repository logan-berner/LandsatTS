#' Summarize Availability of Landsat for each Sample Site
#' 
#' @description This little function summarizes the temporal period and availability 
#'    of observations at each sample site.
#' @param dt Data.table with columns named "sample.id" and "year".
#' @return Data.table summarizing for each site the first, last, and number of years 
#'    with observations, the minimum and maximum number of observations in a year, and 
#'    the total number of observations across years. Also returns a figure showing the 
#'    median (2.5 and 97.5 percentiles) number of observations per sample site across years 
#'    for each Landsat satellite. 
#' @import data.table
#' @export lsat_summarize_data_avail
#'
#' @examples
#' data(lsat.example.dt)
#' lsat.dt <- lsat_general_prep(lsat.example.dt)
#' lsat.dt <- lsat_clean_data(lsat.dt)
#' lsat_summarize_data_avail(lsat.dt)


lsat_summarize_data_avail <- function(dt){
  dt <- data.table::data.table(dt)

  # summarize data availability for each site
  yr.dt <- dt[, .(n.obs = .N), by = c('sample.id','year')]
  full.fac <- data.table(expand.grid(sample.id = unique(yr.dt$sample.id), year = min(yr.dt$year):max(yr.dt$year)))
  yr.dt <- yr.dt[full.fac, on = c('sample.id','year')]
  yr.dt <- yr.dt[is.na(n.obs), n.obs := 0]
  
  smry1.dt <- yr.dt[n.obs != 0, .(first.yr = min(year), last.yr = max(year), n.yrs.with.obs = length(unique(year))),
                    by = sample.id]
  
  smry2.dt <- yr.dt[, .(n.obs.yrly.min = min(n.obs), n.obs.yrly.max = max(n.obs), n.obs.tot = sum(n.obs)), by = sample.id]
  
  smry.dt <- smry1.dt[smry2.dt, on = 'sample.id']
  
  smry.msg <- paste0("Total of ", nrow(smry.dt), " sample locations with ", sum(smry.dt$n.obs.tot), " multi-band observations from ",
                     min(smry.dt$first.yr), " to ", max(smry.dt$last.yr))
  
  
  # summarize number of observations by sample site, year, and satellite  
  yr.sat.dt <- dt[, .(n.obs = .N), by = c('sample.id','year','satellite')]
  full.fac <- data.table(expand.grid(sample.id = unique(yr.sat.dt$sample.id), year = min(yr.sat.dt$year):max(yr.sat.dt$year), satellite = unique(yr.sat.dt$satellite)))
  yr.sat.dt <- yr.sat.dt[full.fac, on = c('sample.id','year','satellite')]
  yr.sat.dt <- yr.sat.dt[is.na(n.obs), n.obs := 0]
  
  yr.sat.smry.dt <- yr.sat.dt[, .(n.q025 = quantile(n.obs, 0.025),
                                  n.q500 = quantile(n.obs, 0.500),
                                  n.q975 = quantile(n.obs, 0.975)),
                              by = c('year','satellite')]
  
  yr.sat.smry.dt <- rbind(yr.sat.smry.dt[satellite == 'LANDSAT_5' & year <= 2013],
                          yr.sat.smry.dt[satellite == 'LANDSAT_7' & year >= 1999],
                          yr.sat.smry.dt[satellite == 'LANDSAT_8' & year >= 2013])
  
  fig <- ggplot2::ggplot(yr.sat.smry.dt, aes(x = year, y = n.q500, group = satellite, color = satellite, fill = satellite)) + ggplot2::geom_ribbon(aes(ymin=n.q025, ymax=n.q975), alpha=0.3, color=NA) + 
    ggplot2::geom_line(lwd = 1.5, alpha = 0.5) + 
    ggplot2::geom_point(size = 3, alpha = 0.5) + 
    ggplot2::ylim(c(0,max(yr.sat.smry.dt$n.q975)+max(yr.sat.smry.dt$n.q975)*0.05))+
    ggplot2::labs(y='Number of observations per sample site', x='Year') + 
    ggplot2::ggtitle('Median count with 2.5th and 97.5th percentiles') + 
    ggplot2::scale_fill_discrete('Satellite') +
    ggplot2::scale_color_discrete('Satellite') +
    ggplot2::theme_bw() + 
    ggplot2::theme(legend.position = c(0.15, 0.8), axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14))
  
  # return
  print(smry.msg)
  print(fig)
  smry.dt
  
}
