#' Evaluate estimates of annual phenological maximum

#' @description Assess how the number of annual Landsat measurements impacts 
#' estimates of annual maximum vegetation greenness derived from raw measurements and
#' phenological modeling. The algorithm computes annual maximum vegetation greenness 
#' for each site using years with at least a user-specific number of measurements and then compares 
#' these with estimates derived when using progressively smaller subsets of measurements. 
#' This lets the user determine the degree to which annual estimates of maximum vegetation 
#' greenness are impacted by the number of available measurements.
#' @param dt Data.table output from lsat_fit_phenological_curves().
#' @param si Character string specifying the spectral index (SI) to evaluate (e.g., NDVI).
#' @param min.frac.of.max Numeric threshold (0-1) that defines the "growing season" as 
#'  the seasonal window when the phenological curves indicate the SI is within a specified 
#'  fraction of the maximum SI. In other words, an observation is considered to be from 
#'  the "growing season" when the SI is within a user-specified fraction of the curve-fit 
#'  growing season maximum SI.
#' @param min.obs Minimum number of site-level measurements needed each year to be included 
#'     in the evaluation (Default = 10).
#' @param reps Number of times to bootstrap the assessment (Default = 10).
#' @param zscore.thresh Numeric threshold specifying the Z-score value beyond which individual 
#'     measurements are filtered before computing the maximum SI.
#' @param outdir If desired, specify the output directory where evaluation data and figure 
#'     should be written. If left as NA, then output is only displayed in the console 
#'     and not written to disk.
#'
#' @return A data.table and a figure summarizing how estimates of annual maximum SI
#'     vary with the number of Landsat measurements made during each growing season.  
#' @import data.table
#' @export lsat_evaluate_phenological_max
#' @examples
#' data(lsat.example.dt)
#' lsat.dt <- lsat_format_data(lsat.example.dt)
#' lsat.dt <- lsat_clean_data(lsat.dt)
#' lsat.dt <- lsat_calc_spectral_index(lsat.dt, 'ndvi')
#' # lsat.dt <- lsat_calibrate_rf(lsat.dt, band.or.si = 'ndvi', write.output = FALSE)
#' lsat.pheno.dt <- lsat_fit_phenological_curves(lsat.dt, si = 'ndvi') 
#' lsat_evaluate_phenological_max(lsat.pheno.dt, si = 'ndvi')


lsat_evaluate_phenological_max <- function(dt, 
                                           si, 
                                           min.frac.of.max = 0.75, 
                                           zscore.thresh = 3, 
                                           min.obs = 6, 
                                           reps = 10, 
                                           outdir = NA){

  colnames(dt) <- gsub(si, 'si', colnames(dt))

  # if desired, only use measurements from the growing season
  dt <- dt[spl.frac.max >= min.frac.of.max]

  # for each site, get the years with at least the min number of measurements specified
  dt <- dt[, n.obs.gs := .N, by = c('sample.id','year')]
  dt <- dt[n.obs.gs > min.obs]

  # identify and filter out obs-level predictions of max si that are 
  # anomalously high or low relative to other obs from that site and year
  dt <- dt[, ':='(avg = mean(si.max.pred), 
                  sd = stats::sd(si.max.pred), 
                  n=.N), 
           by = c('sample.id','year')]
  dt <- dt[, abs.zscore := abs((si.max.pred - avg )/sd)]
  dt <- dt[abs.zscore <= zscore.thresh]

  # compute max observed si (actually 90% percentile to avoid spuriously high values)
  dt <- dt[, si.max.obs := stats::quantile(si, 0.90), by = c('sample.id','year')]

  # iteratively loop through sample size and reps
  out.list <- list()
  cnt=1
  for (i in 1:(min.obs-1)){
    for (j in 1:reps){
      rep.dt <- dt[,.SD[sample(.N, i)], by=c('sample.id','year')]
      rep.dt <- rep.dt[,':='(n.obs=i, rep=j)]
      rep.dt <- rep.dt[, .(si.max.obs = data.table::first(si.max.obs), 
                           si.max.uncor = stats::quantile(si,0.9), 
                           si.max.cor = stats::median(si.max.pred)), 
                       by = c('n.obs','rep','sample.id','year')]
      rep.dt <- rep.dt[si.max.cor < si.max.uncor, si.max.cor := si.max.uncor]
      rep.dt[, si.uncor.pcntdif := (si.max.uncor - si.max.obs)/si.max.obs*100]
      rep.dt[, si.cor.pcntdif := (si.max.cor - si.max.obs)/si.max.obs*100]
      out.list[[cnt]] <- rep.dt
      cnt = cnt + 1
    }
  }

  eval.dt <- data.table::rbindlist(out.list)
  eval.dt <- na.omit(eval.dt)

  # summarize across iterations
  eval.smry.dt <- eval.dt[,.(si.uncor.pcntdif.med = stats::median(si.uncor.pcntdif, na.rm=T), 
                             si.cor.pcntdif.med = stats::median(si.cor.pcntdif, na.rm=T)), 
                          by = c('sample.id','year','n.obs')]
  eval.smry.dt <- melt.data.table(eval.smry.dt, 
                                  id.vars=c('sample.id','year','n.obs'), 
                                  value.name='pcnt.dif', variable.name='Processing')
  eval.smry.dt$n.obs.fac <- as.factor(eval.smry.dt$n.obs)
  eval.smry.dt$Processing <- factor(eval.smry.dt$Processing, labels = c('Raw','Modeled'))

  # EVALUATION PLOT
  ylab.pcntdif <- bquote("Difference from observed "~.(toupper(gsub('.xcal','',si)))['max']~" (%)")
  fig <- ggplot2::ggplot(eval.smry.dt, ggplot2::aes(n.obs.fac, pcnt.dif, fill=Processing)) + 
    ggplot2::geom_boxplot(outlier.size=0.7, outlier.color='gray') + 
    ggplot2::theme_bw() + 
    ggplot2::labs(y=ylab.pcntdif, x='Number of observations') + 
    ggplot2::theme(legend.position="right", 
                   axis.text=ggplot2::element_text(size=12), 
                   axis.title=ggplot2::element_text(size=14))
  print(fig)
  
  # console output
  colnames(eval.smry.dt) <- gsub('si', si, colnames(eval.smry.dt))
  eval.smry.dt
  
  # should data and figure be written to disk?
  if (is.na(outdir) == F){
    R.utils::mkdirs(outdir)
  
    fig.outname <- paste0(outdir,'pheno_max_eval_',si,'.jpg')
    grDevices::jpeg(fig.outname, width = 6, height = 4, res = 400, units = 'in')
    print(fig)
    grDevices::dev.off()
    
    data.outname <- paste0(outdir,'pheno_max_eval_',si,'.csv')
    data.table::fwrite(eval.smry.dt, data.outname)
  }
  
}
