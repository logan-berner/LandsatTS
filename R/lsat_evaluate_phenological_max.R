#' Evaluate Estimates of Annual Phenological Maximum
#' @description Assess how the number of annual Landsat observations impacts estimates of annual maximum vegetation greenness derived from raw observations and
#' phenologic modeling. The algorithem computes annual maximum vegetation greenness using site x years with a user-specific number of observations and then
#' compares these with estimates derived when using progressively smaller subsets of observations. This lets you determine the degree to which
#' annual estimates of maximum vegetation greenness are impacted the the number of available observations.
#' @param dt Data.table output from lsat_fit_phenologic_curves().
#' @param vi Character string specifying the vegetation index (e.g., NDVI) to evaluate.
#' @param min.frac.of.max Numeric threshold (0-1) that defines the "growing season" as the seasonal window when the
#' phenological curves indicate the VI is within a specified fraction of the maximum VI. In otherwords, an observation
#' is considered to be from the "growing season" when the VI is within a user-specified fraction of the curve-fit growing season maximum VI.
#' @param zscore.thresh Numeric threshold specifying the Z-score value beyond which individual observations are filtered before computing the maximum VI.
#' @param min.obs Minimum number of observations needed for a site x year to be included in the evaluation (Default = 10)
#' @param reps Number of times to bootstrap the assessment (Default = 10)
#' @param zscore.thresh Numeric threshold specifying the Z-score value beyond which individual observations are filtered before computing the maximum VI.
#' @param outdir Output directory into which evaluation data and figure will be writen.
#'
#' @return Data.table
#' @export lsat_evaluate_phenological_max
#' @examples # Forthcoming...

lsat_evaluate_phenological_max <- function(dt, vi, min.frac.of.max = 0.75, min.obs = 6, reps = 10, outdir = 'output/pheno_max_eval/'){

  colnames(dt) <- gsub(vi, 'vi', colnames(dt))

  # if desired, only use observations from the growing season
  dt <- dt[spl.frac.max >= min.frac.of.max]

  # get site x years with atleast the min number of observations specificed
  dt <- dt[, n.obs.gs := .N, by = c('site','year')]
  dt <- dt[n.obs.gs > min.obs]

  # identify and filter out obs-level predictions of max VI that are anomalously high or low relative to other obs from that site x year
  dt <- dt[, ':='(avg = mean(vi.max.pred), sd = stats::sd(vi.max.pred), n=.N), by = c('site','year')]
  dt <- dt[, abs.zscore := abs((vi.max.pred - avg )/sd)]
  dt <- dt[abs.zscore < 2]

  # compute max observed VI (actually 90% percentile to avoid spuriously high values)
  dt <- dt[, vi.max.obs := stats::quantile(vi, 0.90), by = c('site','year')]

  # iteratively loop through sample size and reps
  out.list <- list()
  cnt=1
  for (i in 1:(min.obs-1)){
    for (j in 1:reps){
      rep.dt <- dt[,.SD[sample(.N, i)], by=c('site','year')]
      rep.dt <- rep.dt[,':='(n.obs=i, rep=j)]
      rep.dt <- rep.dt[, .(vi.max.obs = data.table::first(vi.max.obs), vi.max.uncor = stats::quantile(vi,0.9), vi.max.cor = stats::median(vi.max.pred)), by = c('n.obs','rep','site','year')]
      rep.dt <- rep.dt[vi.max.cor < vi.max.uncor, vi.max.cor := vi.max.uncor]
      rep.dt[, vi.uncor.pcntdif := (vi.max.uncor - vi.max.obs)/vi.max.obs*100]
      rep.dt[, vi.cor.pcntdif := (vi.max.cor - vi.max.obs)/vi.max.obs*100]
      out.list[[cnt]] <- rep.dt
      cnt = cnt + 1
    }
  }

  eval.dt <- data.table::rbindlist(out.list)
  eval.dt <- na.omit(eval.dt)

  # summarize across iterations
  eval.smry.dt <- eval.dt[,.(vi.uncor.pcntdif.med = stats::median(vi.uncor.pcntdif, na.rm=T), vi.cor.pcntdif.med = stats::median(vi.cor.pcntdif, na.rm=T)), by = c('site','year','n.obs')]
  eval.smry.dt <- melt.data.table(eval.smry.dt, id.vars=c('site','year','n.obs'), value.name='pcnt.dif', variable.name='correction')
  eval.smry.dt$n.obs.fac <- as.factor(eval.smry.dt$n.obs)
  eval.smry.dt$correction <- factor(eval.smry.dt$correction, labels = c('Raw','Corrected'))

  # EVALUATION PLOT
  ylab.pcntdif <- bquote("Difference from observed "~.(toupper(gsub('.xcal','',vi)))['max']~" (%)")
  fig <- ggplot2::ggplot(eval.smry.dt, ggplot2::aes(n.obs.fac, pcnt.dif, fill=correction)) + ggplot2::geom_boxplot(outlier.size=0.7, outlier.color='gray')
  fig <- fig + ggplot2::theme_bw() + ggplot2::labs(y=ylab.pcntdif, x='Number of observations')
  fig <- fig + ggplot2::theme(legend.position="right", axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14,face="bold"))

  # output
  colnames(eval.smry.dt) <- gsub('vi', vi, colnames(eval.smry.dt))
  R.utils::mkdirs(outdir)

  fig.outname <- paste0(outdir,'pheno_max_eval_',vi,'.jpg')
  grDevices::jpeg(fig.outname, width = 6, height = 4, res = 400, units = 'in')
  print(fig)
  grDevices::dev.off()

  data.outname <- paste0(outdir,'pheno_max_eval_',vi,'.csv')
  data.table::fwrite(eval.smry.dt, data.outname)

  eval.smry.dt
}
