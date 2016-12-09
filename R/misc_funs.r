#' Calculate summary statistic by station
#' @param dfm data frame containing the data
#' @param stat statistical function to calculate.  Mean is the default.
#' @param station station field name, supplied as a string
#' @param vbl field name numeric variable to summarize, supplied as a string
#' @param cens optional field name indicating whether the analyte was detected ('=') or not ('<')
#' @return a data frame with summary statistic by station
#' @import plyr
#' @export

statByStation <- function(dfm, stat = 'mean', station = 'loc_code',
                          vbl = 'result', cens = NULL) {
  statByStat_1 <- function(x) {
    stat = do.call(stat, list(x[[vbl]]))
    if (!is.null(cens)) {
      cns = ifelse(any(x[[cens]]=='='), 'Detected', 'None\nDetected')
      pct_det = mean(x$cens == '=')
      data.frame(stat, cns, pct_det)
    } else {data.frame(stat)}

  }

  ddply(dfm, station, statByStat_1)
}

#' @export
gmean <- function (x) {
  exp(mean(log(x)))
}
