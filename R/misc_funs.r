#' Calculate summary statistic by station
#' @param dfm data frame containing the data
#' @param stat statistical function to calculate.  Mean is the default.
#' @param station station field name, supplied as a string
#' @param vbl field name numeric variable to summarize, supplied as a string
#' @return a data frame with summary statistic by station
#' @import plyr
#' @export

statByStation <- function(dfm, stat = 'mean', station = 'loc_code',
                          vbl = 'result') {
  statByStat_1 <- function(x) {
    stat = do.call(stat, list(x[[vbl]]))
    cns = ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    pct_det = mean(x$cens == '=')
    data.frame(stat, cns, pct_det)
  }

  ddply(dfm, station, statByStat_1)
}

#' @export
gmean <- function (x) {
  exp(mean(log(x)))
}
