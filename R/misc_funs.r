#' @import plyr
#' @export

statByStation <- function(dfm, stat = 'mean', station = 'loc_code',
                          vbl = 'result') {
  statByStat_1 <- function(x) {
    stat = do.call(stat, list(x[['result']]))
    cns=ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    data.frame(stat, cns)
  }

  ddply(dfm, station, statByStat_1)
}

#' @export
gmean <- function (x) {
  exp(mean(log(x)))
}
