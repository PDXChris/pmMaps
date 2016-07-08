#' @import plyr
#' @export

statByStation <- function(dfm, stat = 'mean', station = 'loc_code',
                          vbl = 'result') {
  statByStat_1 <- function(x) {
    stat = do.call(stat, list(x[['result']], na.rm = TRUE))
    cns=ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    data.frame(stat, cns)
  }

  ddply(dfm, station, statByStat_1)
}
