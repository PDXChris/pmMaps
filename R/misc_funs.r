#' @import plyr
#' @export

statWQMnCns <- function(dfm) {
  statWQMnCns1 <- function(x) {
    mn=mean(x[['result']], na.rm = TRUE)
    cns=ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
    data.frame(mn, cns)
  }

  ddply(dfm, 'loc_code', statWQMnCns1)
}
