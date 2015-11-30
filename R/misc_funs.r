#' @export

statWQMnCns <- function(x) {
  mn=mean(x[['result']], na.rm = TRUE)
  cns=ifelse(any(x[['cens']]=='='), 'Detected', 'None\nDetected')
  data.frame(mn, cns)
}
