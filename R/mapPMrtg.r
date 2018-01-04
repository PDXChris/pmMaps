#' Map PAWMAP data by size w/ quantile indicated by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param legend Text for the map legend
#' @import ggplot2
#' @import classInt
#' @export

mapPMrtg <- function(dfm, vbl, legend="",  ...){

  p <- mapPMbase(...)
  mapStations <- pmStations
  mapStations@data <- merge(mapStations@data, dfm,
                            by.x = "id", by.y = "loc_code")

  nclr <- 5
  class <- with(mapStations@data, classIntervals(dfm[[vbl]], nclr, style="quantile"))
  colcode <- c('forestgreen', 'green', 'yellow', 'darkorange', 'red2')

  mapStations@data$cuts <- cut(mapStations@data[[vbl]], breaks=class$brks, include.lowest=TRUE, right=FALSE)

  p <-
    p + geom_point(aes(x,y, size=cuts, fill=cuts), shape=21, data=mapStations@data) +
    #   geom_text(aes(x=x, y=y, label=watershed, fontface=2), data=ggwtshd.lab) +
    scale_size_manual(name='Urban Metal\nIndex', values= c(4,6,8,10, 12),
                      labels=c('Low', 'Moderately Low', 'Average', 'Moderately High', 'High')) +
    scale_fill_manual(name='Urban Metal\nIndex', values=colcode, labels=c('Low', 'Moderately Low',
                                                                         'Average', 'Moderately High', 'High'))

  p
}
