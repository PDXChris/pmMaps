#' Map PAWMAP data by size w/ quantile indicated by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param legend Text for the map legend
#' @param highgood Are high values good (e.g., biotic indices = TRUE) or
#' bad (e.g., pollutants = FALSE)?  Default is FALSE.
#' @param nclr number of colors/breaks in data
#' @param by.y field in data which matches with station field in shapefile
#' @param cutStyle method to use in classInterval for developing classes
#' @param ... additional parameters to pass to mapPMbase
#' @import ggplot2
#' @import classInt
#' @export

mapPMrtg <- function(dfm, vbl, legend="",  highgood=FALSE,
                     nclr=5, by.y = 'station', cutStyle="quantile", ...){

  p <- mapPMbase(...)
  mapStations <- pmStations
  mapStations@data <- merge(mapStations@data, dfm, by.x = "station", by.y = by.y)

  cut_classes <- with(mapStations@data,
                      classIntervals(dfm[[vbl]], nclr, style=cutStyle))
  colcode <- c('forestgreen', 'green', 'yellow', 'darkorange', 'red2')[1:nclr]
  if (highgood) colcode <- rev(colcode)

  mapStations@data$cuts <- cut(mapStations@data[[vbl]], breaks=cut_classes$brks,
                               include.lowest=TRUE, right=FALSE)
  leg.labels <- c('Low', 'Moderately Low', 'Average', 'Moderately High', 'High')

  p <- p + geom_point(aes(POINT_X, POINT_Y, size=cuts, fill=cuts),
                      shape=21, data=mapStations@data) +
    scale_size_manual(name=legend, values= c(4,6,8,10, 12),
                      values=colcode, labels=leg.labels) +
    scale_fill_manual(name=legend, values=leg.labels)

  p
}
