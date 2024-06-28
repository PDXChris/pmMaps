#' Map PAWMAP data by size w/ quantile indicated by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param legend Text for the map legend
#' @param highgood Are high values good (e.g., biotic indices = TRUE) or
#' bad (e.g., pollutants = FALSE)?  Default is FALSE.
#' @param nclr number of colors/breaks in data
#' @param by.y field in data which matches with station field in shapefile
#' @param cutStyle method to use in classInterval for developing classes
#' @param sizeVals vector of point sizes for each color level
#' @param extent Axes limits for map extent.  Currently one of
#' c('Portland', 'West Hills', 'Tryon Creek', 'NW Streams')
#' @param ... Additional arguments passed to mapPMbase
#' @import ggplot2
#' @import classInt
#' @export

mapPMrtg <- function(dfm, vbl, legend="",  highgood=FALSE,
                     nclr=5, by.y = 'site_identifier', cutStyle="quantile",
                     sizeVals = c(4,6,8,10, 12), extent = 'Portland', ...){

  p <- mapPMbase(...)

  # Merge data frame to station layer
  mapStations <- merge(pmStations, dfm, by.x = "location_code", by.y = by.y)

  # set up colors and labels for legend levels
  cut_classes <- with(mapStations,
                      classIntervals(dfm[[vbl]], nclr, style=cutStyle))
  colcode <- c('forestgreen', 'green', 'yellow', 'darkorange', 'red2')[1:nclr]
  if (highgood) colcode <- rev(colcode)

  mapStations$cuts <- cut(mapStations[[vbl]], breaks=cut_classes$brks,
                               include.lowest=TRUE, right=FALSE)
  leg.labels <- c('Low', 'Moderately Low', 'Average', 'Moderately High', 'High')

  ### Map ###
  # Set extent
  map_lims <- setMapFrame()

  p <- p + geom_sf(aes(size=cuts, fill=cuts),
                      shape=21, data=mapStations) +
    scale_size_manual(name=legend, values= sizeVals, labels=leg.labels) +
    scale_fill_manual(name=legend, values=colcode, labels=leg.labels) +
    coord_sf(xlim=map_lims$xlim, ylim=map_lims$ylim)

  p
}
