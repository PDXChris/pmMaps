#' Map PAWMAP data
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the categorical value to map
#' @param legend Text for the map legend
#' @import ggplot2
#' @export

mapPMdiscr <- function(dfm, vbl, legend=NULL, ...){

  # plot base map
  p <- mapPMbase(...)

  # Merge station shapefile w/ data
  mapStations <- pmStations
  mapStations@data <- merge(mapStations@data, dfm, by.x='id', by.y='loc_code')

  # Map categorical variable
  p <- p + geom_point(data=mapStations@data,
                      aes_string('x', 'y', colour=vbl),
                      size=5)
  if (!is.null(legend)) {
    p <- p + scale_color_discrete(legend)
  }
  p
}