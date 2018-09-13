#' Map PAWMAP data by size w/ 2nd continuous variable by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param color The field containing the variable for the color aesthetic.
#' Typically used to indicate percent detection. If not specified, the
#' aesthetic is not mapped.
#' @param size The size range for the plotted points
#' @param legend Text for the map legend
#' @import ggplot2
#' @export

mapPMdata <- function(dfm, vbl, color=NULL, size=c(3,12), legend="", ...){

  # plot base map
  p <- mapPMbase(...)

  # Merge station shapefile w/ data
  mapStations <- pmStations
  mapStations@data <- merge(mapStations@data, dfm, by='station')

  # Add color if selected
  if (is.null(color)) {
    p <- p + geom_point(data=mapStations@data,
                        aes_string('POINT_X', 'POINT_Y', size=vbl),
                        colour='darkorange')
  } else {
    p <- p + geom_point(data=mapStations@data,
                        aes_string('POINT_X', 'POINT_Y', size=vbl, colour=color)) +
      scale_color_continuous(name='Pct.\nDetect', low = 'grey', high = 'red')
  }

    # Set scales
    p <- p + scale_size(name=legend, range=size)
    p
}
