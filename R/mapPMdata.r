#' Map PAWMAP data
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param color The field containing the color variable.  Typically used to
#' indicate percent detection. If not specified, the aesthetic is not mapped.
#' @param size The size range for the plotted points
#' @import ggplot2
#' @export

mapPMdata <- function(dfm, vbl, color=NULL, size=c(3,12)){

  # plot base map
  p <- mapPMbase()

  # Merge station shapefile w/ data
  mapStations <- pmStations
  mapStations@data <- merge(mapStations@data, dfm, by.x='id', by.y='loc_code')

  # Add color if selected
  if (is.null(color)) {
    p <- p + geom_point(data=mapStations@data,
                        aes_string('x', 'y', size=vbl),
                        colour='darkorange')
  } else {
    p <- p + geom_point(data=mapStations@data,
                        aes_string('x', 'y', size=vbl, colour=color)) +
      scale_color_continuous(name='Pct.\nDetect', low = 'grey', high = 'red')
  }

    # Set scales
    p <- p + scale_size(name='Mean', range=size)
    p
}
