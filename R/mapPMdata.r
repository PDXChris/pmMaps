#' Map PAWMAP data by size w/ 2nd continuous variable by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param color The field containing the variable for the color aesthetic.
#' Typically used to indicate percent detection. If not specified, the
#' aesthetic is not mapped.
#' @param size The size range for the plotted points
#' @param legend Text for the map legend
#' @param stationName Data frame field containing PAWMAP site identifier
#' @import ggplot2
#' @export

mapPMdata <- function(dfm, vbl, color=NULL, size=c(3,12), legend="",
                      stationName = 'site_identifier', ...){

  # plot base map
  p <- mapPMbase(...)

  # Merge station shapefile w/ data
  #mapStations <- pmStations
  mapStations <- merge(pmStations, dfm, by.x='location_code',
                       by.y = stationName)

  # Add color if selected
  if (is.null(color)) {
    p <- p + geom_sf(data=mapStations, inherit.aes = FALSE,
                        aes_string(size=vbl),
                        fill='darkorange', colour = 'black', shape = 21)
  } else {
    p <- p + geom_sf(data=mapStations,
                        aes_string(size=vbl, colour=color)) +
      scale_color_continuous(name='Pct.\nDetect', low = 'grey', high = 'red')
  }

    # Set scales
    p <- p + scale_size(name=legend, range=size) +
      coord_sf(xlim=mapFrame$Portland$xlim, ylim=mapFrame$Portland$ylim)

    p
}
