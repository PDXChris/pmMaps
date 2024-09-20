#' Map PAWMAP data by size w/ 2nd continuous variable by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param fill The field containing the variable for the fill aesthetic.
#' Typically used to indicate percent detection. If not specified, the
#' aesthetic is not mapped.
#' @param size The size range for the plotted points
#' @param stationName Data frame field containing PAWMAP site identifier
#' @param size.leg Heading for the map size legend
#' @param fill.leg Heading for the map fill legend
#' @param ... Additional arguments passed to mapPMbase
#' @import ggplot2
#' @export

mapPMdata <- function(dfm, vbl, fill=NULL, size=c(3,12), legend="",
                      stationName = 'site_identifier',
                      size.leg=NULL, fill.leg=NULL, ...){

  # plot base map
  p <- mapPMbase(...)

  # Merge station shapefile w/ data
  #mapStations <- pmStations
  mapStations <- merge(pmStations, dfm, by.x='location_code',
                       by.y = stationName)

  # Add fill if selected
  if (is.null(fill)) {
    p <- p + geom_sf(data=mapStations, inherit.aes = FALSE,
                        aes_string(size=vbl),
                        fill='darkorange', colour = 'black', shape = 21)
  } else {
    p <- p + geom_sf(data=mapStations,
                        aes_string(size=vbl, fill=fill),
                     colour = 'black', shape = 21) +
      scale_fill_continuous(name=fill.leg, low = 'grey', high = 'red2')
  }

    # Set scales
    p <- p + scale_size(name=size.leg, range=size) +
      coord_sf(xlim=mapFrame$Portland$xlim, ylim=mapFrame$Portland$ylim)

    p
}
