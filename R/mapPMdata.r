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
#' @param extent Axes limits for map extent.  Currently one of
#' c('Portland', 'West Hills', 'Tryon Creek', 'NW Streams')
#' @import ggplot2
#' @export

mapPMdata <- function(dfm, vbl, fill=NULL, size=c(3,12), legend="",
                      stationName = 'site_identifier',
                      size.leg=NULL, fill.leg=NULL, extent = "Portland", ...){

  # plot base map
  p <- mapPMbase(...)

  # Merge station shapefile w/ data
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
  map_lims <- setMapFrame(extent = extent)

  p <- p + scale_size(name=size.leg, range=size) +
    coord_sf(xlim=map_lims$xlim, ylim=map_lims$ylim)

  p
}
