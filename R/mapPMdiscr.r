#' Map PAWMAP categorical data by color
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the categorical value to map
#' @param legend Text for the map legend
#' @param by.y field in data which matches with station field in shapefile
#' @param extent Axes limits for map extent.  Currently one of
#' c('Portland', 'West Hills', 'Tryon Creek', 'NW Streams')
#' @import ggplot2
#' @export

mapPMdiscr <- function(dfm, vbl, legend=NULL, by.y = 'site_identifier',
                       extent = 'Portland', ...){

  # plot base map
  p <- mapPMbase(...)

  # Merge station shapefile w/ data
  mapStations <- merge(pmStations, dfm, by.x = "location_code", by.y = by.y)

  map_lims <- setMapFrame(extent = extent)

  # Map categorical variable
  p <- p + geom_sf(data=mapStations,
                      aes_string(fill=vbl),
                      size=5, shape = 21)
  if (!is.null(legend)) {
    p <- p + scale_fill_discrete(legend) +
      coord_sf(xlim=map_lims$xlim, ylim=map_lims$ylim)

  }
  p
}
