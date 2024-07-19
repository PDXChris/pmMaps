#' Return Axis Limits for a Map
#' @param extent The extent of the map frame.
#' @export


setMapFrame <- function(extent = c('Portland', 'West Hills',
                                   'Tryon Creek', 'NW Streams')){

  map_lims <- mapFrame[extent][[1]]

  return(map_lims)

}
