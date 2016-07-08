#' Map PAWMAP data
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param loadObj Path of any data objects that need to be loaded (e.g., wq14)
#' @param GISdir Path to GIS data
#' @param ldRivers Should the river shapefile be loaded?
#' @param ldStreams Should the streams shapefile be loaded?  Note: this layer
#' takes considerable time to plot
#' @import rgdal
#' @import ggplot2
#' @import rgeos
#' @import maptools
#' @export


mapPMdata <- function(dfm, vbl, loadObj=NULL, GISdir=NULL, ldStreams=FALSE){
  # load data if specified
  if (!is.null(loadObj)) load(loadObj)
  # Default GISdir
  if (is.null(GISdir)) GISdir <- '../../../../General_GIS'


  GISdir <- '../../../../General_GIS'

  ### load streams if TRUE
  if (ldStreams) load('../pmMapsFiles/data/streamsFonly.rda')

  # Merge station shapefile w/ data
  mapStations <- pmStations
  mapStations@data <- merge(pmStations@data, dfm, by.x='id', by.y='loc_code')


  p <- ggplot() +
    # Map rivers
    geom_polygon(data=rivers[rivers$hole=='FALSE',],
                 aes(long, lat, group=group),
                 colour='royalblue2', fill='royalblue2') +
    geom_polygon(data=rivers[rivers$hole==TRUE,],
                 aes(long, lat, group=group),
                 colour='royalblue2', fill='white') +

    # Add highways as landmarks
#     geom_path(aes(long, lat, group=group), colour='black', lwd=1, data=arter) +
    # geom_path(aes(long, lat, group=group), colour='red', lwd=1.7, data=freeway) +


    # Set up aesthetic elements
    coord_equal(xlim=c(7600000, 7705000), ylim=c(647000,732000)) +
    xlab('') + ylab('')  + theme_bw() +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
          panel.grid.major=element_blank(),
          plot.title=element_text(size=16, face="bold"))

  # Add streams layer if TRUE
  if (ldStreams) {
    # Map streams
    p <- p + geom_line(aes(long, lat, group=group),
                       colour='royalblue2', data=streams)
  }

  # map PAWMAP station data on top
  p <- p + geom_point(data=mapStations@data,
             aes_string('x', 'y', size=vbl), colour='darkorange') +
    # guides(color = guide_legend(override.aes = list(size = 6))) +

    # Set scales
    # scale_color_manual(name='Detected', values=c('red', 'gray44')) +
    scale_size(name='Mean', range=c(3,12))

  return(p)
}

