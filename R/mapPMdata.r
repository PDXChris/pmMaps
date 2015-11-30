#' Map PAWMAP data
#' @param dfm Data frame with data to be mapped
#' @param vbl The field containing the value to map
#' @param loadObj Path of any data objects that need to be loaded (e.g., wq14)
#' @param GISdir Path to GIS data
#' @param ldRivers Should the river shapefile be loaded?
#' @param ldStreams Should the streams shapefile be loaded?
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

  ## Old way
  # streams <- readOGR(GISdir, 'pdx_streams_red.shp')
  # rivers.shp <- readShapePoly(file.path(GISdir, 'pdx_waterbodies.shp'))
  # wtshds.shp <- readShapePoly(file.path(datwd, 'BES_Topo_Watershed.shp'))

  ### Rivers
  if (ldStreams) load('../pmMapsFiles/data/streamsFonly.rda')
  ## To read in rivers polygon
  # rivers <- readOGR(dsn=GISdir, layer="pdx_waterbodies")
  # rivers@data$id = rownames(rivers@data)
  # rivers.points = fortify(rivers, region="id")
  # rivers = merge(rivers.points, rivers@data, by="id")

  ### PAWMAP Stations
  # stations <- readOGR(dsn=GISdir, layer="Final_PAWMAP_ALLPanels")
  # stations@data$id = rownames(stations@data)
  # stations.points = fortify(stations, region="id")
  # stations.df = merge(stations.points, stations@data, by="id")

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

    coord_equal(xlim=c(7600000, 7705000), ylim=c(647000,732000)) +
    xlab('') + ylab('')  + theme_bw() +

    # Set up aesthetic elements
    coord_equal(xlim=c(7600000, 7705000), ylim=c(647000,732000)) +
    xlab('') + ylab('')  + theme_bw() +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
          panel.grid.major=element_blank(),
          plot.title=element_text(size=16, face="bold")) +

    # map PAWMAP stations
    geom_point(data=mapStations@data,
               aes_string('x', 'y', size=vbl, color='cns')) +

    # Set scales
    scale_size(name='Mean', range=c(5,12)) +
    scale_color_manual(name='Detected', values=c('red', 'darkgrey')) +
    guides(color = guide_legend(override.aes = list(size = 6)))

  if (ldStreams) {
    # Map streams
    p <- p + geom_line(aes(long, lat, group=group), colour='royalblue2', data=streams)
  }

  p
}

