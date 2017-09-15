#' Map PAWMAP base map
#' @param pltArterial Should the arterials shapefile be plotted?
#' @param ldStreams Should the streams shapefile be loaded?  Note: this layer
#' takes considerable time to plot
#' @import ggplot2
#' @export


mapPMbase <- function(pltArterial=TRUE, ldStreams=FALSE,
                      xlim=c(7607000, 7700000), ylim=c(650000,729000),
                      streamsLoc='../R/pmMapsFiles/data/streamsFonly.rda'){

  ### load streams if TRUE
  if (ldStreams) load(streamsLoc)

  p <- ggplot() +
    # Map rivers
    geom_polygon(data=rivers[rivers$hole=='FALSE',],
                 aes(long, lat, group=group),
                 colour='royalblue2', fill='royalblue2') +
    geom_polygon(data=rivers[rivers$hole==TRUE,],
                 aes(long, lat, group=group),
                 colour='royalblue2', fill='white') +

    # Set up aesthetic elements
    coord_equal(xlim=xlim, ylim=ylim) +
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

  if (pltArterial) {
    # Add highways as landmarks
    p <- p + geom_path(aes(long, lat, group=group), colour='black', lwd=1, data=arter) +
      geom_path(aes(long, lat, group=group), colour='red', lwd=1.7, data=freeway)

  }

  return(p)
}

