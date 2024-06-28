#' Map PAWMAP base map
#' @param pltArterial Should the arterials shapefile be plotted?
#' @param ldStreams Should the streams shapefile be loaded?  Note: this layer
#' takes more time to plot
#' @param waterColor color for rivers and streams
#' @import ggplot2
#' @import sf
#' @export


mapPMbase <- function(pltArterial=TRUE, ldStreams=FALSE,
                      waterColor = 'royalblue2'){

  # Map rivers
  p <- ggplot() +
    geom_sf(data=rivers_pdx, colour = waterColor, fill = waterColor)


  # Add streams layer if TRUE
    if (ldStreams) {
      # Map streams
      p <- p + geom_sf(colour = waterColor, inherit.aes = FALSE,
                       data=streams)
    }

  if (pltArterial) {
    # Add highways as landmarks
    p <- p + geom_sf(colour='black', lwd=1, data=arter, inherit.aes = FALSE) +
      geom_sf(colour='red', lwd=1.7, inherit.aes = FALSE, data=freeways)

  }

  # Set up aesthetic elements
  p <- p +
    xlab('') + ylab('')  + theme_bw() +
    theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
          panel.grid.major=element_blank(),
          plot.title=element_text(size=16, face="bold"))

  return(p)
}

