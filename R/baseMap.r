library(rgdal)
library(ggplot2)


GISdir <- '../../../../General_GIS'

## Old way
# streams <- readOGR(GISdir, 'pdx_streams_red.shp')
# rivers.shp <- readShapePoly(file.path(GISdir, 'pdx_waterbodies.shp'))
# wtshds.shp <- readShapePoly(file.path(datwd, 'BES_Topo_Watershed.shp'))

### Rivers
load('./data/rivers.rda')
## To read in rivers polygon
# rivers <- readOGR(dsn=GISdir, layer="pdx_waterbodies")
# rivers@data$id = rownames(rivers@data)
# rivers.points = fortify(rivers, region="id")
# rivers.df = merge(rivers.points, rivers@data, by="id")

### PAWMAP Stations
stations <- readOGR(dsn=GISdir, layer="Final_PAWMAP_ALLPanels")
stations@data$id = rownames(stations@data)
stations.points = fortify(stations, region="id")
stations.df = merge(stations.points, stations@data, by="id")

ggplot(rivers.df) +
  # Map rivers
  geom_polygon(data=rivers.df[rivers.df$hole=='FALSE',],
               aes(long, lat, group=group),
               colour='royalblue2', fill='royalblue2') +
  geom_polygon(data=rivers.df[rivers.df$hole==TRUE,],
               aes(long, lat, group=group),
               colour='royalblue2', fill='white') +
  coord_equal(xlim=c(7600000, 7720000), ylim=c(642000,729000)) +
  xlab('') + ylab('')  + theme_bw() +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(),
        panel.grid.major=element_blank(),
                            plot.title=element_text(size=16, face="bold")) +
  geom_point(data=stations.df, aes(x, y))

