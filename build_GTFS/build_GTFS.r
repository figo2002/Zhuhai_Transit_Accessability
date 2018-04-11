rm(list=ls(all=TRUE))
Sys.setlocale(locale="chinese")
setwd('./data')
#library(geojsonio)
library(ropencc)
library(spdplyr)
library(jsonlite)
library(leaflet)
library(geojson)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(stringdist)
library(data.table)
library(fasttime)

jsonfile <- "osm_bus_stops_zhuhai.geojson"


stops<- readOGR(jsonfile, require_geomType="wkbPoint",use_iconv = TRUE, encoding = "UTF-8")

# read stations defined by polygons
stations <- readOGR(jsonfile,  require_geomType="wkbPolygon",use_iconv = TRUE, encoding = "UTF-8")
# convert station polygons to points
stations <- SpatialPointsDataFrame(gCentroid(stations, byid=TRUE), 
                                     stations@data, match.ID=FALSE)

all_stops=rbind(stops,stations)
#all_stops=as.data.frame(rbind(stops,stations))


noname_stops <- all_stops[is.na(all_stops$name),]
  
named_stops <- all_stops[!is.na(all_stops$name),]

ccts = converter(T2S)
named_stops$name <- sapply(as.character(named_stops$name),function(x) {
  ccts[x]
})

################################################### 
#filter out stops points with at most 4 spottings


stop_names=as.data.frame(tapply(named_stops$name,named_stops$name,FUN=length))
names(stop_names) <- c("spots")
stop_names$name <- row.names(stop_names)
good_stops <- subset(stop_names,spots<=4)

################################################################

coupled_polygon=lapply(good_stops$name,
      FUN=function(x,SpPointsDF){
        points <- coordinates(subset(SpPointsDF,name==x))
        Polygons(list(Polygon(rbind(points,points[1,]))),x)
      },
      SpPointsDF=named_stops)

coupled_polygon=SpatialPolygons(coupled_polygon,proj4string = CRS("+proj=longlat +ellps=WGS84"))

coupled_polygon=SpatialPolygonsDataFrame(coupled_polygon,good_stops, match.ID = TRUE)

simple_stops=gCentroid(coupled_polygon,byid=TRUE)

simple_stops <- SpatialPointsDataFrame(simple_stops,good_stops)
#####################################################################
#write  out stops.txt
stop_ouput <- as.data.frame(simple_stops)
stop_ouput$stop_id <- seq(1,nrow(stop_ouput))
stop_ouput <- stop_ouput[,c(5,2,4,3)]
names(stop_ouput) <- c("stop_id","stop_name","stop_lat","stop_lon")

write.csv()

#####################################################################
#for one single route, output the 



######################################################################
#visualize stops

m <- leaflet(data=simple_stops) %>%
  addTiles() %>%
  addMarkers(popup = ~as.character(name), label = ~as.character(name))
m

#########################################
