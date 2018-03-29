rm(list=ls(all=TRUE))
Sys.setlocale(locale="chinese")
setwd('./data')
#library(geojsonio)
library(jsonlite)
library(leaflet)
library(geojson)
library(sp)
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


