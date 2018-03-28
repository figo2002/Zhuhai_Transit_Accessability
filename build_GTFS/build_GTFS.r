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

jsonfile <- paste0(getwd(),"/osm_bus_stops_zhuhai.geojson")



stops<- readOGR(jsonfile, require_geomType="wkbPoint",use_iconv = TRUE, encoding = "UTF-8")

stations <- readOGR(jsonfile,  require_geomType="wkbPolygon",use_iconv = TRUE, encoding = "UTF-8")

stations_p <- SpatialPointsDataFrame(gCentroid(stations, byid=TRUE), 
                                     stations@data, match.ID=FALSE)
tem=rbind(stops,stations_p)
kk=as.data.frame(rbind(stops,stations_p))