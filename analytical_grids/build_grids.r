rm(list=ls(all=TRUE))
Sys.setlocale(locale="chinese")
setwd('./data')
#library(geojsonio)
#library(ropencc)
library(dplyr)
#library(jsonlite)
library(leaflet)
library(geojson)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
#library(stringdist)
library(data.table)
#library(fasttime)
#library(tidyr)
library(raster)

jsonfile <- "boundary.geojson"

boundary <- readOGR(jsonfile,  require_geomType="wkbPolygon",use_iconv = TRUE, encoding = "UTF-8")

boundary <- boundary[boundary@data$name=="珠海市" & !is.na(boundary@data$name) ,]


zh_mainland=boundary@polygons[[1]]@Polygons[[15]]

zh_mainland <- SpatialPolygons(list(Polygons(list(zh_mainland),ID=1)),
                proj4string= CRS("+proj=longlat +ellps=WGS84")
)


e <- extent(bbox(zh_mainland))                  # define boundaries of object
r <- raster(e)                           # create raster object 
dim(r) <- c(80, 80)                      # specify number of cells
projection(r) <- CRS(proj4string(zh_mainland))  # give it the same projection as port
g <- as(r, 'SpatialPolygonsDataFrame')

p <- g[zh_mainland,]

map <- gIntersection(p,zh_mainland,byid = TRUE,drop_lower_td = TRUE) 

map <-SpatialPolygonsDataFrame(map,data.frame(UID=seq(1,length(map))),match.ID = FALSE)

#leaflet(boundary@polygons[[1]]@Polygons[[15]]) %>% 
#addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
#                                           color="black") %>% 
#  addTiles()

#plot(map)

leaflet(map) %>%
addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.4, smoothFactor = 0.5,
            color="black",weight = 0.2) %>% 
  addTiles()

setwd('../analytical_grids')
writeOGR(obj=map, dsn="grids", layer="grids", driver="ESRI Shapefile")
setwd('../data')


