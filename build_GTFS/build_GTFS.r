rm(list=ls(all=TRUE))
Sys.setlocale(locale="chinese")
setwd('./data')
#library(geojsonio)
library(ropencc)
library(dplyr)
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
library(tidyr)

arr_dep <- fread('1213.txt',header=F,quote='"',encoding = 'UTF-8',sep=",")
arr_dep$time <- fastPOSIXct(arr_dep$V6,'UTC')

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
stop_output <- as.data.frame(simple_stops)
stop_output$stop_id <- seq(1,nrow(stop_output))
stop_output <- stop_output[,c(5,2,4,3)]
names(stop_output) <- c("stop_id","stop_name","stop_lat","stop_lon")


setwd('../build_GTFS')
cat("stop_id,stop_name,stop_lat,stop_lon","\n",file="stops.txt")
write.table(stop_output,file="stops.txt",sep = ",",row.names=FALSE,col.names=FALSE,append=TRUE)
setwd('../data')

#####################################################################
#write out routes.txt
routes_output <- data.frame(route_id=seq(1,length(unique(arr_dep$V1))),
                        route_short_name=unique(arr_dep$V1),
                        route_long_name=unique(arr_dep$V1),
                         route_type=3
)

setwd('../build_GTFS')
cat("route_id,route_short_name,route_long_name,route_type","\n",file="routes.txt")
write.table(routes_output,file="routes.txt",sep = ",",row.names=FALSE,col.names=FALSE,append=TRUE,fileEncoding = "UTF-8")
setwd('../data')

#####################################################################
#write out trips.txt
#first eliminate duplicate records



route_id=5
route_name=routes_output[routes_output$route_id==route_id,]$route_short_name
temp_dep <- arr_dep[V1==route_name]
setkey(temp_dep,V2,time)

temp_dep[,CID:=paste0(V4,V5)]

temp_dep$seq <- with(temp_dep,
     rep(seq(length(rle(CID)$values)),rle(CID)$lengths)
     )
temp_dep$inner_seq <- with(temp_dep,
                           ave(seq_along(seq),seq,FUN=seq_along)
)

temp_dep <- temp_dep[inner_seq==1]

temp_dep[,c("V6","CID","seq","inner_seq"):=NULL]

temp_dep$seq <- with(temp_dep,
                     rep(seq(length(rle(V5)$values)),rle(V5)$lengths)
)
temp_dep$inner_seq <- with(temp_dep,
                           ave(seq_along(seq),seq,FUN=seq_along)
)

temp_dep$instance <- with(temp_dep,
  rep(rle(seq)$lengths,rle(seq)$lengths)
)

temp_dep <- temp_dep[inner_seq==1 |inner_seq==instance ]



temp_faulty=as.data.frame(tapply(temp_dep$V4,temp_dep$seq,FUN=paste, collapse = ""))
names(temp_faulty) <- "value"
temp_faulty[temp_faulty$value=="到站到站",]

temp_dep[inner_seq==instance & inner_seq>1]$V4 <- "离站"


temp_dep_stripped <- temp_dep[,c(seq(1,7))]

temp_dep=temp_dep_stripped %>% spread(V4,time)

tryCatch(temp_dep_stripped %>% spread(V4,time),error=c)

setkey(temp_dep,seq)


#temp_dep <- temp_dep[order(seq)]

temp_dep[c(seq(12832, 12833))]

temp_dep[c(seq(16363, 16364))]

#####################################################################
#write out stop_times.txt




######################################################################
#visualize stops

m <- leaflet(data=simple_stops) %>%
  addTiles() %>%
  addMarkers(popup = ~as.character(name), label = ~as.character(name))
m

#########################################
