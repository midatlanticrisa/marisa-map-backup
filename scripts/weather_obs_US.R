# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Originally authored by Kelsey Ruckert,
# Currently maintained by Matthew D. Lisk (mdl5548@psu.edu)
# Last edit: December , 2019
# Edit Dates: April 4, 2017; April 14 (change to creating geojson file)
#
# This script parses XML data of current weather station observations from the
# National Weather Service and outputs the results in a single file.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# --------------------------------------------------------------------------------------------------------------------
# Ensure necessary packages are installed and loaded
if (!require("XML")) { install.packages("XML") }
if (!require("httr")) { install.packages("httr") }
library(XML)
library(httr)
library(pbapply)

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else{  ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

cores <- 1 

# --------------------------------------------------------------------------------------------------------------------
# Read in station IDs
weather_stations <- read.csv(paste0(inDir,"current_weather_stations.csv"), header=FALSE, col.names=c("name", "id"))

# collect weather data for each station
if(cores>1){
  library(parallel)
  weather_stat_data <- mclapply(weather_stations$id, parseWS_xml, mc.cores=cores)
  weather_stat_data <- do.call(rbind.data.frame, weather_stat_data)
}else{
  weather_stat_data <- t(pbsapply(weather_stations$id, parseWS_xml))
  weather_stat_data <- data.frame(weather_stat_data, row.names=weather_stations$id)
}
colnames(weather_stat_data) <- c("name", "id", "lat", "lon", "obs", "link", "date", "time")
weather_stat_data$lon <- as.numeric(as.character(weather_stat_data$lon))
weather_stat_data$lat <- as.numeric(as.character(weather_stat_data$lat))

# Remove stations without latitude or longitude
weather_stat_data <- weather_stat_data[!is.na(weather_stat_data$lat) | !is.na(weather_stat_data$lon),]
#ggg <- weather_stat_data[weather_stat_data$id=="KFFA",]

##remove points outside of project area for faster loading
weather_stat_data <- weather_stat_data[weather_stat_data$lon>=-82.0 & weather_stat_data$lon<=-73.0 & weather_stat_data$lat>=36.45 & weather_stat_data$lat<=43.75,]

##create the observation records to be mapped
weatherString <- paste0('{"type": "Feature", "properties": {"name": "', weather_stat_data$name, '", "id": "', weather_stat_data$id, '", "url": "', weather_stat_data$link, 
                        '", "obs": "', weather_stat_data$obs, '", "time": "', paste0("Last Updated on ", weather_stat_data$date, " at ", weather_stat_data$time), 
                        '"}, "geometry": {"type": "Point", "coordinates": [', weather_stat_data$lon, ',',  weather_stat_data$lat, ']}}')
json_merge <- paste0('weatherStations = {"type": "FeatureCollection","features": [', paste(weatherString, collapse=","), ']};')

## Export data to geojson.
cat(json_merge, file=paste0(outDir, "weather_observations_extend.json"))
# --------------------------------------------------------------------------------------------------------------------

#############################################
##test code to write out as geojson file
#library(rgdal)

#fullTab <- rbind.data.frame(NDBC_buoy_data, NDBC_stat_data, non_NDBC_data)
#coordinates(fullTab) <- c("lon", "lat")
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(weather_stat_data$lon), as.numeric(weather_stat_data$lat)), data=weather_stat_data, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#sptData <- data.frame(id=weather_stat_data$id, name=weather_stat_data$name, lon=as.numeric(as.character(weather_stat_data$lon)), lat=as.numeric(as.character(weather_stat_data$lat)))
#nonSptData <- data.frame(id=weather_stat_data$id, obs=weather_stat_data$obs, link=weather_stat_data$link, time=weather_stat_data$time)
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(sptData$lon), as.numeric(sptData$lat)), data=sptData, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#writeOGR(spTab, dsn=paste0(outDir, "testOutput/"), layer="weatherObs", driver="ESRI Shapefile")
#write.csv(nonSptData, paste0(outDir, "testOutput/weatherObsTab.csv"), row.names=F)
#write.csv(weather_stat_data, paste0(outDir, "testOutput/weatherObsFull.csv"), row.names=F)


