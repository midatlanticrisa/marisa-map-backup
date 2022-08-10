# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Feb. 5, 2019: Extend country-wide
# Previous edit: June 13, 2018
# Previous edit: April 27, 2017
#
# This script parses RSS meteorological data from buoy stations from the
# National Data Buoy Center and outputs the results in a single file.
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
# https://www.ndbc.noaa.gov/docs/ndbc_web_data_guide.pdf
# Ensure necessary packages are installed and loaded
#ptm <- proc.time()
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("XML")) { install.packages("XML") }
if (!require("stringr")) { install.packages("stringr") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(XML)
library(stringr)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local" | comp$nodename=="E2-EES-RSML638" | comp$nodename=="rsc64dot1x-59.ems.psu.edu"){  ##workstation
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

# --------------------------------------------------------------------------------------------------------------------
# Create a vector of buoy stations using lists from the NDBC
readr.total <- retry(read.csv("https://www.ndbc.noaa.gov/data/stations/station_table.txt", sep="|"))
colnames(readr.total)[1] <- sapply(strsplit(colnames(readr.total)[1],"X.."), "[[", 2)
readr.total <- readr.total[-1,]  ##removes the first row as nothing of importance

##pair down to only the columns needed
isoCoords <- gsub(pattern=" \\(.*$", "", x=readr.total$LOCATION)
splitCoords <- strsplit(isoCoords," ")
buoy_dat <- data.frame(ID=readr.total$STATION_ID, name=readr.total$NAME, 
                         lat=sapply(splitCoords, function(xy){ifelse(xy[2]=="N", as.numeric(xy[1]), -as.numeric(xy[1]))}), 
                         lon=sapply(splitCoords, function(xy){ifelse(xy[4]=="E", as.numeric(xy[3]), -as.numeric(xy[3]))}))

# US bounding box based on the farthest points: includes alaska and hawaii
# # http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
# pointBurrow = 71.388889, -156.479167 #northernmost point
# kaLae = 18.910833, -155.681111 # southern most point
# sailRock = 44.812556, -66.947028 # east
# peakedIsland = 52.920556, -172.437778 #west
#US_buoys <- unique(buoy_dat[buoy_dat$lon>=-172.437778 & buoy_dat$lon<=-66.947028 & buoy_dat$lat>=18.910833 & buoy_dat$lat<=71.388889,])
##remove points outside of project area for faster loading
US_buoys <- unique(buoy_dat[buoy_dat$lon>=-82.0 & buoy_dat$lon<=-73.0 & buoy_dat$lat>=36.46 & buoy_dat$lat<=43.75,])

NDBC_buoys <- retry(read.table("https://www.ndbc.noaa.gov/data/stations/buoyht.txt", skip=7, col.names=c("ID", "siteElv", "airTempElv", "anemometerElv", "barometerElv")))
NDBC_stations <- retry(read.table("https://www.ndbc.noaa.gov/data/stations/cmanht.txt", skip=7, col.names=c("ID", "siteElv", "airTempElv", "anemometerElv", "tideRef", "barometerElv")))
NDBC_stations$ID <- sapply(as.character(NDBC_stations$ID), tolower)
non_NDBC_stations <- retry(read.table("https://www.ndbc.noaa.gov/data/stations/non_ndbc_heights.txt", skip=3, col.names=c("ID", "siteElv", "airTempElv", "anemometerElv", "tideRef", "barometerElv", "wtmpElv", "waterDpth", "watchCircle")))[-1,]


# Remove all buoys outside the US boundary
NDBC_buoys <- NDBC_buoys[NDBC_buoys$ID %in% US_buoys$ID,]
NDBC_stations <- NDBC_stations[NDBC_stations$ID %in% US_buoys$ID,]
non_NDBC_stations <- non_NDBC_stations[non_NDBC_stations$ID %in% US_buoys$ID,]

##format buoy data
#ptmDownload <- proc.time()
NDBC_buoy_data <- collectBuoyData(NDBC_buoys$ID, US_buoys)
NDBC_stat_data <- collectBuoyData(NDBC_stations$ID, US_buoys)
non_NDBC_data <- collectBuoyData(non_NDBC_stations$ID, US_buoys)
#ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))

# Combine all buoy info into one string
ndbc_bu <- paste0('{"type": "Feature", "properties": {"name": "', NDBC_buoy_data$name, '", "id": "', NDBC_buoy_data$id, '", "url": "', NDBC_buoy_data$link, '", "obs": "',
                  NDBC_buoy_data$obs, '", "time": "', paste0("Last Updated on ", NDBC_buoy_data$date, " at ", NDBC_buoy_data$time), '"}, "geometry": {"type": "Point", "coordinates": [', NDBC_buoy_data$lon, ',', NDBC_buoy_data$lat, ']}}')
ndbc_st <- paste0('{"type": "Feature", "properties": {"name": "', NDBC_stat_data$name, '", "id": "', NDBC_stat_data$id, '", "url": "', NDBC_stat_data$link, '", "obs": "',
                  NDBC_stat_data$obs, '", "time": "', paste0("Last Updated on ", NDBC_stat_data$date, " at ", NDBC_stat_data$time), '"}, "geometry": {"type": "Point", "coordinates": [', NDBC_stat_data$lon, ',', NDBC_stat_data$lat, ']}}')
ndbc_non <- paste0('{"type": "Feature", "properties": {"name": "', non_NDBC_data$name, '", "id": "', non_NDBC_data$id, '", "url": "', non_NDBC_data$link, '", "obs": "',
                   non_NDBC_data$obs, '", "time": "', paste0("Last Updated on ", non_NDBC_data$date, " at ", non_NDBC_data$time), '"}, "geometry": {"type": "Point", "coordinates": [', non_NDBC_data$lon, ',', non_NDBC_data$lat, ']}}')

# Create a geojson object with the observation and statement info and merge into a
# specific file format with Buoys as the variable name.
json_merge = paste0('Buoys = {"type": "FeatureCollection","features": [', paste(ndbc_st, collapse=","), ",", paste(ndbc_bu, collapse=","), ",", paste(ndbc_non, collapse=","), ']};')

# Export data to geojson.
# cat(json_merge, file="buoys_extend2.js")

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "buoys_extend.js"))
# --------------------------------------------------------------------------------------------------------------------
#ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd[3]))


##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "buoyObsTracking.RData")
if(file.exists(timeFile)==T){
  load(tideFile)
  timeBuoyObs[nrow(timeBuoyObs)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timeBuoyObs", file=timeFile)
}else{
  timeBuoyObs <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timeBuoyObs", file=timeFile)
}

#############################################
##test code to write out as geojson file
#library(rgdal)

#fullTab <- rbind.data.frame(NDBC_buoy_data, NDBC_stat_data, non_NDBC_data)
#coordinates(fullTab) <- c("lon", "lat")
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(fullTab$lon), as.numeric(fullTab$lat)), data=fullTab, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#writeOGR(spTab, dsn=paste0(outDir, "buoys_extend.GeoJSON"), layer="Bouys", driver="GeoJSON")
#writeOGR(spTab, dsn=paste0(outDir, "buoys_extend.kml"), layer="Bouys", driver="KML")
#write.csv(fullTab, paste0(outDir, "buoys_extend.csv"), row.names=F)

#sptData <- data.frame(id=fullTab$id, name=fullTab$name, lon=as.numeric(as.character(fullTab$lon)), lat=as.numeric(as.character(fullTab$lat)))
#nonSptData <- data.frame(id=fullTab$id, obs=fullTab$obs, link=fullTab$link, time=fullTab$time)
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(sptData$lon), as.numeric(sptData$lat)), data=sptData, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#writeOGR(spTab, dsn=paste0(outDir, "testOutput/"), layer="buoysObs", driver="ESRI Shapefile")
#write.csv(nonSptData, paste0(outDir, "testOutput/buoysObsTab.csv"), row.names=F)
#write.csv(fullTab, paste0(outDir, "testOutput/buoyObsFull.csv"), row.names=F)

