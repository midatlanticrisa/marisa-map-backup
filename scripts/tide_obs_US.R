# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Jan 25, 2019    - expanded to entire US
# prior edit: June 18, 2018
# prior edit: June 16, 2017
#
# This script parses XML data of current tide station observations from the
# National Ocean and Atmospheric Administration and outputs the results as
# a figure of preliminery 6-minute water level heights.
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
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("XML")) { install.packages("XML") }
if (!require("httr")) { install.packages("httr") }

library(RCurl)
library(XML)
library(httr)
library(anytime)
library(pbapply)


# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="rsc64dot1x-60.ems.psu.edu"){
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else{
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

##load station ids
#load(paste0(inDir, "tideStationIDs.RData"))
load(paste0(inDir, "tideStationIDs_regional.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Set up common variables.
datum <- "MLLW"
gl.datum <- "IGLD"
msl.datum <- "MSL"
timezone <- "GMT"
units <- "metric"
cores <- 1

# --------------------------------------------------------------------------------------------------------------------

# Run through each station.
if(cores>1){
  ##run in parallel
  library(parallel)
  tideStations <- mclapply(tideIDs, tideStationData, spDatum=datum, timez=timezone, un=units, mc.cores=cores)
  #tideStationsMSL <- pblapply(tideIDsMSL, tideStationData, spDatum=msl.datum, timez=timezone, un=units)  ##Uncomment is MSL stations included
  tideStationsGL <- mclapply(tideIDsGrtLakes, tideStationData, spDatum=gl.datum, timez=timezone, un=units, mc.cores=cores)
}else{
  ##run on single core
  tideStations <- pblapply(tideIDs, tideStationData, spDatum=datum, timez=timezone, un=units)
  #tideStationsMSL <- pblapply(tideIDsMSL, tideStationData, spDatum=msl.datum, timez=timezone, un=units)  ##Uncomment is MSL stations included
  tideStationsGL <- pblapply(tideIDsGrtLakes, tideStationData, spDatum=gl.datum, timez=timezone, un=units)
}

tideStations <- do.call(rbind.data.frame, tideStations)
#tideStationsMSL <- do.call(rbind.data.frame, tideStationsMSL)  ##Uncomment is MSL stations included
tideStationsGL <- do.call(rbind.data.frame, tideStationsGL)

tideStations <- tideStations[tideStations$lon>=-82.0 & tideStations$lon<=-73.0 & tideStations$lat>=36.0 & tideStations$lat<=43.5,]
tideStationsGL <- tideStationsGL[tideStationsGL$lon>=-82.0 & tideStationsGL$lon<=-73.0 & tideStationsGL$lat>=36.0 & tideStationsGL$lat<=43.5,]

# --------------------------------------------------------------------------------------------------------------------
# Combine all info into one string

json_merge = paste0('tideStations = {"type": "FeatureCollection","features": [',
                   #paste(tideStations, collapse=", "), paste(tideStationsMSL, collapse=", "), paste(tideStationsGL, collapse=", "), ']};')
                  paste(tideStations, collapse=", "), paste(tideStationsGL, collapse=", "), ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "tide_station_obs_extend.json"))


#############################################
##test code to write out as geojson file
#library(rgdal)

#fullTab <- rbind.data.frame(tideStations, tideStationsMSL, tideStationsGL)
#fullTab <- fullTab[is.na(fullTab$lat)==F,]
#fullTab$id <- as.character(fullTab$id)
#coordinates(fullTab) <- c("lon", "lat")
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(weather_stat_data$lon), as.numeric(weather_stat_data$lat)), data=weather_stat_data, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#sptData <- data.frame(id=fullTab$id, lon=as.numeric(as.character(fullTab$lon)), lat=as.numeric(as.character(fullTab$lat)))
#nonSptData <- data.frame(id=fullTab$id, obs=fullTab$obs, link=fullTab$url, image=fullTab$image, time=fullTab$time)
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(sptData$lon), as.numeric(sptData$lat)), data=sptData, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#writeOGR(spTab, dsn=paste0(outDir, "testOutput/"), layer="tideObs", driver="ESRI Shapefile")
#write.csv(nonSptData, paste0(outDir, "testOutput/tideObsTab.csv"), row.names=F)
#write.csv(fullTab, paste0(outDir, "testOutput/tideObsFull.csv"), row.names=F)

