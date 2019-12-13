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
#library(parallel)

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
if (!dir.exists(outDir)){
  dir.create(outDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Read in station IDs
weather_stations <- read.csv(paste0(inDir,"current_weather_stations.csv"), header=FALSE, col.names=c("name", "id"))

# collect weather data for each station
weather_stat_data = t(pbsapply(weather_stations$id, parse_xml))
weather_stat_data = data.frame(weather_stat_data, row.names=weather_stations$id)

#cores <- 3  
#cl <- makeCluster(cores)
#clusterEvalQ(cl, {library(XML); library(httr)})
#clusterExport(cl, varlist=c("weather_stations", "parse_xml"))
#parCollect <- parLapply(cl, weather_stations$id, fun=parse_xml)
#stopCluster(cl)
#weather_stat_data <- do.call(rbind.data.frame, parCollect)

colnames(weather_stat_data) <- c("name", "id", "lat", "lon", "obs", "link", "time")

# Remove stations without latitude or longitude
weather_stat_data <- weather_stat_data[!is.na(weather_stat_data$lat) | !is.na(weather_stat_data$lon),]

# --------------------------------------------------------------------------------------------------------------------
# Format information to a feature string for json
#weather_string = function(ID, name, link, obs, time, lon, lat){
#  str = paste('{"type": "Feature", "properties": {"name": "', name[match(ID, ID)], '", "id": "', ID, '", "url": "', link[match(ID, ID)], '", "obs": "',
#              obs[match(ID, ID)], '", "time": "', time[match(ID, ID)], '"}, "geometry": {"type": "Point", "coordinates": [',
#              lon[match(ID, ID)], ',',  lat[match(ID, ID)], ']}}', sep="")
#  return(str)
#}
# --------------------------------------------------------------------------------------------------------------------
# Combine all info into one string
#weat_obs <- weather_string(weather_stat_data$id, weather_stat_data$name, weather_stat_data$link, weather_stat_data$obs, weather_stat_data$time,
#                          weather_stat_data$lon, weather_stat_data$lat)
#weat_obs_last <- weat_obs[length(weat_obs)] #make sure the last feature doesn't end with a ","

weatherString <- paste0('{"type": "Feature", "properties": {"name": "', weather_stat_data$name, '", "id": "', weather_stat_data$id, '", "url": "', weather_stat_data$link, 
                        '", "obs": "', weather_stat_data$obs, '", "time": "', weather_stat_data$time, '"}, "geometry": {"type": "Point", "coordinates": [',
                        weather_stat_data$lon, ',',  weather_stat_data$lat, ']}}')
fullWString <- paste(weatherString, collapse=",")
json_merge <- paste0('weatherStations = {"type": "FeatureCollection","features": [', fullWString, ']};')

#weat_obs <- paste(weat_obs[1:length(weat_obs) - 1], ",", collapse="")

# Create geojson objects with the information.
# Merge geojson objects into a specific file format with data as the variable name.
#json_merge = paste('weatherStations = {"type": "FeatureCollection","features": [', weat_obs, weat_obs_last, ']};', sep="")



# Export data to geojson.
# cat(json_merge, file="weather_observations_extend.json")

# --------------------------------------------------------------------------------------------------------------------

# # Export data to geojson.
cat(json_merge, file=paste0(outDir, "weather_observations_extend.json"))
# --------------------------------------------------------------------------------------------------------------------
