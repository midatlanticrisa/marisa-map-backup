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
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

##load station ids
load(paste0(inDir, "tideStationIDs.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Set up common variables.
datum <- "MLLW"
gl.datum <- "IGLD"
msl.datum <- "MSL"
timezone <- "GMT"
units <- "metric"
cores <- 3

# --------------------------------------------------------------------------------------------------------------------


# Run through each station.
tideStations <- pbsapply(tideIDs, tideStationData, spDatum=datum, timez=timezone, un=units)
tideStationsMSL <- pbsapply(tideIDsMSL, tideStationData, spDatum=msl.datum, timez=timezone, un=units)
tideStationsGL <- pbsapply(tideIDsGrtLakes, tideStationData, spDatum=gl.datum, timez=timezone, un=units)

#cl <- makeCluster(cores)
#clusterEvalQ(cl, {library(RCurl); library(XML); library(httr); library(anytime)})
#clusterExport(cl, varlist=c("tideIDs", "tideStationData", "datum", "timezone", "units", "collectLatestTidal", "retry"))
#tideStations <- parSapply(cl, tideIDs, tideStationData, spDatum=datum, timez=timezone, un=units)  
#tideStationsGL <- parSapply(cl, tideIDs, tideIDsGrtLakes, spDatum=gl.datum, timez=timezone, un=units) 
#stopCluster(cl)
#tideStationsMSL <- pbsapply(tideIDsMSL, tideStationData, spDatum=msl.datum, timez=timezone, un=units)

# --------------------------------------------------------------------------------------------------------------------
# Combine all info into one string

json_merge = paste0('tideStations = {"type": "FeatureCollection","features": [',
                   paste(tideStations, collapse=", "), paste(tideStationsMSL, collapse=", "), paste(tideStationsGL, collapse=", "), ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "tide_station_obs_extend.json"))

