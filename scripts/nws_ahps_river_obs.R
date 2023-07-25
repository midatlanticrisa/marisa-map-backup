# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: May 18, 2023: Complete revamp of data download of stream gauges
#  Changing data source from the USGS to the AHPS.
#
# This script collects river gauge observations from the NOAA NWS AHPS:
# https://water.weather.gov/ahps/download.php
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
ptm <- proc.time()
if (!require("terra")) { install.packages("terra") }
if (!require("sf")) { install.packages("sf") }
if (!require("geojsonsf")) { install.packages("geojsonsf") }
if (!require("geojsonio")) { install.packages("geojsonio") }

library(terra)
library(sf)
library(geojsonsf)
library(geojsonio)
library(compiler)
enableJIT(3)
enableJIT(3)

inDir <- getwd() #"/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
downDir <- "/Users/klr324/Documents/Github/marisa-map-backup/resources/"
outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
outDir <- getwd()
plotDir <- paste0(outDir, "River_figs/")

# Should you record the current observations
recordObservations <- FALSE

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

if (!dir.exists(plotDir)){
  dir.create(plotDir, recursive=T)
}

# Source functions
source(paste0(inDir, "/download_parse_functions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Download only the river gauges for the MARISA region exporting the information
# into a geojson file.
ptm <- proc.time()
bbox = c(-82.0, -73.0, 36.46, 43.75)
river_sf = collectRiverData(bbox, downDir, 
                            outfile=paste0(outDir, "NWSRiverGauges.geojson"))
ptmEnd <- proc.time() - ptm
# --------------------------------------------------------------------------------------------------------------------
if(recordObservations){
  # Create a list of the current observations for each river gauge to keep a 
  # record of observations on file. This will be used for plots.
  riverObs = lapply(X = 1:nrow(river_sf), 
                    function(X){
                      list(ID = river_sf$GaugeLID[X], 
                           loc = river_sf$Location[X], 
                           lat = river_sf$Latitude[X], 
                           lon = river_sf$Longitude[X], 
                           action = river_sf$Action[X], 
                           minor = river_sf$Flood[X], 
                           mod = river_sf$Moderate[X], 
                           major = river_sf$Major[X], 
                           obs = data.frame(time = river_sf$ObsTime[X], 
                                            height = river_sf$Observed[X], 
                                            discharge = river_sf$SecValue[X]))
                    })
  names(riverObs) = river_sf$GaugeLID
  
  # Save the current observations to a file recording the past 7 days of observations
  recordedObs = recordData(currentObs = riverObs, 
                           recordFile = paste0(outDir, "riverObsRecord.RData"), tz = "GMT", 
                           return.val=TRUE)
}

# --------------------------------------------------------------------------------------------------------------------

# Download forecasts and past observations
ptm <- proc.time()
plotData = lapply(X = river_sf$GaugeLID[1:10], function(X){collectRiverForecast(X)})
proc.time() - ptm

library(compiler)
RiverFuncCmp <- cmpfun(collectRiverForecast)

ptm <- proc.time()
timetest = RiverFuncCmp(river_sf$GaugeLID[1])
proc.time() - ptm

ptm <- proc.time()
# Create the plots with operational forecasts where available
lapply(X=1:length(plotData), function(X){
  river_plot(plotData[[X]], tz="UTC", 
             p.tz="America/New_York",
             p.width = 4, p.height = 2.5, p.dir = plotDir)
})

# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
