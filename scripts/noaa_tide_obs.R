# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Latest edit: May 4, 2023: Complete revamp of data download and tide functions
# prior edit: Jan 25, 2019: expanded to entire US
# prior edit: June 18, 2018
# prior edit: June 16, 2017
#
# This script parses XML data of 6-minute tide station observations from the
# National Ocean and Atmospheric Administration.
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
if (!require("data.table")) { install.packages("data.table") }
if (!require("XML")) { install.packages("XML") }
if (!require("httr")) { install.packages("httr") }

library(data.table)
library(XML)
library(httr)
library(compiler)
enableJIT(3)
enableJIT(3)

# # what computer am I on?
# comp <- as.data.frame(t(Sys.info()))
# 
# # important file locations
# if(comp$nodename=="E2-EES-RSML638.local" | comp$nodename=="E2-EES-RSML638" | comp$nodename=="rsc64dot1x-59.ems.psu.edu"){  ##workstation
#   inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
#   outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
# }else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
#   inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
#   outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
# }else if(comp$nodename=="firkin.eesi.psu.edu"){  ##firkin
#   inDir <- "/firkin/s0/mdl5548/githubRepos/marisa-map-backup/scripts/"
#   outDir <- "/firkin/s0/mdl5548/marisaMapOutput/"
# }else{ ##idocrase
#   inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
#   outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
# }

inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
plotDir <- paste0(outDir, "Tide_figs/")

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

if (!dir.exists(plotDir)){
  dir.create(plotDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Load or download active NOAA tide station metadata for the MARISA region
# Load if the file exists and is less than 24 hours old, otherwise
# download the data. We should only need to do this once a day (24 hours) to capture new or 
# get rid of nonactive stations.
filenm = "NOAAtideIDs.txt"

# Time differences in mins
if(file.exists(filenm) & (Sys.time() - file.info(filenm)$ctime) < (24*60)){
  tideStations = read.table(filenm, header=TRUE)
  
} else {
  bbox = c(-82.0, -73.0, 36.46, 43.75)
  tideStations = collectTideIDs(filenm=filenm, bbox, returnIDs=TRUE)
  
}

# --------------------------------------------------------------------------------------------------------------------
# Coastal areas experience two high and two low tides every 24 hours and 50 mins. 
# High tides occur 12 hours and 25 minutes apart. It takes six hours and 12.5 mins
# for the water at the shore to go from high to low, or from low to high.
# To account for tidal changes, we should only need to regenerate the plots every
# 3 hours.
# https://oceanservice.noaa.gov/education/tutorial_tides/tides05_lunarday.html

# Time differences in mins
if(all((Sys.time() - file.info(paste0(plotDir, list.files(plotDir)))$ctime) >= (3*60))){
  
  # Create the tidal plots with operational forecasts where available
  lapply(X=1:nrow(tideStations), function(X){
    tides_plot(tideStations[X, ], p.dir = plotDir)
  })
  
}

# --------------------------------------------------------------------------------------------------------------------
# Download, parse, and format tide station information for each tide station
formatTideStations = lapply(X=1:nrow(tideStations), function(X){
  tide_data <- collectTideData(tideStations[X, ])
  tide_format <- parseTideData(tide_data)
  return(tide_format)
  })
# List to data frame
tideDF = do.call(rbind.data.frame, formatTideStations) 
tideString = paste(tideDF[,1], collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format with tideStations as the variable name.
json_merge = paste0('tideStations = {"type": "FeatureCollection","features": [', 
                    tideString, ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "NOAATideStations.js"))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
