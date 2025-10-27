# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Latest edit: May 4, 2023: Complete revamp of data download and buoy functions
# Previous edit: Feb. 5, 2019: Extend country-wide
# Previous edit: June 13, 2018
# Previous edit: April 27, 2017
#
# This script parses txt meteorological data from National Data Buoy Center buoy 
# stations and outputs the results in a single file.
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

ptm <- proc.time()
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("measurements")) { install.packages("measurements") }

library(RCurl)
library(measurements)
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

inDir <- "/clima/rtdatamap/scripts/"
outDir <- "/var/www/html/rtdatamap/"

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Download NDBC buoys and keep only those in the MARISA region
# bbox: c(Min LON, Max LON, Min LAT, Max LAT)
# bbox = c(-82.0, -73.0, 36.46, 43.75)
bbox = c(-84.95,-71.24,36.42,45.15) # Includes all of NY, NJ, WV, and OH
buoy_data = collectBuoyData(bbox)

# Parse and format buoy information for each buoy downloaded
formatBuoys = lapply(X=1:nrow(buoy_data), function(X){parseBuoyData(buoy_data[X,])})
buoyDF = do.call(rbind.data.frame, formatBuoys) # list to data frame
buoyString = paste(buoyDF[,1], collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format with Buoys as the variable name.
json_merge = paste0('Buoys = {"type": "FeatureCollection","features": [', 
                    buoyString, ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "NDBCbuoys.js"))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
