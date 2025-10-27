# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: Oct 3, 2023: Data download of METAR Stations
#
# This script collects METAR station observations:
# https://www.aviationweather.gov/dataserver
# https://www.aviationweather.gov/metar
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
library(measurements)
library(compiler)
enableJIT(3)
enableJIT(3)

inDir <- "/clima/rtdatamap/scripts/"
dataDir <- "/clima/rtdatamap/resources/"
outDir <- "/var/www/html/rtdatamap/weather/"

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# Read in all METAR station metadata
metar_stations = read.csv(paste0(dataDir, "metar_stations.csv"))

# Read in all METAR stations
awsURL <- "https://aviationweather.gov/data/cache/metars.cache.csv.gz"

temp <- tempfile()
download.file(awsURL,temp)
# awsData <- read.csv(gzfile(temp, "metars.cache.csv"), skip=5)
awsData <- read.csv(gzfile(temp, "metars.cache.csv"))
unlink(temp)

# --------------------------------------------------------------------------------------------------------------------
# Subset NOAA METAR stations to only those in the MARISA region
# bbox: c(Min LON, Max LON, Min LAT, Max LAT)
# bbox = c(-82.0, -73.0, 36.46, 43.75)
bbox = c(-84.95,-71.24,36.42,45.15) # Includes all of NY, NJ, WV, and OH
awsData <- awsData[awsData$longitude >= bbox[1] & 
                     awsData$longitude <= bbox[2] & 
                     awsData$latitude >= bbox[3] & 
                     awsData$latitude <= bbox[4], ]

# Remove any stations with no data
if(any(is.na(awsData$raw_text))){
  awsData = awsData[!is.na(awsData$raw_text), ]
}

# Parse and format METAR information for each station downloaded
formatMetar = lapply(X=1:nrow(awsData), function(X){parseMETARdata(awsData[X,], 
                                                                   metar_stations)})
MetarDF = do.call(rbind.data.frame, formatMetar) # list to data frame
MetarString = paste(MetarDF[,1], collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format
json_merge = paste0('Metar = {"type": "FeatureCollection","features": [', 
                    MetarString, ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "NoaaMetar.js"))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
