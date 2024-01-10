# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: Oct 24, 2023: Data download of WeatherStem station observations
#
# This script collects WeatherStem station observations:
# https://www.weatherstem.com/dashboard?public_access_token=e75cd9ae5f91981fbdab9e7abbde8866
# https://www.weatherstem.com/api_docs
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
library(jsonlite)
library(compiler)
enableJIT(3)
enableJIT(3)

inDir <- "/clima/rtdatamap/scripts/"
outDir <- "/var/www/html/rtdatamap/weather/"
dataDir <- "/clima/rtdatamap/resources/"

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# Read in WeatherStem station IDs
stationIDs = read.csv(paste0(dataDir, "weatherStemStations.csv"))

# --------------------------------------------------------------------------------------------------------------------
# Download, parse, and format weather stem information
formatWeatherstem = lapply(X = 1:nrow(stationIDs), function(X){
  parseWeatherStem(stationIDs[X, ])
})
WeatherDF = do.call(rbind.data.frame, formatWeatherstem) # list to data frame
WeatherString = paste(WeatherDF[,1], collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format with Buoys as the variable name.
json_merge = paste0('WeatherStem = {"type": "FeatureCollection","features": [', 
                    WeatherString, ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "WeatherStem.js"))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
print(ptmEnd)
##########################################################################
# END
##########################################################################
