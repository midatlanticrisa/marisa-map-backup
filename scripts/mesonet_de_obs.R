# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: Oct 3, 2023: Data download of DE Mesonet Stations
#
# This script collects DE Mesonet station observations:
# http://www.deos.udel.edu
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
outDir <- "/var/www/html/rtdatamap/weather/"

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Create URLs for each network
# 5 mins update
deosURL = "http://services.deos.udel.edu/ws/REST/V1/getStationList?key=a437d9133a41f47555e5d60c4f7e4154&network=DEOS"
# 10 mins update
deldotURL = "http://services.deos.udel.edu/ws/REST/V1/getStationList?key=a437d9133a41f47555e5d60c4f7e4154&network=DelDOT"

# Read the xml file
deosData = read_xml(deosURL)
deldotData = read_xml(deldotURL)

# Parse into an R structure representing XML tree
deosxml <- xmlParse(deosData)
deldotxml <- xmlParse(deldotData)

# Convert the parsed XML to a dataframe
deos_df <- xmlToDataFrame(nodes=getNodeSet(deosxml, "//deos:station"))
deldot_df <- xmlToDataFrame(nodes=getNodeSet(deldotxml, "//deos:station"))

# Download station observations
deosObs = lapply(deos_df$id, collectDEMesonetData, network="DEOS")
names(deosObs) = paste(deos_df$city, deos_df$state, sep=", ")

deldotObs = lapply(deldot_df$id, collectDEMesonetData, network="DelDOT")
names(deldotObs) = paste(deldot_df$city, deldot_df$state, sep=", ")

# --------------------------------------------------------------------------------------------------------------------
# Parse and format mesonet information for each mesonet downloaded
formatDOTMesonet = lapply(X=1:length(deldotObs), function(X){
  parseDEMesonetData(deldotObs[[X]], names(deldotObs)[X])})

formatDEOSMesonet = lapply(X=1:length(deosObs), function(X){
  parseDEMesonetData(deosObs[[X]], names(deosObs)[X])})

# List to data frame
MesonetDF = do.call(rbind.data.frame, c(formatDOTMesonet, formatDEOSMesonet))
MesonetDF = MesonetDF[!is.na(MesonetDF)] # Remove NA stations
MesonetString = paste(MesonetDF, collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format
json_mesonet = paste0('DEMesonet = {"type": "FeatureCollection","features": [', 
                      MesonetString, ']};')

# Export data to geojson.
cat(json_mesonet, file=paste0(outDir, "DEMesonet.js"))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
