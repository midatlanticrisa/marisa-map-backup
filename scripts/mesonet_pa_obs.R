# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: Oct 4, 2023: Data download of Mesonet station observations
#
# This script collects PA Mesonet station observations that update every 10, 15,
# and 60 mins:
# https://keystone-mesonet.org/
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
outDir <- "/var/www/html/rtdatamap/weather/"

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# Read in Mesonet data that updates every 10 minutes
mesonetURL = "https://pema.ems.psu.edu/geoserver/pema/ows?service=WFS&version=1.0.0&request=GetFeature&outputFormat=CSV&typeName="
pennDot = read.csv(paste0(mesonetURL, "pema:rwis"))
paTurnpike = read.csv(paste0(mesonetURL, "pema:tp2020"))
pemn = read.csv(paste0(mesonetURL, "pema:pemn"))
# 15 mins
depCopams = read.csv(paste0(mesonetURL, "pema:dep"))
# 60 mins
dcnr_stations = read.csv(paste0(mesonetURL, "pema:dcnr"))

# Make some format changes and converstions for consistency between the networks
pennDot = updatePaMesonetColnames(pennDot)
paTurnpike = updatePaMesonetColnames(paTurnpike)
pemn = updatePaMesonetColnames(pemn)
depCopams = updatePaMesonetColnames(depCopams)
dcnr = updatePaMesonetColnames(dcnr_stations)

pennDot$name = paste0("PennDot: ", pennDot$name)
paTurnpike$name = paste0("PA Turnpike: milepost ", paTurnpike$milepost,  "; ", paTurnpike$description)
depCopams$name = ifelse(depCopams$long_name != "", as.character(depCopams$long_name), 
                        as.character(depCopams$name))

# Combine the networks into one data.frame
dfs = list(pennDot, paTurnpike, pemn, depCopams, dcnr)
# get all variable names
allNms <- unique(unlist(lapply(dfs, names)))

# put em all together
combinedMesonet <- do.call(rbind, lapply(dfs,
            function(x) data.frame(c(x, sapply(setdiff(allNms, names(x)),
                                                  function(y) NA)))))

# --------------------------------------------------------------------------------------------------------------------
# Parse and format mesonet information for each mesonet downloaded
formatMesonet = lapply(X=1:nrow(combinedMesonet), function(X){
  parsePaMesonetData(combinedMesonet[X,])})
MesonetDF = do.call(rbind.data.frame, formatMesonet) # list to data frame
MesonetString = paste(MesonetDF[,1], collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format with Buoys as the variable name.
json_merge = paste0('PAMesonet = {"type": "FeatureCollection","features": [', 
                    MesonetString, ']};')

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "PAMesonet.js"))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
