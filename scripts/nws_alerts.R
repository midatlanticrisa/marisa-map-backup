# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: May 16, 2023: Complete revamp of data download of NWS alerts
#
# This script collect warnings, watches, and advisories from the NOAA NWS API:
# https://www.weather.gov/documentation/services-web-api#/default/alerts_types
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
if (!require("jsonlite")) { install.packages("jsonlite") }
if (!require("terra")) { install.packages("terra") }
if (!require("sf")) { install.packages("sf") }
if (!require("geojsonsf")) { install.packages("geojsonsf") }

library(jsonlite)
library(terra)
library(sf)
library(geojsonsf)
library(compiler)
enableJIT(3)
enableJIT(3)

inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
dataDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/resources/"
outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists(outDir)){
  dir.create(outDir, recursive=T)
}

# Source functions
source(paste0(inDir, "download_parse_functions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Download only the NWS alerts for the MARISA region exporting the information
# into a geojson file.
collectWarningsAlerts(area=c("VA", "PA", "MD", "DE", "DC", "WV", "OH"), 
                      colorfile = paste0(dataDir, "WeatherEventColors.csv"),
                      cntyShp = paste0(dataDir, "cb_2018_us_county_500k/cb_2018_us_county_500k.shp"),
                      coastalShp = paste0(dataDir, "mz08mr23/mz08mr23.shp"),
                      offshoreShp = paste0(dataDir, "oz08mr23/oz08mr23.shp"),
                      outfile = paste0(outDir, "NWSalerts.geojson"))

# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
