# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: Oct 3, 2023: Metadata download of METAR Stations
#
# This script collects METAR station observations:
# https://www.aviationweather.gov
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
library(xml2)
library(XML)
library(compiler)
enableJIT(3)
enableJIT(3)

dataDir <- "/clima/rtdatamap/resources/"

# Read in all METAR stations and save to a csv file
stationURL <- "https://aviationweather.gov/data/cache/stations.cache.xml.gz"

download.file(stationURL,paste0(dataDir, "metar_stations.xml"))

statData <- read_xml(paste0(dataDir, "metar_stations.xml"))
statData_xml <- xmlParse(statData)
df_stations <- xmlToDataFrame(nodes = getNodeSet(statData_xml, "//Station"))
write.csv(df_stations, paste0(dataDir, "metar_stations.csv"), row.names = FALSE)
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
##########################################################################
# END
##########################################################################
