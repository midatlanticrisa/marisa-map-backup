# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Jan 25, 2019    - expanded to entire US
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
library(pbapply)
library(anytime)

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
plotDir <- paste0(outDir, "Tide_figs/")

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir, recursive=T)
}
if (!dir.exists(plotDir)){
  dir.create(plotDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

##load station ids
#load(paste0(inDir, "tideStationIDs.RData"))
load(paste0(inDir, "tideStationIDs_regional.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width <- 4            # Width
p.height <- 2.5           # Height

# Extract the dates of this previous week for use in the url.
endDate <- format(Sys.Date(), "%Y%m%d")       # End date
originDate <- format(Sys.Date() - 2, "%Y%m%d")  # Beginning date
datum <- "MLLW"
gl.datum <- "IGLD"
msl.datum <- "MSL"
timezone <- "GMT"
units <- "metric"
cores <- 1

# Determine midnight and noon for dates of this previous week
day <- 0:7
day_midnight <- as.POSIXct(paste0(Sys.Date() - day, "00:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
day_noon <- as.POSIXct(paste0(Sys.Date() - day, "12:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")

# --------------------------------------------------------------------------------------------------------------------

# Use the ID, b.date, e.date, datum, timezone, and units to create a URL to the XML file.
tideURLs <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=', originDate, '&end_date=', endDate, 
                    '&datum=', datum, '&station=', tideIDs, '&time_zone=', timezone, '&units=', units, '&format=xml')
tideMSLURLs <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=', originDate, '&end_date=', endDate, 
                   '&datum=', msl.datum, '&station=', tideIDsMSL, '&time_zone=', timezone, '&units=', units, '&format=xml')
tideGrtLakesURLs <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=', originDate, '&end_date=', endDate, 
                   '&datum=', gl.datum, '&station=', tideIDsGrtLakes, '&time_zone=', timezone, '&units=', units, '&format=xml')

# Run the function extracting the data we want and creating a plot.
if(cores>1){
  library(parallel)
  mclapply(tideURLs, waterheight_plot, weekMidnight=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir, mc.cores=cores)
  mclapply(tideMSLURLs, waterheight_plot, weekMidnight=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir, mc.cores=cores)
  mclapply(tideGrtLakesURLs, waterheight_plot, weekMidnight=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir, mc.cores=cores)
}else{
  pbsapply(tideIDs, waterheight_plot, weekMidnight=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir)
  pbsapply(tideIDsMSL, waterheight_plot, weekMidnight=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir)
  pbsapply(tideIDsGrtLakes, waterheight_plot, weekMidnight=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir)
}

# --------------------------------------------------------------------------------------------------------------------


