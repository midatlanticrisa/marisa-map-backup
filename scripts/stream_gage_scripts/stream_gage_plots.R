# --------------------------------------------------------------------------------------------------------------------
# Copyright 2018 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 14, 2018
# Last edit: June 7, 2018
#
# This script parses RSS meteorological data from buoy stations from the
# National Data Buoy Center and outputs the results in a single file.
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
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)

ptm <- proc.time()

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else{  ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}
plotDir <- paste0(outDir, "Stream_figs/")


# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!dir.exists(plotDir)){
  dir.create(plotDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width   = 4            # Width
p.height = 2.5           # Height

# We only need todays date to get the most recent value.
eDate <- Sys.Date()      # End date
bDate <- Sys.Date()-7       # Beginning date

cores <- 1

##read in the .csvs associated with each state, to be able to more efficiantly format the output geojson file
stateGageFiles <- list.files(paste0(inDir, "stream_gage_scripts/stream_gages_csv/"), pattern=".csv", full.names=T)
##subset files unitl ready to map to full country
#subStates <- c("pennsylvania", "delaware")
subStates <- c("pennsylvania", "Delaware", "maryland", "DC", "newyork", "newjersey", "/virginia", "westvirginia", "ohio", "conneticut", "massachusetts")
gageCSVs <- lapply(stateGageFiles[sapply(subStates, grep, x=stateGageFiles)], read.csv)
gageRecs <- do.call(rbind.data.frame, gageCSVs)
gageRecs <- gageRecs[gageRecs$SiteLongitude>=-82.0 & gageRecs$SiteLongitude<=-73.0 & gageRecs$SiteLatitude>=36.0 & gageRecs$SiteLatitude<=43.5,]

# Site numbers should be between 8 and 15 digits long
shortIDs <- which(nchar(gageRecs$SiteNumber)<8)
gageRecs$SiteNumber[shortIDs] <- paste0("0", gageRecs$SiteNumber[shortIDs])

stationIDs <- gageRecs$SiteNumber

# Determine midnight and noon for dates of this previous week
day <- 0:7
day_midnight <- as.POSIXct(paste(Sys.Date() - day, "00:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
day_noon <- as.POSIXct(paste(Sys.Date() - day, "12:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")

# --------------------------------------------------------------------------------------------------------------------

gageDisURLs <- paste0('https://waterdata.usgs.gov/nwis/uv?cb_00060=on&format=rdb&site_no=', stationIDs, '&period=&begin_date=', bDate, '&end_date=', eDate)
gageGagURLs <- paste0('https://waterdata.usgs.gov/nwis/uv?cb_00065=on&format=rdb&site_no=', stationIDs, '&period=&begin_date=', bDate, '&end_date=', eDate)
#grep("01362336", gageDisURLs)

if(cores>1){
  mcmapply(stream_gage_plot, gageDisURLs, gageGagURLs, MoreArgs=list(weekMidnights=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir), mc.cores=cores)
}else{
  mapply(stream_gage_plot, gageDisURLs, gageGagURLs, MoreArgs=list(weekMidnights=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir))
}

#stream_gage_plot(gageDisURLs[1838], gageGagURLs[1838], weekMidnights=day_midnight, weekNoons=day_noon, plotW=p.width, plotH=p.height, plotOut=plotDir)

ptmEnd <- proc.time() - ptm
stop(paste0("Total Runtime: ", ptmEnd))