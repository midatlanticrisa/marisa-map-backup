# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 13, 2018; set as seperate script; chance avg. discharge to 1 value and fix midnight/noon axis
# Previous edit: May 29, 2018; implement techniques from dataRetrieval package to improve data extraction; add gage height and avg. discharge
# Previous edit: June 17, 2017; Created
#
# This script parses discharge and gage height data of stream gauges from the
# United States Geological Survey (USGS) and plots the results.
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
options(stringsAsFactors=F)
ptm <- proc.time()
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)


# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local" | comp$nodename=="E2-EES-RSML638" | comp$nodename=="rsc64dot1x-59.ems.psu.edu"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/stream_gage_scripts/"
  idRecDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/stream_gage_scripts/"
  idRecDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="firkin.eesi.psu.edu"){  ##firkin
  inDir <- "/firkin/s0/mdl5548/githubRepos/marisa-map-backup/scripts/stream_gage_scripts/"
  idRecDir <- "/firkin/s0/mdl5548/marisaMapOutput/"
}else{ ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/stream_gage_scripts/"
  idRecDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}

# Read functions to retrieve data and plot
source(paste0(inDir,"usgs_dataRetrieve.R"))
source(paste0(inDir,"stream_gage_plot_func.R"))
# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width   = 4            # Width
p.height = 2.5           # Height

# Extract the dates of this previous week for use in the url.
e.date = Sys.Date()      # End date
b.date = Sys.Date() - 7  # Beginning date

# Determine midnight and noon for dates of this previous week
day = 0:7
day_midnight = as.POSIXct(paste(Sys.Date() - day, "00:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
day_noon = as.POSIXct(paste(Sys.Date() - day, "12:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
# --------------------------------------------------------------------------------------------------------------------
# Create vector including each station ID.
# <!-- USGS STATIONS -->
# <!-- New York -->
# NY_ID = c("03014500", "04213500", "03011020", "04215500", "04214500", "04215000", "04218518", "04218000", "04221000",
#           "04223000", "04230380", "04216418", "04217000", "04231000", "04230500", "04231600", "04232050", "04229500",
#           "04228500", "04227500", "04224775", "01524500", "01529500", "01526500", "01529950", "04232482", "04235000",
#           "04235250", "01531000", "01515000", "04234000", "04249000", "04239000", "04240010", "04240300", "04240120",
#           "04240105", "01509000", "01510000", "01513500", "01512500", "01503000", "01426500", "01425000", "01507000",
#           "01502500", "01505000", "04243500", "04242500", "01336000", "04258000", "04250750", "04260500", "04262500",
#           "04262000", "04267500", "04268000", "04266500", "04273500", "01312000", "01315000", "01321000", "01315500",
#           "01334500", "01329490", "01327750", "01325000", "01330000", "01335754", "01357500", "01358000", "01351500",
#           "01348000", "01346000", "01500500", "01500000", "0142400103", "01423000", "01417500", "01421000", "01420500",
#           "01417000", "01421900", "01422500", "01415000", "01414500", "01414000", "01413500", "01350000", "01350140",
#           "01350180", "01350355", "01361000", "01372500", "01364500", "01367500", "01371500", "01362500", "01362200",
#           "01434025", "01365000", "01365500", "01435000", "01436000", "01427510", "01428500", "01433500", "01434000",
#           "01437500", "01387420", "01387450", "01376800", "01375000", "01311500", "01309500", "01303500", "01308500",
#           "01304000", "01306460", "01304500")
load(paste0(idRecDir, "NY_streamIDs.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Run the function extracting the data we want and creating a plot.
# Run through each New York station.
ptmDownload <- proc.time()
for(i in 1:length(NY_ID)){ stream_gage_plot(NY_ID[i], b.date, e.date, day_midnight, day_noon) }
ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "NYstreamPlotsTracking.RData")
if(file.exists(timeFile)==T){
  load(timeFile)
  timeNYStreamPlots[nrow(timeNYStreamPlots)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timeNYStreamPlots", file=timeFile)
}else{
  timeNYStreamPlots <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timeNYStreamPlots", file=timeFile)
}