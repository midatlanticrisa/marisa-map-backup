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
ptm <- proc.time()
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)


# important file locations
if(comp$nodename=="E2-EES-RSML638.local" | comp$nodename=="E2-EES-RSML638" | comp$nodename=="rsc64dot1x-59.ems.psu.edu"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="firkin.eesi.psu.edu"){  ##firkin
  inDir <- "/firkin/s0/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/firkin/s0/mdl5548/marisaMapOutput"
}else{ ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
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
# <!-- Ohio -->
# OH_ID = c("03159540", "03159500", "03115400", "03150000", "03142000", "03144500", "03111548", "03111500", "03129000",
#           "03140500", "03140000", "03139000", "03120500", "03110000", "03109500", "03117500", "03118500", "03117000",
#           "03118000", "04206000", "03091500", "03094000", "03093000", "04202000", "04207200", "04208000", "04201500",
#           "04209000", "04212100", "04213000")
# 
# # <!-- Delaware -->
# DE_ID = c("01487000", "01488500", "01484100", "01483700", "01483200", "01478000", "01479000", "01480000", "01481500",
#           "01477800")
# 
# # <!-- Washington DC -->
# DC_ID = c("01648000", "01646500")
# 
# # <!-- West Virginia -->
# WV_ID = c("03112000",
#           "03062500", "03070500", "01595800", "01608500", "01610000", "01611500", "01614000", "01617000", "01616500",
#           "01618000", "01636500", "01606500", "01607500", "03065000", "03069500", "03050000", "03051000", "03052000",
#           "03053500", "03054500", "03056250", "03052500", "03057000", "03061500", "03114500", "03180500",
#           "03182500", "03186500", "03194700", "03151400", "03154000", "03155000", "03200500", "03198000", "03197000",
#           "03193000", "03192000", "03191500", "03185400", "03190000", "03189100", "03187500", "03183500", "03184000",
#           "03179000", "03185000", "03202400", "03202750", "03213500", "03203000", "03203600", "03198500", "03213700",
#           "03214500", "03206600")
# 
# # <!-- Tennessee -->
# TN_ID = "03532000"
# 
# # <!-- North Carolina -->
# NC_ID = c("02068500", "02070500", "02074000", "02077670", "02080500")
# 
# # <!-- Conneticut -->
# CT_ID = c("01209700", "01200500", "01200000", "01199050")
# 
# # <!-- Massachusetts -->
# MA_ID = c("01198000", "01197500")
load(paste0(idRecDir, "OHDEDCWVTNNCCTMA_streamIDs.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Run the function extracting the data we want and creating a plot.
# Run through each Ohio station.
ptmDownload <- proc.time()
for(i in 1:length(OH_ID)){ stream_gage_plot(OH_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run through each Delaware station.
for(i in 1:length(DE_ID)){ stream_gage_plot(DE_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run through each Washington DC station.
for(i in 1:length(DC_ID)){ stream_gage_plot(DC_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run through each West Virginia station.
for(i in 1:length(WV_ID)){ stream_gage_plot(WV_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run the Tennessee station.
#stream_gage_plot(TN_ID, b.date, e.date, day_midnight, day_noon)

# Run through each North Carolina station.
#for(i in 1:length(NC_ID)){ stream_gage_plot(NC_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run through each Conneticut station.
for(i in 1:length(CT_ID)){ stream_gage_plot(CT_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run through each Massachusetts station.
for(i in 1:length(MA_ID)){ stream_gage_plot(MA_ID[i], b.date, e.date, day_midnight, day_noon) }
ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))
# Run through a few other stations. These stations are sometimes difficult to extract
# data, so they are run seperately from the others
#WV_ID_NC_ID = c("03056000", "02077303")
#for(i in 1:length(WV_ID_NC_ID)){ stream_gage_plot(WV_ID_NC_ID[i], b.date, e.date, day_midnight, day_noon) }

# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "OHDEDCWVTNNCCTMAstreamPlotsTracking.RData")
if(file.exists(timeFile)==T){
  load(timeFile)
  timeOHDEDCWVTNNCCTMAStreamPlots[nrow(timeOHDEDCWVTNNCCTMAStreamPlots)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timeOHDEDCWVTNNCCTMAStreamPlots", file=timeFile)
}else{
  timeOHDEDCWVTNNCCTMAStreamPlots <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timeOHDEDCWVTNNCCTMAStreamPlots", file=timeFile)
}