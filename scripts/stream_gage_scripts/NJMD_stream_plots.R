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
# <!-- New Jersey -->
# NJ_ID = c("01411300", "01411500", "01482500", "01477120", "01475001", "01411000", "01467150", "01467081",
#           "01409400", "01410150", "01410000", "01409810", "01466500", "01467000", "01408900", "01408500", "01408120",
#           "01408000", "01407705", "01407760", "01464500", "01407500", "01463500", "01464000", "01401000", "01405400",
#           "01398000", "01397000", "01396800", "01399670", "01396660", "01396500", "01396582", "01401650", "01402000",
#           "01403060", "01400500", "01400000", "01403150", "01403540", "01379000", "01398500", "01403400", "01395000",
#           "01393450", "01394500", "01379500", "01457000", "01446500", "01445500", "01446000", "01443500", "01443900",
#           "01399500", "01381500", "01455500", "01379780", "01379773", "01380450", "01381000", "01388500", "01381900",
#           "01392500", "01389500", "01388000", "01391500", "01378500", "01377000", "01377500", "01390500", "01391000",
#           "01387500", "01387000", "01386000", "01384500", "01383500", "01382500", "01445000", "01440000", "01438500")
# 
# # <!-- Maryland -->
# MD_ID = c("01485500", "01486000", "01485000", "01490000", "01491000", "01493000", "01493500", "01495000",
#           "01580000", "01581500", "01581700", "01584500", "01582000", "01582500", "01583500", "01583600", "01584050",
#           "01585100", "01585200", "01589440", "01589300", "01589330", "01589000", "01589100", "01589500", "01594440",
#           "01661500", "01661050", "01660920", "01658000", "01653600", "01649500", "01651000", "01650500", "01645000",
#           "01591700", "01591000", "01591610", "01591400", "01593500", "01594000", "01638500", "01643500", "01643000",
#           "01637500", "01586610", "01586000", "01586210", "01585500", "01639500", "01639000", "01619500", "01617800",
#           "01614500", "01613000", "01609000", "01603000", "01601500", "01599000", "01598500", "01597500", "01596500",
#           "03078000", "03076600", "03076500", "01595500", "01595000", "03075500")
load(paste0(idRecDir, "NJMD_streamIDs.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Run the function extracting the data we want and creating a plot.
# Run through each New Jersey station.
ptmDownload <- proc.time()
for(i in 1:length(NJ_ID)){ stream_gage_plot(NJ_ID[i], b.date, e.date, day_midnight, day_noon) }

# Run through each Maryland station.
for(i in 1:length(MD_ID)){ stream_gage_plot(MD_ID[i], b.date, e.date, day_midnight, day_noon) }
ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))


##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "NJMDstreamPlotsTracking.RData")
if(file.exists(timeFile)==T){
  load(timeFile)
  timeNJMDStreamPlots[nrow(timeNJMDStreamPlots)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timeNJMDStreamPlots", file=timeFile)
}else{
  timeNJMDStreamPlots <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timeNJMDStreamPlots", file=timeFile)
}