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
#--------------------------------------------------------------------------------------------------------------------
# Ensure necessary packages are installed and loaded
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
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="firkin.eesi.psu.edu"){  ##firkin
  inDir <- "/firkin/s0/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/firkin/s0/mdl5548/marisaMapOutput/"
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
# <!-- Pennsylvania -->
#PA_ID = c("04213075", "03021350", "03102500", "03015500", "03015000", "03011800",
#          "03010500", "03007800", "03010655", "01544500", "01518862", "01520000", "01518000", "01518700", "01516350",
#          "01516500", "01532000", "01531500", "01534300", "01428750", "01429000", "01429500", "01430000", "01431500",
#          "01534500", "01536000", "01534000", "01533400", "01552500", "01552000", "01551500", "01550000", "01549500",
#          "01549700", "01548500", "01548005", "01547700", "01547500", "01547950", "01545600", "01545500", "01545000",
#          "01544000", "01543500", "01543000", "01542810", "03026500", "03027500", "03028500", "03028000", "03016000",
#          "03020000", "03020500", "03025500", "03024000", "03101500", "03102850", "03103500", "03105500",
#          "03106500",
#          "03106300", "03049000", "03031500", "03030500", "03029500", "03034000", "01541000", "01541200",
#          "01541303", "01541500", "01542500", "01546400", "01546500", "01547100", "01547200", "01555000", "01553500",
#          "01554000", "01553700", "01540500", "01539000", "01538000", "01536500", "01447800", "01449000", "01447500",
#          "01449800", "01450500", "01447720", "01447680", "01449360", "01440400", "01442500", "01439500", "01454700",
#          "01453000", "01452500", "01451000", "01451650", "01451500", "01452000", "01451800", "01471510", "01471000",
#          "01470960", "01470779", "01470500", "01468500", "01469500", "01573000", "01573560", "01555500", "01570500",
#          "01567000", "01568000", "01567500", "01566000", "01565000", "01564500", "01563500", "01563200", "01559000",
#          "01558000", "01557500", "01556000", "03040000", "03041500", "03042280", "03034500", "03042500",
#          "03042000", "03038000", "03036000", "03036500", "03039000", "03048500", "03106000", "03107500", "03041000",
#          "03108000", "03086000", "03085500", "03049800", "03075070", "03083500", "03072655", "03072000", "03074500",
#          "03082500", "03049500", "03047000", "03044000", "03045000", "03081000", "03077500", "03080000", "03079000",
#          "01601000", "01560000", "01562000", "01613050", "01570000", "01571500", "01574500", "01575500", "01574000",
#          "01576000", "01576754", "01576500", "01480300", "01480500", "01480617", "01480675", "01480685", "01480700",
#          "01480870", "01481000", "01472000", "01472157", "01473169", "01475850", "01477000", "01472199", "01472198",
#          "01473000", "01464645", "01459500", "01474500", "01474000", "01467087", "01467086", "01467048", "01465798")
load(paste0(idRecDir, "PA_streamIDs.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Run the function extracting the data we want and creating a plot.
# Run through each Pennsylvania station.
ptmDownload <- proc.time()
for(i in 1:length(PA_ID)){ stream_gage_plot(PA_ID[i], b.date, e.date, day_midnight, day_noon) }
ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))
# --------------------------------------------------------------------------------------------------------------------
ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "PAstreamPlotsTracking.RData")
if(file.exists(timeFile)==T){
  load(timeFile)
  timePAStreamPlots[nrow(timePAStreamPlots)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timePAStreamPlots", file=timeFile)
}else{
  timePAStreamPlots <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timePAStreamPlots", file=timeFile)
}