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
# Install and read packages
# install.packages("RCurl")
# install.packages("readr")
library(RCurl)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)

# Read function to retrieve data
source("../usgs_dataRetrieve.R")
source("../obs_string_single.R")
# --------------------------------------------------------------------------------------------------------------------
# We only need todays date to get the most recent value.
e.date = Sys.Date()      # End date
b.date = Sys.Date()-1      # Beginning date
# --------------------------------------------------------------------------------------------------------------------
# Read in USGS stream gage metadata for Washington
WA_ID = read.csv("stream_gages/washington.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(WA_ID$SiteNumber)){
    if(nchar(WA_ID$SiteNumber[i]) < 8){
        WA_ID$SiteNumber[i] = paste(0, WA_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
WA_TMP = mat.or.vec(length(WA_ID$SiteNumber), 3)
WA_TMP[ ,1] = WA_ID$SiteNumber; WA_DIS = WA_GAG = WA_TMP
for(i in 1:length(WA_ID$SiteNumber)){
  WA_TMP[i, 2:3] = usgs_dataRetrieveTemp(WA_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  WA_DIS[i, 2:3] = usgs_dataRetrieveTemp(WA_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  WA_GAG[i, 2:3] = usgs_dataRetrieveTemp(WA_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
WA_OBS = as.data.frame(obs_string(WA_TMP, WA_DIS, WA_GAG))
colnames(WA_OBS) = c("ID", "obs", "time")

# add other important details
WA_OBS$name = WA_ID$SiteName
WA_OBS$url = WA_ID$SiteNWISURL
WA_OBS$lon = WA_ID$SiteLongitude
WA_OBS$lat = WA_ID$SiteLatitude

# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(WA_OBS, file="rds_output/washington_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
