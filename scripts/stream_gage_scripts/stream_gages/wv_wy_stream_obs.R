# --------------------------------------------------------------------------------------------------------------------
# Copyright 2018 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Feb 15, 2019
# Edit: June 14, 2018
# Edit: June 7, 2018
#
# This script parses stream gage data from the USGS.
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
# OUT OF OR IN CONNECTION WITH THE SOFTWIRE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWIRE.
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
# Read in USGS stream gage metadata for West Virginia
WV_ID = read.csv("stream_gages/westvirginia.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(WV_ID$SiteNumber)){
    if(nchar(WV_ID$SiteNumber[i]) < 8){
        WV_ID$SiteNumber[i] = paste(0, WV_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
WV_TMP = mat.or.vec(length(WV_ID$SiteNumber), 3)
WV_TMP[ ,1] = WV_ID$SiteNumber; WV_DIS = WV_GAG = WV_TMP
for(i in 1:length(WV_ID$SiteNumber)){
  WV_TMP[i, 2:3] = usgs_dataRetrieveTemp(WV_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  WV_DIS[i, 2:3] = usgs_dataRetrieveTemp(WV_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  WV_GAG[i, 2:3] = usgs_dataRetrieveTemp(WV_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
WV_OBS = as.data.frame(obs_string(WV_TMP, WV_DIS, WV_GAG))
colnames(WV_OBS) = c("ID", "obs", "time")

# add other important details
WV_OBS$name = WV_ID$SiteName
WV_OBS$url = WV_ID$SiteNWISURL
WV_OBS$lon = WV_ID$SiteLongitude
WV_OBS$lat = WV_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Wyoming
WY_ID = read.csv("stream_gages/wyoming.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(WY_ID$SiteNumber)){
    if(nchar(WY_ID$SiteNumber[i]) < 8){
        WY_ID$SiteNumber[i] = paste(0, WY_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
WY_TMP = mat.or.vec(length(WY_ID$SiteNumber), 3)
WY_TMP[ ,1] = WY_ID$SiteNumber; WY_DIS = WY_GAG = WY_TMP
for(i in 1:length(WY_ID$SiteNumber)){
  WY_TMP[i, 2:3] = usgs_dataRetrieveTemp(WY_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  WY_DIS[i, 2:3] = usgs_dataRetrieveTemp(WY_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  WY_GAG[i, 2:3] = usgs_dataRetrieveTemp(WY_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
WY_OBS = as.data.frame(obs_string(WY_TMP, WY_DIS, WY_GAG))
colnames(WY_OBS) = c("ID", "obs", "time")

# add other important details
WY_OBS$name = WY_ID$SiteName
WY_OBS$url = WY_ID$SiteNWISURL
WY_OBS$lon = WY_ID$SiteLongitude
WY_OBS$lat = WY_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(WV_OBS, file="rds_output/westvirginia_obs.rds")
saveRDS(WY_OBS, file="rds_output/wyoming_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
