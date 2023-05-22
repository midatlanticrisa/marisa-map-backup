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
# Read in USGS stream gage metadata for Nevada
NV_ID = read.csv("stream_gages/nevada.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(NV_ID$SiteNumber)){
    if(nchar(NV_ID$SiteNumber[i]) < 8){
        NV_ID$SiteNumber[i] = paste(0, NV_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
NV_TMP = mat.or.vec(length(NV_ID$SiteNumber), 3)
NV_TMP[ ,1] = NV_ID$SiteNumber; NV_DIS = NV_GAG = NV_TMP
for(i in 1:length(NV_ID$SiteNumber)){
  NV_TMP[i, 2:3] = usgs_dataRetrieveTemp(NV_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  NV_DIS[i, 2:3] = usgs_dataRetrieveTemp(NV_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  NV_GAG[i, 2:3] = usgs_dataRetrieveTemp(NV_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
NV_OBS = as.data.frame(obs_string(NV_TMP, NV_DIS, NV_GAG))
colnames(NV_OBS) = c("ID", "obs", "time")

# add other important details
NV_OBS$name = NV_ID$SiteName
NV_OBS$url = NV_ID$SiteNWISURL
NV_OBS$lon = NV_ID$SiteLongitude
NV_OBS$lat = NV_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for New Jersey
NJ_ID = read.csv("stream_gages/newjersey.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(NJ_ID$SiteNumber)){
    if(nchar(NJ_ID$SiteNumber[i]) < 8){
        NJ_ID$SiteNumber[i] = paste(0, NJ_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
NJ_TMP = mat.or.vec(length(NJ_ID$SiteNumber), 3)
NJ_TMP[ ,1] = NJ_ID$SiteNumber; NJ_DIS = NJ_GAG = NJ_TMP
for(i in 1:length(NJ_ID$SiteNumber)){
  NJ_TMP[i, 2:3] = usgs_dataRetrieveTemp(NJ_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  NJ_DIS[i, 2:3] = usgs_dataRetrieveTemp(NJ_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  NJ_GAG[i, 2:3] = usgs_dataRetrieveTemp(NJ_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
NJ_OBS = as.data.frame(obs_string(NJ_TMP, NJ_DIS, NJ_GAG))
colnames(NJ_OBS) = c("ID", "obs", "time")

# add other important details
NJ_OBS$name = NJ_ID$SiteName
NJ_OBS$url = NJ_ID$SiteNWISURL
NJ_OBS$lon = NJ_ID$SiteLongitude
NJ_OBS$lat = NJ_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(NV_OBS, file="rds_output/nevada_obs.rds")
saveRDS(NJ_OBS, file="rds_output/newjersey_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
