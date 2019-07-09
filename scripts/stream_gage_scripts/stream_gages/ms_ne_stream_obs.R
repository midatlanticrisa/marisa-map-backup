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
# Read in USGS stream gage metadata for Mississippi
MS_ID = read.csv("stream_gages/mississippi.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MS_ID$SiteNumber)){
    if(nchar(MS_ID$SiteNumber[i]) < 8){
        MS_ID$SiteNumber[i] = paste(0, MS_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MS_TMP = mat.or.vec(length(MS_ID$SiteNumber), 3)
MS_TMP[ ,1] = MS_ID$SiteNumber; MS_DIS = MS_GAG = MS_TMP
for(i in 1:length(MS_ID$SiteNumber)){
  MS_TMP[i, 2:3] = usgs_dataRetrieveTemp(MS_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MS_DIS[i, 2:3] = usgs_dataRetrieveTemp(MS_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MS_GAG[i, 2:3] = usgs_dataRetrieveTemp(MS_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MS_OBS = as.data.frame(obs_string(MS_TMP, MS_DIS, MS_GAG))
colnames(MS_OBS) = c("ID", "obs", "time")

# add other important details
MS_OBS$name = MS_ID$SiteName
MS_OBS$url = MS_ID$SiteNWISURL
MS_OBS$lon = MS_ID$SiteLongitude
MS_OBS$lat = MS_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Nebraska
NE_ID = read.csv("stream_gages/nebraska.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(NE_ID$SiteNumber)){
    if(nchar(NE_ID$SiteNumber[i]) < 8){
        NE_ID$SiteNumber[i] = paste(0, NE_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
NE_TMP = mat.or.vec(length(NE_ID$SiteNumber), 3)
NE_TMP[ ,1] = NE_ID$SiteNumber; NE_DIS = NE_GAG = NE_TMP
for(i in 1:length(NE_ID$SiteNumber)){
  NE_TMP[i, 2:3] = usgs_dataRetrieveTemp(NE_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  NE_DIS[i, 2:3] = usgs_dataRetrieveTemp(NE_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  NE_GAG[i, 2:3] = usgs_dataRetrieveTemp(NE_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
NE_OBS = as.data.frame(obs_string(NE_TMP, NE_DIS, NE_GAG))
colnames(NE_OBS) = c("ID", "obs", "time")

# add other important details
NE_OBS$name = NE_ID$SiteName
NE_OBS$url = NE_ID$SiteNWISURL
NE_OBS$lon = NE_ID$SiteLongitude
NE_OBS$lat = NE_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(MS_OBS, file="rds_output/mississippi_obs.rds")
saveRDS(NE_OBS, file="rds_output/nebraska_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
