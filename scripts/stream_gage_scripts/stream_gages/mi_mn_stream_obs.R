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
# Read in USGS stream gage metadata for Michigan
MI_ID = read.csv("stream_gages/michigan.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MI_ID$SiteNumber)){
    if(nchar(MI_ID$SiteNumber[i]) < 8){
        MI_ID$SiteNumber[i] = paste(0, MI_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MI_TMP = mat.or.vec(length(MI_ID$SiteNumber), 3)
MI_TMP[ ,1] = MI_ID$SiteNumber; MI_DIS = MI_GAG = MI_TMP
for(i in 1:length(MI_ID$SiteNumber)){
  MI_TMP[i, 2:3] = usgs_dataRetrieveTemp(MI_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MI_DIS[i, 2:3] = usgs_dataRetrieveTemp(MI_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MI_GAG[i, 2:3] = usgs_dataRetrieveTemp(MI_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MI_OBS = as.data.frame(obs_string(MI_TMP, MI_DIS, MI_GAG))
colnames(MI_OBS) = c("ID", "obs", "time")

# add other important details
MI_OBS$name = MI_ID$SiteName
MI_OBS$url = MI_ID$SiteNWISURL
MI_OBS$lon = MI_ID$SiteLongitude
MI_OBS$lat = MI_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Minnesota
MN_ID = read.csv("stream_gages/minnesota.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MN_ID$SiteNumber)){
    if(nchar(MN_ID$SiteNumber[i]) < 8){
        MN_ID$SiteNumber[i] = paste(0, MN_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MN_TMP = mat.or.vec(length(MN_ID$SiteNumber), 3)
MN_TMP[ ,1] = MN_ID$SiteNumber; MN_DIS = MN_GAG = MN_TMP
for(i in 1:length(MN_ID$SiteNumber)){
  MN_TMP[i, 2:3] = usgs_dataRetrieveTemp(MN_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MN_DIS[i, 2:3] = usgs_dataRetrieveTemp(MN_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MN_GAG[i, 2:3] = usgs_dataRetrieveTemp(MN_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MN_OBS = as.data.frame(obs_string(MN_TMP, MN_DIS, MN_GAG))
colnames(MN_OBS) = c("ID", "obs", "time")

# add other important details
MN_OBS$name = MN_ID$SiteName
MN_OBS$url = MN_ID$SiteNWISURL
MN_OBS$lon = MN_ID$SiteLongitude
MN_OBS$lat = MN_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(MI_OBS, file="rds_output/michigan_obs.rds")
saveRDS(MN_OBS, file="rds_output/minnesota_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
