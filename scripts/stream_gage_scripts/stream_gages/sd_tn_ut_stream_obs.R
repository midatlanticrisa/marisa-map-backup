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
# Read in USGS stream gage metadata for South Dakota
SD_ID = read.csv("stream_gages/southdakota.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(SD_ID$SiteNumber)){
    if(nchar(SD_ID$SiteNumber[i]) < 8){
        SD_ID$SiteNumber[i] = paste(0, SD_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
SD_TMP = mat.or.vec(length(SD_ID$SiteNumber), 3)
SD_TMP[ ,1] = SD_ID$SiteNumber; SD_DIS = SD_GAG = SD_TMP
for(i in 1:length(SD_ID$SiteNumber)){
  SD_TMP[i, 2:3] = usgs_dataRetrieveTemp(SD_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  SD_DIS[i, 2:3] = usgs_dataRetrieveTemp(SD_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  SD_GAG[i, 2:3] = usgs_dataRetrieveTemp(SD_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
SD_OBS = as.data.frame(obs_string(SD_TMP, SD_DIS, SD_GAG))
colnames(SD_OBS) = c("ID", "obs", "time")

# add other important details
SD_OBS$name = SD_ID$SiteName
SD_OBS$url = SD_ID$SiteNWISURL
SD_OBS$lon = SD_ID$SiteLongitude
SD_OBS$lat = SD_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Tennessee
TN_ID = read.csv("stream_gages/tennessee.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(TN_ID$SiteNumber)){
    if(nchar(TN_ID$SiteNumber[i]) < 8){
        TN_ID$SiteNumber[i] = paste(0, TN_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
TN_TMP = mat.or.vec(length(TN_ID$SiteNumber), 3)
TN_TMP[ ,1] = TN_ID$SiteNumber; TN_DIS = TN_GAG = TN_TMP
for(i in 1:length(TN_ID$SiteNumber)){
  TN_TMP[i, 2:3] = usgs_dataRetrieveTemp(TN_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  TN_DIS[i, 2:3] = usgs_dataRetrieveTemp(TN_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  TN_GAG[i, 2:3] = usgs_dataRetrieveTemp(TN_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
TN_OBS = as.data.frame(obs_string(TN_TMP, TN_DIS, TN_GAG))
colnames(TN_OBS) = c("ID", "obs", "time")

# add other important details
TN_OBS$name = TN_ID$SiteName
TN_OBS$url = TN_ID$SiteNWISURL
TN_OBS$lon = TN_ID$SiteLongitude
TN_OBS$lat = TN_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Utah
UT_ID = read.csv("stream_gages/utah.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(UT_ID$SiteNumber)){
    if(nchar(UT_ID$SiteNumber[i]) < 8){
        UT_ID$SiteNumber[i] = paste(0, UT_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
UT_TMP = mat.or.vec(length(UT_ID$SiteNumber), 3)
UT_TMP[ ,1] = UT_ID$SiteNumber; UT_DIS = UT_GAG = UT_TMP
for(i in 1:length(UT_ID$SiteNumber)){
  UT_TMP[i, 2:3] = usgs_dataRetrieveTemp(UT_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  UT_DIS[i, 2:3] = usgs_dataRetrieveTemp(UT_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  UT_GAG[i, 2:3] = usgs_dataRetrieveTemp(UT_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
UT_OBS = as.data.frame(obs_string(UT_TMP, UT_DIS, UT_GAG))
colnames(UT_OBS) = c("ID", "obs", "time")

# add other important details
UT_OBS$name = UT_ID$SiteName
UT_OBS$url = UT_ID$SiteNWISURL
UT_OBS$lon = UT_ID$SiteLongitude
UT_OBS$lat = UT_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(SD_OBS, file="rds_output/southdakota_obs.rds")
saveRDS(TN_OBS, file="rds_output/tennessee_obs.rds")
saveRDS(UT_OBS, file="rds_output/utah_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
