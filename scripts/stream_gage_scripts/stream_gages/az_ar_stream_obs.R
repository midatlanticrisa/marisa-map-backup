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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WIRRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WIRRANTIES OF MERCHANTABILITY,
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
# Read in USGS stream gage metadata for Arizona
AZ_ID = read.csv("stream_gages/arizona.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(AZ_ID$SiteNumber)){
  if(nchar(AZ_ID$SiteNumber[i]) < 8){
    AZ_ID$SiteNumber[i] = paste(0, AZ_ID$SiteNumber[i], sep="")
  }
}

# AZ_ID$SiteNumber = paste(0, AZ_ID$SiteNumber, sep="")

# extract observation data
AZ_TMP = mat.or.vec(length(AZ_ID$SiteNumber), 3)
AZ_TMP[ ,1] = AZ_ID$SiteNumber; AZ_DIS = AZ_GAG = AZ_TMP
for(i in 1:length(AZ_ID$SiteNumber)){
  AZ_TMP[i, 2:3] = usgs_dataRetrieveTemp(AZ_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  AZ_DIS[i, 2:3] = usgs_dataRetrieveTemp(AZ_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  AZ_GAG[i, 2:3] = usgs_dataRetrieveTemp(AZ_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
AZ_OBS = as.data.frame(obs_string(AZ_TMP, AZ_DIS, AZ_GAG))
colnames(AZ_OBS) = c("ID", "obs", "time")

# add other important details
AZ_OBS$name = AZ_ID$SiteName
AZ_OBS$url = AZ_ID$SiteNWISURL
AZ_OBS$lon = AZ_ID$SiteLongitude
AZ_OBS$lat = AZ_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Arkansas
AR_ID = read.csv("stream_gages/arkansas.csv", header = TRUE)
AR_ID$SiteNumber = paste(0, AR_ID$SiteNumber, sep="")

# Site numbers are between 8 and 15 digits long
for(i in 1:length(AR_ID$SiteNumber)){
  if(nchar(AR_ID$SiteNumber[i]) < 8){
    AR_ID$SiteNumber[i] = paste(0, AR_ID$SiteNumber[i], sep="")
  }
}

# extract observation data
AR_TMP = mat.or.vec(length(AR_ID$SiteNumber), 3)
AR_TMP[ ,1] = AR_ID$SiteNumber; AR_DIS = AR_GAG = AR_TMP
for(i in 1:length(AR_ID$SiteNumber)){
  AR_TMP[i, 2:3] = usgs_dataRetrieveTemp(AR_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  AR_DIS[i, 2:3] = usgs_dataRetrieveTemp(AR_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  AR_GAG[i, 2:3] = usgs_dataRetrieveTemp(AR_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
AR_OBS = as.data.frame(obs_string(AR_TMP, AR_DIS, AR_GAG))
colnames(AR_OBS) = c("ID", "obs", "time")

# add other important details
AR_OBS$name = AR_ID$SiteName
AR_OBS$url = AR_ID$SiteNWISURL
AR_OBS$lon = AR_ID$SiteLongitude
AR_OBS$lat = AR_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(AZ_OBS, file="rds_output/arizona_obs.rds")
saveRDS(AR_OBS, file="rds_output/arkansas_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
