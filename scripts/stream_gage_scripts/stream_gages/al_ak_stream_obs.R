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
# Read in USGS stream gage metadata for Alabama
AL_ID = read.csv("stream_gages/alabama.csv", header = TRUE)
# AL_ID$SiteNumber = paste(0, AL_ID$SiteNumber, sep="")

# Site numbers are between 8 and 15 digits long
for(i in 1:length(AL_ID$SiteNumber)){
  if(nchar(AL_ID$SiteNumber[i]) < 8){
    AL_ID$SiteNumber[i] = paste(0, AL_ID$SiteNumber[i], sep="")
  }
}

# extract observation data
AL_TMP = mat.or.vec(length(AL_ID$SiteNumber), 3)
AL_TMP[ ,1] = AL_ID$SiteNumber; AL_DIS = AL_GAG = AL_TMP
for(i in 1:length(AL_ID$SiteNumber)){
  AL_TMP[i, 2:3] = usgs_dataRetrieveTemp(AL_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Central")
  AL_DIS[i, 2:3] = usgs_dataRetrieveTemp(AL_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Central")
  AL_GAG[i, 2:3] = usgs_dataRetrieveTemp(AL_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Central")
}
AL_OBS = as.data.frame(obs_string(AL_TMP, AL_DIS, AL_GAG))
colnames(AL_OBS) = c("ID", "obs", "time")

# add other important details
AL_OBS$name = AL_ID$SiteName
AL_OBS$url = AL_ID$SiteNWISURL
AL_OBS$lon = AL_ID$SiteLongitude
AL_OBS$lat = AL_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Alaska
AK_ID = read.csv("stream_gages/alaska.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(AK_ID$SiteNumber)){
  if(nchar(AK_ID$SiteNumber[i]) < 8){
    AK_ID$SiteNumber[i] = paste(0, AK_ID$SiteNumber[i], sep="")
  }
}

# extract observation data
AK_TMP = mat.or.vec(length(AK_ID$SiteNumber), 3)
AK_TMP[ ,1] = AK_ID$SiteNumber; AK_DIS = AK_GAG = AK_TMP
for(i in 1:length(AK_ID$SiteNumber)){
  AK_TMP[i, 2:3] = usgs_dataRetrieveTemp(AK_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Alaska")
  AK_DIS[i, 2:3] = usgs_dataRetrieveTemp(AK_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Alaska")
  AK_GAG[i, 2:3] = usgs_dataRetrieveTemp(AK_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Alaska")
}
AK_OBS = as.data.frame(obs_string(AK_TMP, AK_DIS, AK_GAG))
colnames(AK_OBS) = c("ID", "obs", "time")

# add other important details
AK_OBS$name = AK_ID$SiteName
AK_OBS$url = AK_ID$SiteNWISURL
AK_OBS$lon = AK_ID$SiteLongitude
AK_OBS$lat = AK_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(AL_OBS, file="rds_output/alabama_obs.rds")
saveRDS(AK_OBS, file="rds_output/alaska_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
