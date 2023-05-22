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
# Read in USGS stream gage metadata for Maryland
MD_ID = read.csv("stream_gages/maryland.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MD_ID$SiteNumber)){
    if(nchar(MD_ID$SiteNumber[i]) < 8){
        MD_ID$SiteNumber[i] = paste(0, MD_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MD_TMP = mat.or.vec(length(MD_ID$SiteNumber), 3)
MD_TMP[ ,1] = MD_ID$SiteNumber; MD_DIS = MD_GAG = MD_TMP
for(i in 1:length(MD_ID$SiteNumber)){
  MD_TMP[i, 2:3] = usgs_dataRetrieveTemp(MD_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MD_DIS[i, 2:3] = usgs_dataRetrieveTemp(MD_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MD_GAG[i, 2:3] = usgs_dataRetrieveTemp(MD_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MD_OBS = as.data.frame(obs_string(MD_TMP, MD_DIS, MD_GAG))
colnames(MD_OBS) = c("ID", "obs", "time")

# add other important details
MD_OBS$name = MD_ID$SiteName
MD_OBS$url = MD_ID$SiteNWISURL
MD_OBS$lon = MD_ID$SiteLongitude
MD_OBS$lat = MD_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Massachusetts
MA_ID = read.csv("stream_gages/massachusetts.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MA_ID$SiteNumber)){
    if(nchar(MA_ID$SiteNumber[i]) < 8){
        MA_ID$SiteNumber[i] = paste(0, MA_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MA_TMP = mat.or.vec(length(MA_ID$SiteNumber), 3)
MA_TMP[ ,1] = MA_ID$SiteNumber; MA_DIS = MA_GAG = MA_TMP
for(i in 1:length(MA_ID$SiteNumber)){
  MA_TMP[i, 2:3] = usgs_dataRetrieveTemp(MA_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MA_DIS[i, 2:3] = usgs_dataRetrieveTemp(MA_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MA_GAG[i, 2:3] = usgs_dataRetrieveTemp(MA_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MA_OBS = as.data.frame(obs_string(MA_TMP, MA_DIS, MA_GAG))
colnames(MA_OBS) = c("ID", "obs", "time")

# add other important details
MA_OBS$name = MA_ID$SiteName
MA_OBS$url = MA_ID$SiteNWISURL
MA_OBS$lon = MA_ID$SiteLongitude
MA_OBS$lat = MA_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(MD_OBS, file="rds_output/maryland_obs.rds")
saveRDS(MA_OBS, file="rds_output/massachusetts_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
