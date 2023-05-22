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
# Read in USGS stream gage metadata for New Mexico
NM_ID = read.csv("stream_gages/newmexico.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(NM_ID$SiteNumber)){
    if(nchar(NM_ID$SiteNumber[i]) < 8){
        NM_ID$SiteNumber[i] = paste(0, NM_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
NM_TMP = mat.or.vec(length(NM_ID$SiteNumber), 3)
NM_TMP[ ,1] = NM_ID$SiteNumber; NM_DIS = NM_GAG = NM_TMP
for(i in 1:length(NM_ID$SiteNumber)){
  NM_TMP[i, 2:3] = usgs_dataRetrieveTemp(NM_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  NM_DIS[i, 2:3] = usgs_dataRetrieveTemp(NM_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  NM_GAG[i, 2:3] = usgs_dataRetrieveTemp(NM_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
NM_OBS = as.data.frame(obs_string(NM_TMP, NM_DIS, NM_GAG))
colnames(NM_OBS) = c("ID", "obs", "time")

# add other important details
NM_OBS$name = NM_ID$SiteName
NM_OBS$url = NM_ID$SiteNWISURL
NM_OBS$lon = NM_ID$SiteLongitude
NM_OBS$lat = NM_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for North Dakota
ND_ID = read.csv("stream_gages/northdakota.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(ND_ID$SiteNumber)){
    if(nchar(ND_ID$SiteNumber[i]) < 8){
        ND_ID$SiteNumber[i] = paste(0, ND_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
ND_TMP = mat.or.vec(length(ND_ID$SiteNumber), 3)
ND_TMP[ ,1] = ND_ID$SiteNumber; ND_DIS = ND_GAG = ND_TMP
for(i in 1:length(ND_ID$SiteNumber)){
  ND_TMP[i, 2:3] = usgs_dataRetrieveTemp(ND_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  ND_DIS[i, 2:3] = usgs_dataRetrieveTemp(ND_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  ND_GAG[i, 2:3] = usgs_dataRetrieveTemp(ND_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
ND_OBS = as.data.frame(obs_string(ND_TMP, ND_DIS, ND_GAG))
colnames(ND_OBS) = c("ID", "obs", "time")

# add other important details
ND_OBS$name = ND_ID$SiteName
ND_OBS$url = ND_ID$SiteNWISURL
ND_OBS$lon = ND_ID$SiteLongitude
ND_OBS$lat = ND_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(NM_OBS, file="rds_output/newmexico_obs.rds")
saveRDS(ND_OBS, file="rds_output/northdakota_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
