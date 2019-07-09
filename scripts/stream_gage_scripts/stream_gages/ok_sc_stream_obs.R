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
# Read in USGS stream gage metadata for Oklahoma
OK_ID = read.csv("stream_gages/oklahoma.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(OK_ID$SiteNumber)){
    if(nchar(OK_ID$SiteNumber[i]) < 8){
        OK_ID$SiteNumber[i] = paste(0, OK_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
OK_TMP = mat.or.vec(length(OK_ID$SiteNumber), 3)
OK_TMP[ ,1] = OK_ID$SiteNumber; OK_DIS = OK_GAG = OK_TMP
for(i in 1:length(OK_ID$SiteNumber)){
  OK_TMP[i, 2:3] = usgs_dataRetrieveTemp(OK_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  OK_DIS[i, 2:3] = usgs_dataRetrieveTemp(OK_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  OK_GAG[i, 2:3] = usgs_dataRetrieveTemp(OK_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
OK_OBS = as.data.frame(obs_string(OK_TMP, OK_DIS, OK_GAG))
colnames(OK_OBS) = c("ID", "obs", "time")

# add other important details
OK_OBS$name = OK_ID$SiteName
OK_OBS$url = OK_ID$SiteNWISURL
OK_OBS$lon = OK_ID$SiteLongitude
OK_OBS$lat = OK_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for South Carolina
SC_ID = read.csv("stream_gages/southcarolina.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(SC_ID$SiteNumber)){
    if(nchar(SC_ID$SiteNumber[i]) < 8){
        SC_ID$SiteNumber[i] = paste(0, SC_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
SC_TMP = mat.or.vec(length(SC_ID$SiteNumber), 3)
SC_TMP[ ,1] = SC_ID$SiteNumber; SC_DIS = SC_GAG = SC_TMP
for(i in 1:length(SC_ID$SiteNumber)){
  SC_TMP[i, 2:3] = usgs_dataRetrieveTemp(SC_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  SC_DIS[i, 2:3] = usgs_dataRetrieveTemp(SC_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  SC_GAG[i, 2:3] = usgs_dataRetrieveTemp(SC_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
SC_OBS = as.data.frame(obs_string(SC_TMP, SC_DIS, SC_GAG))
colnames(SC_OBS) = c("ID", "obs", "time")

# add other important details
SC_OBS$name = SC_ID$SiteName
SC_OBS$url = SC_ID$SiteNWISURL
SC_OBS$lon = SC_ID$SiteLongitude
SC_OBS$lat = SC_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(OK_OBS, file="rds_output/oklahoma_obs.rds")
saveRDS(SC_OBS, file="rds_output/southcarolina_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
