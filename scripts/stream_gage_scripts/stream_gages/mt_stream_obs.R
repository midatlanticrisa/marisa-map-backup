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
# Read in USGS stream gage metadata for Montana
MT_ID = read.csv("stream_gages/montana.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MT_ID$SiteNumber)){
    if(nchar(MT_ID$SiteNumber[i]) < 8){
        MT_ID$SiteNumber[i] = paste(0, MT_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MT_TMP = mat.or.vec(length(MT_ID$SiteNumber), 3)
MT_TMP[ ,1] = MT_ID$SiteNumber; MT_DIS = MT_GAG = MT_TMP
for(i in 1:length(MT_ID$SiteNumber)){
  MT_TMP[i, 2:3] = usgs_dataRetrieveTemp(MT_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MT_DIS[i, 2:3] = usgs_dataRetrieveTemp(MT_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MT_GAG[i, 2:3] = usgs_dataRetrieveTemp(MT_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MT_OBS = as.data.frame(obs_string(MT_TMP, MT_DIS, MT_GAG))
colnames(MT_OBS) = c("ID", "obs", "time")

# add other important details
MT_OBS$name = MT_ID$SiteName
MT_OBS$url = MT_ID$SiteNWISURL
MT_OBS$lon = MT_ID$SiteLongitude
MT_OBS$lat = MT_ID$SiteLatitude

# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(MT_OBS, file="rds_output/montana_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
