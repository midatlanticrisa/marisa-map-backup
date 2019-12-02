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
# Read in USGS stream gage metadata for Missouri
MO_ID = read.csv("stream_gages/missouri.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(MO_ID$SiteNumber)){
    if(nchar(MO_ID$SiteNumber[i]) < 8){
        MO_ID$SiteNumber[i] = paste(0, MO_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
MO_TMP = mat.or.vec(length(MO_ID$SiteNumber), 3)
MO_TMP[ ,1] = MO_ID$SiteNumber; MO_DIS = MO_GAG = MO_TMP
for(i in 1:length(MO_ID$SiteNumber)){
  MO_TMP[i, 2:3] = usgs_dataRetrieveTemp(MO_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  MO_DIS[i, 2:3] = usgs_dataRetrieveTemp(MO_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  MO_GAG[i, 2:3] = usgs_dataRetrieveTemp(MO_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
MO_OBS = as.data.frame(obs_string(MO_TMP, MO_DIS, MO_GAG))
colnames(MO_OBS) = c("ID", "obs", "time")

# add other important details
MO_OBS$name = MO_ID$SiteName
MO_OBS$url = MO_ID$SiteNWISURL
MO_OBS$lon = MO_ID$SiteLongitude
MO_OBS$lat = MO_ID$SiteLatitude

# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(MO_OBS, file="rds_output/missouri_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
