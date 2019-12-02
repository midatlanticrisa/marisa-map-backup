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
# Read in USGS stream gage metadata for Kansas
KS_ID = read.csv("stream_gages/kansas.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(KS_ID$SiteNumber)){
    if(nchar(KS_ID$SiteNumber[i]) < 8){
        KS_ID$SiteNumber[i] = paste(0, KS_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
KS_TMP = mat.or.vec(length(KS_ID$SiteNumber), 3)
KS_TMP[ ,1] = KS_ID$SiteNumber; KS_DIS = KS_GAG = KS_TMP
for(i in 1:length(KS_ID$SiteNumber)){
  KS_TMP[i, 2:3] = usgs_dataRetrieveTemp(KS_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  KS_DIS[i, 2:3] = usgs_dataRetrieveTemp(KS_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  KS_GAG[i, 2:3] = usgs_dataRetrieveTemp(KS_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
KS_OBS = as.data.frame(obs_string(KS_TMP, KS_DIS, KS_GAG))
colnames(KS_OBS) = c("ID", "obs", "time")

# add other important details
KS_OBS$name = KS_ID$SiteName
KS_OBS$url = KS_ID$SiteNWISURL
KS_OBS$lon = KS_ID$SiteLongitude
KS_OBS$lat = KS_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Kentucky
KY_ID = read.csv("stream_gages/kentucky.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(KY_ID$SiteNumber)){
    if(nchar(KY_ID$SiteNumber[i]) < 8){
        KY_ID$SiteNumber[i] = paste(0, KY_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
KY_TMP = mat.or.vec(length(KY_ID$SiteNumber), 3)
KY_TMP[ ,1] = KY_ID$SiteNumber; KY_DIS = KY_GAG = KY_TMP
for(i in 1:length(KY_ID$SiteNumber)){
  KY_TMP[i, 2:3] = usgs_dataRetrieveTemp(KY_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  KY_DIS[i, 2:3] = usgs_dataRetrieveTemp(KY_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  KY_GAG[i, 2:3] = usgs_dataRetrieveTemp(KY_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
KY_OBS = as.data.frame(obs_string(KY_TMP, KY_DIS, KY_GAG))
colnames(KY_OBS) = c("ID", "obs", "time")

# add other important details
KY_OBS$name = KY_ID$SiteName
KY_OBS$url = KY_ID$SiteNWISURL
KY_OBS$lon = KY_ID$SiteLongitude
KY_OBS$lat = KY_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(KS_OBS, file="rds_output/kansas_obs.rds")
saveRDS(KY_OBS, file="rds_output/kentucky_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
