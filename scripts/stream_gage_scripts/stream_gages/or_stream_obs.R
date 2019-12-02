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
# Read in USGS stream gage metadata for Oregon
OR_ID = read.csv("stream_gages/oregon.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(OR_ID$SiteNumber)){
    if(nchar(OR_ID$SiteNumber[i]) < 8){
        OR_ID$SiteNumber[i] = paste(0, OR_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
OR_TMP = mat.or.vec(length(OR_ID$SiteNumber), 3)
OR_TMP[ ,1] = OR_ID$SiteNumber; OR_DIS = OR_GAG = OR_TMP
for(i in 1:length(OR_ID$SiteNumber)){
  OR_TMP[i, 2:3] = usgs_dataRetrieveTemp(OR_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  OR_DIS[i, 2:3] = usgs_dataRetrieveTemp(OR_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  OR_GAG[i, 2:3] = usgs_dataRetrieveTemp(OR_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
OR_OBS = as.data.frame(obs_string(OR_TMP, OR_DIS, OR_GAG))
colnames(OR_OBS) = c("ID", "obs", "time")

# add other important details
OR_OBS$name = OR_ID$SiteName
OR_OBS$url = OR_ID$SiteNWISURL
OR_OBS$lon = OR_ID$SiteLongitude
OR_OBS$lat = OR_ID$SiteLatitude

# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(OR_OBS, file="rds_output/oregon_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
