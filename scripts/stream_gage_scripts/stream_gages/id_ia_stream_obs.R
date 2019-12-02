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
# Read in USGS stream gage metadata for Idaho
ID_ID = read.csv("stream_gages/idaho.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(ID_ID$SiteNumber)){
    if(nchar(ID_ID$SiteNumber[i]) < 8){
        ID_ID$SiteNumber[i] = paste(0, ID_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
ID_TMP = mat.or.vec(length(ID_ID$SiteNumber), 3)
ID_TMP[ ,1] = ID_ID$SiteNumber; ID_DIS = ID_GAG = ID_TMP
for(i in 1:length(ID_ID$SiteNumber)){
  ID_TMP[i, 2:3] = usgs_dataRetrieveTemp(ID_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  ID_DIS[i, 2:3] = usgs_dataRetrieveTemp(ID_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  ID_GAG[i, 2:3] = usgs_dataRetrieveTemp(ID_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
ID_OBS = as.data.frame(obs_string(ID_TMP, ID_DIS, ID_GAG))
colnames(ID_OBS) = c("ID", "obs", "time")

# add other important details
ID_OBS$name = ID_ID$SiteName
ID_OBS$url = ID_ID$SiteNWISURL
ID_OBS$lon = ID_ID$SiteLongitude
ID_OBS$lat = ID_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Iowa
IA_ID = read.csv("stream_gages/iowa.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(IA_ID$SiteNumber)){
    if(nchar(IA_ID$SiteNumber[i]) < 8){
        IA_ID$SiteNumber[i] = paste(0, IA_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
IA_TMP = mat.or.vec(length(IA_ID$SiteNumber), 3)
IA_TMP[ ,1] = IA_ID$SiteNumber; IA_DIS = IA_GAG = IA_TMP
for(i in 1:length(IA_ID$SiteNumber)){
  IA_TMP[i, 2:3] = usgs_dataRetrieveTemp(IA_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  IA_DIS[i, 2:3] = usgs_dataRetrieveTemp(IA_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  IA_GAG[i, 2:3] = usgs_dataRetrieveTemp(IA_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
IA_OBS = as.data.frame(obs_string(IA_TMP, IA_DIS, IA_GAG))
colnames(IA_OBS) = c("ID", "obs", "time")

# add other important details
IA_OBS$name = IA_ID$SiteName
IA_OBS$url = IA_ID$SiteNWISURL
IA_OBS$lon = IA_ID$SiteLongitude
IA_OBS$lat = IA_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(ID_OBS, file="rds_output/idaho_obs.rds")
saveRDS(IA_OBS, file="rds_output/iowa_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
