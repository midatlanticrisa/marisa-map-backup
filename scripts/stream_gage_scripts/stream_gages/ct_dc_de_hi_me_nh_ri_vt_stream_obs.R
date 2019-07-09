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
# Read in USGS stream gage metadata for Conneticut
CT_ID = read.csv("stream_gages/conneticut.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(CT_ID$SiteNumber)){
    if(nchar(CT_ID$SiteNumber[i]) < 8){
        CT_ID$SiteNumber[i] = paste(0, CT_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
CT_TMP = mat.or.vec(length(CT_ID$SiteNumber), 3)
CT_TMP[ ,1] = CT_ID$SiteNumber; CT_DIS = CT_GAG = CT_TMP
for(i in 1:length(CT_ID$SiteNumber)){
  CT_TMP[i, 2:3] = usgs_dataRetrieveTemp(CT_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  CT_DIS[i, 2:3] = usgs_dataRetrieveTemp(CT_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  CT_GAG[i, 2:3] = usgs_dataRetrieveTemp(CT_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
CT_OBS = as.data.frame(obs_string(CT_TMP, CT_DIS, CT_GAG))
colnames(CT_OBS) = c("ID", "obs", "time")

# add other important details
CT_OBS$name = CT_ID$SiteName
CT_OBS$url = CT_ID$SiteNWISURL
CT_OBS$lon = CT_ID$SiteLongitude
CT_OBS$lat = CT_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for District of Columbia
DC_ID = read.csv("stream_gages/DC.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(DC_ID$SiteNumber)){
    if(nchar(DC_ID$SiteNumber[i]) < 8){
        DC_ID$SiteNumber[i] = paste(0, DC_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
DC_TMP = mat.or.vec(length(DC_ID$SiteNumber), 3)
DC_TMP[ ,1] = DC_ID$SiteNumber; DC_DIS = DC_GAG = DC_TMP
for(i in 1:length(DC_ID$SiteNumber)){
  DC_TMP[i, 2:3] = usgs_dataRetrieveTemp(DC_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  DC_DIS[i, 2:3] = usgs_dataRetrieveTemp(DC_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  DC_GAG[i, 2:3] = usgs_dataRetrieveTemp(DC_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
DC_OBS = as.data.frame(obs_string(DC_TMP, DC_DIS, DC_GAG))
colnames(DC_OBS) = c("ID", "obs", "time")

# add other important details
DC_OBS$name = DC_ID$SiteName
DC_OBS$url = DC_ID$SiteNWISURL
DC_OBS$lon = DC_ID$SiteLongitude
DC_OBS$lat = DC_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Delaware
DE_ID = read.csv("stream_gages/delaware.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(DE_ID$SiteNumber)){
    if(nchar(DE_ID$SiteNumber[i]) < 8){
        DE_ID$SiteNumber[i] = paste(0, DE_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
DE_TMP = mat.or.vec(length(DE_ID$SiteNumber), 3)
DE_TMP[ ,1] = DE_ID$SiteNumber; DE_DIS = DE_GAG = DE_TMP
for(i in 1:length(DE_ID$SiteNumber)){
  DE_TMP[i, 2:3] = usgs_dataRetrieveTemp(DE_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  DE_DIS[i, 2:3] = usgs_dataRetrieveTemp(DE_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  DE_GAG[i, 2:3] = usgs_dataRetrieveTemp(DE_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
DE_OBS = as.data.frame(obs_string(DE_TMP, DE_DIS, DE_GAG))
colnames(DE_OBS) = c("ID", "obs", "time")

# add other important details
DE_OBS$name = DE_ID$SiteName
DE_OBS$url = DE_ID$SiteNWISURL
DE_OBS$lon = DE_ID$SiteLongitude
DE_OBS$lat = DE_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Hawaii
HI_ID = read.csv("stream_gages/hawaii.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(HI_ID$SiteNumber)){
    if(nchar(HI_ID$SiteNumber[i]) < 8){
        HI_ID$SiteNumber[i] = paste(0, HI_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
HI_TMP = mat.or.vec(length(HI_ID$SiteNumber), 3)
HI_TMP[ ,1] = HI_ID$SiteNumber; HI_DIS = HI_GAG = HI_TMP
for(i in 1:length(HI_ID$SiteNumber)){
  HI_TMP[i, 2:3] = usgs_dataRetrieveTemp(HI_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  HI_DIS[i, 2:3] = usgs_dataRetrieveTemp(HI_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  HI_GAG[i, 2:3] = usgs_dataRetrieveTemp(HI_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
HI_OBS = as.data.frame(obs_string(HI_TMP, HI_DIS, HI_GAG))
colnames(HI_OBS) = c("ID", "obs", "time")

# add other important details
HI_OBS$name = HI_ID$SiteName
HI_OBS$url = HI_ID$SiteNWISURL
HI_OBS$lon = HI_ID$SiteLongitude
HI_OBS$lat = HI_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Maine
ME_ID = read.csv("stream_gages/maine.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(ME_ID$SiteNumber)){
    if(nchar(ME_ID$SiteNumber[i]) < 8){
        ME_ID$SiteNumber[i] = paste(0, ME_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
ME_TMP = mat.or.vec(length(ME_ID$SiteNumber), 3)
ME_TMP[ ,1] = ME_ID$SiteNumber; ME_DIS = ME_GAG = ME_TMP
for(i in 1:length(ME_ID$SiteNumber)){
  ME_TMP[i, 2:3] = usgs_dataRetrieveTemp(ME_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  ME_DIS[i, 2:3] = usgs_dataRetrieveTemp(ME_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  ME_GAG[i, 2:3] = usgs_dataRetrieveTemp(ME_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
ME_OBS = as.data.frame(obs_string(ME_TMP, ME_DIS, ME_GAG))
colnames(ME_OBS) = c("ID", "obs", "time")

# add other important details
ME_OBS$name = ME_ID$SiteName
ME_OBS$url = ME_ID$SiteNWISURL
ME_OBS$lon = ME_ID$SiteLongitude
ME_OBS$lat = ME_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for New Hampshire
NH_ID = read.csv("stream_gages/newhampshire.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(NH_ID$SiteNumber)){
    if(nchar(NH_ID$SiteNumber[i]) < 8){
        NH_ID$SiteNumber[i] = paste(0, NH_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
NH_TMP = mat.or.vec(length(NH_ID$SiteNumber), 3)
NH_TMP[ ,1] = NH_ID$SiteNumber; NH_DIS = NH_GAG = NH_TMP
for(i in 1:length(NH_ID$SiteNumber)){
  NH_TMP[i, 2:3] = usgs_dataRetrieveTemp(NH_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  NH_DIS[i, 2:3] = usgs_dataRetrieveTemp(NH_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  NH_GAG[i, 2:3] = usgs_dataRetrieveTemp(NH_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
NH_OBS = as.data.frame(obs_string(NH_TMP, NH_DIS, NH_GAG))
colnames(NH_OBS) = c("ID", "obs", "time")

# add other important details
NH_OBS$name = NH_ID$SiteName
NH_OBS$url = NH_ID$SiteNWISURL
NH_OBS$lon = NH_ID$SiteLongitude
NH_OBS$lat = NH_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Rhode Island
RI_ID = read.csv("stream_gages/rhodeisland.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(RI_ID$SiteNumber)){
    if(nchar(RI_ID$SiteNumber[i]) < 8){
        RI_ID$SiteNumber[i] = paste(0, RI_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
RI_TMP = mat.or.vec(length(RI_ID$SiteNumber), 3)
RI_TMP[ ,1] = RI_ID$SiteNumber; RI_DIS = RI_GAG = RI_TMP
for(i in 1:length(RI_ID$SiteNumber)){
  RI_TMP[i, 2:3] = usgs_dataRetrieveTemp(RI_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  RI_DIS[i, 2:3] = usgs_dataRetrieveTemp(RI_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  RI_GAG[i, 2:3] = usgs_dataRetrieveTemp(RI_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
RI_OBS = as.data.frame(obs_string(RI_TMP, RI_DIS, RI_GAG))
colnames(RI_OBS) = c("ID", "obs", "time")

# add other important details
RI_OBS$name = RI_ID$SiteName
RI_OBS$url = RI_ID$SiteNWISURL
RI_OBS$lon = RI_ID$SiteLongitude
RI_OBS$lat = RI_ID$SiteLatitude
# -------------------------------------------------
# Read in USGS stream gage metadata for Vermont
VT_ID = read.csv("stream_gages/vermont.csv", header = TRUE)

# Site numbers are between 8 and 15 digits long
for(i in 1:length(VT_ID$SiteNumber)){
    if(nchar(VT_ID$SiteNumber[i]) < 8){
        VT_ID$SiteNumber[i] = paste(0, VT_ID$SiteNumber[i], sep="")
    }
}

# extract observation data
VT_TMP = mat.or.vec(length(VT_ID$SiteNumber), 3)
VT_TMP[ ,1] = VT_ID$SiteNumber; VT_DIS = VT_GAG = VT_TMP
for(i in 1:length(VT_ID$SiteNumber)){
  VT_TMP[i, 2:3] = usgs_dataRetrieveTemp(VT_ID$SiteNumber[i], "00010", b.date, e.date, tz="US/Pacific")
  VT_DIS[i, 2:3] = usgs_dataRetrieveTemp(VT_ID$SiteNumber[i], "00060", b.date, e.date, tz="US/Pacific")
  VT_GAG[i, 2:3] = usgs_dataRetrieveTemp(VT_ID$SiteNumber[i], "00065", b.date, e.date, tz="US/Pacific")
}
VT_OBS = as.data.frame(obs_string(VT_TMP, VT_DIS, VT_GAG))
colnames(VT_OBS) = c("ID", "obs", "time")

# add other important details
VT_OBS$name = VT_ID$SiteName
VT_OBS$url = VT_ID$SiteNWISURL
VT_OBS$lon = VT_ID$SiteLongitude
VT_OBS$lat = VT_ID$SiteLatitude
# --------------------------------------------------------------------------------------------------------------------
# save output
saveRDS(CT_OBS, file="rds_output/conneticut_obs.rds")
saveRDS(DC_OBS, file="rds_output/district_of_columbia_obs.rds")
saveRDS(DE_OBS, file="rds_output/delaware_obs.rds")
saveRDS(HI_OBS, file="rds_output/hawaii_obs.rds")
saveRDS(ME_OBS, file="rds_output/maine_obs.rds")
saveRDS(NH_OBS, file="rds_output/newhampshire_obs.rds")
saveRDS(RI_OBS, file="rds_output/rhodeisland_obs.rds")
saveRDS(VT_OBS, file="rds_output/vermont_obs.rds")
# --------------------------------------------------------------------------------------------------------------------
