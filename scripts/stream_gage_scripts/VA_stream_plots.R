# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 13, 2018; set as seperate script; chance avg. discharge to 1 value and fix midnight/noon axis
# Previous edit: May 29, 2018; implement techniques from dataRetrieval package to improve data extraction; add gage height and avg. discharge
# Previous edit: June 17, 2017; Created
#
# This script parses discharge and gage height data of stream gauges from the
# United States Geological Survey (USGS) and plots the results.
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
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
# --------------------------------------------------------------------------------------------------------------------
# Ensure necessary packages are installed and loaded
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)

# Read functions to retrieve data and plot
inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/stream_gage_scripts/"
source(paste0(inDir,"usgs_dataRetrieve.R"))
source(paste0(inDir,"stream_gage_plot_func.R"))
# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width   = 4            # Width
p.height = 2.5           # Height

# Extract the dates of this previous week for use in the url.
e.date = Sys.Date()      # End date
b.date = Sys.Date() - 7  # Beginning date

# Determine midnight and noon for dates of this previous week
day = 0:7
day_midnight = as.POSIXct(paste(Sys.Date() - day, "00:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
day_noon = as.POSIXct(paste(Sys.Date() - day, "12:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
# --------------------------------------------------------------------------------------------------------------------
# Create vector including each station ID.
# <!-- USGS STATIONS -->
# <!-- Virginia -->
VA_ID = c("01652500",
          "01653000", "01654000", "01646000", "01644000", "01643700", "01638480", "01615000", "01613900", "01634500",
          "01634000", "01635500", "01631000", "01633000", "01632900", "01629500", "01662800", "01663500", "01664000",
          "01656000", "01658500", "01660400", "01668000", "01673800", "01667500", "01666500", "01665500", "01628500",
          "01632082", "01632000", "01622000", "01620500", "01627500", "01625000", "01626850", "01626000", "02031000",
          "02029000", "02030000", "02034000", "01674000", "01671100", "01672500", "01671020", "01673000", "01673550",
          "01674500", "01669000", "01669520", "02042500", "02037500", "02037000", "02041650", "02041000", "02040000",
          "02036500", "02035000", "02039500", "02030500", "02039000", "02038850", "02026000", "02028500", "02027000",
          "02027500", "02025500", "02024000", "02022500", "02021500", "02020500", "02015700", "02011460", "02011490",
          "02011470", "02011500", "02011400", "02011800", "02013000", "02013100", "02014000", "02016000", "02018000",
          "02017500", "02055100", "02019500", "02055000", "02059500", "02061500", "02064000", "02062500",
          "02075500", "02077000", "02066000", "02079640", "02051500", "02044500", "02046000", "02052000", "02045500",
          "02047000", "02047500", "02049500", "02060500", "02058400", "02056900", "02073000", "02072000", "02072500",
          "02054500", "03170000", "03171000", "03173000", "03175500", "03176500", "03168000", "03167000", "03165500",
          "03165000", "03164000", "03474000", "03471500", "03488000", "03475000", "03473000", "03478400", "03524000",
          "03207800", "03208500", "03209000", "03208950", "03490000", "03527000", "03529500", "03531500")

# --------------------------------------------------------------------------------------------------------------------
# Run the function extracting the data we want and creating a plot.
# Run through each Virginia station.
for(i in 1:length(VA_ID)){ stream_gage_plot(VA_ID[i], b.date, e.date, day_midnight, day_noon) }

# --------------------------------------------------------------------------------------------------------------------
