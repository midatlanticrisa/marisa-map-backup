# --------------------------------------------------------------------------------------------------------------------
# Copyright 2018 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 14, 2018
# Last edit: June 7, 2018
#
# This script parses RSS meteorological data from buoy stations from the
# National Data Buoy Center and outputs the results in a single file.
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

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
#if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata")){
#  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata")
#}

# Read function to retrieve data
source("usgs_dataRetrieve.R")
source("obs_string_single.R")
# --------------------------------------------------------------------------------------------------------------------
# We only need todays date to get the most recent value.
e.date = Sys.Date()      # End date
b.date = Sys.Date()-1      # Beginning date
# --------------------------------------------------------------------------------------------------------------------
# Function to create a string of observations based on what is available and which time is oldest.
obs_string = function(TMP, DIS, GAG){
  OBS = TMP
  for(i in 1:length(TMP[ ,1])){
    if(TMP[i,2]=="NA" && DIS[i,2]=="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = ""
      OBS[i,3] = ""

    } else if (TMP[i,2]=="NA" && DIS[i,2]=="NA" && GAG[i,2]!="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Gage height: </strong><br/><br/>", GAG[i,2], " ft", sep="")
      OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")

    } else if (TMP[i,2]=="NA" && DIS[i,2]!="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Discharge: </strong>",DIS[i,2], " ft&#179;/s<br/><br/>", sep="")
      OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")

    } else if (TMP[i,2]!="NA" && DIS[i,2]=="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><br/>", sep="")
      OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")

    } else if (TMP[i,2]!="NA" && DIS[i,2]!="NA" && GAG[i,2]=="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><strong>Discharge: </strong>",
                       DIS[i,2], " ft&#179;/s<br/><br/>", sep="")

      if(TMP[i,3] < DIS[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")
      }

    } else if (TMP[i,2]!="NA" && DIS[i,2]=="NA" && GAG[i,2]!="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><strong>Gage height: </strong>",
                       GAG[i,2], " ft<br/><br/>", sep="")

      if(TMP[i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      }

    } else if (TMP[i,2]=="NA" && DIS[i,2]!="NA" && GAG[i,2]!="NA"){
      OBS[i,2] = paste("<br/><br/><strong>Discharge: </strong>", DIS[i,2], " ft&#179;/s<br/><strong>Gage height: </strong>", GAG[i,2], " ft<br/><br/>", sep="")

      if(DIS[i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      }

    } else {
      OBS[i,2] = paste("<br/><br/><strong>Temperature: </strong>", TMP[i,2], " &#8457;<br/><strong>Discharge: </strong>",
                       DIS[i,2], " ft&#179;/s<br/><strong>Gage height: </strong>", GAG[i,2], " ft<br/><br/>", sep="")

      if(TMP[i,3] < DIS[i,3] && TMP[i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", TMP[i,3], sep="")
      } else if(DIS[i,3] < TMP[i,3] && DIS [i,3] < GAG[i,3]){
        OBS[i,3] = paste("<br/><br/>Last Updated on ", DIS[i,3], sep="")
      } else {
        OBS[i,3] = paste("<br/><br/>Last Updated on ", GAG[i,3], sep="")
      }}
  }
  return(OBS)
}
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

# Run through each Virginia station.
VA_TMP = mat.or.vec(length(VA_ID), 3)
VA_TMP[ ,1] = VA_ID; VA_DIS = VA_TMP; VA_GAG = VA_TMP
for(i in 1:length(VA_ID)){
  VA_TMP[i, 2:3] = usgs_dataRetrieveTemp(VA_ID[i], "00010", b.date, e.date, tz="America/New_York")
  VA_DIS[i, 2:3] = usgs_dataRetrieveTemp(VA_ID[i], "00060", b.date, e.date, tz="America/New_York")
  VA_GAG[i, 2:3] = usgs_dataRetrieveTemp(VA_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
VA_OBS = obs_string(VA_TMP, VA_DIS, VA_GAG)

# --------------------------------------------------------------------------------------------------------------------
# Create a geojson object with the observation and statement info and merge into a
# specific file format with adding stream temp and time.
json_merge = paste('VA_streamGauges = {"type": "FeatureCollection","features": [{"type": "Feature", "properties": {"name": "FOURMILE RUN AT ALEXANDRIA", "id": "01652500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01652500", "obs": "',
                   VA_OBS[match("01652500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01652500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01652500.png"}, "geometry": {"type": "Point", "coordinates": [-77.085861, 38.843333]}}, {"type": "Feature", "properties": {"name": "CAMERON RUN AT ALEXANDRIA", "id": "01653000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01653000", "obs": "',
                   VA_OBS[match("01653000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01653000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01653000.png"}, "geometry": {"type": "Point", "coordinates": [-77.11, 38.806389]}}, {"type": "Feature", "properties": {"name": "ACCOTINK CREEK NEAR ANNANDALE", "id": "01654000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01654000", "obs": "',
                   VA_OBS[match("01654000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01654000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01654000.png"}, "geometry": {"type": "Point", "coordinates": [-77.228611, 38.812778]}}, {"type": "Feature", "properties": {"name": "DIFFICULT RUN NEAR GREAT FALLS", "id": "01646000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01646000", "obs": "',
                   VA_OBS[match("01646000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01646000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01646000.png"}, "geometry": {"type": "Point", "coordinates": [-77.246111, 38.975833]}}, {"type": "Feature", "properties": {"name": "GOOSE CREEK NEAR LEESBURG", "id": "01644000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01644000", "obs": "',
                   VA_OBS[match("01644000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01644000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01644000.png"}, "geometry": {"type": "Point", "coordinates": [-77.577778, 39.019444]}}, {"type": "Feature", "properties": {"name": "GOOSE CREEK NEAR MIDDLEBURG", "id": "01643700", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01643700", "obs": "',
                   VA_OBS[match("01643700", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01643700", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01643700.png"}, "geometry": {"type": "Point", "coordinates": [-77.796944, 38.986389]}}, {"type": "Feature", "properties": {"name": "CATOCTIN CREEK AT TAYLORSTOWN", "id": "01638480", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01638480", "obs": "',
                   VA_OBS[match("01638480", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01638480", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01638480.png"}, "geometry": {"type": "Point", "coordinates": [-77.576667, 39.255]}}, {"type": "Feature", "properties": {"name": "OPEQUON CREEK NEAR BERRYVILLE", "id": "01615000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01615000", "obs": "',
                   VA_OBS[match("01615000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01615000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01615000.png"}, "geometry": {"type": "Point", "coordinates": [-78.078333, 39.174722]}}, {"type": "Feature", "properties": {"name": "HOGUE CREEK NEAR HAYFIELD", "id": "01613900", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01613900", "obs": "',
                   VA_OBS[match("01613900", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01613900", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01613900.png"}, "geometry": {"type": "Point", "coordinates": [-78.288333, 39.214444]}}, {"type": "Feature", "properties": {"name": "CEDAR CREEK NEAR WINCHESTER", "id": "01634500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01634500", "obs": "',
                   VA_OBS[match("01634500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01634500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01634500.png"}, "geometry": {"type": "Point", "coordinates": [-78.329722, 39.081111]}}, {"type": "Feature", "properties": {"name": "N F SHENANDOAH RIVER NEAR STRASBURG", "id": "01634000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01634000", "obs": "',
                   VA_OBS[match("01634000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01634000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01634000.png"}, "geometry": {"type": "Point", "coordinates": [-78.336389, 38.976667]}}, {"type": "Feature", "properties": {"name": "PASSAGE CREEK NEAR BUCKTON", "id": "01635500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01635500", "obs": "',
                   VA_OBS[match("01635500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01635500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01635500.png"}, "geometry": {"type": "Point", "coordinates": [-78.266944, 38.958056]}}, {"type": "Feature", "properties": {"name": "S F SHENANDOAH RIVER AT FRONT ROYAL", "id": "01631000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01631000", "obs": "',
                   VA_OBS[match("01631000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01631000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01631000.png"}, "geometry": {"type": "Point", "coordinates": [-78.211111, 38.913889]}}, {"type": "Feature", "properties": {"name": "N F SHENANDOAH RIVER AT MOUNT JACKSON", "id": "01633000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01633000", "obs": "',
                   VA_OBS[match("01633000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01633000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01633000.png"}, "geometry": {"type": "Point", "coordinates": [-78.639167, 38.745556]}}, {"type": "Feature", "properties": {"name": "SMITH CREEK NEAR NEW MARKET", "id": "01632900", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01632900", "obs": "',
                   VA_OBS[match("01632900", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01632900", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01632900.png"}, "geometry": {"type": "Point", "coordinates": [-78.643056, 38.693333]}}, {"type": "Feature", "properties": {"name": "S F SHENANDOAH RIVER NEAR LURAY", "id": "01629500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01629500", "obs": "',
                   VA_OBS[match("01629500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01629500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01629500.png"}, "geometry": {"type": "Point", "coordinates": [-78.535, 38.646111]}}, {"type": "Feature", "properties": {"name": "BATTLE RUN NEAR LAUREL MILLS", "id": "01662800", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01662800", "obs": "',
                   VA_OBS[match("01662800", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01662800", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01662800.png"}, "geometry": {"type": "Point", "coordinates": [-78.074167, 38.655556]}}, {"type": "Feature", "properties": {"name": "HAZEL RIVER AT RIXEYVILLE", "id": "01663500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01663500", "obs": "',
                   VA_OBS[match("01663500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01663500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01663500.png"}, "geometry": {"type": "Point", "coordinates": [-77.965278, 38.591667]}}, {"type": "Feature", "properties": {"name": "RAPPAHANNOCK RIVER AT REMINGTON", "id": "01664000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01664000", "obs": "',
                   VA_OBS[match("01664000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01664000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01664000.png"}, "geometry": {"type": "Point", "coordinates": [-77.813889, 38.530556]}}, {"type": "Feature", "properties": {"name": "CEDAR RUN NEAR CATLETT", "id": "01656000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01656000", "obs": "',
                   VA_OBS[match("01656000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01656000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01656000.png"}, "geometry": {"type": "Point", "coordinates": [-77.625278, 38.636667]}}, {"type": "Feature", "properties": {"name": "S F QUANTICO CREEK NEAR INDEPENDENT HILL", "id": "01658500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01658500", "obs": "',
                   VA_OBS[match("01658500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01658500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01658500.png"}, "geometry": {"type": "Point", "coordinates": [-77.428889, 38.587222]}}, {"type": "Feature", "properties": {"name": "AQUIA CREEK NEAR GARRISONVILLE", "id": "01660400", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01660400", "obs": "',
                   VA_OBS[match("01660400", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01660400", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01660400.png"}, "geometry": {"type": "Point", "coordinates": [-77.433889, 38.490278]}}, {"type": "Feature", "properties": {"name": "RAPPAHANNOCK RIVER NEAR FREDERICKSBURG", "id": "01668000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01668000", "obs": "',
                   VA_OBS[match("01668000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01668000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01668000.png"}, "geometry": {"type": "Point", "coordinates": [-77.529444, 38.308333]}}, {"type": "Feature", "properties": {"name": "PO RIVER NEAR SPOTSYLVANIA", "id": "01673800", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01673800", "obs": "',
                   VA_OBS[match("01673800", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01673800", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01673800.png"}, "geometry": {"type": "Point", "coordinates": [-77.595, 38.171389]}}, {"type": "Feature", "properties": {"name": "RAPIDAN RIVER NEAR CULPEPER", "id": "01667500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01667500", "obs": "',
                   VA_OBS[match("01667500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01667500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01667500.png"}, "geometry": {"type": "Point", "coordinates": [-77.975278, 38.350278]}}, {"type": "Feature", "properties": {"name": "ROBINSON RIVER NEAR LOCUST DALE", "id": "01666500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01666500", "obs": "',
                   VA_OBS[match("01666500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01666500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01666500.png"}, "geometry": {"type": "Point", "coordinates": [-78.095833, 38.325]}}, {"type": "Feature", "properties": {"name": "RAPIDAN RIVER NEAR RUCKERSVILLE", "id": "01665500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01665500", "obs": "',
                   VA_OBS[match("01665500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01665500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01665500.png"}, "geometry": {"type": "Point", "coordinates": [-78.340278, 38.280556]}}, {"type": "Feature", "properties": {"name": "S F SHENANDOAH RIVER NEAR LYNNWOOD", "id": "01628500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01628500", "obs": "',
                   VA_OBS[match("01628500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01628500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01628500.png"}, "geometry": {"type": "Point", "coordinates": [-78.755, 38.3225]}}, {"type": "Feature", "properties": {"name": "LINVILLE CREEK AT BROADWAY", "id": "01632082", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01632082", "obs": "',
                   VA_OBS[match("01632082", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01632082", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01632082.png"}, "geometry": {"type": "Point", "coordinates": [-78.803611, 38.606667]}}, {"type": "Feature", "properties": {"name": "N F SHENANDOAH RIVER AT COOTES STORE", "id": "01632000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01632000", "obs": "',
                   VA_OBS[match("01632000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01632000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01632000.png"}, "geometry": {"type": "Point", "coordinates": [-78.853056, 38.636944]}}, {"type": "Feature", "properties": {"name": "NORTH RIVER NEAR BURKETOWN", "id": "01622000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01622000", "obs": "',
                   VA_OBS[match("01622000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01622000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01622000.png"}, "geometry": {"type": "Point", "coordinates": [-78.913889, 38.340278]}}, {"type": "Feature", "properties": {"name": "NORTH RIVER NEAR STOKESVILLE", "id": "01620500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01620500", "obs": "',
                   VA_OBS[match("01620500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01620500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01620500.png"}, "geometry": {"type": "Point", "coordinates": [-79.239167, 38.335]}}, {"type": "Feature", "properties": {"name": "SOUTH RIVER AT HARRISTON", "id": "01627500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01627500", "obs": "',
                   VA_OBS[match("01627500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01627500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01627500.png"}, "geometry": {"type": "Point", "coordinates": [-78.836944, 38.218611]}}, {"type": "Feature", "properties": {"name": "MIDDLE RIVER NEAR GROTTOES", "id": "01625000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01625000", "obs": "',
                   VA_OBS[match("01625000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01625000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01625000.png"}, "geometry": {"type": "Point", "coordinates": [-78.862222, 38.261667]}}, {"type": "Feature", "properties": {"name": "SOUTH RIVER NEAR DOOMS", "id": "01626850", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01626850", "obs": "',
                   VA_OBS[match("01626850", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01626850", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01626850.png"}, "geometry": {"type": "Point", "coordinates": [-78.877222, 38.088611]}}, {"type": "Feature", "properties": {"name": "SOUTH RIVER NEAR WAYNESBORO", "id": "01626000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01626000", "obs": "',
                   VA_OBS[match("01626000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01626000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01626000.png"}, "geometry": {"type": "Point", "coordinates": [-78.908333, 38.0575]}}, {"type": "Feature", "properties": {"name": "MECHUMS RIVER NEAR WHITE HALL", "id": "02031000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02031000", "obs": "',
                   VA_OBS[match("02031000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02031000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02031000.png"}, "geometry": {"type": "Point", "coordinates": [-78.593056, 38.1025]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER AT SCOTTSVILLE", "id": "02029000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02029000", "obs": "',
                   VA_OBS[match("02029000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02029000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02029000.png"}, "geometry": {"type": "Point", "coordinates": [-78.491667, 37.797222]}}, {"type": "Feature", "properties": {"name": "HARDWARE RIVER BL BRIERY RUN NR SCOTTSVILLE", "id": "02030000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02030000", "obs": "',
                   VA_OBS[match("02030000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02030000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02030000.png"}, "geometry": {"type": "Point", "coordinates": [-78.455556, 37.8125]}}, {"type": "Feature", "properties": {"name": "RIVANNA RIVER AT PALMYRA", "id": "02034000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02034000", "obs": "',
                   VA_OBS[match("02034000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02034000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02034000.png"}, "geometry": {"type": "Point", "coordinates": [-78.266111, 37.857778]}}, {"type": "Feature", "properties": {"name": "MATTAPONI RIVER NEAR BOWLING GREEN", "id": "01674000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01674000", "obs": "',
                   VA_OBS[match("01674000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01674000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01674000.png"}, "geometry": {"type": "Point", "coordinates": [-77.386111, 38.061667]}}, {"type": "Feature", "properties": {"name": "LITTLE RIVER NEAR DOSWELL", "id": "01671100", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01671100", "obs": "',
                   VA_OBS[match("01671100", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01671100", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01671100.png"}, "geometry": {"type": "Point", "coordinates": [-77.513333, 37.8725]}}, {"type": "Feature", "properties": {"name": "SOUTH ANNA RIVER NEAR ASHLAND", "id": "01672500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01672500", "obs": "',
                   VA_OBS[match("01672500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01672500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01672500.png"}, "geometry": {"type": "Point", "coordinates": [-77.549167, 37.796667]}}, {"type": "Feature", "properties": {"name": "NORTH ANNA RIVER AT HART CORNER NEAR DOSWELL", "id": "01671020", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01671020", "obs": "',
                   VA_OBS[match("01671020", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01671020", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01671020.png"}, "geometry": {"type": "Point", "coordinates": [-77.428056, 37.85]}}, {"type": "Feature", "properties": {"name": "PAMUNKEY RIVER NEAR HANOVER", "id": "01673000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01673000", "obs": "',
                   VA_OBS[match("01673000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01673000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01673000.png"}, "geometry": {"type": "Point", "coordinates": [-77.3325, 37.7675]}}, {"type": "Feature", "properties": {"name": "TOTOPOTOMOY CREEK NEAR STUDLEY", "id": "01673550", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01673550", "obs": "',
                   VA_OBS[match("01673550", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01673550", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01673550.png"}, "geometry": {"type": "Point", "coordinates": [-77.258056, 37.6625]}}, {"type": "Feature", "properties": {"name": "MATTAPONI RIVER NEAR BEULAHVILLE", "id": "01674500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01674500", "obs": "',
                   VA_OBS[match("01674500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01674500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01674500.png"}, "geometry": {"type": "Point", "coordinates": [-77.165278, 37.883889]}}, {"type": "Feature", "properties": {"name": "PISCATAWAY CREEK NEAR TAPPAHANNOCK", "id": "01669000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01669000", "obs": "',
                   VA_OBS[match("01669000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01669000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01669000.png"}, "geometry": {"type": "Point", "coordinates": [-76.900833, 37.876944]}}, {"type": "Feature", "properties": {"name": "DRAGON SWAMP AT MASCOT", "id": "01669520", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=01669520", "obs": "',
                   VA_OBS[match("01669520", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("01669520", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01669520.png"}, "geometry": {"type": "Point", "coordinates": [-76.696667, 37.633611]}}, {"type": "Feature", "properties": {"name": "CHICKAHOMINY RIVER NEAR PROVIDENCE FORGE", "id": "02042500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02042500", "obs": "',
                   VA_OBS[match("02042500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02042500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02042500.png"}, "geometry": {"type": "Point", "coordinates": [-77.061111, 37.436111]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER NEAR RICHMOND", "id": "02037500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02037500", "obs": "',
                   VA_OBS[match("02037500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02037500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02037500.png"}, "geometry": {"type": "Point", "coordinates": [-77.547222, 37.563056]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER AND KANAWHA CANAL NEAR RICHMOND", "id": "02037000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02037000", "obs": "',
                   VA_OBS[match("02037000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02037000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02037000.png"}, "geometry": {"type": "Point", "coordinates": [-77.574444, 37.564444]}}, {"type": "Feature", "properties": {"name": "APPOMATTOX RIVER AT MATOACA", "id": "02041650", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02041650", "obs": "',
                   VA_OBS[match("02041650", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02041650", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02041650.png"}, "geometry": {"type": "Point", "coordinates": [-77.475556, 37.225]}}, {"type": "Feature", "properties": {"name": "DEEP CREEK NEAR MANNBORO", "id": "02041000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02041000", "obs": "',
                   VA_OBS[match("02041000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02041000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02041000.png"}, "geometry": {"type": "Point", "coordinates": [-77.87, 37.283056]}}, {"type": "Feature", "properties": {"name": "APPOMATTOX RIVER AT MATTOAX", "id": "02040000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02040000", "obs": "',
                   VA_OBS[match("02040000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02040000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02040000.png"}, "geometry": {"type": "Point", "coordinates": [-77.859167, 37.421389]}}, {"type": "Feature", "properties": {"name": "FINE CREEK AT FINE CREEK MILLS", "id": "02036500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02036500", "obs": "',
                   VA_OBS[match("02036500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02036500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02036500.png"}, "geometry": {"type": "Point", "coordinates": [-77.82, 37.597778]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER AT CARTERSVILLE", "id": "02035000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02035000", "obs": "',
                   VA_OBS[match("02035000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02035000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02035000.png"}, "geometry": {"type": "Point", "coordinates": [-78.086111, 37.670833]}}, {"type": "Feature", "properties": {"name": "APPOMATTOX RIVER AT FARMVILLE", "id": "02039500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02039500", "obs": "',
                   VA_OBS[match("02039500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02039500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02039500.png"}, "geometry": {"type": "Point", "coordinates": [-78.388889, 37.306944]}}, {"type": "Feature", "properties": {"name": "SLATE RIVER NEAR ARVONIA", "id": "02030500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02030500", "obs": "',
                   VA_OBS[match("02030500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02030500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02030500.png"}, "geometry": {"type": "Point", "coordinates": [-78.377778, 37.702778]}}, {"type": "Feature", "properties": {"name": "BUFFALO CREEK NEAR HAMPDEN SYDNEY", "id": "02039000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02039000", "obs": "',
                   VA_OBS[match("02039000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02039000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02039000.png"}, "geometry": {"type": "Point", "coordinates": [-78.486667, 37.256944]}}, {"type": "Feature", "properties": {"name": "HOLIDAY CREEK NEAR ANDERSONVILLE", "id": "02038850", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02038850", "obs": "',
                   VA_OBS[match("02038850", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02038850", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02038850.png"}, "geometry": {"type": "Point", "coordinates": [-78.636111, 37.415278]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER AT BENT CREEK", "id": "02026000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02026000", "obs": "',
                   VA_OBS[match("02026000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02026000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02026000.png"}, "geometry": {"type": "Point", "coordinates": [-78.829722, 37.536111]}}, {"type": "Feature", "properties": {"name": "ROCKFISH RIVER NEAR GREENFIELD", "id": "02028500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02028500", "obs": "',
                   VA_OBS[match("02028500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02028500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02028500.png"}, "geometry": {"type": "Point", "coordinates": [-78.823611, 37.869444]}}, {"type": "Feature", "properties": {"name": "TYE RIVER NEAR LOVINGSTON", "id": "02027000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02027000", "obs": "',
                   VA_OBS[match("02027000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02027000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02027000.png"}, "geometry": {"type": "Point", "coordinates": [-78.981944, 37.715278]}}, {"type": "Feature", "properties": {"name": "PINEY RIVER AT PINEY RIVER", "id": "02027500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02027500", "obs": "',
                   VA_OBS[match("02027500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02027500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02027500.png"}, "geometry": {"type": "Point", "coordinates": [-79.027778, 37.702222]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER AT HOLCOMB ROCK", "id": "02025500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02025500", "obs": "',
                   VA_OBS[match("02025500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02025500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02025500.png"}, "geometry": {"type": "Point", "coordinates": [-79.262778, 37.501111]}}, {"type": "Feature", "properties": {"name": "MAURY RIVER NEAR BUENA VISTA", "id": "02024000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02024000", "obs": "',
                   VA_OBS[match("02024000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02024000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02024000.png"}, "geometry": {"type": "Point", "coordinates": [-79.391667, 37.7625]}}, {"type": "Feature", "properties": {"name": "KERRS CREEK NEAR LEXINGTON", "id": "02022500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02022500", "obs": "',
                   VA_OBS[match("02022500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02022500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02022500.png"}, "geometry": {"type": "Point", "coordinates": [-79.443333, 37.825556]}}, {"type": "Feature", "properties": {"name": "MAURY RIVER AT ROCKBRIDGE BATHS", "id": "02021500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02021500", "obs": "',
                   VA_OBS[match("02021500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02021500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02021500.png"}, "geometry": {"type": "Point", "coordinates": [-79.422222, 37.907222]}}, {"type": "Feature", "properties": {"name": "CALFPASTURE RIVER ABOVE MILL CREEK AT GOSHEN", "id": "02020500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02020500", "obs": "',
                   VA_OBS[match("02020500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02020500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02020500.png"}, "geometry": {"type": "Point", "coordinates": [-79.493889, 37.987778]}}, {"type": "Feature", "properties": {"name": "BULLPASTURE RIVER AT WILLIAMSVILLE", "id": "02015700", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02015700", "obs": "',
                   VA_OBS[match("02015700", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02015700", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02015700.png"}, "geometry": {"type": "Point", "coordinates": [-79.570556, 38.195278]}}, {"type": "Feature", "properties": {"name": "BACK CREEK NEAR SUNRISE", "id": "02011460", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02011460", "obs": "',
                   VA_OBS[match("02011460", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02011460", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02011460.png"}, "geometry": {"type": "Point", "coordinates": [-79.768889, 38.245278]}}, {"type": "Feature", "properties": {"name": "LITTLE BACK CREEK NEAR SUNRISE", "id": "02011490", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02011490", "obs": "',
                   VA_OBS[match("02011490", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02011490", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02011490.png"}, "geometry": {"type": "Point", "coordinates": [-79.837778, 38.214444]}}, {"type": "Feature", "properties": {"name": "BACK CREEK AT SUNRISE", "id": "02011470", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02011470", "obs": "',
                   VA_OBS[match("02011470", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02011470", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02011470.png"}, "geometry": {"type": "Point", "coordinates": [-79.811944, 38.190278]}}, {"type": "Feature", "properties": {"name": "BACK CREEK NEAR MOUNTAIN GROVE", "id": "02011500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02011500", "obs": "',
                   VA_OBS[match("02011500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02011500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02011500.png"}, "geometry": {"type": "Point", "coordinates": [-79.897222, 38.069444]}}, {"type": "Feature", "properties": {"name": "JACKSON RIVER NEAR BACOVA", "id": "02011400", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02011400", "obs": "',
                   VA_OBS[match("02011400", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02011400", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02011400.png"}, "geometry": {"type": "Point", "coordinates": [-79.881667, 38.042222]}}, {"type": "Feature", "properties": {"name": "JACKSON RIVER BL GATHRIGHT DAM NR HOT SPGS", "id": "02011800", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02011800", "obs": "',
                   VA_OBS[match("02011800", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02011800", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02011800.png"}, "geometry": {"type": "Point", "coordinates": [-79.949444, 37.948333]}}, {"type": "Feature", "properties": {"name": "DUNLAP CREEK NEAR COVINGTON", "id": "02013000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02013000", "obs": "',
                   VA_OBS[match("02013000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02013000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02013000.png"}, "geometry": {"type": "Point", "coordinates": [-80.047222, 37.802778]}}, {"type": "Feature", "properties": {"name": "JACKSON RIVER BL DUNLAP CREEK AT COVINGTON", "id": "02013100", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02013100", "obs": "',
                   VA_OBS[match("02013100", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02013100", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02013100.png"}, "geometry": {"type": "Point", "coordinates": [-80.000833, 37.788611]}}, {"type": "Feature", "properties": {"name": "POTTS CREEK NEAR COVINGTON", "id": "02014000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02014000", "obs": "',
                   VA_OBS[match("02014000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02014000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02014000.png"}, "geometry": {"type": "Point", "coordinates": [-80.0425, 37.728889]}}, {"type": "Feature", "properties": {"name": "COWPASTURE RIVER NEAR CLIFTON FORGE", "id": "02016000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02016000", "obs": "',
                   VA_OBS[match("02016000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02016000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02016000.png"}, "geometry": {"type": "Point", "coordinates": [-79.759722, 37.791667]}}, {"type": "Feature", "properties": {"name": "CRAIG CREEK AT PARR", "id": "02018000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02018000", "obs": "',
                   VA_OBS[match("02018000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02018000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02018000.png"}, "geometry": {"type": "Point", "coordinates": [-79.911667, 37.665833]}}, {"type": "Feature", "properties": {"name": "JOHNS CREEK AT NEW CASTLE", "id": "02017500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02017500", "obs": "',
                   VA_OBS[match("02017500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02017500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02017500.png"}, "geometry": {"type": "Point", "coordinates": [-80.106944, 37.506111]}}, {"type": "Feature", "properties": {"name": "TINKER CREEK NEAR DALEVILLE", "id": "02055100", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02055100", "obs": "',
                   VA_OBS[match("02055100", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02055100", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02055100.png"}, "geometry": {"type": "Point", "coordinates": [-79.935556, 37.4175]}}, {"type": "Feature", "properties": {"name": "JAMES RIVER AT BUCHANAN", "id": "02019500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02019500", "obs": "',
                   VA_OBS[match("02019500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02019500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02019500.png"}, "geometry": {"type": "Point", "coordinates": [-79.679167, 37.530556]}}, {"type": "Feature", "properties": {"name": "ROANOKE RIVER AT ROANOKE", "id": "02055000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02055000", "obs": "',
                   VA_OBS[match("02055000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02055000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02055000.png"}, "geometry": {"type": "Point", "coordinates": [-79.938889, 37.258333]}}, {"type": "Feature", "properties": {"name": "GOOSE CREEK NEAR HUDDLESTON", "id": "02059500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02059500", "obs": "',
                   VA_OBS[match("02059500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02059500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02059500.png"}, "geometry": {"type": "Point", "coordinates": [-79.520556, 37.173056]}}, {"type": "Feature", "properties": {"name": "BIG OTTER RIVER NEAR EVINGTON", "id": "02061500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02061500", "obs": "',
                   VA_OBS[match("02061500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02061500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02061500.png"}, "geometry": {"type": "Point", "coordinates": [-79.303889, 37.208333]}}, {"type": "Feature", "properties": {"name": "FALLING RIVER NEAR NARUNA", "id": "02064000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02064000", "obs": "',
                   VA_OBS[match("02064000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02064000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02064000.png"}, "geometry": {"type": "Point", "coordinates": [-78.96, 37.126667]}}, {"type": "Feature", "properties": {"name": "ROANOKE (STAUNTON) RIVER AT BROOKNEAL", "id": "02062500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02062500", "obs": "',
                   VA_OBS[match("02062500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02062500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02062500.png"}, "geometry": {"type": "Point", "coordinates": [-78.945722, 37.039444]}}, {"type": "Feature", "properties": {"name": "DAN RIVER AT PACES", "id": "02075500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02075500", "obs": "',
                   VA_OBS[match("02075500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02075500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02075500.png"}, "geometry": {"type": "Point", "coordinates": [-79.089722, 36.642222]}}, {"type": "Feature", "properties": {"name": "BANISTER RIVER AT HALIFAX", "id": "02077000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02077000", "obs": "',
                   VA_OBS[match("02077000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02077000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02077000.png"}, "geometry": {"type": "Point", "coordinates": [-78.916111, 36.776389]}}, {"type": "Feature", "properties": {"name": "ROANOKE (STAUNTON) RIVER AT RANDOLPH", "id": "02066000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02066000", "obs": "',
                   VA_OBS[match("02066000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02066000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02066000.png"}, "geometry": {"type": "Point", "coordinates": [-78.741111, 36.915]}}, {"type": "Feature", "properties": {"name": "ALLEN CREEK NEAR BOYDTON", "id": "02079640", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02079640", "obs": "',
                   VA_OBS[match("02079640", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02079640", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02079640.png"}, "geometry": {"type": "Point", "coordinates": [-78.326944, 36.679444]}}, {"type": "Feature", "properties": {"name": "MEHERRIN RIVER NEAR LAWRENCEVILLE", "id": "02051500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02051500", "obs": "',
                   VA_OBS[match("02051500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02051500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02051500.png"}, "geometry": {"type": "Point", "coordinates": [-77.831944, 36.716667]}}, {"type": "Feature", "properties": {"name": "NOTTOWAY RIVER NEAR RAWLINGS", "id": "02044500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02044500", "obs": "',
                   VA_OBS[match("02044500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02044500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02044500.png"}, "geometry": {"type": "Point", "coordinates": [-77.8, 36.983333]}}, {"type": "Feature", "properties": {"name": "STONY CREEK NEAR DINWIDDIE", "id": "02046000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02046000", "obs": "',
                   VA_OBS[match("02046000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02046000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02046000.png"}, "geometry": {"type": "Point", "coordinates": [-77.602778, 37.066944]}}, {"type": "Feature", "properties": {"name": "MEHERRIN RIVER AT EMPORIA", "id": "02052000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02052000", "obs": "',
                   VA_OBS[match("02052000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02052000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02052000.png"}, "geometry": {"type": "Point", "coordinates": [-77.540833, 36.69]}}, {"type": "Feature", "properties": {"name": "NOTTOWAY RIVER NEAR STONY CREEK", "id": "02045500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02045500", "obs": "',
                   VA_OBS[match("02045500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02045500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02045500.png"}, "geometry": {"type": "Point", "coordinates": [-77.4, 36.9]}}, {"type": "Feature", "properties": {"name": "NOTTOWAY RIVER NEAR SEBRELL", "id": "02047000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02047000", "obs": "',
                   VA_OBS[match("02047000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02047000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02047000.png"}, "geometry": {"type": "Point", "coordinates": [-77.166389, 36.770278]}}, {"type": "Feature", "properties": {"name": "BLACKWATER RIVER NEAR DENDRON", "id": "02047500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02047500", "obs": "',
                   VA_OBS[match("02047500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02047500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02047500.png"}, "geometry": {"type": "Point", "coordinates": [-76.875, 37.025]}}, {"type": "Feature", "properties": {"name": "BLACKWATER RIVER NEAR FRANKLIN", "id": "02049500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02049500", "obs": "',
                   VA_OBS[match("02049500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02049500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02049500.png"}, "geometry": {"type": "Point", "coordinates": [-76.898611, 36.7625]}}, {"type": "Feature", "properties": {"name": "ROANOKE RIVER AT ALTAVISTA", "id": "02060500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02060500", "obs": "',
                   VA_OBS[match("02060500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02060500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02060500.png"}, "geometry": {"type": "Point", "coordinates": [-79.295556, 37.104444]}}, {"type": "Feature", "properties": {"name": "PIGG RIVER NEAR SANDY LEVEL", "id": "02058400", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02058400", "obs": "',
                   VA_OBS[match("02058400", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02058400", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02058400.png"}, "geometry": {"type": "Point", "coordinates": [-79.525, 36.945833]}}, {"type": "Feature", "properties": {"name": "BLACKWATER RIVER NEAR ROCKY MOUNT", "id": "02056900", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02056900", "obs": "',
                   VA_OBS[match("02056900", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02056900", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02056900.png"}, "geometry": {"type": "Point", "coordinates": [-79.844444, 37.045]}}, {"type": "Feature", "properties": {"name": "SMITH RIVER AT MARTINSVILLE", "id": "02073000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02073000", "obs": "',
                   VA_OBS[match("02073000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02073000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02073000.png"}, "geometry": {"type": "Point", "coordinates": [-79.880833, 36.661111]}}, {"type": "Feature", "properties": {"name": "SMITH RIVER NEAR PHILPOTT", "id": "02072000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02072000", "obs": "',
                   VA_OBS[match("02072000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02072000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02072000.png"}, "geometry": {"type": "Point", "coordinates": [-80.025, 36.780556]}}, {"type": "Feature", "properties": {"name": "SMITH RIVER AT BASSETT", "id": "02072500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02072500", "obs": "',
                   VA_OBS[match("02072500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02072500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02072500.png"}, "geometry": {"type": "Point", "coordinates": [-80.001111, 36.77]}}, {"type": "Feature", "properties": {"name": "ROANOKE RIVER AT LAFAYETTE", "id": "02054500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=02054500", "obs": "',
                   VA_OBS[match("02054500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("02054500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02054500.png"}, "geometry": {"type": "Point", "coordinates": [-80.209444, 37.236389]}}, {"type": "Feature", "properties": {"name": "LITTLE RIVER AT GRAYSONTOWN", "id": "03170000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03170000", "obs": "',
                   VA_OBS[match("03170000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03170000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03170000.png"}, "geometry": {"type": "Point", "coordinates": [-80.556944, 37.0375]}}, {"type": "Feature", "properties": {"name": "NEW RIVER AT RADFORD", "id": "03171000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03171000", "obs": "',
                   VA_OBS[match("03171000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03171000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03171000.png"}, "geometry": {"type": "Point", "coordinates": [-80.569444, 37.141667]}}, {"type": "Feature", "properties": {"name": "WALKER CREEK AT BANE", "id": "03173000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=03173000", "obs": "',
                   VA_OBS[match("03173000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03173000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03173000.png"}, "geometry": {"type": "Point", "coordinates": [-80.709722, 37.268056]}}, {"type": "Feature", "properties": {"name": "WOLF CREEK NEAR NARROWS", "id": "03175500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03175500", "obs": "',
                   VA_OBS[match("03175500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03175500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03175500.png"}, "geometry": {"type": "Point", "coordinates": [-80.85, 37.305556]}}, {"type": "Feature", "properties": {"name": "NEW RIVER AT GLEN LYN", "id": "03176500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03176500", "obs": "',
                   VA_OBS[match("03176500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03176500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03176500.png"}, "geometry": {"type": "Point", "coordinates": [-80.860833, 37.372778]}}, {"type": "Feature", "properties": {"name": "NEW RIVER AT ALLISONIA", "id": "03168000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03168000", "obs": "',
                   VA_OBS[match("03168000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03168000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03168000.png"}, "geometry": {"type": "Point", "coordinates": [-80.745833, 36.9375]}}, {"type": "Feature", "properties": {"name": "REED CREEK AT GRAHAMS FORGE", "id": "03167000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03167000", "obs": "',
                   VA_OBS[match("03167000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03167000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03167000.png"}, "geometry": {"type": "Point", "coordinates": [-80.8875, 36.938889]}}, {"type": "Feature", "properties": {"name": "NEW RIVER AT IVANHOE", "id": "03165500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03165500", "obs": "',
                   VA_OBS[match("03165500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03165500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03165500.png"}, "geometry": {"type": "Point", "coordinates": [-80.952778, 36.834722]}}, {"type": "Feature", "properties": {"name": "CHESTNUT CREEK AT GALAX", "id": "03165000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03165000", "obs": "',
                   VA_OBS[match("03165000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03165000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03165000.png"}, "geometry": {"type": "Point", "coordinates": [-80.919444, 36.645833]}}, {"type": "Feature", "properties": {"name": "NEW RIVER NEAR GALAX", "id": "03164000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=03164000", "obs": "',
                   VA_OBS[match("03164000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03164000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03164000.png"}, "geometry": {"type": "Point", "coordinates": [-80.979167, 36.647222]}}, {"type": "Feature", "properties": {"name": "M F HOLSTON RIVER AT SEVEN MILE FORD", "id": "03474000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03474000", "obs": "',
                   VA_OBS[match("03474000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03474000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03474000.png"}, "geometry": {"type": "Point", "coordinates": [-81.622222, 36.807222]}}, {"type": "Feature", "properties": {"name": "S F HOLSTON RIVER AT RIVERSIDE, NEAR CHILHOWIE", "id": "03471500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03471500", "obs": "',
                   VA_OBS[match("03471500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03471500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03471500.png"}, "geometry": {"type": "Point", "coordinates": [-81.631389, 36.760278]}}, {"type": "Feature", "properties": {"name": "N F HOLSTON RIVER NEAR SALTVILLE", "id": "03488000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03488000", "obs": "',
                   VA_OBS[match("03488000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03488000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03488000.png"}, "geometry": {"type": "Point", "coordinates": [-81.746389, 36.896667]}}, {"type": "Feature", "properties": {"name": "M F HOLSTON RIVER NEAR MEADOWVIEW", "id": "03475000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03475000", "obs": "',
                   VA_OBS[match("03475000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03475000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03475000.png"}, "geometry": {"type": "Point", "coordinates": [-81.818889, 36.713056]}}, {"type": "Feature", "properties": {"name": "S F HOLSTON RIVER NEAR DAMASCUS", "id": "03473000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03473000", "obs": "',
                   VA_OBS[match("03473000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03473000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03473000.png"}, "geometry": {"type": "Point", "coordinates": [-81.844167, 36.651667]}}, {"type": "Feature", "properties": {"name": "BEAVER CREEK AT BRISTOL", "id": "03478400", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03478400", "obs": "',
                   VA_OBS[match("03478400", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03478400", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03478400.png"}, "geometry": {"type": "Point", "coordinates": [-82.133889, 36.631667]}}, {"type": "Feature", "properties": {"name": "CLINCH RIVER AT CLEVELAND", "id": "03524000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03524000", "obs": "',
                   VA_OBS[match("03524000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03524000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03524000.png"}, "geometry": {"type": "Point", "coordinates": [-82.155, 36.944722]}}, {"type": "Feature", "properties": {"name": "LEVISA FORK AT BIG ROCK", "id": "03207800", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03207800", "obs": "',
                   VA_OBS[match("03207800", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03207800", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03207800.png"}, "geometry": {"type": "Point", "coordinates": [-82.195833, 37.353611]}}, {"type": "Feature", "properties": {"name": "RUSSELL FORK AT HAYSI", "id": "03208500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03208500", "obs": "',
                   VA_OBS[match("03208500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03208500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03208500.png"}, "geometry": {"type": "Point", "coordinates": [-82.295833, 37.206944]}}, {"type": "Feature", "properties": {"name": "POUND RIVER BELOW FLANNAGAN DAM, NEAR HAYSI", "id": "03209000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03209000", "obs": "',
                   VA_OBS[match("03209000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03209000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03209000.png"}, "geometry": {"type": "Point", "coordinates": [-82.343333, 37.236944]}}, {"type": "Feature", "properties": {"name": "CRANES NEST RIVER NEAR CLINTWOOD", "id": "03208950", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03208950", "obs": "',
                   VA_OBS[match("03208950", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03208950", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03208950.png"}, "geometry": {"type": "Point", "coordinates": [-82.438889, 37.123889]}}, {"type": "Feature", "properties": {"name": "N F HOLSTON RIVER NEAR GATE CITY", "id": "03490000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03490000", "obs": "',
                   VA_OBS[match("03490000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03490000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03490000.png"}, "geometry": {"type": "Point", "coordinates": [-82.568056, 36.608611]}}, {"type": "Feature", "properties": {"name": "CLINCH RIVER AT SPEERS FERRY", "id": "03527000", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03527000", "obs": "',
                   VA_OBS[match("03527000", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03527000", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03527000.png"}, "geometry": {"type": "Point", "coordinates": [-82.750556, 36.648611]}}, {"type": "Feature", "properties": {"name": "POWELL RIVER AT BIG STONE GAP", "id": "03529500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03529500", "obs": "',
                   VA_OBS[match("03529500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03529500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03529500.png"}, "geometry": {"type": "Point", "coordinates": [-82.775556, 36.868889]}}, {"type": "Feature", "properties": {"name": "POWELL RIVER NEAR JONESVILLE", "id": "03531500", "url": "https://waterdata.usgs.gov/va/nwis/inventory/?site_no=03531500", "obs": "',
                   VA_OBS[match("03531500", VA_OBS[,1]), 2], '", "time": "', VA_OBS[match("03531500", VA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03531500.png"}, "geometry": {"type": "Point", "coordinates": [-83.095, 36.661944]}}]};', sep="")

# Export data to geojson.
cat(json_merge, file="/net/www/www.marisa.psu.edu/htdocs/mapdata/VA_stream_obs.js")
# --------------------------------------------------------------------------------------------------------------------
