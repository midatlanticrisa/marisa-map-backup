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
if (!require(“RCurl”)) { install.packages(“RCurl”) }
if (!require(“readr”)) { install.packages(“readr”) }

library(RCurl)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists("/home/staff/klr324/marisa.psu.edu/mapdata")){
  dir.create("/home/staff/klr324/marisa.psu.edu/mapdata")
}

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
# <!-- New York -->
NY_ID = c("03014500", "04213500", "03011020", "04215500", "04214500", "04215000", "04218518", "04218000", "04221000",
          "04223000", "04230380", "04216418", "04217000", "04231000", "04230500", "04231600", "04232050", "04229500",
          "04228500", "04227500", "04224775", "01524500", "01529500", "01526500", "01529950", "04232482", "04235000",
          "04235250", "01531000", "01515000", "04234000", "04249000", "04239000", "04240010", "04240300", "04240120",
          "04240105", "01509000", "01510000", "01513500", "01512500", "01503000", "01426500", "01425000", "01507000",
          "01502500", "01505000", "04243500", "04242500", "01336000", "04258000", "04250750", "04260500", "04262500",
          "04262000", "04267500", "04268000", "04266500", "04273500", "01312000", "01315000", "01321000", "01315500",
          "01334500", "01329490", "01327750", "01325000", "01330000", "01335754", "01357500", "01358000", "01351500",
          "01348000", "01346000", "01500500", "01500000", "0142400103", "01423000", "01417500", "01421000", "01420500",
          "01417000", "01421900", "01422500", "01415000", "01414500", "01414000", "01413500", "01350000", "01350140",
          "01350180", "01350355", "01361000", "01372500", "01364500", "01367500", "01371500", "01362500", "01362200",
          "01434025", "01365000", "01365500", "01435000", "01436000", "01427510", "01428500", "01433500", "01434000",
          "01437500", "01387420", "01387450", "01376800", "01375000", "01311500", "01309500", "01303500", "01308500",
          "01304000", "01306460", "01304500")

# Run through each New York station.
NY_TMP = mat.or.vec(length(NY_ID), 3)
NY_TMP[ ,1] = NY_ID; NY_DIS = NY_TMP; NY_GAG = NY_TMP
for(i in 1:length(NY_ID)){
  NY_TMP[i, 2:3] = usgs_dataRetrieveTemp(NY_ID[i], "00010", b.date, e.date, tz="America/New_York")
  NY_DIS[i, 2:3] = usgs_dataRetrieveTemp(NY_ID[i], "00060", b.date, e.date, tz="America/New_York")
  NY_GAG[i, 2:3] = usgs_dataRetrieveTemp(NY_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
NY_OBS = obs_string(NY_TMP, NY_DIS, NY_GAG)

# --------------------------------------------------------------------------------------------------------------------
# Create a geojson object with the observation and statement info and merge into a 
# specific file format with adding stream temp and time.
json_merge = paste('NY_streamGauges = {"type": "FeatureCollection","features": [{"type": "Feature", "properties": {"name": "CHADAKOIN RIVER AT FALCONER", "id": "03014500", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=03014500", "obs": "',
                   NY_OBS[match("03014500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("03014500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03014500.png"}, "geometry": {"type": "Point", "coordinates": [-79.203889, 42.1125]}}, {"type": "Feature", "properties": {"name": "CATTARAUGUS CREEK AT GOWANDA", "id": "04213500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04213500", "obs": "',
                   NY_OBS[match("04213500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04213500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04213500.png"}, "geometry": {"type": "Point", "coordinates": [-78.934167, 42.463333]}}, {"type": "Feature", "properties": {"name": "ALLEGHENY RIVER AT SALAMANCA", "id": "03011020", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=03011020", "obs": "',
                   NY_OBS[match("03011020", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("03011020", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03011020.png"}, "geometry": {"type": "Point", "coordinates": [-78.715278, 42.156389]}}, {"type": "Feature", "properties": {"name": "CAZENOVIA CREEK AT EBENEZER", "id": "04215500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04215500", "obs": "',
                   NY_OBS[match("04215500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04215500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04215500.png"}, "geometry": {"type": "Point", "coordinates": [-78.775, 42.829722]}}, {"type": "Feature", "properties": {"name": "BUFFALO CREEK AT GARDENVILLE", "id": "04214500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04214500", "obs": "',
                   NY_OBS[match("04214500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04214500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04214500.png"}, "geometry": {"type": "Point", "coordinates": [-78.755, 42.854722]}}, {"type": "Feature", "properties": {"name": "CAYUGA CREEK NEAR LANCASTER", "id": "04215000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04215000", "obs": "',
                   NY_OBS[match("04215000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04215000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04215000.png"}, "geometry": {"type": "Point", "coordinates": [-78.645, 42.89]}}, {"type": "Feature", "properties": {"name": "ELLICOTT CREEK BELOW WILLIAMSVILLE", "id": "04218518", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04218518", "obs": "',
                   NY_OBS[match("04218518", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04218518", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04218518.png"}, "geometry": {"type": "Point", "coordinates": [-78.763611, 42.977778]}}, {"type": "Feature", "properties": {"name": "TONAWANDA CREEK AT RAPIDS", "id": "04218000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04218000", "obs": "',
                   NY_OBS[match("04218000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04218000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04218000.png"}, "geometry": {"type": "Point", "coordinates": [-78.636111, 43.093056]}}, {"type": "Feature", "properties": {"name": "GENESEE RIVER AT WELLSVILLE", "id": "04221000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04221000", "obs": "',
                   NY_OBS[match("04221000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04221000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04221000.png"}, "geometry": {"type": "Point", "coordinates": [-77.957222, 42.122222]}}, {"type": "Feature", "properties": {"name": "GENESEE RIVER AT PORTAGEVILLE", "id": "04223000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04223000", "obs": "',
                   NY_OBS[match("04223000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04223000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04223000.png"}, "geometry": {"type": "Point", "coordinates": [-78.042222, 42.570278]}}, {"type": "Feature", "properties": {"name": "OATKA CREEK AT WARSAW", "id": "04230380", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04230380", "obs": "',
                   NY_OBS[match("04230380", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04230380", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04230380.png"}, "geometry": {"type": "Point", "coordinates": [-78.1375, 42.744167]}}, {"type": "Feature", "properties": {"name": "TONAWANDA CREEK AT ATTICA", "id": "04216418", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04216418", "obs": "',
                   NY_OBS[match("04216418", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04216418", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04216418.png"}, "geometry": {"type": "Point", "coordinates": [-78.283611, 42.863889]}}, {"type": "Feature", "properties": {"name": "TONAWANDA CREEK AT BATAVIA", "id": "04217000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04217000", "obs": "',
                   NY_OBS[match("04217000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04217000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04217000.png"}, "geometry": {"type": "Point", "coordinates": [-78.188611, 42.9975]}}, {"type": "Feature", "properties": {"name": "BLACK CREEK AT CHURCHVILLE", "id": "04231000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04231000", "obs": "',
                   NY_OBS[match("04231000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04231000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04231000.png"}, "geometry": {"type": "Point", "coordinates": [-77.882222, 43.100556]}}, {"type": "Feature", "properties": {"name": "OATKA CREEK AT GARBUTT", "id": "04230500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04230500", "obs": "',
                   NY_OBS[match("04230500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04230500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04230500.png"}, "geometry": {"type": "Point", "coordinates": [-77.791389, 43.01]}}, {"type": "Feature", "properties": {"name": "GENESEE RIVER AT FORD STREET BRIDGE, ROCHESTER", "id": "04231600", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04231600", "obs": "',
                   NY_OBS[match("04231600", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04231600", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04231600.png"}, "geometry": {"type": "Point", "coordinates": [-77.616306, 43.141722]}}, {"type": "Feature", "properties": {"name": "ALLEN CREEK NEAR ROCHESTER", "id": "04232050", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04232050", "obs": "',
                   NY_OBS[match("04232050", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04232050", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04232050.png"}, "geometry": {"type": "Point", "coordinates": [-77.518611, 43.130278]}}, {"type": "Feature", "properties": {"name": "HONEOYE CREEK AT HONEOYE FALLS", "id": "04229500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04229500", "obs": "',
                   NY_OBS[match("04229500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04229500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04229500.png"}, "geometry": {"type": "Point", "coordinates": [-77.588889, 42.957222]}}, {"type": "Feature", "properties": {"name": "GENESEE RIVER AT AVON", "id": "04228500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04228500", "obs": "',
                   NY_OBS[match("04228500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04228500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04228500.png"}, "geometry": {"type": "Point", "coordinates": [-77.757222, 42.917778]}}, {"type": "Feature", "properties": {"name": "GENESEE RIVER NEAR MOUNT MORRIS", "id": "04227500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04227500", "obs": "',
                   NY_OBS[match("04227500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04227500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04227500.png"}, "geometry": {"type": "Point", "coordinates": [-77.838889, 42.766667]}}, {"type": "Feature", "properties": {"name": "CANASERAGA CREEK ABOVE DANSVILLE", "id": "04224775", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04224775", "obs": "',
                   NY_OBS[match("04224775", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04224775", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04224775.png"}, "geometry": {"type": "Point", "coordinates": [-77.704167, 42.535556]}}, {"type": "Feature", "properties": {"name": "CANISTEO RIVER BELOW CANACADEA CREEK AT HORNELL", "id": "01524500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01524500", "obs": "',
                   NY_OBS[match("01524500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01524500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01524500.png"}, "geometry": {"type": "Point", "coordinates": [-77.651111, 42.313889]}}, {"type": "Feature", "properties": {"name": "COHOCTON RIVER NEAR CAMPBELL", "id": "01529500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01529500", "obs": "',
                   NY_OBS[match("01529500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01529500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01529500.png"}, "geometry": {"type": "Point", "coordinates": [-77.216667, 42.2525]}}, {"type": "Feature", "properties": {"name": "TIOGA RIVER NEAR ERWINS", "id": "01526500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01526500", "obs": "',
                   NY_OBS[match("01526500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01526500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01526500.png"}, "geometry": {"type": "Point", "coordinates": [-77.129167, 42.121111]}}, {"type": "Feature", "properties": {"name": "CHEMUNG RIVER AT CORNING", "id": "01529950", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01529950", "obs": "',
                   NY_OBS[match("01529950", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01529950", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01529950.png"}, "geometry": {"type": "Point", "coordinates": [-77.0575, 42.146389]}}, {"type": "Feature", "properties": {"name": "KEUKA LAKE OUTLET AT DRESDEN", "id": "04232482", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04232482", "obs": "',
                   NY_OBS[match("04232482", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04232482", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04232482.png"}, "geometry": {"type": "Point", "coordinates": [-76.953889, 42.680278]}}, {"type": "Feature", "properties": {"name": "CANANDAIGUA OUTLET AT CHAPIN", "id": "04235000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04235000", "obs": "',
                   NY_OBS[match("04235000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04235000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04235000.png"}, "geometry": {"type": "Point", "coordinates": [-77.232778, 42.918056]}}, {"type": "Feature", "properties": {"name": "FLINT CREEK AT PHELPS", "id": "04235250", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04235250", "obs": "',
                   NY_OBS[match("04235250", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04235250", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04235250.png"}, "geometry": {"type": "Point", "coordinates": [-77.068056, 42.957778]}}, {"type": "Feature", "properties": {"name": "CHEMUNG RIVER AT CHEMUNG", "id": "01531000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01531000", "obs": "',
                   NY_OBS[match("01531000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01531000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01531000.png"}, "geometry": {"type": "Point", "coordinates": [-76.634722, 42.002222]}}, {"type": "Feature", "properties": {"name": "SUSQUEHANNA RIVER NEAR WAVERLY", "id": "01515000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01515000", "obs": "',
                   NY_OBS[match("01515000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01515000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01515000.png"}, "geometry": {"type": "Point", "coordinates": [-76.501111, 41.984722]}}, {"type": "Feature", "properties": {"name": "FALL CREEK NEAR ITHACA", "id": "04234000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04234000", "obs": "',
                   NY_OBS[match("04234000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04234000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04234000.png"}, "geometry": {"type": "Point", "coordinates": [-76.472778, 42.453333]}}, {"type": "Feature", "properties": {"name": "OSWEGO RIVER AT LOCK 7, OSWEGO", "id": "04249000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04249000", "obs": "',
                   NY_OBS[match("04249000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04249000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04249000.png"}, "geometry": {"type": "Point", "coordinates": [-76.505278, 43.451667]}}, {"type": "Feature", "properties": {"name": "ONONDAGA CREEK AT DORWIN AVENUE, SYRACUSE", "id": "04239000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04239000", "obs": "',
                   NY_OBS[match("04239000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04239000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04239000.png"}, "geometry": {"type": "Point", "coordinates": [-76.150833, 42.983333]}}, {"type": "Feature", "properties": {"name": "ONONDAGA CREEK AT SPENCER STREET, SYRACUSE", "id": "04240010", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04240010", "obs": "',
                   NY_OBS[match("04240010", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04240010", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04240010.png"}, "geometry": {"type": "Point", "coordinates": [-76.1625, 43.0575]}}, {"type": "Feature", "properties": {"name": "NINEMILE CREEK AT LAKELAND", "id": "04240300", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04240300", "obs": "',
                   NY_OBS[match("04240300", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04240300", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04240300.png"}, "geometry": {"type": "Point", "coordinates": [-76.226389, 43.080833]}}, {"type": "Feature", "properties": {"name": "LEY CREEK AT PARK STREET, SYRACUSE", "id": "04240120", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04240120", "obs": "',
                   NY_OBS[match("04240120", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04240120", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04240120.png"}, "geometry": {"type": "Point", "coordinates": [-76.170278, 43.077222]}}, {"type": "Feature", "properties": {"name": "HARBOR BROOK AT HIAWATHA BOULEVARD, SYRACUSE", "id": "04240105", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04240105", "obs": "',
                   NY_OBS[match("04240105", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04240105", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04240105.png"}, "geometry": {"type": "Point", "coordinates": [-76.185, 43.056111]}}, {"type": "Feature", "properties": {"name": "TIOUGHNIOGA RIVER AT CORTLAND", "id": "01509000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01509000", "obs": "',
                   NY_OBS[match("01509000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01509000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01509000.png"}, "geometry": {"type": "Point", "coordinates": [-76.159444, 42.602778]}}, {"type": "Feature", "properties": {"name": "OTSELIC RIVER AT CINCINNATUS", "id": "01510000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01510000", "obs": "',
                   NY_OBS[match("01510000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01510000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01510000.png"}, "geometry": {"type": "Point", "coordinates": [-75.899722, 42.541111]}}, {"type": "Feature", "properties": {"name": "SUSQUEHANNA RIVER AT VESTAL", "id": "01513500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01513500", "obs": "',
                   NY_OBS[match("01513500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01513500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01513500.png"}, "geometry": {"type": "Point", "coordinates": [-76.056111, 42.090833]}}, {"type": "Feature", "properties": {"name": "CHENANGO RIVER NEAR CHENANGO FORKS", "id": "01512500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01512500", "obs": "',
                   NY_OBS[match("01512500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01512500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01512500.png"}, "geometry": {"type": "Point", "coordinates": [-75.848333, 42.218056]}}, {"type": "Feature", "properties": {"name": "SUSQUEHANNA RIVER AT CONKLIN", "id": "01503000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01503000", "obs": "',
                   NY_OBS[match("01503000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01503000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01503000.png"}, "geometry": {"type": "Point", "coordinates": [-75.803056, 42.035278]}}, {"type": "Feature", "properties": {"name": "WEST BRANCH DELAWARE RIVER AT HALE EDDY", "id": "01426500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01426500", "obs": "',
                   NY_OBS[match("01426500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01426500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01426500.png"}, "geometry": {"type": "Point", "coordinates": [-75.383611, 42.003056]}}, {"type": "Feature", "properties": {"name": "WEST BRANCH DELAWARE RIVER AT STILESVILLE", "id": "01425000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01425000", "obs": "',
                   NY_OBS[match("01425000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01425000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01425000.png"}, "geometry": {"type": "Point", "coordinates": [-75.396111, 42.074722]}}, {"type": "Feature", "properties": {"name": "CHENANGO RIVER AT GREENE", "id": "01507000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01507000", "obs": "',
                   NY_OBS[match("01507000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01507000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01507000.png"}, "geometry": {"type": "Point", "coordinates": [-75.771389, 42.324444]}}, {"type": "Feature", "properties": {"name": "UNADILLA RIVER AT ROCKDALE", "id": "01502500", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01502500", "obs": "',
                   NY_OBS[match("01502500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01502500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01502500.png"}, "geometry": {"type": "Point", "coordinates": [-75.406111, 42.377778]}}, {"type": "Feature", "properties": {"name": "CHENANGO RIVER AT SHERBURNE", "id": "01505000", "url": "http://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01505000", "obs": "',
                   NY_OBS[match("01505000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01505000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01505000.png"}, "geometry": {"type": "Point", "coordinates": [-75.510556, 42.678611]}}, {"type": "Feature", "properties": {"name": "ONEIDA CREEK AT ONEIDA", "id": "04243500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04243500", "obs": "',
                   NY_OBS[match("04243500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04243500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04243500.png"}, "geometry": {"type": "Point", "coordinates": [-75.639167, 43.0975]}}, {"type": "Feature", "properties": {"name": "EAST BRANCH FISH CREEK AT TABERG", "id": "04242500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=04242500", "obs": "',
                   NY_OBS[match("04242500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04242500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04242500.png"}, "geometry": {"type": "Point", "coordinates": [-75.621472, 43.301111]}}, {"type": "Feature", "properties": {"name": "MOHAWK RIVER BELOW DELTA DAM NEAR ROME", "id": "01336000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01336000", "obs": "',
                   NY_OBS[match("01336000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01336000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01336000.png"}, "geometry": {"type": "Point", "coordinates": [-75.436389, 43.264444]}}, {"type": "Feature", "properties": {"name": "BEAVER RIVER AT CROGHAN", "id": "04258000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04258000", "obs": "',
                   NY_OBS[match("04258000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04258000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04258000.png"}, "geometry": {"type": "Point", "coordinates": [-75.404167, 43.897222]}}, {"type": "Feature", "properties": {"name": "SANDY CREEK NEAR ADAMS", "id": "04250750", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04250750", "obs": "',
                   NY_OBS[match("04250750", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04250750", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04250750.png"}, "geometry": {"type": "Point", "coordinates": [-76.074722, 43.813333]}}, {"type": "Feature", "properties": {"name": "BLACK RIVER AT WATERTOWN", "id": "04260500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04260500", "obs": "',
                   NY_OBS[match("04260500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04260500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04260500.png"}, "geometry": {"type": "Point", "coordinates": [-75.924722, 43.985556]}}, {"type": "Feature", "properties": {"name": "WEST BRANCH OSWEGATCHIE RIVER NEAR HARRISVILLE", "id": "04262500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04262500", "obs": "',
                   NY_OBS[match("04262500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04262500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04262500.png"}, "geometry": {"type": "Point", "coordinates": [-75.330833, 44.185556]}}, {"type": "Feature", "properties": {"name": "OSWEGATCHIE RIVER NEAR OSWEGATCHIE", "id": "04262000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04262000", "obs": "',
                   NY_OBS[match("04262000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04262000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04262000.png"}, "geometry": {"type": "Point", "coordinates": [-75.074444, 44.2225]}}, {"type": "Feature", "properties": {"name": "RAQUETTE RIVER AT SOUTH COLTON", "id": "04267500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04267500", "obs": "',
                   NY_OBS[match("04267500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04267500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04267500.png"}, "geometry": {"type": "Point", "coordinates": [-74.883611, 44.509833]}}, {"type": "Feature", "properties": {"name": "RAQUETTE RIVER AT RAYMONDVILLE", "id": "04268000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04268000", "obs": "',
                   NY_OBS[match("04268000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04268000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04268000.png"}, "geometry": {"type": "Point", "coordinates": [-74.978889, 44.838889]}}, {"type": "Feature", "properties": {"name": "RAQUETTE RIVER AT PIERCEFIELD", "id": "04266500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04266500", "obs": "',
                   NY_OBS[match("04266500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04266500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04266500.png"}, "geometry": {"type": "Point", "coordinates": [-74.571944, 44.234722]}}, {"type": "Feature", "properties": {"name": "SARANAC RIVER AT PLATTSBURGH", "id": "04273500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=04273500", "obs": "',
                   NY_OBS[match("04273500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("04273500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04273500.png"}, "geometry": {"type": "Point", "coordinates": [-73.471111, 44.681667]}}, {"type": "Feature", "properties": {"name": "HUDSON RIVER NEAR NEWCOMB", "id": "01312000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01312000", "obs": "',
                   NY_OBS[match("01312000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01312000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01312000.png"}, "geometry": {"type": "Point", "coordinates": [-74.131667, 43.966667]}}, {"type": "Feature", "properties": {"name": "INDIAN RIVER NEAR INDIAN LAKE", "id": "01315000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01315000", "obs": "',
                   NY_OBS[match("01315000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01315000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01315000.png"}, "geometry": {"type": "Point", "coordinates": [-74.267222, 43.756389]}}, {"type": "Feature", "properties": {"name": "SACANDAGA RIVER NEAR HOPE", "id": "01321000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01321000", "obs": "',
                   NY_OBS[match("01321000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01321000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01321000.png"}, "geometry": {"type": "Point", "coordinates": [-74.270278, 43.352778]}}, {"type": "Feature", "properties": {"name": "HUDSON RIVER AT NORTH CREEK", "id": "01315500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01315500", "obs": "',
                   NY_OBS[match("01315500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01315500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01315500.png"}, "geometry": {"type": "Point", "coordinates": [-73.983333, 43.700833]}}, {"type": "Feature", "properties": {"name": "HOOSIC RIVER NEAR EAGLE BRIDGE", "id": "01334500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01334500", "obs": "',
                   NY_OBS[match("01334500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01334500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01334500.png"}, "geometry": {"type": "Point", "coordinates": [-73.376944, 42.938611]}}, {"type": "Feature", "properties": {"name": "BATTEN KILL BELOW MILL AT BATTENVILLE", "id": "01329490", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01329490", "obs": "',
                   NY_OBS[match("01329490", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01329490", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01329490.png"}, "geometry": {"type": "Point", "coordinates": [-73.421667, 43.108611]}}, {"type": "Feature", "properties": {"name": "HUDSON RIVER AT FORT EDWARD", "id": "01327750", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01327750", "obs": "',
                   NY_OBS[match("01327750", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01327750", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01327750.png"}, "geometry": {"type": "Point", "coordinates": [-73.595833, 43.269444]}}, {"type": "Feature", "properties": {"name": "SACANDAGA RIVER AT STEWARTS BRIDGE NR HADLEY", "id": "01325000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01325000", "obs": "',
                   NY_OBS[match("01325000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01325000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01325000.png"}, "geometry": {"type": "Point", "coordinates": [-73.867222, 43.311389]}}, {"type": "Feature", "properties": {"name": "GLOWEGEE CREEK AT WEST MILTON", "id": "01330000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01330000", "obs": "',
                   NY_OBS[match("01330000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01330000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01330000.png"}, "geometry": {"type": "Point", "coordinates": [-73.927222, 43.030556]}}, {"type": "Feature", "properties": {"name": "HUDSON RIVER ABOVE LOCK 1 NEAR WATERFORD", "id": "01335754", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01335754", "obs": "',
                   NY_OBS[match("01335754", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01335754", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01335754.png"}, "geometry": {"type": "Point", "coordinates": [-73.666111, 42.829167]}}, {"type": "Feature", "properties": {"name": "MOHAWK RIVER AT COHOES", "id": "01357500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01357500", "obs": "',
                   NY_OBS[match("01357500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01357500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01357500.png"}, "geometry": {"type": "Point", "coordinates": [-73.7075, 42.785278]}}, {"type": "Feature", "properties": {"name": "HUDSON RIVER AT GREEN ISLAND", "id": "01358000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01358000", "obs": "',
                   NY_OBS[match("01358000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01358000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01358000.png"}, "geometry": {"type": "Point", "coordinates": [-73.688889, 42.752222]}}, {"type": "Feature", "properties": {"name": "SCHOHARIE CREEK AT BURTONSVILLE", "id": "01351500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01351500", "obs": "',
                   NY_OBS[match("01351500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01351500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01351500.png"}, "geometry": {"type": "Point", "coordinates": [-74.262778, 42.8]}}, {"type": "Feature", "properties": {"name": "EAST CANADA CREEK AT EAST CREEK", "id": "01348000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01348000", "obs": "',
                   NY_OBS[match("01348000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01348000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01348000.png"}, "geometry": {"type": "Point", "coordinates": [-74.740833, 43.016667]}}, {"type": "Feature", "properties": {"name": "WEST CANADA CREEK AT KAST BRIDGE", "id": "01346000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01346000", "obs": "',
                   NY_OBS[match("01346000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01346000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01346000.png"}, "geometry": {"type": "Point", "coordinates": [-74.988333, 43.068889]}}, {"type": "Feature", "properties": {"name": "SUSQUEHANNA RIVER AT UNADILLA", "id": "01500500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01500500", "obs": "',
                   NY_OBS[match("01500500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01500500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01500500.png"}, "geometry": {"type": "Point", "coordinates": [-75.316667, 42.321389]}}, {"type": "Feature", "properties": {"name": "OULEOUT CREEK AT EAST SIDNEY", "id": "01500000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01500000", "obs": "',
                   NY_OBS[match("01500000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01500000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01500000.png"}, "geometry": {"type": "Point", "coordinates": [-75.235, 42.333333]}}, {"type": "Feature", "properties": {"name": "TROUT CREEK NEAR TROUT CREEK", "id": "0142400103", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=0142400103", "obs": "',
                   NY_OBS[match("0142400103", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("0142400103", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_0142400103.png"}, "geometry": {"type": "Point", "coordinates": [-75.279444, 42.173611]}}, {"type": "Feature", "properties": {"name": "WEST BRANCH DELAWARE RIVER AT WALTON", "id": "01423000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01423000", "obs": "',
                   NY_OBS[match("01423000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01423000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01423000.png"}, "geometry": {"type": "Point", "coordinates": [-75.14, 42.166111]}}, {"type": "Feature", "properties": {"name": "EAST BRANCH DELAWARE RIVER AT HARVARD", "id": "01417500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01417500", "obs": "',
                   NY_OBS[match("01417500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01417500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01417500.png"}, "geometry": {"type": "Point", "coordinates": [-75.119278, 42.024583]}}, {"type": "Feature", "properties": {"name": "EAST BRANCH DELAWARE RIVER AT FISHS EDDY", "id": "01421000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01421000", "obs": "',
                   NY_OBS[match("01421000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01421000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01421000.png"}, "geometry": {"type": "Point", "coordinates": [-75.174167, 41.973056]}}, {"type": "Feature", "properties": {"name": "BEAVER KILL AT COOKS FALLS", "id": "01420500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01420500", "obs": "',
                   NY_OBS[match("01420500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01420500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01420500.png"}, "geometry": {"type": "Point", "coordinates": [-74.979722, 41.946389]}}, {"type": "Feature", "properties": {"name": "EAST BRANCH DELAWARE RIVER AT DOWNSVILLE", "id": "01417000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01417000", "obs": "',
                   NY_OBS[match("01417000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01417000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01417000.png"}, "geometry": {"type": "Point", "coordinates": [-74.976389, 42.075]}}, {"type": "Feature", "properties": {"name": "WEST BRANCH DELAWARE RIVER UPSTREAM FROM DELHI", "id": "01421900", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01421900", "obs": "',
                   NY_OBS[match("01421900", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01421900", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01421900.png"}, "geometry": {"type": "Point", "coordinates": [-74.907222, 42.280278]}}, {"type": "Feature", "properties": {"name": "LITTLE DELAWARE RIVER NEAR DELHI", "id": "01422500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01422500", "obs": "',
                   NY_OBS[match("01422500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01422500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01422500.png"}, "geometry": {"type": "Point", "coordinates": [-74.901667, 42.252222]}}, {"type": "Feature", "properties": {"name": "TREMPER KILL NEAR ANDES", "id": "01415000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01415000", "obs": "',
                   NY_OBS[match("01415000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01415000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01415000.png"}, "geometry": {"type": "Point", "coordinates": [-74.818611, 42.12]}}, {"type": "Feature", "properties": {"name": "MILL BROOK NEAR DUNRAVEN", "id": "01414500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01414500", "obs": "',
                   NY_OBS[match("01414500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01414500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01414500.png"}, "geometry": {"type": "Point", "coordinates": [-74.730556, 42.106111]}}, {"type": "Feature", "properties": {"name": "PLATTE KILL AT DUNRAVEN", "id": "01414000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01414000", "obs": "',
                   NY_OBS[match("01414000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01414000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01414000.png"}, "geometry": {"type": "Point", "coordinates": [-74.695556, 42.133056]}}, {"type": "Feature", "properties": {"name": "EAST BRANCH DELAWARE RIVER AT MARGARETVILLE", "id": "01413500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01413500", "obs": "',
                   NY_OBS[match("01413500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01413500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01413500.png"}, "geometry": {"type": "Point", "coordinates": [-74.653611, 42.144722]}}, {"type": "Feature", "properties": {"name": "SCHOHARIE CREEK AT PRATTSVILLE", "id": "01350000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01350000", "obs": "',
                   NY_OBS[match("01350000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01350000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01350000.png"}, "geometry": {"type": "Point", "coordinates": [-74.436667, 42.319444]}}, {"type": "Feature", "properties": {"name": "MINE KILL NEAR NORTH BLENHEIM", "id": "01350140", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01350140", "obs": "',
                   NY_OBS[match("01350140", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01350140", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01350140.png"}, "geometry": {"type": "Point", "coordinates": [-74.473056, 42.428889]}}, {"type": "Feature", "properties": {"name": "SCHOHARIE CREEK AT NORTH BLENHEIM", "id": "01350180", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01350180", "obs": "',
                   NY_OBS[match("01350180", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01350180", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01350180.png"}, "geometry": {"type": "Point", "coordinates": [-74.462222, 42.465833]}}, {"type": "Feature", "properties": {"name": "SCHOHARIE CREEK AT BREAKABEEN", "id": "01350355", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01350355", "obs": "',
                   NY_OBS[match("01350355", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01350355", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01350355.png"}, "geometry": {"type": "Point", "coordinates": [-74.410556, 42.536944]}}, {"type": "Feature", "properties": {"name": "KINDERHOOK CREEK AT ROSSMAN", "id": "01361000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01361000", "obs": "',
                   NY_OBS[match("01361000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01361000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01361000.png"}, "geometry": {"type": "Point", "coordinates": [-73.743889, 42.330556]}}, {"type": "Feature", "properties": {"name": "WAPPINGER CREEK NEAR WAPPINGERS FALLS", "id": "01372500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01372500", "obs": "',
                   NY_OBS[match("01372500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01372500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01372500.png"}, "geometry": {"type": "Point", "coordinates": [-73.8725, 41.653056]}}, {"type": "Feature", "properties": {"name": "ESOPUS CREEK AT MOUNT MARION", "id": "01364500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01364500", "obs": "',
                   NY_OBS[match("01364500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01364500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01364500.png"}, "geometry": {"type": "Point", "coordinates": [-73.971944, 42.037778]}}, {"type": "Feature", "properties": {"name": "RONDOUT CREEK AT ROSENDALE", "id": "01367500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01367500", "obs": "',
                   NY_OBS[match("01367500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01367500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01367500.png"}, "geometry": {"type": "Point", "coordinates": [-74.086111, 41.843056]}}, {"type": "Feature", "properties": {"name": "WALLKILL RIVER AT GARDINER", "id": "01371500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01371500", "obs": "',
                   NY_OBS[match("01371500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01371500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01371500.png"}, "geometry": {"type": "Point", "coordinates": [-74.165278, 41.686111]}}, {"type": "Feature", "properties": {"name": "ESOPUS CREEK AT COLDBROOK", "id": "01362500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01362500", "obs": "',
                   NY_OBS[match("01362500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01362500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01362500.png"}, "geometry": {"type": "Point", "coordinates": [-74.270556, 42.014167]}}, {"type": "Feature", "properties": {"name": "ESOPUS CREEK AT ALLABEN", "id": "01362200", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01362200", "obs": "',
                   NY_OBS[match("01362200", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01362200", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01362200.png"}, "geometry": {"type": "Point", "coordinates": [-74.380278, 42.116944]}}, {"type": "Feature", "properties": {"name": "BISCUIT BK ABOVE PIGEON BK AT FROST VALLEY", "id": "01434025", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01434025", "obs": "',
                   NY_OBS[match("01434025", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01434025", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01434025.png"}, "geometry": {"type": "Point", "coordinates": [-74.500222, 41.996083]}}, {"type": "Feature", "properties": {"name": "RONDOUT CREEK NEAR LOWES CORNERS", "id": "01365000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01365000", "obs": "',
                   NY_OBS[match("01365000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01365000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01365000.png"}, "geometry": {"type": "Point", "coordinates": [-74.487222, 41.866389]}}, {"type": "Feature", "properties": {"name": "CHESTNUT CREEK AT GRAHAMSVILLE", "id": "01365500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01365500", "obs": "',
                   NY_OBS[match("01365500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01365500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01365500.png"}, "geometry": {"type": "Point", "coordinates": [-74.539444, 41.845]}}, {"type": "Feature", "properties": {"name": "NEVERSINK RIVER NEAR CLARYVILLE", "id": "01435000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01435000", "obs": "',
                   NY_OBS[match("01435000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01435000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01435000.png"}, "geometry": {"type": "Point", "coordinates": [-74.59, 41.89]}}, {"type": "Feature", "properties": {"name": "NEVERSINK RIVER AT NEVERSINK", "id": "01436000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01436000", "obs": "',
                   NY_OBS[match("01436000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01436000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01436000.png"}, "geometry": {"type": "Point", "coordinates": [-74.635556, 41.82]}}, {"type": "Feature", "properties": {"name": "DELAWARE RIVER AT CALLICOON", "id": "01427510", "url": "https://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01427510", "obs": "',
                   NY_OBS[match("01427510", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01427510", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01427510.png"}, "geometry": {"type": "Point", "coordinates": [-75.0575, 41.756667]}}, {"type": "Feature", "properties": {"name": "DELAWARE R ABOVE LACKAWAXEN R NEAR BARRYVILLE", "id": "01428500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01428500", "obs": "',
                   NY_OBS[match("01428500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01428500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01428500.png"}, "geometry": {"type": "Point", "coordinates": [-74.985833, 41.508889]}}, {"type": "Feature", "properties": {"name": "MONGAUP RIVER NEAR MONGAUP", "id": "01433500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01433500", "obs": "',
                   NY_OBS[match("01433500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01433500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01433500.png"}, "geometry": {"type": "Point", "coordinates": [-74.758889, 41.461389]}}, {"type": "Feature", "properties": {"name": "DELAWARE RIVER AT PORT JERVIS", "id": "01434000", "url": "https://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01434000", "obs": "',
                   NY_OBS[match("01434000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01434000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01434000.png"}, "geometry": {"type": "Point", "coordinates": [-74.6975, 41.370556]}}, {"type": "Feature", "properties": {"name": "NEVERSINK RIVER AT GODEFFROY", "id": "01437500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01437500", "obs": "',
                   NY_OBS[match("01437500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01437500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01437500.png"}, "geometry": {"type": "Point", "coordinates": [-74.601944, 41.441111]}}, {"type": "Feature", "properties": {"name": "RAMAPO RIVER AT SUFFERN", "id": "01387420", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01387420", "obs": "',
                   NY_OBS[match("01387420", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01387420", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01387420.png"}, "geometry": {"type": "Point", "coordinates": [-74.160278, 41.118333]}}, {"type": "Feature", "properties": {"name": "Mahwah River near Suffern", "id": "01387450", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01387450", "obs": "',
                   NY_OBS[match("01387450", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01387450", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01387450.png"}, "geometry": {"type": "Point", "coordinates": [-74.116111, 41.141111]}}, {"type": "Feature", "properties": {"name": "HACKENSACK RIVER AT WEST NYACK", "id": "01376800", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01376800", "obs": "',
                   NY_OBS[match("01376800", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01376800", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01376800.png"}, "geometry": {"type": "Point", "coordinates": [-73.963889, 41.095556]}}, {"type": "Feature", "properties": {"name": "CROTON R AT NEW CROTON DAM NR CROTON-ON-HUDSON", "id": "01375000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01375000", "obs": "',
                   NY_OBS[match("01375000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01375000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01375000.png"}, "geometry": {"type": "Point", "coordinates": [-73.858889, 41.225028]}}, {"type": "Feature", "properties": {"name": "MASSAPEQUA CREEK AT MASSAPEQUA", "id": "01309500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01309500", "obs": "',
                   NY_OBS[match("01309500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01309500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01309500.png"}, "geometry": {"type": "Point", "coordinates": [-73.454722, 40.688889]}}, {"type": "Feature", "properties": {"name": "COLD SPRING BROOK AT COLD SPRING HARBOR", "id": "01303500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01303500", "obs": "',
                   NY_OBS[match("01303500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01303500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01303500.png"}, "geometry": {"type": "Point", "coordinates": [-73.463333, 40.857222]}}, {"type": "Feature", "properties": {"name": "CARLLS RIVER AT BABYLON", "id": "01308500", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01308500", "obs": "',
                   NY_OBS[match("01308500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01308500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01308500.png"}, "geometry": {"type": "Point", "coordinates": [-73.328333, 40.708611]}}, {"type": "Feature", "properties": {"name": "NISSEQUOGUE RIVER NEAR SMITHTOWN", "id": "01304000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01304000", "obs": "',
                   NY_OBS[match("01304000", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01304000", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01304000.png"}, "geometry": {"type": "Point", "coordinates": [-73.224167, 40.849444]}}, {"type": "Feature", "properties": {"name": "CONNETQUOT BK NR CENTRAL ISLIP", "id": "01306460", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01306460", "obs": "',
                   NY_OBS[match("01306460", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01306460", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01306460.png"}, "geometry": {"type": "Point", "coordinates": [-73.158611, 40.771944]}}, {"type": "Feature", "properties": {"name": "PECONIC RIVER AT RIVERHEAD", "id": "01304500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01304500", "obs": "',
                   NY_OBS[match("01304500", NY_OBS[,1]), 2], '", "time": "', NY_OBS[match("01304500", NY_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01304500.png"}, "geometry": {"type": "Point", "coordinates": [-72.686667, 40.913611]}}]};', sep="")

# Export data to geojson.
cat(json_merge, file="/home/staff/klr324/marisa.psu.edu/mapdata/NY_stream_obs.js")
# --------------------------------------------------------------------------------------------------------------------
