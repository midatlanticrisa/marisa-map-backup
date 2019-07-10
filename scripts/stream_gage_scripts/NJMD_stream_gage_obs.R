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
# <!-- New Jersey -->
NJ_ID = c("01411300", "01411500", "01482500", "01477120", "01475001", "01411000", "01467150", "01467081",
          "01409400", "01410150", "01410000", "01409810", "01466500", "01467000", "01408900", "01408500", "01408120",
          "01408000", "01407705", "01407760", "01464500", "01407500", "01463500", "01464000", "01401000", "01405400",
          "01398000", "01397000", "01396800", "01399670", "01396660", "01396500", "01396582", "01401650", "01402000",
          "01403060", "01400500", "01400000", "01403150", "01403540", "01379000", "01398500", "01403400", "01395000",
          "01393450", "01394500", "01379500", "01457000", "01446500", "01445500", "01446000", "01443500", "01443900",
          "01399500", "01381500", "01455500", "01379780", "01379773", "01380450", "01381000", "01388500", "01381900",
          "01392500", "01389500", "01388000", "01391500", "01378500", "01377000", "01377500", "01390500", "01391000",
          "01387500", "01387000", "01386000", "01384500", "01383500", "01382500", "01445000", "01440000", "01438500")

# Run through each New Jersey station.
NJ_TMP = mat.or.vec(length(NJ_ID), 3)
NJ_TMP[ ,1] = NJ_ID; NJ_DIS = NJ_TMP; NJ_GAG = NJ_TMP
for(i in 1:length(NJ_ID)){
  NJ_TMP[i, 2:3] = usgs_dataRetrieveTemp(NJ_ID[i], "00010", b.date, e.date, tz="America/New_York")
  NJ_DIS[i, 2:3] = usgs_dataRetrieveTemp(NJ_ID[i], "00060", b.date, e.date, tz="America/New_York")
  NJ_GAG[i, 2:3] = usgs_dataRetrieveTemp(NJ_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
NJ_OBS = obs_string(NJ_TMP, NJ_DIS, NJ_GAG)

# <!-- Maryland -->
MD_ID = c("01485500", "01486000", "01485000", "01490000", "01491000", "01493000", "01493500", "01495000",
          "01580000", "01581500", "01581700", "01584500", "01582000", "01582500", "01583500", "01583600", "01584050",
          "01585100", "01585200", "01589440", "01589300", "01589330", "01589000", "01589100", "01589500", "01594440",
          "01661500", "01661050", "01660920", "01658000", "01653600", "01649500", "01651000", "01650500", "01645000",
          "01591700", "01591000", "01591610", "01591400", "01593500", "01594000", "01638500", "01643500", "01643000",
          "01637500", "01586610", "01586000", "01586210", "01585500", "01639500", "01639000", "01619500", "01617800",
          "01614500", "01613000", "01609000", "01603000", "01601500", "01599000", "01598500", "01597500", "01596500",
          "03078000", "03076600", "03076500", "01595500", "01595000", "03075500")

# Run through each Maryland station.
MD_TMP = mat.or.vec(length(MD_ID), 3)
MD_TMP[ ,1] = MD_ID; MD_DIS = MD_TMP; MD_GAG = MD_TMP
for(i in 1:length(MD_ID)){
  MD_TMP[i, 2:3] = usgs_dataRetrieveTemp(MD_ID[i], "00010", b.date, e.date, tz="America/New_York")
  MD_DIS[i, 2:3] = usgs_dataRetrieveTemp(MD_ID[i], "00060", b.date, e.date, tz="America/New_York")
  MD_GAG[i, 2:3] = usgs_dataRetrieveTemp(MD_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
MD_OBS = obs_string(MD_TMP, MD_DIS, MD_GAG)

# # Discontinued station
# MD_ID_DIS = c("01486500")
# MD_DIS_TMP = c("01486500", "NA", "NA")

# --------------------------------------------------------------------------------------------------------------------
# Create a geojson object with the observation and statement info and merge into a 
# specific file format with adding stream temp and time.
json_merge = paste('NJMD_streamGauges = {"type": "FeatureCollection","features": [{"type": "Feature", "properties": {"name": "Tuckahoe River at Head of River", "id": "01411300", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01411300", "obs": "',
                   NJ_OBS[match("01411300", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01411300", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01411300.png"}, "geometry": {"type": "Point", "coordinates": [-74.820556, 39.306944]}}, {"type": "Feature", "properties": {"name": "Maurice River at Norma", "id": "01411500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01411500", "obs": "',
                   NJ_OBS[match("01411500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01411500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01411500.png"}, "geometry": {"type": "Point", "coordinates": [-75.076944, 39.495556]}}, {"type": "Feature", "properties": {"name": "Salem River at Woodstown", "id": "01482500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01482500", "obs": "',
                   NJ_OBS[match("01482500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01482500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01482500.png"}, "geometry": {"type": "Point", "coordinates": [-75.330278, 39.643889]}}, {"type": "Feature", "properties": {"name": "Raccoon Creek near Swedesboro", "id": "01477120", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01477120", "obs": "',
                   NJ_OBS[match("01477120", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01477120", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01477120.png"}, "geometry": {"type": "Point", "coordinates": [-75.259167, 39.740556]}}, {"type": "Feature", "properties": {"name": "Mantua Creek at East Holly Avenue at Pitman", "id": "01475001", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01475001", "obs": "',
                   NJ_OBS[match("01475001", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01475001", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01475001.png"}, "geometry": {"type": "Point", "coordinates": [-75.114444, 39.738889]}}, {"type": "Feature", "properties": {"name": "Great Egg Harbor River at Folsom", "id": "01411000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01411000", "obs": "',
                   NJ_OBS[match("01411000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01411000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01411000.png"}, "geometry": {"type": "Point", "coordinates": [-74.851667, 39.594722]}}, {"type": "Feature", "properties": {"name": "Cooper River at Haddonfield", "id": "01467150", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01467150", "obs": "',
                   NJ_OBS[match("01467150", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01467150", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01467150.png"}, "geometry": {"type": "Point", "coordinates": [-75.021389, 39.903056]}}, {"type": "Feature", "properties": {"name": "South Branch Pennsauken Creek at Cherry Hill", "id": "01467081", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01467081", "obs": "',
                   NJ_OBS[match("01467081", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01467081", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01467081.png"}, "geometry": {"type": "Point", "coordinates": [-75.001111, 39.941667]}}, {"type": "Feature", "properties": {"name": "Mullica River near Batsto", "id": "01409400", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01409400", "obs": "',
                   NJ_OBS[match("01409400", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01409400", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01409400.png"}, "geometry": {"type": "Point", "coordinates": [-74.665, 39.674444]}}, {"type": "Feature", "properties": {"name": "East Branch Bass River near New Gretna", "id": "01410150", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01410150", "obs": "',
                   NJ_OBS[match("01410150", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01410150", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01410150.png"}, "geometry": {"type": "Point", "coordinates": [-74.441389, 39.623056]}}, {"type": "Feature", "properties": {"name": "Oswego River at Harrisville", "id": "01410000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01410000", "obs": "',
                   NJ_OBS[match("01410000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01410000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01410000.png"}, "geometry": {"type": "Point", "coordinates": [-74.523611, 39.663333]}}, {"type": "Feature", "properties": {"name": "West Branch Wading River near Jenkins", "id": "01409810", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01409810", "obs": "',
                   NJ_OBS[match("01409810", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01409810", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01409810.png"}, "geometry": {"type": "Point", "coordinates": [-74.548056, 39.688056]}}, {"type": "Feature", "properties": {"name": "McDonalds Branch in Byrne State Forest", "id": "01466500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01466500", "obs": "',
                   NJ_OBS[match("01466500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01466500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01466500.png"}, "geometry": {"type": "Point", "coordinates": [-74.505278, 39.885]}}, {"type": "Feature", "properties": {"name": "North Branch Rancocas Creek at Pemberton", "id": "01467000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01467000", "obs": "',
                   NJ_OBS[match("01467000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01467000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01467000.png"}, "geometry": {"type": "Point", "coordinates": [-74.684444, 39.97]}}, {"type": "Feature", "properties": {"name": "Cedar Creek at Western Blvd near Lanoka Harbor", "id": "01408900", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01408900", "obs": "',
                   NJ_OBS[match("01408900", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01408900", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01408900.png"}, "geometry": {"type": "Point", "coordinates": [-74.190556, 39.879167]}}, {"type": "Feature", "properties": {"name": "Toms River near Toms River", "id": "01408500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01408500", "obs": "',
                   NJ_OBS[match("01408500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01408500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01408500.png"}, "geometry": {"type": "Point", "coordinates": [-74.223333, 39.986389]}}, {"type": "Feature", "properties": {"name": "North Branch Metedeconk River near Lakewood", "id": "01408120", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01408120", "obs": "',
                   NJ_OBS[match("01408120", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01408120", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01408120.png"}, "geometry": {"type": "Point", "coordinates": [-74.1525, 40.091667]}}, {"type": "Feature", "properties": {"name": "Manasquan River at Squankum", "id": "01408000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01408000", "obs": "',
                   NJ_OBS[match("01408000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01408000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01408000.png"}, "geometry": {"type": "Point", "coordinates": [-74.154722, 40.161389]}}, {"type": "Feature", "properties": {"name": "Shark River near Neptune City", "id": "01407705", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01407705", "obs": "',
                   NJ_OBS[match("01407705", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01407705", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01407705.png"}, "geometry": {"type": "Point", "coordinates": [-74.07, 40.198611]}}, {"type": "Feature", "properties": {"name": "Jumping Brook near Neptune City", "id": "01407760", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01407760", "obs": "',
                   NJ_OBS[match("01407760", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01407760", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01407760.png"}, "geometry": {"type": "Point", "coordinates": [-74.065833, 40.203333]}}, {"type": "Feature", "properties": {"name": "Crosswicks Creek at Extonville", "id": "01464500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01464500", "obs": "',
                   NJ_OBS[match("01464500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01464500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01464500.png"}, "geometry": {"type": "Point", "coordinates": [-74.6, 40.137222]}}, {"type": "Feature", "properties": {"name": "Swimming River near Red Bank", "id": "01407500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01407500", "obs": "',
                   NJ_OBS[match("01407500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01407500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01407500.png"}, "geometry": {"type": "Point", "coordinates": [-74.115556, 40.319722]}}, {"type": "Feature", "properties": {"name": "Delaware River at Trenton", "id": "01463500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01463500", "obs": "',
                   NJ_OBS[match("01463500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01463500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01463500.png"}, "geometry": {"type": "Point", "coordinates": [-74.778056, 40.221667]}}, {"type": "Feature", "properties": {"name": "Assunpink Creek at Trenton", "id": "01464000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01464000", "obs": "',
                   NJ_OBS[match("01464000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01464000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01464000.png"}, "geometry": {"type": "Point", "coordinates": [-74.749167, 40.224167]}}, {"type": "Feature", "properties": {"name": "Stony Brook at Princeton", "id": "01401000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01401000", "obs": "',
                   NJ_OBS[match("01401000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01401000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01401000.png"}, "geometry": {"type": "Point", "coordinates": [-74.681944, 40.333056]}}, {"type": "Feature", "properties": {"name": "Manalapan Brook at Spotswood", "id": "01405400", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01405400", "obs": "',
                   NJ_OBS[match("01405400", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01405400", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01405400.png"}, "geometry": {"type": "Point", "coordinates": [-74.390556, 40.389444]}}, {"type": "Feature", "properties": {"name": "Neshanic River at Reaville", "id": "01398000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01398000", "obs": "',
                   NJ_OBS[match("01398000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01398000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01398000.png"}, "geometry": {"type": "Point", "coordinates": [-74.827778, 40.473333]}}, {"type": "Feature", "properties": {"name": "South Branch Raritan River at Stanton", "id": "01397000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01397000", "obs": "',
                   NJ_OBS[match("01397000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01397000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01397000.png"}, "geometry": {"type": "Point", "coordinates": [-74.868056, 40.572222]}}, {"type": "Feature", "properties": {"name": "Spruce Run at Clinton", "id": "01396800", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01396800", "obs": "',
                   NJ_OBS[match("01396800", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01396800", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01396800.png"}, "geometry": {"type": "Point", "coordinates": [-74.915556, 40.64]}}, {"type": "Feature", "properties": {"name": "South B Rockaway Creek at Whitehouse Station", "id": "01399670", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01399670", "obs": "',
                   NJ_OBS[match("01399670", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01399670", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01399670.png"}, "geometry": {"type": "Point", "coordinates": [-74.773611, 40.619444]}}, {"type": "Feature", "properties": {"name": "Mulhockaway Creek at Van Syckel", "id": "01396660", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01396660", "obs": "',
                   NJ_OBS[match("01396660", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01396660", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01396660.png"}, "geometry": {"type": "Point", "coordinates": [-74.968889, 40.6475]}}, {"type": "Feature", "properties": {"name": "South Branch Raritan River near High Bridge", "id": "01396500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01396500", "obs": "',
                   NJ_OBS[match("01396500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01396500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01396500.png"}, "geometry": {"type": "Point", "coordinates": [-74.879167, 40.677778]}}, {"type": "Feature", "properties": {"name": "Spruce Run at Main Street at Glen Gardner", "id": "01396582", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01396582", "obs": "',
                   NJ_OBS[match("01396582", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01396582", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01396582.png"}, "geometry": {"type": "Point", "coordinates": [-74.936944, 40.691389]}}, {"type": "Feature", "properties": {"name": "Pike Run at Belle Mead", "id": "01401650", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01401650", "obs": "',
                   NJ_OBS[match("01401650", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01401650", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01401650.png"}, "geometry": {"type": "Point", "coordinates": [-74.648889, 40.468056]}}, {"type": "Feature", "properties": {"name": "Millstone River at Blackwells Mills", "id": "01402000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01402000", "obs": "',
                   NJ_OBS[match("01402000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01402000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01402000.png"}, "geometry": {"type": "Point", "coordinates": [-74.575833, 40.475]}}, {"type": "Feature", "properties": {"name": "Raritan River below Calco Dam at Bound Brook", "id": "01403060", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01403060", "obs": "',
                   NJ_OBS[match("01403060", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01403060", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01403060.png"}, "geometry": {"type": "Point", "coordinates": [-74.548333, 40.551111]}}, {"type": "Feature", "properties": {"name": "Raritan River at Manville", "id": "01400500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01400500", "obs": "',
                   NJ_OBS[match("01400500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01400500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01400500.png"}, "geometry": {"type": "Point", "coordinates": [-74.582778, 40.555556]}}, {"type": "Feature", "properties": {"name": "North Branch Raritan River near Raritan", "id": "01400000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01400000", "obs": "',
                   NJ_OBS[match("01400000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01400000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01400000.png"}, "geometry": {"type": "Point", "coordinates": [-74.679167, 40.570556]}}, {"type": "Feature", "properties": {"name": "West Branch Middle Brook near Martinsville", "id": "01403150", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01403150", "obs": "',
                   NJ_OBS[match("01403150", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01403150", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01403150.png"}, "geometry": {"type": "Point", "coordinates": [-74.590833, 40.612222]}}, {"type": "Feature", "properties": {"name": "Stony Brook at Watchung", "id": "01403540", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01403540", "obs": "',
                   NJ_OBS[match("01403540", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01403540", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01403540.png"}, "geometry": {"type": "Point", "coordinates": [-74.451389, 40.636389]}}, {"type": "Feature", "properties": {"name": "Passaic River near Millington", "id": "01379000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01379000", "obs": "',
                   NJ_OBS[match("01379000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01379000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01379000.png"}, "geometry": {"type": "Point", "coordinates": [-74.528889, 40.68]}}, {"type": "Feature", "properties": {"name": "North Branch Raritan River near Far Hills", "id": "01398500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01398500", "obs": "',
                   NJ_OBS[match("01398500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01398500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01398500.png"}, "geometry": {"type": "Point", "coordinates": [-74.636111, 40.708333]}}, {"type": "Feature", "properties": {"name": "Green Brook at Seeley Mills", "id": "01403400", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01403400", "obs": "',
                   NJ_OBS[match("01403400", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01403400", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01403400.png"}, "geometry": {"type": "Point", "coordinates": [-74.403889, 40.666111]}}, {"type": "Feature", "properties": {"name": "Rahway River at Rahway", "id": "01395000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01395000", "obs": "',
                   NJ_OBS[match("01395000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01395000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01395000.png"}, "geometry": {"type": "Point", "coordinates": [-74.283333, 40.618889]}}, {"type": "Feature", "properties": {"name": "Elizabeth River at Ursino Lake at Elizabeth", "id": "01393450", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01393450", "obs": "',
                   NJ_OBS[match("01393450", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01393450", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01393450.png"}, "geometry": {"type": "Point", "coordinates": [-74.221944, 40.675]}}, {"type": "Feature", "properties": {"name": "Rahway River near Springfield", "id": "01394500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01394500", "obs": "',
                   NJ_OBS[match("01394500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01394500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01394500.png"}, "geometry": {"type": "Point", "coordinates": [-74.311667, 40.6875]}}, {"type": "Feature", "properties": {"name": "Passaic River near Chatham", "id": "01379500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01379500", "obs": "',
                   NJ_OBS[match("01379500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01379500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01379500.png"}, "geometry": {"type": "Point", "coordinates": [-74.389722, 40.726111]}}, {"type": "Feature", "properties": {"name": "Musconetcong River near Bloomsbury", "id": "01457000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01457000", "obs": "',
                   NJ_OBS[match("01457000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01457000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01457000.png"}, "geometry": {"type": "Point", "coordinates": [-75.060833, 40.672222]}}, {"type": "Feature", "properties": {"name": "Delaware River at Belvidere", "id": "01446500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01446500", "obs": "',
                   NJ_OBS[match("01446500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01446500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01446500.png"}, "geometry": {"type": "Point", "coordinates": [-75.0825, 40.826389]}}, {"type": "Feature", "properties": {"name": "Pequest River at Pequest", "id": "01445500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01445500", "obs": "',
                   NJ_OBS[match("01445500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01445500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01445500.png"}, "geometry": {"type": "Point", "coordinates": [-74.977778, 40.830556]}}, {"type": "Feature", "properties": {"name": "Beaver Brook near Belvidere", "id": "01446000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01446000", "obs": "',
                   NJ_OBS[match("01446000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01446000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01446000.png"}, "geometry": {"type": "Point", "coordinates": [-75.046389, 40.843333]}}, {"type": "Feature", "properties": {"name": "Paulins Kill at Blairstown", "id": "01443500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01443500", "obs": "',
                   NJ_OBS[match("01443500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01443500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01443500.png"}, "geometry": {"type": "Point", "coordinates": [-74.953333, 40.980833]}}, {"type": "Feature", "properties": {"name": "Yards Creek near Blairstown", "id": "01443900", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01443900", "obs": "',
                   NJ_OBS[match("01443900", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01443900", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01443900.png"}, "geometry": {"type": "Point", "coordinates": [-75.039167, 40.980556]}}, {"type": "Feature", "properties": {"name": "Lamington (Black) River near Pottersville", "id": "01399500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01399500", "obs": "',
                   NJ_OBS[match("01399500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01399500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01399500.png"}, "geometry": {"type": "Point", "coordinates": [-74.730278, 40.7275]}}, {"type": "Feature", "properties": {"name": "Whippany River at Morristown", "id": "01381500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01381500", "obs": "',
                   NJ_OBS[match("01381500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01381500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01381500.png"}, "geometry": {"type": "Point", "coordinates": [-74.456944, 40.807222]}}, {"type": "Feature", "properties": {"name": "Musconetcong River at outlet of Lake Hopatcong", "id": "01455500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01455500", "obs": "',
                   NJ_OBS[match("01455500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01455500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01455500.png"}, "geometry": {"type": "Point", "coordinates": [-74.665556, 40.917222]}}, {"type": "Feature", "properties": {"name": "Green Pond Bk blw Picatinny Lk at Picatinny Ars", "id": "01379780", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01379780", "obs": "',
                   NJ_OBS[match("01379780", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01379780", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01379780.png"}, "geometry": {"type": "Point", "coordinates": [-74.557222, 40.949444]}}, {"type": "Feature", "properties": {"name": "Green Pond Brook at Picatinny Arsenal", "id": "01379773", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01379773", "obs": "',
                   NJ_OBS[match("01379773", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01379773", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01379773.png"}, "geometry": {"type": "Point", "coordinates": [-74.539722, 40.96]}}, {"type": "Feature", "properties": {"name": "Rockaway River at Main Street at Boonton", "id": "01380450", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01380450", "obs": "',
                   NJ_OBS[match("01380450", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01380450", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01380450.png"}, "geometry": {"type": "Point", "coordinates": [-74.419167, 40.9075]}}, {"type": "Feature", "properties": {"name": "Rockaway River below Reservoir at Boonton", "id": "01381000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01381000", "obs": "',
                   NJ_OBS[match("01381000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01381000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01381000.png"}, "geometry": {"type": "Point", "coordinates": [-74.394722, 40.896944]}}, {"type": "Feature", "properties": {"name": "Pompton River at Pompton Plains", "id": "01388500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01388500", "obs": "',
                   NJ_OBS[match("01388500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01388500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01388500.png"}, "geometry": {"type": "Point", "coordinates": [-74.281944, 40.969722]}}, {"type": "Feature", "properties": {"name": "Passaic River at Pine Brook", "id": "01381900", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01381900", "obs": "',
                   NJ_OBS[match("01381900", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01381900", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01381900.png"}, "geometry": {"type": "Point", "coordinates": [-74.321667, 40.8625]}}, {"type": "Feature", "properties": {"name": "Second River at Belleville", "id": "01392500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01392500", "obs": "',
                   NJ_OBS[match("01392500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01392500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01392500.png"}, "geometry": {"type": "Point", "coordinates": [-74.171667, 40.788056]}}, {"type": "Feature", "properties": {"name": "Passaic River at Little Falls", "id": "01389500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01389500", "obs": "',
                   NJ_OBS[match("01389500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01389500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01389500.png"}, "geometry": {"type": "Point", "coordinates": [-74.226111, 40.884722]}}, {"type": "Feature", "properties": {"name": "Ramapo River at Pompton Lakes", "id": "01388000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01388000", "obs": "',
                   NJ_OBS[match("01388000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01388000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01388000.png"}, "geometry": {"type": "Point", "coordinates": [-74.28, 40.991944]}}, {"type": "Feature", "properties": {"name": "Saddle River at Lodi", "id": "01391500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01391500", "obs": "',
                   NJ_OBS[match("01391500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01391500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01391500.png"}, "geometry": {"type": "Point", "coordinates": [-74.080556, 40.890278]}}, {"type": "Feature", "properties": {"name": "Hackensack River at New Milford", "id": "01378500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01378500", "obs": "',
                   NJ_OBS[match("01378500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01378500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01378500.png"}, "geometry": {"type": "Point", "coordinates": [-74.026667, 40.948333]}}, {"type": "Feature", "properties": {"name": "Hackensack River at Rivervale", "id": "01377000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01377000", "obs": "',
                   NJ_OBS[match("01377000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01377000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01377000.png"}, "geometry": {"type": "Point", "coordinates": [-73.989167, 40.999167]}}, {"type": "Feature", "properties": {"name": "Pascack Brook at Westwood", "id": "01377500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01377500", "obs": "',
                   NJ_OBS[match("01377500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01377500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01377500.png"}, "geometry": {"type": "Point", "coordinates": [-74.021111, 40.992778]}}, {"type": "Feature", "properties": {"name": "Saddle River at Ridgewood", "id": "01390500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01390500", "obs": "',
                   NJ_OBS[match("01390500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01390500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01390500.png"}, "geometry": {"type": "Point", "coordinates": [-74.090556, 40.985]}}, {"type": "Feature", "properties": {"name": "Hohokus Brook at Ho-Ho-Kus", "id": "01391000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01391000", "obs": "',
                   NJ_OBS[match("01391000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01391000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01391000.png"}, "geometry": {"type": "Point", "coordinates": [-74.111944, 40.997778]}}, {"type": "Feature", "properties": {"name": "Ramapo River near Mahwah", "id": "01387500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01387500", "obs": "',
                   NJ_OBS[match("01387500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01387500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01387500.png"}, "geometry": {"type": "Point", "coordinates": [-74.162778, 41.098056]}}, {"type": "Feature", "properties": {"name": "Wanaque River at Wanaque", "id": "01387000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01387000", "obs": "',
                   NJ_OBS[match("01387000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01387000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01387000.png"}, "geometry": {"type": "Point", "coordinates": [-74.293056, 41.044167]}}, {"type": "Feature", "properties": {"name": "West Brook near Wanaque", "id": "01386000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01386000", "obs": "',
                   NJ_OBS[match("01386000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01386000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01386000.png"}, "geometry": {"type": "Point", "coordinates": [-74.311667, 41.073611]}}, {"type": "Feature", "properties": {"name": "Ringwood Creek near Wanaque", "id": "01384500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01384500", "obs": "',
                   NJ_OBS[match("01384500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01384500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01384500.png"}, "geometry": {"type": "Point", "coordinates": [-74.265833, 41.127222]}}, {"type": "Feature", "properties": {"name": "Wanaque River at Awosting", "id": "01383500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01383500", "obs": "',
                   NJ_OBS[match("01383500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01383500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01383500.png"}, "geometry": {"type": "Point", "coordinates": [-74.333611, 41.160278]}}, {"type": "Feature", "properties": {"name": "Pequannock River at Macopin Intake Dam", "id": "01382500", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01382500", "obs": "',
                   NJ_OBS[match("01382500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01382500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01382500.png"}, "geometry": {"type": "Point", "coordinates": [-74.401111, 41.018333]}}, {"type": "Feature", "properties": {"name": "Pequest River at Huntsville", "id": "01445000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01445000", "obs": "',
                   NJ_OBS[match("01445000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01445000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01445000.png"}, "geometry": {"type": "Point", "coordinates": [-74.776389, 40.980833]}}, {"type": "Feature", "properties": {"name": "Flat Brook near Flatbrookville", "id": "01440000", "url": "https://waterdata.usgs.gov/nj/nwis/inventory/?site_no=01440000", "obs": "',
                   NJ_OBS[match("01440000", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01440000", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01440000.png"}, "geometry": {"type": "Point", "coordinates": [-74.9525, 41.106111]}}, {"type": "Feature", "properties": {"name": "Delaware River at Montague", "id": "01438500", "url": "https://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01438500", "obs": "',
                   NJ_OBS[match("01438500", NJ_OBS[,1]), 2], '", "time": "', NJ_OBS[match("01438500", NJ_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01438500.png"}, "geometry": {"type": "Point", "coordinates": [-74.795278, 41.309167]}}, {"type": "Feature", "properties": {"name": "NASSAWANGO CREEK NEAR SNOW HILL", "id": "01485500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01485500", "obs": "',
                   MD_OBS[match("01485500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01485500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01485500.png"}, "geometry": {"type": "Point", "coordinates": [-75.471444, 38.228917]}}, {"type": "Feature", "properties": {"name": "MANOKIN BRANCH NEAR PRINCESS ANNE", "id": "01486000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01486000", "obs": "',
                   MD_OBS[match("01486000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01486000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01486000.png"}, "geometry": {"type": "Point", "coordinates": [-75.671389, 38.213889]}}, {"type": "Feature", "properties": {"name": "POCOMOKE RIVER NEAR WILLARDS", "id": "01485000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01485000", "obs": "',
                   MD_OBS[match("01485000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01485000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01485000.png"}, "geometry": {"type": "Point", "coordinates": [-75.324444, 38.388889]}}, {"type": "Feature", "properties": {"name": "CHICAMACOMICO RIVER NEAR SALEM", "id": "01490000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01490000", "obs": "',
                   MD_OBS[match("01490000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01490000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01490000.png"}, "geometry": {"type": "Point", "coordinates": [-75.879917, 38.511667]}}, {"type": "Feature", "properties": {"name": "CHOPTANK RIVER NEAR GREENSBORO", "id": "01491000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01491000", "obs": "',
                   MD_OBS[match("01491000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01491000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01491000.png"}, "geometry": {"type": "Point", "coordinates": [-75.785806, 38.997194]}}, {"type": "Feature", "properties": {"name": "UNICORN BRANCH NEAR MILLINGTON", "id": "01493000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01493000", "obs": "',
                   MD_OBS[match("01493000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01493000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01493000.png"}, "geometry": {"type": "Point", "coordinates": [-75.861306, 39.249694]}}, {"type": "Feature", "properties": {"name": "MORGAN CREEK NEAR KENNEDYVILLE", "id": "01493500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01493500", "obs": "',
                   MD_OBS[match("01493500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01493500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01493500.png"}, "geometry": {"type": "Point", "coordinates": [-76.014556, 39.280028]}}, {"type": "Feature", "properties": {"name": "BIG ELK CREEK AT ELK MILLS", "id": "01495000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01495000", "obs": "',
                   MD_OBS[match("01495000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01495000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01495000.png"}, "geometry": {"type": "Point", "coordinates": [-75.825583, 39.667583]}}, {"type": "Feature", "properties": {"name": "DEER CREEK AT ROCKS", "id": "01580000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01580000", "obs": "',
                   MD_OBS[match("01580000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01580000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01580000.png"}, "geometry": {"type": "Point", "coordinates": [-76.403306, 39.629972]}}, {"type": "Feature", "properties": {"name": "BYNUM RUN AT BEL AIR", "id": "01581500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01581500", "obs": "',
                   MD_OBS[match("01581500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01581500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01581500.png"}, "geometry": {"type": "Point", "coordinates": [-76.330111, 39.541472]}}, {"type": "Feature", "properties": {"name": "WINTERS RUN NEAR BENSON", "id": "01581700", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01581700", "obs": "',
                   MD_OBS[match("01581700", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01581700", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01581700.png"}, "geometry": {"type": "Point", "coordinates": [-76.372972, 39.519944]}}, {"type": "Feature", "properties": {"name": "LITTLE GUNPOWDER FALLS AT LAUREL BROOK", "id": "01584500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01584500", "obs": "',
                   MD_OBS[match("01584500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01584500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01584500.png"}, "geometry": {"type": "Point", "coordinates": [-76.431778, 39.505361]}}, {"type": "Feature", "properties": {"name": "LITTLE FALLS AT BLUE MOUNT", "id": "01582000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01582000", "obs": "',
                   MD_OBS[match("01582000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01582000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01582000.png"}, "geometry": {"type": "Point", "coordinates": [-76.620472, 39.604083]}}, {"type": "Feature", "properties": {"name": "GUNPOWDER FALLS AT GLENCOE", "id": "01582500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01582500", "obs": "',
                   MD_OBS[match("01582500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01582500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01582500.png"}, "geometry": {"type": "Point", "coordinates": [-76.636111, 39.549694]}}, {"type": "Feature", "properties": {"name": "WESTERN RUN AT WESTERN RUN", "id": "01583500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01583500", "obs": "',
                   MD_OBS[match("01583500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01583500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01583500.png"}, "geometry": {"type": "Point", "coordinates": [-76.6765, 39.510778]}}, {"type": "Feature", "properties": {"name": "BEAVERDAM RUN AT COCKEYSVILLE", "id": "01583600", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01583600", "obs": "',
                   MD_OBS[match("01583600", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01583600", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01583600.png"}, "geometry": {"type": "Point", "coordinates": [-76.645722, 39.485583]}}, {"type": "Feature", "properties": {"name": "LONG GREEN CREEK AT GLEN ARM", "id": "01584050", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01584050", "obs": "',
                   MD_OBS[match("01584050", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01584050", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01584050.png"}, "geometry": {"type": "Point", "coordinates": [-76.478889, 39.454694]}}, {"type": "Feature", "properties": {"name": "WHITEMARSH RUN AT WHITE MARSH", "id": "01585100", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01585100", "obs": "',
                   MD_OBS[match("01585100", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01585100", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01585100.png"}, "geometry": {"type": "Point", "coordinates": [-76.445917, 39.370528]}}, {"type": "Feature", "properties": {"name": "WEST BRANCH HERRING RUN AT IDLEWYLDE", "id": "01585200", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01585200", "obs": "',
                   MD_OBS[match("01585200", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01585200", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01585200.png"}, "geometry": {"type": "Point", "coordinates": [-76.584333, 39.373639]}}, {"type": "Feature", "properties": {"name": "JONES FALLS AT SORRENTO", "id": "01589440", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01589440", "obs": "',
                   MD_OBS[match("01589440", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01589440", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01589440.png"}, "geometry": {"type": "Point", "coordinates": [-76.660944, 39.391722]}}, {"type": "Feature", "properties": {"name": "GWYNNS FALLS AT VILLA NOVA", "id": "01589300", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01589300", "obs": "',
                   MD_OBS[match("01589300", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01589300", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01589300.png"}, "geometry": {"type": "Point", "coordinates": [-76.733194, 39.345889]}}, {"type": "Feature", "properties": {"name": "DEAD RUN AT FRANKLINTOWN", "id": "01589330", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01589330", "obs": "',
                   MD_OBS[match("01589330", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01589330", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01589330.png"}, "geometry": {"type": "Point", "coordinates": [-76.716639, 39.311222]}}, {"type": "Feature", "properties": {"name": "PATAPSCO RIVER AT HOLLOFIELD", "id": "01589000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01589000", "obs": "',
                   MD_OBS[match("01589000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01589000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01589000.png"}, "geometry": {"type": "Point", "coordinates": [-76.792417, 39.310306]}}, {"type": "Feature", "properties": {"name": "EAST BRANCH HERBERT RUN AT ARBUTUS", "id": "01589100", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01589100", "obs": "',
                   MD_OBS[match("01589100", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01589100", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01589100.png"}, "geometry": {"type": "Point", "coordinates": [-76.692194, 39.24]}}, {"type": "Feature", "properties": {"name": "SAWMILL CREEK AT GLEN BURNIE", "id": "01589500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01589500", "obs": "',
                   MD_OBS[match("01589500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01589500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01589500.png"}, "geometry": {"type": "Point", "coordinates": [-76.630611, 39.17]}}, {"type": "Feature", "properties": {"name": "PATUXENT RIVER NEAR BOWIE", "id": "01594440", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01594440", "obs": "',
                   MD_OBS[match("01594440", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01594440", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01594440.png"}, "geometry": {"type": "Point", "coordinates": [-76.693694, 38.955917]}}, {"type": "Feature", "properties": {"name": "ST MARYS RIVER AT GREAT MILLS", "id": "01661500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01661500", "obs": "',
                   MD_OBS[match("01661500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01661500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01661500.png"}, "geometry": {"type": "Point", "coordinates": [-76.503667, 38.24175]}}, {"type": "Feature", "properties": {"name": "ST CLEMENT CREEK NEAR CLEMENTS", "id": "01661050", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01661050", "obs": "',
                   MD_OBS[match("01661050", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01661050", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01661050.png"}, "geometry": {"type": "Point", "coordinates": [-76.725, 38.333306]}}, {"type": "Feature", "properties": {"name": "ZEKIAH SWAMP RUN NEAR NEWTOWN", "id": "01660920", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01660920", "obs": "',
                   MD_OBS[match("01660920", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01660920", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01660920.png"}, "geometry": {"type": "Point", "coordinates": [-76.927083, 38.490583]}}, {"type": "Feature", "properties": {"name": "MATTAWOMAN CREEK NEAR POMONKEY", "id": "01658000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01658000", "obs": "',
                   MD_OBS[match("01658000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01658000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01658000.png"}, "geometry": {"type": "Point", "coordinates": [-77.056028, 38.596139]}}, {"type": "Feature", "properties": {"name": "PISCATAWAY CREEK AT PISCATAWAY", "id": "01653600", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01653600", "obs": "',
                   MD_OBS[match("01653600", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01653600", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01653600.png"}, "geometry": {"type": "Point", "coordinates": [-76.966194, 38.705778]}}, {"type": "Feature", "properties": {"name": "NORTHEAST BRANCH ANACOSTIA RIVER AT RIVERDALE", "id": "01649500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01649500", "obs": "',
                   MD_OBS[match("01649500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01649500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01649500.png"}, "geometry": {"type": "Point", "coordinates": [-76.925972, 38.96025]}}, {"type": "Feature", "properties": {"name": "NORTHWEST BR ANACOSTIA RIVER NR HYATTSVILLE", "id": "01651000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01651000", "obs": "',
                   MD_OBS[match("01651000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01651000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01651000.png"}, "geometry": {"type": "Point", "coordinates": [-76.966056, 38.952333]}}, {"type": "Feature", "properties": {"name": "NORTHWEST BRANCH ANACOSTIA RIVER NR COLESVILLE", "id": "01650500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01650500", "obs": "',
                   MD_OBS[match("01650500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01650500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01650500.png"}, "geometry": {"type": "Point", "coordinates": [-77.029333, 39.065667]}}, {"type": "Feature", "properties": {"name": "SENECA CREEK AT DAWSONVILLE", "id": "01645000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01645000", "obs": "',
                   MD_OBS[match("01645000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01645000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01645000.png"}, "geometry": {"type": "Point", "coordinates": [-77.335778, 39.128083]}}, {"type": "Feature", "properties": {"name": "HAWLINGS RIVER NEAR SANDY SPRING", "id": "01591700", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01591700", "obs": "',
                   MD_OBS[match("01591700", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01591700", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01591700.png"}, "geometry": {"type": "Point", "coordinates": [-77.021583, 39.174667]}}, {"type": "Feature", "properties": {"name": "PATUXENT RIVER NEAR UNITY", "id": "01591000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01591000", "obs": "',
                   MD_OBS[match("01591000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01591000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01591000.png"}, "geometry": {"type": "Point", "coordinates": [-77.055722, 39.23825]}}, {"type": "Feature", "properties": {"name": "PATUXENT RIVER BELOW BRIGHTON DAM NEAR BRIGHTON", "id": "01591610", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01591610", "obs": "',
                   MD_OBS[match("01591610", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01591610", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01591610.png"}, "geometry": {"type": "Point", "coordinates": [-77.004389, 39.192194]}}, {"type": "Feature", "properties": {"name": "CATTAIL CREEK NEAR GLENWOOD", "id": "01591400", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01591400", "obs": "',
                   MD_OBS[match("01591400", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01591400", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01591400.png"}, "geometry": {"type": "Point", "coordinates": [-77.051056, 39.255972]}}, {"type": "Feature", "properties": {"name": "LITTLE PATUXENT RIVER AT GUILFORD", "id": "01593500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01593500", "obs": "',
                   MD_OBS[match("01593500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01593500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01593500.png"}, "geometry": {"type": "Point", "coordinates": [-76.85125, 39.16775]}}, {"type": "Feature", "properties": {"name": "LITTLE PATUXENT RIVER AT SAVAGE", "id": "01594000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01594000", "obs": "',
                   MD_OBS[match("01594000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01594000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01594000.png"}, "geometry": {"type": "Point", "coordinates": [-76.816167, 39.134417]}}, {"type": "Feature", "properties": {"name": "POTOMAC RIVER AT POINT OF ROCKS", "id": "01638500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01638500", "obs": "',
                   MD_OBS[match("01638500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01638500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01638500.png"}, "geometry": {"type": "Point", "coordinates": [-77.543111, 39.273583]}}, {"type": "Feature", "properties": {"name": "ENNETT CREEK AT PARK MILLS", "id": "01643500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01643500", "obs": "',
                   MD_OBS[match("01643500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01643500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01643500.png"}, "geometry": {"type": "Point", "coordinates": [-77.407083, 39.294139]}}, {"type": "Feature", "properties": {"name": "MONOCACY RIVER AT JUG BRIDGE NEAR FREDERICK", "id": "01643000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01643000", "obs": "',
                   MD_OBS[match("01643000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01643000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01643000.png"}, "geometry": {"type": "Point", "coordinates": [-77.366083, 39.402833]}}, {"type": "Feature", "properties": {"name": "CATOCTIN CREEK NEAR MIDDLETOWN", "id": "01637500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01637500", "obs": "',
                   MD_OBS[match("01637500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01637500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01637500.png"}, "geometry": {"type": "Point", "coordinates": [-77.556167, 39.42725]}}, {"type": "Feature", "properties": {"name": "MORGAN RUN NEAR LOUISVILLE", "id": "01586610", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01586610", "obs": "',
                   MD_OBS[match("01586610", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01586610", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01586610.png"}, "geometry": {"type": "Point", "coordinates": [-76.955306, 39.451889]}}, {"type": "Feature", "properties": {"name": "NORTH BRANCH PATAPSCO RIVER AT CEDARHURST", "id": "01586000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01586000", "obs": "',
                   MD_OBS[match("01586000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01586000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01586000.png"}, "geometry": {"type": "Point", "coordinates": [-76.884861, 39.503667]}}, {"type": "Feature", "properties": {"name": "BEAVER RUN NEAR FINKSBURG", "id": "01586210", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01586210", "obs": "',
                   MD_OBS[match("01586210", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01586210", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01586210.png"}, "geometry": {"type": "Point", "coordinates": [-76.902944, 39.489444]}}, {"type": "Feature", "properties": {"name": "CRANBERRY BRANCH NEAR WESTMINSTER", "id": "01585500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01585500", "obs": "',
                   MD_OBS[match("01585500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01585500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01585500.png"}, "geometry": {"type": "Point", "coordinates": [-76.967528, 39.593333]}}, {"type": "Feature", "properties": {"name": "BIG PIPE CREEK AT BRUCEVILLE", "id": "01639500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01639500", "obs": "',
                   MD_OBS[match("01639500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01639500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01639500.png"}, "geometry": {"type": "Point", "coordinates": [-77.237444, 39.612361]}}, {"type": "Feature", "properties": {"name": "MONOCACY RIVER AT BRIDGEPORT", "id": "01639000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01639000", "obs": "',
                   MD_OBS[match("01639000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01639000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01639000.png"}, "geometry": {"type": "Point", "coordinates": [-77.234528, 39.679056]}}, {"type": "Feature", "properties": {"name": "ANTIETAM CREEK NEAR SHARPSBURG", "id": "01619500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01619500", "obs": "',
                   MD_OBS[match("01619500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01619500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01619500.png"}, "geometry": {"type": "Point", "coordinates": [-77.730194, 39.449778]}}, {"type": "Feature", "properties": {"name": "MARSH RUN AT GRIMES", "id": "01617800", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01617800", "obs": "',
                   MD_OBS[match("01617800", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01617800", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01617800.png"}, "geometry": {"type": "Point", "coordinates": [-77.777222, 39.514556]}}, {"type": "Feature", "properties": {"name": "CONOCOCHEAGUE CREEK AT FAIRVIEW", "id": "01614500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01614500", "obs": "',
                   MD_OBS[match("01614500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01614500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01614500.png"}, "geometry": {"type": "Point", "coordinates": [-77.824778, 39.716389]}}, {"type": "Feature", "properties": {"name": "POTOMAC RIVER AT HANCOCK", "id": "01613000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01613000", "obs": "',
                   MD_OBS[match("01613000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01613000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01613000.png"}, "geometry": {"type": "Point", "coordinates": [-78.177889, 39.697556]}}, {"type": "Feature", "properties": {"name": "TOWN CREEK NEAR OLDTOWN", "id": "01609000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01609000", "obs": "',
                   MD_OBS[match("01609000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01609000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01609000.png"}, "geometry": {"type": "Point", "coordinates": [-78.555, 39.553222]}}, {"type": "Feature", "properties": {"name": "NORTH BRANCH POTOMAC RIVER NEAR CUMBERLAND", "id": "01603000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01603000", "obs": "',
                   MD_OBS[match("01603000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01603000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01603000.png"}, "geometry": {"type": "Point", "coordinates": [-78.773417, 39.621806]}}, {"type": "Feature", "properties": {"name": "WILLS CREEK NEAR CUMBERLAND", "id": "01601500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01601500", "obs": "',
                   MD_OBS[match("01601500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01601500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01601500.png"}, "geometry": {"type": "Point", "coordinates": [-78.788028, 39.669611]}}, {"type": "Feature", "properties": {"name": "GEORGES CREEK AT FRANKLIN", "id": "01599000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01599000", "obs": "',
                   MD_OBS[match("01599000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01599000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01599000.png"}, "geometry": {"type": "Point", "coordinates": [-79.044694, 39.493917]}}, {"type": "Feature", "properties": {"name": "NORTH BRANCH POTOMAC RIVER AT LUKE", "id": "01598500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01598500", "obs": "',
                   MD_OBS[match("01598500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01598500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01598500.png"}, "geometry": {"type": "Point", "coordinates": [-79.063778, 39.478972]}}, {"type": "Feature", "properties": {"name": "SAVAGE RIV BL SAVAGE RIV DAM NEAR BLOOMINGTON", "id": "01597500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01597500", "obs": "',
                   MD_OBS[match("01597500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01597500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01597500.png"}, "geometry": {"type": "Point", "coordinates": [-79.123972, 39.50275]}}, {"type": "Feature", "properties": {"name": "SAVAGE RIVER NEAR BARTON", "id": "01596500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01596500", "obs": "',
                   MD_OBS[match("01596500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01596500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01596500.png"}, "geometry": {"type": "Point", "coordinates": [-79.101944, 39.570056]}}, {"type": "Feature", "properties": {"name": "CASSELMAN RIVER AT GRANTSVILLE", "id": "03078000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=03078000", "obs": "',
                   MD_OBS[match("03078000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("03078000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03078000.png"}, "geometry": {"type": "Point", "coordinates": [-79.136389, 39.702194]}}, {"type": "Feature", "properties": {"name": "BEAR CREEK AT FRIENDSVILLE", "id": "03076600", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=03076600", "obs": "',
                   MD_OBS[match("03076600", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("03076600", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03076600.png"}, "geometry": {"type": "Point", "coordinates": [-79.394111, 39.656139]}}, {"type": "Feature", "properties": {"name": "YOUGHIOGHENY RIVER AT FRIENDSVILLE", "id": "03076500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=03076500", "obs": "',
                   MD_OBS[match("03076500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("03076500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03076500.png"}, "geometry": {"type": "Point", "coordinates": [-79.408306, 39.653611]}}, {"type": "Feature", "properties": {"name": "NORTH BRANCH POTOMAC RIVER AT KITZMILLER", "id": "01595500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01595500", "obs": "',
                   MD_OBS[match("01595500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01595500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01595500.png"}, "geometry": {"type": "Point", "coordinates": [-79.181694, 39.393889]}}, {"type": "Feature", "properties": {"name": "NORTH BRANCH POTOMAC RIVER AT STEYER", "id": "01595000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01595000", "obs": "',
                   MD_OBS[match("01595000", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("01595000", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01595000.png"}, "geometry": {"type": "Point", "coordinates": [-79.306889, 39.301889]}}, {"type": "Feature", "properties": {"name": "YOUGHIOGHENY RIVER NEAR OAKLAND", "id": "03075500", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=03075500", "obs": "',
                   MD_OBS[match("03075500", MD_OBS[,1]), 2], '", "time": "', MD_OBS[match("03075500", MD_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03075500.png"}, "geometry": {"type": "Point", "coordinates": [-79.423639, 39.421583]}}]};', sep="")

# Export data to geojson.
cat(json_merge, file="/home/staff/klr324/marisa.psu.edu/mapdata/NJMD_stream_obs.js")
# --------------------------------------------------------------------------------------------------------------------
