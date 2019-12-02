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
if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata")){
  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata")
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
# <!-- Pennsylvania -->
PA_ID = c("04213075", "03021350", "03102500", "03015500", "03015000", "03011800",
          "03010500", "03007800", "03010655", "01544500", "01518862", "01520000", "01518000", "01518700", "01516350",
          "01516500", "01532000", "01531500", "01534300", "01428750", "01429000", "01429500", "01430000", "01431500",
          "01534500", "01536000", "01534000", "01533400", "01552500", "01552000", "01551500", "01550000", "01549500",
          "01549700", "01548500", "01548005", "01547700", "01547500", "01547950", "01545600", "01545500", "01545000",
          "01544000", "01543500", "01543000", "01542810", "03026500", "03027500", "03028500", "03028000", "03016000",
          "03020000", "03020500", "03025500", "03024000", "03101500", "03102850", "03103500", "03105500",
          "03106500",
          "03106300", "03049000", "03031500", "03030500", "03029500", "03034000", "01541000", "01541200",
          "01541303", "01541500", "01542500", "01546400", "01546500", "01547100", "01547200", "01555000", "01553500",
          "01554000", "01553700", "01540500", "01539000", "01538000", "01536500", "01447800", "01449000", "01447500",
          "01449800", "01450500", "01447720", "01447680", "01449360", "01440400", "01442500", "01439500", "01454700",
          "01453000", "01452500", "01451000", "01451650", "01451500", "01452000", "01451800", "01471510", "01471000",
          "01470960", "01470779", "01470500", "01468500", "01469500", "01573000", "01573560", "01555500", "01570500",
          "01567000", "01568000", "01567500", "01566000", "01565000", "01564500", "01563500", "01563200", "01559000",
          "01558000", "01557500", "01556000", "03040000", "03041500", "03042280", "03034500", "03042500",
          "03042000", "03038000", "03036000", "03036500", "03039000", "03048500", "03106000", "03107500", "03041000",
          "03108000", "03086000", "03085500", "03049800", "03075070", "03083500", "03072655", "03072000", "03074500",
          "03082500", "03049500", "03047000", "03044000", "03045000", "03081000", "03077500", "03080000", "03079000",
          "01601000", "01560000", "01562000", "01613050", "01570000", "01571500", "01574500", "01575500", "01574000",
          "01576000", "01576754", "01576500", "01480300", "01480500", "01480617", "01480675", "01480685", "01480700",
          "01480870", "01481000", "01472000", "01472157", "01473169", "01475850", "01477000", "01472199", "01472198",
          "01473000", "01464645", "01459500", "01474500", "01474000", "01467087", "01467086", "01467048", "01465798")

PA_TMP = mat.or.vec(length(PA_ID), 3)
PA_TMP[ ,1] = PA_ID; PA_DIS = PA_TMP; PA_GAG = PA_TMP
for(i in 1:length(PA_ID)){
  PA_TMP[i, 2:3] = usgs_dataRetrieveTemp(PA_ID[i], "00010", b.date, e.date, tz="America/New_York")
  PA_DIS[i, 2:3] = usgs_dataRetrieveTemp(PA_ID[i], "00060", b.date, e.date, tz="America/New_York")
  PA_GAG[i, 2:3] = usgs_dataRetrieveTemp(PA_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
PA_OBS = obs_string(PA_TMP, PA_DIS, PA_GAG)

# --------------------------------------------------------------------------------------------------------------------
# Create a geojson object with the observation and statement info and merge into a
# specific file format with adding stream temp and time.
json_merge = paste('PA_streamGauges = {"type": "FeatureCollection","features": [{"type": "Feature", "properties": {"name": "Brandy Run near Girard", "id": "04213075", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=04213075", "obs": "',
                   PA_OBS[match("04213075", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("04213075", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04213075.png"}, "geometry": {"type": "Point", "coordinates": [-80.291389, 41.991944]}}, {"type": "Feature", "properties": {"name": "French Creek near Wattsburg", "id": "03021350", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03021350", "obs": "',
                   PA_OBS[match("03021350", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03021350", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03021350.png"}, "geometry": {"type": "Point", "coordinates": [-79.782778, 42.015278]}}, {"type": "Feature", "properties": {"name": "Little Shenango River at Greenville", "id": "03102500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03102500", "obs": "',
                   PA_OBS[match("03102500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03102500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03102500.png"}, "geometry": {"type": "Point", "coordinates": [-80.376389, 41.421944]}}, {"type": "Feature", "properties": {"name": "Brokenstraw Creek at Youngsville", "id": "03015500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03015500", "obs": "',
                   PA_OBS[match("03015500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03015500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03015500.png"}, "geometry": {"type": "Point", "coordinates": [-79.3175, 41.8525]}}, {"type": "Feature", "properties": {"name": "Conewango Creek at Russell", "id": "03015000", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=03015000", "obs": "',
                   PA_OBS[match("03015000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03015000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03015000.png"}, "geometry": {"type": "Point", "coordinates": [-79.133333, 41.938056]}}, {"type": "Feature", "properties": {"name": "Kinzua Creek near Guffey", "id": "03011800", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03011800", "obs": "',
                   PA_OBS[match("03011800", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03011800", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03011800.png"}, "geometry": {"type": "Point", "coordinates": [-78.718889, 41.766389]}}, {"type": "Feature", "properties": {"name": "Allegheny River at Eldred", "id": "03010500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03010500", "obs": "',
                   PA_OBS[match("03010500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03010500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03010500.png"}, "geometry": {"type": "Point", "coordinates": [-78.386389, 41.963333]}}, {"type": "Feature", "properties": {"name": "Allegheny River at Port Allegany", "id": "03007800", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03007800", "obs": "',
                   PA_OBS[match("03007800", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03007800", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03007800.png"}, "geometry": {"type": "Point", "coordinates": [-78.293056, 41.818611]}}, {"type": "Feature", "properties": {"name": "Oswayo Creek at Shinglehouse", "id": "03010655", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03010655", "obs": "',
                   PA_OBS[match("03010655", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03010655", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03010655.png"}, "geometry": {"type": "Point", "coordinates": [-78.198333, 41.961667]}}, {"type": "Feature", "properties": {"name": "Kettle Creek at Cross Fork", "id": "01544500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01544500", "obs": "',
                   PA_OBS[match("01544500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01544500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01544500.png"}, "geometry": {"type": "Point", "coordinates": [-77.826111, 41.475833]}}, {"type": "Feature", "properties": {"name": "Cowanesque River at Westfield", "id": "01518862", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01518862", "obs": "',
                   PA_OBS[match("01518862", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01518862", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01518862.png"}, "geometry": {"type": "Point", "coordinates": [-77.532222, 41.923056]}}, {"type": "Feature", "properties": {"name": "Cowanesque River near Lawrenceville", "id": "01520000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01520000", "obs": "',
                   PA_OBS[match("01520000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01520000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01520000.png"}, "geometry": {"type": "Point", "coordinates": [-77.140278, 41.996667]}}, {"type": "Feature", "properties": {"name": "Tioga River at Tioga", "id": "01518000", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01518000", "obs": "',
                   PA_OBS[match("01518000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01518000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01518000.png"}, "geometry": {"type": "Point", "coordinates": [-77.129722, 41.908333]}}, {"type": "Feature", "properties": {"name": "Tioga River at Tioga Junction", "id": "01518700", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01518700", "obs": "',
                   PA_OBS[match("01518700", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01518700", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01518700.png"}, "geometry": {"type": "Point", "coordinates": [-77.115556, 41.9525]}}, {"type": "Feature", "properties": {"name": "Tioga River near Mansfield", "id": "01516350", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01516350", "obs": "',
                   PA_OBS[match("01516350", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01516350", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01516350.png"}, "geometry": {"type": "Point", "coordinates": [-77.080556, 41.796944]}}, {"type": "Feature", "properties": {"name": "Corey Creek near Mainesburg", "id": "01516500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01516500", "obs": "',
                   PA_OBS[match("01516500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01516500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01516500.png"}, "geometry": {"type": "Point", "coordinates": [-77.015, 41.790833]}}, {"type": "Feature", "properties": {"name": "Towanda Creek near Monroeton", "id": "01532000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01532000", "obs": "',
                   PA_OBS[match("01532000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01532000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01532000.png"}, "geometry": {"type": "Point", "coordinates": [-76.485, 41.706944]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Towanda", "id": "01531500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01531500", "obs": "',
                   PA_OBS[match("01531500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01531500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01531500.png"}, "geometry": {"type": "Point", "coordinates": [-76.441111, 41.765278]}}, {"type": "Feature", "properties": {"name": "Lackawanna River near Forest City", "id": "01534300", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01534300", "obs": "',
                   PA_OBS[match("01534300", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01534300", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01534300.png"}, "geometry": {"type": "Point", "coordinates": [-75.472222, 41.679722]}}, {"type": "Feature", "properties": {"name": "West Branch Lackawaxen River near Aldenville", "id": "01428750", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01428750", "obs": "',
                   PA_OBS[match("01428750", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01428750", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01428750.png"}, "geometry": {"type": "Point", "coordinates": [-75.376389, 41.674444]}}, {"type": "Feature", "properties": {"name": "West Branch Lackawaxen River at Prompton", "id": "01429000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01429000", "obs": "',
                   PA_OBS[match("01429000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01429000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01429000.png"}, "geometry": {"type": "Point", "coordinates": [-75.327222, 41.587222]}}, {"type": "Feature", "properties": {"name": "Dyberry Creek near Honesdale", "id": "01429500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01429500", "obs": "',
                   PA_OBS[match("01429500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01429500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01429500.png"}, "geometry": {"type": "Point", "coordinates": [-75.2675, 41.607222]}}, {"type": "Feature", "properties": {"name": "Lackawaxen River near Honesdale", "id": "01430000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01430000", "obs": "',
                   PA_OBS[match("01430000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01430000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01430000.png"}, "geometry": {"type": "Point", "coordinates": [-75.248333, 41.561944]}}, {"type": "Feature", "properties": {"name": "Lackawaxen River at Hawley", "id": "01431500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01431500", "obs": "',
                   PA_OBS[match("01431500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01431500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01431500.png"}, "geometry": {"type": "Point", "coordinates": [-75.1725, 41.476111]}}, {"type": "Feature", "properties": {"name": "Lackawanna River at Archbald", "id": "01534500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01534500", "obs": "',
                   PA_OBS[match("01534500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01534500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01534500.png"}, "geometry": {"type": "Point", "coordinates": [-75.5425, 41.504444]}}, {"type": "Feature", "properties": {"name": "Lackawanna River at Old Forge", "id": "01536000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01536000", "obs": "',
                   PA_OBS[match("01536000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01536000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01536000.png"}, "geometry": {"type": "Point", "coordinates": [-75.744722, 41.359167]}}, {"type": "Feature", "properties": {"name": "Tunkhannock Creek near Tunkhannock", "id": "01534000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01534000", "obs": "',
                   PA_OBS[match("01534000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01534000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01534000.png"}, "geometry": {"type": "Point", "coordinates": [-75.895, 41.558333]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Meshoppen", "id": "01533400", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01533400", "obs": "',
                   PA_OBS[match("01533400", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01533400", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01533400.png"}, "geometry": {"type": "Point", "coordinates": [-76.050556, 41.607222]}}, {"type": "Feature", "properties": {"name": "Muncy Creek near Sonestown", "id": "01552500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01552500", "obs": "',
                   PA_OBS[match("01552500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01552500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01552500.png"}, "geometry": {"type": "Point", "coordinates": [-76.535, 41.356944]}}, {"type": "Feature", "properties": {"name": "Loyalsock Creek at Loyalsockville", "id": "01552000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01552000", "obs": "',
                   PA_OBS[match("01552000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01552000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01552000.png"}, "geometry": {"type": "Point", "coordinates": [-76.912778, 41.325]}}, {"type": "Feature", "properties": {"name": "WB Susquehanna River at Williamsport", "id": "01551500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01551500", "obs": "',
                   PA_OBS[match("01551500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01551500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01551500.png"}, "geometry": {"type": "Point", "coordinates": [-76.996944, 41.236111]}}, {"type": "Feature", "properties": {"name": "Lycoming Creek near Trout Run", "id": "01550000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01550000", "obs": "',
                   PA_OBS[match("01550000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01550000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01550000.png"}, "geometry": {"type": "Point", "coordinates": [-77.033056, 41.418333]}}, {"type": "Feature", "properties": {"name": "Blockhouse Creek near English Center", "id": "01549500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01549500", "obs": "',
                   PA_OBS[match("01549500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01549500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01549500.png"}, "geometry": {"type": "Point", "coordinates": [-77.231111, 41.473611]}}, {"type": "Feature", "properties": {"name": "Pine Creek bl L Pine Creek near Waterville", "id": "01549700", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01549700", "obs": "',
                   PA_OBS[match("01549700", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01549700", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01549700.png"}, "geometry": {"type": "Point", "coordinates": [-77.324444, 41.273611]}}, {"type": "Feature", "properties": {"name": "Pine Creek at Cedar Run", "id": "01548500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01548500", "obs": "',
                   PA_OBS[match("01548500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01548500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01548500.png"}, "geometry": {"type": "Point", "coordinates": [-77.447778, 41.521667]}}, {"type": "Feature", "properties": {"name": "Bald Eagle Creek near Beech Creek Station", "id": "01548005", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01548005", "obs": "',
                   PA_OBS[match("01548005", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01548005", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01548005.png"}, "geometry": {"type": "Point", "coordinates": [-77.549722, 41.080833]}}, {"type": "Feature", "properties": {"name": "Marsh Creek at Blanchard", "id": "01547700", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01547700", "obs": "',
                   PA_OBS[match("01547700", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01547700", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01547700.png"}, "geometry": {"type": "Point", "coordinates": [-77.606111, 41.059444]}}, {"type": "Feature", "properties": {"name": "Bald Eagle Creek at Blanchard", "id": "01547500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01547500", "obs": "',
                   PA_OBS[match("01547500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01547500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01547500.png"}, "geometry": {"type": "Point", "coordinates": [-77.604722, 41.051667]}}, {"type": "Feature", "properties": {"name": "Beech Creek at Monument", "id": "01547950", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01547950", "obs": "',
                   PA_OBS[match("01547950", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01547950", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01547950.png"}, "geometry": {"type": "Point", "coordinates": [-77.7025, 41.111667]}}, {"type": "Feature", "properties": {"name": "Young Womans Creek near Renovo", "id": "01545600", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01545600", "obs": "',
                   PA_OBS[match("01545600", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01545600", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01545600.png"}, "geometry": {"type": "Point", "coordinates": [-77.691111, 41.389444]}}, {"type": "Feature", "properties": {"name": "West Branch Susquehanna River at Renovo", "id": "01545500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01545500", "obs": "',
                   PA_OBS[match("01545500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01545500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01545500.png"}, "geometry": {"type": "Point", "coordinates": [-77.750278, 41.323611]}}, {"type": "Feature", "properties": {"name": "Kettle Creek near Westport", "id": "01545000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01545000", "obs": "',
                   PA_OBS[match("01545000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01545000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01545000.png"}, "geometry": {"type": "Point", "coordinates": [-77.874167, 41.319444]}}, {"type": "Feature", "properties": {"name": "First Fork Sinnemahoning Cr near Sinnemahoning", "id": "01544000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01544000", "obs": "',
                   PA_OBS[match("01544000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01544000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01544000.png"}, "geometry": {"type": "Point", "coordinates": [-78.024444, 41.401667]}}, {"type": "Feature", "properties": {"name": "Sinnemahoning Creek at Sinnemahoning", "id": "01543500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01543500", "obs": "',
                   PA_OBS[match("01543500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01543500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01543500.png"}, "geometry": {"type": "Point", "coordinates": [-78.103333, 41.317222]}}, {"type": "Feature", "properties": {"name": "Driftwood Br Sinnemahoning Cr at Sterling Run", "id": "01543000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01543000", "obs": "',
                   PA_OBS[match("01543000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01543000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01543000.png"}, "geometry": {"type": "Point", "coordinates": [-78.197222, 41.413333]}}, {"type": "Feature", "properties": {"name": "Waldy Run near Emporium", "id": "01542810", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01542810", "obs": "',
                   PA_OBS[match("01542810", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01542810", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01542810.png"}, "geometry": {"type": "Point", "coordinates": [-78.292778, 41.578889]}}, {"type": "Feature", "properties": {"name": "Sevenmile Run near Rasselas", "id": "03026500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03026500", "obs": "',
                   PA_OBS[match("03026500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03026500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03026500.png"}, "geometry": {"type": "Point", "coordinates": [-78.576944, 41.631111]}}, {"type": "Feature", "properties": {"name": "EB Clarion River at EB Clarion River Dam", "id": "03027500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03027500", "obs": "',
                   PA_OBS[match("03027500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03027500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03027500.png"}, "geometry": {"type": "Point", "coordinates": [-78.596389, 41.553056]}}, {"type": "Feature", "properties": {"name": "Clarion River at Johnsonburg", "id": "03028500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03028500", "obs": "',
                   PA_OBS[match("03028500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03028500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03028500.png"}, "geometry": {"type": "Point", "coordinates": [-78.678611, 41.486111]}}, {"type": "Feature", "properties": {"name": "West Branch Clarion River at Wilcox", "id": "03028000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03028000", "obs": "',
                   PA_OBS[match("03028000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03028000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03028000.png"}, "geometry": {"type": "Point", "coordinates": [-78.6925, 41.575278]}}, {"type": "Feature", "properties": {"name": "Allegheny River at West Hickory", "id": "03016000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03016000", "obs": "',
                   PA_OBS[match("03016000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03016000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03016000.png"}, "geometry": {"type": "Point", "coordinates": [-79.408056, 41.570833]}}, {"type": "Feature", "properties": {"name": "Tionesta Creek at Tionesta Creek Dam", "id": "03020000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03020000", "obs": "',
                   PA_OBS[match("03020000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03020000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03020000.png"}, "geometry": {"type": "Point", "coordinates": [-79.444167, 41.477778]}}, {"type": "Feature", "properties": {"name": "Oil Creek at Rouseville", "id": "03020500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03020500", "obs": "',
                   PA_OBS[match("03020500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03020500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03020500.png"}, "geometry": {"type": "Point", "coordinates": [-79.695556, 41.481667]}}, {"type": "Feature", "properties": {"name": "Allegheny River at Franklin", "id": "03025500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03025500", "obs": "',
                   PA_OBS[match("03025500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03025500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03025500.png"}, "geometry": {"type": "Point", "coordinates": [-79.820556, 41.389444]}}, {"type": "Feature", "properties": {"name": "French Creek at Utica", "id": "03024000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03024000", "obs": "',
                   PA_OBS[match("03024000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03024000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03024000.png"}, "geometry": {"type": "Point", "coordinates": [-79.956111, 41.4375]}}, {"type": "Feature", "properties": {"name": "Shenango River at Pymatuning Dam", "id": "03101500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03101500", "obs": "',
                   PA_OBS[match("03101500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03101500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03101500.png"}, "geometry": {"type": "Point", "coordinates": [-80.460278, 41.498056]}}, {"type": "Feature", "properties": {"name": "Shenango River near Transfer", "id": "03102850", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03102850", "obs": "',
                   PA_OBS[match("03102850", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03102850", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03102850.png"}, "geometry": {"type": "Point", "coordinates": [-80.398056, 41.353611]}}, {"type": "Feature", "properties": {"name": "Shenango River at Sharpsville", "id": "03103500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03103500", "obs": "',
                   PA_OBS[match("03103500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03103500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03103500.png"}, "geometry": {"type": "Point", "coordinates": [-80.472778, 41.266111]}}, {"type": "Feature", "properties": {"name": "Beaver River at Wampum", "id": "03105500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03105500", "obs": "',
                   PA_OBS[match("03105500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03105500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03105500.png"}, "geometry": {"type": "Point", "coordinates": [-80.337222, 40.888611]}}, {"type": "Feature", "properties": {"name": "Slippery Rock Creek at Wurtemburg", "id": "03106500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03106500", "obs": "',
                   PA_OBS[match("03106500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03106500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03106500.png"}, "geometry": {"type": "Point", "coordinates": [-80.233889, 40.883889]}}, {"type": "Feature", "properties": {"name": "Muddy Creek near Portersville", "id": "03106300", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03106300", "obs": "',
                   PA_OBS[match("03106300", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03106300", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03106300.png"}, "geometry": {"type": "Point", "coordinates": [-80.125278, 40.963056]}}, {"type": "Feature", "properties": {"name": "Buffalo Creek near Freeport", "id": "03049000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03049000", "obs": "',
                   PA_OBS[match("03049000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03049000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03049000.png"}, "geometry": {"type": "Point", "coordinates": [-79.699917, 40.716444]}}, {"type": "Feature", "properties": {"name": "Allegheny River at Parker", "id": "03031500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03031500", "obs": "',
                   PA_OBS[match("03031500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03031500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03031500.png"}, "geometry": {"type": "Point", "coordinates": [-79.681389, 41.100556]}}, {"type": "Feature", "properties": {"name": "Clarion River near Piney", "id": "03030500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03030500", "obs": "',
                   PA_OBS[match("03030500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03030500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03030500.png"}, "geometry": {"type": "Point", "coordinates": [-79.440278, 41.1925]}}, {"type": "Feature", "properties": {"name": "Clarion River at Cooksburg", "id": "03029500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03029500", "obs": "',
                   PA_OBS[match("03029500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03029500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03029500.png"}, "geometry": {"type": "Point", "coordinates": [-79.209167, 41.330556]}}, {"type": "Feature", "properties": {"name": "Mahoning Creek at Punxsutawney", "id": "03034000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03034000", "obs": "',
                   PA_OBS[match("03034000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03034000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03034000.png"}, "geometry": {"type": "Point", "coordinates": [-79.008611, 40.939167]}}, {"type": "Feature", "properties": {"name": "West Branch Susquehanna River at Bower", "id": "01541000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01541000", "obs": "',
                   PA_OBS[match("01541000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01541000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01541000.png"}, "geometry": {"type": "Point", "coordinates": [-78.677222, 40.896944]}}, {"type": "Feature", "properties": {"name": "WB Susquehanna River near Curwensville", "id": "01541200", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01541200", "obs": "',
                   PA_OBS[match("01541200", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01541200", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01541200.png"}, "geometry": {"type": "Point", "coordinates": [-78.519444, 40.961389]}}, {"type": "Feature", "properties": {"name": "West Branch Susquehanna River at Hyde", "id": "01541303", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01541303", "obs": "',
                   PA_OBS[match("01541303", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01541303", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01541303.png"}, "geometry": {"type": "Point", "coordinates": [-78.456944, 41.004444]}}, {"type": "Feature", "properties": {"name": "Clearfield Creek at Dimeling", "id": "01541500", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01541500", "obs": "',
                   PA_OBS[match("01541500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01541500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01541500.png"}, "geometry": {"type": "Point", "coordinates": [-78.406111, 40.971667]}}, {"type": "Feature", "properties": {"name": "WB Susquehanna River at Karthaus", "id": "01542500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01542500", "obs": "',
                   PA_OBS[match("01542500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01542500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01542500.png"}, "geometry": {"type": "Point", "coordinates": [-78.109167, 41.1175]}}, {"type": "Feature", "properties": {"name": "Spring Creek at Houserville", "id": "01546400", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01546400", "obs": "',
                   PA_OBS[match("01546400", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01546400", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01546400.png"}, "geometry": {"type": "Point", "coordinates": [-77.827778, 40.833611]}}, {"type": "Feature", "properties": {"name": "Spring Creek near Axemann", "id": "01546500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01546500", "obs": "',
                   PA_OBS[match("01546500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01546500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01546500.png"}, "geometry": {"type": "Point", "coordinates": [-77.794444, 40.889722]}}, {"type": "Feature", "properties": {"name": "Spring Creek at Milesburg", "id": "01547100", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01547100", "obs": "',
                   PA_OBS[match("01547100", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01547100", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01547100.png"}, "geometry": {"type": "Point", "coordinates": [-77.785833, 40.931667]}}, {"type": "Feature", "properties": {"name": "Bald Eagle Creek bl Spring Creek at Milesburg", "id": "01547200", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01547200", "obs": "',
                   PA_OBS[match("01547200", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01547200", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01547200.png"}, "geometry": {"type": "Point", "coordinates": [-77.786667, 40.943056]}}, {"type": "Feature", "properties": {"name": "Penns Creek at Penns Creek", "id": "01555000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01555000", "obs": "',
                   PA_OBS[match("01555000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01555000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01555000.png"}, "geometry": {"type": "Point", "coordinates": [-77.048611, 40.866667]}}, {"type": "Feature", "properties": {"name": "West Branch Susquehanna River at Lewisburg", "id": "01553500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01553500", "obs": "',
                   PA_OBS[match("01553500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01553500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01553500.png"}, "geometry": {"type": "Point", "coordinates": [-76.876667, 40.9675]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Sunbury", "id": "01554000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01554000", "obs": "',
                   PA_OBS[match("01554000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01554000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01554000.png"}, "geometry": {"type": "Point", "coordinates": [-76.826944, 40.834444]}}, {"type": "Feature", "properties": {"name": "Chillisquaque Creek at Washingtonville", "id": "01553700", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01553700", "obs": "',
                   PA_OBS[match("01553700", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01553700", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01553700.png"}, "geometry": {"type": "Point", "coordinates": [-76.680556, 41.061667]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Danville", "id": "01540500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01540500", "obs": "',
                   PA_OBS[match("01540500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01540500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01540500.png"}, "geometry": {"type": "Point", "coordinates": [-76.619444, 40.958056]}}, {"type": "Feature", "properties": {"name": "Fishing Creek near Bloomsburg", "id": "01539000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01539000", "obs": "',
                   PA_OBS[match("01539000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01539000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01539000.png"}, "geometry": {"type": "Point", "coordinates": [-76.431389, 41.078056]}}, {"type": "Feature", "properties": {"name": "Wapwallopen Creek near Wapwallopen", "id": "01538000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01538000", "obs": "',
                   PA_OBS[match("01538000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01538000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01538000.png"}, "geometry": {"type": "Point", "coordinates": [-76.093889, 41.059167]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Wilkes-Barre", "id": "01536500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01536500", "obs": "',
                   PA_OBS[match("01536500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01536500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01536500.png"}, "geometry": {"type": "Point", "coordinates": [-75.881111, 41.250833]}}, {"type": "Feature", "properties": {"name": "Lehigh R bl Francis E Walter Res nr White Haven", "id": "01447800", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01447800", "obs": "',
                   PA_OBS[match("01447800", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01447800", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01447800.png"}, "geometry": {"type": "Point", "coordinates": [-75.7325, 41.104722]}}, {"type": "Feature", "properties": {"name": "Lehigh River at Lehighton", "id": "01449000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01449000", "obs": "',
                   PA_OBS[match("01449000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01449000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01449000.png"}, "geometry": {"type": "Point", "coordinates": [-75.705556, 40.829167]}}, {"type": "Feature", "properties": {"name": "Lehigh River at Stoddartsville", "id": "01447500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01447500", "obs": "',
                   PA_OBS[match("01447500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01447500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01447500.png"}, "geometry": {"type": "Point", "coordinates": [-75.625833, 41.130278]}}, {"type": "Feature", "properties": {"name": "Pohopoco Cr bl Beltzville Dam nr Parryville", "id": "01449800", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01449800", "obs": "',
                   PA_OBS[match("01449800", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01449800", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01449800.png"}, "geometry": {"type": "Point", "coordinates": [-75.646111, 40.845556]}}, {"type": "Feature", "properties": {"name": "Aquashicola Creek at Palmerton", "id": "01450500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01450500", "obs": "',
                   PA_OBS[match("01450500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01450500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01450500.png"}, "geometry": {"type": "Point", "coordinates": [-75.598333, 40.806111]}}, {"type": "Feature", "properties": {"name": "Tobyhanna Creek near Blakeslee", "id": "01447720", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01447720", "obs": "',
                   PA_OBS[match("01447720", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01447720", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01447720.png"}, "geometry": {"type": "Point", "coordinates": [-75.605833, 41.084722]}}, {"type": "Feature", "properties": {"name": "Tunkhannock Creek near Long Pond", "id": "01447680", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01447680", "obs": "',
                   PA_OBS[match("01447680", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01447680", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01447680.png"}, "geometry": {"type": "Point", "coordinates": [-75.521389, 41.065278]}}, {"type": "Feature", "properties": {"name": "Pohopoco Creek at Kresgeville", "id": "01449360", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01449360", "obs": "',
                   PA_OBS[match("01449360", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01449360", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01449360.png"}, "geometry": {"type": "Point", "coordinates": [-75.502778, 40.8975]}}, {"type": "Feature", "properties": {"name": "Brodhead Creek near Analomink", "id": "01440400", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01440400", "obs": "',
                   PA_OBS[match("01440400", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01440400", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01440400.png"}, "geometry": {"type": "Point", "coordinates": [-75.215, 41.084722]}}, {"type": "Feature", "properties": {"name": "Brodhead Creek at Minisink Hills", "id": "01442500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01442500", "obs": "',
                   PA_OBS[match("01442500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01442500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01442500.png"}, "geometry": {"type": "Point", "coordinates": [-75.143056, 40.998611]}}, {"type": "Feature", "properties": {"name": "Bush Kill at Shoemakers", "id": "01439500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01439500", "obs": "',
                   PA_OBS[match("01439500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01439500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01439500.png"}, "geometry": {"type": "Point", "coordinates": [-75.038056, 41.088056]}}, {"type": "Feature", "properties": {"name": "Lehigh River at Glendon", "id": "01454700", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01454700", "obs": "',
                   PA_OBS[match("01454700", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01454700", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01454700.png"}, "geometry": {"type": "Point", "coordinates": [-75.236667, 40.669167]}}, {"type": "Feature", "properties": {"name": "Lehigh River at Bethlehem", "id": "01453000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01453000", "obs": "',
                   PA_OBS[match("01453000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01453000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01453000.png"}, "geometry": {"type": "Point", "coordinates": [-75.379167, 40.615278]}}, {"type": "Feature", "properties": {"name": "Monocacy Creek at Bethlehem", "id": "01452500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01452500", "obs": "',
                   PA_OBS[match("01452500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01452500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01452500.png"}, "geometry": {"type": "Point", "coordinates": [-75.379722, 40.641111]}}, {"type": "Feature", "properties": {"name": "Lehigh River at Walnutport", "id": "01451000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01451000", "obs": "',
                   PA_OBS[match("01451000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01451000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01451000.png"}, "geometry": {"type": "Point", "coordinates": [-75.603333, 40.756944]}}, {"type": "Feature", "properties": {"name": "Little Lehigh Creek at Tenth St. Br. at Allentown", "id": "01451650", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01451650", "obs": "',
                   PA_OBS[match("01451650", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01451650", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01451650.png"}, "geometry": {"type": "Point", "coordinates": [-75.474444, 40.596389]}}, {"type": "Feature", "properties": {"name": "Little Lehigh Creek near Allentown", "id": "01451500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01451500", "obs": "',
                   PA_OBS[match("01451500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01451500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01451500.png"}, "geometry": {"type": "Point", "coordinates": [-75.483333, 40.582222]}}, {"type": "Feature", "properties": {"name": "Jordan Creek at Allentown", "id": "01452000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01452000", "obs": "',
                   PA_OBS[match("01452000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01452000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01452000.png"}, "geometry": {"type": "Point", "coordinates": [-75.482778, 40.623056]}}, {"type": "Feature", "properties": {"name": "Jordan Creek near Schnecksville", "id": "01451800", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01451800", "obs": "',
                   PA_OBS[match("01451800", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01451800", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01451800.png"}, "geometry": {"type": "Point", "coordinates": [-75.627222, 40.661667]}}, {"type": "Feature", "properties": {"name": "Schuylkill River at Reading", "id": "01471510", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01471510", "obs": "',
                   PA_OBS[match("01471510", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01471510", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01471510.png"}, "geometry": {"type": "Point", "coordinates": [-75.936667, 40.334722]}}, {"type": "Feature", "properties": {"name": "Tulpehocken Creek near Reading", "id": "01471000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01471000", "obs": "',
                   PA_OBS[match("01471000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01471000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01471000.png"}, "geometry": {"type": "Point", "coordinates": [-75.979444, 40.368889]}}, {"type": "Feature", "properties": {"name": "Tulpehocken Cr at Blue Marsh Damsite near Reading", "id": "01470960", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01470960", "obs": "',
                   PA_OBS[match("01470960", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01470960", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01470960.png"}, "geometry": {"type": "Point", "coordinates": [-76.025556, 40.370556]}}, {"type": "Feature", "properties": {"name": "Tulpehocken Creek near Bernville", "id": "01470779", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01470779", "obs": "',
                   PA_OBS[match("01470779", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01470779", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01470779.png"}, "geometry": {"type": "Point", "coordinates": [-76.171944, 40.413333]}}, {"type": "Feature", "properties": {"name": "Schuylkill River at Berne", "id": "01470500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01470500", "obs": "',
                   PA_OBS[match("01470500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01470500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01470500.png"}, "geometry": {"type": "Point", "coordinates": [-75.998611, 40.5225]}}, {"type": "Feature", "properties": {"name": "Schuylkill River at Landingville", "id": "01468500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01468500", "obs": "',
                   PA_OBS[match("01468500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01468500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01468500.png"}, "geometry": {"type": "Point", "coordinates": [-76.125, 40.629167]}}, {"type": "Feature", "properties": {"name": "Little Schuylkill River at Tamaqua", "id": "01469500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01469500", "obs": "',
                   PA_OBS[match("01469500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01469500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01469500.png"}, "geometry": {"type": "Point", "coordinates": [-75.972222, 40.806944]}}, {"type": "Feature", "properties": {"name": "Swatara Creek at Harper Tavern", "id": "01573000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01573000", "obs": "',
                   PA_OBS[match("01573000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01573000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01573000.png"}, "geometry": {"type": "Point", "coordinates": [-76.5775, 40.4025]}}, {"type": "Feature", "properties": {"name": "Swatara Creek near Hershey", "id": "01573560", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01573560", "obs": "',
                   PA_OBS[match("01573560", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01573560", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01573560.png"}, "geometry": {"type": "Point", "coordinates": [-76.668056, 40.298333]}}, {"type": "Feature", "properties": {"name": "East Mahantango Creek near Dalmatia", "id": "01555500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01555500", "obs": "',
                   PA_OBS[match("01555500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01555500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01555500.png"}, "geometry": {"type": "Point", "coordinates": [-76.912222, 40.611111]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Harrisburg", "id": "01570500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01570500", "obs": "',
                   PA_OBS[match("01570500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01570500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01570500.png"}, "geometry": {"type": "Point", "coordinates": [-76.886389, 40.254722]}}, {"type": "Feature", "properties": {"name": "Juniata River at Newport", "id": "01567000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01567000", "obs": "',
                   PA_OBS[match("01567000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01567000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01567000.png"}, "geometry": {"type": "Point", "coordinates": [-77.129444, 40.478333]}}, {"type": "Feature", "properties": {"name": "Sherman Creek at Shermans Dale", "id": "01568000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01568000", "obs": "',
                   PA_OBS[match("01568000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01568000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01568000.png"}, "geometry": {"type": "Point", "coordinates": [-77.169167, 40.323333]}}, {"type": "Feature", "properties": {"name": "Bixler Run near Loysville", "id": "01567500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01567500", "obs": "',
                   PA_OBS[match("01567500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01567500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01567500.png"}, "geometry": {"type": "Point", "coordinates": [-77.4025, 40.370833]}}, {"type": "Feature", "properties": {"name": "Tuscarora Creek near Port Royal", "id": "01566000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01566000", "obs": "',
                   PA_OBS[match("01566000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01566000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01566000.png"}, "geometry": {"type": "Point", "coordinates": [-77.419444, 40.515278]}}, {"type": "Feature", "properties": {"name": "Kishacoquillas Creek at Reedsville", "id": "01565000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01565000", "obs": "',
                   PA_OBS[match("01565000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01565000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01565000.png"}, "geometry": {"type": "Point", "coordinates": [-77.583333, 40.654722]}}, {"type": "Feature", "properties": {"name": "Aughwick Creek near Three Springs", "id": "01564500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01564500", "obs": "',
                   PA_OBS[match("01564500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01564500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01564500.png"}, "geometry": {"type": "Point", "coordinates": [-77.925556, 40.2125]}}, {"type": "Feature", "properties": {"name": "Juniata River at Mapleton Depot", "id": "01563500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01563500", "obs": "',
                   PA_OBS[match("01563500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01563500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01563500.png"}, "geometry": {"type": "Point", "coordinates": [-77.935278, 40.392222]}}, {"type": "Feature", "properties": {"name": "Rays Br Juniata R bl Rays Dam nr Huntingdon", "id": "01563200", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01563200", "obs": "',
                   PA_OBS[match("01563200", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01563200", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01563200.png"}, "geometry": {"type": "Point", "coordinates": [-77.991389, 40.428889]}}, {"type": "Feature", "properties": {"name": "Juniata River at Huntingdon", "id": "01559000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01559000", "obs": "',
                   PA_OBS[match("01559000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01559000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01559000.png"}, "geometry": {"type": "Point", "coordinates": [-78.019167, 40.484722]}}, {"type": "Feature", "properties": {"name": "Little Juniata River at Spruce Creek", "id": "01558000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01558000", "obs": "',
                   PA_OBS[match("01558000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01558000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01558000.png"}, "geometry": {"type": "Point", "coordinates": [-78.140833, 40.6125]}}, {"type": "Feature", "properties": {"name": "Bald Eagle Creek at Tyrone", "id": "01557500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01557500", "obs": "',
                   PA_OBS[match("01557500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01557500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01557500.png"}, "geometry": {"type": "Point", "coordinates": [-78.233889, 40.683611]}}, {"type": "Feature", "properties": {"name": "Frankstown Br Juniata River at Williamsburg", "id": "01556000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01556000", "obs": "',
                   PA_OBS[match("01556000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01556000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01556000.png"}, "geometry": {"type": "Point", "coordinates": [-78.2, 40.463056]}}, {"type": "Feature", "properties": {"name": "Little Conemaugh River at East Conemaugh", "id": "03041000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03041000", "obs": "',
                   PA_OBS[match("03041000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03041000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03041000.png"}, "geometry": {"type": "Point", "coordinates": [-78.882778, 40.345833]}}, {"type": "Feature", "properties": {"name": "Stonycreek River at Ferndale", "id": "03040000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03040000", "obs": "',
                   PA_OBS[match("03040000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03040000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03040000.png"}, "geometry": {"type": "Point", "coordinates": [-78.920833, 40.285556]}}, {"type": "Feature", "properties": {"name": "Conemaugh River at Seward", "id": "03041500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03041500", "obs": "',
                   PA_OBS[match("03041500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03041500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03041500.png"}, "geometry": {"type": "Point", "coordinates": [-79.026389, 40.419167]}}, {"type": "Feature", "properties": {"name": "Yellow Creek near Homer City", "id": "03042280", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03042280", "obs": "',
                   PA_OBS[match("03042280", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03042280", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03042280.png"}, "geometry": {"type": "Point", "coordinates": [-79.103611, 40.5725]}}, {"type": "Feature", "properties": {"name": "Little Mahoning Creek at McCormick", "id": "03034500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03034500", "obs": "',
                   PA_OBS[match("03034500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03034500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03034500.png"}, "geometry": {"type": "Point", "coordinates": [-79.110278, 40.836111]}}, {"type": "Feature", "properties": {"name": "Two Lick Creek at Graceton", "id": "03042500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03042500", "obs": "',
                   PA_OBS[match("03042500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03042500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03042500.png"}, "geometry": {"type": "Point", "coordinates": [-79.171944, 40.517222]}}, {"type": "Feature", "properties": {"name": "Blacklick Creek at Josephine", "id": "03042000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03042000", "obs": "',
                   PA_OBS[match("03042000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03042000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03042000.png"}, "geometry": {"type": "Point", "coordinates": [-79.186944, 40.476944]}}, {"type": "Feature", "properties": {"name": "Crooked Creek at Idaho", "id": "03038000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03038000", "obs": "',
                   PA_OBS[match("03038000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03038000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03038000.png"}, "geometry": {"type": "Point", "coordinates": [-79.348889, 40.654722]}}, {"type": "Feature", "properties": {"name": "Mahoning Creek at Mahoning Creek Dam", "id": "03036000", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=03036000", "obs": "',
                   PA_OBS[match("03036000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03036000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03036000.png"}, "geometry": {"type": "Point", "coordinates": [-79.291389, 40.9275]}}, {"type": "Feature", "properties": {"name": "Allegheny River at Kittanning", "id": "03036500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03036500", "obs": "',
                   PA_OBS[match("03036500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03036500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03036500.png"}, "geometry": {"type": "Point", "coordinates": [-79.531667, 40.820278]}}, {"type": "Feature", "properties": {"name": "Crooked Creek at Crooked Creek Dam near Ford City", "id": "03039000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03039000", "obs": "',
                   PA_OBS[match("03039000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03039000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03039000.png"}, "geometry": {"type": "Point", "coordinates": [-79.511667, 40.720278]}}, {"type": "Feature", "properties": {"name": "Kiskiminetas River at Vandergrift", "id": "03048500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03048500", "obs": "',
                   PA_OBS[match("03048500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03048500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03048500.png"}, "geometry": {"type": "Point", "coordinates": [-79.552222, 40.604444]}}, {"type": "Feature", "properties": {"name": "Connoquenessing Creek near Zelienople", "id": "03106000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03106000", "obs": "',
                   PA_OBS[match("03106000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03106000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03106000.png"}, "geometry": {"type": "Point", "coordinates": [-80.2425, 40.816944]}}, {"type": "Feature", "properties": {"name": "Beaver River at Beaver Falls", "id": "03107500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03107500", "obs": "',
                   PA_OBS[match("03107500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03107500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03107500.png"}, "geometry": {"type": "Point", "coordinates": [-80.315278, 40.763333]}}, {"type": "Feature", "properties": {"name": "Raccoon Creek at Moffatts Mill", "id": "03108000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03108000", "obs": "',
                   PA_OBS[match("03108000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03108000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03108000.png"}, "geometry": {"type": "Point", "coordinates": [-80.337778, 40.627778]}}, {"type": "Feature", "properties": {"name": "Ohio River at Sewickley", "id": "03086000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03086000", "obs": "',
                   PA_OBS[match("03086000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03086000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03086000.png"}, "geometry": {"type": "Point", "coordinates": [-80.205833, 40.549167]}}, {"type": "Feature", "properties": {"name": "Chartiers Creek at Carnegie", "id": "03085500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03085500", "obs": "',
                   PA_OBS[match("03085500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03085500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03085500.png"}, "geometry": {"type": "Point", "coordinates": [-80.096667, 40.400556]}}, {"type": "Feature", "properties": {"name": "Little Pine Creek near Etna", "id": "03049800", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03049800", "obs": "',
                   PA_OBS[match("03049800", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03049800", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03049800.png"}, "geometry": {"type": "Point", "coordinates": [-79.938333, 40.520278]}}, {"type": "Feature", "properties": {"name": "Monongahela River at Elizabeth", "id": "03075070", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03075070", "obs": "',
                   PA_OBS[match("03075070", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03075070", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03075070.png"}, "geometry": {"type": "Point", "coordinates": [-79.901389, 40.262222]}}, {"type": "Feature", "properties": {"name": "Youghiogheny River at Sutersville", "id": "03083500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03083500", "obs": "',
                   PA_OBS[match("03083500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03083500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03083500.png"}, "geometry": {"type": "Point", "coordinates": [-79.806694, 40.240222]}}, {"type": "Feature", "properties": {"name": "Monongahela River near Masontown", "id": "03072655", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03072655", "obs": "',
                   PA_OBS[match("03072655", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03072655", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03072655.png"}, "geometry": {"type": "Point", "coordinates": [-79.923056, 39.825]}}, {"type": "Feature", "properties": {"name": "Dunkard Creek at Shannopin", "id": "03072000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03072000", "obs": "',
                   PA_OBS[match("03072000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03072000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03072000.png"}, "geometry": {"type": "Point", "coordinates": [-79.970833, 39.759167]}}, {"type": "Feature", "properties": {"name": "Redstone Creek at Waltersburg", "id": "03074500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03074500", "obs": "',
                   PA_OBS[match("03074500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03074500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03074500.png"}, "geometry": {"type": "Point", "coordinates": [-79.764444, 39.98]}}, {"type": "Feature", "properties": {"name": "Youghiogheny River at Connellsville", "id": "03082500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03082500", "obs": "',
                   PA_OBS[match("03082500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03082500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03082500.png"}, "geometry": {"type": "Point", "coordinates": [-79.593222, 40.017528]}}, {"type": "Feature", "properties": {"name": "Allegheny River at Natrona", "id": "03049500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03049500", "obs": "',
                   PA_OBS[match("03049500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03049500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03049500.png"}, "geometry": {"type": "Point", "coordinates": [-79.718611, 40.615278]}}, {"type": "Feature", "properties": {"name": "Loyalhanna Creek at Loyalhanna Dam", "id": "03047000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03047000", "obs": "',
                   PA_OBS[match("03047000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03047000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03047000.png"}, "geometry": {"type": "Point", "coordinates": [-79.449444, 40.458889]}}, {"type": "Feature", "properties": {"name": "Conemaugh River at Tunnelton", "id": "03044000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03044000", "obs": "',
                   PA_OBS[match("03044000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03044000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03044000.png"}, "geometry": {"type": "Point", "coordinates": [-79.391111, 40.454444]}}, {"type": "Feature", "properties": {"name": "Loyalhanna Creek at Kingston", "id": "03045000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03045000", "obs": "',
                   PA_OBS[match("03045000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03045000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03045000.png"}, "geometry": {"type": "Point", "coordinates": [-79.340833, 40.2925]}}, {"type": "Feature", "properties": {"name": "Youghiogheny River below Confluence", "id": "03081000", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=03081000", "obs": "',
                   PA_OBS[match("03081000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03081000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03081000.png"}, "geometry": {"type": "Point", "coordinates": [-79.372778, 39.8275]}}, {"type": "Feature", "properties": {"name": "Youghiogheny River at Youghiogheny River Dam", "id": "03077500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03077500", "obs": "',
                   PA_OBS[match("03077500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03077500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03077500.png"}, "geometry": {"type": "Point", "coordinates": [-79.364444, 39.805278]}}, {"type": "Feature", "properties": {"name": "Laurel Hill Creek at Ursina", "id": "03080000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03080000", "obs": "',
                   PA_OBS[match("03080000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03080000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03080000.png"}, "geometry": {"type": "Point", "coordinates": [-79.321667, 39.820278]}}, {"type": "Feature", "properties": {"name": "Casselman River at Markleton", "id": "03079000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=03079000", "obs": "',
                   PA_OBS[match("03079000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("03079000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03079000.png"}, "geometry": {"type": "Point", "coordinates": [-79.228833, 39.859917]}}, {"type": "Feature", "properties": {"name": "Wills Creek below Hyndman", "id": "01601000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01601000", "obs": "',
                   PA_OBS[match("01601000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01601000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01601000.png"}, "geometry": {"type": "Point", "coordinates": [-78.716667, 39.811944]}}, {"type": "Feature", "properties": {"name": "Dunning Creek at Belden", "id": "01560000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01560000", "obs": "',
                   PA_OBS[match("01560000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01560000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01560000.png"}, "geometry": {"type": "Point", "coordinates": [-78.492778, 40.071667]}}, {"type": "Feature", "properties": {"name": "Raystown Branch Juniata River at Saxton", "id": "01562000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01562000", "obs": "',
                   PA_OBS[match("01562000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01562000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01562000.png"}, "geometry": {"type": "Point", "coordinates": [-78.265556, 40.215833]}}, {"type": "Feature", "properties": {"name": "Tonoloway Creek near Needmore", "id": "01613050", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01613050", "obs": "',
                   PA_OBS[match("01613050", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01613050", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01613050.png"}, "geometry": {"type": "Point", "coordinates": [-78.1325, 39.898333]}}, {"type": "Feature", "properties": {"name": "Conodoguinet Creek near Hogestown", "id": "01570000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01570000", "obs": "',
                   PA_OBS[match("01570000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01570000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01570000.png"}, "geometry": {"type": "Point", "coordinates": [-77.021111, 40.252222]}}, {"type": "Feature", "properties": {"name": "Yellow Breeches Creek near Camp Hill", "id": "01571500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01571500", "obs": "',
                   PA_OBS[match("01571500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01571500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01571500.png"}, "geometry": {"type": "Point", "coordinates": [-76.898333, 40.224722]}}, {"type": "Feature", "properties": {"name": "Codorus Creek at Spring Grove", "id": "01574500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01574500", "obs": "',
                   PA_OBS[match("01574500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01574500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01574500.png"}, "geometry": {"type": "Point", "coordinates": [-76.853611, 39.878611]}}, {"type": "Feature", "properties": {"name": "Codorus Creek near York", "id": "01575500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01575500", "obs": "',
                   PA_OBS[match("01575500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01575500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01575500.png"}, "geometry": {"type": "Point", "coordinates": [-76.755556, 39.946111]}}, {"type": "Feature", "properties": {"name": "West Conewago Creek near Manchester", "id": "01574000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01574000", "obs": "',
                   PA_OBS[match("01574000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01574000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01574000.png"}, "geometry": {"type": "Point", "coordinates": [-76.720278, 40.082222]}}, {"type": "Feature", "properties": {"name": "Susquehanna River at Marietta", "id": "01576000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01576000", "obs": "',
                   PA_OBS[match("01576000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01576000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01576000.png"}, "geometry": {"type": "Point", "coordinates": [-76.531111, 40.054444]}}, {"type": "Feature", "properties": {"name": "Conestoga River at Conestoga", "id": "01576754", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01576754", "obs": "',
                   PA_OBS[match("01576754", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01576754", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01576754.png"}, "geometry": {"type": "Point", "coordinates": [-76.368056, 39.946389]}}, {"type": "Feature", "properties": {"name": "Conestoga River at Lancaster", "id": "01576500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01576500", "obs": "',
                   PA_OBS[match("01576500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01576500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01576500.png"}, "geometry": {"type": "Point", "coordinates": [-76.2775, 40.05]}}, {"type": "Feature", "properties": {"name": "West Branch Brandywine Creek near Honey Brook", "id": "01480300", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480300", "obs": "',
                   PA_OBS[match("01480300", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480300", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480300.png"}, "geometry": {"type": "Point", "coordinates": [-75.861111, 40.072778]}}, {"type": "Feature", "properties": {"name": "West Branch Brandywine Creek at Coatesville", "id": "01480500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480500", "obs": "',
                   PA_OBS[match("01480500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480500.png"}, "geometry": {"type": "Point", "coordinates": [-75.827778, 39.985556]}}, {"type": "Feature", "properties": {"name": "West Branch Brandywine Creek at Modena", "id": "01480617", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480617", "obs": "',
                   PA_OBS[match("01480617", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480617", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480617.png"}, "geometry": {"type": "Point", "coordinates": [-75.801667, 39.961667]}}, {"type": "Feature", "properties": {"name": "Marsh Creek near Glenmoore", "id": "01480675", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480675", "obs": "',
                   PA_OBS[match("01480675", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480675", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480675.png"}, "geometry": {"type": "Point", "coordinates": [-75.741944, 40.097778]}}, {"type": "Feature", "properties": {"name": "Marsh Creek near Downingtown", "id": "01480685", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480685", "obs": "',
                   PA_OBS[match("01480685", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480685", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480685.png"}, "geometry": {"type": "Point", "coordinates": [-75.716667, 40.055278]}}, {"type": "Feature", "properties": {"name": "East Branch Brandywine Creek near Downingtown", "id": "01480700", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480700", "obs": "',
                   PA_OBS[match("01480700", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480700", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480700.png"}, "geometry": {"type": "Point", "coordinates": [-75.708889, 40.034722]}}, {"type": "Feature", "properties": {"name": "East Branch Brandywine Creek below Downingtown", "id": "01480870", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01480870", "obs": "',
                   PA_OBS[match("01480870", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01480870", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480870.png"}, "geometry": {"type": "Point", "coordinates": [-75.673611, 39.968611]}}, {"type": "Feature", "properties": {"name": "Brandywine Creek at Chadds Ford", "id": "01481000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01481000", "obs": "',
                   PA_OBS[match("01481000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01481000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01481000.png"}, "geometry": {"type": "Point", "coordinates": [-75.593611, 39.869722]}}, {"type": "Feature", "properties": {"name": "Schuylkill River at Pottstown", "id": "01472000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01472000", "obs": "',
                   PA_OBS[match("01472000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01472000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01472000.png"}, "geometry": {"type": "Point", "coordinates": [-75.651944, 40.241667]}}, {"type": "Feature", "properties": {"name": "French Creek near Phoenixville", "id": "01472157", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01472157", "obs": "',
                   PA_OBS[match("01472157", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01472157", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01472157.png"}, "geometry": {"type": "Point", "coordinates": [-75.601667, 40.151389]}}, {"type": "Feature", "properties": {"name": "Valley Creek at PA Turnpike Br near Valley Forge", "id": "01473169", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01473169", "obs": "',
                   PA_OBS[match("01473169", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01473169", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01473169.png"}, "geometry": {"type": "Point", "coordinates": [-75.461111, 40.079167]}}, {"type": "Feature", "properties": {"name": "Crum Creek near Newtown Square", "id": "01475850", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01475850", "obs": "',
                   PA_OBS[match("01475850", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01475850", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01475850.png"}, "geometry": {"type": "Point", "coordinates": [-75.436944, 39.976389]}}, {"type": "Feature", "properties": {"name": "Chester Creek near Chester", "id": "01477000", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01477000", "obs": "',
                   PA_OBS[match("01477000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01477000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01477000.png"}, "geometry": {"type": "Point", "coordinates": [-75.408611, 39.868889]}}, {"type": "Feature", "properties": {"name": "West Branch Perkiomen Creek at Hillegass", "id": "01472199", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01472199", "obs": "',
                   PA_OBS[match("01472199", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01472199", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01472199.png"}, "geometry": {"type": "Point", "coordinates": [-75.522778, 40.373889]}}, {"type": "Feature", "properties": {"name": "Perkiomen Creek at East Greenville", "id": "01472198", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01472198", "obs": "',
                   PA_OBS[match("01472198", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01472198", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01472198.png"}, "geometry": {"type": "Point", "coordinates": [-75.515833, 40.393889]}}, {"type": "Feature", "properties": {"name": "Perkiomen Creek at Graterford", "id": "01473000", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01473000", "obs": "',
                   PA_OBS[match("01473000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01473000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01473000.png"}, "geometry": {"type": "Point", "coordinates": [-75.451944, 40.229444]}}, {"type": "Feature", "properties": {"name": "NB Neshaminy Cr bl Lake Galena nr New Britain", "id": "01464645", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01464645", "obs": "',
                   PA_OBS[match("01464645", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01464645", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01464645.png"}, "geometry": {"type": "Point", "coordinates": [-75.206944, 40.312222]}}, {"type": "Feature", "properties": {"name": "Tohickon Creek near Pipersville", "id": "01459500", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=01459500", "obs": "',
                   PA_OBS[match("01459500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01459500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01459500.png"}, "geometry": {"type": "Point", "coordinates": [-75.116944, 40.433611]}}, {"type": "Feature", "properties": {"name": "Schuylkill River at Philadelphia", "id": "01474500", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01474500", "obs": "',
                   PA_OBS[match("01474500", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01474500", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01474500.png"}, "geometry": {"type": "Point", "coordinates": [-75.188889, 39.967778]}}, {"type": "Feature", "properties": {"name": "Wissahickon Creek at Mouth, Philadelphia", "id": "01474000", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01474000", "obs": "',
                   PA_OBS[match("01474000", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01474000", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01474000.png"}, "geometry": {"type": "Point", "coordinates": [-75.207222, 40.015278]}}, {"type": "Feature", "properties": {"name": "Frankford Creek at Castor Ave, Philadelphia", "id": "01467087", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01467087", "obs": "',
                   PA_OBS[match("01467087", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01467087", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01467087.png"}, "geometry": {"type": "Point", "coordinates": [-75.097222, 40.015833]}}, {"type": "Feature", "properties": {"name": "Tacony Creek ab Adams Avenue, Philadelphia", "id": "01467086", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01467086", "obs": "',
                   PA_OBS[match("01467086", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01467086", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01467086.png"}, "geometry": {"type": "Point", "coordinates": [-75.111111, 40.046389]}}, {"type": "Feature", "properties": {"name": "Pennypack Cr at Lower Rhawn St Bdg, Philadelphia", "id": "01467048", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01467048", "obs": "',
                   PA_OBS[match("01467048", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01467048", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01467048.png"}, "geometry": {"type": "Point", "coordinates": [-75.033056, 40.05]}}, {"type": "Feature", "properties": {"name": "Poquessing Creek at Grant Ave. at Philadelphia", "id": "01465798", "url": "http://waterdata.usgs.gov/pa/nwis/inventory/?site_no=01465798", "obs": "',
                   PA_OBS[match("01465798", PA_OBS[,1]), 2], '", "time": "', PA_OBS[match("01465798", PA_OBS[,1]), 3],
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01465798.png"}, "geometry": {"type": "Point", "coordinates": [-74.985556, 40.056944]}}]};', sep="")

# Export data to geojson.
cat(json_merge, file="/net/www/www.marisa.psu.edu/htdocs/mapdata/PA_stream_obs.js")
# --------------------------------------------------------------------------------------------------------------------
