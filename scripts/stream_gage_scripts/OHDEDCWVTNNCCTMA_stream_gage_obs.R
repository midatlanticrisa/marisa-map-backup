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
# Create vector including each station ID.
# <!-- USGS STATIONS -->
# <!-- Ohio -->
OH_ID = c("03159540", "03159500", "03115400", "03150000", "03142000", "03144500", "03111548", "03111500", "03129000",
          "03140500", "03140000", "03139000", "03120500", "03110000", "03109500", "03117500", "03118500", "03117000",
          "03118000", "04206000", "03091500", "03094000", "03093000", "04202000", "04207200", "04208000", "04201500",
          "04209000", "04212100", "04213000")

OH_TMP = mat.or.vec(length(OH_ID), 3)
OH_TMP[ ,1] = OH_ID; OH_DIS = OH_TMP; OH_GAG = OH_TMP
for(i in 1:length(OH_ID)){
  OH_TMP[i, 2:3] = usgs_dataRetrieveTemp(OH_ID[i], "00010", b.date, e.date, tz="America/New_York")
  OH_DIS[i, 2:3] = usgs_dataRetrieveTemp(OH_ID[i], "00060", b.date, e.date, tz="America/New_York")
  OH_GAG[i, 2:3] = usgs_dataRetrieveTemp(OH_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
OH_OBS = obs_string(OH_TMP, OH_DIS, OH_GAG)

# <!-- Delaware -->
DE_ID = c("01487000", "01488500", "01484100", "01483700", "01483200", "01478000", "01479000", "01480000", "01481500",
          "01477800")

# Run through each Delaware station.
DE_TMP = mat.or.vec(length(DE_ID), 3)
DE_TMP[ ,1] = DE_ID; DE_DIS = DE_TMP; DE_GAG = DE_TMP
for(i in 1:length(DE_ID)){
  DE_TMP[i, 2:3] = usgs_dataRetrieveTemp(DE_ID[i], "00010", b.date, e.date, tz="America/New_York")
  DE_DIS[i, 2:3] = usgs_dataRetrieveTemp(DE_ID[i], "00060", b.date, e.date, tz="America/New_York")
  DE_GAG[i, 2:3] = usgs_dataRetrieveTemp(DE_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
DE_OBS = obs_string(DE_TMP, DE_DIS, DE_GAG)

# <!-- Washington DC -->
DC_ID = c("01648000", "01646500")

# Run through each Washington DC station.
DC_TMP = mat.or.vec(length(DC_ID), 3)
DC_TMP[ ,1] = DC_ID; DC_DIS = DC_TMP; DC_GAG = DC_TMP
for(i in 1:length(DC_ID)){
  DC_TMP[i, 2:3] = usgs_dataRetrieveTemp(DC_ID[i], "00010", b.date, e.date, tz="America/New_York")
  DC_DIS[i, 2:3] = usgs_dataRetrieveTemp(DC_ID[i], "00060", b.date, e.date, tz="America/New_York")
  DC_GAG[i, 2:3] = usgs_dataRetrieveTemp(DC_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
DC_OBS = obs_string(DC_TMP, DC_DIS, DC_GAG)

# <!-- West Virginia -->
WV_ID = c("03112000",
          "03062500", "03070500", "01595800", "01608500", "01610000", "01611500", "01614000", "01617000", "01616500",
          "01618000", "01636500", "01606500", "01607500", "03065000", "03069500", "03050000", "03051000", "03052000",
          "03053500", "03054500", "03056250", "03052500", "03057000", "03061500", "03114500", "03180500",
          "03182500", "03186500", "03194700", "03151400", "03154000", "03155000", "03200500", "03198000", "03197000",
          "03193000", "03192000", "03191500", "03185400", "03190000", "03189100", "03187500", "03183500", "03184000",
          "03179000", "03185000", "03202400", "03202750", "03213500", "03203000", "03203600", "03198500", "03213700",
          "03214500", "03206600")

# Run through each West Virginia station.
WV_TMP = mat.or.vec(length(WV_ID), 3)
WV_TMP[ ,1] = WV_ID; WV_DIS = WV_TMP; WV_GAG = WV_TMP
for(i in 1:length(WV_ID)){
  WV_TMP[i, 2:3] = usgs_dataRetrieveTemp(WV_ID[i], "00010", b.date, e.date, tz="America/New_York")
  WV_DIS[i, 2:3] = usgs_dataRetrieveTemp(WV_ID[i], "00060", b.date, e.date, tz="America/New_York")
  WV_GAG[i, 2:3] = usgs_dataRetrieveTemp(WV_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
WV_OBS = obs_string(WV_TMP, WV_DIS, WV_GAG)

# Run through a few other stations. These stations are sometimes difficult to extract 
# data, so they are run seperately from the others
WV_ID_NC_ID = c("03056000", "02077303")

WVNC_TMP = mat.or.vec(length(WV_ID_NC_ID), 3)
WVNC_TMP[ ,1] = WV_ID_NC_ID ; WVNC_DIS = WVNC_TMP; WVNC_GAG = WVNC_TMP
for(i in 1:length(WV_ID_NC_ID)){
  WVNC_TMP[i, 2:3] = usgs_dataRetrieveTemp(WV_ID_NC_ID[i], "00010", b.date, e.date, tz="America/New_York")
  WVNC_DIS[i, 2:3] = usgs_dataRetrieveTemp(WV_ID_NC_ID[i], "00060", b.date, e.date, tz="America/New_York")
  WVNC_GAG[i, 2:3] = usgs_dataRetrieveTemp(WV_ID_NC_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
WVNC_OBS = obs_string(WVNC_TMP, WVNC_DIS, WVNC_GAG)

# <!-- Tennessee -->
TN_ID = "03532000"

# Run the Tennessee station.
TN_TMP = rep(TN_ID, 3); TN_DIS = TN_TMP; TN_GAG = TN_TMP
TN_TMP[2:3] = usgs_dataRetrieveTemp(TN_ID, "00010", b.date, e.date, tz="America/New_York")
TN_DIS[2:3] = usgs_dataRetrieveTemp(TN_ID, "00060", b.date, e.date, tz="America/New_York")
TN_GAG[2:3] = usgs_dataRetrieveTemp(TN_ID, "00065", b.date, e.date, tz="America/New_York")

TN_OBS = obs_string_single(TN_TMP, TN_DIS, TN_GAG)

# <!-- North Carolina -->
NC_ID = c("02068500", "02070500", "02074000", "02077670", "02080500")

# Run through each North Carolina station.
NC_TMP = mat.or.vec(length(NC_ID), 3)
NC_TMP[ ,1] = NC_ID; NC_DIS = NC_TMP; NC_GAG = NC_TMP
for(i in 1:length(NC_ID)){
  NC_TMP[i, 2:3] = usgs_dataRetrieveTemp(NC_ID[i], "00010", b.date, e.date, tz="America/New_York")
  NC_DIS[i, 2:3] = usgs_dataRetrieveTemp(NC_ID[i], "00060", b.date, e.date, tz="America/New_York")
  NC_GAG[i, 2:3] = usgs_dataRetrieveTemp(NC_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
NC_OBS = obs_string(NC_TMP, NC_DIS, NC_GAG)

# <!-- Conneticut -->
CT_ID = c("01209700", "01200500", "01200000", "01199050")

# Run through each Conneticut station.
CT_TMP = mat.or.vec(length(CT_ID), 3)
CT_TMP[ ,1] = CT_ID; CT_DIS = CT_TMP; CT_GAG = CT_TMP
for(i in 1:length(CT_ID)){
  CT_TMP[i, 2:3] = usgs_dataRetrieveTemp(CT_ID[i], "00010", b.date, e.date, tz="America/New_York")
  CT_DIS[i, 2:3] = usgs_dataRetrieveTemp(CT_ID[i], "00060", b.date, e.date, tz="America/New_York")
  CT_GAG[i, 2:3] = usgs_dataRetrieveTemp(CT_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
CT_OBS = obs_string(CT_TMP, CT_DIS, CT_GAG)

# <!-- Massachusetts -->
MA_ID = c("01198000", "01197500")

# Run through each Massachusetts station.
MA_TMP = mat.or.vec(length(MA_ID), 3)
MA_TMP[ ,1] = MA_ID; MA_DIS = MA_TMP; MA_GAG = MA_TMP
for(i in 1:length(MA_ID)){
  MA_TMP[i, 2:3] = usgs_dataRetrieveTemp(MA_ID[i], "00010", b.date, e.date, tz="America/New_York")
  MA_DIS[i, 2:3] = usgs_dataRetrieveTemp(MA_ID[i], "00060", b.date, e.date, tz="America/New_York")
  MA_GAG[i, 2:3] = usgs_dataRetrieveTemp(MA_ID[i], "00065", b.date, e.date, tz="America/New_York")
}
MA_OBS = obs_string(MA_TMP, MA_DIS, MA_GAG)

# --------------------------------------------------------------------------------------------------------------------
# Create a geojson object with the observation and statement info and merge into a 
# specific file format with adding stream temp and time.
json_merge = paste('OHDEDCWVTNNCCTMA_streamGauges = {"type": "FeatureCollection","features": [{"type": "Feature", "properties": {"name": "Shade River near Chester", "id": "03159540", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03159540", "obs": "', 
                   OH_OBS[match("03159540", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03159540", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03159540.png"}, "geometry": {"type": "Point", "coordinates": [-81.881944, 39.063611]}}, {"type": "Feature", "properties": {"name": "Hocking River at Athens", "id": "03159500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03159500", "obs": "',
                   OH_OBS[match("03159500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03159500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03159500.png"}, "geometry": {"type": "Point", "coordinates": [-82.087778, 39.328889]}}, {"type": "Feature", "properties": {"name": "Little Muskingum River at Bloomfield", "id": "03115400", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03115400", "obs": "',
                   OH_OBS[match("03115400", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03115400", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03115400.png"}, "geometry": {"type": "Point", "coordinates": [-81.881944, 39.063611]}}, {"type": "Feature", "properties": {"name": "Muskingum River at McConnelsville", "id": "03150000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03150000", "obs": "',
                   OH_OBS[match("03150000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03150000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03150000.png"}, "geometry": {"type": "Point", "coordinates": [-81.85, 39.645]}}, {"type": "Feature", "properties": {"name": "Wills Creek at Cambridge", "id": "03142000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03142000", "obs": "',
                   OH_OBS[match("03142000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03142000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03142000.png"}, "geometry": {"type": "Point", "coordinates": [-81.587222, 40.014444]}}, {"type": "Feature", "properties": {"name": "Muskingum River at Dresden", "id": "03144500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03144500", "obs": "',
                   OH_OBS[match("03144500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03144500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03144500.png"}, "geometry": {"type": "Point", "coordinates": [-81.999722, 40.120278]}}, {"type": "Feature", "properties": {"name": "Wheeling Creek below Blaine", "id": "03111548", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=03111548", "obs": "',
                   OH_OBS[match("03111548", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03111548", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03111548.png"}, "geometry": {"type": "Point", "coordinates": [-80.808611, 40.066944]}}, {"type": "Feature", "properties": {"name": "Short Creek near Dillonvale", "id": "03111500", "url": "http://waterdata.usgs.gov/nwis/inventory/?site_no=03111500", "obs": "',
                   OH_OBS[match("03111500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03111500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03111500.png"}, "geometry": {"type": "Point", "coordinates": [-80.734444, 40.193333]}}, {"type": "Feature", "properties": {"name": "Tuscarawas River at Newcomerstown", "id": "03129000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03129000", "obs": "',
                   OH_OBS[match("03129000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03129000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03129000.png"}, "geometry": {"type": "Point", "coordinates": [-81.609167, 40.261389]}}, {"type": "Feature", "properties": {"name": "Muskingum River near Coshocton", "id": "03140500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03140500", "obs": "',
                   OH_OBS[match("03140500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03140500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03140500.png"}, "geometry": {"type": "Point", "coordinates": [-81.873056, 40.248333]}}, {"type": "Feature", "properties": {"name": "Mill Creek near Coshocton", "id": "03140000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03140000", "obs": "',
                   OH_OBS[match("03140000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03140000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03140000.png"}, "geometry": {"type": "Point", "coordinates": [-81.8625, 40.362778]}}, {"type": "Feature", "properties": {"name": "Killbuck Creek at Killbuck", "id": "03139000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03139000", "obs": "',
                   OH_OBS[match("03139000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03139000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03139000.png"}, "geometry": {"type": "Point", "coordinates": [-81.986111, 40.481389]}}, {"type": "Feature", "properties": {"name": "McGuire Creek near Leesville", "id": "03120500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03120500", "obs": "',
                   OH_OBS[match("03120500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03120500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03120500.png"}, "geometry": {"type": "Point", "coordinates": [-81.196667, 40.470278]}}, {"type": "Feature", "properties": {"name": "Yellow Creek near Hammondsville", "id": "03110000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03110000", "obs": "',
                   OH_OBS[match("03110000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03110000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03110000.png"}, "geometry": {"type": "Point", "coordinates": [-80.725278, 40.537778]}}, {"type": "Feature", "properties": {"name": "Little Beaver Creek near East Liverpool", "id": "03109500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03109500", "obs": "',
                   OH_OBS[match("03109500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03109500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03109500.png"}, "geometry": {"type": "Point", "coordinates": [-80.540833, 40.675833]}}, {"type": "Feature", "properties": {"name": "Sandy Creek at Waynesburg", "id": "03117500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03117500", "obs": "',
                   OH_OBS[match("03117500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03117500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03117500.png"}, "geometry": {"type": "Point", "coordinates": [-81.26, 40.6725]}}, {"type": "Feature", "properties": {"name": "Nimishillen Creek at North Industry", "id": "03118500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03118500", "obs": "',
                   OH_OBS[match("03118500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03118500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03118500.png"}, "geometry": {"type": "Point", "coordinates": [-81.369444, 40.749722]}}, {"type": "Feature", "properties": {"name": "Tuscarawas River at Massillon", "id": "03117000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03117000", "obs": "',
                   OH_OBS[match("03117000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03117000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03117000.png"}, "geometry": {"type": "Point", "coordinates": [-81.524167, 40.770278]}}, {"type": "Feature", "properties": {"name": "Middle Branch Nimishillen Creek at Canton", "id": "03118000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03118000", "obs": "',
                   OH_OBS[match("03118000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03118000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03118000.png"}, "geometry": {"type": "Point", "coordinates": [-81.353889, 40.841389]}}, {"type": "Feature", "properties": {"name": "Cuyahoga River at Old Portage", "id": "04206000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04206000", "obs": "',
                   OH_OBS[match("04206000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04206000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04206000.png"}, "geometry": {"type": "Point", "coordinates": [-81.547222, 41.135556]}}, {"type": "Feature", "properties": {"name": "Mahoning River at Pricetown", "id": "03091500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03091500", "obs": "',
                   OH_OBS[match("03091500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03091500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03091500.png"}, "geometry": {"type": "Point", "coordinates": [-80.971389, 41.131389]}}, {"type": "Feature", "properties": {"name": "Mahoning River at Leavittsburg", "id": "03094000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03094000", "obs": "',
                   OH_OBS[match("03094000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03094000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03094000.png"}, "geometry": {"type": "Point", "coordinates": [-80.880833, 41.239167]}}, {"type": "Feature", "properties": {"name": "Eagle Creek at Phalanx Station", "id": "03093000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=03093000", "obs": "',
                   OH_OBS[match("03093000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("03093000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03093000.png"}, "geometry": {"type": "Point", "coordinates": [-80.954444, 41.261111]}}, {"type": "Feature", "properties": {"name": "Cuyahoga River at Hiram Rapids", "id": "04202000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04202000", "obs": "',
                   OH_OBS[match("04202000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04202000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04202000.png"}, "geometry": {"type": "Point", "coordinates": [-81.166944, 41.340556]}}, {"type": "Feature", "properties": {"name": "Tinkers Creek at Bedford", "id": "04207200", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04207200", "obs": "',
                   OH_OBS[match("04207200", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04207200", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04207200.png"}, "geometry": {"type": "Point", "coordinates": [-81.5275, 41.384444]}}, {"type": "Feature", "properties": {"name": "Cuyahoga River at Independence", "id": "04208000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04208000", "obs": "',
                   OH_OBS[match("04208000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04208000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04208000.png"}, "geometry": {"type": "Point", "coordinates": [-81.63, 41.395278]}}, {"type": "Feature", "properties": {"name": "Rocky River near Berea", "id": "04201500", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04201500", "obs": "',
                   OH_OBS[match("04201500", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04201500", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04201500.png"}, "geometry": {"type": "Point", "coordinates": [-81.887222, 41.406667]}}, {"type": "Feature", "properties": {"name": "Chagrin River at Willoughby", "id": "04209000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04209000", "obs": "',
                   OH_OBS[match("04209000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04209000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04209000.png"}, "geometry": {"type": "Point", "coordinates": [-81.403611, 41.630833]}}, {"type": "Feature", "properties": {"name": "Grand River near Painesville", "id": "04212100", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04212100", "obs": "',
                   OH_OBS[match("04212100", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04212100", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04212100.png"}, "geometry": {"type": "Point", "coordinates": [-81.228056, 41.718889]}}, {"type": "Feature", "properties": {"name": "Conneaut Creek at Conneaut", "id": "04213000", "url": "http://waterdata.usgs.gov/oh/nwis/inventory/?site_no=04213000", "obs": "',
                   OH_OBS[match("04213000", OH_OBS[,1]), 2], '", "time": "', OH_OBS[match("04213000", OH_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_04213000.png"}, "geometry": {"type": "Point", "coordinates": [-80.604167, 41.926944]}}, {"type": "Feature", "properties": {"name": "NANTICOKE RIVER NEAR BRIDGEVILLE", "id": "01487000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01487000", "obs": "',
                   DE_OBS[match("01487000", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01487000", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01487000.png"}, "geometry": {"type": "Point", "coordinates": [-75.561861, 38.728333]}}, {"type": "Feature", "properties": {"name": "MARSHYHOPE CREEK NEAR ADAMSVILLE", "id": "01488500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01488500", "obs": "',
                   DE_OBS[match("01488500", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01488500", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01488500.png"}, "geometry": {"type": "Point", "coordinates": [-75.673111, 38.849694]}}, {"type": "Feature", "properties": {"name": "BEAVERDAM BRANCH AT HOUSTON", "id": "01484100", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01484100", "obs": "',
                   DE_OBS[match("01484100", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01484100", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01484100.png"}, "geometry": {"type": "Point", "coordinates": [-75.51275, 38.905778]}}, {"type": "Feature", "properties": {"name": "ST JONES RIVER AT DOVER", "id": "01483700", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01483700", "obs": "',
                   DE_OBS[match("01483700", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01483700", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01483700.png"}, "geometry": {"type": "Point", "coordinates": [-75.519083, 39.163722]}}, {"type": "Feature", "properties": {"name": "BLACKBIRD CREEK AT BLACKBIRD", "id": "01483200", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01483200", "obs": "',
                   DE_OBS[match("01483200", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01483200", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01483200.png"}, "geometry": {"type": "Point", "coordinates": [-75.669389, 39.366278]}}, {"type": "Feature", "properties": {"name": "CHRISTINA RIVER AT COOCHS BRIDGE", "id": "01478000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01478000", "obs": "',
                   DE_OBS[match("01478000", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01478000", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01478000.png"}, "geometry": {"type": "Point", "coordinates": [-75.727889, 39.637389]}}, {"type": "Feature", "properties": {"name": "WHITE CLAY CREEK NEAR NEWARK", "id": "01479000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01479000", "obs": "',
                   DE_OBS[match("01479000", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01479000", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01479000.png"}, "geometry": {"type": "Point", "coordinates": [-75.675028, 39.699222]}}, {"type": "Feature", "properties": {"name": "RED CLAY CREEK AT WOODDALE", "id": "01480000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01480000", "obs": "',
                   DE_OBS[match("01480000", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01480000", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01480000.png"}, "geometry": {"type": "Point", "coordinates": [-75.6365, 39.762806]}}, {"type": "Feature", "properties": {"name": "BRANDYWINE CREEK AT WILMINGTON", "id": "01481500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01481500", "obs": "',
                   DE_OBS[match("01481500", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01481500", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01481500.png"}, "geometry": {"type": "Point", "coordinates": [-75.576694, 39.7695]}}, {"type": "Feature", "properties": {"name": "SHELLPOT CREEK AT WILMINGTON", "id": "01477800", "url": "https://waterdata.usgs.gov/de/nwis/inventory/?site_no=01477800", "obs": "',
                   DE_OBS[match("01477800", DE_OBS[,1]), 2], '", "time": "', DE_OBS[match("01477800", DE_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01477800.png"}, "geometry": {"type": "Point", "coordinates": [-75.518694, 39.760972]}}, {"type": "Feature", "properties": {"name": "ROCK CREEK AT SHERRILL DRIVE WASHINGTON", "id": "01648000", "url": "https://waterdata.usgs.gov/dc/nwis/inventory/?site_no=01648000", "obs": "',
                   DC_OBS[match("01648000", DC_OBS[,1]), 2], '", "time": "', DC_OBS[match("01648000", DC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01648000.png"}, "geometry": {"type": "Point", "coordinates": [-77.04, 38.9725]}}, {"type": "Feature", "properties": {"name": "POTOMAC RIVER NEAR WASH, DC LITTLE FALLS PUMP STA", "id": "01646500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01646500", "obs": "',
                   DC_OBS[match("01646500", DC_OBS[,1]), 2], '", "time": "', DC_OBS[match("01646500", DC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01646500.png"}, "geometry": {"type": "Point", "coordinates": [-77.127639, 38.949778]}}, {"type": "Feature", "properties": {"name": "WHEELING CREEK AT ELM GROVE", "id": "03112000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03112000", "obs": "',
                   WV_OBS[match("03112000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03112000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03112000.png"}, "geometry": {"type": "Point", "coordinates": [-80.661111, 40.044444]}}, {"type": "Feature", "properties": {"name": "DECKERS CREEK AT MORGANTOWN", "id": "03062500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03062500", "obs": "',
                   WV_OBS[match("03062500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03062500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03062500.png"}, "geometry": {"type": "Point", "coordinates": [-79.952778, 39.629167]}}, {"type": "Feature", "properties": {"name": "BIG SANDY CREEK AT ROCKVILLE", "id": "03070500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03070500", "obs": "',
                   WV_OBS[match("03070500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03070500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03070500.png"}, "geometry": {"type": "Point", "coordinates": [-79.704556, 39.62175]}}, {"type": "Feature", "properties": {"name": "NORTH BRANCH POTOMAC RIVER AT BARNUM", "id": "01595800", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01595800", "obs": "',
                   WV_OBS[match("01595800", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01595800", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01595800.png"}, "geometry": {"type": "Point", "coordinates": [-79.110806, 39.445111]}}, {"type": "Feature", "properties": {"name": "SOUTH BRANCH POTOMAC RIVER NEAR SPRINGFIELD", "id": "01608500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01608500", "obs": "',
                   WV_OBS[match("01608500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01608500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01608500.png"}, "geometry": {"type": "Point", "coordinates": [-78.654444, 39.446944]}}, {"type": "Feature", "properties": {"name": "POTOMAC RIVER AT PAW PAW", "id": "01610000", "url": "https://waterdata.usgs.gov/md/nwis/inventory/?site_no=01610000", "obs": "',
                   WV_OBS[match("01610000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01610000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01610000.png"}, "geometry": {"type": "Point", "coordinates": [-78.456389, 39.538917]}}, {"type": "Feature", "properties": {"name": "CACAPON RIVER NEAR GREAT CACAPON", "id": "01611500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01611500", "obs": "',
                   WV_OBS[match("01611500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01611500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01611500.png"}, "geometry": {"type": "Point", "coordinates": [-78.31, 39.582222]}}, {"type": "Feature", "properties": {"name": "BACK CREEK NEAR JONES SPRINGS", "id": "01614000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01614000", "obs": "',
                   WV_OBS[match("01614000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01614000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01614000.png"}, "geometry": {"type": "Point", "coordinates": [-78.0375, 39.511944]}}, {"type": "Feature", "properties": {"name": "TUSCARORA CREEK ABOVE MARTINSBURG", "id": "01617000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01617000", "obs": "',
                   WV_OBS[match("01617000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01617000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01617000.png"}, "geometry": {"type": "Point", "coordinates": [-77.971667, 39.469444]}}, {"type": "Feature", "properties": {"name": "OPEQUON CREEK NEAR MARTINSBURG", "id": "01616500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01616500", "obs": "',
                   WV_OBS[match("01616500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01616500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01616500.png"}, "geometry": {"type": "Point", "coordinates": [-77.938889, 39.423611]}}, {"type": "Feature", "properties": {"name": "POTOMAC RIVER AT SHEPHERDSTOWN", "id": "01618000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=01618000", "obs": "',
                   WV_OBS[match("01618000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01618000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01618000.png"}, "geometry": {"type": "Point", "coordinates": [-77.801389, 39.434722]}}, {"type": "Feature", "properties": {"name": "SHENANDOAH RIVER AT MILLVILLE", "id": "01636500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01636500", "obs": "',
                   WV_OBS[match("01636500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01636500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01636500.png"}, "geometry": {"type": "Point", "coordinates": [-77.789444, 39.281944]}}, {"type": "Feature", "properties": {"name": "SOUTH BRANCH POTOMAC RIVER NEAR PETERSBURG", "id": "01606500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01606500", "obs": "',
                   WV_OBS[match("01606500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01606500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01606500.png"}, "geometry": {"type": "Point", "coordinates": [-79.176111, 38.991111]}}, {"type": "Feature", "properties": {"name": "S F SOUTH BRANCH POTOMAC RIVER AT BRANDYWINE", "id": "01607500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=01607500", "obs": "',
                   WV_OBS[match("01607500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("01607500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01607500.png"}, "geometry": {"type": "Point", "coordinates": [-79.243889, 38.631389]}}, {"type": "Feature", "properties": {"name": "DRY FORK AT HENDRICKS", "id": "03065000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=03065000", "obs": "',
                   WV_OBS[match("03065000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03065000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03065000.png"}, "geometry": {"type": "Point", "coordinates": [-79.623056, 39.072222]}}, {"type": "Feature", "properties": {"name": "CHEAT RIVER NEAR PARSONS", "id": "03069500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03069500", "obs": "',
                   WV_OBS[match("03069500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03069500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03069500.png"}, "geometry": {"type": "Point", "coordinates": [-79.675528, 39.121556]}}, {"type": "Feature", "properties": {"name": "TYGART VALLEY RIVER NEAR DAILEY", "id": "03050000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03050000", "obs": "',
                   WV_OBS[match("03050000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03050000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03050000.png"}, "geometry": {"type": "Point", "coordinates": [-79.881944, 38.809167]}}, {"type": "Feature", "properties": {"name": "TYGART VALLEY RIVER AT BELINGTON", "id": "03051000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03051000", "obs": "',
                   WV_OBS[match("03051000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03051000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03051000.png"}, "geometry": {"type": "Point", "coordinates": [-79.936111, 39.029167]}}, {"type": "Feature", "properties": {"name": "MIDDLE FORK RIVER AT AUDRA", "id": "03052000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03052000", "obs": "',
                   WV_OBS[match("03052000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03052000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03052000.png"}, "geometry": {"type": "Point", "coordinates": [-80.068333, 39.039444]}}, {"type": "Feature", "properties": {"name": "BUCKHANNON RIVER AT HALL", "id": "03053500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03053500", "obs": "',
                   WV_OBS[match("03053500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03053500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03053500.png"}, "geometry": {"type": "Point", "coordinates": [-80.114722, 39.051111]}}, {"type": "Feature", "properties": {"name": "TYGART VALLEY RIVER AT PHILIPPI", "id": "03054500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03054500", "obs": "',
                   WV_OBS[match("03054500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03054500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03054500.png"}, "geometry": {"type": "Point", "coordinates": [-80.038889, 39.150278]}}, {"type": "Feature", "properties": {"name": "THREE FORK CREEK NR GRAFTON", "id": "03056250", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03056250", "obs": "',
                   WV_OBS[match("03056250", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03056250", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03056250.png"}, "geometry": {"type": "Point", "coordinates": [-79.993611, 39.336389]}}, {"type": "Feature", "properties": {"name": "SAND RUN NEAR BUCKHANNON", "id": "03052500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03052500", "obs": "',
                   WV_OBS[match("03052500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03052500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03052500.png"}, "geometry": {"type": "Point", "coordinates": [-80.152778, 38.963889]}}, {"type": "Feature", "properties": {"name": "TYGART VALLEY RIVER AT COLFAX", "id": "03057000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03057000", "obs": "',
                   WV_OBS[match("03057000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03057000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03057000.png"}, "geometry": {"type": "Point", "coordinates": [-80.132778, 39.435]}}, {"type": "Feature", "properties": {"name": "BUFFALO CREEK AT BARRACKVILLE", "id": "03061500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03061500", "obs": "',
                   WV_OBS[match("03061500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03061500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03061500.png"}, "geometry": {"type": "Point", "coordinates": [-80.172222, 39.503889]}}, {"type": "Feature", "properties": {"name": "MIDDLE ISLAND CREEK AT LITTLE", "id": "03114500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03114500", "obs": "',
                   WV_OBS[match("03114500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03114500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03114500.png"}, "geometry": {"type": "Point", "coordinates": [-80.997222, 39.475]}}, {"type": "Feature", "properties": {"name": "GREENBRIER RIVER AT DURBIN", "id": "03180500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03180500", "obs": "',
                   WV_OBS[match("03180500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03180500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03180500.png"}, "geometry": {"type": "Point", "coordinates": [-79.833333, 38.543611]}}, {"type": "Feature", "properties": {"name": "GREENBRIER RIVER AT BUCKEYE", "id": "03182500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03182500", "obs": "',
                   WV_OBS[match("03182500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03182500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03182500.png"}, "geometry": {"type": "Point", "coordinates": [-80.130833, 38.185833]}}, {"type": "Feature", "properties": {"name": "WILLIAMS RIVER AT DYER", "id": "03186500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03186500", "obs": "',
                   WV_OBS[match("03186500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03186500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03186500.png"}, "geometry": {"type": "Point", "coordinates": [-80.484167, 38.378889]}}, {"type": "Feature", "properties": {"name": "ELK RIVER BELOW WEBSTER SPRINGS", "id": "03194700", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03194700", "obs": "',
                   WV_OBS[match("03194700", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03194700", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03194700.png"}, "geometry": {"type": "Point", "coordinates": [-80.490556, 38.597222]}}, {"type": "Feature", "properties": {"name": "LITTLE KANAWHA RIVER NR WILDCAT", "id": "03151400", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03151400", "obs": "',
                   WV_OBS[match("03151400", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03151400", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03151400.png"}, "geometry": {"type": "Point", "coordinates": [-80.525556, 38.743333]}}, {"type": "Feature", "properties": {"name": "WEST FORK LITTLE KANAWHA RIVER AT ROCKSDALE", "id": "03154000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03154000", "obs": "',
                   WV_OBS[match("03154000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03154000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03154000.png"}, "geometry": {"type": "Point", "coordinates": [-81.222778, 38.844167]}}, {"type": "Feature", "properties": {"name": "LITTLE KANAWHA RIVER AT PALESTINE", "id": "03155000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=03155000", "obs": "',
                   WV_OBS[match("03155000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03155000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03155000.png"}, "geometry": {"type": "Point", "coordinates": [-81.389722, 39.058889]}}, {"type": "Feature", "properties": {"name": "COAL RIVER AT TORNADO", "id": "03200500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03200500", "obs": "',
                   WV_OBS[match("03200500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03200500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03200500.png"}, "geometry": {"type": "Point", "coordinates": [-81.841667, 38.338889]}}, {"type": "Feature", "properties": {"name": "KANAWHA RIVER AT CHARLESTON", "id": "03198000", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=03198000", "obs": "',
                   WV_OBS[match("03198000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03198000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03198000.png"}, "geometry": {"type": "Point", "coordinates": [-81.702222, 38.371389]}}, {"type": "Feature", "properties": {"name": "ELK RIVER AT QUEEN SHOALS", "id": "03197000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03197000", "obs": "',
                   WV_OBS[match("03197000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03197000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03197000.png"}, "geometry": {"type": "Point", "coordinates": [-81.284167, 38.470833]}}, {"type": "Feature", "properties": {"name": "KANAWHA RIVER AT KANAWHA FALLS", "id": "03193000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03193000", "obs": "',
                   WV_OBS[match("03193000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03193000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03193000.png"}, "geometry": {"type": "Point", "coordinates": [-81.214444, 38.138056]}}, {"type": "Feature", "properties": {"name": "GAULEY RIVER ABOVE BELVA", "id": "03192000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03192000", "obs": "',
                   WV_OBS[match("03192000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03192000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03192000.png"}, "geometry": {"type": "Point", "coordinates": [-81.181111, 38.233333]}}, {"type": "Feature", "properties": {"name": "PETERS CREEK NEAR LOCKWOOD", "id": "03191500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03191500", "obs": "',
                   WV_OBS[match("03191500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03191500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03191500.png"}, "geometry": {"type": "Point", "coordinates": [-81.023333, 38.2625]}}, {"type": "Feature", "properties": {"name": "NEW RIVER AT THURMOND", "id": "03185400", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03185400", "obs": "',
                   WV_OBS[match("03185400", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03185400", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03185400.png"}, "geometry": {"type": "Point", "coordinates": [-81.076667, 37.955]}}, {"type": "Feature", "properties": {"name": "MEADOW RIVER AT NALLEN", "id": "03190000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03190000", "obs": "',
                   WV_OBS[match("03190000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03190000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03190000.png"}, "geometry": {"type": "Point", "coordinates": [-80.876389, 38.1125]}}, {"type": "Feature", "properties": {"name": "GAULEY RIVER NEAR CRAIGSVILLE", "id": "03189100", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03189100", "obs": "',
                   WV_OBS[match("03189100", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03189100", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03189100.png"}, "geometry": {"type": "Point", "coordinates": [-80.641111, 38.290833]}}, {"type": "Feature", "properties": {"name": "CRANBERRY RIVER NEAR RICHWOOD", "id": "03187500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03187500", "obs": "',
                   WV_OBS[match("03187500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03187500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03187500.png"}, "geometry": {"type": "Point", "coordinates": [-80.526667, 38.295278]}}, {"type": "Feature", "properties": {"name": "GREENBRIER RIVER AT ALDERSON", "id": "03183500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03183500", "obs": "',
                   WV_OBS[match("03183500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03183500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03183500.png"}, "geometry": {"type": "Point", "coordinates": [-80.641667, 37.724167]}}, {"type": "Feature", "properties": {"name": "GREENBRIER RIVER AT HILLDALE", "id": "03184000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03184000", "obs": "',
                   WV_OBS[match("03184000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03184000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03184000.png"}, "geometry": {"type": "Point", "coordinates": [-80.805278, 37.64]}}, {"type": "Feature", "properties": {"name": "BLUESTONE RIVER NEAR PIPESTEM", "id": "03179000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03179000", "obs": "',
                   WV_OBS[match("03179000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03179000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03179000.png"}, "geometry": {"type": "Point", "coordinates": [-81.010556, 37.543889]}}, {"type": "Feature", "properties": {"name": "PINEY CREEK AT RALEIGH", "id": "03185000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03185000", "obs": "',
                   WV_OBS[match("03185000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03185000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03185000.png"}, "geometry": {"type": "Point", "coordinates": [-81.1625, 37.760556]}}, {"type": "Feature", "properties": {"name": "GUYANDOTTE RIVER NEAR BAILEYSVILLE", "id": "03202400", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03202400", "obs": "',
                   WV_OBS[match("03202400", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03202400", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03202400.png"}, "geometry": {"type": "Point", "coordinates": [-81.645278, 37.603889]}}, {"type": "Feature", "properties": {"name": "CLEAR FORK AT CLEAR FORK", "id": "03202750", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03202750", "obs": "',
                   WV_OBS[match("03202750", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03202750", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03202750.png"}, "geometry": {"type": "Point", "coordinates": [-81.7075, 37.623056]}}, {"type": "Feature", "properties": {"name": "PANTHER CREEK NEAR PANTHER", "id": "03213500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03213500", "obs": "',
                   WV_OBS[match("03213500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03213500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03213500.png"}, "geometry": {"type": "Point", "coordinates": [-81.871111, 37.445556]}}, {"type": "Feature", "properties": {"name": "GUYANDOTTE RIVER AT MAN", "id": "03203000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03203000", "obs": "',
                   WV_OBS[match("03203000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03203000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03203000.png"}, "geometry": {"type": "Point", "coordinates": [-81.876944, 37.740278]}}, {"type": "Feature", "properties": {"name": "GUYANDOTTE RIVER AT MAN", "id": "03203000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03203000", "obs": "',
                   WV_OBS[match("03203000", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03203000", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03203000.png"}, "geometry": {"type": "Point", "coordinates": [-81.876944, 37.740278]}}, {"type": "Feature", "properties": {"name": "GUYANDOTTE RIVER AT LOGAN", "id": "03203600", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03203600", "obs": "',
                   WV_OBS[match("03203600", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03203600", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03203600.png"}, "geometry": {"type": "Point", "coordinates": [-81.976111, 37.842222]}}, {"type": "Feature", "properties": {"name": "BIG COAL RIVER AT ASHFORD", "id": "03198500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03198500", "obs": "',
                   WV_OBS[match("03198500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03198500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03198500.png"}, "geometry": {"type": "Point", "coordinates": [-81.711667, 38.179722]}}, {"type": "Feature", "properties": {"name": "TUG FORK AT WILLIAMSON", "id": "03213700", "url": "https://waterdata.usgs.gov/ky/nwis/inventory/?site_no=03213700", "obs": "',
                   WV_OBS[match("03213700", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03213700", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03213700.png"}, "geometry": {"type": "Point", "coordinates": [-82.280278, 37.673056]}}, {"type": "Feature", "properties": {"name": "TUG FORK AT KERMIT", "id": "03214500", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03214500", "obs": "',
                   WV_OBS[match("03214500", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03214500", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03214500.png"}, "geometry": {"type": "Point", "coordinates": [-82.408889, 37.837222]}}, {"type": "Feature", "properties": {"name": "EAST FORK TWELVEPOLE CREEK NEAR DUNLOW", "id": "03206600", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03206600", "obs": "',
                   WV_OBS[match("03206600", WV_OBS[,1]), 2], '", "time": "', WV_OBS[match("03206600", WV_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03206600.png"}, "geometry": {"type": "Point", "coordinates": [-82.305972, 38.011833]}}, {"type": "Feature", "properties": {"name": "TYGART VALLEY R AT TYGART DAM NR GRAFTON", "id": "03056000", "url": "https://waterdata.usgs.gov/wv/nwis/inventory/?site_no=03056000", "obs": "',
                   WVNC_OBS[match("03056000", WVNC_OBS[,1]), 2], '", "time": "', WVNC_OBS[match("03056000", WVNC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03056000.png"}, "geometry": {"type": "Point", "coordinates": [-80.025278, 39.319722]}}, {"type": "Feature", "properties": {"name": "POWELL RIVER NEAR ARTHUR", "id": "03532000", "url": "https://waterdata.usgs.gov/tn/nwis/inventory/?site_no=03532000", "obs": "',
                   TN_OBS[2], '", "time": "', TN_OBS[3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_03532000.png"}, "geometry": {"type": "Point", "coordinates": [-83.630419, 36.542064]}}, {"type": "Feature", "properties": {"name": "DAN RIVER NEAR FRANCISCO", "id": "02068500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02068500", "obs": "',
                   NC_OBS[match("02068500", NC_OBS[,1]), 2], '", "time": "', NC_OBS[match("02068500", NC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02068500.png"}, "geometry": {"type": "Point", "coordinates": [-80.303056, 36.515]}}, {"type": "Feature", "properties": {"name": "MAYO RIVER NEAR PRICE", "id": "02070500", "url": "https://waterdata.usgs.gov/nwis/inventory/?site_no=02070500", "obs": "',
                   NC_OBS[match("02070500", NC_OBS[,1]), 2], '", "time": "', NC_OBS[match("02070500", NC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02070500.png"}, "geometry": {"type": "Point", "coordinates": [-79.991389, 36.533889]}}, {"type": "Feature", "properties": {"name": "SMITH RIVER AT EDEN", "id": "02074000", "url": "https://waterdata.usgs.gov/nc/nwis/inventory/?site_no=02074000", "obs": "',
                   NC_OBS[match("02074000", NC_OBS[,1]), 2], '", "time": "', NC_OBS[match("02074000", NC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02074000.png"}, "geometry": {"type": "Point", "coordinates": [-79.765556, 36.525556]}}, {"type": "Feature", "properties": {"name": "MAYO CR NR BETHEL HILL", "id": "02077670", "url": "https://waterdata.usgs.gov/nc/nwis/inventory/?site_no=02077670", "obs": "',
                   NC_OBS[match("02077670", NC_OBS[,1]), 2], '", "time": "', NC_OBS[match("02077670", NC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02077670.png"}, "geometry": {"type": "Point", "coordinates": [-78.871944, 36.540833]}}, {"type": "Feature", "properties": {"name": "ROANOKE RIVER AT ROANOKE RAPIDS", "id": "02080500", "url": "https://waterdata.usgs.gov/nc/nwis/inventory/?site_no=02080500", "obs": "',
                   NC_OBS[match("02080500", NC_OBS[,1]), 2], '", "time": "', NC_OBS[match("02080500", NC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02080500.png"}, "geometry": {"type": "Point", "coordinates": [-77.633611, 36.46]}}, {"type": "Feature", "properties": {"name": "HYCO R BL ABAY D NR MCGEHEES MILL", "id": "02077303", "url": "https://waterdata.usgs.gov/nc/nwis/inventory/?site_no=02077303", "obs": "',
                   WVNC_OBS[match("02077303", WVNC_OBS[,1]), 2], '", "time": "', WVNC_OBS[match("02077303", WVNC_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_02077303.png"}, "geometry": {"type": "Point", "coordinates": [-78.9975, 36.5225]}}, {"type": "Feature", "properties": {"name": "NORWALK RIVER AT SOUTH WILTON", "id": "01209700", "url": "https://waterdata.usgs.gov/ct/nwis/inventory/?site_no=01209700", "obs": "',
                   CT_OBS[match("01209700", CT_OBS[,1]), 2], '", "time": "', CT_OBS[match("01209700", CT_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01209700.png"}, "geometry": {"type": "Point", "coordinates": [-73.419544, 41.163767]}}, {"type": "Feature", "properties": {"name": "HOUSATONIC RIVER AT GAYLORDSVILLE", "id": "01200500", "url": "https://waterdata.usgs.gov/ct/nwis/inventory/?site_no=01200500", "obs": "',
                   CT_OBS[match("01200500", CT_OBS[,1]), 2], '", "time": "', CT_OBS[match("01200500", CT_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01200500.png"}, "geometry": {"type": "Point", "coordinates": [-73.490278, 41.653056]}}, {"type": "Feature", "properties": {"name": "TENMILE RIVER NEAR GAYLORDSVILLE", "id": "01200000", "url": "https://waterdata.usgs.gov/ny/nwis/inventory/?site_no=01200000", "obs": "',
                   CT_OBS[match("01200000", CT_OBS[,1]), 2], '", "time": "', CT_OBS[match("01200000", CT_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01200000.png"}, "geometry": {"type": "Point", "coordinates": [-73.528683, 41.658764]}}, {"type": "Feature", "properties": {"name": "SALMON CREEK AT LIME ROCK", "id": "01199050", "url": "https://waterdata.usgs.gov/ct/nwis/inventory/?site_no=01199050", "obs": "',
                   CT_OBS[match("01199050", CT_OBS[,1]), 2], '", "time": "', CT_OBS[match("01199050", CT_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01199050.png"}, "geometry": {"type": "Point", "coordinates": [-73.391389, 41.942222]}}, {"type": "Feature", "properties": {"name": "GREEN RIVER NEAR GREAT BARRINGTON", "id": "01198000", "url": "https://waterdata.usgs.gov/ma/nwis/inventory/?site_no=01198000", "obs": "',
                   MA_OBS[match("01198000", MA_OBS[,1]), 2], '", "time": "', MA_OBS[match("01198000", MA_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01198000.png"}, "geometry": {"type": "Point", "coordinates": [-73.391231, 42.192908]}}, {"type": "Feature", "properties": {"name": "HOUSATONIC RIVER NEAR GREAT BARRINGTON", "id": "01197500", "url": "https://waterdata.usgs.gov/ma/nwis/inventory/?site_no=01197500", "obs": "',
                   MA_OBS[match("01197500", MA_OBS[,1]), 2], '", "time": "', MA_OBS[match("01197500", MA_OBS[,1]), 3], 
                   '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_01197500.png"}, "geometry": {"type": "Point", "coordinates": [-73.354667, 42.231917]}}]};', sep="")

# Export data to geojson.
cat(json_merge, file="/home/staff/klr324/marisa.psu.edu/mapdata/OHDEDCWVTNNCCTMA_stream_obs.js")
# --------------------------------------------------------------------------------------------------------------------
