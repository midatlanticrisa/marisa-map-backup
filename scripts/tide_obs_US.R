# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Jan 25, 2019    - expanded to entire US
# prior edit: June 18, 2018
# prior edit: June 16, 2017
#
# This script parses XML data of current tide station observations from the
# National Ocean and Atmospheric Administration and outputs the results as
# a figure of preliminery 6-minute water level heights.
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
if (!require("XML")) { install.packages("XML") }
if (!require("httr")) { install.packages("httr") }

library(RCurl)
library(XML)
library(httr)
library(anytime)
library(pbapply)
library(parallel)


# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="rsc64dot1x-60.ems.psu.edu"){
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else{
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

##load station ids
load(paste0(outDir, "tideStationIDs.RData"))

# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width <- 4            # Width
p.height <- 2.5           # Height

# Set up common variables.
datum <- "MLLW"
gl.datum <- "IGLD"
msl.datum <- "MSL"
timezone <- "GMT"
units <- "metric"
cores <- 3

# --------------------------------------------------------------------------------------------------------------------


# Run through each station.
tideStations <- pbsapply(tideIDs, tideStationData, spDatum=datum, timez=timezone, un=units)

#cl <- makeCluster(cores)
#clusterEvalQ(cl, {library(RCurl); library(XML); library(httr); library(anytime)})
#clusterExport(cl, varlist=c("tideIDs", "tideStationData", "datum", "timezone", "units", "collectLatestTidal", "retry"))
#tideStations <- parLapply(cl, tideIDs, tideStationData, spDatum=datum, timez=timezone, un=units)  
#stopCluster(cl)


# --------------------------------------------------------------------------------------------------------------------

station_string = function(ID, MET, OBS, TIME){
  if(is.null(ncol(MET))){
    str = paste('{"type": "Feature", "properties": {"name": "', MET[2], '", "id": "', ID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', ID, '", "obs": "',
                OBS[2], '", "time": "', TIME[2], '", "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', ID, '.png"},
                "geometry": {"type": "Point", "coordinates": [', MET[4], ',',  MET[3], ']}}', sep="")
  } else {
    str = paste('{"type": "Feature", "properties": {"name": "', MET[match(ID, MET[,1]), 2], '", "id": "', ID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', ID, '", "obs": "',
                OBS[match(ID, OBS[,1]), 2], '", "time": "', TIME[match(ID, TIME[,1]), 2], '",
                "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', ID, '.png"},
                "geometry": {"type": "Point", "coordinates": [', MET[match(ID, MET[,1]), 4], ',',  MET[match(ID, MET[,1]), 3], ']}}', sep="")
    }
  return(str)
}
# --------------------------------------------------------------------------------------------------------------------
# Combine all info into one string
# Run through each Alabama station.
al_st = station_string(AL_ID, AL_data$MET, AL_data$OBS, AL_data$TIME)
al_st = paste(al_st, ",", collapse="")
# Run through each Alaska station.
ak_st = station_string(AK_ID, AK_data$MET, AK_data$OBS, AK_data$TIME)
ak_st = paste(ak_st, ",", collapse="")
# Run through each California station.
ca_st = station_string(CA_ID, CA_data$MET, CA_data$OBS, CA_data$TIME)
ca_st = paste(ca_st, ",", collapse="")
# Run through each Conneticut station.
ct_st = station_string(CT_ID, CT_data$MET, CT_data$OBS, CT_data$TIME)
ct_st = paste(ct_st, ",", collapse="")
# Run through each Delaware station.
de_st = station_string(DE_ID, DE_data$MET, DE_data$OBS, DE_data$TIME)
de_st = paste(de_st, ",", collapse="")
# Run through each District of Columbia station.
dc_st = station_string(DC_ID, DC_data$MET, DC_data$OBS, DC_data$TIME)
dc_st = paste(dc_st, ",", collapse="")
# Run through each Florida station.
fl_st = station_string(FL_ID, FL_data$MET, FL_data$OBS, FL_data$TIME)
fl_st = paste(fl_st, ",", collapse="")
# Run through each Georgia station.
ga_st = station_string(GA_ID, GA_data$MET, GA_data$OBS, GA_data$TIME)
ga_st = paste(ga_st, ",", collapse="")
# Run through each Hawaii station.
hi_st = station_string(HI_ID, HI_data$MET, HI_data$OBS, HI_data$TIME)
hi_st = paste(hi_st, ",", collapse="")
# Run through each Louisiana station.
la_st = station_string(LA_ID, LA_data$MET, LA_data$OBS, LA_data$TIME)
la_st = paste(la_st, ",", collapse="")
# Run through each Louisiana MSL station.
la_msl_st = station_string(LA_MSL_ID, LA_MSL_data$MET, LA_MSL_data$OBS, LA_MSL_data$TIME)
la_msl_st = paste(la_msl_st, ",", collapse="")
# Run through each Maine station.
me_st = station_string(ME_ID, ME_data$MET, ME_data$OBS, ME_data$TIME)
me_st = paste(me_st, ",", collapse="")
# Run through each Maryland station.
md_st = station_string(MD_ID, MD_data$MET, MD_data$OBS, MD_data$TIME)
md_st = paste(md_st, ",", collapse="")
# Run through each Massachusetts station.
ma_st = station_string(MA_ID, MA_data$MET, MA_data$OBS, MA_data$TIME)
ma_st = paste(ma_st, ",", collapse="")
# Run through each Mississippi station.
ms_st = station_string(MS_ID, MS_data$MET, MS_data$OBS, MS_data$TIME)
ms_st = paste(ms_st, ",", collapse="")
# Run through each New Hampshire station.
nh_st = station_string(NH_ID, NH_data$MET, NH_data$OBS, NH_data$TIME)
nh_st = paste(nh_st, ",", collapse="")
# Run through each New Jersey station.
nj_st = station_string(NJ_ID, NJ_data$MET, NJ_data$OBS, NJ_data$TIME)
nj_st = paste(nj_st, ",", collapse="")
# Run through each New York station.
ny_st = station_string(NY_ID, NY_data$MET, NY_data$OBS, NY_data$TIME)
ny_st = paste(ny_st, ",", collapse="")
# Run through each North Carolina station.
nc_st = station_string(NC_ID, NC_data$MET, NC_data$OBS, NC_data$TIME)
nc_st = paste(nc_st, ",", collapse="")
# Run through each Oregon station.
or_st = station_string(OR_ID, OR_data$MET, OR_data$OBS, OR_data$TIME)
or_st = paste(or_st, ",", collapse="")
# Run through each Pennsylvania station.
pa_st = station_string(PA_ID, PA_data$MET, PA_data$OBS, PA_data$TIME)
pa_st = paste(pa_st, ",", collapse="")
# Run through each Rhode Island station.
ri_st = station_string(RI_ID, RI_data$MET, RI_data$OBS, RI_data$TIME)
ri_st = paste(ri_st, ",", collapse="")
# Run through each South Carolina station.
sc_st = station_string(SC_ID, SC_data$MET, SC_data$OBS, SC_data$TIME)
sc_st = paste(sc_st, ",", collapse="")
# Run through each Texas station.
tx_st = station_string(TX_ID, TX_data$MET, TX_data$OBS, TX_data$TIME)
tx_st = paste(tx_st, ",", collapse="")
# Run through each Texas MSL station.
tx_msl_st = station_string(TX_MSL_ID, TX_MSL_data$MET, TX_MSL_data$OBS, TX_MSL_data$TIME)
tx_msl_st = paste(tx_msl_st, ",", collapse="")
# Run through each Virginia station.
va_st = station_string(VA_ID, VA_data$MET, VA_data$OBS, VA_data$TIME)
va_st = paste(va_st, ",", collapse="")
# Run through each Washington station.
wa_st = station_string(WA_ID, WA_data$MET, WA_data$OBS, WA_data$TIME)
wa_st = paste(wa_st, ",", collapse="")
# Run through each station in the Great lakes.
gl_st = station_string(GL_ID, GL_data$MET, GL_data$OBS, GL_data$TIME)
gl_st = paste(gl_st, ",", collapse="")
gldr_st = station_string(GLDR_ID, GLDR_data$MET, GLDR_data$OBS, GLDR_data$TIME)
gldr_st = paste(gldr_st, ",", collapse="")
gllh_st = station_string(GLLH_ID, GLLH_data$MET, GLLH_data$OBS, GLLH_data$TIME)
gllh_st = paste(gllh_st, ",", collapse="")
grlm_st = station_string(GRLM_ID, GRLM_data$MET, GRLM_data$OBS, GRLM_data$TIME)
grlm_st = paste(grlm_st, ",", collapse="")
grlo_st = station_string(GRLO_ID, GRLO_data$MET, GRLO_data$OBS, GRLO_data$TIME)
grlo_st = paste(grlo_st, ",", collapse="")
gllsc_st = station_string(GLLSC_ID, GLLSC_data$MET, GLLSC_data$OBS, GLLSC_data$TIME)
gllsc_st = paste(gllsc_st, ",", collapse="")
glls_st = station_string(GLLS_ID, GLLS_data$MET, GLLS_data$OBS, GLLS_data$TIME)
glls_st = paste(glls_st, ",", collapse="")
glnr_st = station_string(GLNR_ID, GLNR_data$MET, GLNR_data$OBS, GLNR_data$TIME)
glnr_st = paste(glnr_st, ",", collapse="")
glscr_st = station_string(GLSCR_ID, GLSCR_data$MET, GLSCR_data$OBS, GLSCR_data$TIME)
glscr_st = paste(glscr_st, ",", collapse="")
glslr_st = station_string(GLSLR_ID, GLSLR_data$MET, GLSLR_data$OBS, GLSLR_data$TIME)
glslr_st = paste(glslr_st, ",", collapse="")
glsmr_st = station_string(GLSMR_ID, GLSMR_data$MET, GLSMR_data$OBS, GLSMR_data$TIME)
glsmr_st_last = glsmr_st[length(glsmr_st)] #make sure the last feature doesn't end with a ","
glsmr_st = paste(glsmr_st[1:length(glsmr_st) - 1], ",", collapse="")

# --------------------------------------------------------------------------------------------------------------------
json_merge = paste('tideStations = {"type": "FeatureCollection","features": [',
                   al_st, ak_st, ca_st, ct_st, de_st, dc_st, fl_st, ga_st, hi_st, la_st,
                   la_msl_st, me_st, md_st, ma_st, ms_st, nh_st, nj_st, ny_st, nc_st, or_st,
                   pa_st, ri_st, sc_st, tx_st, tx_msl_st, va_st, wa_st, gl_st, gldr_st, gllh_st,
                   grlm_st, grlo_st, gllsc_st, glls_st, glnr_st, glscr_st, glslr_st, glsmr_st,
                   glsmr_st_last, ']};', sep="")

# Export data to geojson.
cat(json_merge, file="/net/www/www.marisa.psu.edu/htdocs/mapdata/tide_station_obs_extend.js")
