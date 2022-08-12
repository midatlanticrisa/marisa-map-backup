# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: April 4, 2017; April 14 (change to creating geojson file)
#
# This script parses XML data of current weather station observations from the
# National Weather Service and outputs the results in a single file.
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
ptm <- proc.time()
if (!require("XML")) { install.packages("XML") }
if (!require("httr")) { install.packages("httr") }
if (!require("stringr")) { install.packages("stringr") }

library(XML)
library(httr)
library(stringr)
library(compiler)
library(rgdal)
library(geojson)
library(geojsonio)
#install.packages("geojsonlint")
enableJIT(3)
enableJIT(3)


# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
#if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata")){
#  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata")
#}
# --------------------------------------------------------------------------------------------------------------------
#inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/warnings_watches_advisories/"

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
# important file locations
if(comp$nodename=="E2-EES-RSML638.local" | comp$nodename=="E2-EES-RSML638" | comp$nodename=="rsc64dot1x-59.ems.psu.edu"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="firkin.eesi.psu.edu"){  ##firkin
  inDir <- "/firkin/s0/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/firkin/s0/mdl5548/marisaMapOutput/"
}else{ ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}
inDir <- paste0(baseDir, "warnings_watches_advisories/")
cores <- 1


# https://www.weather.gov/help-map
# NWS_cols[ ,1] : List of all warnings, watches, advisories, and statements
# NWS_cols[ ,2] : Associated colors for all warnings, watches, advisories, and statements.
NWS_cols = read.csv(paste0(inDir,"NationalWeatherService_Events.csv"))

# https://alerts.weather.gov/cap
# county_codes[ ,1] : County code
# county_codes[ ,2] : County name
# county_codes[ ,3] : State (DE, DC, MD, NJ, NY)
county_codes = read.csv(paste0(inDir,"NationalWeatherService_CountyCodes.csv"))
atlantic_codes = read.csv(paste0(inDir,"NationalWeatherService_AtlanticCodes.csv"))
greatlake_codes = read.csv(paste0(inDir,"NationalWeatherService_GreatLakesCodes.csv"))

stateTrans <- matrix(c("10", "DE",
                       "11", "DC",
                       "24", "MD",
                       "34", "NJ",
                       "36", "NY",
                       "39", "OH",
                       "42", "PA",
                       "51", "VA",
                       "54", "WV"), 
                     nrow=9, ncol=2, byrow=T)
colnames(stateTrans) <- c("STATE", "stateABRS")

##read in the spatial data
spCounties <- readOGR(dsn=inDir, layer="MARISASite_selectStates") 
spAtlantic <- readOGR(dsn=inDir, layer="MARISASite_atlantic")
spGrtLakes <- readOGR(dsn=inDir, layer="MARISASite_GLakes")
  
##sourced functions
source(paste0(baseDir, "MARISA_mapFunctions.R"))

# --------------------------------------------------------------------------------------------------------------------
# Create empty vectors.
#xml_info <- mat.or.vec(nrow(county_codes), 3)
# # Start the clock!
# ptm <- proc.time()
# Run through each station url.
#for(i in 1:length(county_codes$code)){
  # Run the function parsing the data we want.
#  xml_info[i,1:3] <- parse_xml(as.character(county_codes$code[i]))
#}
# Run through each county
#ptmDownload <- proc.time()
if(cores>1){
  ##run in parallel
  library(parallel)
  countyXML <- mclapply(as.character(county_codes$code), parseWW_xml, mc.cores=cores)
  atlanticXML <- mclapply(as.character(atlantic_codes$code), parseWW_xml, mc.cores=cores)
  grtLakesXML <- mclapply(as.character(greatlake_codes$code), parseWW_xml, mc.cores=cores)
}else{
  ##run on single core
  countyXML <- lapply(as.character(county_codes$code), parseWW_xml)
  #countyXML_sub1 <- lapply(as.character(county_codes$code[1:110]), parseWW_xml)
  #countyXML_sub2 <- lapply(as.character(county_codes$code[111:220]), parseWW_xml)
  #countyXML_sub3 <- lapply(as.character(county_codes$code[221:330]), parseWW_xml)
  #countyXML_sub4 <- lapply(as.character(county_codes$code[331:440]), parseWW_xml)
  #countyXML_sub5 <- lapply(as.character(county_codes$code[441:nrow(county_codes)]), parseWW_xml)
  #countyXML <- c(countyXML_sub1, countyXML_sub2, countyXML_sub3, countyXML_sub4, countyXML_sub5)
  atlanticXML <- lapply(as.character(atlantic_codes$code), parseWW_xml)
  grtLakesXML <- lapply(as.character(greatlake_codes$code), parseWW_xml)
}
ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))

countyInfo <- do.call(rbind.data.frame, countyXML)
atlanticInfo <- do.call(rbind.data.frame, atlanticXML)
grtLakesInfo <- do.call(rbind.data.frame, grtLakesXML)
##rename columns
colnames(countyInfo) <- c("countyCodes", "OBS", "COL")
colnames(atlanticInfo) <- c("atlanticCodes", "OBS", "COL")
colnames(grtLakesInfo) <- c("grtLakesCodes", "OBS", "COL")

# # Stop the clock
# proc.time() - ptm
# --------------------------------------------------------------------------------------------------------------------
# Edit a premade geojson file of county boarders based off of the 2010 Census: http://eric.clst.org/tech/usgeojson/
# Merge geojson objects into a specific file format with data as the variable name.
spCounties$countyCodes <- paste0(spCounties$stateABRS, "C", spCounties$COUNTY)
countyInfo <- merge(x=spCounties, y=countyInfo, by="countyCodes")
atlanticInfo <- merge(x=spAtlantic, y=atlanticInfo, by.x="ID", by.y="atlanticCodes")
grtLakesInfo <- merge(x=spGrtLakes, y=grtLakesInfo, by.x="ID", by.y="grtLakesCodes")
row.names(countyInfo) <- as.character(countyInfo$GEO_ID)
row.names(atlanticInfo) <- as.character(atlanticInfo$ID)
row.names(grtLakesInfo) <- as.character(grtLakesInfo$ID)
outObjNames <- c("DEtoNYalerts = ", "OHPAalerts = ", "VAWValerts = ", "AT_p1_alerts = ", "AT_p2_alerts = ", "AT_p3_alerts = ", "AT_p4_alerts = ", "GLalerts = ")
subSelections <- list(c("DE","MD","NJ","NY"), c("OH","PA"), c("VA","WV"), 1:12, 13:28, 29:42, 43:57, "placeholder")
writeOutNames <- c("DEtoNYcounty_alerts.json", "OHPAcounty_alerts.json", "VAWVcounty_alerts.json", "atlantic_p1_alerts.json", "atlantic_p2_alerts.json", 
                   "atlantic_p3_alerts.json", "atlantic_p4_alerts.json", "greatlakes_alerts.json")

##create and write output
if(cores>1){
  ##run in parallel
  library(parallel)
  #x = 4
  #nams = outObjNames[x]
  #subS = subSelections[x]
  createOutput <- mcmapply(function(nams, subS){
                                  if(length(grep("AT_", nams))>0){
                                    subTab<-paste0(nams, as.character(geojson_json(atlanticInfo[subS,])))
                                  }else if(length(grep("place", nams))>0){
                                    subTab<-paste0(nams, as.character(geojson_json(grtLakesInfo)))
                                  }else{
                                    subTab<-paste0(nams, as.character(geojson_json(countyInfo[countyInfo$stateABRS%in%subS,])))
                                  }
                                  return(subTab)}, nams=outObjNames, subS=subSelections, mc.cores=cores)
  mcmapply(cat, createOutput, file=paste0(outDir, writeOutNames), mc.cores=cores)
}else{
  ##run on single core
  x = 2
  nams = outObjNames[x]
  subS = subSelections[x]
  createOutput <- mapply(function(nams, subS){
                                  if(length(grep("AT_", nams))>0){
                                    subTab<-paste0(nams, as.character(geojson_json(atlanticInfo[subS,])))
                                  }else if(length(grep("place", nams))>0){
                                    subTab<-paste0(nams, as.character(geojson_json(grtLakesInfo)))
                                  }else{
                                    subTab<-paste0(nams, as.character(geojson_json(countyInfo[countyInfo$stateABRS%in%subS,])))
                                  }
                                  return(subTab)}, nams=outObjNames, subS=subSelections)
  mapply(cat, createOutput, file=paste0(outDir, writeOutNames))
}

ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd[3]))

##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "weatherWarningsTracking.RData")
if(file.exists(timeFile)==T){
  load(timeFile)
  timeWeatherWarn[nrow(timeWeatherWarn)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timeWeatherWarn", file=timeFile)
}else{
  timeWeatherWarn <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timeWeatherWarn", file=timeFile)
}


#DEtoNYwarnings <- paste0("DEtoNYalerts = ", as.character(geojson_json(countyInfo[countyInfo$stateABRS%in%c("DE","MD","NJ","NY"),])))
#OHPAwarnings <- paste0("OHPAalerts = ", as.character(geojson_json(countyInfo[countyInfo$stateABRS%in%c("OH","PA"),])))
#VAWVwarnings <- paste0("VAWValerts = ", as.character(geojson_json(countyInfo[countyInfo$stateABRS%in%c("VA","WV"),]))) 
#atlArea1 <- paste0("AT_p1_alerts = ", as.character(geojson_json(atlanticInfo[1:12,])))
#atlArea2 <- paste0("AT_p2_alerts = ", as.character(geojson_json(atlanticInfo[13:28,])))
#atlArea3 <- paste0("AT_p3_alerts = ", as.character(geojson_json(atlanticInfo[29:42,])))
#atlArea4 <- paste0("AT_p4_alerts = ", as.character(geojson_json(atlanticInfo[43:57,])))
#grtLakes <- paste0("GLalerts = ", as.character(geojson_json(grtLakesInfo)))
#cat(json_merge, file="/net/www/www.marisa.psu.edu/htdocs/mapdata/DEtoNYcounty_alerts.json")
#cat(DEtoNYwarnings, file=paste0(outDir, "DEtoNYcounty_alerts.json"))
#cat(OHPAwarnings, file=paste0(outDir, "OHPAcounty_alerts.json"))
#cat(VAWVwarnings, file=paste0(outDir, "VAWVcounty_alerts.json"))
#cat(atlArea1, file=paste0(outDir, "atlantic_p1_alerts.json"))
#cat(atlArea2, file=paste0(outDir, "atlantic_p2_alerts.json"))
#cat(atlArea3, file=paste0(outDir, "atlantic_p3_alerts.json"))
#cat(atlArea4, file=paste0(outDir, "atlantic_p4_alerts.json"))
#cat(grtLakes, file=paste0(outDir, "greatlakes_alerts.json"))

# --------------------------------------------------------------------------------------------------------------------

# Virginia VAC515 doesn't exist: https://alerts.weather.gov/cap/va.php?x=3
# { "type": "Feature", "properties": { "GEO_ID": "0500000US51515", "STATE": "51", "COUNTY": "515", "NAME": "Bedford", "LSAD": "city", "CENSUSAREA": 6.878000 }, "geometry": { "type": "Polygon", "coordinates": [ [ [ -79.543597, 37.324178 ], [ -79.535648, 37.355314 ], [ -79.487211, 37.345777 ], [ -79.503624, 37.316653 ], [ -79.543597, 37.324178 ] ] ] } },
