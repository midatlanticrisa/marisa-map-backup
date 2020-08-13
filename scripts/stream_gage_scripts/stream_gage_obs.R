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

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

# --------------------------------------------------------------------------------------------------------------------
# We only need todays date to get the most recent value.
eDate <- Sys.Date()      # End date
#bDate = Sys.Date()-1      # Beginning date

cores <- 3

##read in the .csvs associated with each state, to be able to more efficiantly format the output geojson file
stateGageFiles <- list.files(paste0(inDir, "stream_gage_scripts/stream_gages_csv/"), pattern=".csv", full.names=T)
#gageCSVs <- lapply(stateGageFiles, read.csv)
##subset files unitl ready to map to full country
#subStates <- c("pennsylvania", "delaware")
#subStates <- c("newjersey", "delaware")
#subStates <- c("pennsylvania", "delaware", "maryland", "DC", "newyork", "newjersey", "/virginia", "westvirginia", "ohio", "connecticut", "massachusetts", "northcarolina")
subStates <- c("pennsylvania", "delaware", "maryland", "DC", "newyork", "newjersey", "/virginia", "westvirginia", "ohio", "connecticut", "massachusetts")
gageCSVs <- lapply(stateGageFiles[sapply(subStates, grep, x=stateGageFiles)], read.csv)

##the rest of the script
gageRecs <- do.call(rbind.data.frame, gageCSVs)
gageRecs <- gageRecs[gageRecs$SiteLongitude>=-82.0 & gageRecs$SiteLongitude<=-73.0 & gageRecs$SiteLatitude>=36.0 & gageRecs$SiteLatitude<=43.5,]
# Site numbers should be between 8 and 15 digits long
shortIDs <- which(nchar(gageRecs$SiteNumber)<8)
gageRecs$SiteNumber[shortIDs] <- paste0("0", gageRecs$SiteNumber[shortIDs])

stationIDs <- gageRecs$SiteNumber

# --------------------------------------------------------------------------------------------------------------------

gageTmpURLs <- paste0('https://waterdata.usgs.gov/nwis/uv?cb_00010=on&format=rdb&site_no=', stationIDs, '&period=&begin_date=', eDate, '&end_date=', eDate)
gageDisURLs <- paste0('https://waterdata.usgs.gov/nwis/uv?cb_00060=on&format=rdb&site_no=', stationIDs, '&period=&begin_date=', eDate, '&end_date=', eDate)
gageGagURLs <- paste0('https://waterdata.usgs.gov/nwis/uv?cb_00065=on&format=rdb&site_no=', stationIDs, '&period=&begin_date=', eDate, '&end_date=', eDate)

if(cores>1){
  library(parallel)  
  gageTemps <- mclapply(gageTmpURLs, function(x){if(getURL(x)=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(x,"America/New_York", "latest"))}}, mc.cores=cores)
  
  gageDischarge <- mclapply(gageDisURLs, function(x){if(getURL(x)=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(x,"America/New_York", "latest"))}}, mc.cores=cores)
  
  gageHeight <- mclapply(gageGagURLs, function(x){if(getURL(x)=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(x,"America/New_York", "latest"))}}, mc.cores=cores)
}else{
  gageTemps <- lapply(gageTmpURLs, function(x){if(getURL(x)=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(x,"America/New_York", "latest"))}})
  
  gageDischarge <- lapply(gageDisURLs, function(x){if(getURL(x)=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(x,"America/New_York", "latest"))}})
  
  gageHeight <- lapply(gageGagURLs, function(x){if(getURL(x)=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(x,"America/New_York", "latest"))}})
}

##the process of transforming the collected data into a single dataframe
fullGageTemps <- cbind.data.frame(stationIDs, do.call(rbind.data.frame, gageTemps))
fullGageDischarge <- cbind.data.frame(stationIDs, do.call(rbind.data.frame, gageDischarge))
fullGageHeight <- cbind.data.frame(stationIDs, do.call(rbind.data.frame, gageHeight))
#colnames(fullGageTemps)[2:3] <- c("temp", "dateTime")
#colnames(fullGageDischarge)[2:3] <- c("discharge", "dateTime")  
#colnames(fullGageHeight)[2:3] <- c("gageHeight", "dateTime")
colnames(fullGageTemps)[2:4] <- c("temp", "date_t", "time_t")
colnames(fullGageDischarge)[2:4] <- c("discharge", "date_d", "time_d")  
colnames(fullGageHeight)[2:4] <- c("gageHeight", "date_h", "time_h")
fullObsData <- merge(fullGageTemps, fullGageDischarge, by="stationIDs")
fullObsData <- merge(fullObsData, fullGageHeight, by="stationIDs")

##double check to make sure have to latest data time 
#fullObsData$latestDateTime <- apply(fullObsData[,grepl("dateTime", colnames(fullObsData))], MARGIN=1, FUN=min, na.rm=T)
#fullObsData$latestDateTime[is.na(fullObsData$latestDateTime)] <- ""
fullObsData$date <- NA
fullObsData$time <- NA
findMinDate <- unlist(mapply(function(td,dd,hd){dateVars<-c(td,dd,hd);
                      if(FALSE %in% unique(is.na(dateVars))){
                        return(which.min(dateVars))
                      }else{
                        return(NA)}}, 
                      td=utctime(as.POSIXlt(paste(fullObsData$date_t,fullObsData$time_t),format="%b %d, %Y %I:%M %p",tz="EDT")),
                      dd=utctime(as.POSIXlt(paste(fullObsData$date_d,fullObsData$time_d),format="%b %d, %Y %I:%M %p",tz="EDT")),
                      hd=utctime(as.POSIXlt(paste(fullObsData$date_h,fullObsData$time_h),format="%b %d, %Y %I:%M %p",tz="EDT"))))
fullObsData$date[which(is.na(findMinDate)==F & findMinDate==1)] <- as.character(fullObsData$date_t[which(is.na(findMinDate)==F & findMinDate==1)])
fullObsData$time[which(is.na(findMinDate)==F & findMinDate==1)] <- as.character(fullObsData$time_t[which(is.na(findMinDate)==F & findMinDate==1)])
fullObsData$date[which(is.na(findMinDate)==F & findMinDate==2)] <- as.character(fullObsData$date_d[which(is.na(findMinDate)==F & findMinDate==2)])
fullObsData$time[which(is.na(findMinDate)==F & findMinDate==2)] <- as.character(fullObsData$time_d[which(is.na(findMinDate)==F & findMinDate==2)])
fullObsData$date[which(is.na(findMinDate)==F & findMinDate==3)] <- as.character(fullObsData$date_h[which(is.na(findMinDate)==F & findMinDate==3)])
fullObsData$time[which(is.na(findMinDate)==F & findMinDate==3)] <- as.character(fullObsData$time_h[which(is.na(findMinDate)==F & findMinDate==3)])

##create the observation string to the map popup
#stationObs <- data.frame(stationIDs=fullObsData$stationIDs, obsString=createObsString(fullObsData), latestDate=paste0("<br/><br/>Last Updated on ", fullObsData$latestDateTime))
stationObs <- data.frame(stationIDs=fullObsData$stationIDs, obsString=createObsString(fullObsData), latestDate=fullObsData$date, latestTime=fullObsData$time)

##merge collected data with the read in csv records
fullStationData <- merge(x=gageRecs, y=stationObs, by.x="SiteNumber", by.y="stationIDs")

##create the feature record for each station for the geojson file
stream_string <- paste0('{"type": "Feature", "properties": {"name": "', fullStationData$SiteName, '", "id": "', fullStationData$SiteNumber, '", "url": "', fullStationData$SiteNWISURL, 
                        '", "obs": "', fullStationData$obsString, '", "time": "', fullStationData$latestDate, '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', fullStationData$SiteNumber,
                        '.png"}, "geometry": {"type": "Point", "coordinates": [', fullStationData$SiteLongitude, ',', fullStationData$SiteLatitude, ']}}')
##create the final string for the output file
json_merge <- paste0('Streams = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')


# Export data to geojson.
cat(json_merge, file=paste0(outDir, "stream_extend.json"))


#############################################
##test code to write out as geojson file
#library(rgdal)

#fullTab <- rbind.data.frame(NDBC_buoy_data, NDBC_stat_data, non_NDBC_data)
#coordinates(fullTab) <- c("lon", "lat")
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(weather_stat_data$lon), as.numeric(weather_stat_data$lat)), data=weather_stat_data, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#sptData <- data.frame(id=fullStationData$SiteNumber, name=fullStationData$SiteName, lon=fullStationData$SiteLongitude, lat=fullStationData$SiteLatitude)
#nonSptData <- data.frame(id=fullStationData$SiteNumber, obs=fullStationData$obsString, url=fullStationData$SiteNWISURL, time=fullStationData$latestDate)
#spTab <- SpatialPointsDataFrame(coords=cbind(as.numeric(sptData$lon), as.numeric(sptData$lat)), data=sptData, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#writeOGR(spTab, dsn=paste0(outDir, "testOutput/"), layer="streamGageObs", driver="ESRI Shapefile", overwrite_layer=T)
#write.csv(nonSptData, paste0(outDir, "testOutput/streamGageTab.csv"), row.names=F)
#write.csv(fullStationData, paste0(outDir, "testOutput/streamgagesFull.csv"), row.names=F)


