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
ptm <- proc.time()
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(readr)
library(compiler)
library(anytime)
enableJIT(3)
enableJIT(3)


# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local" | comp$nodename=="E2-EES-RSML638" | comp$nodename=="rsc64dot1x-59.ems.psu.edu"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="firkin.eesi.psu.edu"){  ##firkin
  inDir <- "/firkin/s0/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/firkin/s0/mdl5548/marisaMapOutput"
}else{ ##idocrase
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

cores <- 1

##read in the .csvs associated with each state, to be able to more efficiantly format the output geojson file
stateGageFiles <- list.files(paste0(inDir, "stream_gage_scripts/stream_gages_csv/"), pattern=".csv", full.names=T)
#gageCSVs <- lapply(stateGageFiles, read.csv)
##subset files unitl ready to map to full country
#subStates <- c("pennsylvania", "delaware")
subStates <- c("ohio", "Delaware", "DC", "westvirginia", "conneticut", "massachusetts")
#subStates <- c("pennsylvania", "delaware", "maryland", "DC", "newyork", "newjersey", "/virginia", "westvirginia", "ohio", "connecticut", "massachusetts", "northcarolina")
#subStates <- c("pennsylvania", "Delaware", "maryland", "DC", "newyork", "newjersey", "/virginia", "westvirginia", "ohio", "conneticut", "massachusetts")
gageCSVs <- lapply(subStates, function(st){readTab<-read.csv(stateGageFiles[grep(st, stateGageFiles)]);
                                            ##adding state column, as descriptions included are not all formatted the same
                                            readTab$state<-st;
                                            return(readTab)})

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

ptmDownload <- proc.time()
if(cores>1){
  library(parallel)  
  gageTemps <- mclapply(gageTmpURLs, function(x){chkURL<-getURL(x)
                                                  if(chkURL=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(chkURL,x,"America/New_York", "latest"))}}, mc.cores=cores)
  
  gageDischarge <- mclapply(gageDisURLs, function(x){chkURL<-getURL(x)
                                                  if(chkURL=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(chkURL,x,"America/New_York", "latest"))}}, mc.cores=cores)
  
  gageHeight <- mclapply(gageGagURLs, function(x){chkURL<-getURL(x)
                                                  if(chkURL=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(chkURL,x,"America/New_York", "latest"))}}, mc.cores=cores)
}else{
  gageTemps <- lapply(gageTmpURLs, function(x){chkURL<-getURL(x)
                                                  if(chkURL=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(chkURL, x, "America/New_York", "latest"))}})
  
  gageDischarge <- lapply(gageDisURLs, function(x){chkURL<-getURL(x)
                                                  if(chkURL=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(chkURL, x, "America/New_York", "latest"))}})
  
  gageHeight <- lapply(gageGagURLs, function(x){chkURL<-getURL(x)
                                                  if(chkURL=="No sites/data found using the selection criteria specified \n"){
                                                    return(c(NA,NA,NA))}else{
                                                    return(usgs_dataRetrieveVar(chkURL, x, "America/New_York", "latest"))}})
}
ptmDownloadEnd <- proc.time() - ptmDownload
#print(paste0("Download Time: ", ptmDownloadEnd[3]))

##the process of transforming the collected data into a single dataframe
fullGageTemps <- cbind.data.frame(stationIDs, do.call(rbind.data.frame, gageTemps))
fullGageDischarge <- cbind.data.frame(stationIDs, do.call(rbind.data.frame, gageDischarge))
fullGageHeight <- cbind.data.frame(stationIDs, do.call(rbind.data.frame, gageHeight))
#colnames(fullGageTemps)[2:3] <- c("temp", "dateTime")
#colnames(fullGageDischarge)[2:3] <- c("discharge", "dateTime")  
#colnames(fullGageHeight)[2:3] <- c("gageHeight", "dateTime")
colnames(fullGageTemps)[2:4] <- c("temp", "date_t", "time_t")
##convert tempurature values from C to F
fullGageTemps$temp <- as.character(as.numeric(as.character(fullGageTemps$temp)) * (9/5) + 32)
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
                      td=utctime(as.POSIXlt(paste(fullObsData$date_t,fullObsData$time_t),format="%b %d, %Y %I:%M %p",tz="America/New_York")),
                      dd=utctime(as.POSIXlt(paste(fullObsData$date_d,fullObsData$time_d),format="%b %d, %Y %I:%M %p",tz="America/New_York")),
                      hd=utctime(as.POSIXlt(paste(fullObsData$date_h,fullObsData$time_h),format="%b %d, %Y %I:%M %p",tz="America/New_York"))))
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
fullStationData$latestDate <- as.character(fullStationData$latestDate)
fullStationData$latestTime <- as.character(fullStationData$latestTime)
if(TRUE%in%is.na(fullStationData$latestDate)){
  fullStationData$latestDate[is.na(fullStationData$latestDate)==T] <- max(fullStationData$latestDate[is.na(fullStationData$latestDate)==F])
  fullStationData$latestTime[is.na(fullStationData$latestTime)==T] <- max(fullStationData$latestTime[is.na(fullStationData$latestTime)==F])
}


##NJ and MD
#njmdRecs <- rbind.data.frame(fullStationData[grep("NJ", fullStationData$SiteName),], fullStationData[grep("MD", fullStationData$SiteName),])
##create the feature record for each station for the geojson file
#stream_string <- paste0('{"type": "Feature", "properties": {"name": "', njmdRecs$SiteName, '", "id": "', njmdRecs$SiteNumber, '", "url": "', njmdRecs$SiteNWISURL, 
#                        '", "obs": "', njmdRecs$obsString, '", "time": "', paste0("Last Updated on ", njmdRecs$latestDate, " at ", njmdRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', njmdRecs$SiteNumber,
#                        '.png"}, "geometry": {"type": "Point", "coordinates": [', njmdRecs$SiteLongitude, ',', njmdRecs$SiteLatitude, ']}}')
##create the final string for the output file
#json_merge <- paste0('NJMD_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')
# Export data to geojson.
#cat(json_merge, file=paste0(outDir, "NJMD_stream_obs.js"))

##NY
#if("newyork" %in% subStates){
#  nyRecs <- fullStationData[grep("NY", fullStationData$SiteName),]
  
  ##create the feature record for each station for the geojson file
#  stream_string <- paste0('{"type": "Feature", "properties": {"name": "', nyRecs$SiteName, '", "id": "', nyRecs$SiteNumber, '", "url": "', nyRecs$SiteNWISURL, 
#                          '", "obs": "', nyRecs$obsString, '", "time": "', paste0("Last Updated on ", nyRecs$latestDate, " at ", nyRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', nyRecs$SiteNumber,
#                          '.png"}, "geometry": {"type": "Point", "coordinates": [', nyRecs$SiteLongitude, ',', nyRecs$SiteLatitude, ']}}')
  ##create the final string for the output file
#  json_merge <- paste0('NY_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')
  # Export data to geojson.
#  cat(json_merge, file=paste0(outDir, "NY_stream_obs.js"))
#}

##OH, DE, DC, WV, CT, MA
#rangeRecs <- rbind.data.frame(fullStationData[grep("OH", fullStationData$SiteName),], fullStationData[grep("DE", fullStationData$SiteName),],
#                              fullStationData[grep("DC", fullStationData$SiteName),], fullStationData[grep("WV", fullStationData$SiteName),],
#                              fullStationData[grep("CT", fullStationData$SiteName),], fullStationData[grep("MA", fullStationData$SiteName),])
rangeRecs <- fullStationData

##create the feature record for each station for the geojson file
stream_string <- paste0('{"type": "Feature", "properties": {"name": "', rangeRecs$SiteName, '", "id": "', rangeRecs$SiteNumber, '", "url": "', rangeRecs$SiteNWISURL, 
                        '", "obs": "', rangeRecs$obsString, '", "time": "', paste0("Last Updated on ", rangeRecs$latestDate, " at ", rangeRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', rangeRecs$SiteNumber,
                        '.png"}, "geometry": {"type": "Point", "coordinates": [', rangeRecs$SiteLongitude, ',', rangeRecs$SiteLatitude, ']}}')
##create the final string for the output file
json_merge <- paste0('OHDEDCWVTNNCCTMA_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')

##save object with ids for plotting
OH_ID <- rangeRecs$SiteNumber[which(rangeRecs$state=="ohio")]
DE_ID <- rangeRecs$SiteNumber[which(rangeRecs$state=="Delaware")]
DC_ID <- rangeRecs$SiteNumber[which(rangeRecs$state=="DC")]
WV_ID <- rangeRecs$SiteNumber[which(rangeRecs$state=="westvirginia")]
CT_ID <- rangeRecs$SiteNumber[which(rangeRecs$state=="conneticut")]
MA_ID <- rangeRecs$SiteNumber[which(rangeRecs$state=="massachusetts")]
save("OH_ID", "DE_ID", "WV_ID", "DC_ID", "MA_ID", file=paste0(outDir, "OHDEDCWVTNNCCTMA_streamIDs.RData"))

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "OHDEDCWVTNNCCTMA_stream_obs.js"))

ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
timeFile <- paste0(outDir, "OHDEDCWVTNNCCTMAstreamGagesTracking.RData")
if(file.exists(timeFile)==T){
  load(timeFile)
  timeOHDEDCWVTNNCCTMAStreams[nrow(timeOHDEDCWVTNNCCTMAStreams)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
  save("timeOHDEDCWVTNNCCTMAStreams", file=timeFile)
}else{
  timeOHDEDCWVTNNCCTMAStreams <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
  save("timeOHDEDCWVTNNCCTMAStreams", file=timeFile)
}


##PA
#if("pennsylvania" %in% subStates){
#  paRecs <- fullStationData[grep("PA", fullStationData$SiteName),]
  
  ##create the feature record for each station for the geojson file
#  stream_string <- paste0('{"type": "Feature", "properties": {"name": "', paRecs$SiteName, '", "id": "', paRecs$SiteNumber, '", "url": "', paRecs$SiteNWISURL, 
#                          '", "obs": "', paRecs$obsString, '", "time": "', paste0("Last Updated on ", paRecs$latestDate, " at ", paRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', paRecs$SiteNumber,
#                          '.png"}, "geometry": {"type": "Point", "coordinates": [', paRecs$SiteLongitude, ',', paRecs$SiteLatitude, ']}}')
  ##create the final string for the output file
#  json_merge <- paste0('PA_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')
  # Export data to geojson.
#  cat(json_merge, file=paste0(outDir, "PA_stream_obs.js"))
#}

##VA
#if("/virginia" %in% subStates){
#  vaRecs <- fullStationData[grep("VA", fullStationData$SiteName),]
  
  ##create the feature record for each station for the geojson file
#  stream_string <- paste0('{"type": "Feature", "properties": {"name": "', vaRecs$SiteName, '", "id": "', vaRecs$SiteNumber, '", "url": "', vaRecs$SiteNWISURL, 
#                          '", "obs": "', vaRecs$obsString, '", "time": "', paste0("Last Updated on ", vaRecs$latestDate, " at ", vaRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', vaRecs$SiteNumber,
#                          '.png"}, "geometry": {"type": "Point", "coordinates": [', vaRecs$SiteLongitude, ',', vaRecs$SiteLatitude, ']}}')
  ##create the final string for the output file
#  json_merge <- paste0('VA_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')
  # Export data to geojson.
#  cat(json_merge, file=paste0(outDir, "VA_stream_obs.js"))
#}



##create the feature record for each station for the geojson file
#stream_string <- paste0('{"type": "Feature", "properties": {"name": "', fullStationData$SiteName, '", "id": "', fullStationData$SiteNumber, '", "url": "', fullStationData$SiteNWISURL, 
#                        '", "obs": "', fullStationData$obsString, '", "time": "', fullStationData$latestDate, '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', fullStationData$SiteNumber,
#                        '.png"}, "geometry": {"type": "Point", "coordinates": [', fullStationData$SiteLongitude, ',', fullStationData$SiteLatitude, ']}}')
##create the final string for the output file
#json_merge <- paste0('Streams = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')
# Export data to geojson.
#cat(json_merge, file=paste0(outDir, "stream_extend.json"))


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


