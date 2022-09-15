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
options(stringsAsFactors=F)
#ptm <- proc.time()
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(readr)
library(compiler)
library(anytime)
library(jsonlite)
library(stringr)
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
  outDir <- "/firkin/s0/mdl5548/marisaMapOutput/"
}else{ ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}
plottingDir <- paste0(outDir, "Stream_figs/")

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!dir.exists(outDir)){
  dir.create(outDir, recursive=T)
}
if (!dir.exists(plottingDir)){
  dir.create(plottingDir, recursive=T)
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))
source(paste0(inDir, "stream_gage_scripts/stream_gage_plot_func_test.R"))

# --------------------------------------------------------------------------------------------------------------------
# # We only need todays date to get the most recent value.
eDate <- Sys.Date()      # End date
bDate <- Sys.Date() - 7      # Beginning date

# Determine midnight and noon for dates of this previous week
day = 0:7
day_midnight = as.POSIXct(paste(Sys.Date() - day, "00:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
day_noon = as.POSIXct(paste(Sys.Date() - day, "12:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")

# --------------------------------------------------------------------------------------------------------------------

#testFile <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/paGages.json"
#testFile <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/testSubSites.json"

#outDir <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/"
downloadFileNJ <- paste0(outDir, "njGageObs.json")
dailyValNJ <- paste0(outDir, "njDailyGageObs.json")
downloadFileMD <- paste0(outDir, "mdGageObs.json")
dailyValMD <- paste0(outDir, "mdDailyGageObs.json")

#ptmDownload <- proc.time()
system(paste0("wget -O ", downloadFileNJ, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=nj&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValNJ, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=nj&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileMD, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=md&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValMD, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=md&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
#ptmDownloadEnd <- proc.time() - ptmDownload

##for local testing
#downloadFile <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/testPAweekRecsI.json"
#dailyVal <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/paDailyGageObs.json"

##read in downloaded data
readGageDataNJ <- fromJSON(downloadFileNJ)
dailyValDataNJ <- fromJSON(dailyValNJ)
readGageDataMD <- fromJSON(downloadFileMD)
dailyValDataMD <- fromJSON(dailyValMD)
##format data into dataframe
frameDataNJ <- as.data.frame(readGageDataNJ$value$timeSeries)
frameDataMD <- as.data.frame(readGageDataMD$value$timeSeries)
dailyFrameDataNJ <- as.data.frame(dailyValDataNJ$value$timeSeries)
dailyFrameDataMD <- as.data.frame(dailyValDataMD$value$timeSeries)
##checks the location of all site to make sure they are within the state, as well as within the study area
gageRecsNJ <- frameDataNJ[frameDataNJ$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataNJ$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataNJ$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataNJ$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsMD <- frameDataMD[frameDataMD$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataMD$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataMD$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataMD$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
##daily data
dailyGageRecsNJ <- dailyFrameDataNJ[dailyFrameDataNJ$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataNJ$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataNJ$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataNJ$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsMD <- dailyFrameDataMD[dailyFrameDataMD$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataMD$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataMD$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataMD$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]

##get unique site name
uniSiteNamesNJ <- unique(gageRecsNJ$sourceInfo$siteName)
uniSiteNamesMD <- unique(gageRecsMD$sourceInfo$siteName)
##could use some more cleaning for final use

latestData <- list()
for(loc in 1:length(uniSiteNamesNJ)){
  #print(loc)
  siteName <- uniSiteNamesNJ[loc]
  ##records of the specific site
  siteRecs <- gageRecsNJ[gageRecsNJ$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsNJ[dailyGageRecsNJ$sourceInfo$siteName==siteName,]
  siteID <- siteRecs$sourceInfo$siteCode[[1]]$value
  
  latestDates <- c()
  ##parse out variables - temperature
  ##This variable only needs the latest value - temperature not used in the stream graph
  tempInd <- grep("Temperature", siteRecs$variable$variableDescription)
  if(length(tempInd)>0){
    tempVal <- siteRecs$values[[tempInd]]$value[[1]]$value[nrow(siteRecs$values[[tempInd]]$value[[1]])]
    if(is.null(tempVal)==T){
      tempVal <- NA
    }else{
      latestDates <- c(latestDates, siteRecs$values[[tempInd]]$value[[1]]$dateTime)
    }
  }else{
    tempVal <- NA
  }
  
  ##parse out variables - discharge
  dischargeInd <- grep("Discharge", siteRecs$variable$variableDescription)
  if(length(dischargeInd)>0){
    seriesDischargeVals <- siteRecs$values[[dischargeInd]]$value[[1]]$value
    if(is.null(seriesDischargeVals)==T){
      seriesDischargeVals <- NA
      latestDischargeVal <- NA
      seriesDischargeTimes <- NA
    }else{
      latestDischargeVal <- seriesDischargeVals[length(seriesDischargeVals)]
      seriesDischargeTimes <- anytime(siteRecs$values[[dischargeInd]]$value[[1]]$dateTime)
      latestDates <- c(latestDates, seriesDischargeTimes[seriesDischargeTimes])
    }
  }else{
    seriesDischargeVals <- NA
    latestDischargeVal <- NA
  }
  dischargeSeriesVals <- data.frame(var=as.numeric(seriesDischargeVals), dateTime=seriesDischargeTimes)
  
  ##parse out variables - gage height
  gageInd <- grep("Gage height", siteRecs$variable$variableDescription)
  if(length(gageInd)>0){
    seriesGageVals <- siteRecs$values[[gageInd]]$value[[1]]$value
    if(is.null(seriesGageVals)==T){
      seriesGageVals <- NA
      latestGageVal <- NA
      seriesGageTimes <- NA
    }else{
      latestGageVal <- seriesGageVals[length(seriesGageVals)]
      seriesGageTimes <- anytime(siteRecs$values[[gageInd]]$value[[1]]$dateTime)
      latestDates <- c(latestDates, seriesGageTimes[length(seriesGageTimes)])
    }
  }else{
    seriesGageVals <- NA
    latestGageVal <- NA
  }
  gageSeriesVals <- data.frame(var=as.numeric(seriesGageVals), dateTime=seriesGageTimes)
  
  ##extract the date and times for each record
  if(length(latestDates)>0){
    setDates <- anytime(latestDates)
    latestDate <- max(setDates, na.rm=T)
    latestTime <- format(latestDate, "%I:%M %p %Z")
    latestDate <- format(latestDate, "%b %d, %Y")
  }else{
    latestDate <- NA
    latestTime <- NA
  }
  
  ##create the site data plot
  if(nrow(siteDailyRecs)>0){
    dischargeMean <- mean(as.numeric(siteDailyRecs$values[[1]]$value[[1]]$value), na.rm=T)
  }else{
    dischargeMean <- NA
  }
  stream_gage_plot(ID=siteID, discharge_dat=dischargeSeriesVals, daily_avgQ=dischargeMean, gaugeheight_dat=gageSeriesVals, 
                   day_midnight=day_midnight, day_noon=day_noon, p.width=4, p.height=2.5, plotDir=plottingDir)
  
  
  ##set up the record for the site
  latestData[[loc]] <- data.frame(SiteNumber=siteID, SiteName=siteName, 
                                  SiteLongitude=siteRecs$sourceInfo$geoLocation$geogLocation$longitude[1], SiteLatitude=siteRecs$sourceInfo$geoLocation$geogLocation$latitude[1], 
                                  temp=tempVal, discharge=latestDischargeVal, gageHeight=latestGageVal, state="newjersey", 
                                  latestDate=latestDate, latestTime=latestTime)
}
njRecs <- do.call(rbind.data.frame, latestData)
njRecs$obsString <- createObsString(njRecs)
njRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", njRecs$SiteNumber)
njRecs$SiteName <- gsub(" NJ", ", NJ", njRecs$SiteName)


latestData <- list()
for(loc in 1:length(uniSiteNamesMD)){
  #print(loc)
  siteName <- uniSiteNamesMD[loc]
  ##records of the specific site
  siteRecs <- gageRecsMD[gageRecsMD$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsMD[dailyGageRecsMD$sourceInfo$siteName==siteName,]
  siteID <- siteRecs$sourceInfo$siteCode[[1]]$value
  
  latestDates <- c()
  ##parse out variables - temperature
  ##This variable only needs the latest value - temperature not used in the stream graph
  tempInd <- grep("Temperature", siteRecs$variable$variableDescription)
  if(length(tempInd)>0){
    tempVal <- siteRecs$values[[tempInd]]$value[[1]]$value[nrow(siteRecs$values[[tempInd]]$value[[1]])]
    if(is.null(tempVal)==T){
      tempVal <- NA
    }else{
      latestDates <- c(latestDates, siteRecs$values[[tempInd]]$value[[1]]$dateTime)
    }
  }else{
    tempVal <- NA
  }
  
  ##parse out variables - discharge
  dischargeInd <- grep("Discharge", siteRecs$variable$variableDescription)
  if(length(dischargeInd)>0){
    seriesDischargeVals <- siteRecs$values[[dischargeInd]]$value[[1]]$value
    if(is.null(seriesDischargeVals)==T){
      seriesDischargeVals <- NA
      latestDischargeVal <- NA
      seriesDischargeTimes <- NA
    }else{
      latestDischargeVal <- seriesDischargeVals[length(seriesDischargeVals)]
      seriesDischargeTimes <- anytime(siteRecs$values[[dischargeInd]]$value[[1]]$dateTime)
      latestDates <- c(latestDates, seriesDischargeTimes[seriesDischargeTimes])
    }
  }else{
    seriesDischargeVals <- NA
    latestDischargeVal <- NA
  }
  dischargeSeriesVals <- data.frame(var=as.numeric(seriesDischargeVals), dateTime=seriesDischargeTimes)
  
  ##parse out variables - gage height
  gageInd <- grep("Gage height", siteRecs$variable$variableDescription)
  if(length(gageInd)>0){
    seriesGageVals <- siteRecs$values[[gageInd]]$value[[1]]$value
    if(is.null(seriesGageVals)==T){
      seriesGageVals <- NA
      latestGageVal <- NA
      seriesGageTimes <- NA
    }else{
      latestGageVal <- seriesGageVals[length(seriesGageVals)]
      seriesGageTimes <- anytime(siteRecs$values[[gageInd]]$value[[1]]$dateTime)
      latestDates <- c(latestDates, seriesGageTimes[length(seriesGageTimes)])
    }
  }else{
    seriesGageVals <- NA
    latestGageVal <- NA
  }
  gageSeriesVals <- data.frame(var=as.numeric(seriesGageVals), dateTime=seriesGageTimes)
  
  ##extract the date and times for each record
  if(length(latestDates)>0){
    setDates <- anytime(latestDates)
    latestDate <- max(setDates, na.rm=T)
    latestTime <- format(latestDate, "%I:%M %p %Z")
    latestDate <- format(latestDate, "%b %d, %Y")
  }else{
    latestDate <- NA
    latestTime <- NA
  }
  
  ##create the site data plot
  if(nrow(siteDailyRecs)>0){
    dischargeMean <- mean(as.numeric(siteDailyRecs$values[[1]]$value[[1]]$value), na.rm=T)
  }else{
    dischargeMean <- NA
  }
  stream_gage_plot(ID=siteID, discharge_dat=dischargeSeriesVals, daily_avgQ=dischargeMean, gaugeheight_dat=gageSeriesVals, 
                   day_midnight=day_midnight, day_noon=day_noon, p.width=4, p.height=2.5, plotDir=plottingDir)
  
  
  ##set up the record for the site
  latestData[[loc]] <- data.frame(SiteNumber=siteID, SiteName=siteName, 
                                  SiteLongitude=siteRecs$sourceInfo$geoLocation$geogLocation$longitude[1], SiteLatitude=siteRecs$sourceInfo$geoLocation$geogLocation$latitude[1], 
                                  temp=tempVal, discharge=latestDischargeVal, gageHeight=latestGageVal, state="pennsylvania", 
                                  latestDate=latestDate, latestTime=latestTime)
}
mdRecs <- do.call(rbind.data.frame, latestData)
mdRecs$obsString <- createObsString(mdRecs)
mdRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", mdRecs$SiteNumber)
mdRecs$SiteName <- str_to_title(mdRecs$SiteName)
mdRecs$SiteName <- gsub(", Md", ", MD", mdRecs$SiteName)
mdRecs$SiteName <- gsub(" At ", " at ", mdRecs$SiteName)

##NJ and MD
njmdRecs <- rbind(njRecs, mdRecs)

##convert the temperature values from C to F
njmdRecs$temp <- as.character(as.numeric(njmdRecs$temp) * (9/5) + 32)

##create the feature record for each station for the geojson file
stream_string <- paste0('{"type": "Feature", "properties": {"name": "', njmdRecs$SiteName, '", "id": "', njmdRecs$SiteNumber, '", "url": "', njmdRecs$SiteNWISURL, 
                        '", "obs": "', njmdRecs$obsString, '", "time": "', paste0("Last Updated on ", njmdRecs$latestDate, " at ", njmdRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', njmdRecs$SiteNumber,
                        '.png"}, "geometry": {"type": "Point", "coordinates": [', njmdRecs$SiteLongitude, ',', njmdRecs$SiteLatitude, ']}}')
##create the final string for the output file
json_merge <- paste0('NJMD_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')


# Export data to geojson.
cat(json_merge, file=paste0(outDir, "NJMD_stream_obs.js"))

#ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
#timeFile <- paste0(outDir, "NJMDstreamGagesTracking.RData")
#if(file.exists(timeFile)==T){
#  load(timeFile)
#  timeNJMDStreams[nrow(timeNJMDStreams)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
#  save("timeNJMDStreams", file=timeFile)
#}else{
#  timeNJMDStreams <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
#  save("timeNJMDStreams", file=timeFile)
#}


