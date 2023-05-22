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

downloadFileOH <- paste0(outDir, "ohGageObs.json")
dailyValOH <- paste0(outDir, "ohDailyGageObs.json")
downloadFileDE <- paste0(outDir, "deGageObs.json")
dailyValDE <- paste0(outDir, "deDailyGageObs.json")
downloadFileDC <- paste0(outDir, "dcGageObs.json")
dailyValDC <- paste0(outDir, "dcDailyGageObs.json")
downloadFileWV <- paste0(outDir, "wvGageObs.json")
dailyValWV <- paste0(outDir, "wvDailyGageObs.json")
#downloadFileTN <- paste0(outDir, "tnGageObs.json")
#dailyValTN <- paste0(outDir, "tnDailyGageObs.json")
downloadFileNC <- paste0(outDir, "ncGageObs.json")
dailyValNC <- paste0(outDir, "ncDailyGageObs.json")
downloadFileCT <- paste0(outDir, "ctGageObs.json")
dailyValCT <- paste0(outDir, "ctDailyGageObs.json")
downloadFileMA <- paste0(outDir, "maGageObs.json")
dailyValMA <- paste0(outDir, "maDailyGageObs.json")


#ptmDownload <- proc.time()
system(paste0("wget -O ", downloadFileOH, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=oh&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValOH, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=oh&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileDE, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=de&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValDE, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=de&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileDC, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=dc&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValDC, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=dc&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileWV, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=wv&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValWV, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=wv&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
#system(paste0("wget -O ", downloadFileTN, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=tn&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
#system(paste0("wget -O ", dailyValTN, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=tn&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileNC, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=nc&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValNC, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=nc&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileCT, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=ct&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValCT, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=ct&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
system(paste0("wget -O ", downloadFileMA, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=ma&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyValMA, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=ma&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
#ptmDownloadEnd <- proc.time() - ptmDownload

##read in downloaded data
readGageDataOH <- fromJSON(downloadFileOH)
dailyValDataOH <- fromJSON(dailyValOH)
readGageDataDE <- fromJSON(downloadFileDE)
dailyValDataDE <- fromJSON(dailyValDE)
readGageDataDC <- fromJSON(downloadFileDC)
dailyValDataDC <- fromJSON(dailyValDC)
readGageDataWV <- fromJSON(downloadFileWV)
dailyValDataWV <- fromJSON(dailyValWV)
#readGageDataTN <- fromJSON(downloadFileTN)
#dailyValDataTN <- fromJSON(dailyValTN)
readGageDataNC <- fromJSON(downloadFileNC)
dailyValDataNC <- fromJSON(dailyValNC)
readGageDataCT <- fromJSON(downloadFileCT)
dailyValDataCT <- fromJSON(dailyValCT)
readGageDataMA <- fromJSON(downloadFileMA)
dailyValDataMA <- fromJSON(dailyValMA)


##format data into dataframe
frameDataOH <- as.data.frame(readGageDataOH$value$timeSeries)
frameDataDE <- as.data.frame(readGageDataDE$value$timeSeries)
frameDataDC <- as.data.frame(readGageDataDC$value$timeSeries)
frameDataWV <- as.data.frame(readGageDataWV$value$timeSeries)
#frameDataTN <- as.data.frame(readGageDataTN$value$timeSeries)
frameDataNC <- as.data.frame(readGageDataNC$value$timeSeries)
frameDataCT <- as.data.frame(readGageDataCT$value$timeSeries)
frameDataMA <- as.data.frame(readGageDataMA$value$timeSeries)
dailyFrameDataOH <- as.data.frame(dailyValDataOH$value$timeSeries)
dailyFrameDataDE <- as.data.frame(dailyValDataDE$value$timeSeries)
dailyFrameDataDC <- as.data.frame(dailyValDataDC$value$timeSeries)
dailyFrameDataWV <- as.data.frame(dailyValDataWV$value$timeSeries)
#dailyFrameDataTN <- as.data.frame(dailyValDataTN$value$timeSeries)
dailyFrameDataNC <- as.data.frame(dailyValDataNC$value$timeSeries)
dailyFrameDataCT <- as.data.frame(dailyValDataCT$value$timeSeries)
dailyFrameDataMA <- as.data.frame(dailyValDataMA$value$timeSeries)
##checks the location of all site to make sure they are within the state, as well as within the study area
gageRecsOH <- frameDataOH[frameDataOH$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataOH$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataOH$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataOH$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsDE <- frameDataDE[frameDataDE$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataDE$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataDE$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataDE$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsDC <- frameDataDC[frameDataDC$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataDC$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataDC$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataDC$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsWV <- frameDataWV[frameDataWV$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataWV$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataWV$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataWV$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
#gageRecsTN <- frameDataTN[frameDataTN$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataTN$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataTN$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataTN$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsNC <- frameDataNC[frameDataNC$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataNC$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataNC$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataNC$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsCT <- frameDataCT[frameDataCT$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataCT$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataCT$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataCT$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
gageRecsMA <- frameDataMA[frameDataMA$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameDataMA$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameDataMA$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameDataMA$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]

##daily data
dailyGageRecsOH <- dailyFrameDataOH[dailyFrameDataOH$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataOH$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataOH$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataOH$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsDE <- dailyFrameDataDE[dailyFrameDataDE$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataDE$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataDE$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataDE$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsDC <- dailyFrameDataDC[dailyFrameDataDC$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataDC$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataDC$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataDC$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsWV <- dailyFrameDataWV[dailyFrameDataWV$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataWV$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataWV$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataWV$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
#dailyGageRecsTN <- dailyFrameDataTN[dailyFrameDataTN$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataTN$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataTN$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataTN$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsNC <- dailyFrameDataNC[dailyFrameDataNC$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataNC$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataNC$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataNC$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsCT <- dailyFrameDataCT[dailyFrameDataCT$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataCT$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataCT$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataCT$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
dailyGageRecsMA <- dailyFrameDataMA[dailyFrameDataMA$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameDataMA$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameDataMA$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameDataMA$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]

##get unique site name
uniSiteNamesOH <- unique(gageRecsOH$sourceInfo$siteName)
uniSiteNamesDE <- unique(gageRecsDE$sourceInfo$siteName)
uniSiteNamesDC <- unique(gageRecsDC$sourceInfo$siteName)
uniSiteNamesWV <- unique(gageRecsWV$sourceInfo$siteName)
#uniSiteNamesTN <- unique(gageRecsTN$sourceInfo$siteName)
uniSiteNamesNC <- unique(gageRecsNC$sourceInfo$siteName)
uniSiteNamesCT <- unique(gageRecsCT$sourceInfo$siteName)
uniSiteNamesMA <- unique(gageRecsMA$sourceInfo$siteName)


latestData <- list()
for(loc in 1:length(uniSiteNamesOH)){
  #print(loc)
  siteName <- uniSiteNamesOH[loc]
  ##records of the specific site
  siteRecs <- gageRecsOH[gageRecsOH$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsOH[dailyGageRecsOH$sourceInfo$siteName==siteName,]
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
ohRecs <- do.call(rbind.data.frame, latestData)
ohRecs$obsString <- createObsString(ohRecs)
ohRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", ohRecs$SiteNumber)
ohRecs$SiteName <- gsub(" OH", ", OH", ohRecs$SiteName)

latestData <- list()
for(loc in 1:length(uniSiteNamesDE)){
  #print(loc)
  siteName <- uniSiteNamesDE[loc]
  ##records of the specific site
  siteRecs <- gageRecsDE[gageRecsDE$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsDE[dailyGageRecsDE$sourceInfo$siteName==siteName,]
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
deRecs <- do.call(rbind.data.frame, latestData)
deRecs$obsString <- createObsString(deRecs)
deRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", deRecs$SiteNumber)
deRecs$SiteName <- str_to_title(deRecs$SiteName)
deRecs$SiteName <- gsub(" At ", " at ", deRecs$SiteName)
deRecs$SiteName <- gsub(", De", ", DE", deRecs$SiteName)

latestData <- list()
for(loc in 1:length(uniSiteNamesDC)){
  #print(loc)
  siteName <- uniSiteNamesDC[loc]
  ##records of the specific site
  siteRecs <- gageRecsDC[gageRecsDC$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsDC[dailyGageRecsDC$sourceInfo$siteName==siteName,]
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
dcRecs <- do.call(rbind.data.frame, latestData)
dcRecs$obsString <- createObsString(dcRecs)
dcRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", dcRecs$SiteNumber)
dcRecs$SiteName <- str_to_title(dcRecs$SiteName)
dcRecs$SiteName <- gsub(" At ", " at ", dcRecs$SiteName)
dcRecs$SiteName <- gsub(", Dc", ", DC", dcRecs$SiteName)

latestData <- list()
for(loc in 1:length(uniSiteNamesWV)){
  #print(loc)
  siteName <- uniSiteNamesWV[loc]
  ##records of the specific site
  siteRecs <- gageRecsWV[gageRecsWV$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsWV[dailyGageRecsWV$sourceInfo$siteName==siteName,]
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
wvRecs <- do.call(rbind.data.frame, latestData)
wvRecs$obsString <- createObsString(wvRecs)
wvRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", wvRecs$SiteNumber)
wvRecs$SiteName <- str_to_title(wvRecs$SiteName)
wvRecs$SiteName <- gsub(" At ", " at ", wvRecs$SiteName)
wvRecs$SiteName <- gsub(", Wv", ", WV", wvRecs$SiteName)

# latestData <- list()
# for(loc in 1:length(uniSiteNamesTN)){
#   #print(loc)
#   siteName <- uniSiteNamesTN[loc]
#   ##records of the specific site
#   siteRecs <- gageRecsTN[gageRecsTN$sourceInfo$siteName==siteName,]
#   siteDailyRecs <- dailyGageRecsTN[dailyGageRecsTN$sourceInfo$siteName==siteName,]
#   siteID <- siteRecs$sourceInfo$siteCode[[1]]$value
#   
#   latestDates <- c()
#   ##parse out variables - temperature
#   ##This variable only needs the latest value - temperature not used in the stream graph
#   tempInd <- grep("Temperature", siteRecs$variable$variableDescription)
#   if(length(tempInd)>0){
#     tempVal <- siteRecs$values[[tempInd]]$value[[1]]$value[nrow(siteRecs$values[[tempInd]]$value[[1]])]
#     if(is.null(tempVal)==T){
#       tempVal <- NA
#     }else{
#       latestDates <- c(latestDates, siteRecs$values[[tempInd]]$value[[1]]$dateTime)
#     }
#   }else{
#     tempVal <- NA
#   }
#   
#   ##parse out variables - discharge
#   dischargeInd <- grep("Discharge", siteRecs$variable$variableDescription)
#   if(length(dischargeInd)>0){
#     seriesDischargeVals <- siteRecs$values[[dischargeInd]]$value[[1]]$value
#     if(is.null(seriesDischargeVals)==T){
#       seriesDischargeVals <- NA
#       latestDischargeVal <- NA
#       seriesDischargeTimes <- NA
#     }else{
#       latestDischargeVal <- seriesDischargeVals[length(seriesDischargeVals)]
#       seriesDischargeTimes <- anytime(siteRecs$values[[dischargeInd]]$value[[1]]$dateTime)
#       latestDates <- c(latestDates, seriesDischargeTimes[seriesDischargeTimes])
#     }
#   }else{
#     seriesDischargeVals <- NA
#     latestDischargeVal <- NA
#   }
#   dischargeSeriesVals <- data.frame(var=as.numeric(seriesDischargeVals), dateTime=seriesDischargeTimes)
#   
#   ##parse out variables - gage height
#   gageInd <- grep("Gage height", siteRecs$variable$variableDescription)
#   if(length(gageInd)>0){
#     seriesGageVals <- siteRecs$values[[gageInd]]$value[[1]]$value
#     if(is.null(seriesGageVals)==T){
#       seriesGageVals <- NA
#       latestGageVal <- NA
#       seriesGageTimes <- NA
#     }else{
#       latestGageVal <- seriesGageVals[length(seriesGageVals)]
#       seriesGageTimes <- anytime(siteRecs$values[[gageInd]]$value[[1]]$dateTime)
#       latestDates <- c(latestDates, seriesGageTimes[length(seriesGageTimes)])
#     }
#   }else{
#     seriesGageVals <- NA
#     latestGageVal <- NA
#   }
#   gageSeriesVals <- data.frame(var=as.numeric(seriesGageVals), dateTime=seriesGageTimes)
#   
#   ##extract the date and times for each record
#   if(length(latestDates)>0){
#     setDates <- anytime(latestDates)
#     latestDate <- max(setDates, na.rm=T)
#     latestTime <- format(latestDate, "%I:%M %p %Z")
#     latestDate <- format(latestDate, "%b %d, %Y")
#   }else{
#     latestDate <- NA
#     latestTime <- NA
#   }
#   
#   ##create the site data plot
#   if(nrow(siteDailyRecs)>0){
#     dischargeMean <- mean(as.numeric(siteDailyRecs$values[[1]]$value[[1]]$value), na.rm=T)
#   }else{
#     dischargeMean <- NA
#   }
#   stream_gage_plot(ID=siteID, discharge_dat=dischargeSeriesVals, daily_avgQ=dischargeMean, gaugeheight_dat=gageSeriesVals, 
#                    day_midnight=day_midnight, day_noon=day_noon, p.width=4, p.height=2.5, plotDir=plottingDir)
#   
#   
#   ##set up the record for the site
#   latestData[[loc]] <- data.frame(SiteNumber=siteID, SiteName=siteName, 
#                                   SiteLongitude=siteRecs$sourceInfo$geoLocation$geogLocation$longitude[1], SiteLatitude=siteRecs$sourceInfo$geoLocation$geogLocation$latitude[1], 
#                                   temp=tempVal, discharge=latestDischargeVal, gageHeight=latestGageVal, state="newjersey", 
#                                   latestDate=latestDate, latestTime=latestTime)
# }
# tnRecs <- do.call(rbind.data.frame, latestData)
# tnRecs$obsString <- createObsString(tnRecs)
# tnRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", tnRecs$SiteNumber)
# tnRecs$SiteName <- gsub(" TN", ", TN", tnRecs$SiteName)

latestData <- list()
for(loc in 1:length(uniSiteNamesNC)){
  #print(loc)
  siteName <- uniSiteNamesNC[loc]
  ##records of the specific site
  siteRecs <- gageRecsNC[gageRecsNC$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsNC[dailyGageRecsNC$sourceInfo$siteName==siteName,]
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
ncRecs <- do.call(rbind.data.frame, latestData)
ncRecs$obsString <- createObsString(ncRecs)
ncRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", ncRecs$SiteNumber)
ncRecs$SiteName <- str_to_title(ncRecs$SiteName)
ncRecs$SiteName <- gsub(" At ", " at ", ncRecs$SiteName)
ncRecs$SiteName <- gsub(", Nc", ", NC", ncRecs$SiteName)

latestData <- list()
for(loc in 1:length(uniSiteNamesCT)){
  #print(loc)
  siteName <- uniSiteNamesCT[loc]
  ##records of the specific site
  siteRecs <- gageRecsCT[gageRecsCT$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsCT[dailyGageRecsCT$sourceInfo$siteName==siteName,]
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
ctRecs <- do.call(rbind.data.frame, latestData)
ctRecs$obsString <- createObsString(ctRecs)
ctRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", ctRecs$SiteNumber)
ctRecs$SiteName <- str_to_title(ctRecs$SiteName)
ctRecs$SiteName <- gsub(" At ", " at ", ctRecs$SiteName)
ctRecs$SiteName <- gsub(", Ct", ", CT", ctRecs$SiteName)

latestData <- list()
for(loc in 1:length(uniSiteNamesMA)){
  #print(loc)
  siteName <- uniSiteNamesMA[loc]
  ##records of the specific site
  siteRecs <- gageRecsMA[gageRecsMA$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecsMA[dailyGageRecsMA$sourceInfo$siteName==siteName,]
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
maRecs <- do.call(rbind.data.frame, latestData)
maRecs$obsString <- createObsString(maRecs)
maRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", maRecs$SiteNumber)
maRecs$SiteName <- str_to_title(maRecs$SiteName)
maRecs$SiteName <- gsub(" At ", " at ", maRecs$SiteName)
maRecs$SiteName <- gsub(", Ma", ", MA", maRecs$SiteName)


rangeRecs <- rbind(ohRecs, deRecs, dcRecs, wvRecs, ncRecs, ctRecs, maRecs)  #, tnRecs

##convert the temperature values from C to F
rangeRecs$temp <- as.character(as.numeric(rangeRecs$temp) * (9/5) + 32)

##create the feature record for each station for the geojson file
stream_string <- paste0('{"type": "Feature", "properties": {"name": "', rangeRecs$SiteName, '", "id": "', rangeRecs$SiteNumber, '", "url": "', rangeRecs$SiteNWISURL, 
                        '", "obs": "', rangeRecs$obsString, '", "time": "', paste0("Last Updated on ", rangeRecs$latestDate, " at ", rangeRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', rangeRecs$SiteNumber,
                        '.png"}, "geometry": {"type": "Point", "coordinates": [', rangeRecs$SiteLongitude, ',', rangeRecs$SiteLatitude, ']}}')
##create the final string for the output file
json_merge <- paste0('OHDEDCWVTNNCCTMA_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')


# Export data to geojson.
cat(json_merge, file=paste0(outDir, "OHDEDCWVTNNCCTMA_stream_obs.js"))

#ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
#timeFile <- paste0(outDir, "OHDEDCWVTNNCCTMAstreamGagesTracking.RData")
#if(file.exists(timeFile)==T){
#  load(timeFile)
#  timeOHDEDCWVTNNCCTMAStreams[nrow(timeOHDEDCWVTNNCCTMAStreams)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
#  save("timeOHDEDCWVTNNCCTMAStreams", file=timeFile)
#}else{
#  timeOHDEDCWVTNNCCTMAStreams <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
#  save("timeOHDEDCWVTNNCCTMAStreams", file=timeFile)
#}



