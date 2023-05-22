# --------------------------------------------------------------------------------------------------------------------
# Copyright 2018 The Pennsylvania State University
#
# Matthew D. Lisk (mdl5548@psu.edu)
# Last edit: September 1, 2022
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
downloadFile <- paste0(outDir, "nyGageObs.json")
dailyVal <- paste0(outDir, "nyDailyGageObs.json")

#ptmDownload <- proc.time()
system(paste0("wget -O ", downloadFile, " 'https://waterservices.usgs.gov/nwis/iv?format=json&stateCd=ny&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00010,00060,00065'"))
system(paste0("wget -O ", dailyVal, " 'https://waterservices.usgs.gov/nwis/dv?format=json&stateCd=ny&startDT=", bDate, "&endDT=", eDate, "&parameterCd=00060'"))
#ptmDownloadEnd <- proc.time() - ptmDownload

##for local testing
#downloadFile <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/testPAweekRecsI.json"
#dailyVal <- "/Users/mdl5548/Documents/tempDataChk/timesFromFirkin/paDailyGageObs.json"

##read in downloaded data
readGageData <- fromJSON(downloadFile)
dailyValData <- fromJSON(dailyVal)
##format data into dataframe
frameData <- as.data.frame(readGageData$value$timeSeries)
dailyFrameData <- as.data.frame(dailyValData$value$timeSeries)
##checks the location of all site to make sure they are within the state, as well as within the study area
gageRecs <- frameData[frameData$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & frameData$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & frameData$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & frameData$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]
##daily data
dailyGageRecs <- dailyFrameData[dailyFrameData$sourceInfo$geoLocation$geogLocation$longitude>=-82.0 & dailyFrameData$sourceInfo$geoLocation$geogLocation$longitude<=-73.0 & dailyFrameData$sourceInfo$geoLocation$geogLocation$latitude>=36.0 & dailyFrameData$sourceInfo$geoLocation$geogLocation$latitude<=43.5,]


##get unique site name
uniSiteNames <- unique(gageRecs$sourceInfo$siteName)
##could use some more cleaning for final use

latestData <- list()
for(loc in 1:length(uniSiteNames)){
  #print(loc)
  siteName <- uniSiteNames[loc]
  ##records of the specific site
  siteRecs <- gageRecs[gageRecs$sourceInfo$siteName==siteName,]
  siteDailyRecs <- dailyGageRecs[dailyGageRecs$sourceInfo$siteName==siteName,]
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
                                  temp=tempVal, discharge=latestDischargeVal, gageHeight=latestGageVal, state="newyork", 
                                  latestDate=latestDate, latestTime=latestTime)
}
nyRecs <- do.call(rbind.data.frame, latestData)
nyRecs$obsString <- createObsString(nyRecs)
nyRecs$SiteNWISURL <- paste0("https://waterdata.usgs.gov/nwis/inventory?agency_code=USGS&site_no=", nyRecs$SiteNumber)
nyRecs$SiteName <- str_to_title(nyRecs$SiteName)
nyRecs$SiteName <- gsub(" At ", " at ", nyRecs$SiteName)
nyRecs$SiteName <- gsub(" Ny", ", NY", nyRecs$SiteName)
nyRecs$SiteName <- gsub(" Ct", " CT", nyRecs$SiteName)

##convert the temperature values from C to F
nyRecs$temp <- as.character(as.numeric(nyRecs$temp) * (9/5) + 32)

##NY
##create the feature record for each station for the geojson file
stream_string <- paste0('{"type": "Feature", "properties": {"name": "', nyRecs$SiteName, '", "id": "', nyRecs$SiteNumber, '", "url": "', nyRecs$SiteNWISURL, 
                        '", "obs": "', nyRecs$obsString, '", "time": "', paste0("Last Updated on ", nyRecs$latestDate, " at ", nyRecs$latestTime), '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', nyRecs$SiteNumber,
                        '.png"}, "geometry": {"type": "Point", "coordinates": [', nyRecs$SiteLongitude, ',', nyRecs$SiteLatitude, ']}}')
##create the final string for the output file
json_merge <- paste0('NY_streamGauges = {"type": "FeatureCollection","features": [', paste(stream_string, collapse=', '), ']};')
  
##save object with ids for plotting
NY_ID <- nyRecs$SiteNumber
save("NY_ID", file=paste0(outDir, "NY_streamIDs.RData"))

# Export data to geojson.
cat(json_merge, file=paste0(outDir, "NY_stream_obs.js"))


#ptmEnd <- proc.time() - ptm
#stop(paste0("Total Runtime: ", ptmEnd))

##check if a time stop file already exists. If it does not, create one
#timeFile <- paste0(outDir, "NYstreamGagesTracking.RData")
#if(file.exists(timeFile)==T){
#  load(timeFile)
#  timeNYStreams[nrow(timeNYStreams)+1,] <- c(date(), ptmDownloadEnd[3], ptmEnd[3])
#  save("timeNYStreams", file=timeFile)
#}else{
#  timeNYStreams <- data.frame(dateTime=date(), DT=ptmDownloadEnd[3], TT=ptmEnd[3])
#  save("timeNYStreams", file=timeFile)
#}



