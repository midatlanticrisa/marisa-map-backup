##header

##########################################################################
##########################################################################
##a function to retry running a function if there is an error
##modified from code found at: https://stackoverflow.com/questions/37379472/creating-a-retry-function-in-r
retry <- function(a, max=6, init=0, delay=0){
  suppressWarnings(tryCatch({
  if(init<max){
    a
  }}, error=function(e){
    Sys.sleep(delay)
    retry(a, max, init=init+1)
    }))
}
##########################################################################
##########################################################################
# Function extracting weather data from an XML file.
parseWS_xml = function(id){
  #################
  #id <- weather_stations$id[1]
  #################

  xml.url <- paste0('http://w1.weather.gov/xml/current_obs/', id, '.xml')
  
  # Turn XML data into a list.
  xml_data <- retry(xmlToList(rawToChar(GET(xml.url, user_agent("httr (mdl5548@psu.edu)"))$content)))
  
  # Station location.
  if(is.null(xml_data$location)){
    name <- ""
  } else {
    name <- xml_data$location
  }
  # Latitude.
  if(is.null(xml_data$latitude)){
    latitude <- NA
  } else {
    latitude <- xml_data$latitude
  }
  # Longitude.
  if(is.null(xml_data$longitude)){
    longitude <- NA
  } else {
    longitude <- xml_data$longitude
  }
  # Time when observations were collected.
  if(is.null(xml_data$observation_time)){
    date <- ""
    time <- ""
  } else {
    exTime <- sapply(strsplit(xml_data$observation_time, "on "), "[[", 2)
    exZ <- sapply(strsplit(exTime," "), "[[", length(strsplit(exTime[1]," ")[[1]]))
    if(exZ=="HST"|exZ=="HDT"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="Pacific/Honolulu")
    }else if(exZ=="EST"|exZ=="EDT"|exZ=="ZST"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/New_York")
    }else if(exZ=="CST"|exZ=="CDT"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Chicago")
    }else if(exZ=="MST"|exZ=="MDT"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Denver")
    }else if(exZ=="PST"|exZ=="PDT"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Los_Angeles")
    }else if(exZ=="AKST"|exZ=="AKDT"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Anchorage")
    }else if(exZ=="SST"|exZ=="SDT"){
      dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="Pacific/Samoa")
    }
    date <- format(dateBase, format="%b %d, %Y") # convert from GMT to current time zone
    time <- format(dateBase, format="%I:%M %p %Z") # convert from GMT to current time zone
  }
  # General weather condition.
  if(is.null(xml_data$weather)){
    weather <- ""
  } else {
    weather <- paste0("<strong>Weather: </strong>", xml_data$weather, "<br/>")
  }
  # Air temperature in F.
  if(is.null(xml_data$temp_f)){
    temp <- ""
  } else {
    temp <- paste0("<strong>Temperature: </strong>", xml_data$temp_f, " &#8457;<br/>")
  }
  # Relative humidity in %.
  if(is.null(xml_data$relative_humidity)){
    humidity <- ""
  } else {
    humidity <- paste0("<strong>Relative humidity: </strong>", xml_data$relative_humidity, " %<br/>")
  }
  # Which direction the wind is blowing.
  if(is.null(xml_data$wind_string)){
    wind <- ""
  } else {
    wind <- paste0("<strong>Wind: </strong>", xml_data$wind_string, " <br/>")
  }
  # Pressure in mb.
  if(is.null(xml_data$pressure_mb)){
    speed <- ""
  } else {
    speed <- paste0("<strong>Pressure: </strong>", xml_data$pressure_mb, " mb<br/>")
  }
  # Dew point in F.
  if(is.null(xml_data$dewpoint_f)){
    dewpoint <- ""
  } else {
    dewpoint <- paste0("<strong>Dewpoint: </strong>", xml_data$dewpoint_f, " &#8457;<br/>")
  }
  # Windchill in F.
  if(is.null(xml_data$windchill_f)){
    windchill <- ""
  } else {
    windchill <- paste0("<strong>Wind chill: </strong>", xml_data$windchill_f, " &#8457;<br/>")
  }
  # Current visibility in miles.
  if(is.null(xml_data$visibility_mi)){
    visibility <- ""
  } else {
    visibility <- paste0("<strong>Visibility: </strong>", xml_data$visibility_mi, " miles<br/>")
  }
  # Observation url.
  if(is.null(xml_data$ob_url)){
    link <- ""
  } else {
    link <- xml_data$ob_url
  }
  
  obs = paste0(weather, temp, humidity, wind, speed, dewpoint, windchill, visibility)
  
  # Return the weather variables
  return(c(name, as.character(id), latitude, longitude, obs, link, date, time))
}
##########################################################################
##########################################################################
collectBuoyData = function(buoys_ids, US_buoys){
  #################
  #buoys_ids <- NDBC_buoys$ID
  #buoys_ids <- NDBC_stations$ID
  #buoys_ids <- non_NDBC_stations$ID
  #US_buoys <- US_buoys
  #################
  print(buoys_ids)
  ##set up 'bones' for data to be returned, mostly for preserving input/output order
  outTab <- data.frame(id=buoys_ids)
  
  # Use the ID to create a URL to the RSS file.
  buoyURLs <- paste0('https://www.ndbc.noaa.gov/data/latest_obs/', buoys_ids, '.rss')
  
  # Check if url/ observations exist
  buoyExistance <- as.character(sapply(buoyURLs, url.exists))
  
  ##########################################
  ##for those urls that do exist
  if(T%in%buoyExistance){
    scripts <- sapply(buoyURLs[which(buoyExistance==T)], function(x){retry(getURL(x))})
    month <- format(Sys.Date(),"%B")
    
    # Use the ID to create a URL to the RSS file. 
    scripts <- sapply(scripts, function(txt){subTxt<-gsub(pattern="&", "&amp;", x=txt);
                                              subTxt<-gsub(pattern="&amp;#", "&#", x=subTxt);
                                              subTxt<-gsub(pattern='</rss"', '</rss>"', x=subTxt);
                                              subTxt<-gsub("\"","\'", subTxt);
                                              return(subTxt)})

    docs <- lapply(scripts, xmlParse)
  
    # Extract the meteorological data, which is in the 'description' node.
    buoy_vects <- sapply(docs, xpathSApply, path='//channel/item/description', fun=xmlValue)
    # Remove the '\n' character from the extracted string.
    removeEnd <- sapply(buoy_vects, str_replace_all, pattern="([\n])", replacement="")
    
    exTime <- sapply(strsplit(buoy_vects,"<strong>|</strong>"), "[[", 2)
    buoySubObs <- sapply(strsplit(removeEnd, paste0(exTime, "</strong><br />")), "[[", 2)
    
    splitLength <- sapply(exTime, function(x){length(strsplit(x, " ")[[1]])})
    exZ <- sapply(1:length(exTime), function(x){sapply(strsplit(exTime[x]," "), "[[", splitLength[x])})
    dateBases <- mapply(function(tim, zon){if(zon=="EST"|zon=="EDT"){  ##currently only have to worry about records in one time zone
                                            reForm<-as.POSIXlt(tim, format="%b %d, %Y %I:%M %p", tz="America/New_York")
                                          }
                                          return(reForm)}, tim=exTime, zon=exZ, SIMPLIFY=F)
    date <- sapply(dateBases, format, format="%b %d, %Y")  #format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from GMT to current time zone
    time <- sapply(dateBases, format, format="%I:%M %p %Z")  #format(dateBase, format="%I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
    
    # Extract the name, link, and coordinates which are in the 'title', 'link', and 'georss:point' node.
    buoy_names <- sapply(docs, xpathSApply, path='//channel/item/title', fun=xmlValue)
    buoy_links <- sapply(docs, xpathSApply, path='//channel/item/link', fun=xmlValue)
    buoy_coords <- sapply(docs, xpathSApply, path='//channel/item/georss:point', fun=xmlValue)
    buoy_lats <- as.numeric(sapply(strsplit(buoy_coords," "),"[[", 1))
    buoy_lons <- as.numeric(sapply(strsplit(buoy_coords," "),"[[", 2))
    
    #existFrame <- data.frame(id=buoys_ids[which(buoyExistance==T)], obs=paste0(buoySubObs, "<br/>"), time=paste0("Late Updated on ", exTime), name=buoy_names,
    #                         link=buoy_links, lat=buoy_lats, lon=buoy_lons)
    existFrame <- data.frame(id=buoys_ids[which(buoyExistance==T)], obs=paste0(buoySubObs, "<br/>"), date=date, time=time, name=buoy_names,
                             link=buoy_links, lat=buoy_lats, lon=buoy_lons)
  }
  
  ##########################################
  ##for those urls which do not exist
  if(F%in%buoyExistance){
    noExist <- buoys_ids[which(buoyExistance==F)]
    noExistUS <- which(US_buoys$ID%in%noExist)
    noExistFrame <- data.frame(id=noExist, obs="There are no current meteorological observations recorded at this buoy.<br/><br/>", date="", time="",
                               name=as.character(US_buoys$name[noExistUS]), link=paste0("http://www.ndbc.noaa.gov/station_page.php?station=", noExist),
                               lat=as.character(US_buoys$lat[noExistUS]), lon=as.character(US_buoys$lon[noExistUS]))
    existFrame <- rbind.data.frame(existFrame,noExistFrame)
  }
  
  ##output table
  fullFrame <- merge(x=outTab, y=existFrame, by="id", sort=F)
  
  fullFrame$date <-as.character(fullFrame$date)
  fullFrame$time <-as.character(fullFrame$time)
  if(""%in%fullFrame$date){
    fullFrame$date[fullFrame$date==""] <- max(fullFrame$date[fullFrame$date!=""])
    fullFrame$time[fullFrame$time==""] <- max(fullFrame$time[fullFrame$time!=""])
  }
  
  return(fullFrame)
}
##########################################################################
##########################################################################
# Function extracting tide data from within the past 18 mins from a XML file online.
collectLatestTidal <- function(varURL){
  #################
  #varURL <- varURLs[1]
  #varURL <- varURLs[9]
  #################
  xml_data <- retry(xmlToList(rawToChar(GET(varURL)$content)))
  var <- sapply(strsplit(varURL, "product=|&datum="), "[[", 2)
  
  if("error" %in% names(xml_data) | class(xml_data)!="list"){
    ##if there is no available data
    value <- NA
    date <- NA
    time <- NA
    metaID <- NA
    metaName <- NA
    metaLat <- NA
    metaLon <- NA
  }else if(is.null(xml_data$observations$disclaimers)==FALSE){
    remove <- c("disclaimers.disclaimer.text", "disclaimers.disclaimer..attrs")
    chkVals <- data.frame(t(xml_data$observations[[1]]))
    chkVals <- chkVals[!rownames(chkVals) %in% remove,]
    value <- chkVals$v
    if(var=="wind"){
      value <- paste0("From the ", chkVals$dr, " at ", chkVals$s)
    }
    #date <- as.POSIXct(chkVals$t, format = "%Y-%m-%d %H:%M", tz = "GMT")
    #date <- format(date, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York") # convert from GMT to current time zone
    dateBase <- as.POSIXct(chkVals$t, format="%Y-%m-%d %H:%M", tz="GMT")
    date <- format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from GMT to current time zone
    time <- format(dateBase, format="%I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
    metaID <- xml_data$metadata["id"]
    metaName <- xml_data$metadata["name"]
    metaLat <- xml_data$metadata["lat"]
    metaLon <- xml_data$metadata["lon"]
  }else{
    chkVals <- data.frame(t(xml_data$observations[[1]]))
    if(var=="wind"){
      value <- paste0("From the ", as.character(chkVals$dr), " at ", as.character(chkVals$s))
    }else{
      value <- as.numeric(as.character(chkVals[1,2]))
    }
    #date <- as.POSIXct(chkVals$t, format="%Y-%m-%d %H:%M", tz="GMT")
    #date <- format(date, format="%b %d, %Y %I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
    dateBase <- as.POSIXct(chkVals$t, format="%Y-%m-%d %H:%M", tz="GMT")
    date <- format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from GMT to current time zone
    time <- format(dateBase, format="%I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
    metaID <- as.character(xml_data$metadata["id"])
    metaName <- as.character(xml_data$metadata["name"])
    metaLat <- as.numeric(as.character(xml_data$metadata["lat"]))
    metaLon <- as.numeric(as.character(xml_data$metadata["lon"]))
  }
  
  #return(c(value, date, metaID, metaName, metaLat, metaLon))
  return(c(value, date, time, metaID, metaName, metaLat, metaLon))
}
##########################################################################
##########################################################################
# Run the function extracting the data we want and creating a plot.
# CREATE a function of this step!!
tideStationData <- function(statID, spDatum, timez, un){
  #################
  #statID <- tideIDs[206]
  #statID <- "8571421"
  #statID <- tideIDsMSL[1]
  #spDatum <- datum
  #timez <- timezone
  #un <- units
  #################
  vars <- c("air_temperature", "air_pressure", "visibility", "humidity", "wind", "water_level", "water_temperature", "conductivity", "salinity")

  # Use the ID, variable, datum, timezone, and units to create a URL to the XML file.
  varURLs <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?date=latest&station=', statID, '&product=', vars, '&datum=', spDatum, '&units=', un, '&time_zone=', timez, '&application=web_services&format=xml')
  
  getVarVals <- lapply(varURLs, collectLatestTidal)
  tableVars <- cbind.data.frame(vars, do.call(rbind.data.frame, getVarVals))
  colnames(tableVars)[2:ncol(tableVars)] <- c("value", "date", "time", "metaID", "metaName", "metaLat", "metaLon") 
  
  if(F %in% is.na(tableVars$time)){
    ##collect the non-NA update times
    validTimes <- which(is.na(tableVars$time)==F)
    ##determine which time is the latest
    #latTimeInd <- which.min(utctime(validTimes))
    latTimeInd <- which.min(utctime(as.POSIXlt(paste(tableVars$date[validTimes], tableVars$time[validTimes]), format="%b %d, %Y %I:%M %p", tz="EDT")))
    ##set all times to latest updated time
    tableVars$date <- as.character(tableVars$date[validTimes[latTimeInd]])
    tableVars$time <- as.character(tableVars$time[validTimes[latTimeInd]])
  }
  
  ##determine which variables to include in variable
  subVarTab <- tableVars[which(is.na(tableVars$value)==F),]
  
  #metaString <- ""
  dataFramed <- data.frame(id=statID, url=paste0("https://tidesandcurrents.noaa.gov/stationhome.html?id=",statID), obs=NA, date=NA, time=NA, image=paste0("https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_",statID,".png"), lon=NA, lat=NA)
  obsString <- ""
  if(nrow(subVarTab)>0){
    subVarNames <- subVarTab$vars
    if("air_temperature" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Air temperature: </strong>", subVarTab$value[which(subVarNames=="air_temperature")], " &#8451;<br/>")
    }
    
    if("air_pressure" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Air pressure: </strong>", subVarTab$value[which(subVarNames=="air_pressure")], " mbar<br/>")
    }
    
    if("visibility" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Visibility: </strong>", subVarTab$value[which(subVarNames=="visibility")], " nmi<br/>")
    }
    
    if("humidity" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Relative humidity: </strong>", subVarTab$value[which(subVarNames=="humidity")], " %<br/>")
    }
    
    if("wind" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Wind: </strong>", subVarTab$value[which(subVarNames=="wind")], " kn<br/>")
    }
    
    if("water_level" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Water level: </strong>", subVarTab$value[which(subVarNames=="water_level")], " m ", spDatum, "<br/>")
    }
    
    if("water_temperature" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Water temperature: </strong>", subVarTab$value[which(subVarNames=="water_temperature")], " &#8451;<br/>")
    }
    
    if("conductivity" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Conductivity: </strong>", subVarTab$value[which(subVarNames=="conductivity")], " mS/cm<br/>")
    }
    
    if("salinity" %in% subVarNames){
      obsString <- paste0(obsString, "<strong>Salinity: </strong>", subVarTab$value[which(subVarNames=="salinity")], " psu<br/>")
    }
    
    #metaString <- paste0(metaString, '{"type": "Feature", "properties": {"name": "', unique(subVarTab$metaName), '", "id": "', statID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', statID, '", "obs": "',
    #                     obsString, '", "time": "', unique(subVarTab$time), '", "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', statID, '.png"}, geometry": {"type": "Point", "coordinates": [', 
    #                     unique(subVarTab$metaLon), ',',  unique(subVarTab$metaLat), ']}}')
    dataFramed$obs <- obsString
    dataFramed$date <- unique(subVarTab$date)
    dataFramed$time <- unique(subVarTab$time)
    dataFramed$lon <- unique(subVarTab$metaLon)
    dataFramed$lat <- unique(subVarTab$metaLat)
  }
  
  #return(metaString)
  return(dataFramed)
}
##########################################################################
##########################################################################
# Function extracting tide data (hight and time) from a XML file online.
waterheight_plot <- function(url, weekMidnights, weekNoons, plotW, plotH, plotOut){
  #################
  #url <- 
  #weekMidnights <- day_midnight
  #weekNoons <- day_noon
  #plotW <- p.width
  #plotH <- p.height
  #plotOut <- plotDir
  #################
  
  statID <- sapply(strsplit(url, "&station=|&time_"), "[[", 2)
  #print(statID)

  xml_data <- xmlToList(rawToChar(GET(url)$content))
  
  ##create plot
  png(file=paste0(plotOut, "Fig_", statID, ".png"), family="Helvetica", units="in", width=plotW, height=plotH, pointsize=14, res=300)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
  if("error" %in% names(xml_data) | class(xml_data)!="list"){
    plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    legend("center", "No data available", bg="white")
  }else{
    if(is.null(xml_data$observations$disclaimers)==FALSE){
      remove <- which(names(xml_data$observations)=="disclaimers")
      xml_data$observations[remove] <- NULL
    }
    stationData <- do.call(rbind.data.frame, xml_data$observations)
    colnames(stationData) <- c("time", "values", "s", "f", "q")
    
    date <- as.POSIXct(stationData$time, format="%Y-%m-%d %H:%M", tz="GMT")
    date <- format(date, tz="America/New_York") # convert from GMT to current time zone
    stationData$time <- as.POSIXct(date, format="%Y-%m-%d %H:%M", tz="")
    # Determine which indices of the date occur at midnight and noon.
    hours <- strftime(stationData$time, format="%H:%M")
    midnight <- which(hours=="00:00")
    noon <- which(hours=="12:00")
    
    plot(stationData$time, stationData$values, type="n", ylab=paste0("Height (m ", datum, ")"), xlab="Past 3 days", xaxt="n")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    axis(1, at=weekMidnights, labels=FALSE, tick=TRUE)
    axis(1, at=weekNoons, labels=gsub("0(\\d)", "\\1", format(weekNoons, "%m/%d")), tick=FALSE)
    grid(NA, NULL, lty=6, col="gray")
    abline(v=weekMidnights, lty=6, col="gray")
    lines(stationData$time, stationData$values, lwd=2, col="steelblue")
  }
  dev.off()
}
##########################################################################
##########################################################################
# Function extracting stream data (discharge and time) from a TXT file online.
usgs_dataRetrieveVar = function(url, tz, data){
  #################
  #url <- gageTmpURLs[1]
  #url <- dischargeURL
  #url <- dailyAveURL
  #url <- heightURL
  #url <- gageURL<-paste0("https://waterdata.usgs.gov/nwis/uv?cb_", var, "=on&format=rdb&site_no=", gageID, "&period=&begin_date=", getDate, "&end_date=", getDate)
  #tz <- "America/New_York"
  #data <- "latest"  ##"full"  ##daily
  #data <- "full"
  #data <- "daily"
  #################
  # Extract number of metadata rows to skip and determine header names
  readr.total <- retry(read_lines(url))
  totalRows <- length(readr.total)
  meta.rows <- length(readr.total[grep("^#", readr.total)])  ##rows that begin with a hashtag
  
  header.names <- strsplit(readr.total[meta.rows+1],"\t")[[1]]

  ##format the data based on what data is desired
  if(data=="latest"){
    ##only concerned with the latest data
    latestData <- strsplit(readr.total[totalRows],"\t")[[1]]
    if((totalRows==meta.rows+3) & (length(latestData)==0)){  ##there is no actual data
      return(c(NA,NA))
    }
    
    ##get varialbe name and index
    var <- strsplit(url, "cb_|=on")[[1]][2]
    varInd <- grep(var, header.names)[1]
    # NEED TO CHECK FOR MISSING DATA!!!
    if(is.na(varInd) || is.na(latestData[varInd])==T){
      return(c(NA,NA))
    }
    
    # Convert and extract the date and time.
    dateInd <- which(header.names=="datetime")
    tzInd <- which(header.names=="tz_cd")
    #latestData[dateInd] <- format(as.POSIXct(latestData[dateInd], format="%Y-%m-%d %H:%M", tz=tz), format="%b %d, %Y %I:%M %p %Z")
    dateBase <- as.POSIXct(latestData[dateInd], format="%Y-%m-%d %H:%M", tz=latestData[tzInd])
    #latestData[dateInd] <- format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from local time zone to Eastern NA
    #latestData <- c(latestData[1:dateInd], format(dateBase, format="%I:%M %p %Z", tz="America/New_York"), latestData[tzInd:length(latestData)]) # convert from local time zone to Eastern NA
    
    return(c(latestData[varInd], format(dateBase, format="%b %d, %Y", tz="America/New_York"), format(dateBase, format="%I:%M %p %Z", tz="America/New_York")))
    
  }else if(data=="full"){
    ##want all of the available within the timeframe
    splitData <- strsplit(readr.total[(meta.rows+3):totalRows],"\t")
    tableData <- do.call(rbind.data.frame, splitData)
    if(nrow(tableData)<1){
      return(NA)
    }
    
    colnames(tableData) <- header.names
    # Give parameter data a common header name 
    varCol <- grep(sapply(strsplit(url, "cb_|=on"), "[[", 2), header.names)[1]
    colnames(tableData)[varCol] <- "var"
    tableData$var <- as.numeric(as.character(tableData$var))
    
    # NEED TO CHECK FOR MISSING DATA!!!
    if(unique(is.na(tableData$var))==T){
      return(NA)
    }
    
    # Convert and extract the date and time.
    tableData$datetime <- as.POSIXct(tableData$datetime, format="%Y-%m-%d %H:%M", tz=tz)
    tableData$time <- strftime(tableData$datetime, format="%H:%M", tz=tz)
    
    return(tableData)
    
  }else if(data=="daily"){
    ##want all of the daily data within the timeframe
    splitData <- strsplit(readr.total[(meta.rows+3):totalRows],"\t")
    tableData <- do.call(rbind.data.frame, splitData)
    if(nrow(tableData)<1){
      return(NA)
    }
    
    colnames(tableData) <- header.names[1:ncol(tableData)]
    ##match dates 
    # Convert month and day to a date.
    tableData$MonDay <- as.POSIXct(paste(tableData$month_nu, tableData$day_nu, sep="-"), format="%m-%d", tz="America/New_York")
    #format(paste(tableData$month_nu, tableData$day_nu, sep="-"), format="%m-%d")
    
    # NEED TO CHECK FOR MISSING DATA!!!
    if(unique(is.na(tableData$mean_va))==T){
      return(NA)
    }else{
      ##bDate and eDate are global variables
      ##remove year from bDate and eDate
      datesInd <- sapply(c(bDate, eDate), function(x){grep(format(x, format="%m-%d"), tableData$MonDay)})
      
      if(class(datesInd)=="list"){
        weeksAve <- NA
      }else if(datesInd[1] > datesInd[2]){
        ##this may happen around the New Year
        weeksAve <- mean(as.numeric(as.character(tableData$mean_va[c(1:datesInd[2], datesInd[1]:nrow(tableData))])), na.rm=T)
      }else{
        weeksAve <- mean(as.numeric(as.character(tableData$mean_va[datesInd[1]:datesInd[2]])), na.rm=T)
      }
    }
    return(weeksAve)
  }
}
##########################################################################
##########################################################################
##function to create a string of observations based on what is available
createObsString <- function(tab){
  #################
  #tab <- fullObsData
  #################
  ##character objects to put results into 
  tempObs <- dischargeObs <- heightObs <- vector(mode="character", length=nrow(tab))
  
  ##determine the index for each possible variable combination
  tempNotNA <- which(is.na(tab$temp)==F)
  dischargeNotNA <- which(is.na(tab$discharge)==F)
  heightNotNA <- which(is.na(tab$gageHeight)==F)
  
  ##construct the observation string
  tempObs[tempNotNA] <- paste0(tempObs[tempNotNA], "<strong>Temperature: </strong>", tab$temp[tempNotNA], " &#8457")
  dischargeObs[dischargeNotNA] <- paste0(dischargeObs[dischargeNotNA], "<strong>Discharge: </strong>", tab$discharge[dischargeNotNA], " ft&#179;/s")
  heightObs[heightNotNA] <- paste0(heightObs[heightNotNA], "<strong>Gage height: </strong>", tab$gageHeight[heightNotNA], " ft")
  
  outObs <- sapply(1:nrow(tab), function(x){obs<-c(tempObs[x],dischargeObs[x],heightObs[x]);
                                            obs<-obs[which(obs!="")];
                                            if(length(obs)>1){
                                              return(paste0("<br/>", paste0("<br/>", obs, collapse=""), "<br/><br/>"))
                                            }else if(length(obs)==1){
                                              return(paste0("<br/>", paste0("<br/>", obs), "<br/><br/>"))
                                            }else{
                                              return("")
                                            }})
  return(outObs)
}
##########################################################################
##########################################################################
# Function extracting stream data (discharge, gage height, and time) from USGS stream gages.
stream_gage_plot <- function(dischargeURL, heightURL, weekMidnights, weekNoons, plotW, plotH, plotOut){
  #################
  #dischargeURL <- gageDisURLs[616]
  #heightURL <- gageGagURLs[616]
  #weekMidnights <- day_midnight
  #weekNoons <- day_noon
  #plotW <- p.width
  #plotH <- p.height
  #plotOut <- plotDir
  #################
  
  statID <- sapply(strsplit(dischargeURL, "site_no=|&period"), "[[", 2)
  print(statID)
  
  if(getURL(dischargeURL)=="No sites/data found using the selection criteria specified \n"){
    dischargeDat <- NA
  }else{
    dischargeDat <- usgs_dataRetrieveVar(dischargeURL, "America/New_York", "full")
  }
  if(getURL(heightURL)=="No sites/data found using the selection criteria specified \n"){
    heightDat <- NA
  }else{
    heightDat <- usgs_dataRetrieveVar(heightURL, "America/New_York", "full")
  }
  
  
  # Export a plot from the discharge data.
  png(file=paste0(plotOut, "Fig_", statID, ".png"), family="sans", units="in", width=plotW, height=plotH, pointsize=14, res=300)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,2.5))
  
  if(all(is.na(dischargeDat)) && all(is.na(heightDat))){    # No data is available
    # During cold weather, stage and discharge values may be affected by ice at some
    # streamgages. Streamgages experiencing ice conditions will have the discharge record
    # temporarily disabled to prevent the display of erroneous discharge values. Display of
    # discharge record will resume when it is determined that ice conditions are no longer
    # present. Adjustment of data affected by ice can only be done after detailed analysis.
    plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    legend("center", "No data available", bg="white")
    
  }else if(all(is.na(dischargeDat))==FALSE && all(is.na(heightDat))){    # Only discharge data is available
    # Extract daily mean statistic for days of the current week
    dailyAveURL <- paste0('https://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=', statID, '&parameterCd=00060&statReportType=daily')
    if(getURL(dailyAveURL)=="# //Output-Format: RDB\n# //Response-Status: OK\n# //Response-Message: No sites found matching all criteria\n"){
      daily_avgQ <- NA
    }else{
      daily_avgQ <- usgs_dataRetrieveVar(dailyAveURL, "America/New_York", "daily")
    }
    
    # Create discharge plot.
    plot(dischargeDat$datetime, dischargeDat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n",
         col="#018571", col.ticks="#018571", col.axis="#018571", ylim=c(min(daily_avgQ, min(dischargeDat$var, na.rm=TRUE), na.rm=TRUE),
                                                                        max(daily_avgQ, max(dischargeDat$var, na.rm=TRUE), na.rm=TRUE)))
    
    mtext(2, text=expression("Discharge"~(ft^3/s)), line=1.25, col="#018571")
    # Color the background light gray.
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    
    # Set axis ticks to midnight and labels to noon.
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    
    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    # Add the data.
    lines(dischargeDat$datetime, dischargeDat$var, lwd=2, col="#018571") # steelblue
    
    # Add climate average
    abline(h=daily_avgQ, lty=3, lwd=2, col="#018571")
    
  }else if(all(is.na(dischargeDat)) && all(is.na(heightDat))==FALSE ){    # Only gauge height data is available
    # Create gage height plot.
    plot(heightDat$datetime, heightDat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n", yaxt="n")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    
    axis(4, col="#a6611a", col.ticks="#a6611a", col.axis="#a6611a")
    mtext(4, text="Gage height (ft)", line=1.25, col="#a6611a")
    
    # Set axis ticks to midnight and labels to noon.
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    
    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    # Add the data.
    lines(heightDat$datetime, heightDat$var, lwd=2, col="#a6611a") # steelblue
    
  } else if(all(is.na(dischargeDat))==FALSE && all(is.na(heightDat))==FALSE){    # Discharge and gauge height data are available
    # Extract daily mean statistic for days of the current week
    dailyAveURL <- paste0('https://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=', statID, '&parameterCd=00060&statReportType=daily')
    if(getURL(dailyAveURL)=="# //Output-Format: RDB\n# //Response-Status: OK\n# //Response-Message: No sites found matching all criteria\n"){
      daily_avgQ <- NA
    }else{
      daily_avgQ <- usgs_dataRetrieveVar(dailyAveURL, "America/New_York", "daily")
    }
    
    # Create discharge and gage height plot.
    plot(dischargeDat$datetime, dischargeDat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n",
         col="#018571", col.ticks="#018571", col.axis="#018571", ylim=c(min(daily_avgQ, min(heightDat$var, na.rm=TRUE), min(dischargeDat$var, na.rm=TRUE), na.rm=TRUE),
                                                                        max(daily_avgQ, max(heightDat$var, na.rm=TRUE), max(dischargeDat$var, na.rm=TRUE), na.rm=TRUE)))
    
    mtext(2, text=expression("Discharge"~(ft^3/s)), line=1.25, col="#018571")
    # Color the background light gray.
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    
    # Set axis ticks to midnight and labels to noon.
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    
    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    # Add the data.
    lines(dischargeDat$datetime, dischargeDat$var, lwd=2, col="#018571") # steelblue
    
    # Add climate average
    abline(h=daily_avgQ, lty=3, lwd=2, col="#018571")
    
    # Add gage height data
    par(new=TRUE)
    plot(heightDat$datetime, heightDat$var, lwd=2, col="#a6611a", typ="l",
         ylab="", xlab="", xaxt="n", yaxt="n")
    axis(4, col="#a6611a", col.ticks="#a6611a", col.axis="#a6611a")
    mtext(4, text="Gage height (ft)", line=1.25, col="#a6611a")
    
  }else{ # Create empty plot to indicate there may be something wrong with the script
    plot(c(b.date,e.date), rep(0, 2), xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    
    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    legend("center", "Check import script for errors", bg = "white")
    
  }
  dev.off()
}
##########################################################################
##########################################################################
# Function extracting weather data from an XML file.
parseWW_xml = function(ID){
  #################
  #ID <- "OHC023"
  #ID <- "OHC057"
  #ID <- "OHC109"
  #ID <- "NJC013"
  #################
  print(ID)
  #url = paste("https://alerts.weather.gov/cap/wwaatmget.php?x=", ID, "&y=1", sep="")
  url = paste("https://alerts.weather.gov/cap/wwaatmget.php?x=", ID, "&amp;y=1", sep="")
  
  #for(i in 1:25000){
  #  getData <- GET(url)
  #  getContent <- getData$content
  #  makeChar <- rawToChar(getContent)
  #  makeList <- xmlToList(makeChar)
  #  holdChar <- makeChar
  #}
  
  # Turn XML data into a list.
  xml_data <- retry(xmlToList(rawToChar(GET(url)$content)), max=6, delay=60)
  name <- xml_data$title
  entry <- xml_data$entry$title
  link <- xml_data$entry$id
  
  if(length(entry)<1 | entry=="There are no active watches, warnings or advisories"){
    cols = "#00000000" # 100% transparent black
    
    time <- xml_data$updated
    
    # Reformat time to match the rest of the Marisa data
    format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "GMT")
    format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York")
    
    OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", entry, "</strong><br/><br/>Last Updated on ", format_time, sep="")
    
  } else {
    xml_active <- xmlToList(rawToChar(GET(link)$content))
    time <- xml_data$updated
    
    # Reformat time to match the rest of the Marisa data
    format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "EST8EDT")
    format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "EST8EDT")

    # headline <- xml_active$info$headline
    event <- xml_active$info$event
    issued <- xml_active$sent
    # effective <-xml_active$info$effective
    expiring <- xml_active$info$expires
    severity <- xml_active$info$severity
    description <- xml_active$info$description
    instructions <- xml_active$info$instruction
    areas_affected <- xml_active$info$area$areaDesc

    cols <- as.character(NWS_cols[match(event, NWS_cols[ ,1]), 2])

    # Reformat time to match the rest of the Marisa data
    format_issued <- as.POSIXct(str_replace_all(issued, "T", " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
    format_issued <- format(format_issued, format = "%b %d, %Y %I:%M %p %Z")
    format_expiring <- as.POSIXct(str_replace_all(expiring, "T", " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
    format_expiring <- format(format_expiring, format = "%b %d, %Y %I:%M %p %Z")

    OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", event,
                "</strong><br/><strong>Issued: </strong>", format_issued, "<br/><strong>Expires: </strong>", format_expiring,
                "<br/><strong>Severity: </strong><br/><br/><strong>Alert: </strong>", description,
                "<br/><br/><strong>Instructions: </strong>", instructions, "<br/><br/><strong>Areas affected: </strong>", areas_affected,
                "<br/><br/>Last Updated on ", format_time, sep="")
    OBS<- str_replace_all(OBS, "([\n])([*])", "<br/>*")
    OBS<- str_replace_all(OBS, "([\n])", " ")
    OBS<- str_replace_all(OBS, "([\"])", " inches")
  }
  return(c(ID, OBS, cols))
}
##########################################################################
##########################################################################
# Function to create plots
plot_climdiv = function(climate_dat, state, writeDir){
  #################
  #climate_dat <- stateDivTabs[[1]]
  #state <- state_name
  #writeDir <- wrtDir
  #################
  
  clim_30yrs <- aggregate(climate_dat, by=list(climate_dat$month), FUN=mean, na.rm=T)
  monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  clim_30yrs$month <- factor(monthNames, levels=monthNames)
  
  
  #years = unique(climate_division$Year)
  #pcp.yrs = rep(NA, length(years))
  #for(i in 1:length(years)){
  #  pcp.yrs[i] = mean(climate_division$PCP[which(climate_division$Year == years[i])])
  #}
  #yearly.avg.prcp = mean(pcp.yrs)
  
  #clim_30yrs = data.frame(tmax = tmax, tmin = tmin, tavg = tavg, prcp = pcp,
  #                        month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), month_num = 1:12)
  #clim_30yrs$month <- factor(clim_30yrs$month, levels = clim_30yrs$month[order(clim_30yrs$month_num)])
  
  png(file=paste0(writeDir, "Fig_", gsub(" ","",state), "-", as.numeric(unique(climate_dat$division)), ".png"), family="sans", units="in", width=p.width, height=p.height*2, pointsize=12, res=300)
  par(mfrow=c(2,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.5))
  plot(1:12, clim_30yrs$tmax, type="l", lwd = 2, col=temp_cols[3], xaxt="n", yaxt="n",
       ylab=expression(paste("Temperature (", degree, "F)", sep="")), xlab="",
       ylim = c(min(clim_30yrs$tmin), max(clim_30yrs$tmax)), bty="l")
  lines(1:12, clim_30yrs$tmin, type="l", lwd = 2, col=pcp_cols[2])
  lines(1:12, clim_30yrs$tmpc, type="l", lwd = 2, col="black")
  axis(2, las=2, tck=-0.025)
  axis(1, labels=clim_30yrs$month, at=1:12, tck=-0.025)
  legend("topleft", legend=c("Maximum", "Average", "Minimum"), pch = 15, col = c(temp_cols[3], "black", pcp_cols[2]), ncol=1, bty="n", cex=1)
  mtext(paste0("(", min(climate_dat$year), "-", max(climate_dat$year), ")"), side = 3, las=1, adj = 1, line=-1)
  
  plot.new()
  vps <- baseViewports()
  pushViewport(vps$figure)
  
  pp = ggplot(clim_30yrs, aes(x=month, y=pcpn)) + geom_bar(stat = "identity", fill=pcp_cols[3], width=0.6) +
    scale_x_discrete(breaks = clim_30yrs$month[seq(1, length(clim_30yrs$month), by = 2)]) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text=element_text(size=11, colour = "black"),
          axis.title=element_text(size=11, colour = "black")) +
    labs(x="", y = "Precipitation (in)")
  
  vp <- viewport(height = unit(1,"npc"), width=unit(1, "npc"),
                 just = c("left","top"),
                 y = 1, x = 0)
  print(pp, vp = vp)
  dev.off()
}
##########################################################################
##########################################################################
# Function to cycle through climate divisions within a state
state_climdiv = function(stateNum, fullTab, wrtDir){
  #################
  #stateNum <- "36"
  #fullTab <- fullVarTab
  #wrtDir <- outDir
  #################
  stateTab <- fullTab[fullTab$state==stateNum,]
  
  #statediv$Year = substr(statediv$YearMonth, 1, 4)
  #statediv$Month = substr(statediv$YearMonth, 5, 6)
  #statediv30yrs = statediv[min(which(statediv$Year == 1988)):max(which(statediv$Year == 2018)), ]
  latestYr <- max(as.numeric(stateTab$year))
  seqYrs <- as.character((latestYr-29):latestYr)
  stateTab30yrs <- stateTab[which(stateTab$year %in% seqYrs),]
  
  subVars <- stateTab30yrs[,c("division", "year", "month", "tmax", "tmin", "tmpc", "pcpn")]
  stateDivTabs <- lapply(unique(subVars$division), function(div_num){subVars[subVars$division==div_num,]})
  
  lapply(stateDivTabs, plot_climdiv, state=stateTab$Name, writeDir=wrtDir)
  #apply(as.array(unique(statediv30yrs$Division)), 1, plot_climdiv, climate_dat=statediv30yrs, state=state_name)
}
##########################################################################
##########################################################################
combineClimDivDat <- function(tabFile){
  #################
  #tabFile <- downloadFileNames[1]
  #tabFile <- downloadFileNames[9]
  #################
  #varTab <- read.table(tabFile, header=F, na.strings=c(-9.99, -99.9, -99.99, -9999, -9999.))
  varTab <- read.table(tabFile, header=F, na.strings=c("-9999.", "-9.99", "-99.99", "-99.90"))
  varTxt <- sapply(strsplit(tabFile, "_Data/|_download"), "[[", 2)
  colnames(varTab) <- c("headerInfo", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  
  ##splitting up the header info
  ##first, checks the number of characters in each header, and if only have 9, add the 0 to the front
  numChars <- sapply(varTab$headerInfo, nchar)
  varTab$state <- mapply(function(ent,nCh){substr(ent,1,nCh-8)}, ent=varTab$headerInfo, nCh=numChars)  ##state
  varTab$state <- sapply(varTab$state, function(txt){if(nchar(txt)==1){
                                                      return(paste0("0",txt))
                                                    }else{
                                                      return(txt)
                                                    }})
  varTab$year <- mapply(function(ent,nCh){substr(ent,nCh-3,nCh)}, ent=varTab$headerInfo, nCh=numChars)  ##year
  #varTab$variable <- mapply(function(ent,nCh){substr(ent,nCh-5,nCh-4)}, ent=varTab$headerInfo, nCh=numChars)  ##variable
  varTab$division <- mapply(function(ent,nCh){substr(ent,nCh-7,nCh-6)}, ent=varTab$headerInfo, nCh=numChars)  ##division
  ##split off the original head column
  varTab <- varTab[,-which(colnames(varTab) %in% "headerInfo")]
  
  ##make a melt table (cheesy) to be in the form needed
  cheesyTable <- melt(varTab, varible.name="month", value.names=vars[1], id.vars=c("division", "state", "year"))
  colnames(cheesyTable)[which(colnames(cheesyTable) %in% c("variable", "value"))] <-c("month", varTxt)
  
  ##just in case the na.strings argument from the read in table
  #cheesyTable[,5][cheesyTable[,5]==-99.99]
  #cheesyTable[,5][cheesyTable[,5]==-99.9]
  #cheesyTable[,5][cheesyTable[,5]==-9999]
  
  return(cheesyTable)
}
##########################################################################
##########################################################################