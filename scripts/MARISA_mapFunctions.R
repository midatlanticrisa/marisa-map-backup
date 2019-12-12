##header

##########################################################################
##########################################################################
##a function to retry running a function if there is an error
##modified from code found at: https://stackoverflow.com/questions/37379472/creating-a-retry-function-in-r
retry <- function(a, max=6, init=0){
  suppressWarnings(tryCatch({
  if(init<max){
    a
  }}, error=function(e){
    retry(a, max, init=init+1)
    }))
}
##########################################################################
##########################################################################
# Function extracting weather data from an XML file.
parse_xml = function(id){
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
    time <- ""
  } else {
    time <- xml_data$observation_time
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
  return(c(name, as.character(id), latitude, longitude, obs, link, time))
}
##########################################################################
##########################################################################
collectBuoyData = function(buoys_ids, US_buoys){
  #################
  #buoys_ids <- NDBC_stations$ID
  #buoys_ids <- NDBC_buoys$ID
  #US_buoys <- US_buoys
  #################
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
    
    # Extract the name, link, and coordinates which are in the 'title', 'link', and 'georss:point' node.
    buoy_names <- sapply(docs, xpathSApply, path='//channel/item/title', fun=xmlValue)
    buoy_links <- sapply(docs, xpathSApply, path='//channel/item/link', fun=xmlValue)
    buoy_coords <- sapply(docs, xpathSApply, path='//channel/item/georss:point', fun=xmlValue)
    buoy_lats <- as.numeric(sapply(strsplit(buoy_coords," "),"[[", 1))
    buoy_lons <- as.numeric(sapply(strsplit(buoy_coords," "),"[[", 2))
    
    existFrame <- data.frame(id=buoys_ids[which(buoyExistance==T)], obs=paste0(buoySubObs, "<br />"), time=paste0("Late Updated on ", exTime), name=buoy_names,
                             link=buoy_links, lat=buoy_lats, lon=buoy_lons)
  }
  
  ##########################################
  ##for those urls which do not exist
  if(F%in%buoyExistance){
    noExist <- buoys_ids[which(buoyExistance==F)]
    noExistUS <- which(US_buoys$ID%in%noExist)
    noExistFrame <- data.frame(id=noExist, obs="There are no current meteorological observations recorded at this buoy.", time="",
                               name=as.character(US_buoys$name[noExistUS]), link=paste0("http://www.ndbc.noaa.gov/station_page.php?station=", noExist),
                               lat=as.character(US_buoys$lat[noExistUS]), lon=as.character(US_buoys$lon[noExistUS]))
  }
  
  ##output table
  fullFrame <- merge(x=outTab, y=rbind.data.frame(existFrame,noExistFrame), by="id", sort=F)
  
  return(fullFrame)
}
##########################################################################
##########################################################################
# Function extracting tide data from within the past 18 mins from a XML file online.
collectLatestTidal <- function(varURL){
  #################
  #varURL <- varURLs[1]
  #################

  xml_data <- retry(xmlToList(rawToChar(GET(varURL)$content)))
  var <- sapply(strsplit(varURL, "product=|&datum="), "[[", 2)
  
  if("error" %in% names(xml_data)){
    ##if there is no available data
    value <- NA
    date <- NA
    metaID <- NA
    metaName <- NA
    metaLat <- NA
    metaLon <- NA
  }else if(is.null(xml_data$observations$disclaimers)==FALSE){
    remove <- c("disclaimers.disclaimer.text", "disclaimers.disclaimer..attrs")
    chkVals <- chkVals[!rownames(chkVals) %in% remove,]
    value <- chkVals$v
    if(var=="wind"){
      value <- paste0("From the ", chkVals$dr, " at ", chkVals$s)
    }
    date <- as.POSIXct(chkVals$t, format = "%Y-%m-%d %H:%M", tz = "GMT")
    date <- format(date, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York") # convert from GMT to current time zone
    metaID <- xml_data$metadata["id"]
    metaName <- xml_data$metadata["name"]
    metaLat <- xml_data$metadata["lat"]
    metaLon <- xml_data$metadata["lon"]
  }else{
    chkVals <- data.frame(t(xml_data$observations[[1]]))
    if(var=="wind"){
      value <- paste0("From the ", chkVals$dr, " at ", chkVals$s)
    }else{
      value <- chkVals$v
    }
    date <- as.POSIXct(chkVals$t, format = "%Y-%m-%d %H:%M", tz = "GMT")
    date <- format(date, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York") # convert from GMT to current time zone
    metaID <- xml_data$metadata["id"]
    metaName <- xml_data$metadata["name"]
    metaLat <- xml_data$metadata["lat"]
    metaLon <- xml_data$metadata["lon"]
  }
  return(c(value, date, metaID, metaName, metaLat, metaLon))
}
##########################################################################
##########################################################################
# Run the function extracting the data we want and creating a plot.
# CREATE a function of this step!!
tideStationData <- function(statID, spDatum, timez, un){
  #################
  #statID <- tideIDs[5]
  #spDatum <- datum
  #timez <- timezone
  #un <- units
  #################
  vars <- c("air_temperature", "air_pressure", "visibility", "humidity", "wind", "water_level", "water_temperature", "conductivity", "salinity")
  
  # Use the ID, variable, datum, timezone, and units to create a URL to the XML file.
  varURLs <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?date=latest&station=', statID, '&product=', vars, '&datum=', spDatum, '&units=', un, '&time_zone=', timez, '&application=web_services&format=xml')
  
  getVarVals <- lapply(varURLs, collectLatestTidal)
  tableVars <- cbind.data.frame(vars, do.call(rbind.data.frame, getVarVals))
  colnames(tableVars)[2:ncol(tableVars)] <- c("value", "time", "metaID", "metaName", "metaLat", "metaLon") 
  
  if(F %in% is.na(tableVars$time)){
    ##collect the non-NA update times
    validTimes <- tableVars$time[which(is.na(tableVars$time)==F)]
    ##determine which time is the latest
    latTimeInd <- which.min(utctime(validTimes))
    ##set all times to latest updated time
    tableVars$time <- as.character(validTimes[latTimeInd])
  }
  
  ##determine which variables to include in variable
  subVarTab <- tableVars[which(is.na(tableVars$value)==F),]
  
  metaString <- ""
  obsString <- ""
  if(nrow(subVarTab)>0){
    metaString <- paste0(metaString, '{"type": "Feature", "properties": {"name": "', unique(subVarTab$metaName), '", "id": "', statID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', statID, '", "obs": "',
                         OBS[2], '", "time": "', TIME[2], '", "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', statID, '.png"}, geometry": {"type": "Point", "coordinates": [', unique(subVarTab$metaLon), 
                         ',',  unique(subVarTab$metaLat), ']}}')
    
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
  }
  
  return(c(metaString, obsString))
}
##########################################################################
##########################################################################








