
library(RCurl)
library(measurements)

##########################################################################
##########################################################################
#' Convert direction in degrees to one of 16 cardinal directions
#' 
#' @param direction value in degrees between 0-360
#' @return string of cardinal direction
# ------------------------------------------------------------------------
cardinal_direction = function(direction){
  if(is.na(direction)){
    cardDir <- "NA"
  }else if(direction>=0 & direction<=11.24){
    cardDir <- "N"
  }else if(direction>=11.25 & direction<=33.74){
    cardDir <- "NNE"
  }else if(direction>=33.75 & direction<=56.24){
    cardDir <- "NE"
  }else if(direction>=56.25 & direction<=78.74){
    cardDir <- "ENE"
  }else if(direction>=78.75 & direction<=101.24){
    cardDir <- "E"
  }else if(direction>=101.25 & direction<=123.74){
    cardDir <- "ESE"
  }else if(direction>=123.75 & direction<=146.24){
    cardDir <- "SE"
  }else if(direction>=146.25 & direction<=168.74){
    cardDir <- "SSE"
  }else if(direction>=168.75 & direction<=191.24){
    cardDir <- "S"
  }else if(direction>=191.25 & direction<=213.74){
    cardDir <- "SSW"
  }else if(direction>=213.75 & direction<=236.24){
    cardDir <- "SW"
  }else if(direction>=236.25 & direction<=258.74){
    cardDir <- "WSW"
  }else if(direction>=258.75 & direction<=281.24){
    cardDir <- "W"
  }else if(direction>=281.25 & direction<=303.74){
    cardDir <- "WNW"
  }else if(direction>=303.75 & direction<=326.24){
    cardDir <- "NW"
  }else if(direction>=326.25 & direction<=348.74){
    cardDir <- "NNW"
  }else if(direction>=348.75 & direction<=360){
    cardDir <- "N"
  }
  return(cardDir)
}
##########################################################################
##########################################################################
#' Convert coordinates to S/N or W/E
#' 
#' @param value value in degrees between -180-180
#' @param coord string if latitude or longitude. Default: "lat"
#' @return string of cardinal direction
# ------------------------------------------------------------------------
coordinate_hemi = function(val, coord = "lat"){
  if(coord == "lat"){
    valDir = ifelse(val<0, paste0(abs(val), "S"), paste0(val, "N"))
  }else {
    valDir = ifelse(val<0, paste0(abs(val), "W"), paste0(val, "E"))
  }
  return(valDir)
}
##########################################################################
##########################################################################
#' Download realtime NDBC buoy data
#' 
#' @param bbox Boundary extent for buoys to return. 
#'   Format = c('xmin','ymin','xmax','ymax'). Default: all buoys
#' @return data.frame of buoy observations for the region of interest.
#
# The most recent observation (less than two hours old) from all stations hosted 
# on the NDBC web site. Less than 100KB, and updated approximately every 5 minutes.
# https://www.ndbc.noaa.gov/docs/ndbc_web_data_guide.pdf
# ------------------------------------------------------------------------
# Measurement descriptions and units:
# https://www.ndbc.noaa.gov/faq/measdes.shtml
# ------------------------------------------------------------------------
collectBuoyData = function(bbox=NULL){
  
  buoyURL <- 'https://www.ndbc.noaa.gov/data/latest_obs/latest_obs.txt'
  metaURL <- 'https://www.ndbc.noaa.gov/data/stations/station_table.txt'
  
  # Check if url exist
  buoyExistance <- url.exists(buoyURL)
  metaExistance <- url.exists(metaURL)
  
  if(buoyExistance & metaExistance){ 
    # Download data reading each line
    readBuoyObs <- readLines(buoyURL)
    
    # Extract and clean up headers
    buoyHeaders <- readBuoyObs[1]
    buoyHeaders <- gsub(pattern="#", "", buoyHeaders)
    buoyHeaders <- strsplit(buoyHeaders, split="\\s+")[[1]] # split by 1 or more spaces
    
    # Extract and clean up units
    buoyUnits <- readBuoyObs[2]
    buoyUnits <- gsub(pattern="#", "", buoyUnits)
    buoyUnits <- strsplit(buoyUnits, split="\\s+")[[1]] # split by 1 or more spaces
    
    # Extract and clean up observations
    buoyObs <- readBuoyObs[3:length(readBuoyObs)]
    buoyObs = strsplit(buoyObs, split="\\s+") # split by 1 or more spaces
    buoyObs = do.call(rbind.data.frame, buoyObs) # list to data frame
    colnames(buoyObs) <- buoyHeaders
    # Missing data in the Realtime files are denoted by "MM"
    buoyObs[buoyObs == "MM"] <- NA
    
    # Download metadata reading each line
    readBuoyMeta <- readLines(metaURL)
    
    # Extract and clean up headers
    metaHeaders <- readBuoyMeta[1]
    metaHeaders <- gsub(pattern="#", "", metaHeaders)
    metaHeaders <- gsub(pattern="\\|", "", metaHeaders)
    metaHeaders <- trimws(metaHeaders) # remove leading/trailing whitespace
    metaHeaders <- strsplit(metaHeaders, split="\\s+")[[1]] # split by 1 or more spaces
    
    # Extract and clean up observations
    buoyMeta <- readBuoyMeta[3:length(readBuoyMeta)]
    buoyMeta = strsplit(buoyMeta, split="\\|") # split by 1 or more spaces
    buoyMeta = do.call(rbind.data.frame, buoyMeta) # list to data frame
    colnames(buoyMeta) <- metaHeaders
    # Missing data in the Realtime files are denoted by "MM"
    buoyMeta[buoyMeta == ""] <- NA
    
    # We only need the station ID and name
    subMeta = data.frame(STATION_ID = buoyMeta$STATION_ID, NAME = buoyMeta$NAME)
    subMeta$STATION_ID = toupper(subMeta$STATION_ID)
    
    # Add buoy names to the observation data frame using the station IDs
    mergedBuoy = merge(buoyObs, subMeta, by.x = "STN", by.y = "STATION_ID")
    
    # Return a subset of buoys or all buoys?
    if(is.null(bbox)){
      buoys <- mergedBuoy
      
    } else {
      # bbox: c('xmin','ymin','xmax','ymax')
      buoys <- mergedBuoy[as.numeric(mergedBuoy$LON) >= bbox[1] & 
                            as.numeric(mergedBuoy$LON) <= bbox[2] & 
                            as.numeric(mergedBuoy$LAT) >= bbox[3] & 
                            as.numeric(mergedBuoy$LAT) <= bbox[4], ]
    }

  } else {
    stop("collectBuoyData() buoy URL or Metadata URL does not exist") 
  }
  
  return(buoys)
}
##########################################################################
##########################################################################
#' Parse and format realtime NDBC buoy data into a geojson format
#' 
#' @param dat data.frame of observations from one buoy. See collectBuoyData() output
#' @return string of formatted buoy observations
# ------------------------------------------------------------------------
parseBuoyData = function(dat){
  obs = paste0(
    "<strong>Location:</strong> ", coordinate_hemi(as.numeric(dat$LAT)), 
    " ", coordinate_hemi(as.numeric(dat$LON), "lon"), "<br />",
    "<strong>Wind direction:</strong> from ", cardinal_direction(as.numeric(dat$WDIR)), " (",
    dat$WDIR, " &deg;)<br />", 
    "<strong>Wind speed:</strong> ", round(conv_unit(as.numeric(dat$WSPD), from="m_per_sec", to="mph"),1),
    " mph (", dat$WSPD, " m/s)<br />",
    "<strong>Peak 5 or 8 second gust speed:</strong> ", round(conv_unit(as.numeric(dat$GST), from="m_per_sec", to="mph"),1),
    " mph (", dat$GST, " m/s)<br />",
    "<strong>Significant wave height:</strong> ", round(conv_unit(as.numeric(dat$WVHT), from="m", to="ft"),1),
    " ft (", dat$WVHT, " m)<br />",
    "<strong>Dominant wave period:</strong> ", dat$DPD, " s<br />",
    "<strong>Average Wave Period:</strong> ", dat$APD, " s<br />",
    "<strong>Mean Wave Direction:</strong> from ", cardinal_direction(as.numeric(dat$MWD[748])), " (",
    dat$MWD[748], " &deg;)<br />",
    "<strong>Sea level pressure:</strong> ", round(conv_unit(as.numeric(dat$PRES), from="hPa", to="mbar"),1),
    " mbar (", dat$PRES, " hPa)<br />",
    "<strong>Pressure Tendency:</strong> ", round(conv_unit(as.numeric(dat$PTDY), from="hPa", to="mbar"),1),
    " mbar (", dat$PTDY, " hPa)<br />",
    "<strong>Air temperature:</strong> ", round(conv_unit(as.numeric(dat$ATMP), from="C", to="F"),1),
    " F (", dat$ATMP, " C)<br />",
    "<strong>Sea surface temperature:</strong> ", round(conv_unit(as.numeric(dat$WTMP), from="C", to="F"),1),
    " F (", dat$WTMP, " C)<br />",
    "<strong>Dewpoint temperature:</strong> ", round(conv_unit(as.numeric(dat$DEWP), from="C", to="F"),1),
    " F (", dat$DEWP, " C)<br />",
    "<strong>Station visibility:</strong> ", dat$VIS, " nmi (", 
    round(conv_unit(as.numeric(dat$VIS), from="naut_mi", to="km"),1), " km)<br />",
    "<strong>Water level:</strong> ", dat$TIDE, " ft (", 
    round(conv_unit(as.numeric(dat$TIDE), from="ft", to="m"),1), " m) MLLW<br />")
  
  # Convert date and time to an R object
  datetime = as.POSIXlt(paste0(dat$MM, " ", dat$DD, ", ", dat$YYYY, " ", dat$hh, ":", dat$mm),
                        format="%M %d, %Y %H:%M")
  
  # Ensure all stations have a name even if it is the station ID
  if(is.na(dat$NAME)){
    name = dat$STN
  } else {
    name = gsub('\\"', "'", dat$NAME) #Make sure to substitute any quotes in names
  }
  
  # Combine all buoy info into one string on geojson format
  jsFormat = paste0('{"type": "Feature", "properties": {', '"name": "', name, '", "id": "', dat$STN, 
                   '", "url": "https://www.ndbc.noaa.gov/station_page.php?station=', dat$STN,
                   '", "obs": "', obs, '", "time": "Last updated on', format(datetime, "%b %d, %Y %I:%M %p"), '"},',
                   ' "geometry": {"type": "Point", "coordinates": [', as.numeric(dat$LON), ',', 
                   as.numeric(dat$LAT), ']}}')
  
  return(jsFormat)
}

buoy_data = collectBuoyData()

formatBuoys = lapply(X=1:nrow(buoy_data), function(X){parseBuoyData(buoy_data[X,])})
buoyDF = do.call(rbind.data.frame, formatBuoys) # list to data frame
buoyString = paste(buoyDF[,1], collapse=",")

# Create a geojson object with the observation and statement info and merge into a
# specific file format with Buoys as the variable name.
json_merge = paste0('Buoys = {"type": "FeatureCollection","features": [', 
                    buoyString, ']};')

# testjson <- gsub(pattern='\\\\"', '\"', json_merge)

# Export data to geojson.
cat(json_merge, file="buoys_test.js")
 
bbox = c(-82.0, -73.0, 36.46, 43.75)
collectTideIDs(bbox)

# GET -> httr
# xmlToList -> XML
library(data.table)

forecast <- "ofs_water_level"



metaDat, vars = c("air_temperature", "air_pressure", 
                  "visibility", "humidity", "wind", 
                  "water_level", "water_temperature", 
                  "conductivity", "salinity"),
units = "english", tz = "lst", dates = "latest",
datum=NULL

##########################################################################
##########################################################################
# Function extracting tide data (hight and time) from a XML file online.
waterheight_plot <- function(url, weekMidnights, weekNoons, plotW, plotH, plotOut){
  #################
  #url <- tideURLs[1]
  #weekMidnights <- day_midnight
  #weekNoons <- day_noon
  #plotW <- p.width
  #plotH <- p.height
  #plotOut <- plotDir
  #################
  
  p.width <- 4            # Width
  p.height <- 2.5           # Height
  # Determine midnight and noon for dates of this previous week
  day <- -5:7
  day_midnight <- as.POSIXct(paste0(Sys.Date() - day, "00:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  day_noon <- as.POSIXct(paste0(Sys.Date() - day, "12:00"), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  
  
  tideStations = read.table("NOAAtideIDs.txt", header=TRUE)
  dat = tideStations[1,]
  
  watLev = collectTideData(dat, vars="water_level", dates="recent")
  forecast = collectTideData(dat, vars="ofs_water_level", begin_date=format(Sys.Date(), "%Y%m%d"), range=72)
  
  watLev$Date = as.POSIXct(watLev$t, format="%Y-%m-%d %H:%M", tz="")
  forecast$Date = as.POSIXct(forecast$t, format="%Y-%m-%d %H:%M", tz="")
  
  timernge = range(c(watLev$Date, forecast$Date), na.rm = TRUE)
  valrnge = range(c(as.numeric(watLev$v), as.numeric(forecast$v)), na.rm = TRUE) 
  
  plot(0, type="n", ylab=paste0("Height (ft ", datum, ")"), 
       xlab="Past 3 days (LST)", xaxt="n", xlim = timernge, ylim = valrnge) #klr changed m to ft
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
  axis(1, at=day_midnight, labels=FALSE, tick=TRUE)
  axis(1, at=day_noon, labels=gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick=FALSE)
  grid(NA, NULL, lty=6, col="gray")
  abline(v=day_midnight, lty=6, col="gray")
  lines(watLev$Date, as.numeric(watLev$v), lwd=2, col="steelblue")
  lines(forecast$Date, as.numeric(forecast$v), lwd=2, lty=2, col="red")
  abline(v=watLev$Date[length(watLev$Date)], lwd=2, col="black")
  
  
  plot(watLev$Date, as.numeric(watLev$v), ylab=paste0("Height (ft ", datum, ")"), xlab="Past 3 days", xaxt="n") #klr changed m to ft
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
  axis(1, at=day_midnight, labels=FALSE, tick=TRUE)
  axis(1, at=day_noon, labels=gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick=FALSE)
  grid(NA, NULL, lty=6, col="gray")
  abline(v=day_midnight, lty=6, col="gray")
  lines(stationData$time, as.numeric(as.character(stationData$values)), lwd=2, col="steelblue")
  
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
    
    plot(stationData$time, as.numeric(as.character(stationData$values)), type="n", ylab=paste0("Height (ft ", datum, ")"), xlab="Past 3 days", xaxt="n") #klr changed m to ft
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    axis(1, at=weekMidnights, labels=FALSE, tick=TRUE)
    axis(1, at=weekNoons, labels=gsub("0(\\d)", "\\1", format(weekNoons, "%m/%d")), tick=FALSE)
    grid(NA, NULL, lty=6, col="gray")
    abline(v=weekMidnights, lty=6, col="gray")
    lines(stationData$time, as.numeric(as.character(stationData$values)), lwd=2, col="steelblue")
  }
  dev.off()
}
##########################################################################
##########################################################################
#' Download NOAA Tide Station data
#' 
#' @param metaDat data.frame of metadata from one tide station. See collectTideIDs() output
#' @param vars Vector of data products to download. Default: c("air_temperature", 
#'   "air_pressure", "visibility", "humidity", "wind", "water_level", "water_temperature", 
#'   "conductivity", "salinity")
#' @param units String to return standard (english) or metric units. Default: "english"
#' @param tz String designating timezone. Default: "lst"; local Standard Time, 
#'   not corrected for Daylight Saving Time, local to the requested station.
#'   Great Lakes stations must use "lst".
#' @param dates String to specify the time of observations to return. Options:
#'  "today": Retrieves data for today
#'  "latest": Retrieves the last data point available within the last 18 min
#'  "recent": Retrieves the last 3 days of data
#'  Default: "latest"
#' @param datum Reference point for water level data. Default: NULL; Nautical 
#'   Chart Datum for all U.S. coastal waters (MLLW) or the Great Lakes (LWD)
#' @return data.frame of tide station observations
#
# Additional information on inputs can be found on the CO-OP API at:
# https://api.tidesandcurrents.noaa.gov/api/prod/
# ------------------------------------------------------------------------
tideStations = read.table("NOAAtideIDs.txt", header=TRUE)
dat = tideStations[2,]
collectTideData = function(metaDat, vars = c("air_temperature", "air_pressure", 
                                             "visibility", "humidity", "wind", 
                                             "water_level", "water_temperature", 
                                             "conductivity", "salinity"),
                           units = "english", tz = "lst", dates = "latest",
                           datum=NULL){
  
  # Datum is mandatory for all water level products to correct the data to the reference point desired.
  if(is.null(datum)){
    if(noquote(dat$greatlakes)){
      datum <- "LWD" # Great Lakes Low Water Datum (Nautical Chart Datum for the Great Lakes).
    } else {
      datum <- "MLLW" # Mean Lower Low Water (Nautical Chart Datum for all U.S. coastal waters)
    }
  }
  
  # Generate a URL for each variable based on function input
  varsURL = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?date=",dates, 
                   "&station=", dat$id, "&product=", vars, "&datum=", datum, "&time_zone=", 
                   tz, "&units=", units, "&format=xml")
  
  # Download the observations for each variable
  obsList <- lapply(varsURL, function(URL){
    station_data = xmlToList(rawToChar(GET(URL)$content))
    
    # If the station doesn't record the variable an error will appear.
    # Report missing variables as NA
    if(any(names(station_data) == "error")){
      df = as.data.frame(t(as.data.frame(station_data$metadata)))
      df$t = NA
      df$v = NA
      
    } else { # Read the observations
      df = cbind(t(as.data.frame(station_data$metadata)), t(as.data.frame(station_data$observations[[1]])))
      df = as.data.frame(df)
    }
    return(df)
  })
  
  # Record the variable names
  names(obsList) = vars
  
  # Merge the list of data frames filling in missing columns
  tide_df <- rbindlist(obsList, fill = TRUE, idcol=TRUE)
  colnames(tide_df)[1] = "var" # change from ".id"
  
  return(tide_df)
}

##########################################################################
##########################################################################
#' Parse and format realtime NOAA tide station data into a geojson format
#' 
#' @param tide_df data.frame of observations from one tide station. See collectTideData() output
#' @return string of formatted tide station observations
# ------------------------------------------------------------------------
parseTideData = function(tide_df){
  obs = paste0(
    "<strong>Location:</strong> ", coordinate_hemi(as.numeric(tide_df$lat[1])), 
    " ", coordinate_hemi(as.numeric(tide_df$lon[1]), "lon"), "<br />",
    "<strong>Air temperature:</strong> ", tide_df$v[tide_df$var == "air_temperature"],
    " F (", round(conv_unit(as.numeric(tide_df$v[tide_df$var == "air_temperature"]), from="F", to="C"),1), " C)<br />",
    "<strong>Barometric pressure:</strong> ", tide_df$v[tide_df$var == "air_pressure"],
    " mbar (", round(conv_unit(as.numeric(tide_df$v[tide_df$var == "air_pressure"]), from="mbar", to="hPa"),1), " hPa)<br />",
    "<strong>Visibility:</strong> ", tide_df$v[tide_df$var == "visibility"], " nmi (", 
    round(conv_unit(as.numeric(tide_df$v[tide_df$var == "visibility"]), from="naut_mi", to="km"),1), " km)<br />",
    "<strong>Relative humidity:</strong> ", tide_df$v[tide_df$var == "humidity"], " % <br />",
    "<strong>Wind:</strong> from ", tide_df$dr[tide_df$var == "wind"], " (",
    tide_df$d[tide_df$var == "wind"], " &deg;) at ", 
    round(conv_unit(as.numeric(tide_df$s[tide_df$var == "wind"]), from="knot", to="mph"),1), "mph (",
    round(conv_unit(as.numeric(tide_df$s[tide_df$var == "wind"]), from="knot", to="m_per_sec"),1), " m/s)<br />",
    "<strong>Gusting to:</strong> ", round(conv_unit(as.numeric(tide_df$g[tide_df$var == "wind"]), from="knot", to="mph"),1), "mph (",
    round(conv_unit(as.numeric(tide_df$g[tide_df$var == "wind"]), from="knot", to="m_per_sec"),1), " m/s)<br />",
    "<strong>Water level:</strong> ", tide_df$v[tide_df$var == "water_level"], " ft (", 
    round(conv_unit(as.numeric(tide_df$v[tide_df$var == "water_level"]), from="ft", to="m"),1), " m) ", datum, "<br />",
    "<strong>Water temperature:</strong> ", tide_df$v[tide_df$var == "water_temperature"],
    " F (", round(conv_unit(as.numeric(tide_df$v[tide_df$var == "water_temperature"]), from="F", to="C"),1), " C)<br />",
    "<strong>Conductivity:</strong> ", tide_df$v[tide_df$var == "conductivity"], " mS/cm <br />",
    "<strong>Salinity:</strong> ", tide_df$v[tide_df$var == "salinity"], " PSU <br />"
  )
  
  # Convert date and time to an R object
  times = unique(tide_df$t)
  datetime = as.POSIXlt(times[!is.na(times)], format="%Y-%M-%d %H:%M")
  formatTime = format(datetime, "%b %d, %Y %I:%M")
  
  # Combine all tide info into one string in geojson format
  jsFormat = paste0('{"type": "Feature", "properties": {', '"name": "', unique(tide_df$name), '", "id": "', unique(tide_df$id), 
                    '", "url": "https://www.ndbc.noaa.gov/station_page.php?station=', unique(tide_df$id),
                    '", "obs": "', obs, '", "time": "Last updated on ', formatTime, ' LST"},',
                    ' "geometry": {"type": "Point", "coordinates": [', as.numeric(unique(tide_df$lon)), ',', 
                    as.numeric(unique(tide_df$lat)), ']}}')
  
  return(jsFormat)
}




# Read the xml file and convert the raw content to a list


# Parse into an R structure representing XML tree
station_xml <- xmlParse(station_data)





# Convert the parsed XML to a dataframe
tideStation_df <- xmlToDataFrame(nodes=getNodeSet(station_xml, "//observations"))

xml_data <- retry(xmlToList(rawToChar(GET(varURL)$content)))
var <- sapply(strsplit(varURL, "product=|&datum="), "[[", 2)


  getVarVals <- lapply(varURLs, collectLatestTidal)
  tableVars <- cbind.data.frame(vars, do.call(rbind.data.frame, getVarVals))
  colnames(tableVars)[2:ncol(tableVars)] <- c("value", "date", "time", "metaID", "metaName", "metaLat", "metaLon") 


##########################################################################
##########################################################################
collectBuoyData = function(buoys_ids, US_buoys){
  #################
  #buoys_ids <- NDBC_buoys$ID
  #buoys_ids <- NDBC_stations$ID
  #buoys_ids <- non_NDBC_stations$ID
  #US_buoys <- US_buoys
  #################
  #print(buoys_ids)
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
    dateBases <- mapply(function(tim, zon){reForm<-as.POSIXlt(tim, format="%b %d, %Y %I:%M %p", tz="EST5EDT")
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