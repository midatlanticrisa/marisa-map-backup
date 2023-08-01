##header

# Buoys
library(RCurl)
library(measurements)
# Tide stations
library(xml2)
library(httr)
library(XML)
library(data.table)
# Alerts
library(jsonlite)
library(terra)
library(sf)
library(geojsonsf)
# Weather observations
# Stream gauges
library(geojsonio)

##########################################################################
##########################################################################
# Transparent Color Function 
makeTransparent <- function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor,2 ,
        function(curcoldata)
        {rgb(red=curcoldata[1],
             green=curcoldata[2],
             blue=curcoldata[3], alpha=alpha,
             maxColorValue=255)})
}

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
      # mergedBuoy is a factor. The values must be converted to a character than numeric before evaluating
      buoys <- mergedBuoy[as.numeric(as.character(mergedBuoy$LON)) >= bbox[1] & 
                            as.numeric(as.character(mergedBuoy$LON)) <= bbox[2] & 
                            as.numeric(as.character(mergedBuoy$LAT)) >= bbox[3] & 
                            as.numeric(as.character(mergedBuoy$LAT)) <= bbox[4], ]
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
  # Format the observations
  obs = paste0(
    "<strong>Location:</strong> ", coordinate_hemi(as.numeric(as.character(dat$LAT))), 
    " ", coordinate_hemi(as.numeric(as.character(dat$LON)), "lon"), "<br />",
    "<strong>Wind direction:</strong> from ", cardinal_direction(as.numeric(as.character(dat$WDIR))), " (",
    dat$WDIR, " &deg;)<br />", 
    "<strong>Wind speed:</strong> ", round(conv_unit(as.numeric(as.character(dat$WSPD)), from="m_per_sec", to="mph"),1),
    " mph (", dat$WSPD, " m/s)<br />",
    "<strong>Peak 5 or 8 second gust speed:</strong> ", round(conv_unit(as.numeric(as.character(dat$GST)), from="m_per_sec", to="mph"),1),
    " mph (", dat$GST, " m/s)<br />",
    "<strong>Significant wave height:</strong> ", round(conv_unit(as.numeric(as.character(dat$WVHT)), from="m", to="ft"),1),
    " ft (", dat$WVHT, " m)<br />",
    "<strong>Dominant wave period:</strong> ", dat$DPD, " s<br />",
    "<strong>Average Wave Period:</strong> ", dat$APD, " s<br />",
    "<strong>Mean Wave Direction:</strong> from ", cardinal_direction(as.numeric(as.character(dat$MWD))), " (",
    dat$MWD, " &deg;)<br />",
    "<strong>Sea level pressure:</strong> ", round(conv_unit(as.numeric(as.character(dat$PRES)), from="hPa", to="mbar"),1),
    " mbar (", dat$PRES, " hPa)<br />",
    "<strong>Pressure Tendency:</strong> ", round(conv_unit(as.numeric(as.character(dat$PTDY)), from="hPa", to="mbar"),1),
    " mbar (", dat$PTDY, " hPa)<br />",
    "<strong>Air temperature:</strong> ", round(conv_unit(as.numeric(as.character(dat$ATMP)), from="C", to="F"),1),
    " F (", dat$ATMP, " C)<br />",
    "<strong>Sea surface temperature:</strong> ", round(conv_unit(as.numeric(as.character(dat$WTMP)), from="C", to="F"),1),
    " F (", dat$WTMP, " C)<br />",
    "<strong>Dewpoint temperature:</strong> ", round(conv_unit(as.numeric(as.character(dat$DEWP)), from="C", to="F"),1),
    " F (", dat$DEWP, " C)<br />",
    "<strong>Station visibility:</strong> ", dat$VIS, " nmi (", 
    round(conv_unit(as.numeric(as.character(dat$VIS)), from="naut_mi", to="km"),1), " km)<br />",
    "<strong>Water level:</strong> ", dat$TIDE, " ft (", 
    round(conv_unit(as.numeric(as.character(dat$TIDE)), from="ft", to="m"),1), " m) MLLW<br />")
  
  # Convert date and time to an R object
  datetime = as.POSIXlt(paste0(dat$MM, " ", dat$DD, ", ", dat$YYYY, " ", dat$hh, ":", dat$mm),
                        format="%M %d, %Y %H:%M")
  
  # Ensure all stations have a name even if it is the station ID
  if(is.na(dat$NAME)){
    name = dat$STN
  } else {
    name = gsub('\\"', "'", dat$NAME) #Make sure to substitute any quotes in names
  }
  
  # Combine all buoy info into one string in geojson format
  jsFormat = paste0('{"type": "Feature", "properties": {', '"name": "', name, '", "id": "', dat$STN, 
                    '", "url": "https://www.ndbc.noaa.gov/station_page.php?station=', dat$STN,
                    '", "obs": "', obs, '", "time": "Last updated on', format(datetime, "%b %d, %Y %I:%M %p"), '"},',
                    ' "geometry": {"type": "Point", "coordinates": [', as.numeric(as.character(dat$LON)), ',', 
                    as.numeric(as.character(dat$LAT)), ']}}')
  
  return(jsFormat)
}

##########################################################################
##########################################################################
#' Download NOAA Tide Station Metadata (e.g., IDs) for active water level stations 
#' 
#' @param filenm name of file to save tide station metadata to. Default: "NOAAtideIDs.txt"
#' @param bbox Boundary extent for tide stations to return. 
#'   Format = c('xmin','ymin','xmax','ymax'). Default: NULL; all tide stations
#' @param returnIDs logical. Determines whether to return the data.frame or just 
#'   save the data.frame as a text file. Default: FALSE; only text file saved.
#' @return data.frame of tide station metadata for the region of interest.
#
# The NOAA CO-OPS Metadata API (MDAPI) can be used to retrieve information about 
# CO-OPS' stations. A request can be made to return information about a specific 
# station, or information about multiple stations can be returned. The types of 
# information accessible via the API are listed in detail at:
# https://api.tidesandcurrents.noaa.gov/mdapi/prod/
# ------------------------------------------------------------------------
# Example of parsing XML data:
# https://medium.com/geekculture/reading-xml-files-in-r-3122c3a2a8d9
# ------------------------------------------------------------------------
collectTideIDs = function(filenm="NOAAtideIDs.txt", bbox=NULL, returnIDs=FALSE){
  
  # Active water level stations
  tideStationURL <- "https://api.tidesandcurrents.noaa.gov/mdapi/prod/webapi/stations.xml?type=waterlevels"
  
  # Read the xml file
  station_data = read_xml(tideStationURL)
  
  # Parse into an R structure representing XML tree
  station_xml <- xmlParse(station_data)
  
  # Convert the parsed XML to a dataframe
  tideStation_df <- xmlToDataFrame(nodes=getNodeSet(station_xml, "//Station"))
  
  # Save only the information we need: id, name, lat, lon
  tideStation_df <- data.frame(id = tideStation_df$id, name = tideStation_df$name, 
                               lat = tideStation_df$lat, lon = tideStation_df$lng,
                               greatlakes = tideStation_df$greatlakes)
  
  # Return a subset of tide stations or all tide stations?
  if(is.null(bbox)){
    tideStations <- tideStation_df
    
  } else {
    # bbox: c('xmin','ymin','xmax','ymax')
    tideStations <- tideStation_df[as.numeric(as.character(tideStation_df$lon)) >= bbox[1] & 
                                     as.numeric(as.character(tideStation_df$lon)) <= bbox[2] & 
                                     as.numeric(as.character(tideStation_df$lat)) >= bbox[3] & 
                                     as.numeric(as.character(tideStation_df$lat)) <= bbox[4], ]
  }
  
  # Export data to geojson.
  write.table(tideStations, file=filenm, row.names=FALSE)
  
  if(returnIDs){
    return(tideStations)
  }
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
#'   Chart Datum for all U.S. coastal waters (MLLW) or the International Great 
#'   Lakes Datum (IGLD)
#' @param begin_date String to specify a starting date (YYYYMMDD) to return 
#'   observations. Default: NULL. Only used when downloading "ofs_water_level".
#' @param range String to specify the number of hours after the starting date 
#'   (begin_date) to return observations. Default: 48 hours. Only used when 
#'   downloading "ofs_water_level".
#' @return data.frame of tide station observations
#
# Additional information on inputs can be found on the CO-OP API at:
# https://api.tidesandcurrents.noaa.gov/api/prod/
# ------------------------------------------------------------------------
collectTideData = function(metaDat, vars = c("air_temperature", "air_pressure", 
                                             "visibility", "humidity", "wind", 
                                             "water_level", "water_temperature", 
                                             "conductivity", "salinity"),
                           units = "english", tz = "lst", dates = "latest",
                           datum=NULL, begin_date=NULL, range=48){
  
  # Datum is mandatory for all water level products to correct the data to the reference point desired.
  is.greatlakes <- noquote(as.character(metaDat$greatlakes))
  if(is.null(datum)){
    if(is.greatlakes){
      datum <- "IGLD" # International Great Lakes Datum.
    } else {
      datum <- "MLLW" # Mean Lower Low Water (Nautical Chart Datum for all U.S. coastal waters)
    }
  }
  
  # Generate a URL for each variable based on function input
  if(length(vars) == 1 && vars == "ofs_water_level"){
    varsURL = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?begin_date=",begin_date, 
                     "&range=", range, "&station=", metaDat$id, "&product=", vars, "&datum=", datum, "&time_zone=", 
                     tz, "&units=", units, "&format=xml")
  } else {
    varsURL = paste0("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?date=",dates, 
                     "&station=", metaDat$id, "&product=", vars, "&datum=", datum, "&time_zone=", 
                     tz, "&units=", units, "&format=xml")
  }
  
  # Download the observations for each variable
  obsList <- lapply(varsURL, function(URL){
    station_data = xmlToList(rawToChar(GET(URL)$content))
    
    # If the station doesn't record the variable an error will appear.
    # Report missing variables as NA
    if(any(names(station_data) == "error")){
      df = as.data.frame(t(as.data.frame(station_data$metadata)))
      df$t = NA
      df$v = NA
      
    } else { # Read the observations. 
      if(length(station_data$observations) > 1){
        stationdf = do.call(rbind, station_data$observations)
        # Suppress row.names to stop warnings messages of duplicate row.names
        # Names will be recorded in the list
        df = cbind(t(as.data.frame(station_data$metadata)), as.data.frame(stationdf),
                   row.names = NULL)
        
      } else {
        df = cbind(t(as.data.frame(station_data$metadata)), 
                   t(as.data.frame(station_data$observations[[1]])), 
                   row.names = NULL)
        df = as.data.frame(df)
      }
    }
    return(df)
  })
  
  # Record the variable names
  names(obsList) = vars
  
  # Merge the list of data frames filling in missing columns
  tide_df <- rbindlist(obsList, fill = TRUE, idcol=TRUE)
  colnames(tide_df)[1] = "var" # change from ".id"
  tide_df$datum <- datum
  
  return(tide_df)
}

##########################################################################
##########################################################################
#' Parse and format NOAA tide station data into a geojson format
#' 
#' @param tide_df data.frame of observations from one tide station. See 
#'   collectTideData() output
#' @return string of formatted tide station observations
# ------------------------------------------------------------------------
parseTideData = function(tide_df){
  # Format the observations
  obs = paste0(
    "<strong>Location:</strong> ", coordinate_hemi(as.numeric(as.character(tide_df$lat[1]))), 
    " ", coordinate_hemi(as.numeric(as.character(tide_df$lon[1])), "lon"), "<br />",
    "<strong>Air temperature:</strong> ", tide_df$v[tide_df$var == "air_temperature"],
    " F (", round(conv_unit(as.numeric(as.character(tide_df$v[tide_df$var == "air_temperature"])), from="F", to="C"),1), " C)<br />",
    "<strong>Barometric pressure:</strong> ", tide_df$v[tide_df$var == "air_pressure"],
    " mbar (", round(conv_unit(as.numeric(as.character(tide_df$v[tide_df$var == "air_pressure"])), from="mbar", to="hPa"),1), " hPa)<br />",
    "<strong>Visibility:</strong> ", tide_df$v[tide_df$var == "visibility"], " nmi (", 
    round(conv_unit(as.numeric(as.character(tide_df$v[tide_df$var == "visibility"])), from="naut_mi", to="km"),1), " km)<br />",
    "<strong>Relative humidity:</strong> ", tide_df$v[tide_df$var == "humidity"], " % <br />",
    "<strong>Wind:</strong> from ", tide_df$dr[tide_df$var == "wind"], " (",
    tide_df$d[tide_df$var == "wind"], " &deg;) at ", 
    round(conv_unit(as.numeric(as.character(tide_df$s[tide_df$var == "wind"])), from="knot", to="mph"),1), "mph (",
    round(conv_unit(as.numeric(as.character(tide_df$s[tide_df$var == "wind"])), from="knot", to="m_per_sec"),1), " m/s)<br />",
    "<strong>Gusting to:</strong> ", round(conv_unit(as.numeric(as.character(tide_df$g[tide_df$var == "wind"])), from="knot", to="mph"),1), "mph (",
    round(conv_unit(as.numeric(as.character(tide_df$g[tide_df$var == "wind"])), from="knot", to="m_per_sec"),1), " m/s)<br />",
    "<strong>Water level:</strong> ", tide_df$v[tide_df$var == "water_level"], " ft (", 
    round(conv_unit(as.numeric(as.character(tide_df$v[tide_df$var == "water_level"])), from="ft", to="m"),1), " m) ", tide_df$datum, "<br />",
    "<strong>Water temperature:</strong> ", tide_df$v[tide_df$var == "water_temperature"],
    " F (", round(conv_unit(as.numeric(as.character(tide_df$v[tide_df$var == "water_temperature"])), from="F", to="C"),1), " C)<br />",
    "<strong>Conductivity:</strong> ", tide_df$v[tide_df$var == "conductivity"], " mS/cm <br />",
    "<strong>Salinity:</strong> ", tide_df$v[tide_df$var == "salinity"], " PSU <br />"
  )
  
  # Convert date and time to an R object
  times = unique(tide_df$t)
  datetime = as.POSIXlt(times[!is.na(times)], format="%Y-%M-%d %H:%M")
  formatTime = format(datetime, "%b %d, %Y %I:%M")
  
  # Combine all tide info into one string in geojson format
  jsFormat = paste0('{"type": "Feature", "properties": {', '"name": "', unique(tide_df$name), '", "id": "', unique(tide_df$id), 
                    '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', unique(tide_df$id),
                    '", "obs": "', obs, '", "time": "Last updated on ', formatTime, 
                    ' LST", "image": "https://download.clima.psu.edu/rtdatamap/Tide_figs/Fig_"', unique(tide_df$id), '.png"},',
                    ' "geometry": {"type": "Point", "coordinates": [', as.numeric(as.character(unique(tide_df$lon))), ',', 
                    as.numeric(as.character(unique(tide_df$lat))), ']}}')
  
  return(jsFormat)
}

##########################################################################
##########################################################################
#' Download and Plot recent and forecast NOAA Tide Station water levels
#' 
#' @param metaDat data.frame of metadata from one tide station. See collectTideIDs() output
#' @param p.width Plot width in inches. Default: 4
#' @param p.height Plot height in inches. Default: 2.5
#' @param p.dir Output directory for plots to be saved in.
#' @return plot saved in p.dir as Fig_<station ID>.png
#
# Additional information on inputs can be found on the CO-OP API at:
# https://api.tidesandcurrents.noaa.gov/api/prod/
# ------------------------------------------------------------------------

tides_plot <- function(metaDat, p.width = 4, p.height = 2.5, p.dir, datum=NULL){
  
  # Determine midnight and noon for dates of this previous week
  day <- -5:7
  day_midnight <- as.POSIXct(paste0(Sys.Date() - day, "00:00"), 
                             format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  day_noon <- as.POSIXct(paste0(Sys.Date() - day, "12:00"), 
                         format = "%Y-%m-%d %H:%M", tz = "America/New_York")
  
  # Download the recent and forecast water levels
  watLev = collectTideData(metaDat, vars="water_level", dates="recent")
  forecast = collectTideData(metaDat, vars="ofs_water_level", 
                             begin_date=format(Sys.Date(), "%Y%m%d"), 
                             range=72)
  
  ##create plot
  png(file=paste0(p.dir, "Fig_", metaDat$id, ".png"), family="Helvetica", units="in", 
      width=p.width, height=p.height, pointsize=12, res=300)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
  
  if(length(watLev$v) == 1 && is.na(watLev$v)){ # If no data
    plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    legend("center", "No data available", bg="white")
    
  } else { # if data
    # Format the date/time into an R object
    watLev$Date = as.POSIXct(watLev$t, format="%Y-%m-%d %H:%M", tz="")
    forecast$Date = as.POSIXct(forecast$t, format="%Y-%m-%d %H:%M", tz="")
    
    # Remove all forecast values prior to the present time frame
    lastObsTime <- watLev$Date[length(watLev$Date)]
    forecast <- forecast[-which(as.POSIXct(as.character(forecast$t)) < lastObsTime), ]
    
    # Find the range of both the time and values for plotting axis limits
    timernge = range(c(watLev$Date, forecast$Date), na.rm = TRUE)
    valrnge = range(c(as.numeric(as.character(watLev$v)), 
                      as.numeric(as.character(forecast$v))), na.rm = TRUE) 
    
    plot(0, type="n", ylab=paste0("Height (ft ", unique(watLev$datum), ")"), 
         xlab="Local standard time", xaxt="n", xlim = timernge, ylim = valrnge) #klr changed m to ft
    
    # Add some nice gridding
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    axis(1, at=day_midnight, labels=FALSE, tick=TRUE)
    axis(1, at=day_noon, labels=gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick=FALSE)
    grid(NA, NULL, lty=6, col="gray")
    abline(v=day_midnight, lty=6, col="gray")
    
    # Add the water level values and forecast line
    lines(watLev$Date, as.numeric(as.character(watLev$v)), lwd=2, col="steelblue")
    lines(forecast$Date, as.numeric(as.character(forecast$v)), lwd=2, lty=2, col="steelblue")
    abline(v=watLev$Date[length(watLev$Date)], lwd=2, col="black")
    
  }
  dev.off()
}

##########################################################################
##########################################################################
#' Collect warnings, watches, and advisories from NOAA NWS API:
#'   https://www.weather.gov/documentation/services-web-api#/default/alerts_types
#'   Future updates to this function may come from downloading the alerts via the
#'   NOAAPORT push method rather than the NWS API pull method. The push method
#'   is typically available less than 45 seconds from creation. The pull method
#'   is only as fast as the client is configured to pull it - every 2, 5, 10, 
#'   even 15 minutes. Alerts like Tornado Warnings are extremely time sensitive.
#' 
#' @param area Vector of State abbreviations to pull. Default: NULL; will 
#'   download all alerts
#' @param colorfile CSV table connecting alert type with map color
#' @param cntyShp path/file name to shapefile of US county boundaries. 
#'   See https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
#' @param coastalShp path/file name to shapefile of NOAA coastal marine and 
#'   great lakes zone boundaries. See https://www.weather.gov/gis/MarineZones
#' @param offshoreShp path/file name to shapefile of NOAA offshore marine zone 
#'   boundaries. See https://www.weather.gov/gis/MarineZones
#' @param outfile path/file name of geojson file of alerts
#' @return geojson file of alerts at outfile
# ------------------------------------------------------------------------
collectWarningsAlerts = function(area = NULL, colorfile, cntyShp, coastalShp, 
                                 offshoreShp, outfile){
  # Read color/event table
  colfle <- read.csv(colorfile, skip=2, header=TRUE)
  
  if(is.null(area)){
    weatherURL <- "https://api.weather.gov/alerts/active"
  } else {
    weatherURL <- paste0("https://api.weather.gov/alerts/active?area=", 
                         paste(area, collapse=","))
  }
  
  # https://stackoverflow.com/questions/68784976/reading-in-a-geojson-file-with-geojsonio-geojsonr
  alerts <- fromJSON(weatherURL)
  features <- alerts$features
  
  ## TO DO - ADD OPTION IF THERE ARE NO ALERTS ##
  
  ## Read and extract the US state
  # "cb_2018_us_state_500k" is the 2018 spatial census data 
  # (https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html), 
  # which is used for general reference of US counties unless a higher detailed product is 
  # required. It is also the layer in which was used to create the "drop" maps in the 
  # header of the Outlooks.
  usStates <- vect(cntyShp)
  
  # convert the coordinate system to WGS84 for consistency with web mapping
  crs(usStates) <- "EPSG:4326"
  fullFips <- paste0("0", usStates$STATEFP, usStates$COUNTYFP)
  
  # https://www.weather.gov/gis/AWIPSShapefiles
  usCoast <- vect(coastalShp)
  usOffshore <- vect(offshoreShp)
  
  # convert the coordinate system to WGS84 for consistency with web mapping
  crs(usCoast) <- "EPSG:4326"
  crs(usOffshore) <- "EPSG:4326"
  
  if(is.null(names(features$geometry))){ # NOT the case
    # appendLines = 'alerts = "No advisories, watches, or warnings."'
    # example = 'diagnostic = [{"type": "Feature","properties": {"name": "Diagnostic test", "pass": "true"}, "geometry": {"type": "Point", "coordinates": [-73.4, 36.7]}}];'
    # Find all the alerts with na geometry
    null_geom_id <- which(is.na(sapply(features$geometry, "[[", 1)))
    features$geometry = data.frame(type=rep(NA,length(features$geometry)),
                                   coordinates=I(vector('list', length(features$geometry))))
    
  } else {
    # Find all the alerts with na type geometry
    null_geom_id <- which(is.na(sapply(features$geometry$type, "[[", 1)))
  }
  
  # Replace the null geometry with county coordinates based on fips
  for(i in 1:length(null_geom_id)){
    # Use the fips code to find the geometry of the event
    cnty_ind <- which(fullFips %in% features$properties$geocode$SAME[null_geom_id[i]][[1]])
    coast_ind <- which(usCoast$ID %in% features$properties$geocode$UGC[null_geom_id[i]][[1]])
    offshore_ind <- which(usOffshore$ID %in% features$properties$geocode$UGC[null_geom_id[i]][[1]])
    
    # Dissolve boundaries to reduce the size
    if(length(cnty_ind) != 0){
      dissol <- aggregate(usStates[cnty_ind, ])
    } else if(length(coast_ind) != 0){
      dissol <- aggregate(usCoast[coast_ind, ])
    } else if(length(offshore_ind) != 0){
      dissol <- aggregate(usOffshore[offshore_ind, ])
    } else {
      stop("Error: No matching zone found.")
    }
    
    # Convert SpatVec to geojson via a sf object. This should protect multipolygons
    spatVect_sf = st_as_sf(dissol)
    geosV <- sf_geojson(spatVect_sf)
    mul <- fromJSON(geosV)
    # coords <- crds(dissol)
    # 
    # # Set the geometry to the boundary coordinates
    # dim3mat <- array(NA, dim = c(1,nrow(coords),2))
    # dim3mat[,,1] <- coords[ ,"x"]
    # dim3mat[,,2] <- coords[ ,"y"]
    features$geometry$coordinates[null_geom_id[i]][[1]] <- mul$coordinates #dim3mat
    features$geometry$type[null_geom_id[i]] = mul$type #"Polygon"
  }
  
  # Look up the map colors for each event
  event_colors <- sapply(features$properties$event, 
                         function(X){paste0("#", 
                                            colfle$Hex.Code[which(colfle$Hazard...Weather.Event == X)])})
  features$properties$color <- event_colors
  
  # Remove parameters and other properties to reduce file size.
  features$properties = subset(features$properties, select = -c(parameters,sender,senderName))
  
  write_json(features, outfile)
  
  textLines = readLines(outfile)
  # alerts = 
  appendLines = c('{"type": "FeatureCollection","features": ', textLines, '}')
  cat(appendLines, file = outfile)
  
}

##########################################################################
##########################################################################
#' Download NWS Advanced Hydrologic Prediction Service (AHPS) River Gauge 
#'   Observations
#' 
#' @param bbox Boundary extent for river gauges to return. 
#'   Format = c('xmin','ymin','xmax','ymax'). Default: NULL; all river gauges
#' @param downDir path name of directory for the data to download to.
#' @param outfile path/file name of geojson file of current observations
#' @return sf object of current observations for the region of interest.
#
# The most recent observation from all river gauges hosted on the NWS web site 
# are updated approximately every 15 minutes.
# https://water.weather.gov/ahps/download.php
# ------------------------------------------------------------------------
collectRiverData = function(bbox=NULL, downDir, outfile){
  
  # Download the current observed AHPS river gauge observations and flood stages
  # The resulting shapefile is zipped.
  # https://water.weather.gov/ahps/download.php
  download.file("https://water.weather.gov/ahps/download.php?data=tgz_obs", 
                destfile=paste0(downDir, "ahps_shp.tgz"))
  
  # Create a directory to save the zipped contents into, if the directory doesn't
  # already exist.
  zipDir = paste0(downDir, "ahps_shp/")
  if (!file.exists(zipDir)){
    dir.create(zipDir, recursive=T)
  }
  
  # Unzip the contents
  system(paste0("tar -zxvf ", downDir, "ahps_shp.tgz -C ", zipDir))
  
  # Read in the shapefile and convert the coordinate system to 
  # WGS84 for consistency with web mapping 
  ahps <- vect(paste0(zipDir, "national_shapefile_obs.shp"))
  crs(ahps) <- "EPSG:4326"
  
  # Return a subset of gauges or all gauges?
  if(is.null(bbox)){
    streams <- ahps
    
  } else {
    # bbox: c('xmin','ymin','xmax','ymax')
    streams <- ahps[ahps$Longitude >= bbox[1] & 
                      ahps$Longitude <= bbox[2] & 
                      ahps$Latitude >= bbox[3] & 
                      ahps$Latitude <= bbox[4], ]
  }
  
  # Convert SpatVec object to an sf object and save as a geojson file.
  # SpatVec objects cannot be directly converted to geojson.
  spatVect_sf <- st_as_sf(streams)
  cat(as.character(geojson_json(spatVect_sf)), file = outfile)
  
  return(spatVect_sf)
}

##########################################################################
##########################################################################
#' Download NWS Advanced Hydrologic Prediction Service (AHPS) River Gauge 
#'   Observations (past 3 days) and Forecast
#' 
#' @param GID River gauge ID
#' @return list object of observations for the past 3 days and all available 
#'   forecasts. While the forecast goes out 96 hours, it only includes 48 hours 
#'   of forecast precipitation (or 72 hours when confidence is greater on a 
#'   long-duration heavy rain event). 
#
# The most recent observation from all river gauges hosted on the NWS web site 
# are updated approximately every 15 minutes.
# https://water.weather.gov/ahps/download.php
# ------------------------------------------------------------------------
collectRiverForecast = function(GID){
  # ptm <- proc.time()
  print(GID)
  # Read the xml file
  riverURL <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=", 
                     tolower(GID), "&output=xml")
  riverData = read_xml(riverURL)
  
  # Parse into an R structure representing XML tree
  riverXml <- xmlParse(riverData)
  
  # Convert the parsed XML to a dataframe of flood stages
  stages_df = xmlToDF(riverXml, xpath = "/site/sigstages", verbose = FALSE)
  # stages_df = xmlToDataFrame(nodes=getNodeSet(riverXml, "//sigstages"))
  stage_exist <- is.null(stages_df)
  
  if(stage_exist){
    stages_df <- data.frame(action=NA, flood=NA, moderate=NA, major=NA)
  } else {
    stages_df[stages_df == ""] = NA
  }

  # Convert the parsed XML to a dataframe of observations
  obs_df <- xmlToDF(riverXml, xpath = "/site/observed/datum", verbose = FALSE)
  # obs_df <- xmlToDataFrame(nodes=getNodeSet(riverXml, "//observed//datum"))
  
  # Condition check outside the if/else
  obs_exist <- is.null(obs_df) # == 0
  obs_secondary <- any(colnames(obs_df) == "secondary")
  
  # Update the column names if data exists, otherwise set data to NA
  if(obs_exist){
    obs_df <- data.frame(time = NA, height = NA, discharge = NA)
  } else {
    if(obs_secondary){
      colnames(obs_df) <- c("time", "height", "discharge", "pedts")
    } else {
      colnames(obs_df) <- c("time", "height", "pedts")
    }
  }
  
  # Convert the parsed XML to a dataframe of forecasts
  for_df <- xmlToDF(riverXml, xpath = "/site/forecast/datum", verbose = FALSE)
  # for_df <- xmlToDataFrame(nodes=getNodeSet(riverXml, "//forecast//datum"))
  
  # Condition check outside the if/else
  for_exist <- is.null(for_df) # == 0
  for_secondary <- any(colnames(for_df) == "secondary")
  
  # Update the column names if data exists, otherwise set data to NA
  if(for_exist){
    for_df <- data.frame(time = NA, height = NA, discharge = NA)
  } else {
    if(for_secondary){
      colnames(for_df) <- c("time", "height", "discharge", "pedts")
    } else {
      colnames(for_df) <- c("time", "height", "pedts")
    }
  }
  
  output <- list(ID = GID, 
                 action = stages_df$action, 
                 minor = stages_df$flood, 
                 mod = stages_df$moderate, 
                 major = stages_df$major, 
                 obs = obs_df, forecast = for_df)
  proc.time() - ptm
  
  return(output)
}

##########################################################################
##########################################################################
#' Create a record of observations
#' 
#' @param currentObs List of current observations. Observations and observation 
#'   times must follow the format: ObsRecord[[i]]$obs and ObsRecord[[i]]$obs$time.
#' @param recordFile String of Rdata file path/name to save records to.
#' @param keep number of days to record in the past. Default: 7; keeps the past 
#'   7 days of observations.
#' @param end_date POSIXct or POSIXt date time object to determine the last time 
#'   in the range with \code{keep} to record. Default: Sys.time(); the current 
#'   date time.
#' @param tz String designating timezone.
#' @return List of station/gauge observations for the past \code{keep} number of 
#'   days saved as \code{recordFile}.
#'   
# ------------------------------------------------------------------------
recordData = function(currentObs, recordFile, keep = 7, end_date = Sys.time(), 
                      tz, return.val=FALSE){
  
  # Convert number of days to keep to seconds
  keepSec = keep*86400 # 60 secs * 60 mins * 24 hrs
  start_date = end_date - keepSec
  
  # Check if a record file already exists. If it does not, create one
  if(file.exists(recordFile)){
    
    # Load the records
    load(recordFile)
    
    # Loop through each gauge or station
    for(i in names(ObsRecord)){
      
      # Convert observation time to R time and evaluate if any dates are older
      # than the time we want to record.
      obstime = as.POSIXlt(ObsRecord[[i]]$obs$time, tz = tz, 
                           format = "%Y-%m-%d %H:%M:%S")
      ind = which(obstime < start_date)
      
      # Remove any old records 
      if(length(ind) >  0){
        ObsRecord[[i]]$obs = ObsRecord[[i]]$obs[-ind, ]
      }
      
      # Append new records
      ObsRecord[[i]]$obs = rbind(ObsRecord[[i]]$obs, currentObs[[i]]$obs)
    }
    
    # Save changes to the Rdata file
    save("ObsRecord", file = recordFile)
    
  }else{
    
    # Create a file with current observation records
    ObsRecord = currentObs
    save("ObsRecord", file = recordFile)
  }
  if(return.val){
    return(ObsRecord)
  }
}

##########################################################################
##########################################################################
#' Download and Plot recent and forecast NOAA Tide Station water levels
#' 
#' @param metaList List of metadata and observation record from one river gauge.
#' @param p.width Plot width in inches. Default: 4
#' @param p.height Plot height in inches. Default: 2.5
#' @param p.dir Output directory for plots to be saved in.
#' @return plot saved in p.dir as Fig_<station ID>.png
#
# Additional information on inputs can be found on the CO-OP API at:
# https://api.tidesandcurrents.noaa.gov/api/prod/
# ------------------------------------------------------------------------

river_plot <- function(metaList, tz, p.tz="America/New_York",
                       p.width = 4, p.height = 2.5, p.dir,
                       use.recorded = FALSE,
                       keep = 7, end_date = Sys.time()){
  # print(metaList$ID)
  # Set flood stage colors
  act.col = makeTransparent("#FEFF72", 150)
  min.col = makeTransparent("#FFC672", 150)
  mod.col = makeTransparent("#FF7272", 150)
  maj.col = makeTransparent("#E28EFF", 150)
  
  if(use.recorded){
    # Determine midnight and noon for the record period
    day_midnight <- as.POSIXct(paste0(Sys.Date() - keep:1, "00:00:00"), 
                               format = "%Y-%m-%d %H:%M:%S", tz = p.tz)
    day_noon <- as.POSIXct(paste0(Sys.Date() - keep:1, "12:00:00"), 
                           format = "%Y-%m-%d %H:%M:%S", tz = p.tz)
    
    # Convert number of days to keep to seconds
    keepSec = keep*86400 # 60 secs * 60 mins * 24 hrs
    start_date = end_date - keepSec
    
    # Convert observation time to R time and evaluate if any dates are older
    # than the time we want to record.
    obstime = as.POSIXct(metaList$obs$time, format = "%Y-%m-%d %H:%M:%S", tz = tz)
    obstime = format(obstime, tz=p.tz)
    metaList$obs$time = obstime
    ind = which(obstime < start_date)
    
    # Remove any old records 
    if(length(ind) > 0){
      metaList$obs = metaList$obs[-ind, ]
      obstime = obstime[-ind]
    }
    
    # Convert any missing values to NA
    metaList$obs[metaList$obs == "-999.00"]=NA
    metaList$forecast = NA
    fortime = NA
    
  } else {
    if(!all(is.na(metaList$obs))){ # If they are NOT all NA
      # Convert observation time to R time
      obstime = as.POSIXct(metaList$obs$time, format = "%Y-%m-%dT%H:%M:%S-00:00", tz = tz)
      obstime = as.POSIXct(format(obstime, tz=p.tz))
      
      fortime = as.POSIXct(metaList$forecast$time, format = "%Y-%m-%dT%H:%M:%S-00:00", tz = tz)
      fortime = as.POSIXct(format(fortime, tz=p.tz))
      
      # Determine midnight and noon for the record period
      timeRange = range(c(obstime, fortime), na.rm = TRUE)
      dateRange = as.Date(timeRange)
      day_midnight <- as.POSIXct(paste0(seq(dateRange[1], dateRange[2], by="days"), "00:00:00"), 
                                 format = "%Y-%m-%d %H:%M:%S", tz = p.tz)
      day_noon <- as.POSIXct(paste0(seq(dateRange[1], dateRange[2], by="days"), "12:00:00"), 
                             format = "%Y-%m-%d %H:%M:%S", tz = p.tz)
      
      # Convert any missing values to NA
      metaList$obs[metaList$obs == "-999" | metaList$obs == "-999.00"]=NA
      metaList$forecast[metaList$forecast == "-999" | metaList$forecast == "-999.00"]=NA
    } 
  }
  
  # Create plot
  png(file=paste0(p.dir, "Fig_", metaList$ID, ".png"), family="Helvetica", units="in", 
      width=p.width, height=p.height, pointsize=12, res=300)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(1.75,2.5,0.25,3))
  
  if(all(is.na(as.numeric(metaList$obs$height))) && 
     all(is.na(as.numeric(metaList$obs$discharge)))){ # If no data
    plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
    
    # draw your legend without the border and with the text left-aligned and save the components :
    myleg<-legend("center","No data available", plot=T, bty="n")
    
    # get the user coordinates to adjust the gap between the text and the border
    coord<-par("usr")
    
    # add a border, closer to the text (here, gap between border and beginning of 
    # text is a hundredth of the plot width) :
    rect(myleg$text$x[1]-diff(coord[1:2])/100, myleg$rect$top-myleg$rect$h, 
         myleg$rect$left+myleg$rect$w, myleg$rect$top, bg="white")
    
  } else if(!all(is.na(as.numeric(metaList$obs$height))) && 
            all(is.na(as.numeric(metaList$obs$discharge)))){ # if height but no discharge
    
    # Add gage height data
    hRange = range(c(as.numeric(metaList$forecast$height), as.numeric(metaList$obs$height)), na.rm = TRUE)
    plot(obstime, as.numeric(metaList$obs$height), type="n", xaxs="i",
         ylab="", xlab="", xaxt="n", las=2, xlim=timeRange, ylim=hRange)
    mtext(2, text="Gage height (ft)", line=1.5)
    
    # Add some nice gridding
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    # grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    
    # Add flood stage information
    rect(par("usr")[1], metaList$action, par("usr")[2], metaList$minor, col = act.col)
    rect(par("usr")[1], metaList$minor, par("usr")[2], metaList$mod, col = min.col)
    rect(par("usr")[1], metaList$mod, par("usr")[2], metaList$major, col = mod.col)
    rect(par("usr")[1], metaList$major, par("usr")[2], par("usr")[4], col = maj.col)
    
    text(par("usr")[1], metaList$action, "Action", adj = c(0,0))
    text(par("usr")[1], metaList$minor, "Minor", adj = c(0,0))
    text(par("usr")[1], metaList$mod, "Moderate", adj = c(0,0))
    text(par("usr")[1], metaList$major, "Major", adj = c(0,0))
    
    lines(obstime, as.numeric(metaList$obs$height), lwd=2)
    lines(fortime, as.numeric(metaList$forecast$height), lwd=2, lty=2)
    abline(v = max(obstime), lwd=2, col="black")
    
    } else if(all(is.na(as.numeric(metaList$obs$height))) && 
              !all(is.na(as.numeric(metaList$obs$discharge)))){ # if discharge but no height
      
      dRange = range(c(as.numeric(metaList$forecast$discharge), as.numeric(metaList$obs$discharge)), na.rm = TRUE)
      plot(obstime, as.numeric(metaList$obs$discharge), xaxs="i",
           type = "n", ylab = "", xlab="", xaxt="n", yaxt="n",
           xlim=timeRange, ylim=dRange)
      axis(4, col="#018571", col.ticks="#018571", col.axis="#018571", las=2)
      mtext(4, text=expression("Discharge"~(ft^3/s)), line=2, col="#018571")
      
      # Add some nice gridding
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
      axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
      axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
      # grid(NA, NULL, lty = 6, col = "gray")
      abline(v = day_midnight, lty = 6, col = "gray")
      
      lines(obstime, as.numeric(metaList$obs$discharge), lwd=2, col="#018571")
      lines(fortime, as.numeric(metaList$forecast$discharge), lwd=2, lty=2,
            col="#018571")
      abline(v = max(obstime), lwd=2, col="black")
      
      } else { # Create discharge and gage height plot.
    # Add gage height data
    hRange = range(c(as.numeric(metaList$forecast$height), as.numeric(metaList$obs$height)), na.rm = TRUE)
    plot(obstime, as.numeric(metaList$obs$height), type="n", xaxs="i",
         ylab="", xlab="", xaxt="n", las=2, xlim=timeRange, ylim=hRange)
    mtext(2, text="Gage height (ft)", line=1.5)
    
    # Add some nice gridding
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    # grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    
    # Add flood stage information
    rect(par("usr")[1], metaList$action, par("usr")[2], metaList$minor, col = act.col)
    rect(par("usr")[1], metaList$minor, par("usr")[2], metaList$mod, col = min.col)
    rect(par("usr")[1], metaList$mod, par("usr")[2], metaList$major, col = mod.col)
    rect(par("usr")[1], metaList$major, par("usr")[2], par("usr")[4], col = maj.col)
    
    text(par("usr")[1], metaList$action, "Action", adj = c(0,0))
    text(par("usr")[1], metaList$minor, "Minor", adj = c(0,0))
    text(par("usr")[1], metaList$mod, "Moderate", adj = c(0,0))
    text(par("usr")[1], metaList$major, "Major", adj = c(0,0))
    
    lines(obstime, as.numeric(metaList$obs$height), lwd=2)
    lines(fortime, as.numeric(metaList$forecast$height), lwd=2, lty=2)
    
    # Add the discharge data
    par(new=TRUE)
    dRange = range(c(as.numeric(metaList$forecast$discharge), as.numeric(metaList$obs$discharge)), na.rm = TRUE)
    plot(obstime, as.numeric(metaList$obs$discharge), 
         type = "l", lwd=2, col="#018571", ylab = "", xlab="", xaxt="n", yaxt="n",
         xlim=timeRange, ylim=dRange, xaxs="i")
    axis(4, col="#018571", col.ticks="#018571", col.axis="#018571", las=2)
    mtext(4, text=expression("Discharge"~(ft^3/s)), line=2, col="#018571")
    
    lines(fortime, as.numeric(metaList$forecast$discharge), lwd=2, lty=2,
          col="#018571")
    
    abline(v = max(obstime), lwd=2, col="black")
    
  }
  dev.off()
}
# ##########################################################################
# ##########################################################################
# ##a function to retry running a function if there is an error
# ##modified from code found at: https://stackoverflow.com/questions/37379472/creating-a-retry-function-in-r
# retry <- function(a, max=6, init=0, delay=0){
#   suppressWarnings(tryCatch({
#   if(init<max){
#     a
#   }}, error=function(e){
#     Sys.sleep(delay)
#     retry(a, max, init=init+1)
#     }))
# }
# ##########################################################################
# ##########################################################################
# # Function extracting weather data from an XML file.
# parseWS_xml = function(id){
#   #################
#   #id <- weather_stations$id[1]
#   #id <- "KUNV"
#   #id <- "KBUF"
#   #id <- "KIDI"
#   #id <- "KNYC"
#   #################
#   #print(id)
#   rawDataURL <- paste0("https://w1.weather.gov/data/METAR/", id, ".1.txt")
#   readRawData <- readLines(rawDataURL)
#   getData <- readRawData[4]
#   #getData <- metar_get(id)
#   
#   ##extract various variables, as well as calculate a few followup variables
#   sevWeath <- metar_wx_codes(getData)
#   tempNumC <- metar_temp(getData)
#   tempNumF <- conv_unit(tempNumC, "C", "F")
#   dewNum <- metar_dew_point(getData)
#   windSpeedRaw <- metar_speed(getData, metric=F)
#   windSpeed <- round(conv_unit(windSpeedRaw, "knot", "mph"), 1)
#   if(metar_dir(getData)!=""){
#     windDirDeg <- as.numeric(sapply(strsplit(metar_dir(getData), ","), "[[", 1))
#   }else{
#     windDirDef <- ""
#   }
#   
#   relativeHum <- round(100 * (exp((17.645*dewNum)/(243.04+dewNum)) / exp((17.645*tempNumC)/(243.04+tempNumC))), 2)
#   vis <- metar_visibility(getData, metric=F)
#   rwyVis <- metar_rwy_visibility(getData)
#   airPress <- conv_unit(metar_pressure(getData), "hPa", "mbar")
#   cloudConds <- metar_cloud_coverage(getData)
#     
#   ##create empty objects to be filled with actual values, if available
#   weather <- ""
#   cloudCond <- ""
#   temp <- ""
#   humidity <- ""
#   wind <- ""
#   pressure <- ""
#   dewpoint <- ""
#   windchill <- ""
#   visibility <- ""
#   rwyVisibility <- ""
#   data <- ""
#   time <- ""
#   
#   ##as full date is not included in the raw data records, first get a time as clost to the recorded time as possible, 
#   ##then replace hour with hour presented in raw data 
#   dateToGMT <- .POSIXct(as.integer(as.POSIXct(as.character(Sys.time()), tz="EST5EDT")), tz="GMT")
#   dateBase <- .POSIXct(as.POSIXct(paste(strsplit(as.character(dateToGMT), " ")[[1]][1], metar_hour(getData)), tz="GMT"), tz="EST5EDT")
#   
#   # Time when observations were collected.
#   if(metar_hour(getData)!=""){
#     dateToGMT <- .POSIXct(as.integer(as.POSIXct(as.character(Sys.time()), tz="EST5EDT")), tz="GMT")
#     dateBase <- .POSIXct(as.POSIXct(paste(strsplit(as.character(dateToGMT), " ")[[1]][1], metar_hour(getData)), tz="GMT"), tz="EST5EDT")
#     date <- format(dateBase, format="%b %d, %Y") # convert from GMT to current time zone
#     time <- format(dateBase, format="%I:%M %p %Z") # convert from GMT to current time zone
#   }
#   # General weather condition.
#   if(sevWeath==""){
#     sevWeath <- "No Significant Weather"
#   }
#   weather <- paste0("<strong>Weather: </strong>", sevWeath, "<br/>")
#   # Cloud Conditions
#   if(is.na(cloudConds)==F & cloudConds!=""){
#     if(length(grep("No clouds below", cloudConds))>=1){
#       crCldReport <- "Mostly Sunny (0 oktas)"
#     }else if(length(grep("full cloud coverage", cloudConds))>=1){
#       splitNames1 <- sapply(strsplit(cloudConds, "at "), "[[", 2)
#       whereFT <- which(strsplit(splitNames1, " ")[[1]]=="ft")
#       getCldHt <- sapply(strsplit(splitNames1, " "), "[[", whereFT-1)
#       crCldReport <- paste0("Overcast (8 oktas) at ", getCldHt, " ft")
#     }else{
#       splitMulti <- strsplit(cloudConds, ", ")[[1]]
#       splitNames1 <- sapply(strsplit(splitMulti, " okta"), "[[", 1)
#       validEntries <- grep("-", splitNames1)
#       splitNames1 <- splitNames1[validEntries]
#       getOkta <- as.numeric(sapply(strsplit(splitNames1, "-"), "[[", 2))
#       getOktas <- sapply(strsplit(splitMulti[validEntries], " "), "[[", 2)
#       getCldHt <- sapply(strsplit(splitMulti[validEntries], " "), "[[", 5)
#       ##oktas breaks from: https://worldweather.wmo.int/oktas.htm
#       oktaClass <- sapply(getOkta, function(x){if(x>=0 & x<=2){
#                                                 y <- "Mostly Sunny"
#                                               }else if(x>=3 & x<=5){
#                                                 y <- "Partly Cloudy"
#                                               }else if(x==6 | x==7){
#                                                 y <- "Mostly Cloudy"
#                                               }else if(x==8){
#                                                 y <- "Overcast"
#                                               }
#                                               return(y)})
#       
#       crCldReport <- paste0(oktaClass, " ", getOktas, " oktas) at ", getCldHt, " ft")
#     }
#     if(length(crCldReport)>1){
#       numReports <- length(crCldReport)
#       #addCloudLines <- paste0('<p style="test-indent: 18px">', crCldReport[2:numReports], '</p>')
#       addCloudLines <- paste0("&emsp;&emsp;&emsp;&emsp;&ensp;&ensp;&ensp;&ensp;&nbsp;&nbsp;&nbsp;&nbsp;&emsp;&emsp;&emsp;&emsp;", crCldReport[2:numReports])
#       crCldReport <- c(crCldReport[1], addCloudLines)
#       crCldReport <- paste(crCldReport, collapse="<br/>")
#     }
#     cloudCond <- paste0("<strong>Cloud Conditions: </strong>", crCldReport, "<br/>")
#   }
#   # Air temperature in F.
#   if(is.na(tempNumF)==F & tempNumF!=""){
#     temp <- paste0("<strong>Temperature: </strong>", tempNumF, " &#8457;<br/>")
#   }
#   # Relative humidity in %.
#   if(is.na(relativeHum)==F & relativeHum!=""){
#     humidity <- paste0("<strong>Relative Humidity: </strong>", relativeHum, " %<br/>")
#   }
#   # Which direction the wind is blowing.
#   if(is.na(windSpeed)==F & windSpeed!="" & metar_dir(getData)!=""){
#     if(metar_dir(getData)=="Variable"){
#       wind <- paste0("<strong>Wind: </strong>From ", windDirDeg, " at ", windSpeed, " MPH (", round(windSpeedRaw, 1), " KT) <br/>")
#     }else{
#       if(is.na(windDirDeg)==T){
#         windDir <- "0"
#       }else if(windDirDeg>=0 & windDirDeg<=11.24){
#         windDir <- "N"
#       }else if(windDirDeg>=11.25 & windDirDeg<=33.74){
#         windDir <- "NNE"
#       }else if(windDirDeg>=33.75 & windDirDeg<=56.24){
#         windDir <- "NE"
#       }else if(windDirDeg>=56.25 & windDirDeg<=78.74){
#         windDir <- "ENE"
#       }else if(windDirDeg>=78.75 & windDirDeg<=101.24){
#         windDir <- "E"
#       }else if(windDirDeg>=101.25 & windDirDeg<=123.74){
#         windDir <- "ESE"
#       }else if(windDirDeg>=123.75 & windDirDeg<=146.24){
#         windDir <- "SE"
#       }else if(windDirDeg>=146.25 & windDirDeg<=168.74){
#         windDir <- "SSE"
#       }else if(windDirDeg>=168.75 & windDirDeg<=191.24){
#         windDir <- "S"
#       }else if(windDirDeg>=191.25 & windDirDeg<=213.74){
#         windDir <- "SSW"
#       }else if(windDirDeg>=213.75 & windDirDeg<=236.24){
#         windDir <- "SW"
#       }else if(windDirDeg>=236.25 & windDirDeg<=258.74){
#         windDir <- "WSW"
#       }else if(windDirDeg>=258.75 & windDirDeg<=281.24){
#         windDir <- "W"
#       }else if(windDirDeg>=281.25 & windDirDeg<=303.74){
#         windDir <- "WNW"
#       }else if(windDirDeg>=303.75 & windDirDeg<=326.24){
#         windDir <- "NW"
#       }else if(windDirDeg>=326.25 & windDirDeg<=348.74){
#         windDir <- "NNW"
#       }else if(windDirDeg>=348.75 & windDirDeg<=360){
#         windDir <- "N"
#       }
#       wind <- paste0("<strong>Wind: </strong>From ", windDir, " (", windDirDeg, "&deg;) at ", windSpeed, " MPH (", round(windSpeedRaw, 1), " KT) <br/>")
#     }
#   }
#   # Pressure in mb.
#   if(is.na(airPress)==F & airPress!=""){
#     pressure <- paste0("<strong>Pressure: </strong>", airPress, " mbar<br/>")
#   }
#   # Dew point in F.
#   if(is.na(dewNum)==F & dewNum!=""){
#     dewpoint <- paste0("<strong>Dewpoint: </strong>", conv_unit(dewNum, "C", "F"), " &#8457;<br/>")
#   }
#   # Windchill in F.
#   if(is.na(tempNumF)==F & is.na(windSpeed)==F & tempNumF<=50 & windSpeed>=3){
#     ##based on equation linked to here: https://www.weather.gov/epz/wxcalc_windchill
#     ##limitations are temp <= 50F and wind speed > 3 mph, so not included unless those conditions met in if statement below
#     chillyWind <- round(35.74 + (0.6215*tempNumF) - (35.75*(windSpeed^0.16)) + (0.4275*tempNumF*(windSpeed^0.16)), 1)
#     windchill <- paste0("<strong>Wind chill: </strong>", chillyWind, " &#8457;<br/>")
#   }
#   # Current visibility in miles.
#   if(is.na(vis)==F & vis!=""){
#     visibility <- paste0("<strong>Visibility: </strong>", vis, " miles<br/>")
#   }
#   # Runway visibility
#   if(is.na(rwyVis)==F & rwyVis!=""){
#     rwyVisibility <- paste0("<strong>Runway Visibility: </strong>", rwyVis, "<br/>") 
#   }
#   # Observation url.
#   link <- paste0('http://w1.weather.gov/xml/current_obs/', id, '.xml')
#   
#   
#   #xml.url <- paste0('http://w1.weather.gov/xml/current_obs/', id, '.xml')
#   ## Turn XML data into a list.
#   #xml_data <- retry(xmlToList(rawToChar(GET(xml.url, user_agent("httr (mdl5548@psu.edu)"))$content)))
#   # # Station location.
#   # if(is.null(xml_data$location)){
#   #   name <- ""
#   # } else {
#   #   name <- xml_data$location
#   # }
#   # # Latitude.
#   # if(is.null(xml_data$latitude)){
#   #   latitude <- NA
#   # } else {
#   #   latitude <- xml_data$latitude
#   # }
#   # # Longitude.
#   # if(is.null(xml_data$longitude)){
#   #   longitude <- NA
#   # } else {
#   #   longitude <- xml_data$longitude
#   # }
#   # # Time when observations were collected.
#   # if(is.null(xml_data$observation_time)){
#   #   date <- ""
#   #   time <- ""
#   # } else {
#   #   exTime <- sapply(strsplit(xml_data$observation_time, "on "), "[[", 2)
#   #   exZ <- sapply(strsplit(exTime," "), "[[", length(strsplit(exTime[1]," ")[[1]]))
#   #   if(exZ=="HST"|exZ=="HDT"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="Pacific/Honolulu")
#   #   }else if(exZ=="EST"|exZ=="EDT"|exZ=="ZST"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/New_York")
#   #   }else if(exZ=="CST"|exZ=="CDT"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Chicago")
#   #   }else if(exZ=="MST"|exZ=="MDT"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Denver")
#   #   }else if(exZ=="PST"|exZ=="PDT"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Los_Angeles")
#   #   }else if(exZ=="AKST"|exZ=="AKDT"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="America/Anchorage")
#   #   }else if(exZ=="SST"|exZ=="SDT"){
#   #     dateBase <- as.POSIXlt(exTime, format="%b %d %Y, %I:%M %p", tz="Pacific/Samoa")
#   #   }
#   #   date <- format(dateBase, format="%b %d, %Y") # convert from GMT to current time zone
#   #   time <- format(dateBase, format="%I:%M %p %Z") # convert from GMT to current time zone
#   # }
#   # # General weather condition.
#   # if(is.null(xml_data$weather)){
#   #   weather <- ""
#   # } else {
#   #   weather <- paste0("<strong>Weather: </strong>", xml_data$weather, "<br/>")
#   # }
#   # # Air temperature in F.
#   # if(is.null(xml_data$temp_f)){
#   #   temp <- ""
#   # } else {
#   #   temp <- paste0("<strong>Temperature: </strong>", xml_data$temp_f, " &#8457;<br/>")
#   # }
#   # # Relative humidity in %.
#   # if(is.null(xml_data$relative_humidity)){
#   #   humidity <- ""
#   # } else {
#   #   humidity <- paste0("<strong>Relative humidity: </strong>", xml_data$relative_humidity, " %<br/>")
#   # }
#   # # Which direction the wind is blowing.
#   # if(is.null(xml_data$wind_string)){
#   #   wind <- ""
#   # } else {
#   #   wind <- paste0("<strong>Wind: </strong>", xml_data$wind_string, " <br/>")
#   # }
#   # # Pressure in mb.
#   # if(is.null(xml_data$pressure_mb)){
#   #   pressure <- ""
#   # } else {
#   #   pressure <- paste0("<strong>Pressure: </strong>", xml_data$pressure_mb, " mb<br/>")
#   # }
#   # # Dew point in F.
#   # if(is.null(xml_data$dewpoint_f)){
#   #   dewpoint <- ""
#   # } else {
#   #   dewpoint <- paste0("<strong>Dewpoint: </strong>", xml_data$dewpoint_f, " &#8457;<br/>")
#   # }
#   # # Windchill in F.
#   # if(is.null(xml_data$windchill_f)){
#   #   windchill <- ""
#   # } else {
#   #   windchill <- paste0("<strong>Wind chill: </strong>", xml_data$windchill_f, " &#8457;<br/>")
#   # }
#   # # Current visibility in miles.
#   # if(is.null(xml_data$visibility_mi)){
#   #   visibility <- ""
#   # } else {
#   #   visibility <- paste0("<strong>Visibility: </strong>", xml_data$visibility_mi, " miles<br/>")
#   # }
#   # # Observation url.
#   # if(is.null(xml_data$ob_url)){
#   #   link <- ""
#   # } else {
#   #   link <- xml.url # xml_data$ob_url: klr use human readable link
#   # }
#   
#   
#   obs <- paste0(weather, cloudCond, wind, windchill, temp, humidity, dewpoint, pressure, visibility, rwyVisibility)
#   
#   # Return the weather variables
#   return(c(as.character(id), obs, link, date, time))
# }
# ##########################################################################
# ##########################################################################
# collectBuoyData = function(buoys_ids, US_buoys){
#   #################
#   #buoys_ids <- NDBC_buoys$ID
#   #buoys_ids <- NDBC_stations$ID
#   #buoys_ids <- non_NDBC_stations$ID
#   #US_buoys <- US_buoys
#   #################
#   #print(buoys_ids)
#   ##set up 'bones' for data to be returned, mostly for preserving input/output order
#   outTab <- data.frame(id=buoys_ids)
#   
#   # Use the ID to create a URL to the RSS file.
#   buoyURLs <- paste0('https://www.ndbc.noaa.gov/data/latest_obs/', buoys_ids, '.rss')
#   
#   # Check if url/ observations exist
#   buoyExistance <- as.character(sapply(buoyURLs, url.exists))
#   
#   ##########################################
#   ##for those urls that do exist
#   if(T%in%buoyExistance){
#     scripts <- sapply(buoyURLs[which(buoyExistance==T)], function(x){retry(getURL(x))})
#     month <- format(Sys.Date(),"%B")
#     
#     # Use the ID to create a URL to the RSS file. 
#     scripts <- sapply(scripts, function(txt){subTxt<-gsub(pattern="&", "&amp;", x=txt);
#                                               subTxt<-gsub(pattern="&amp;#", "&#", x=subTxt);
#                                               subTxt<-gsub(pattern='</rss"', '</rss>"', x=subTxt);
#                                               subTxt<-gsub("\"","\'", subTxt);
#                                               return(subTxt)})
# 
#     docs <- lapply(scripts, xmlParse)
#   
#     # Extract the meteorological data, which is in the 'description' node.
#     buoy_vects <- sapply(docs, xpathSApply, path='//channel/item/description', fun=xmlValue)
#     # Remove the '\n' character from the extracted string.
#     removeEnd <- sapply(buoy_vects, str_replace_all, pattern="([\n])", replacement="")
#     
#     exTime <- sapply(strsplit(buoy_vects,"<strong>|</strong>"), "[[", 2)
#     buoySubObs <- sapply(strsplit(removeEnd, paste0(exTime, "</strong><br />")), "[[", 2)
#     
#     splitLength <- sapply(exTime, function(x){length(strsplit(x, " ")[[1]])})
#     exZ <- sapply(1:length(exTime), function(x){sapply(strsplit(exTime[x]," "), "[[", splitLength[x])})
#     dateBases <- mapply(function(tim, zon){reForm<-as.POSIXlt(tim, format="%b %d, %Y %I:%M %p", tz="EST5EDT")
#                                             return(reForm)}, tim=exTime, zon=exZ, SIMPLIFY=F)
#     date <- sapply(dateBases, format, format="%b %d, %Y")  #format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from GMT to current time zone
#     time <- sapply(dateBases, format, format="%I:%M %p %Z")  #format(dateBase, format="%I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
#     
#     # Extract the name, link, and coordinates which are in the 'title', 'link', and 'georss:point' node.
#     buoy_names <- sapply(docs, xpathSApply, path='//channel/item/title', fun=xmlValue)
#     buoy_links <- sapply(docs, xpathSApply, path='//channel/item/link', fun=xmlValue)
#     buoy_coords <- sapply(docs, xpathSApply, path='//channel/item/georss:point', fun=xmlValue)
#     buoy_lats <- as.numeric(sapply(strsplit(buoy_coords," "),"[[", 1))
#     buoy_lons <- as.numeric(sapply(strsplit(buoy_coords," "),"[[", 2))
#     
#     #existFrame <- data.frame(id=buoys_ids[which(buoyExistance==T)], obs=paste0(buoySubObs, "<br/>"), time=paste0("Late Updated on ", exTime), name=buoy_names,
#     #                         link=buoy_links, lat=buoy_lats, lon=buoy_lons)
#     existFrame <- data.frame(id=buoys_ids[which(buoyExistance==T)], obs=paste0(buoySubObs, "<br/>"), date=date, time=time, name=buoy_names,
#                              link=buoy_links, lat=buoy_lats, lon=buoy_lons)
#   }
#   
#   ##########################################
#   ##for those urls which do not exist
#   if(F%in%buoyExistance){
#     noExist <- buoys_ids[which(buoyExistance==F)]
#     noExistUS <- which(US_buoys$ID%in%noExist)
#     noExistFrame <- data.frame(id=noExist, obs="There are no current meteorological observations recorded at this buoy.<br/><br/>", date="", time="",
#                                name=as.character(US_buoys$name[noExistUS]), link=paste0("http://www.ndbc.noaa.gov/station_page.php?station=", noExist),
#                                lat=as.character(US_buoys$lat[noExistUS]), lon=as.character(US_buoys$lon[noExistUS]))
#     existFrame <- rbind.data.frame(existFrame,noExistFrame)
#   }
#   
#   ##output table
#   fullFrame <- merge(x=outTab, y=existFrame, by="id", sort=F)
#   
#   fullFrame$date <-as.character(fullFrame$date)
#   fullFrame$time <-as.character(fullFrame$time)
#   if(""%in%fullFrame$date){
#     fullFrame$date[fullFrame$date==""] <- max(fullFrame$date[fullFrame$date!=""])
#     fullFrame$time[fullFrame$time==""] <- max(fullFrame$time[fullFrame$time!=""])
#   }
#   
#   return(fullFrame)
# }
# ##########################################################################
# ##########################################################################
# # Function extracting tide data from within the past 18 mins from a XML file online.
# collectLatestTidal <- function(varURL){
#   #################
#   #varURL <- varURLs[1]
#   #varURL <- varURLs[9]
#   #################
#   xml_data <- retry(xmlToList(rawToChar(GET(varURL)$content)))
#   var <- sapply(strsplit(varURL, "product=|&datum="), "[[", 2)
#   
#   if("error" %in% names(xml_data) | class(xml_data)!="list"){
#     ##if there is no available data
#     value <- NA
#     date <- NA
#     time <- NA
#     metaID <- NA
#     metaName <- NA
#     metaLat <- NA
#     metaLon <- NA
#   }else if(is.null(xml_data$observations$disclaimers)==FALSE){
#     remove <- c("disclaimers.disclaimer.text", "disclaimers.disclaimer..attrs")
#     chkVals <- data.frame(t(xml_data$observations[[1]]))
#     chkVals <- chkVals[!rownames(chkVals) %in% remove,]
#     value <- chkVals$v
#     if(var=="wind"){
#       value <- paste0("From the ", chkVals$dr, " at ", chkVals$s)
#     }
#     #date <- as.POSIXct(chkVals$t, format = "%Y-%m-%d %H:%M", tz = "GMT")
#     #date <- format(date, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York") # convert from GMT to current time zone
#     dateBase <- as.POSIXct(chkVals$t, format="%Y-%m-%d %H:%M", tz="GMT")
#     date <- format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from GMT to current time zone
#     time <- format(dateBase, format="%I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
#     metaID <- xml_data$metadata["id"]
#     metaName <- xml_data$metadata["name"]
#     metaLat <- xml_data$metadata["lat"]
#     metaLon <- xml_data$metadata["lon"]
#   }else{
#     chkVals <- data.frame(t(xml_data$observations[[1]]))
#     if(var=="wind"){
#       value <- paste0("From the ", as.character(chkVals$dr), " at ", as.character(chkVals$s))
#     }else{
#       value <- as.numeric(as.character(chkVals[1,2]))
#     }
#     #date <- as.POSIXct(chkVals$t, format="%Y-%m-%d %H:%M", tz="GMT")
#     #date <- format(date, format="%b %d, %Y %I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
#     dateBase <- as.POSIXct(chkVals$t, format="%Y-%m-%d %H:%M", tz="GMT")
#     date <- format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from GMT to current time zone
#     time <- format(dateBase, format="%I:%M %p %Z", tz="America/New_York") # convert from GMT to current time zone
#     metaID <- as.character(xml_data$metadata["id"])
#     metaName <- as.character(xml_data$metadata["name"])
#     metaLat <- as.numeric(as.character(xml_data$metadata["lat"]))
#     metaLon <- as.numeric(as.character(xml_data$metadata["lon"]))
#   }
#   
#   #return(c(value, date, metaID, metaName, metaLat, metaLon))
#   return(c(value, date, time, metaID, metaName, metaLat, metaLon))
# }
# ##########################################################################
# ##########################################################################
# # Run the function extracting the data we want and creating a plot.
# # CREATE a function of this step!!
# tideStationData <- function(statID, spDatum, timez, un){
#   #################
#   #statID <- tideIDs[206]
#   #statID <- "8571421"
#   #statID <- tideIDsMSL[1]
#   #spDatum <- datum
#   #timez <- timezone
#   #un <- units
#   #################
#   vars <- c("air_temperature", "air_pressure", "visibility", "humidity", "wind", "water_level", "water_temperature", "conductivity", "salinity")
# 
#   # Use the ID, variable, datum, timezone, and units to create a URL to the XML file.
#   varURLs <- paste0('https://tidesandcurrents.noaa.gov/api/datagetter?date=latest&station=', statID, '&product=', vars, '&datum=', spDatum, '&units=', un, '&time_zone=', timez, '&application=web_services&format=xml')
#   
#   getVarVals <- lapply(varURLs, collectLatestTidal)
#   tableVars <- cbind.data.frame(vars, do.call(rbind.data.frame, getVarVals))
#   colnames(tableVars)[2:ncol(tableVars)] <- c("value", "date", "time", "metaID", "metaName", "metaLat", "metaLon") 
#   
#   if(F %in% is.na(tableVars$time)){
#     ##collect the non-NA update times
#     validTimes <- which(is.na(tableVars$time)==F)
#     ##determine which time is the latest
#     #latTimeInd <- which.min(utctime(validTimes))
#     latTimeInd <- which.min(utctime(as.POSIXlt(paste(tableVars$date[validTimes], tableVars$time[validTimes]), format="%b %d, %Y %I:%M %p", tz="EDT")))
#     ##set all times to latest updated time
#     tableVars$date <- as.character(tableVars$date[validTimes[latTimeInd]])
#     tableVars$time <- as.character(tableVars$time[validTimes[latTimeInd]])
#   }
#   
#   ##determine which variables to include in variable
#   subVarTab <- tableVars[which(is.na(tableVars$value)==F),]
#   
#   #metaString <- ""
#   dataFramed <- data.frame(id=statID, url=paste0("https://tidesandcurrents.noaa.gov/stationhome.html?id=",statID), obs=NA, date=NA, time=NA, image=paste0("https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_",statID,".png"), lon=NA, lat=NA)
#   obsString <- ""
#   if(nrow(subVarTab)>0){
#     subVarNames <- subVarTab$vars
#     if("air_temperature" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Air temperature: </strong>", subVarTab$value[which(subVarNames=="air_temperature")], " &#8457;<br/>") #klr convert C (&#8451;) to F (&#8457;)
#     }
#     
#     if("air_pressure" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Air pressure: </strong>", subVarTab$value[which(subVarNames=="air_pressure")], " mb<br/>") # klr changed mbar to mb
#     }
#     
#     if("visibility" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Visibility: </strong>", subVarTab$value[which(subVarNames=="visibility")], " nmi<br/>")
#     }
#     
#     if("humidity" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Relative humidity: </strong>", subVarTab$value[which(subVarNames=="humidity")], " %<br/>")
#     }
#     
#     if("wind" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Wind: </strong>", subVarTab$value[which(subVarNames=="wind")], " knots<br/>") #klr spell out knots
#     }
#     
#     if("water_level" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Water level: </strong>", subVarTab$value[which(subVarNames=="water_level")], " ft ", spDatum, "<br/>") #klr convert m to ft
#     }
#     
#     if("water_temperature" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Water temperature: </strong>", subVarTab$value[which(subVarNames=="water_temperature")], " &#8457;<br/>") #klr convert C (&#8451;) to F (&#8457;)
#     }
#     
#     if("conductivity" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Conductivity: </strong>", subVarTab$value[which(subVarNames=="conductivity")], " mS/cm<br/>")
#     }
#     
#     if("salinity" %in% subVarNames){
#       obsString <- paste0(obsString, "<strong>Salinity: </strong>", subVarTab$value[which(subVarNames=="salinity")], " psu<br/>")
#     }
#     
#     #metaString <- paste0(metaString, '{"type": "Feature", "properties": {"name": "', unique(subVarTab$metaName), '", "id": "', statID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', statID, '", "obs": "',
#     #                     obsString, '", "time": "', unique(subVarTab$time), '", "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', statID, '.png"}, geometry": {"type": "Point", "coordinates": [', 
#     #                     unique(subVarTab$metaLon), ',',  unique(subVarTab$metaLat), ']}}')
#     dataFramed$obs <- obsString
#     dataFramed$date <- unique(subVarTab$date)
#     dataFramed$time <- unique(subVarTab$time)
#     dataFramed$lon <- unique(subVarTab$metaLon)
#     dataFramed$lat <- unique(subVarTab$metaLat)
#   }
#   
#   #return(metaString)
#   return(dataFramed)
# }
# ##########################################################################
# ##########################################################################
# # Function extracting tide data (hight and time) from a XML file online.
# waterheight_plot <- function(url, weekMidnights, weekNoons, plotW, plotH, plotOut){
#   #################
#   #url <- tideURLs[1]
#   #weekMidnights <- day_midnight
#   #weekNoons <- day_noon
#   #plotW <- p.width
#   #plotH <- p.height
#   #plotOut <- plotDir
#   #################
#   
#   statID <- sapply(strsplit(url, "&station=|&time_"), "[[", 2)
#   #print(statID)
# 
#   xml_data <- xmlToList(rawToChar(GET(url)$content))
#   
#   ##create plot
#   png(file=paste0(plotOut, "Fig_", statID, ".png"), family="Helvetica", units="in", width=plotW, height=plotH, pointsize=14, res=300)
#   par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
#   if("error" %in% names(xml_data) | class(xml_data)!="list"){
#     plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
#     legend("center", "No data available", bg="white")
#   }else{
#     if(is.null(xml_data$observations$disclaimers)==FALSE){
#       remove <- which(names(xml_data$observations)=="disclaimers")
#       xml_data$observations[remove] <- NULL
#     }
#     stationData <- do.call(rbind.data.frame, xml_data$observations)
#     colnames(stationData) <- c("time", "values", "s", "f", "q")
#     
#     date <- as.POSIXct(stationData$time, format="%Y-%m-%d %H:%M", tz="GMT")
#     date <- format(date, tz="America/New_York") # convert from GMT to current time zone
#     stationData$time <- as.POSIXct(date, format="%Y-%m-%d %H:%M", tz="")
#     # Determine which indices of the date occur at midnight and noon.
#     hours <- strftime(stationData$time, format="%H:%M")
#     midnight <- which(hours=="00:00")
#     noon <- which(hours=="12:00")
#     
#     plot(stationData$time, as.numeric(as.character(stationData$values)), type="n", ylab=paste0("Height (ft ", datum, ")"), xlab="Past 3 days", xaxt="n") #klr changed m to ft
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
#     axis(1, at=weekMidnights, labels=FALSE, tick=TRUE)
#     axis(1, at=weekNoons, labels=gsub("0(\\d)", "\\1", format(weekNoons, "%m/%d")), tick=FALSE)
#     grid(NA, NULL, lty=6, col="gray")
#     abline(v=weekMidnights, lty=6, col="gray")
#     lines(stationData$time, as.numeric(as.character(stationData$values)), lwd=2, col="steelblue")
#   }
#   dev.off()
# }
# ##########################################################################
# ##########################################################################
# # Function extracting stream data (discharge and time) from a TXT file online.
# usgs_dataRetrieveVar = function(dataPg, url, timez, data){
#   #################
#   #url <- gageTmpURLs[1]
#   #url <- dischargeURL
#   #url <- dailyAveURL
#   #url <- heightURL
#   #url <- gageTmpURLs[grep("03007800", gageTmpURLs)]
#   #url <- gageTmpURLs[grep("01549700", gageTmpURLs)]
#   #dataPg <- chkURL
#   #url <- gageTmpURLs[1]
#   #timez <- "US/Eastern"
#   #data <- "latest"  ##"full"  ##daily
#   #data <- "full"
#   #data <- "daily"
#   #################
#   # Extract number of metadata rows to skip and determine header names
#   readr.total <- retry(read_lines(url))
#   totalRows <- length(readr.total)
#   meta.rows <- length(readr.total[grep("^#", readr.total)])  ##rows that begin with a hashtag
#   
#   header.names <- strsplit(readr.total[meta.rows+1],"\t")[[1]]
# 
#   ##format the data based on what data is desired
#   if(data=="latest"){
#     ##only concerned with the latest data
#     latestData <- strsplit(readr.total[totalRows],"\t")[[1]]
#     if((totalRows==meta.rows+3) & (length(latestData)==0)){  ##there is no actual data
#       return(c(NA,NA))
#     }
#     
#     ##get varialbe name and index
#     var <- strsplit(url, "cb_|=on")[[1]][2]
#     varInd <- grep(var, header.names)[1]
#     # NEED TO CHECK FOR MISSING DATA!!!
#     if(is.na(varInd) || is.na(latestData[varInd])==T){
#       return(c(NA,NA))
#     }
#     
#     # Convert and extract the date and time.
#     dateInd <- which(header.names=="datetime")
#     tzInd <- which(header.names=="tz_cd")
#     #latestData[dateInd] <- format(as.POSIXct(latestData[dateInd], format="%Y-%m-%d %H:%M", tz=tz), format="%b %d, %Y %I:%M %p %Z")
#     if(latestData[tzInd]=="EST" | latestData[tzInd]=="EDT"){
#       dateBase <- as.POSIXct(latestData[dateInd], format="%Y-%m-%d %H:%M", tz="")
#     }else if(latestData[tzInd]=="CST" | latestData[tzInd]=="CDT"){
#       dateBase <- as.POSIXct(latestData[dateInd], format="%Y-%m-%d %H:%M", tz="CST6CDT")
#     }else{
#       stop(paste0("Unsupported TZ: ", latestData[tzInd]))
#     }
#     #latestData[dateInd] <- format(dateBase, format="%b %d, %Y", tz="America/New_York") # convert from local time zone to Eastern NA
#     #latestData <- c(latestData[1:dateInd], format(dateBase, format="%I:%M %p %Z", tz="America/New_York"), latestData[tzInd:length(latestData)]) # convert from local time zone to Eastern NA
#     
#     return(c(latestData[varInd], format(dateBase, format="%b %d, %Y", tz=timez), format(dateBase, format="%I:%M %p %Z", tz=timez)))
#     
#   }else if(data=="full"){
#     ##want all of the available within the timeframe
#     splitData <- strsplit(readr.total[(meta.rows+3):totalRows],"\t")
#     tableData <- do.call(rbind.data.frame, splitData)
#     if(nrow(tableData)<1){
#       return(NA)
#     }
#     
#     colnames(tableData) <- header.names
#     # Give parameter data a common header name 
#     varCol <- grep(sapply(strsplit(url, "cb_|=on"), "[[", 2), header.names)[1]
#     colnames(tableData)[varCol] <- "var"
#     tableData$var <- as.numeric(as.character(tableData$var))
#     
#     # NEED TO CHECK FOR MISSING DATA!!!
#     if(unique(is.na(tableData$var))==T){
#       return(NA)
#     }
#     
#     # Convert and extract the date and time.
#     tableData$datetime <- as.POSIXct(tableData$datetime, format="%Y-%m-%d %H:%M", tz=timez)
#     tableData$time <- strftime(tableData$datetime, format="%H:%M", tz=timez)
#     
#     return(tableData)
#     
#   }else if(data=="daily"){
#     ##want all of the daily data within the timeframe
#     splitData <- strsplit(readr.total[(meta.rows+3):totalRows],"\t")
#     tableData <- do.call(rbind.data.frame, splitData)
#     if(nrow(tableData)<1){
#       return(NA)
#     }
#     
#     colnames(tableData) <- header.names[1:ncol(tableData)]
#     ##match dates 
#     # Convert month and day to a date.
#     tableData$MonDay <- as.POSIXct(paste(tableData$month_nu, tableData$day_nu, sep="-"), format="%m-%d", tz=timez)
#     #format(paste(tableData$month_nu, tableData$day_nu, sep="-"), format="%m-%d")
#     
#     # NEED TO CHECK FOR MISSING DATA!!!
#     if(unique(is.na(tableData$mean_va))==T){
#       return(NA)
#     }else{
#       ##bDate and eDate are global variables
#       ##remove year from bDate and eDate
#       datesInd <- sapply(c(bDate, eDate), function(x){grep(format(x, format="%m-%d"), tableData$MonDay)})
#       
#       if(class(datesInd)=="list"){
#         weeksAve <- NA
#       }else if(datesInd[1] > datesInd[2]){
#         ##this may happen around the New Year
#         weeksAve <- mean(as.numeric(as.character(tableData$mean_va[c(1:datesInd[2], datesInd[1]:nrow(tableData))])), na.rm=T)
#       }else{
#         weeksAve <- mean(as.numeric(as.character(tableData$mean_va[datesInd[1]:datesInd[2]])), na.rm=T)
#       }
#     }
#     return(weeksAve)
#   }
# }
# ##########################################################################
# ##########################################################################
# ##function to create a string of observations based on what is available
# createObsString <- function(tab){
#   #################
#   #tab <- fullObsData
#   #################
#   ##character objects to put results into 
#   tempObs <- dischargeObs <- heightObs <- vector(mode="character", length=nrow(tab))
#   
#   ##determine the index for each possible variable combination
#   tempNotNA <- which(is.na(tab$temp)==F)
#   dischargeNotNA <- which(is.na(tab$discharge)==F)
#   heightNotNA <- which(is.na(tab$gageHeight)==F)
#   
#   ##construct the observation string
#   tempObs[tempNotNA] <- paste0(tempObs[tempNotNA], "<strong>Temperature: </strong>", tab$temp[tempNotNA], " &#8457")
#   dischargeObs[dischargeNotNA] <- paste0(dischargeObs[dischargeNotNA], "<strong>Discharge: </strong>", tab$discharge[dischargeNotNA], " ft&#179;/s")
#   heightObs[heightNotNA] <- paste0(heightObs[heightNotNA], "<strong>Gage height: </strong>", tab$gageHeight[heightNotNA], " ft")
#   
#   outObs <- sapply(1:nrow(tab), function(x){obs<-c(tempObs[x],dischargeObs[x],heightObs[x]);
#                                             obs<-obs[which(obs!="")];
#                                             if(length(obs)>1){
#                                               return(paste0("<br/>", paste0("<br/>", obs, collapse=""), "<br/><br/>"))
#                                             }else if(length(obs)==1){
#                                               return(paste0("<br/>", paste0("<br/>", obs), "<br/><br/>"))
#                                             }else{
#                                               return("")
#                                             }})
#   return(outObs)
# }
# ##########################################################################
# ##########################################################################
# # Function extracting stream data (discharge, gage height, and time) from USGS stream gages.
# stream_gage_plot <- function(dischargeURL, heightURL, weekMidnights, weekNoons, plotW, plotH, plotOut){
#   #################
#   #dischargeURL <- gageDisURLs[616]
#   #heightURL <- gageGagURLs[616]
#   #weekMidnights <- day_midnight
#   #weekNoons <- day_noon
#   #plotW <- p.width
#   #plotH <- p.height
#   #plotOut <- plotDir
#   #################
#   
#   statID <- sapply(strsplit(dischargeURL, "site_no=|&period"), "[[", 2)
#   print(statID)
#   
#   if(getURL(dischargeURL)=="No sites/data found using the selection criteria specified \n"){
#     dischargeDat <- NA
#   }else{
#     dischargeDat <- usgs_dataRetrieveVar(dischargeURL, "America/New_York", "full")
#   }
#   if(getURL(heightURL)=="No sites/data found using the selection criteria specified \n"){
#     heightDat <- NA
#   }else{
#     heightDat <- usgs_dataRetrieveVar(heightURL, "America/New_York", "full")
#   }
#   
#   
#   # Export a plot from the discharge data.
#   png(file=paste0(plotOut, "Fig_", statID, ".png"), family="sans", units="in", width=plotW, height=plotH, pointsize=14, res=300)
#   par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,2.5))
#   
#   if(all(is.na(dischargeDat)) && all(is.na(heightDat))){    # No data is available
#     # During cold weather, stage and discharge values may be affected by ice at some
#     # streamgages. Streamgages experiencing ice conditions will have the discharge record
#     # temporarily disabled to prevent the display of erroneous discharge values. Display of
#     # discharge record will resume when it is determined that ice conditions are no longer
#     # present. Adjustment of data affected by ice can only be done after detailed analysis.
#     plot(0, xaxt="n", yaxt="n", bty="n", pch="", ylab="", xlab="")
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="snow")
#     legend("center", "No data available", bg="white")
#     
#   }else if(all(is.na(dischargeDat))==FALSE && all(is.na(heightDat))){    # Only discharge data is available
#     # Extract daily mean statistic for days of the current week
#     dailyAveURL <- paste0('https://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=', statID, '&parameterCd=00060&statReportType=daily')
#     if(getURL(dailyAveURL)=="# //Output-Format: RDB\n# //Response-Status: OK\n# //Response-Message: No sites found matching all criteria\n"){
#       daily_avgQ <- NA
#     }else{
#       daily_avgQ <- usgs_dataRetrieveVar(dailyAveURL, "America/New_York", "daily")
#     }
#     
#     # Create discharge plot.
#     plot(dischargeDat$datetime, dischargeDat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n",
#          col="#018571", col.ticks="#018571", col.axis="#018571", ylim=c(min(daily_avgQ, min(dischargeDat$var, na.rm=TRUE), na.rm=TRUE),
#                                                                         max(daily_avgQ, max(dischargeDat$var, na.rm=TRUE), na.rm=TRUE)))
#     
#     mtext(2, text=expression("Discharge"~(ft^3/s)), line=1.25, col="#018571")
#     # Color the background light gray.
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
#     
#     # Set axis ticks to midnight and labels to noon.
#     axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
#     axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
#     
#     # Add a grid.
#     grid(NA, NULL, lty = 6, col = "gray")
#     abline(v = day_midnight, lty = 6, col = "gray")
#     # Add the data.
#     lines(dischargeDat$datetime, dischargeDat$var, lwd=2, col="#018571") # steelblue
#     
#     # Add climate average
#     abline(h=daily_avgQ, lty=3, lwd=2, col="#018571")
#     
#   }else if(all(is.na(dischargeDat)) && all(is.na(heightDat))==FALSE ){    # Only gauge height data is available
#     # Create gage height plot.
#     plot(heightDat$datetime, heightDat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n", yaxt="n")
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
#     
#     axis(4, col="#a6611a", col.ticks="#a6611a", col.axis="#a6611a")
#     mtext(4, text="Gage height (ft)", line=1.25, col="#a6611a")
#     
#     # Set axis ticks to midnight and labels to noon.
#     axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
#     axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
#     
#     # Add a grid.
#     grid(NA, NULL, lty = 6, col = "gray")
#     abline(v = day_midnight, lty = 6, col = "gray")
#     # Add the data.
#     lines(heightDat$datetime, heightDat$var, lwd=2, col="#a6611a") # steelblue
#     
#   } else if(all(is.na(dischargeDat))==FALSE && all(is.na(heightDat))==FALSE){    # Discharge and gauge height data are available
#     # Extract daily mean statistic for days of the current week
#     dailyAveURL <- paste0('https://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=', statID, '&parameterCd=00060&statReportType=daily')
#     if(getURL(dailyAveURL)=="# //Output-Format: RDB\n# //Response-Status: OK\n# //Response-Message: No sites found matching all criteria\n"){
#       daily_avgQ <- NA
#     }else{
#       daily_avgQ <- usgs_dataRetrieveVar(dailyAveURL, "America/New_York", "daily")
#     }
#     
#     # Create discharge and gage height plot.
#     plot(dischargeDat$datetime, dischargeDat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n",
#          col="#018571", col.ticks="#018571", col.axis="#018571", ylim=c(min(daily_avgQ, min(heightDat$var, na.rm=TRUE), min(dischargeDat$var, na.rm=TRUE), na.rm=TRUE),
#                                                                         max(daily_avgQ, max(heightDat$var, na.rm=TRUE), max(dischargeDat$var, na.rm=TRUE), na.rm=TRUE)))
#     
#     mtext(2, text=expression("Discharge"~(ft^3/s)), line=1.25, col="#018571")
#     # Color the background light gray.
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
#     
#     # Set axis ticks to midnight and labels to noon.
#     axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
#     axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
#     
#     # Add a grid.
#     grid(NA, NULL, lty = 6, col = "gray")
#     abline(v = day_midnight, lty = 6, col = "gray")
#     # Add the data.
#     lines(dischargeDat$datetime, dischargeDat$var, lwd=2, col="#018571") # steelblue
#     
#     # Add climate average
#     abline(h=daily_avgQ, lty=3, lwd=2, col="#018571")
#     
#     # Add gage height data
#     par(new=TRUE)
#     plot(heightDat$datetime, heightDat$var, lwd=2, col="#a6611a", typ="l",
#          ylab="", xlab="", xaxt="n", yaxt="n")
#     axis(4, col="#a6611a", col.ticks="#a6611a", col.axis="#a6611a")
#     mtext(4, text="Gage height (ft)", line=1.25, col="#a6611a")
#     
#   }else{ # Create empty plot to indicate there may be something wrong with the script
#     plot(c(b.date,e.date), rep(0, 2), xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
#     rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
#     
#     # Add a grid.
#     grid(NA, NULL, lty = 6, col = "gray")
#     legend("center", "Check import script for errors", bg = "white")
#     
#   }
#   dev.off()
# }
# ##########################################################################
# ##########################################################################
# # Function extracting weather data from an XML file.
# parseWW_xml = function(ID){
#   #################
#   #ID <- "OHC023"
#   #ID <- "OHC057"
#   #ID <- "OHC109"
#   #ID <- "NJC013"
#   #################
#   print(ID)
#   #url = paste("https://alerts.weather.gov/cap/wwaatmget.php?x=", ID, "&y=1", sep="")
#   url = paste("https://alerts.weather.gov/cap/wwaatmget.php?x=", ID, "&amp;y=1", sep="")
#   
#   #for(i in 1:25000){
#   #  getData <- GET(url)
#   #  getContent <- getData$content
#   #  makeChar <- rawToChar(getContent)
#   #  makeList <- xmlToList(makeChar)
#   #  holdChar <- makeChar
#   #}
#   
#   # Turn XML data into a list.
#   xml_data <- retry(xmlToList(rawToChar(GET(url)$content)), max=10, delay=60)
#   name <- xml_data$title
#   entry <- xml_data$entry$title
#   link <- xml_data$entry$id
#   
#   if(length(entry)<1){
#     cols = "#00000000" # 100% transparent black
#     
#     time <- xml_data$updated
#     
#     # Reformat time to match the rest of the Marisa data
#     format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "GMT")
#     format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York")
#     
#     OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", entry, "</strong><br/><br/>Last Updated on ", format_time, sep="")
#     
#   }else if(entry == "There are no active watches, warnings or advisories"){
#     cols = "#00000000" # 100% transparent black
#     
#     time <- xml_data$updated
#     
#     # Reformat time to match the rest of the Marisa data
#     format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "GMT")
#     format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York")
#     
#     OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", entry, "</strong><br/><br/>Last Updated on ", format_time, sep="")
#     
#   }else{
#     xml_active <- xmlToList(rawToChar(GET(link)$content))
#     time <- xml_data$updated
#     
#     # Reformat time to match the rest of the Marisa data
#     format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "EST8EDT")
#     format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "EST8EDT")
# 
#     # headline <- xml_active$info$headline
#     event <- xml_active$info$event
#     issued <- xml_active$sent
#     # effective <-xml_active$info$effective
#     expiring <- xml_active$info$expires
#     severity <- xml_active$info$severity
#     description <- xml_active$info$description
#     instructions <- xml_active$info$instruction
#     areas_affected <- xml_active$info$area$areaDesc
# 
#     cols <- as.character(NWS_cols[match(event, NWS_cols[ ,1]), 2])
# 
#     # Reformat time to match the rest of the Marisa data
#     format_issued <- as.POSIXct(str_replace_all(issued, "T", " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
#     format_issued <- format(format_issued, format = "%b %d, %Y %I:%M %p %Z")
#     format_expiring <- as.POSIXct(str_replace_all(expiring, "T", " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
#     format_expiring <- format(format_expiring, format = "%b %d, %Y %I:%M %p %Z")
# 
#     OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", event,
#                 "</strong><br/><strong>Issued: </strong>", format_issued, "<br/><strong>Expires: </strong>", format_expiring,
#                 "<br/><strong>Severity: </strong><br/><br/><strong>Alert: </strong>", description,
#                 "<br/><br/><strong>Instructions: </strong>", instructions, "<br/><br/><strong>Areas affected: </strong>", areas_affected,
#                 "<br/><br/>Last Updated on ", format_time, sep="")
#     OBS<- str_replace_all(OBS, "([\n])([*])", "<br/>*")
#     OBS<- str_replace_all(OBS, "([\n])", " ")
#     OBS<- str_replace_all(OBS, "([\"])", " inches")
#   }
#   return(c(ID, OBS, cols))
# }
# ##########################################################################
# ##########################################################################
# # Function to create plots
# plot_climdiv = function(climate_dat, state, writeDir){
#   #################
#   #climate_dat <- stateDivTabs[[1]]
#   #state <- state_name
#   #writeDir <- wrtDir
#   #################
#   
#   clim_30yrs <- aggregate(climate_dat, by=list(climate_dat$month), FUN=mean, na.rm=T)
#   monthNames <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
#   clim_30yrs$month <- factor(monthNames, levels=monthNames)
#   
#   
#   #years = unique(climate_division$Year)
#   #pcp.yrs = rep(NA, length(years))
#   #for(i in 1:length(years)){
#   #  pcp.yrs[i] = mean(climate_division$PCP[which(climate_division$Year == years[i])])
#   #}
#   #yearly.avg.prcp = mean(pcp.yrs)
#   
#   #clim_30yrs = data.frame(tmax = tmax, tmin = tmin, tavg = tavg, prcp = pcp,
#   #                        month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), month_num = 1:12)
#   #clim_30yrs$month <- factor(clim_30yrs$month, levels = clim_30yrs$month[order(clim_30yrs$month_num)])
#   
#   png(file=paste0(writeDir, "Fig_", gsub(" ","",state), "-", as.numeric(unique(climate_dat$division)), ".png"), family="sans", units="in", width=p.width, height=p.height*2, pointsize=12, res=300)
#   par(mfrow=c(2,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.5))
#   plot(1:12, clim_30yrs$tmax, type="l", lwd = 2, col=temp_cols[3], xaxt="n", yaxt="n",
#        ylab=expression(paste("Temperature (", degree, "F)", sep="")), xlab="",
#        ylim = c(min(clim_30yrs$tmin), max(clim_30yrs$tmax)), bty="l")
#   lines(1:12, clim_30yrs$tmin, type="l", lwd = 2, col=pcp_cols[2])
#   lines(1:12, clim_30yrs$tmpc, type="l", lwd = 2, col="black")
#   axis(2, las=2, tck=-0.025)
#   axis(1, labels=clim_30yrs$month, at=1:12, tck=-0.025)
#   legend("topleft", legend=c("Maximum", "Average", "Minimum"), pch = 15, col = c(temp_cols[3], "black", pcp_cols[2]), ncol=1, bty="n", cex=1)
#   mtext(paste0("(", min(climate_dat$year), "-", max(climate_dat$year), ")"), side = 3, las=1, adj = 1, line=-1)
#   
#   plot.new()
#   vps <- baseViewports()
#   pushViewport(vps$figure)
#   
#   pp = ggplot(clim_30yrs, aes(x=month, y=pcpn)) + geom_bar(stat = "identity", fill=pcp_cols[3], width=0.6) +
#     scale_x_discrete(breaks = clim_30yrs$month[seq(1, length(clim_30yrs$month), by = 2)]) +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line(colour = "black"),
#           axis.text=element_text(size=11, colour = "black"),
#           axis.title=element_text(size=11, colour = "black")) +
#     labs(x="", y = "Precipitation (in)")
#   
#   vp <- viewport(height = unit(1,"npc"), width=unit(1, "npc"),
#                  just = c("left","top"),
#                  y = 1, x = 0)
#   print(pp, vp = vp)
#   dev.off()
# }
# ##########################################################################
# ##########################################################################
# # Function to cycle through climate divisions within a state
# state_climdiv = function(stateNum, fullTab, wrtDir){
#   #################
#   #stateNum <- "36"
#   #fullTab <- fullVarTab
#   #wrtDir <- outDir
#   #################
#   stateTab <- fullTab[fullTab$state==stateNum,]
#   
#   #statediv$Year = substr(statediv$YearMonth, 1, 4)
#   #statediv$Month = substr(statediv$YearMonth, 5, 6)
#   #statediv30yrs = statediv[min(which(statediv$Year == 1988)):max(which(statediv$Year == 2018)), ]
#   latestYr <- max(as.numeric(stateTab$year))
#   seqYrs <- as.character((latestYr-29):latestYr)
#   stateTab30yrs <- stateTab[which(stateTab$year %in% seqYrs),]
#   
#   subVars <- stateTab30yrs[,c("division", "year", "month", "tmax", "tmin", "tmpc", "pcpn")]
#   stateDivTabs <- lapply(unique(subVars$division), function(div_num){subVars[subVars$division==div_num,]})
#   
#   lapply(stateDivTabs, plot_climdiv, state=stateTab$Name, writeDir=wrtDir)
#   #apply(as.array(unique(statediv30yrs$Division)), 1, plot_climdiv, climate_dat=statediv30yrs, state=state_name)
# }
# ##########################################################################
# ##########################################################################
# combineClimDivDat <- function(tabFile){
#   #################
#   #tabFile <- downloadFileNames[1]
#   #tabFile <- downloadFileNames[9]
#   #################
#   #varTab <- read.table(tabFile, header=F, na.strings=c(-9.99, -99.9, -99.99, -9999, -9999.))
#   varTab <- read.table(tabFile, header=F, na.strings=c("-9999.", "-9.99", "-99.99", "-99.90"))
#   varTxt <- sapply(strsplit(tabFile, "_Data/|_download"), "[[", 2)
#   colnames(varTab) <- c("headerInfo", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
#   
#   ##splitting up the header info
#   ##first, checks the number of characters in each header, and if only have 9, add the 0 to the front
#   numChars <- sapply(varTab$headerInfo, nchar)
#   varTab$state <- mapply(function(ent,nCh){substr(ent,1,nCh-8)}, ent=varTab$headerInfo, nCh=numChars)  ##state
#   varTab$state <- sapply(varTab$state, function(txt){if(nchar(txt)==1){
#                                                       return(paste0("0",txt))
#                                                     }else{
#                                                       return(txt)
#                                                     }})
#   varTab$year <- mapply(function(ent,nCh){substr(ent,nCh-3,nCh)}, ent=varTab$headerInfo, nCh=numChars)  ##year
#   #varTab$variable <- mapply(function(ent,nCh){substr(ent,nCh-5,nCh-4)}, ent=varTab$headerInfo, nCh=numChars)  ##variable
#   varTab$division <- mapply(function(ent,nCh){substr(ent,nCh-7,nCh-6)}, ent=varTab$headerInfo, nCh=numChars)  ##division
#   ##split off the original head column
#   varTab <- varTab[,-which(colnames(varTab) %in% "headerInfo")]
#   
#   ##make a melt table (cheesy) to be in the form needed
#   cheesyTable <- melt(varTab, varible.name="month", value.names=vars[1], id.vars=c("division", "state", "year"))
#   colnames(cheesyTable)[which(colnames(cheesyTable) %in% c("variable", "value"))] <-c("month", varTxt)
#   
#   ##just in case the na.strings argument from the read in table
#   #cheesyTable[,5][cheesyTable[,5]==-99.99]
#   #cheesyTable[,5][cheesyTable[,5]==-99.9]
#   #cheesyTable[,5][cheesyTable[,5]==-9999]
#   
#   return(cheesyTable)
# }
##########################################################################
##########################################################################