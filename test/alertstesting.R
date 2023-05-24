library(jsonlite)
library(terra)
library(sf)
library(geojsonsf)

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

collectWarningsAlerts(area=c("VA", "PA", "MD", "DE", "DC", "WV", "OH"), 
                      colorfile = "WeatherEventColors.csv",
                      cntyShp = "data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp",
                      coastalShp = "data/mz08mr23/mz08mr23.shp",
                      offshoreShp = "data/oz08mr23/oz08mr23.shp",
                      outfile = "appended.geojson")
