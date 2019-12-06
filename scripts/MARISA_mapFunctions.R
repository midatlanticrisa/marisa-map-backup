##header


##########################################################################
##########################################################################
# Function extracting weather data from an XML file.
parse_xml = function(id){
  #################
  #id <- weather_stations$id[1]
  #################
  
  xml.url <- paste0('http://w1.weather.gov/xml/current_obs/', id, '.xml')
  
  # Turn XML data into a list.
  xml_data <- xmlToList(rawToChar(GET(xml.url, user_agent("httr (mdl5548@psu.edu)"))$content))
  
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




















