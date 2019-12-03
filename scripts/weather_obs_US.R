# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: April 4, 2017; April 14 (change to creating geojson file)
#
# This script parses XML data of current weather station observations from the
# National Weather Service and outputs the results in a single file.
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
if (!require("XML")) { install.packages("XML") }
if (!require("httr")) { install.packages("httr") }

library(XML)
library(httr)

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
#if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata")){
#  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata")
#}
# --------------------------------------------------------------------------------------------------------------------
# Read in station IDs
weather_stations <- read.csv("/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/current_weather_stations.csv", header = FALSE, col.names = c("name", "id"))

# Function extracting weather data from an XML file.
parse_xml = function(id){
  xml.url     <- paste('http://w1.weather.gov/xml/current_obs/', id, '.xml', sep="")

  # Turn XML data into a list.
  xml_data <- xmlToList(rawToChar(GET(xml.url)$content))

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
    weather <- paste("<strong>Weather: </strong>", xml_data$weather, "<br/>", sep="")
  }
  # Air temperature in F.
  if(is.null(xml_data$temp_f)){
    temp <- ""
  } else {
    temp <- paste("<strong>Temperature: </strong>", xml_data$temp_f, " &#8457;<br/>", sep="")
  }
  # Relative humidity in %.
  if(is.null(xml_data$relative_humidity)){
    humidity <- ""
  } else {
    humidity <- paste("<strong>Relative humidity: </strong>", xml_data$relative_humidity, " %<br/>", sep="")
  }
  # Which direction the wind is blowing.
  if(is.null(xml_data$wind_string)){
    wind <- ""
  } else {
    wind <- paste("<strong>Wind: </strong>", xml_data$wind_string, " <br/>", sep="")
  }
  # Pressure in mb.
  if(is.null(xml_data$pressure_mb)){
    speed <- ""
  } else {
    speed <- paste("<strong>Pressure: </strong>", xml_data$pressure_mb, " mb<br/>", sep="")
  }
  # Dew point in F.
  if(is.null(xml_data$dewpoint_f)){
    dewpoint <- ""
  } else {
    dewpoint <- paste("<strong>Dewpoint: </strong>", xml_data$dewpoint_f, " &#8457;<br/>", sep="")
  }
  # Windchill in F.
  if(is.null(xml_data$windchill_f)){
    windchill <- ""
  } else {
    windchill <- paste("<strong>Wind chill: </strong>", xml_data$windchill_f, " &#8457;<br/>", sep="")
  }
  # Current visibility in miles.
  if(is.null(xml_data$visibility_mi)){
    visibility <- ""
  } else {
    visibility <- paste("<strong>Visibility: </strong>", xml_data$visibility_mi, " miles<br/>", sep="")
  }
  # Observation url.
  if(is.null(xml_data$ob_url)){
    link <- ""
  } else {
    link <- xml_data$ob_url
  }

  obs = paste(weather, temp, humidity, wind, speed, dewpoint, windchill, visibility, sep="")

  # Return the weather variables
  return(c(name, as.character(id), latitude, longitude, obs, link, time))
}

# Run through each station
weather_stat_data = t(sapply(weather_stations[ ,2], parse_xml))
weather_stat_data = data.frame(weather_stat_data, row.names = weather_stations[ ,2])
colnames(weather_stat_data) <- c("name", "id", "lat", "lon", "obs", "link", "time")

# Remove stations without latitude or longitude
weather_stat_data = weather_stat_data[!is.na(weather_stat_data$lat), ]
weather_stat_data = weather_stat_data[!is.na(weather_stat_data$lon), ]

# --------------------------------------------------------------------------------------------------------------------
# Format information to a feature string for json
weather_string = function(ID, name, link, obs, time, lon, lat){
  str = paste('{"type": "Feature", "properties": {"name": "', name[match(ID, ID)], '", "id": "', ID, '", "url": "', link[match(ID, ID)], '", "obs": "',
              obs[match(ID, ID)], '", "time": "', time[match(ID, ID)], '"}, "geometry": {"type": "Point", "coordinates": [',
              lon[match(ID, ID)], ',',  lat[match(ID, ID)], ']}}', sep="")
  return(str)
}
# --------------------------------------------------------------------------------------------------------------------
# Combine all info into one string
weat_obs = weather_string(weather_stat_data$id, weather_stat_data$name, weather_stat_data$link, weather_stat_data$obs, weather_stat_data$time,
                          weather_stat_data$lon, weather_stat_data$lat)
weat_obs_last = weat_obs[length(weat_obs)] #make sure the last feature doesn't end with a ","
weat_obs = paste(weat_obs[1:length(weat_obs) - 1], ",", collapse="")

# Create geojson objects with the information.
# Merge geojson objects into a specific file format with data as the variable name.
json_merge = paste('weatherStations = {"type": "FeatureCollection","features": [', weat_obs, weat_obs_last, ']};', sep="")

# Export data to geojson.
# cat(json_merge, file="weather_observations_extend.json")

# --------------------------------------------------------------------------------------------------------------------

# # Export data to geojson.
cat(json_merge, file="/net/www/www.marisa.psu.edu/htdocs/mapdata/weather_observations_extend.json")
# --------------------------------------------------------------------------------------------------------------------
