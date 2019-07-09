# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Feb. 5, 2019: Extend country-wide
# Previous edit: June 13, 2018
# Previous edit: April 27, 2017
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
if (!require("RCurl")) { install.packages("RCurl") }
if (!require("XML")) { install.packages("XML") }
if (!require("stringr")) { install.packages("stringr") }
if (!require("readr")) { install.packages("readr") }

library(RCurl)
library(XML)
library(stringr)
library(readr)
library(compiler)
enableJIT(3)
enableJIT(3)

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
if (!file.exists("/home/staff/klr324/marisa.psu.edu/mapdata")){
  dir.create("/home/staff/klr324/marisa.psu.edu/mapdata")
}
# --------------------------------------------------------------------------------------------------------------------
# Create a vector of buoy stations using lists from the NDBC
readr.total <- read_lines("https://www.ndbc.noaa.gov/data/stations/station_table.txt")
total.rows <- length(readr.total)
readr.meta <- readr.total[grep("^#", readr.total)]
meta.rows <- length(readr.meta)
header.names <- gsub(pattern = "# ", "", x= readr.meta[1])
header.names <- strsplit(header.names,"\\|")[[1]]
header.names <- gsub(pattern = " ", "", x=header.names)
data.rows <- total.rows - meta.rows

bouy_list <- strsplit(readr.total[3:total.rows],"\\|")

buoy_dat <- matrix("NA", nrow = data.rows, ncol=4)
for(i in 1:data.rows){ 
  buoy_dat[i,1] <- bouy_list[[i]][1]
  buoy_dat[i,2] <- bouy_list[[i]][5]
  coord = gsub(pattern = " \\(.*$", "", x= bouy_list[[i]][7])
  coords = strsplit(coord," ")[[1]]
  buoy_dat[i,3] <- ifelse(coords[2] == "N", as.numeric(coords[1]), -as.numeric(coords[1]))
  buoy_dat[i,4] <- ifelse(coords[4] == "E", as.numeric(coords[3]), -as.numeric(coords[3]))
}
colnames(buoy_dat) = c("ID", "name", "lat", "lon")
buoy_dat = data.frame(buoy_dat)

# US bounding box based on the farthest points: includes alaska and hawaii
# # http://en.wikipedia.org/wiki/Extreme_points_of_the_United_States#Westernmost
# pointBurrow = 71.388889, -156.479167 #northernmost point
# kaLae = 18.910833, -155.681111 # southern most point
# sailRock = 44.812556, -66.947028 # east
# peakedIsland = 52.920556, -172.437778 #west
west_east_lon = (as.numeric(as.character(buoy_dat$lon)) >= -172.437778) & (as.numeric(as.character(buoy_dat$lon)) <= -66.947028)
north_south_lat = (as.numeric(as.character(buoy_dat$lat)) >= 18.910833) & (as.numeric(as.character(buoy_dat$lat)) <= 71.388889)
rows_in_US = (west_east_lon == TRUE) & (north_south_lat == TRUE)
US_buoys = buoy_dat[rows_in_US, ]

NDBC_buoys <- read.table("https://www.ndbc.noaa.gov/data/stations/buoyht.txt", skip=7)
NDBC_stations <- read.table("https://www.ndbc.noaa.gov/data/stations/cmanht.txt", skip=7)
NDBC_stations[,1] = sapply(as.character(NDBC_stations[,1]), tolower)
non_NDBC_stations <- read.table("https://www.ndbc.noaa.gov/data/stations/non_ndbc_heights.txt", skip=6)

# Remove all buoys outside the US boundary
NDBC_buoys = NDBC_buoys[!is.na(match(NDBC_buoys$V1, US_buoys$ID)), ]
NDBC_stations = NDBC_stations[!is.na(match(NDBC_stations$V1, US_buoys$ID)), ]
non_NDBC_stations = non_NDBC_stations[!is.na(match(non_NDBC_stations$V1, US_buoys$ID)), ]

buoy_data = function(buoys_id){
  # Use the ID to create a URL to the RSS file.
  xml.url     <- paste('https://www.ndbc.noaa.gov/data/latest_obs/', buoys_id, '.rss', sep="")

  # Check if url/ observations exist
  if(url.exists(xml.url) == TRUE) {
    # Use the ID to create a URL to the RSS file.
    script      <- getURL(xml.url)
    
    script <- gsub(pattern = "&", "&amp;", x = script)
    script <- gsub(pattern = "&amp;#", "&#", x = script)
    script <- gsub(pattern = '</rss"', '</rss>"', x = script)
    script <- gsub("\"","\'", script)
    
    # Parse through the data from the RSS file.
    doc         <- xmlParse(script)
    
    # Extract the meteorological data, which is in the 'description' node.  
    buoy_vec <- xpathSApply(doc,'//channel/item/description',xmlValue)
    
    # Extract the name, link, and coordinates which are in the 'title', 'link', and 'georss:point' node.  
    buoy_name <- xpathSApply(doc,'//channel/item/title',xmlValue)
    buoy_link <- xpathSApply(doc,'//channel/item/link',xmlValue)
    buoy_coord <- xpathSApply(doc,'//channel/item/georss:point',xmlValue)
    
    # Remove the '\n' character from the extracted string.
    buoy_obs <- str_replace_all(buoy_vec, "([\n])", "")
    
    buoy_lat = gsub(pattern = " .*$", "", x= buoy_coord)
    buoy_lon = gsub(pattern = "^.* ", "", x= buoy_coord)
    
    update = gsub(pattern = "<strong>Location:</strong>.*$", "", x= buoy_obs)
    removebold = gsub(pattern = "^.*<strong>|</strong><br />", "", x=update)
    buoy_time = paste("Last Updated on ", removebold, sep="")
    
    month = format(Sys.Date(),"%B")
    buoy_obs = gsub(pattern = paste("^.*<strong>", month, ".*</strong><br />", sep=""), "", x= buoy_obs)
    buoy_obs = paste(buoy_obs, "<br />", sep="")
    
   } else {
     buoy_obs <- "There are no current meteorological observations recorded at this buoy."
     buoy_time <- ""
     buoy_link <- paste("http://www.ndbc.noaa.gov/station_page.php?station=", buoys_id, sep="")
     buoy_name <- as.character(US_buoys$name[match(buoys_id, US_buoys$ID)])
     buoy_lat <- as.character(US_buoys$lat[match(buoys_id, US_buoys$ID)])
     buoy_lon <- as.character(US_buoys$lon[match(buoys_id, US_buoys$ID)])
   }
  return(c(buoys_id, buoy_obs, buoy_time, buoy_name, buoy_link, buoy_lat, buoy_lon))
}

NDBC_stat_data = t(sapply(NDBC_stations[ ,1], buoy_data)) # NDBC_stations
NDBC_stat_data = data.frame(NDBC_stat_data, row.names = NDBC_stations[ ,1])
colnames(NDBC_stat_data) <- c("id", "obs", "time", "name", "link", "lat", "lon")

NDBC_buoy_data = t(sapply(NDBC_buoys[ ,1], buoy_data)) # NDBC_buoys
NDBC_buoy_data = data.frame(NDBC_buoy_data, row.names = as.character(NDBC_buoys[ ,1]))
colnames(NDBC_buoy_data) <- c("id", "obs", "time", "name", "link", "lat", "lon")

non_NDBC_data = t(sapply(non_NDBC_stations[ ,1], buoy_data)) # non_NDBC_stations
non_NDBC_data = data.frame(non_NDBC_data, row.names = non_NDBC_stations[ ,1])
colnames(non_NDBC_data) <- c("id", "obs", "time", "name", "link", "lat", "lon")

# --------------------------------------------------------------------------------------------------------------------

buoy_string = function(ID, name, link, obs, time, lon, lat){
    str = paste('{"type": "Feature", "properties": {"name": "', name[match(ID, ID)], '", "id": "', ID, '", "url": "', link[match(ID, ID)], '", "obs": "', 
                obs[match(ID, ID)], '", "time": "', time[match(ID, ID)], '"}, "geometry": {"type": "Point", "coordinates": [', 
                lon[match(ID, ID)], ',',  lat[match(ID, ID)], ']}}', sep="")
  return(str)
}

# Combine all buoy info into one string
ndbc_st = buoy_string(NDBC_stat_data$id, NDBC_stat_data$name, NDBC_stat_data$link, NDBC_stat_data$obs, NDBC_stat_data$time, 
                         NDBC_stat_data$lon, NDBC_stat_data$lat)
ndbc_st = paste(ndbc_st, ",", collapse="")

ndbc_bu = buoy_string(NDBC_buoy_data$id, NDBC_buoy_data$name, NDBC_buoy_data$link, NDBC_buoy_data$obs, NDBC_buoy_data$time, 
                      NDBC_buoy_data$lon, NDBC_buoy_data$lat)
ndbc_bu = paste(ndbc_bu, ",", collapse="")

ndbc_non = buoy_string(non_NDBC_data$id, non_NDBC_data$name, non_NDBC_data$link, non_NDBC_data$obs, non_NDBC_data$time, 
                       non_NDBC_data$lon, non_NDBC_data$lat)
ndbc_non_last = ndbc_non[length(ndbc_non)] #make sure the last feature doesn't end with a ","
ndbc_non = paste(ndbc_non[1:length(ndbc_non) - 1], ",", collapse="")

# Create a geojson object with the observation and statement info and merge into a 
# specific file format with Buoys as the variable name.
json_merge = paste('Buoys = {"type": "FeatureCollection","features": [', ndbc_st, ndbc_bu, ndbc_non, ndbc_non_last, ']};', sep="")

# Export data to geojson.
# cat(json_merge, file="buoys_extend2.js")

# Export data to geojson.
cat(json_merge, file="/home/staff/klr324/marisa.psu.edu/mapdata/buoys_extend.js")
# --------------------------------------------------------------------------------------------------------------------
