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
if (!require("stringr")) { install.packages("stringr") }

library(XML)
library(httr)
library(stringr)
# --------------------------------------------------------------------------------------------------------------------
# Function extracting weather data from an XML file.
parse_xml = function(ID){
  #################
  #ID <- "OHC023"
  #################
  print(ID)
  url = paste("https://alerts.weather.gov/cap/wwaatmget.php?x=", ID, "&y=1", sep="")

  # Turn XML data into a list.
  xml_data <- xmlToList(rawToChar(GET(url)$content))
  print("collected data")
  name <- xml_data$title
  entry <- xml_data$entry$title
  link <- xml_data$entry$id
  print("parsed data")

  if(entry == "There are no active watches, warnings or advisories"){
    print("no warnings")
    cols = "#00000000" # 100% transparent black

    time <- xml_data$updated

    # Reformat time to match the rest of the Marisa data
    format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "GMT")
    format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York")

    OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", entry, "</strong><br/><br/>Last Updated on ", format_time, sep="")

  } else {
    print("active warning")
    xml_active <- xmlToList(rawToChar(GET(link)$content))
    print("parse content")
    time <- xml_data$updated
    
    # Reformat time to match the rest of the Marisa data
    format_time <- as.POSIXct(str_replace_all(time, "T", " "), format = "%Y-%m-%d %H:%M", tz = "EST8EDT")
    format_time <- format(format_time, format = "%b %d, %Y %I:%M %p %Z", tz = "EST8EDT")
    print("updated time")
    # headline <- xml_active$info$headline
    event <- xml_active$info$event
    issued <- xml_active$sent
    # effective <-xml_active$info$effective
    expiring <- xml_active$info$expires
    severity <- xml_active$info$severity
    description <- xml_active$info$description
    instructions <- xml_active$info$instruction
    areas_affected <- xml_active$info$area$areaDesc
    print("parse data some more")
    cols <- as.character(NWS_cols[match(event, NWS_cols[ ,1]), 2])
    print("formatted output")
    # Reformat time to match the rest of the Marisa data
    format_issued <- as.POSIXct(str_replace_all(issued, "T", " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
    format_issued <- format(format_issued, format = "%b %d, %Y %I:%M %p %Z")
    format_expiring <- as.POSIXct(str_replace_all(expiring, "T", " "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
    format_expiring <- format(format_expiring, format = "%b %d, %Y %I:%M %p %Z")
    print("formatted time")
    OBS<- paste("<strong><a href='", link, "' target='_blank'>", name, "</a></strong><br/><br/><strong>", event,
                "</strong><br/><strong>Issued: </strong>", format_issued, "<br/><strong>Expires: </strong>", format_expiring,
                "<br/><strong>Severity: </strong><br/><br/><strong>Alert: </strong>", description,
                "<br/><br/><strong>Instructions: </strong>", instructions, "<br/><br/><strong>Areas affected: </strong>", areas_affected,
                "<br/><br/>Last Updated on ", format_time, sep="")
    print("create observation")
    OBS<- str_replace_all(OBS, "([\n])([*])", "<br/>*")
    OBS<- str_replace_all(OBS, "([\n])", " ")
    OBS<- str_replace_all(OBS, "([\"])", " inches")
    print("observation set")
  }
  return(c(ID, OBS, cols))
}
# --------------------------------------------------------------------------------------------------------------------
