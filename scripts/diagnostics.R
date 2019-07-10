# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 19, 2018; check new features
# Creation: July 3, 2017
#
# This script is a diagnostic test to determine whether the Marisa map up to date. 
# The script simple tests whether the plotting files were created 'today' or 'yesterday'
# in the case of the stream gauges (they are updated twice a day). Based on when the files
# were updated the Marisa map will display a green or red dot in the bottom right corner of
# the map. Green means the map files are up to date, while red means there is an issue in the
# R code used to extract data.
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
# Set variables to today's and yesterday's date.
date_today <- Sys.Date()
date_yesterday <- Sys.Date() - 1

# --------------------------------------------------------------------------------------------------------------------
# Extract when the buoy json file was last created.
buoy_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/buoys_extend.js"
buoy_times <- file.info(buoy_file)$ctime
buoy_Dates <- as.Date(buoy_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
buoy_diag <- buoy_Dates == date_today

if (buoy_diag == FALSE){
  print("check buoys_v3.js")
}

# Extract when the weather observation json file was last created.
weather_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/weather_observations_extend.json"
weather_times <- file.info(weather_file)$ctime
weather_Dates <- as.Date(weather_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
weather_diag <- weather_Dates == date_today

if (weather_diag == FALSE){
  print("check weather_observations_v2.json")
}

# Extract when all the tide station plots were last created.
tide_dir <- "/home/staff/klr324/marisa.psu.edu/mapdata/Tide_figs"
tide_list <- list.files(tide_dir)
tide_files <- tide_list
for(i in 1:length(tide_list)){
  tide_files[i] <- paste(tide_dir, "/", tide_list[i], sep="")
}
tide_times <- file.info(tide_files)$ctime
tide_Dates <- as.Date(tide_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
tide_diag <- all(tide_Dates == date_today)

if (tide_diag == FALSE){
  print("check Tide_figs")
}

# Extract when the tide station json file was last created.
tide_json_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/tide_station_obs_extend.js"
tide_json_times <- file.info(tide_json_file)$ctime
tide_json_Dates <- as.Date(tide_json_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
tide_json_diag <- tide_json_Dates == date_today

if (tide_json_diag == FALSE){
  print("check tide_station_obs.js")
}

# Extract when all the stream gauge plots were last created.
stream_dir <- "/home/staff/klr324/marisa.psu.edu/mapdata/Stream_figs"
stream_list <- list.files(stream_dir)
stream_files <- stream_list
for(i in 1:length(stream_list)){
  stream_files[i] <- paste(stream_dir, "/", stream_list[i], sep="")
}
stream_times <- file.info(stream_files)$ctime
stream_Dates <- as.Date(stream_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's or yesterday's date.
stream_diag <- all(stream_Dates == date_today | stream_Dates == date_yesterday)

if (stream_diag == FALSE){
  print("check Stream_figs")
}

# Extract when the stream gage json file was last created.
njmdstream_json_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/NJMD_stream_obs.js"
njmdstream_json_times <- file.info(njmdstream_json_file)$ctime
njmdstream_json_Dates <- as.Date(njmdstream_json_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
njmdstream_json_diag <- njmdstream_json_Dates == date_today

if (njmdstream_json_diag == FALSE){
  print("check NJMD_stream_obs.js")
}

# Extract when the stream gage json file was last created.
nystream_json_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/NY_stream_obs.js"
nystream_json_times <- file.info(nystream_json_file)$ctime
nystream_json_Dates <- as.Date(nystream_json_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
nystream_json_diag <- nystream_json_Dates == date_today

if (nystream_json_diag == FALSE){
  print("check NY_stream_obs.js")
}

# Extract when the stream gage json file was last created.
ohstream_json_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/OHDEDCWVTNNCCTMA_stream_obs.js"
ohstream_json_times <- file.info(ohstream_json_file)$ctime
ohstream_json_Dates <- as.Date(ohstream_json_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
ohstream_json_diag <- ohstream_json_Dates == date_today

if (ohstream_json_diag == FALSE){
  print("check OHDEDCWVTNNCCTMA_stream_obs.js")
}

# Extract when the stream gage json file was last created.
pastream_json_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/PA_stream_obs.js"
pastream_json_times <- file.info(pastream_json_file)$ctime
pastream_json_Dates <- as.Date(pastream_json_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
pastream_json_diag <- pastream_json_Dates == date_today

if (pastream_json_diag == FALSE){
  print("check PA_stream_obs.js")
}

# Extract when the stream gage json file was last created.
vastream_json_file <- "/home/staff/klr324/marisa.psu.edu/mapdata/VA_stream_obs.js"
vastream_json_times <- file.info(vastream_json_file)$ctime
vastream_json_Dates <- as.Date(vastream_json_times, format = "%y-%m-%d")
# Detemine whether the creation date matches today's date.
vastream_json_diag <- vastream_json_Dates == date_today

if (vastream_json_diag == FALSE){
  print("check VA_stream_obs.js")
}

# --------------------------------------------------------------------------------------------------------------------
# If all files are up to date then create a json file recording passing as true and print that the test passed.
# If the test fails, the json file will record false for the property "pass" and will print that the test failed.
if (all(c(buoy_diag, weather_diag, tide_diag, tide_json_diag, stream_diag,
 njmdstream_json_diag, nystream_json_diag, ohstream_json_diag, pastream_json_diag, vastream_json_diag) == TRUE)){
  print("Automated files PASS diagnostic; Marisa map up to date.")
  json_merge = 'diagnostic = [{"type": "Feature","properties": {"name": "Diagnostic test", "pass": "true"}, "geometry": {"type": "Point", "coordinates": [-73.4, 36.7]}}];'
} else {
  print("Automated files FAIL diagnostic; Check scripts for potential errors.")
  json_merge = 'diagnostic = [{"type": "Feature","properties": {"name": "Diagnostic test", "pass": "false"}, "geometry": {"type": "Point", "coordinates": [-73.4, 36.7]}}];'
  system2('echo "Automated files FAIL diagnostic; Check scripts for potential errors." | mail -s "Error: check Marisa map scripts" klr324@psu.edu')
}

# --------------------------------------------------------------------------------------------------------------------
# Export data to geojson.
cat(json_merge, file="/home/staff/klr324/marisa.psu.edu/mapdata/diagnostic.json")

# --------------------------------------------------------------------------------------------------------------------


