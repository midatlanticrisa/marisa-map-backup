# --------------------------------------------------------------------------------------------------------------------
# Copyright 2023 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Created: Nov 7, 2023: Diagnostics tests for output files
#
# This script tests whether output files exist and if they were updated in the 
# last 24hrs.
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
library(compiler)
enableJIT(3)
enableJIT(3)

# Location of outout files
outDir = "/var/www/html/rtdatamap/"

# List file and folder names to check
outfolder = "Tide_figs" # c("River_figs", "Tide_figs")
outfiles = c("NDBCbuoys.js", "NOAATideStations.js", "NWSalerts.geojson", 
          "NWSRiverGauges.geojson", "riverObsRecord.RData", "weather/DEMesonet.js",
          "weather/NoaaMetar.js", "weather/PAMesonet.js") #, "weather/WeatherStem.js")

# --------------------------------------------------------------------------------------------------------------------
# Diagnostic functions to test file existence and update times
FileDiag = function(outDir, f.name){
  # Check if file exists
  f.exists = file.exists(paste0(outDir, f.name))
  
  # If there is a file, see when it was last updated
  if(f.exists){
    m.time = file.mtime(paste0(outDir, f.name))
    
    # Was the file updated today?
    f.date = as.Date(m.time)
    u.today = Sys.Date() == f.date
    
    # How long ago was the file updated?
    d.mins = difftime(Sys.time(), m.time, units="mins")
    u.24hrs = d.mins <= 1440
    
  } else {
    m.time = NA     # Time of file modification
    f.date = NA     # File date
    u.today = FALSE # Was the file updated today?
    d.mins = NA     # How long ago the file was updated?
    u.24hrs = FALSE # Was the file updated in the last 24 hours?
  }
  return(data.frame(name = f.name, exist = f.exists, time = m.time, date = f.date, 
                    today = u.today, mins = d.mins, hrs24 = u.24hrs))
}
# ----------------------------------------
FolderDiag = function(outDir, outfolder){
  # Check if folder exists
  f.dir = paste0(outDir, outfolder)
  d.exists = file.exists(f.dir)
  
  # If there is a file, see when it was last updated
  if(d.exists){
    l.files = list.files(f.dir)
    m.time = file.mtime(paste0(f.dir, "/", l.files))
    
    # Was the file updated today?
    f.date = as.Date(m.time)
    u.today = Sys.Date() == f.date
    
    # How long ago was the file updated?
    d.mins = difftime(Sys.time(), m.time, units="mins")
    u.24hrs = d.mins <= 1440

  } else {
    
    l.files = NA    # List files in directory
    m.time = NA     # Time of file modification
    f.date = NA     # File date
    u.today = FALSE # Was the file updated today?
    d.mins = NA     # How long ago the file was updated?
    u.24hrs = FALSE # Was the file updated in the last 24 hours?
    
  }
  return(data.frame(folder = outfolder, name = l.files, exist = d.exists, 
                    time = m.time, date = f.date, today = u.today, mins = d.mins, 
                    hrs24 = u.24hrs))
}
# --------------------------------------------------------------------------------------------------------------------
# Check output files
diagFile = lapply(X = 1:length(outfiles), function(X){FileDiag(outDir, outfiles[X])})
diagDF = do.call(rbind.data.frame, diagFile)

# Check output folders
diagFold = FolderDiag(outDir, outfolder)

anyOld = any(!diagDF$today) & any(!diagDF$hrs24) | any(!diagFold$today) & 
  any(!diagFold$hrs24)

if(anyOld){
  indFile = c(which(!diagDF$today), which(!diagDF$hrs24))
  indFile = unique(indFile)
  
  if(length(indFile)==0){
    DatedFile = NA
  } else {
    DatedFile = diagDF$name[indFile]
  }
  
  indFold = c(which(!diagFold$today), which(!diagFold$hrs24))
  indFold = unique(indFold)
  
  if(length(indFold)==0){
    DatedFold = NA
  } else {
    DatedFold = paste(diagFold$folder[indFold], ":", 
                      diagFold$name[indFold], collapse = ", ")
  }
  
  outDated = c(as.character(DatedFile), as.character(DatedFold))
  outDated = outDated[!is.na(outDated)]
  
  stop(paste("Check:", paste(outDated, collapse = ", ")))
  
} else {
  print("All files up to date!")
}

##########################################################################
# END
##########################################################################
