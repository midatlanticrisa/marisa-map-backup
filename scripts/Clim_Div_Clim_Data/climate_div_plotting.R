# --------------------------------------------------------------------------------------------------------------------
# Copyright 2019 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: April 16, 2019; Created
#
# This script creates temperature and precipitation plots of the past 30 year
# (1988 - 2018) average for each climate division.
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
if (!require("RColorBrewer")) { install.packages("RColorBrewer") }
if (!require("ggplot2")) { install.packages("ggplot2") }
if (!require("gridBase")) { install.packages("gridBase") }
if (!require("grid")) { install.packages("grid") }

library(RColorBrewer)
library(ggplot2)
library(gridBase)
library(grid)
library(curl)
library(reshape2)

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/Clim_div_figs/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/Clim_div_figs/"
}else{  ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/Clim_div_figs/"
}

##sourced functions
source(paste0(inDir, "MARISA_mapFunctions.R"))

# Files are saved to a directory called Clim_div_figs in mapdata. Create these directories if they don't exist
if (!file.exists(outDir)){
  dir.create(outDir)
}
downDir <- paste0(inDir, "Clim_Div_Clim_Data/")
if (!file.exists(downDir)){
  dir.create(downDir)
}

# Set variables for colors.
temp_cols = brewer.pal(3, "Reds")
pcp_cols = brewer.pal(3, "Blues")

# Set variables for determining plot size.
p.width = 4            # Width
p.height = 2.5           # Height

# Run through each state.
#state_list = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois",
#               "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
#               "Missouri", "Montana", "Nebraska", "Nevada", "NewHampshire", "NewJersey", "NewMexico", "NewYork", "NorthCarolina", "NorthDakota",
#               "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "RhodeIsland", "SouthCarolina", "SouthDakota", "Tennessee", "Texas", "Utah", "Vermont",
#               "Virginia", "Washington", "WestVirginia", "Wisconsin", "Wyoming")
#state_list <- c("Delaware", "Maryland", "Virginia", "WestVirginia", "Pennsylvania", "Ohio", "NewYork", "Massachusetts", "Vermont", "Connecticut")
state_list <- c("07", "18", "44", "46", "36", "33", "30", "28")
#apply(as.array(state_list), 1, state_climdiv)

##read in FIPS translator
fipsTrans <- read.csv(paste0(downDir, "stateFIPS.csv"))
fipsTrans <- fipsTrans[which(is.na(fipsTrans$ClimDatStateCode)==F),]
fipsTrans <- fipsTrans[,c("ClimDatStateCode", "Name")]
fipsTrans$ClimDatStateCode <- as.character(fipsTrans$ClimDatStateCode)
fipsTrans$ClimDatStateCode <- sapply(fipsTrans$ClimDatStateCode, function(txt){if(nchar(txt)==1){
                                                                                return(paste0("0",txt))
                                                                              }else{
                                                                                return(txt)
                                                                              }})

##collect data from ftp site
url <- "ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/"
h <- new_handle(dirlistonly=T)
con <- curl(url, "r", h)
tbl <- read.table(con, stringsAsFactors=T, fill=T)
close(con)

##find and download the data in order to make the required files
##variables for the files to download
vars <- c("pcpn", "tmpc", "tmin", "tmax", "pdsi", "phdi", "zndx", "pmdi", "cddc", "hddc", "sp01", "sp02", "sp03", "sp06", "sp09", "sp12", "sp24")
addText <- paste0("climdiv-", vars, "dv-v")
varIndex <- sapply(addText, grep, x=tbl$V1)
##collect find name, with variation such as version and date updated
varFileName <- paste0(url, tbl$V1[sapply(addText, grep, x=tbl$V1)])
##download file names
downloadFileNames <- paste0(downDir, vars, "_download.txt")

#curl_download(varFileName[1], downloadFileNames[1])
mapply(curl_download, url=varFileName, destfile=downloadFileNames)

##read in and format downloaded data in order to merge together together 
formatVarTabs <- lapply(downloadFileNames, combineClimDivDat)
fullVarTab <- Reduce(function(dtf1, dtf2){merge(dtf1, dtf2, by=c("division","state","year","month"), all.x=T)}, formatVarTabs)
fullVarTab <- merge(x=fullVarTab, y=fipsTrans, by.x="state", by.y="ClimDatStateCode", all.y=F)


##create plots
lapply(state_list, state_climdiv, fullTab=fullVarTab, wrtDir=outDir)








