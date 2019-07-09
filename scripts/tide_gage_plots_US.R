# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Jan 25, 2019    - expanded to entire US
# prior edit: June 16, 2017
#
# This script parses XML data of current tide station observations from the 
# National Ocean and Atmospheric Administration and outputs the results as
# a figure of preliminery 6-minute water level heights.
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
if (!require("httr")) { install.packages("httr") }

library(RCurl)
library(XML)
library(httr)

# Files are saved to a directory called Tide_figs in mapdata. Create these directories if they don't exist
if (!file.exists("/home/staff/klr324/marisa.psu.edu/mapdata")){
  dir.create("/home/staff/klr324/marisa.psu.edu/mapdata")
}
if (!file.exists("/home/staff/klr324/marisa.psu.edu/mapdata/Tide_figs")){
  dir.create("/home/staff/klr324/marisa.psu.edu/mapdata/Tide_figs")
}
# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width   = 4            # Width
p.height = 2.5           # Height

# Extract the dates of this previous week for use in the url.
e.date = format(Sys.Date(), "%Y%m%d")       # End date
b.date = format(Sys.Date() - 2, "%Y%m%d")  # Beginning date
datum = "MLLW"
gl.datum = "IGLD"
msl.datum = "MSL"
timezone = "GMT"
units = "metric"

# Determine midnight and noon for dates of this previous week
day = 0:7
day_midnight = as.POSIXct(paste(Sys.Date() - day, "00:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")
day_noon = as.POSIXct(paste(Sys.Date() - day, "12:00", sep=" "), format = "%Y-%m-%d %H:%M", tz = "America/New_York")

# --------------------------------------------------------------------------------------------------------------------
# Create vector including each station ID.
# <!-- TIDE STATIONS -->
AL_ID = c("8735523", "8737048", "8738043", "8737138", "8739803", "8732828", "8735180", "8735391", "8736897")
AK_ID = c("9454240", "9457292", "9462450", "9468756", "9491094", "9468333", "9462620", "9461380", "9457804",
          "9455920", "9454050", "9453220", "9452634", "9452210", "9451600", "9451054", "9450460", "9452400", 
          "9455090", "9455500", "9455760", "9459450", "9459881", "9461710", "9464212", "9497645") # not available: "9452635", "9456173", "9458705" 
CA_ID = c("9410170", "9410230", "9411406", "9414290", "9415144", "9419750", "9416841", "9415102", "9415020",
          "9414958", "9414575", "9413450", "9412110", "9411340", "9410660", "9410840", "9414523", "9414750", "9414863",
          "9418767")
CT_ID = c("8461490", "8467150", "8465705")
# <!--Delaware tide stations-->
DE_ID = c("8555889", "8551762", "8557380", "8551910")
DC_ID = "8594900"
FL_ID = c("8720219", "8726384", "8726520", "8726667", "8726724", "8729210", "8729840", "8725520", "8725110",
          "8723970", "8723214", "8722956", "8722670", "8720625", "8720030", "8720215", "8720218", "8720226", "8721604",
          "8724580", "8726607", "8727520", "8728690", "8729108")
GA_ID = "8670870"
HI_ID = c("1612480", "1617760", "1611400", "1615680", "1612340", "1617433")
LA_ID = c("8760721", "8760922", "8761305", "8767961", "8768094", "8766072", "8762075",
          "8761724", "8761927", "8762483", "8764044", "8764227", "8764314", "8767816") 
LA_MSL_ID = c("8761955", "8762482") # MSL datum
ME_ID = c("8410140", "8419317", "8418150", "8413320", "8411060") # c("8419751", "8414821") # error no longer supported
# <!--Maryland tide stations-->
MD_ID = c("8575512", "8574680", "8571421", "8571892", "8573927", "8570283", "8577330", "8573364")
MA_ID = c("8443970", "8447386", "8449130", "8447930", "8447435") # error "8447506" # no longer supported
MS_ID = c("8740166", "8741533", "8747437")
NH_ID = "8423898"
# <!--New Jersey tide stations-->
NJ_ID = c("8534720", "8539094", "8536110", "8531680", "8537121")
# <!--New York tide stations-->
NY_ID = c("8519483", "8516945", "8518750", "8518962", "8510560")
NC_ID = c("8651370", "8652587", "8654467", "8656483", "8658120", "8658163")
OR_ID = c("9439201", "9439099", "9431647", "9432780", "9435380", "9437540", "9439040")
# <!--Pennsylvania tide stations-->
PA_ID = c("8546252", "8548989", "8545240", "8540433")
RI_ID = c("8452660", "8454000", "8452944", "8454049")
SC_ID = c("8665530", "8661070", "8662245")
TX_ID = c("8770475", "8770570", "8770613", "8770808", "8770971", "8771972", "8772447", "8772985", "8773767", "8775237",
          "8775244", "8775296", "8775792", "8779280", "8779770", "8779748", "8775870", "8774770",
          "8773259", "8773037", "8772471", "8771450", "8771013", "8770822", "8770520", "8770777", "8771341", "8771486",
          "8773146", "8773701", "8774230", "8775241", "8779749") # "8771801" not supported
TX_MSL_ID = c("8776604", "8777812", "8778490", "8776139")
# <!--Virginia tide stations-->
VA_ID = c("8638863", "8635027", "8632200", "8635750", "8639348", "8638610", "8631044", "8636580", "8637689", "8638901")
WA_ID = c("9440422", "9440581", "9442396", "9443090", "9444900", "9449880", 
          "9444090", "9440910", "9440569", "9440083", "9441102", "9446484", "9447130", "9449424") # c("9447112", "9446500", "9447113", "9447114", "9447115", "9449896", 9449712") # wa error no longer supported
# <!--Great Lakes: Lake Erie tide stations-->
GL_ID = c("9063020", "9063063", "9063038", "9063053", "9063028", "9063085", "9063079")
GLDR_ID = c("9044020", "9044036", "9044030", "9044049") 
GLLH_ID = c("9075002", "9075065", "9075014", "9075035", "9075099", "9075080") 
GRLM_ID = c("9087031", "9087044", "9087072", "9087079", "9087096", "9087023", "9087088", "9087057", "9087068")
GRLO_ID = c("9052000", "9052030", "9052090", "9052058", "9052025", "9052076")
GLLSC_ID = c("9034052", "9034057")
GLLS_ID = c("9099004", "9099018", "9099064", "9099044", "9099090")
GLNR_ID = c("9063007", "9063012", "9063009")
GLSCR_ID = c("9014070", "9014080", "9014087", "9014096", "9014090", "9014098")
GLSLR_ID = c("8311062", "8311030")
GLSMR_ID = c("9076024", "9076060", "9076027", "9076033", "9076070")

# --------------------------------------------------------------------------------------------------------------------
# Function extracting tide data (hight and time) from a XML file online.
waterheight_plot = function(ID, b.date, e.date, datum, timezone, units, day_midnight, day_noon){ 
  # Use the ID, b.date, e.date, datum, timezone, and units to create a URL to the XML file.
  url = paste('https://tidesandcurrents.noaa.gov/api/datagetter?product=water_level&application=NOS.COOPS.TAC.WL&begin_date=', 
              b.date, '&end_date=', e.date, '&datum=', datum, '&station=', ID, '&time_zone=', timezone, '&units=', units, 
              '&format=xml', sep="")
  
  xml_data <- xmlToList(rawToChar(GET(url)$content))
  test = t(data.frame(xml_data$observations))
  
  if(is.logical(test) == TRUE){
    # Create plot with no data
    png(file=paste("/home/staff/klr324/marisa.psu.edu/mapdata/Tide_figs/Fig_", ID, ".png", sep=""), family="Helvetica", units="in", width=p.width, height=p.height, pointsize=14, res=300)
    par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
    # plot.new()
    plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    legend("center", "No data available", bg = "white")
    dev.off()
    
  } else if (is.null(xml_data$observations$disclaimers) == FALSE) {
    
    remove <- c("disclaimers.disclaimer.text", "disclaimers.disclaimer..attrs")
    test <- test[!rownames(test) %in% remove, ]
    
    values <- as.numeric(test[ ,2])
    
    date <- as.POSIXct(test[ ,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
    date <- format(date, tz = "America/New_York") # convert from GMT to current time zone
    date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M", tz = "")
    
    # Determine which indices of the date occur at midnight and noon.
    #     hours <- strftime(date, format="%H:%M")
    #     midnight = which(hours == "00:00")
    #     noon = which(hours == "12:00")
    
    png(file=paste("/home/staff/klr324/marisa.psu.edu/mapdata/Tide_figs/Fig_", ID, ".png", sep=""), family="Helvetica", units="in", width=p.width, height=p.height, pointsize=14, res=300)
    par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
    
    plot(date, values, type = "n", ylab = paste("Height (m ", datum, ")", sep=""), xlab="Past 3 days", xaxt="n")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    
    grid(NA, NULL, lty = 6, col = "gray") 
    abline(v = day_midnight, lty = 6, col = "gray")
    lines(date, values, lwd=2, col="steelblue")
    dev.off()
    
  } else {
    
    values <- as.numeric(test[ ,2])
    
    date <- as.POSIXct(test[ ,1], format = "%Y-%m-%d %H:%M", tz = "GMT")
    date <- format(date, tz = "America/New_York") # convert from GMT to current time zone
    date <- as.POSIXct(date, format = "%Y-%m-%d %H:%M", tz = "")
    
    # Determine which indices of the date occur at midnight and noon.
    hours <- strftime(date, format="%H:%M")
    midnight = which(hours == "00:00")
    noon = which(hours == "12:00")
    
    png(file=paste("/home/staff/klr324/marisa.psu.edu/mapdata/Tide_figs/Fig_", ID, ".png", sep=""), family="Helvetica", units="in", width=p.width, height=p.height, pointsize=14, res=300)
    par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.25))
    
    plot(date, values, type = "n", ylab = paste("Height (m ", datum, ")", sep=""), xlab="Past 3 days", xaxt="n")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)
    
    grid(NA, NULL, lty = 6, col = "gray") 
    abline(v = day_midnight, lty = 6, col = "gray")
    lines(date, values, lwd=2, col="steelblue")
    dev.off()
  }
  # }
}

# -------------------------------------------------------------------------------------------------------------------- 
# Run the function extracting the data we want and creating a plot.
# Run through each Alabama station.
for(i in 1:length(AL_ID)){ waterheight_plot(AL_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Alaska station.
for(i in 1:length(AK_ID)){ waterheight_plot(AK_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each California station.
for(i in 1:length(CA_ID)){ waterheight_plot(CA_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Conneticut station.
for(i in 1:length(CT_ID)){ waterheight_plot(CT_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Delaware station.
for(i in 1:length(DE_ID)){ waterheight_plot(DE_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run the District of Columbia station.
waterheight_plot(DC_ID, b.date, e.date, datum, timezone, units, day_midnight, day_noon)

# Run through each Florida station.
for(i in 1:length(FL_ID)){ waterheight_plot(FL_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run the Georgia station.
waterheight_plot(GA_ID, b.date, e.date, datum, timezone, units, day_midnight, day_noon)

# Run through each Hawaii station.
for(i in 1:length(HI_ID)){ waterheight_plot(HI_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Louisiana station.
for(i in 1:length(LA_ID)){ waterheight_plot(LA_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Louisiana MSL station.
for(i in 1:length(LA_MSL_ID)){ waterheight_plot(LA_MSL_ID[i], b.date, e.date, msl.datum, timezone, units, day_midnight, day_noon) }

# Run through each Maine station.
for(i in 1:length(ME_ID)){ waterheight_plot(ME_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Maryland station.
for(i in 1:length(MD_ID)){ waterheight_plot(MD_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Massachussets station.
for(i in 1:length(MA_ID)){ waterheight_plot(MA_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Mississippi station.
for(i in 1:length(MS_ID)){ waterheight_plot(MS_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run the New Hampshire station.
waterheight_plot(NH_ID, b.date, e.date, datum, timezone, units, day_midnight, day_noon)

# Run through each New Jersey station.
for(i in 1:length(NJ_ID)){ waterheight_plot(NJ_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each New York station.
for(i in 1:length(NY_ID)){ waterheight_plot(NY_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each North Carolina station.
for(i in 1:length(NC_ID)){ waterheight_plot(NC_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Oregon station.
for(i in 1:length(OR_ID)){ waterheight_plot(OR_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Pennsylvania station.
for(i in 1:length(PA_ID)){ waterheight_plot(PA_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Rhode Island station.
for(i in 1:length(RI_ID)){ waterheight_plot(RI_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each South Carolina station.
for(i in 1:length(SC_ID)){ waterheight_plot(SC_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Texas station.
for(i in 1:length(TX_ID)){ waterheight_plot(TX_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Texas msl station.
for(i in 1:length(TX_MSL_ID)){ waterheight_plot(TX_MSL_ID[i], b.date, e.date, msl.datum, timezone, units, day_midnight, day_noon) }

# Run through each Virginia station.
for(i in 1:length(VA_ID)){ waterheight_plot(VA_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each Washington station.
for(i in 1:length(WA_ID)){ waterheight_plot(WA_ID[i], b.date, e.date, datum, timezone, units, day_midnight, day_noon) }

# Run through each station in the Great lakes.
for(i in 1:length(GL_ID)){ waterheight_plot(GL_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLDR_ID)){ waterheight_plot(GLDR_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLLH_ID)){ waterheight_plot(GLLH_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GRLM_ID)){ waterheight_plot(GRLM_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GRLO_ID)){ waterheight_plot(GRLO_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLLSC_ID)){ waterheight_plot(GLLSC_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLLS_ID)){ waterheight_plot(GLLS_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLNR_ID)){ waterheight_plot(GLNR_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLSCR_ID)){ waterheight_plot(GLSCR_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLSLR_ID)){ waterheight_plot(GLSLR_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

for(i in 1:length(GLSMR_ID)){ waterheight_plot(GLSMR_ID[i], b.date, e.date, gl.datum, timezone, units, day_midnight, day_noon) }

# --------------------------------------------------------------------------------------------------------------------
