# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: Jan 25, 2019    - expanded to entire US
# prior edit: June 18, 2018
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

# Files are saved to a directory called mapdata. Create this directory if it doesn't exist
#if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata")){
#  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata")
#}
# --------------------------------------------------------------------------------------------------------------------
# Set variables for determining plot size.
p.width   = 4            # Width
p.height = 2.5           # Height

# Set up common variables.
datum = "MLLW"
gl.datum = "IGLD"
msl.datum = "MSL"
timezone = "GMT"
units = "metric"

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
# Function extracting tide data from within the past 18 mins from a XML file online.
latest_data = function(ID, variable, datum, timezone, units){
  # Use the ID, variable, datum, timezone, and units to create a URL to the XML file.
  url = paste('https://tidesandcurrents.noaa.gov/api/datagetter?date=latest&station=', ID, '&product=', variable, '&datum=', datum,
              '&units=', units, '&time_zone=', timezone, '&application=web_services&format=xml', sep="")

  xml_data <- xmlToList(rawToChar(GET(url)$content))
  test = t(data.frame(xml_data$observations))

  if(is.logical(test) == TRUE){
    value = NA
    date = NA

  } else if (is.null(xml_data$observations$disclaimers) == FALSE) {

    remove <- c("disclaimers.disclaimer.text", "disclaimers.disclaimer..attrs")
    test <- test[!rownames(test) %in% remove, ]

    value <- test[2]
    if(variable=="wind"){
      value <- paste("From the ", test[4], " at ", test[2], sep="")
    }

    date <- as.POSIXct(test[1], format = "%Y-%m-%d %H:%M", tz = "GMT")
    date <- format(date, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York") # convert from GMT to current time zone

  } else {

    value <- test[2]
    if(variable=="wind"){
      value <- paste("From the ", test[4], " at ", test[2], sep="")
    }

    date <- as.POSIXct(test[1], format = "%Y-%m-%d %H:%M", tz = "GMT")
    date <- format(date, format = "%b %d, %Y %I:%M %p %Z", tz = "America/New_York") # convert from GMT to current time zone

  }
  return(c(ID, value, date))
}
# --------------------------------------------------------------------------------------------------------------------
# Function extracting tide data from within the past 18 mins from a XML file online.
meta_data = function(ID, variable, datum, timezone, units){
  # Use the ID, variable, datum, timezone, and units to create a URL to the XML file.
  url = paste('https://tidesandcurrents.noaa.gov/api/datagetter?date=latest&station=', ID, '&product=', variable, '&datum=', datum,
              '&units=', units, '&time_zone=', timezone, '&application=web_services&format=xml', sep="")

  xml_data <- xmlToList(rawToChar(GET(url)$content))
  test = t(data.frame(xml_data$metadata))

  if(is.logical(test) == TRUE){
    id = NA
    name = NA
    lat = NA
    lon = NA

  } else {

    id <- test[1]
    name <- test[2]
    lat <- test[3]
    lon <- test[4]
  }
  return(c(id, name, lat, lon))
}
# --------------------------------------------------------------------------------------------------------------------
# Create time string
time_string = function(ATMP, APR, VIS, HUM, WND, WL, WTMP, CON, SAL){
  TIME = ATMP[1:2]
  if(all(is.na(c(ATMP[3], APR[3], VIS[3], HUM[3], WND[3], WL[3], WTMP[3], CON[3], SAL[3])))){
    TIME[2] = ""
  } else {
    TIME[2] = min(c(ATMP[3], APR[3], VIS[3], HUM[3], WND[3], WL[3],
                    WTMP[3], CON[3], SAL[3]), na.rm = TRUE)
    TIME[2] = paste("<br/><br/>Last Updated on ", TIME[2], sep="")
  }
  return(TIME)
}
# --------------------------------------------------------------------------------------------------------------------
# Create observation string
obs_string = function(ATMP, APR, VIS, HUM, WND, WL, WTMP, CON, SAL, datum){
  OBS = ATMP[1:2]
  if(is.na(ATMP[2])){
    air_temp = ""
  } else {
    air_temp = paste("<strong>Air temperature: </strong>", ATMP[2], " &#8451;<br/>", sep="")
  }

  if(is.na(APR[2])){
    air_pres = ""
  } else {
    air_pres = paste("<strong>Air pressure: </strong>", APR[2], " mbar<br/>", sep="")
  }

  if(is.na(VIS[2])){
    visib = ""
  } else {
    visib = paste("<strong>Visibility: </strong>", ATMP[2], " nmi<br/>", sep="")
  }

  if(is.na(HUM[2])){
    relhum = ""
  } else {
    relhum = paste("<strong>Relative humidity: </strong>", APR[2], " %<br/>", sep="")
  }

  if(is.na(WND[2])){
    wind = ""
  } else {
    wind = paste("<strong>Wind: </strong>", WND[2], " kn<br/>", sep="")
  }

  if(is.na(WL[2])){
    water_lev = ""
  } else {
    water_lev = paste("<strong>Water level: </strong>", WL[2], " m ", datum, "<br/>", sep="")
  }

  if(is.na(WTMP[2])){
    water_temp = ""
  } else {
    water_temp = paste("<strong>Water temperature: </strong>", WTMP[2], " &#8451;<br/>", sep="")
  }

  if(is.na(CON[2])){
    conduct = ""
  } else {
    conduct = paste("<strong>Conductivity: </strong>", CON[2], " mS/cm<br/>", sep="")
  }

  if(is.na(SAL[2])){
    salin = ""
  } else {
    salin = paste("<strong>Salinity: </strong>", SAL[2], " psu<br/>", sep="")
  }
  OBS[2] = paste(air_temp, air_pres, visib, relhum, wind, water_lev, water_temp, conduct, salin, sep="")
  return(OBS)
}
# --------------------------------------------------------------------------------------------------------------------
# Run the function extracting the data we want and creating a plot.
# CREATE a function of this step!!
station_data = function(ID, variable, datum, timezone, units){
  if(length(ID) == 1){ # 1 station
    ATMP = latest_data(ID, "air_temperature", datum, timezone, units)
    APR = latest_data(ID, "air_pressure", datum, timezone, units)
    VIS = latest_data(ID, "visibility", datum, timezone, units)
    HUM = latest_data(ID, "humidity", datum, timezone, units)
    WND = latest_data(ID, "wind", datum, timezone, units)
    WL = latest_data(ID, "water_level", datum, timezone, units)
    WTMP = latest_data(ID, "water_temperature", datum, timezone, units)
    CON = latest_data(ID, "conductivity", datum, timezone, units)
    SAL = latest_data(ID, "salinity", datum, timezone, units)
    MET = meta_data(ID, "water_level", datum, timezone, units)
    TIME = time_string(ATMP, APR, VIS, HUM, WND, WL, WTMP, CON, SAL)
    OBS = obs_string(ATMP, APR, VIS, HUM, WND, WL, WTMP, CON, SAL, datum)

  } else { # Multiple stations
    ATMP = WTMP = APR = VIS = HUM = WND = CON = SAL = WL = mat.or.vec(length(ID), 3)
    MET = mat.or.vec(length(ID), 4)
    for(i in 1:length(ID)){
      ATMP[i, ] = latest_data(ID[i], "air_temperature", datum, timezone, units)
      APR[i, ] = latest_data(ID[i], "air_pressure", datum, timezone, units)
      VIS[i, ] = latest_data(ID[i], "visibility", datum, timezone, units)
      HUM[i, ] = latest_data(ID[i], "humidity", datum, timezone, units)
      WND[i, ] = latest_data(ID[i], "wind", datum, timezone, units)
      WL[i, ] = latest_data(ID[i], "water_level", datum, timezone, units)
      WTMP[i, ] = latest_data(ID[i], "water_temperature", datum, timezone, units)
      CON[i, ] = latest_data(ID[i], "conductivity", datum, timezone, units)
      SAL[i, ] = latest_data(ID[i], "salinity", datum, timezone, units)
      MET[i, ] = meta_data(ID[i], "water_level", datum, timezone, units)
    }

    TIME = OBS = mat.or.vec(length(ID), 2)
    for(i in 1:length(ID)){
      TIME[i, ] = time_string(ATMP[i,], APR[i,], VIS[i,], HUM[i,], WND[i,], WL[i,], WTMP[i,], CON[i,], SAL[i,])
      OBS[i, ] = obs_string(ATMP[i,], APR[i,], VIS[i,], HUM[i,], WND[i,], WL[i,], WTMP[i,], CON[i,], SAL[i,], datum)
    }
  }
  return(list(ATMP = ATMP, WTMP = WTMP, APR = APR, VIS = VIS, HUM = HUM, WND = WND,
              CON = CON, SAL = SAL, WL = WL, MET = MET, TIME = TIME, OBS = OBS))
}
# --------------------------------------------------------------------------------------------------------------------
# Run through each station.
AL_data = station_data(AL_ID, variable, datum, timezone, units) # Alabama
AK_data = station_data(AK_ID, variable, datum, timezone, units) # Alaska
CA_data = station_data(CA_ID, variable, datum, timezone, units) # California
CT_data = station_data(CT_ID, variable, datum, timezone, units) # Conneticut
DE_data = station_data(DE_ID, variable, datum, timezone, units) # Delaware
DC_data = station_data(DC_ID, variable, datum, timezone, units) # District of Columbia
FL_data = station_data(FL_ID, variable, datum, timezone, units) # Florida
GA_data = station_data(GA_ID, variable, datum, timezone, units) # Georgia
HI_data = station_data(HI_ID, variable, datum, timezone, units) # Hawaii
LA_data = station_data(LA_ID, variable, datum, timezone, units) # Louisiana
LA_MSL_data = station_data(LA_MSL_ID, variable, msl.datum, timezone, units) # Louisiana
ME_data = station_data(ME_ID, variable, datum, timezone, units) # Maine #error
MD_data = station_data(MD_ID, variable, datum, timezone, units) # Maryland
MA_data = station_data(MA_ID, variable, datum, timezone, units) # Massachusetts #error
MS_data = station_data(MS_ID, variable, datum, timezone, units) # Mississippi
NH_data = station_data(NH_ID, variable, datum, timezone, units) # New Hampshire
NJ_data = station_data(NJ_ID, variable, datum, timezone, units) # New Jersey
NY_data = station_data(NY_ID, variable, datum, timezone, units) # New York
NC_data = station_data(NC_ID, variable, datum, timezone, units) # North Carolina
OR_data = station_data(OR_ID, variable, datum, timezone, units) # Oregon
PA_data = station_data(PA_ID, variable, datum, timezone, units) # Pennsylvania
RI_data = station_data(RI_ID, variable, datum, timezone, units) # Rhode Island
SC_data = station_data(SC_ID, variable, datum, timezone, units) # South Carolina
TX_data = station_data(TX_ID, variable, datum, timezone, units) # Texas #error
TX_MSL_data = station_data(TX_MSL_ID, variable, msl.datum, timezone, units) # Louisiana
VA_data = station_data(VA_ID, variable, datum, timezone, units) # Virginia
WA_data = station_data(WA_ID, variable, datum, timezone, units) # Washington #error
GL_data = station_data(GL_ID, variable, gl.datum, timezone, units) # Great Lakes: lake erie
GLDR_data = station_data(GLDR_ID, variable, gl.datum, timezone, units) # Great Lakes: Detroit River
GLLH_data = station_data(GLLH_ID, variable, gl.datum, timezone, units) # Great Lakes: lake huron
GRLM_data = station_data(GRLM_ID, variable, gl.datum, timezone, units) # Great Lakes: lake michigan
GRLO_data = station_data(GRLO_ID, variable, gl.datum, timezone, units) # Great Lakes: lake ontario
GLLSC_data = station_data(GLLSC_ID, variable, gl.datum, timezone, units) # Great Lakes: lake st. clair
GLLS_data = station_data(GLLS_ID, variable, gl.datum, timezone, units) # Great Lakes: lake superior
GLNR_data = station_data(GLNR_ID, variable, gl.datum, timezone, units) # Great Lakes: niagara river
GLSCR_data = station_data(GLSCR_ID, variable, gl.datum, timezone, units) # Great Lakes: st. clair river
GLSLR_data = station_data(GLSLR_ID, variable, gl.datum, timezone, units) # Great Lakes: st. lawrence river
GLSMR_data = station_data(GLSMR_ID, variable, gl.datum, timezone, units) # Great Lakes: st. marys river

# --------------------------------------------------------------------------------------------------------------------

station_string = function(ID, MET, OBS, TIME){
  if(is.null(ncol(MET))){
    str = paste('{"type": "Feature", "properties": {"name": "', MET[2], '", "id": "', ID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', ID, '", "obs": "',
                OBS[2], '", "time": "', TIME[2], '", "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', ID, '.png"},
                "geometry": {"type": "Point", "coordinates": [', MET[4], ',',  MET[3], ']}}', sep="")
  } else {
    str = paste('{"type": "Feature", "properties": {"name": "', MET[match(ID, MET[,1]), 2], '", "id": "', ID, '", "url": "https://tidesandcurrents.noaa.gov/stationhome.html?id=', ID, '", "obs": "',
                OBS[match(ID, OBS[,1]), 2], '", "time": "', TIME[match(ID, TIME[,1]), 2], '",
                "image": "https://www.marisa.psu.edu/mapdata/Tide_figs/Fig_', ID, '.png"},
                "geometry": {"type": "Point", "coordinates": [', MET[match(ID, MET[,1]), 4], ',',  MET[match(ID, MET[,1]), 3], ']}}', sep="")
    }
  return(str)
}
# --------------------------------------------------------------------------------------------------------------------
# Combine all info into one string
# Run through each Alabama station.
al_st = station_string(AL_ID, AL_data$MET, AL_data$OBS, AL_data$TIME)
al_st = paste(al_st, ",", collapse="")
# Run through each Alaska station.
ak_st = station_string(AK_ID, AK_data$MET, AK_data$OBS, AK_data$TIME)
ak_st = paste(ak_st, ",", collapse="")
# Run through each California station.
ca_st = station_string(CA_ID, CA_data$MET, CA_data$OBS, CA_data$TIME)
ca_st = paste(ca_st, ",", collapse="")
# Run through each Conneticut station.
ct_st = station_string(CT_ID, CT_data$MET, CT_data$OBS, CT_data$TIME)
ct_st = paste(ct_st, ",", collapse="")
# Run through each Delaware station.
de_st = station_string(DE_ID, DE_data$MET, DE_data$OBS, DE_data$TIME)
de_st = paste(de_st, ",", collapse="")
# Run through each District of Columbia station.
dc_st = station_string(DC_ID, DC_data$MET, DC_data$OBS, DC_data$TIME)
dc_st = paste(dc_st, ",", collapse="")
# Run through each Florida station.
fl_st = station_string(FL_ID, FL_data$MET, FL_data$OBS, FL_data$TIME)
fl_st = paste(fl_st, ",", collapse="")
# Run through each Georgia station.
ga_st = station_string(GA_ID, GA_data$MET, GA_data$OBS, GA_data$TIME)
ga_st = paste(ga_st, ",", collapse="")
# Run through each Hawaii station.
hi_st = station_string(HI_ID, HI_data$MET, HI_data$OBS, HI_data$TIME)
hi_st = paste(hi_st, ",", collapse="")
# Run through each Louisiana station.
la_st = station_string(LA_ID, LA_data$MET, LA_data$OBS, LA_data$TIME)
la_st = paste(la_st, ",", collapse="")
# Run through each Louisiana MSL station.
la_msl_st = station_string(LA_MSL_ID, LA_MSL_data$MET, LA_MSL_data$OBS, LA_MSL_data$TIME)
la_msl_st = paste(la_msl_st, ",", collapse="")
# Run through each Maine station.
me_st = station_string(ME_ID, ME_data$MET, ME_data$OBS, ME_data$TIME)
me_st = paste(me_st, ",", collapse="")
# Run through each Maryland station.
md_st = station_string(MD_ID, MD_data$MET, MD_data$OBS, MD_data$TIME)
md_st = paste(md_st, ",", collapse="")
# Run through each Massachusetts station.
ma_st = station_string(MA_ID, MA_data$MET, MA_data$OBS, MA_data$TIME)
ma_st = paste(ma_st, ",", collapse="")
# Run through each Mississippi station.
ms_st = station_string(MS_ID, MS_data$MET, MS_data$OBS, MS_data$TIME)
ms_st = paste(ms_st, ",", collapse="")
# Run through each New Hampshire station.
nh_st = station_string(NH_ID, NH_data$MET, NH_data$OBS, NH_data$TIME)
nh_st = paste(nh_st, ",", collapse="")
# Run through each New Jersey station.
nj_st = station_string(NJ_ID, NJ_data$MET, NJ_data$OBS, NJ_data$TIME)
nj_st = paste(nj_st, ",", collapse="")
# Run through each New York station.
ny_st = station_string(NY_ID, NY_data$MET, NY_data$OBS, NY_data$TIME)
ny_st = paste(ny_st, ",", collapse="")
# Run through each North Carolina station.
nc_st = station_string(NC_ID, NC_data$MET, NC_data$OBS, NC_data$TIME)
nc_st = paste(nc_st, ",", collapse="")
# Run through each Oregon station.
or_st = station_string(OR_ID, OR_data$MET, OR_data$OBS, OR_data$TIME)
or_st = paste(or_st, ",", collapse="")
# Run through each Pennsylvania station.
pa_st = station_string(PA_ID, PA_data$MET, PA_data$OBS, PA_data$TIME)
pa_st = paste(pa_st, ",", collapse="")
# Run through each Rhode Island station.
ri_st = station_string(RI_ID, RI_data$MET, RI_data$OBS, RI_data$TIME)
ri_st = paste(ri_st, ",", collapse="")
# Run through each South Carolina station.
sc_st = station_string(SC_ID, SC_data$MET, SC_data$OBS, SC_data$TIME)
sc_st = paste(sc_st, ",", collapse="")
# Run through each Texas station.
tx_st = station_string(TX_ID, TX_data$MET, TX_data$OBS, TX_data$TIME)
tx_st = paste(tx_st, ",", collapse="")
# Run through each Texas MSL station.
tx_msl_st = station_string(TX_MSL_ID, TX_MSL_data$MET, TX_MSL_data$OBS, TX_MSL_data$TIME)
tx_msl_st = paste(tx_msl_st, ",", collapse="")
# Run through each Virginia station.
va_st = station_string(VA_ID, VA_data$MET, VA_data$OBS, VA_data$TIME)
va_st = paste(va_st, ",", collapse="")
# Run through each Washington station.
wa_st = station_string(WA_ID, WA_data$MET, WA_data$OBS, WA_data$TIME)
wa_st = paste(wa_st, ",", collapse="")
# Run through each station in the Great lakes.
gl_st = station_string(GL_ID, GL_data$MET, GL_data$OBS, GL_data$TIME)
gl_st = paste(gl_st, ",", collapse="")
gldr_st = station_string(GLDR_ID, GLDR_data$MET, GLDR_data$OBS, GLDR_data$TIME)
gldr_st = paste(gldr_st, ",", collapse="")
gllh_st = station_string(GLLH_ID, GLLH_data$MET, GLLH_data$OBS, GLLH_data$TIME)
gllh_st = paste(gllh_st, ",", collapse="")
grlm_st = station_string(GRLM_ID, GRLM_data$MET, GRLM_data$OBS, GRLM_data$TIME)
grlm_st = paste(grlm_st, ",", collapse="")
grlo_st = station_string(GRLO_ID, GRLO_data$MET, GRLO_data$OBS, GRLO_data$TIME)
grlo_st = paste(grlo_st, ",", collapse="")
gllsc_st = station_string(GLLSC_ID, GLLSC_data$MET, GLLSC_data$OBS, GLLSC_data$TIME)
gllsc_st = paste(gllsc_st, ",", collapse="")
glls_st = station_string(GLLS_ID, GLLS_data$MET, GLLS_data$OBS, GLLS_data$TIME)
glls_st = paste(glls_st, ",", collapse="")
glnr_st = station_string(GLNR_ID, GLNR_data$MET, GLNR_data$OBS, GLNR_data$TIME)
glnr_st = paste(glnr_st, ",", collapse="")
glscr_st = station_string(GLSCR_ID, GLSCR_data$MET, GLSCR_data$OBS, GLSCR_data$TIME)
glscr_st = paste(glscr_st, ",", collapse="")
glslr_st = station_string(GLSLR_ID, GLSLR_data$MET, GLSLR_data$OBS, GLSLR_data$TIME)
glslr_st = paste(glslr_st, ",", collapse="")
glsmr_st = station_string(GLSMR_ID, GLSMR_data$MET, GLSMR_data$OBS, GLSMR_data$TIME)
glsmr_st_last = glsmr_st[length(glsmr_st)] #make sure the last feature doesn't end with a ","
glsmr_st = paste(glsmr_st[1:length(glsmr_st) - 1], ",", collapse="")

# --------------------------------------------------------------------------------------------------------------------
json_merge = paste('tideStations = {"type": "FeatureCollection","features": [',
                   al_st, ak_st, ca_st, ct_st, de_st, dc_st, fl_st, ga_st, hi_st, la_st,
                   la_msl_st, me_st, md_st, ma_st, ms_st, nh_st, nj_st, ny_st, nc_st, or_st,
                   pa_st, ri_st, sc_st, tx_st, tx_msl_st, va_st, wa_st, gl_st, gldr_st, gllh_st,
                   grlm_st, grlo_st, gllsc_st, glls_st, glnr_st, glscr_st, glslr_st, glsmr_st,
                   glsmr_st_last, ']};', sep="")

# Export data to geojson.
cat(json_merge, file="/net/www/www.marisa.psu.edu/htdocs/mapdata/tide_station_obs_extend.js")
