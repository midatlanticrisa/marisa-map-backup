# Ensure necessary packages are installed and loaded
if (!require(“RCurl”)) { install.packages(“RCurl”) }
if (!require(“readr”)) { install.packages(“readr”) }

library(RCurl)
library(readr)

# Function extracting stream data (discharge and time) from a TXT file online.
usgs_dataRetrieve = function(ID, param_cd, b.date, e.date, tz){
  
  URL = paste('https://waterdata.usgs.gov/nwis/uv?cb_', param_cd, '=on&format=rdb&site_no=', ID, '&period=&begin_date=', b.date, '&end_date=', e.date, sep="")
  x.url <- getURL(URL)
  
  if(x.url == "No sites/data found using the selection criteria specified \n"){
    return(NA)
    # stop()
  }
  
  # Extract number of metadata rows to skip and determine header names
  readr.total <- read_lines(URL)
  total.rows <- length(readr.total)
  readr.meta <- readr.total[grep("^#", readr.total)]
  meta.rows <- length(readr.meta)
  header.names <- strsplit(readr.total[meta.rows+1],"\t")[[1]]
  types.names <- strsplit(readr.total[meta.rows+2],"\t")[[1]]
  data.rows <- total.rows - meta.rows - 2
  
  data = read.table(text = x.url, skip = (meta.rows+2), row.names = NULL, sep="\t", col.names = header.names,
                    na.strings = c("Ssn", "Bkw", "Ice", "Pr", "Rat", "Eqp", "Fld", "Dry", "Dis", "DIS", "--", "Mnt", "ZFl", "***", ""))
  
  # Instantaneous and Daily Value Status Codes
  # ---------------------------------
  #   Code   Description
  # ---------------------------------
  # Ssn    Parameter monitored seasonally
  # Bkw    Flow affected by backwater
  # Ice    Ice affected
  # Pr     Partial-record site
  # Rat    Rating being developed or revised
  # Eqp    Equipment malfunction
  # Fld    Flood damage
  # Dry    Dry
  # Dis    Data-collection discontinued
  # --     Parameter not determined
  # Mnt    Maintenance in progress
  # ZFl    Zero flow
  # ***    Temporarily unavailable

  # NEED TO CHECK FOR MISSING DATA!!!
  header.grep <- grep(param_cd, header.names)[1]
#   data.grep <- data[ ,header.grep]
  
  if(is.na(header.grep) || all(is.na(data[ ,header.grep]))){
    return(NA)
  }
  
  # Convert and extract the date and time.
  data$dateTime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M", tz = tz)
  data$Time <- strftime(data$datetime, format="%H:%M", tz = tz)
  
  # Give parameter data a common header name 
  data$var = data[ ,grep(param_cd, header.names)[1]]
  
  return(data)
}

# Function extracting stream data (discharge and time) from a TXT file online.
usgs_dataRetrieveSTAT = function(ID, param_cd, report_type, b.date, e.date, tz){
  
  URL = paste('https://waterservices.usgs.gov/nwis/stat/?format=rdb&sites=', ID, '&parameterCd=', param_cd, '&statReportType=', report_type, sep="")
  x.url <- getURL(URL)
  
  if(x.url == "# //Output-Format: RDB\n# //Response-Status: OK\n# //Response-Message: No sites found matching all criteria\n"){
    return(NA)
    # stop()
  }
  
  # Extract number of metadata rows to skip and determine header names
  readr.total <- read_lines(URL)
  total.rows <- length(readr.total)
  readr.meta <- readr.total[grep("^#", readr.total)]
  meta.rows <- length(readr.meta)
  header.names <- strsplit(readr.total[meta.rows+1],"\t")[[1]]
  types.names <- strsplit(readr.total[meta.rows+2],"\t")[[1]]
  data.rows <- total.rows - meta.rows - 2
  
  data = read.table(text = x.url, skip = (meta.rows+2), row.names = NULL, sep="\t", col.names = header.names, na.strings = "")
  
  # Convert month and day to a date.
  data$MonDay <- as.POSIXct(paste(data$month_nu, data$day_nu, sep="-"), format = "%m-%d", tz = "America/New_York")

  # Extract the average over the week
  if(all(is.na(data))==FALSE){
    weeks_avgQ <- mean(data$mean_va[grep(b.date, data$MonDay):grep(e.date, data$MonDay)], na.rm=TRUE)
  } else {
    weeks_avgQ <- NA
  }
    
  return(weeks_avgQ)
    
}
# 
# daily_avgQ = usgs_dataRetrieveSTAT(ID, "00060", report_type, b.date, e.date, tz="America/New_York")
#   
# discharge = usgs_dataRetrieve("01591000", "00060", b.date, e.date, tz="America/New_York")
# gage_height = usgs_dataRetrieve("04212100", "00065", b.date, e.date, tz="America/New_York")
# temp_C = usgs_dataRetrieve("04212100", "00010", b.date, e.date, tz="America/New_York")
 
# Function extracting stream data (discharge and time) from a TXT file online.
usgs_dataRetrieveTemp = function(ID, param_cd, b.date, e.date, tz){
  
  URL = paste('https://waterdata.usgs.gov/nwis/uv?cb_', param_cd, '=on&format=rdb&site_no=', ID, '&period=&begin_date=', b.date, '&end_date=', e.date, sep="")
  x.url <- getURL(URL)
  
  if(x.url == "No sites/data found using the selection criteria specified \n"){
    return(c("NA",""))
    # stop()
  }
  
  # Extract number of metadata rows to skip and determine header names
  readr.total <- read_lines(URL)
  total.rows <- length(readr.total)
  readr.meta <- readr.total[grep("^#", readr.total)]
  meta.rows <- length(readr.meta)
  header.names <- strsplit(readr.total[meta.rows+1],"\t")[[1]]
  types.names <- strsplit(readr.total[meta.rows+2],"\t")[[1]]
  data.rows <- total.rows - meta.rows - 2
  
  data = read.table(text = x.url, skip = (meta.rows+2), row.names = NULL, sep="\t", col.names = header.names,
                    na.strings = c("Ssn", "Bkw", "Ice", "Pr", "Rat", "Eqp", "Fld", "Dry", "Dis", "DIS", "--", "Mnt", "ZFl", "***", ""))
  
  # Instantaneous and Daily Value Status Codes
  # ---------------------------------
  #   Code   Description
  # ---------------------------------
  # Ssn    Parameter monitored seasonally
  # Bkw    Flow affected by backwater
  # Ice    Ice affected
  # Pr     Partial-record site
  # Rat    Rating being developed or revised
  # Eqp    Equipment malfunction
  # Fld    Flood damage
  # Dry    Dry
  # Dis    Data-collection discontinued
  # --     Parameter not determined
  # Mnt    Maintenance in progress
  # ZFl    Zero flow
  # ***    Temporarily unavailable
  
  # NEED TO CHECK FOR MISSING DATA!!!
  header.grep <- grep(param_cd, header.names)[1]
#   data.grep <- data[ ,grep(param_cd, header.names)[1]]
  
  if(is.na(header.grep) || all(is.na(data[ ,header.grep]))){
    return(c("NA", ""))
  }
  
  # Remove everything, but the most up to date value
  data = data[nrow(data), ]

if(is.na(data[ ,grep(param_cd, header.names)[1]])){
  return(c("NA",""))
}
  
  # Convert and extract the date and time.
  data$dateTime <- as.POSIXct(data$datetime, format = "%Y-%m-%d %H:%M", tz = tz)
  data$dateTime <- format(data$dateTime, format = "%b %d, %Y %I:%M %p %Z")
#   data$dateTime <- paste("<br/><br/>Last Updated on ", data$dateTime, sep="")
  
  #data$Time <- strftime(data$datetime, format="%H:%M", tz = tz)
  
  # Convert temp from C to F if temperature data is requested
  if(param_cd == "00010"){
    data$var = (data[ ,grep(param_cd, header.names)[1]] * (9/5)) + 32
  } else {
    data$var = data[ ,grep(param_cd, header.names)[1]]
  }
  

  return(c(data$var, data$dateTime))
}
  
