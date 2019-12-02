# --------------------------------------------------------------------------------------------------------------------
# Copyright 2017 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 13, 2018; set as seperate script; chance avg. discharge to 1 value and fix midnight/noon axis
# Previous edit: May 29, 2018; implement techniques from dataRetrieval package to improve data extraction; add gage height and avg. discharge
# Previous edit: June 17, 2017; Created
#
# This script parses discharge and gage height data of stream gauges from the
# United States Geological Survey (USGS) and plots the results.
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
# Files are saved to a directory called Stream_figs in mapdata. Create these directories if they don't exist
if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata")){
  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata")
}
if (!file.exists("/home/staff/mdl5548/marisa.psu.edu/mapdata/Stream_figs")){
  dir.create("/home/staff/mdl5548/marisa.psu.edu/mapdata/Stream_figs")
}

# Function extracting stream data (discharge, gage height, and time) from USGS stream gages.
stream_gage_plot = function(ID, b.date, e.date, day_midnight, day_noon){

  # Use the ID, beginning date, and end date to import national water monitoring data.
  discharge_dat <- usgs_dataRetrieve(ID, "00060", b.date, e.date, tz="America/New_York")
  gaugeheight_dat <- usgs_dataRetrieve(ID, "00065", b.date, e.date, tz="America/New_York")

  # Export a plot from the discharge data.
  png(file=paste("/home/staff/mdl5548/marisa.psu.edu/mapdata/Stream_figs/Fig_", ID, ".png", sep=""), family="sans", units="in", width=p.width, height=p.height, pointsize=14, res=300)
  par(mfrow=c(1,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,2.5))

  if(all(is.na(discharge_dat)) && all(is.na(gaugeheight_dat)) ){    # No data is available
    # During cold weather, stage and discharge values may be affected by ice at some
    # streamgages. Streamgages experiencing ice conditions will have the discharge record
    # temporarily disabled to prevent the display of erroneous discharge values. Display of
    # discharge record will resume when it is determined that ice conditions are no longer
    # present. Adjustment of data affected by ice can only be done after detailed analysis.
    plot(c(b.date, e.date), rep(0, 2), xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")

    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    legend("center", "No data currently available", bg = "white")

  } else if(all(is.na(discharge_dat))==FALSE && all(is.na(gaugeheight_dat)) ){    # Only discharge data is available

    # Extract daily mean statistic for days of the current week
    daily_avgQ = usgs_dataRetrieveSTAT(ID, "00060", "daily", b.date, e.date, tz="America/New_York")

    # Create discharge plot.
    plot(discharge_dat$dateTime, discharge_dat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n",
         col="#018571", col.ticks="#018571", col.axis="#018571", ylim=c(min(daily_avgQ, min(discharge_dat$var, na.rm=TRUE), na.rm=TRUE),
                                                                        max(daily_avgQ, max(discharge_dat$var, na.rm=TRUE), na.rm=TRUE)))

    mtext(2, text=expression("Discharge"~(ft^3/s)), line=1.25, col="#018571")
    # Color the background light gray.
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")

    # Set axis ticks to midnight and labels to noon.
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)

    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    # Add the data.
    lines(discharge_dat$dateTime, discharge_dat$var, lwd=2, col="#018571") # steelblue

    # Add climate average
    abline(h=daily_avgQ, lty=3, lwd=2, col="#018571")

  } else if(all(is.na(discharge_dat)) && all(is.na(gaugeheight_dat))==FALSE ){    # Only gauge height data is available

    # Create gage height plot.
    plot(gaugeheight_dat$dateTime, gaugeheight_dat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n", yaxt="n")
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")

    axis(4, col="#a6611a", col.ticks="#a6611a", col.axis="#a6611a")
    mtext(4, text="Gage height (ft)", line=1.25, col="#a6611a")

    # Set axis ticks to midnight and labels to noon.
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)

    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    # Add the data.
    lines(gaugeheight_dat$dateTime, gaugeheight_dat$var, lwd=2, col="#a6611a") # steelblue

  } else if(all(is.na(discharge_dat))==FALSE && all(is.na(gaugeheight_dat))==FALSE ){    # Discharge and gauge height data are available

    # Extract daily mean statistic for days of the current week
    daily_avgQ = usgs_dataRetrieveSTAT(ID, "00060", "daily", b.date, e.date, tz="America/New_York")

    # Create discharge and gage height plot.
    plot(discharge_dat$dateTime, discharge_dat$var, type = "n", ylab = "", xlab="Past 7 days", xaxt="n",
         col="#018571", col.ticks="#018571", col.axis="#018571", ylim=c(min(daily_avgQ, min(gaugeheight_dat$var, na.rm=TRUE), min(discharge_dat$var, na.rm=TRUE), na.rm=TRUE),
                                                                        max(daily_avgQ, max(gaugeheight_dat$var, na.rm=TRUE), max(discharge_dat$var, na.rm=TRUE), na.rm=TRUE)))

    mtext(2, text=expression("Discharge"~(ft^3/s)), line=1.25, col="#018571")
    # Color the background light gray.
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")

    # Set axis ticks to midnight and labels to noon.
    axis(1, at = day_midnight, labels = FALSE, tick = TRUE)
    axis(1, at = day_noon, labels = gsub("0(\\d)", "\\1", format(day_noon, "%m/%d")), tick = FALSE)

    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    abline(v = day_midnight, lty = 6, col = "gray")
    # Add the data.
    lines(discharge_dat$dateTime, discharge_dat$var, lwd=2, col="#018571") # steelblue

    # Add climate average
    abline(h=daily_avgQ, lty=3, lwd=2, col="#018571")

    # Add gage height data
    par(new=TRUE)
    plot(gaugeheight_dat$dateTime, gaugeheight_dat$var, lwd=2, col="#a6611a", typ="l",
         ylab="", xlab="", xaxt="n", yaxt="n")
    axis(4, col="#a6611a", col.ticks="#a6611a", col.axis="#a6611a")
    mtext(4, text="Gage height (ft)", line=1.25, col="#a6611a")

  } else { # Create empty plot to indicate there may be something wrong with the script
    plot(c(b.date,e.date), rep(0, 2), xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "snow")

    # Add a grid.
    grid(NA, NULL, lty = 6, col = "gray")
    legend("center", "Check import script for errors", bg = "white")

  }
  dev.off()
}
# --------------------------------------------------------------------------------------------------------------------
