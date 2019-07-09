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

# Set variables for colors.
temp_cols = brewer.pal(3, "Reds")
pcp_cols = brewer.pal(3, "Blues")

# Set variables for determining plot size.
p.width   = 4            # Width
p.height = 2.5           # Height

# Files are saved to a directory called Clim_div_figs in mapdata. Create these directories if they don't exist
if (!file.exists("/home/staff/klr324/marisa.psu.edu/mapdata")){
  dir.create("/home/staff/klr324/marisa.psu.edu/mapdata")
} 
if (!file.exists("/home/staff/klr324/marisa.psu.edu/mapdata/Clim_div_figs")){
  dir.create("/home/staff/klr324/marisa.psu.edu/mapdata/Clim_div_figs")
}

# Function to create plots
plot_climdiv = function(climate_dat, div_num, state){
  climate_division = climate_dat[which(climate_dat$Division == div_num), ]
  
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  tmax = tmin = tavg = pcp = rep(NA, length(month))
  for(i in 1:length(month)){
    tmax[i] = mean(climate_division$TMAX[which(climate_division$Month == month[i])])
    tmin[i] = mean(climate_division$TMIN[which(climate_division$Month == month[i])])
    tavg[i] = mean(climate_division$TAVG[which(climate_division$Month == month[i])])
    pcp[i] = mean(climate_division$PCP[which(climate_division$Month == month[i])])
  }

  years = unique(climate_division$Year)
  pcp.yrs = rep(NA, length(years))
  for(i in 1:length(years)){
    pcp.yrs[i] = mean(climate_division$PCP[which(climate_division$Year == years[i])])
  }
  yearly.avg.prcp = mean(pcp.yrs)
  
  clim_30yrs = data.frame(tmax = tmax, tmin = tmin, tavg = tavg, prcp = pcp,
                          month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                    "Sep", "Oct", "Nov", "Dec"), month_num = 1:12)
  
  clim_30yrs$month <- factor(clim_30yrs$month, levels = clim_30yrs$month[order(clim_30yrs$month_num)])
  
  png(file=paste("/home/staff/klr324/marisa.psu.edu/mapdata/Clim_div_figs/Fig_", state, "-", div_num, ".png", sep=""), family="sans", units="in", width=p.width, height=p.height*2, pointsize=12, res=300)
  par(mfrow=c(2,1), mgp=c(1.25,0.5,0), mar=c(2.25,2.5,0.5,0.5))
  plot(clim_30yrs$month_num, clim_30yrs$tmax, type="l", lwd = 2, col=temp_cols[3], xaxt="n", yaxt="n", 
       ylab=expression(paste("Temperature (", degree, "F)", sep="")), xlab="", 
       ylim = c(min(clim_30yrs$tmin), max(clim_30yrs$tmax)), bty="l")
  lines(clim_30yrs$month_num, clim_30yrs$tmin, type="l", lwd = 2, col=pcp_cols[2])
  lines(clim_30yrs$month_num, clim_30yrs$tavg, type="l", lwd = 2, col="black")
  axis(2, las=2, tck=-0.025)
  axis(1, labels=clim_30yrs$month, at = clim_30yrs$month_num, tck=-0.025)
  #legend("bottom", legend=c("Maximum", "Average", "Minimum"), pch = 15, col = c(temp_cols[3], "black", pcp_cols[2]), ncol=3, bty="n", cex=1)
  legend("topleft", legend=c("Maximum", "Average", "Minimum"), pch = 15, col = c(temp_cols[3], "black", pcp_cols[2]), ncol=1, bty="n", cex=1)  
  mtext("(1988-2018)", side = 3, las=1, adj = 1, line=-1)
  
  plot.new()
  vps <- baseViewports()
  pushViewport(vps$figure)
  
  pp = ggplot(clim_30yrs, aes(x=month, y=prcp)) + geom_bar(stat = "identity", fill=pcp_cols[3], width=0.6) +
    scale_x_discrete(breaks = clim_30yrs$month[seq(1, length(clim_30yrs$month), by = 2)]) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text=element_text(size=11, colour = "black"),
          axis.title=element_text(size=11, colour = "black")) +
    labs(x="", y = "Precipitation (in)")

  #pp = pp + geom_hline(yintercept=yearly.avg.prcp, linetype="dashed", color = "coral2", size = 1.5)
 
  #grob <- grobTree(textGrob("Yearly average", x=0.08,  y=0.95, hjust=0,
  #                          gp=gpar(col="black", fontsize=11)))
  
  #grob3 <- grobTree(polygonGrob(x=c(0.04, 0.04, 0.06, 0.06),  y=c(0.94, 0.96, 0.96, 0.94),  
  #                             gp=gpar(col="coral2", fill="coral2")))
  
  #pp = pp + annotation_custom(grob3) + annotation_custom(grob)
 
  vp <- viewport(height = unit(1,"npc"), width=unit(1, "npc"),
                 just = c("left","top"),
                 y = 1, x = 0)
  print(pp, vp = vp)
  dev.off()
}

# Function to cycle through climate divisions within a state
state_climdiv = function(state_name){
  statediv = read.table(paste(state_name, ".txt", sep=""), header=TRUE, na.strings = c(-99.99, -99.9, -9999))
  statediv$Year = substr(statediv$YearMonth, 1, 4)
  statediv$Month = substr(statediv$YearMonth, 5, 6)
  statediv30yrs = statediv[min(which(statediv$Year == 1988)):max(which(statediv$Year == 2018)), ]
  
  apply(as.array(unique(statediv30yrs$Division)), 1, plot_climdiv, climate_dat = statediv30yrs, state = state_name)
}

# Run through each state.
state_list = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Idaho", "Illinois", 
               "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", 
               "Missouri", "Montana", "Nebraska", "Nevada", "NewHampshire", "NewJersey", "NewMexico", "NewYork", "NorthCarolina", "NorthDakota", 
               "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "RhodeIsland", "SouthCarolina", "SouthDakota", "Tennessee", "Texas", "Utah", "Vermont", 
               "Virginia", "Washington", "WestVirginia", "Wisconsin", "Wyoming")

apply(as.array(state_list), 1, state_climdiv)
