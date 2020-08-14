library(raster)
library(rvest)
library(pbapply)

# what computer am I on?
comp <- as.data.frame(t(Sys.info()))

# important file locations
if(comp$nodename=="E2-EES-RSML638.local"){  ##workstation
  inDir <- "/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/"
  outDir <- "/Users/mdl5548/Documents/MARISA_outDepot/"
}else if(comp$nodename=="lisk-ZBOX-CI320NANO-series"){  ##zbox
  inDir <- "/home/mdl5548/Documents/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/home/mdl5548/Documents/MARISA_outDepot/"
}else{  ##idocrase
  inDir <- "/home/staff/mdl5548/githubRepos/marisa-map-backup/scripts/"
  outDir <- "/net/www/www.marisa.psu.edu/htdocs/mapdata/"
}

##set up directories in order to download and process radar
downDir <- paste0(inDir, "radar/downloads/")
clipDir <- paste0(outDir, "radar/")
if(dir.exists(downDir)==F){dir.create(downDir, recursive=T)}
if(dir.exists(clipDir)==F){dir.create(clipDir, recursive=T)}

##site indentity
baseURL <- "http://mesonet.agron.iastate.edu"
SiteURL <- paste0(baseURL, '/current/mcview.phtml')

##get site nodes
webpage <- read_html(url)
webSession <- html_session(url)
##extract nodes and get thier text, may need to change based on css of website
getNodes <- html_nodes(webpage, 'a')
getText <- html_text(getNodes)

GTLinks <- grep("GeoTiff", getText)  ##order is oldest first, newest last

getDatLink <- paste0(baseURL, html_attr(getNodes[GTLinks[1]], "href"))

download.file(getDatLink, paste0(downDir, "dlRad_minus55.zip"))







sapply(strsplit(getNodes[GTLinks[1]], "href="), "[[", 2)



follow_link(webSession, getNodes[GTLinks[1]])





sppPage <- read_html(follow_link(sess, link))



bound <- extent()

#https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi
#rast <- raster("https://mesonet.agron.iastate.edu/archive/data/2020/08/14/GIS/uscomp/n0q_202008140000.png")

#http://mesonet.agron.iastate.edu/request/gis/n0q2gtiff.php?dstr=202008141955
rast <- raster("http://mesonet.agron.iastate.edu/request/gis/n0q2gtiff.php?dstr=202008141955")

rast <- raster("/Users/mdl5548/Documents/n0q_202008141955.tif")




