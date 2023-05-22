library(raster)
library(rvest)
library(pbapply)
library(parallel)

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

cores <- 1

##set up directories in order to download and process radar
downDir <- paste0(inDir, "radar/downloads/")
clipDir <- paste0(outDir, "radar/")
if(dir.exists(downDir)==F){dir.create(downDir, recursive=T)}
if(dir.exists(clipDir)==F){dir.create(clipDir, recursive=T)}

#Viewport bounds plus a little extra
cropEx <- extent(c(-85.0, -70.0, 33.0, 46.5))  ##actual coordinates
#tempRast <- raster("/Users/mdl5548/Documents/GitHub/marisa-map-backup/scripts/radar/downloads/n0q_202008171730.tif")
#northRow <- rowFromY(tempRast, 46.5)
#southRow <- rowFromY(tempRast, 33.0)
#eastRow <- colFromX(tempRast, -70.0)
#westRow <- colFromX(tempRast, -85.0)
#cropEx <- extent(c(8201, 11201, 701, 3401))  ##for png, if needed

##site indentity
baseURL <- "http://mesonet.agron.iastate.edu"
SiteURL <- paste0(baseURL, '/current/mcview.phtml')

##get site nodes
webpage <- read_html(SiteURL)
webSession <- html_session(SiteURL)
##extract nodes and get thier text, may need to change based on css of website
getNodes <- html_nodes(webpage, 'a')
getText <- html_text(getNodes)

GTLinks <- rev(grep("GeoTiff", getText))  ##original order is oldest first, newest last
#GTLinks <- rev(grep("n0q_", getText))

subLinks <- GTLinks[1:(length(GTLinks)-2)]
layerNames <- c("minus0", "minus5", "minus10", "minus15", "minus20", "minus25", "minus30", "minus35", "minus40", "minus45", "minus50")

if(cores>1){
  mcmapply(function(lnk, ln){getDatLink <- paste0(baseURL, html_attr(getNodes[lnk], "href"))
                              linkID <- sapply(strsplit(getDatLink, "="), "[[", 2)
                              downloadName <- paste0(downDir, "dlRad_", ln, ".zip")
                              download.file(getDatLink, downloadName)
                              unzip(downloadName, exdir=downDir)
                              findUnzip <- list.files(downDir, linkID, full.names=T)
                              rast <- raster(findUnzip)
                              names(rast) <- ln
                              crRast <- crop(rast, cropEx)
                              writeRaster(crRast, paste0(clipDir, "cropped_", ln, ".tif"), overwrite=T)}, lnk=subLinks, ln=layerNames, mc.cores=cores)
}else{
  #lnk <- subLinks[1]
  #ln <- layerNames[1]
  pbmapply(function(lnk, ln){getDatLink <- paste0(baseURL, html_attr(getNodes[lnk], "href"))
                              linkID <- sapply(strsplit(getDatLink, "="), "[[", 2)
                              downloadName <- paste0(downDir, "dlRad_", ln, ".zip")
                              #linkID <- sapply(strsplit(getDatLink, "n0q_|.png"), "[[", 2)
                              #downloadName <- paste0(downDir, "dlRad_", ln, ".png")
                              download.file(getDatLink, downloadName)
                              unzip(downloadName, exdir=downDir)
                              findUnzip <- list.files(downDir, linkID, full.names=T)
                              #findUnzip <- list.files(downDir, ln, full.names=T)
                              rast <- raster(findUnzip)
                              names(rast) <- ln
                              crRast <- crop(rast, cropEx)
                              writeRaster(crRast, paste0(clipDir, "cropped_", ln, ".asc"), overwrite=T)}, lnk=subLinks, ln=layerNames)
                              #writeRaster(crRast, paste0(clipDir, "cropped_", ln, ".tif"), overwrite=T)}, lnk=subLinks, ln=layerNames)
}  

listTif <- list.files(downDir, ".tif", full.names=T)
sapply(listTif, function(fn){if(file.exists(fn)){file.remove(fn)}})




#rrr <- raster("/Users/mdl5548/Documents/junk/chkCroppedRadar/cropped_minus0.tif")

