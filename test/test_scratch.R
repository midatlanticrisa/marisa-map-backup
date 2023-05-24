library(xml2)
library(XML)

GID = "parp1"
GID = "axep1"

# Read the xml file
riverURL <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=", GID,"&output=xml")
riverData = read_xml(riverURL)

# Parse into an R structure representing XML tree
riverXml <- xmlParse(riverData)

stages_df = xmlToDataFrame(nodes=getNodeSet(riverXml, "//sigstages"))
stages_df[stages_df == ""] = NA

# Convert the parsed XML to a dataframe of observations
obs_df <- xmlToDataFrame(nodes=getNodeSet(riverXml, "//observed//datum"))

# Update the column names if data exists, otherwise set data to NA
if(length(obs_df) == 0){
  obs_df <- NA
} else {
  colnames(obs_df) <- c("time", "height", "discharge", "pedts")
}

# Convert the parsed XML to a dataframe of forecasts
for_df <- xmlToDataFrame(nodes=getNodeSet(riverXml, "//forecast//datum"))

# Update the column names if data exists, otherwise set data to NA
if(length(for_df) == 0){
  for_df <- NA
} else {
  colnames(for_df) <- c("time", "height", "discharge", "pedts")
}

return(list(ID = GID, obs = obs_df, forecast = for_df))




# https://water.weather.gov/ahps/download.php
download.file("https://water.weather.gov/ahps/download.php?data=tgz_obs", destfile="ahps_shp.tgz")
system("tar -zxvf ahps_shp.tgz -C ahps_shp/", show.output.on.console = FALSE)

if (!file.exists("ahps_shp/")){
  dir.create("ahps_shp/", recursive=T)
}

ahps <- vect("ahps_shp/national_shapefile_obs.shp")
crs(ahps) <- "EPSG:4326"

bbox = c(-82.0, -73.0, 36.46, 43.75)

# Return a subset of buoys or all buoys?
if(is.null(bbox)){
  streams <- ahps
  
} else {
  # bbox: c('xmin','ymin','xmax','ymax')
  streams <- ahps[ahps$Longitude >= bbox[1] & 
                    ahps$Longitude <= bbox[2] & 
                    ahps$Latitude >= bbox[3] & 
                    ahps$Latitude <= bbox[4], ]
}


spatVect_sf = st_as_sf(streams)

# outlookString <- paste0("outlookCounties", stateab, " = ", as.character(geojson_json(projVA)))
cat(as.character(geojson_json(spatVect_sf)), file="ahps.json")


untar("test.tgz")





polygon_WITHOUT_dat = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.0), c(100, 1.01)))
polygon_without = init$Polygon(polygon_WITHOUT_dat, stringify = TRUE)
plot(polygon_without)

# Polygon (WITH interior rings)
polygon_WITH_dat = list(list(c(100, 1.01), c(200, 2.01), c(100, 1.0), c(100, 1.01)),
                        list(c(50, 0.5), c(50, 0.8), c(50, 0.9), c(50, 0.5)))
polygon_with = init$Polygon(polygon_WITH_dat, stringify = TRUE)
polygon_with
# MultiPolygon
# the first polygon is without interior rings and the second one is with interior rings
multi_polygon_dat = list(list(list(c(102, 2.0), c(103, 2.0), c(103, 3.0), c(102, 2.0))),
                         list(list(c(100, 0.0), c(101, 1.0), c(101, 1.0), c(100, 0.0)),
                              list(c(100.2, 0.2), c(100.2, 0.8), c(100.8, 0.8), c(100.2, 0.2))))
multi_polygon = init$MultiPolygon(multi_polygon_dat, stringify = TRUE)
multi_polygon






install.packages("geojsonR")
library(geojsonR)
area = c("PA", "MD", "NY")
weatherURL <- "https://api.weather.gov/alerts/active"

weatherURL <- "https://api.weather.gov/alerts/active?area=PA"

tt = GET(weatherURL)
file_js = FROM_GeoJson(url_file_string = weatherURL)
library(geojsonio)

library(sf)
cntyShp = "data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp"
shp = st_read(cntyShp)
shp = st_transform(shp, 4326) #"EPSG:4326"
dissol_shp <- aggregate(shp[cnty_ind, ])


metaList$obs$time, metaList$obs$discharge
metaList$obs$time, metaList$obs$height



app = data.frame(time = as.character(seq(as.POSIXct(metaList$obs$time[1]), by="day", length.out=10)+1), 
           height = 1:10, discharge=rnorm(10))

metaList$obs = rbind(metaList$obs[1, ], app)

metaList$action = 2
metaList$minor = 4 
metaList$mod = 6 
metaList$major = 8 

metaList$obs[metaList$obs == "-999.00"]=NA

multi_point = init$MultiPoint(shp[cnty_ind, ])

list(shp[cnty_ind, ]$geometry)

multicoord = st_coordinates(shp[cnty_ind, ])

library(geojsonsf)
geo <- sf_geojson(shp[cnty_ind, ])

##Loop to calculate values for each observation decade 
polynum = unique(multicoord[,"L2"])
obsdecade.avg <- lapply(polynum, function(dec){as.data.frame(multicoord[multicoord[,"L2"]==dec,1:2])})
plot(obsdecade.avg[[1]], type="l")
lines(obsdecade.avg[[2]])
lines(obsdecade.avg[[3]])


spatVect_sf = st_as_sf(dissol)
plot(spatVect_sf)
geosV <- sf_geojson(spatVect_sf)
cat(geosV, file="geosf.geojson")

mul <- fromJSON(geosV)

features$geometry$coordinates[null_geom_id[i]][[1]] <- mul$coordinates
features$geometry$type[null_geom_id[i]] = mul$type

init = TO_GeoJson$new()
multi_point = init$MultiPoint(crds(dissol), stringify = TRUE)

test = data.frame(coordinates=I(vector('list', length(features$geometry))), 
           type=rep(NA,length(features$geometry)))

my_list=vector('list', length(features$geometry))


features$geometry = list(coordinates=vector('list', length(features$geometry)), 
                         type=vector('list', length(features$geometry)))

# https://stackoverflow.com/questions/68784976/reading-in-a-geojson-file-with-geojsonio-geojsonr
library(jsonlite)
testr2 = read_json(weatherURL)
test <- jsonlite::fromJSON(weatherURL)

write_json(features, "testweather.json")

df = subset(features$properties, select = -c(parameters,sender,senderName))

drop <- "properties$parameters"
df = mydata[,!(names(features$properties) %in% drop)]

## Read and extract the US state
# "cb_2018_us_state_500k" is the 2018 spatial census data 
# (https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html), 
# which is used for general reference of US counties unless a higher detailed product is 
# required. It is also the layer in which was used to create the "drop" maps in the 
# header of the Outlooks.
library(terra)
usStates <- vect("data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")

# convert the coordinate system to WGS84 for consistency with web mapping
crs(usStates) <- "EPSG:4326"

fullFips <- paste0("0", usStates$STATEFP, usStates$COUNTYFP)
cnty_ind <- which(fullFips %in% ptest$properties$geocode$SAME[3][[1]])
subcnty <- usStates[cnty_ind, ]
dissol <- aggregate(subcnty)
coords <- crds(pm)

# pass these vectors as input to the array.
#  4 rows,2 columns and 3 arrays
result <- array(NA, dim = c(1,nrow(coords),2))
result[,,1] <- coords[ ,"x"]
result[,,2] <- coords[ ,"y"]
ptest$geometry$coordinates[3][[1]] <- result
ptest$geometry$type[3] = "Polygon"

colfle <- read.csv("WeatherEventColors.csv", skip=2, header=TRUE)

ptest$properties$color[1] <- paste0("#", colfle$Hex.Code[which(colfle$Hazard...Weather.Event == ptest$properties$event[1])])

plot(crds(pm), type="l")

which(fullFips == ptest$properties$geocode$SAME[3][[1]][1])

which(fullFips == "036115")

baseSelect <- dataTab[which((dataTab$decade %in% past.decades) & dataTab$FIPS==fips),]

# Extract just the state
st <- usStates[which(usStates@data$STATEFP == fips), ]



ptest <- test$features
is.null(ptest$geometry$coordinates[3][[1]])

tyf = is.na(sapply(ptest$geometry$type, "[[", 1))

ptest$properties$geocode$SAME[3]

extract the coordinates from fips in shapefile and set as coordinates.

ptest$properties$description[1]

ptest$geometry$type[466] = "Polygon"

spdf <- geojson_read(weatherURL, "geojson", what="list")

download.file(weatherURL, destfile="alert.geojson")
spdf <- geojson_read("alert.geojson")

tt = readLines(weatherURL)

startid <- grep('type": "FeatureCollection",', tt)


endid <- grep('"title":', tt)
startid <- grep('"id": "https', tt) - 1

endIDs <- c(startid[2:length(startid)] - 1, endid - 2)
extractt <- lapply(X=1:length(startid), function(X){tt[startid[X]:endIDs[X]]})

length(grep('"geometry": null,', extractt[[8]])) == 1

st_fips = grep('"SAME": \\[', extractt[[7]])+1
end_fips = grep('"UGC": \\[', extractt[[7]])-2

fips <- trimws(gsub('\\"|,', "", extractt[[7]][st_fips:end_fips]))

any(extractt == "geometry")