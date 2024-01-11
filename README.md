*updated: 11 Jan, 2024*

# The MARISA RealTime Data Map
The [Climate Data Portal](https://www.marisa.psu.edu) for the NOAA Mid-Atlantic Regional Integrated Sciences and Assessments ([MARISA](https://www.midatlanticrisa.org)) provides users with a common entry point for accessing and visualizing a selection of Earth system and natural hazard data sources curated for their relevance to the Mid-Atlantic region, with a particular focus on the Chesapeake Bay watershed.

A key feature of the portal is an interactive, web-based map ([https://www.marisa.psu.edu/rtdatamap/](https://www.marisa.psu.edu/rtdatamap/)) containing real-time observations to provide a picture of meteorological and hydrological conditions in the Mid-Atlantic region. The map was developed for public engagement and also to help more advanced users in the region by simplifying access to information from multiple data sources. The map is also designed to help support evidence-based decisionmaking for a variety of contexts ranging from outdoor recreation to municipal and business planning, science, and education.

This repository contains the backup scripts and static data for the realtime data map.

## Requirements
The web-map is written using the [Leaflet JavaScript library](https://leafletjs.com), which is an open-source library for building interactive web mapping applications. It is both mobile and desktop friendly. The real-time observations are stored in JSON and GeoJSON files and loaded to the website in the _includes/leaflet.html file within the marisa-cdp GitHub repository. These files include static and dynamic data.

The backend is written in R and are automated to run on a schedule using cron jobs. The scripts are located in /clima/rtdatamap/ of eesi-clima-downloads.ems.psu.edu and the output is sent to /var/www/html/rtdatamap/ where the output is loaded to the website via https://download.clima.psu.edu/rtdatamap/.

R libraries:

* data.table
* geojsonio
* geojsonsf
* httr
* jsonlite
* measurements
* plyr
* RCurl
* sf
* terra
* XML
* xml2
* stringi
* stringr

## Organization

* **scripts Folder**: The backend source code folder! Contains all the files and functions to download and parse the source data.
* **test Folder**: Test files for development.
* **dependencies Folder**: Directory of dependencies.
* **depreciated Folder**: Old, unused scripts and files.
* **doc Folder**: The documentation folder
* **resources Folder**: Static resources in your project. For example, icon images, shapefiles, climate division files.
* **output_examples Folder**: Provides examples of the outputs from the scripts in the scripts Folder.
* **tools Folder**: Cron file which automates the downloading of data.

## Data Sources

| Data | Source | Temporal resolution | Spatial Resolution | Script | Output | Notes |
| ---- | ---- | ------ | ------ | ------ | ----- | ----- |
| Tide stations | [NOAA Tides & Currents](https://tidesandcurrents.noaa.gov) | 6 mins | -82.0, -73.0, 36.46, 43.75 | noaa\_tide\_obs.R | NOAAtideIDs.txt NOAATideStations.js Tide_figs/ | IDs updated once a day |
| Bouys | [NOAA's National Data Buoy Center](https://www.ndbc.noaa.gov) | 5 mins | -82.0, -73.0, 36.46, 43.75 | ndbc\_buoy\_obs.R | NDBCbuoys.js | IDs updated once a day |
| River Gauges | [NOAA's National Weather Service Advanced Hydrologic Prediction Service (AHPS)](https://water.weather.gov/ahps/index.php) | 15 mins | -82.0, -73.0, 36.46, 43.75 | nws\_ahps\_river\_obs.R | NWSRiverGauges.geojson River_figs/ | Forecast download takes 36mins |
| Watches, warnings, & advisories | [NOAA's National Weather Service](https://www.weather.gov/documentation/services-web-api) | 2 mins | VA, PA, MD, DE, DC, WV, OH | nws_alerts.R | NWSalerts.geojson | Update to push service (NOAAPORT or NOAA Weather Wire Service (NWWS)) in the future to reduce update time to 45 seconds |
| U.S. climate divisions | https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-divisions.php | Static | National | Clim\_Div\_Clim\_Data/climate\_div\_plotting.R | clim\_div\_fig\_6prec\_nodup.geojson | -- | 
| U.S. drought monitor | [National Drought Mitigation Center, the U.S. Department of Agriculture, and the National Oceanic and Atmospheric Association](https://droughtmonitor.unl.edu) | every Thursday at 8:30 a.m. Eastern time | National | Web map service | [web map service layer](http://ndmc-001.unl.edu:8080/cgi-bin/mapserv.exe?map=/ms4w/apps/usdm/service/usdm\_current\_wms.map&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap) | -- | 
| Weather Radar | [Iowa Environmental Mesonet (IEM) generated NEXRAD (Next-Generation Radar) composite](https://mesonet.agron.iastate.edu/docs/nexrad_mosaic/) | 5 mins | National | Web map service | [web map service layer](https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi) | Layer: NEXRAD Base Reflectivity current (nexrad-n0q-900913) |  
| Chesapeake Bay Watershed | [United States Department of Agriculture; Natural resources Conservation Service New York](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/ny/programs/farmbill/rcpp/?cid=stelprdb1254128) | Static | Chesapeake Bay watershed | -- | chesbay_small.geojson | -- | 
| Hydrologic unit code 4, 6, 8, 10, & 12 | [HUC layers are from the USGS Watershed Boundary Dataset](https://catalog.data.gov/dataset/usgs-national-watershed-boundary-dataset-wbd-downloadable-data-collection-national-geospatial-) | Static | Chesapeake Bay watershed | -- | huc-4\_chesapeake.json <br> huc-6\_chesapeake.json <br> huc-8\_chesapeake.json <br> huc-10\_chesapeake.json <br> huc-12\_chesapeake\_color.json | -- | 
| Diagnositics test | Tests whether outputs are within 24hrs old | daily at 8 a.m. | -- | daily_diagnostics.R | email | -- |  
| Weather stations | DEOS: Delaware Environmental Observing System <br> DelDOT: Delaware Department of Transportation Weather Network <br> PennDot: Pennsylvania Department of Transportation Weather Network <br> PA Turnpike <br> PEMN: Penn State University <br> PA Dept. of Environmental Protection	<br> PA Dept. Conservation & Natural Resources <br> METAR | 5, 10, 15, 60 mins | -82.0, -73.0, 36.46, 43.75 | mesonet\_de\_obs.R <br> mesonet\_pa\_obs.R <br> noaa\_metar\_meta.R <br> noaa\_metar\_obs.R | DEMesonet.js <br> PAMesonet.js <br> NoaaMetar.js | -- |

Scripts are run regularly using cron jobs. 

```
# Run alerts (a pull service) every 2 minutes as some alerts (e.g., Tornado Warnings) are time sensitive
*/2 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript nws_alerts.R > /dev/null 2>&1

# Update buoys and mesonet weather every 5 minutes
*/5 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript ndbc_buoy_obs.R > /dev/null 2>&1
*/5 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript mesonet_de_obs.R > /dev/null 2>&1
*/5 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript mesonet_pa_obs.R > /dev/null 2>&1

# Update METAR weather every minute (only update the station metadata once a day at 1am)
*/1 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript noaa_metar_obs.R > /dev/null 2>&1
0 1 * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript noaa_metar_meta.R > /dev/null 2>&1

# Run tides every 6 minutes
*/6 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript noaa_tide_obs.R > /dev/null 2>&1

# Run streams every 15 minutes
*/15 * * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript nws_ahps_river_obs.R > /dev/null 2>&1 

# Run the diagnostics everyday at 8am. This is the only job to send emails
0 8 * * * cd /clima/rtdatamap/scripts/;  apptainer exec --bind /d1/www_html,/var/www,/clima/rtdatamap ../rtdatamap_jammy.simg Rscript daily_diagnostics.R
```

## Website URL
www.marisa.psu.edu/rtdatamap/

## For questions contact
Kelsey Ruckert (klr324@psu.edu)  
Rob Nicholas (ren10@psu.edu)
