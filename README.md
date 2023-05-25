*updated: 22 May, 2023*

# The MARISA RealTime Data Map
The [Climate Data Portal](https://www.marisa.psu.edu) for the NOAA Mid-Atlantic Regional Integrated Sciences and Assessments ([MARISA](https://www.midatlanticrisa.org)) provides users with a common entry point for accessing and visualizing a selection of Earth system and natural hazard data sources curated for their relevance to the Mid-Atlantic region, with a particular focus on the Chesapeake Bay watershed.

A key feature of the portal is an interactive, web-based map ([https://www.marisa.psu.edu/rtdatamap/](https://www.marisa.psu.edu/rtdatamap/)) containing real-time observations to provide a picture of meteorological and hydrological conditions in the Mid-Atlantic region. The map was developed for public engagement and also to help more advanced users in the region by simplifying access to information from multiple data sources. The map is also designed to help support evidence-based decisionmaking for a variety of contexts ranging from outdoor recreation to municipal and business planning, science, and education.

## Requirements
The web-map is written using the [Leaflet JavaScript library](https://leafletjs.com), which is an open-source library for building interactive web mapping applications. It is both mobile and desktop friendly. The real-time observations are stored in JS, JSON, and GeoJSON files and loaded to the website in the _includes/leaflet.html file within the marisa-cdp GitHub repository. These files include static and dynamic data.

The backend is written in R and are automated to run on a schedule using cron jobs. The scripts are located in ~/map_data/ of idocrase.geosc.psu.edu and the output is sent to ~/marisa.psu.edu/mapdata where the output is loaded to the website via URLs.

R libraries:

* RCurl
* measurements
* xml2
* httr
* XML
* data.table
* jsonlite
* terra
* sf
* geojsonsf
* geojsonio

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

### TO Update

| Data | File (~/marisa.psu.edu/mapdata/) | Script (~/map\_data) | Extent | Source | Notes |
| ---- | ---- | ------ | ------ | ------ | ----- |
| Weather stations | weather\_observations\_extend.json | weather\_obs\_US.R | National | [NOAA's National Weather Service](https://w1.weather.gov/xml/current_obs/) | updated hourly (new job at 10 mins of every hour: 1:10, 2:10, 3:10) |
| U.S. climate divisions | clim\_div\_fig\_6prec\_nodup.geojson | Clim\_Div\_Clim\_Data/climate\_div\_plotting.R | National | https://www.ncdc.noaa.gov/monitoring-references/maps/us-climate-divisions.php | Static |
| U.S. drought monitor | [web map service layer](http://ndmc-001.unl.edu:8080/cgi-bin/mapserv.exe?map=/ms4w/apps/usdm/service/usdm\_current\_wms.map&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap) | Web map service | National | [National Drought Mitigation Center, the U.S. Department of Agriculture, and the National Oceanic and Atmospheric Association](https://droughtmonitor.unl.edu) | Updated every Thursday at 8:30 a.m. Eastern time |
| Weather Radar | [web map service layer](https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi) | Web map service | National | [Iowa Environmental Mesonet (IEM) generated NEXRAD (Next-Generation Radar) composite](https://mesonet.agron.iastate.edu/docs/nexrad_mosaic/) | Layer: NEXRAD Base Reflectivity current (nexrad-n0q-900913); (Data updated every 5 minutes) |
| Chesapeake Bay Watershed | chesbay_small.geojson | -- | Chesapeake Bay watershed | [United States Department of Agriculture; Natural resources Conservation Service New York](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/ny/programs/farmbill/rcpp/?cid=stelprdb1254128) | Static |
| Hydrologic unit code 4, 6, 8, 10, & 12 | huc-4\_chesapeake.json <br> huc-6\_chesapeake.json <br> huc-8\_chesapeake.json <br> huc-10\_chesapeake.json <br> huc-12\_chesapeake\_color.json | -- | Chesapeake Bay watershed | [HUC layers are from the USGS Watershed Boundary Dataset](https://catalog.data.gov/dataset/usgs-national-watershed-boundary-dataset-wbd-downloadable-data-collection-national-geospatial-) | Static |
|Diagnositics test | diagnostic.json | diagnostics.R | buoys, weather stations, tide station plots and observations, and stream gage observations | -- | run hourly; stream gage plots are turned off due to testing of US extent|

I have automated the scripts to run regularly on idocrase.geosc.psu.edu using cron jobs. The scripts are located in ~/map_data/ and the output is sent to ~/marisa.psu.edu/mapdata where the output is loaded to the website via URLs.

```
10 * * * * cd /home/staff/mdl5548/map_data/; Rscript /home/staff/mdl5548/map_data/weather_obs_US.R
*/15 * * * * cd /home/staff/mdl5548/map_data/; Rscript /home/staff/mdl5548/map_data/buoy_obs_US.R
*/10 * * * * cd /home/staff/mdl5548/map_data/; Rscript /home/staff/mdl5548/map_data/tide_gage_plots_US.R
*/20 * * * * cd /home/staff/mdl5548/map_data/; Rscript /home/staff/mdl5548/map_data/tide_obs_US.R
0 * * * * cd /home/staff/mdl5548/map_data/; Rscript /home/staff/mdl5548/map_data/diagnostics.R

0 */8 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/NJMD_stream_plots.R
0 */8 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/NY_stream_plots.R
0 */8 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/OHDEDCWVTNNCCTMA_stream_plots.R
0 */8 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/PA_stream_plots.R
0 */8 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/VA_stream_plots.R

0 */2 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/NJMD_stream_gage_obs.R
0 */2 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/NY_stream_gage_obs.R
0 */2 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/OHDEDCWVTNNCCTMA_stream_gage_obs.R
0 */2 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/PA_stream_gage_obs.R
0 */2 * * * cd /home/staff/mdl5548/map_data/stream_gage_scripts/; Rscript /home/staff/mdl5548/map_data/stream_gage_scripts/VA_stream_gage_obs.R

*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/DEtoNYwarnings_watches_advisories.R
*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/OHPAwarnings_watches_advisories.R
*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/VAWVwarnings_watches_advisories.R

*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/greatlakes_warnings_watches_advisories.R
*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/atlantic_warnings_watches_advisories_p1.R
*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/atlantic_warnings_watches_advisories_p2.R
*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/atlantic_warnings_watches_advisories_p3.R
*/6 * * * * cd /home/staff/mdl5548/map_data/warnings_watches_advisories/; Rscript /home/staff/mdl5548/map_data/warnings_watches_advisories/atlantic_warnings_watches_advisories_p4.R
```

## Website URL
www.marisa.psu.edu/map

## For questions contact
Kelsey Ruckert (klr324@psu.edu)  
Rob Nicholas (ren10@psu.edu)

# marisa-map-backup
Backup scripts and static data for the interactive web-map at https://www.marisa.psu.edu/rtdatamap/
