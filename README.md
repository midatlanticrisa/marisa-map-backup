*updated: 16 Aug., 2019*

# Managing the MARISA Web-map
The [Climate Data Portal](https://www.marisa.psu.edu) for the NOAA Mid-Atlantic Regional Integrated Sciences and Assessments ([MARISA](https://www.midatlanticrisa.org)) provides users with a common entry point for accessing and visualizing a selection of Earth system and natural hazard data sources curated for their relevance to the Mid-Atlantic region, with a particular focus on the Chesapeake Bay watershed.

A key feature of the portal is an interactive, web-based map ([www.marisa.psu.edu/map](www.marisa.psu.edu/map)) containing real-time observations to provide a picture of meteorological and hydrological conditions in the Mid-Atlantic region. The map is being developed for public engagement and also to help more advanced users in the region by simplifying access to information and delivering data in an understandable way. The map will also be designed to help support evidence-based decisionmaking for a variety of contexts ranging from outdoor recreation to municipal and business planning, science, and education.

The web-map is written using the [Leaflet JavaScript library](https://leafletjs.com), which is an open-source library for building interactive web mapping applications. It is both mobile and desktop friendly. The real-time observations are stored in JS, JSON, and GeoJSON files and loaded to the website in the _includes/leaflet.html file within the marisa-cdp GitHub repository. These files include static and dynamic data.

| Data | File (~/marisa.psu.edu/mapdata/) | Script (~/map\_data) | Extent | Source | Notes |
| ---- | ---- | ------ | ------ | ------ | ----- |
| Tide stations | tide\_station\_obs\_extend.js <br> Tide\_figs/Fig\_[*station ID*].png | tide\_obs\_US.R <br> tide\_gage\_plots\_US.R | National | [NOAA Tides & Currents](https://tidesandcurrents.noaa.gov) | Plots are updated every 10 minutes; observations every 20 minutes (real-time water observations update every 6 minutes) |
| Buoys | buoys_extend.js | buoy\_obs\_US.R | National | [NOAA's National Data Buoy Center](https://www.ndbc.noaa.gov) | Updated every 15 minutes (real-time obs. updated every 5 mins). In future use [guidelines](https://www.ndbc.noaa.gov/docs/ndbc_web_data_guide.pdf) to update code processing (https://www.ndbc.noaa.gov/data/latest\_obs/latest\_obs.txt) |
| Stream gauges | NJMD\_stream\_obs.js <br> NY\_stream\_obs.js <br> OHDEDCWVTNNCCTMA\_stream\_obs.js <br> PA\_stream\_obs.js <br> VA\_stream\_obs.js <br> Stream\_figs/Fig\_[station ID*].png | stream\_gage\_scripts/NJMD\_stream\_plots.R <br> stream\_gage\_scripts/NY\_stream\_plots.R <br> stream\_gage\_scripts/OHDEDCWVTNNCCTMA\_stream\_plots.R <br> stream\_gage\_scripts/PA\_stream\_plots.R <br> stream\_gage\_scripts/VA\_stream\_plots.R <br> stream\_gage\_scripts/NJMD\_stream\_gage\_obs.R <br> stream\_gage\_scripts/NY\_stream\_gage\_obs.R <br> stream\_gage\_scripts/OHDEDCWVTNNCCTMA\_stream\_gage\_obs.R <br> stream\_gage\_scripts/PA\_stream\_gage\_obs.R <br> stream\_gage\_scripts/VA\_stream\_gage\_obs.R | Mid-Atlantic | [United States Geological Survey](https://waterdata.usgs.gov/nwis/rt)| Plots updated every 8 hours; observations every 2 hours (real-time streamflow data are typically recorded at 15-minute intervals, stored onsite, and then transmitted to USGS offices once every hour) |
| Weather stations | weather\_observations\_extend.json | weather\_obs\_US.R | National | [NOAA's National Weather Service](https://w1.weather.gov/xml/current_obs/) | updated hourly (new job at 10 mins of every hour: 1:10, 2:10, 3:10) |
| Watches, warnings, & advisories | DEtoNYcounty\_alerts.json <br> OHPAcounty\_alerts.json <br> VAWVcounty\_alerts.json <br> greatlakes\_alerts.json <br> atlantic\_p1\_alerts.json <br> atlantic\_p2\_alerts.json <br> atlantic\_p3\_alerts.json <br> atlantic\_p4\_alerts.json | warnings\_watches\_advisories/DEtoNYwarnings\_watches\_advisories.R <br> warnings\_watches\_advisories/OHPAwarnings\_watches\_advisories.R <br> warnings\_watches\_advisories/VAWVwarnings\_watches\_advisories.R <br> warnings\_watches\_advisories/greatlakes\_warnings\_watches\_advisories.R <br> warnings\_watches\_advisories/atlantic\_warnings\_watches\_advisories\_p1.R <br> warnings\_watches\_advisories/atlantic\_warnings\_watches\_advisories\_p2.R <br> warnings\_watches\_advisories/atlantic\_warnings\_watches\_advisories\_p3.R <br> warnings\_watches\_advisories/atlantic\_warnings\_watches\_advisories\_p4.R | Mid-Atlantic | [NOAA's National Weather Service](https://alerts.weather.gov) | Updated every 6 minutes (updates approximately every 5 minutes with all warnings, watches, advisories, and statements in effect) |
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
Backup scripts and static data for the interactive web-map at www.marisa.psu.edu/map
