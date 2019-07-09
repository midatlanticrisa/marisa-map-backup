# --------------------------------------------------------------------------------------------------------------------
# Copyright 2018 The Pennsylvania State University
#
# Kelsey Ruckert (klr324@psu.edu)
# Last edit: June 14, 2018
# Last edit: June 7, 2018
#
# This script parses RSS meteorological data from buoy stations from the 
# National Data Buoy Center and outputs the results in a single file.
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
# Read in saved data 
AL_OBS = readRDS("alabama_obs.rds")
AK_OBS = readRDS("alaska_obs.rds")
AZ_OBS = readRDS("arizona_obs.rds")
AR_OBS = readRDS("arkansas_obs.rds")
CA_OBS = readRDS("california_obs.rds")
CO_OBS = readRDS("colorado_obs.rds")
CT_OBS = readRDS("conneticut_obs.rds")
DC_OBS = readRDS("district_of_columbia_obs.rds")
DE_OBS = readRDS("delaware_obs.rds")
FL_OBS = readRDS("florida_obs.rds")
GA_OBS = readRDS("georgia_obs.rds")
HI_OBS = readRDS("hawaii_obs.rds")
ID_OBS = readRDS("idaho_obs.rds")
IL_OBS = readRDS("illinois_obs.rds")
IN_OBS = readRDS("indiana_obs.rds")
IA_OBS = readRDS("iowa_obs.rds")
KS_OBS = readRDS("kansas_obs.rds")
KY_OBS = readRDS("kentucky_obs.rds")
LA_OBS = readRDS("louisiana_obs.rds")
ME_OBS = readRDS("maine_obs.rds")
MD_OBS = readRDS("maryland_obs.rds")
MA_OBS = readRDS("massachusetts_obs.rds")
MI_OBS = readRDS("michigan_obs.rds")
MN_OBS = readRDS("minnesota_obs.rds")
MS_OBS = readRDS("mississippi_obs.rds")
MO_OBS = readRDS("missouri_obs.rds")
MT_OBS = readRDS("montana_obs.rds")
NE_OBS = readRDS("nebraska_obs.rds")
NV_OBS = readRDS("nevada_obs.rds")
NH_OBS = readRDS("newhampshire_obs.rds")
NJ_OBS = readRDS("newjersey_obs.rds")
NM_OBS = readRDS("newmexico_obs.rds")
NY_OBS = readRDS("newyork_obs.rds")
NC_OBS = readRDS("northcarolina_obs.rds")
ND_OBS = readRDS("northdakota_obs.rds")
OH_OBS = readRDS("ohio_obs.rds")
OK_OBS = readRDS("oklahoma_obs.rds")
OR_OBS = readRDS("oregon_obs.rds")
PA_OBS = readRDS("pennsylvania_obs.rds")
RI_OBS = readRDS("rhodeisland_obs.rds")
SC_OBS = readRDS("southcarolina_obs.rds")
SD_OBS = readRDS("southdakota_obs.rds")
TN_OBS = readRDS("tennessee_obs.rds")
TX_OBS = readRDS("texas_obs.rds")
UT_OBS = readRDS("utah_obs.rds")
VT_OBS = readRDS("vermont_obs.rds")
VA_OBS = readRDS("virginia_obs.rds")
WA_OBS = readRDS("washington_obs.rds")
WV_OBS = readRDS("westvirginia_obs.rds")
WI_OBS = readRDS("wisconsin_obs.rds")
WY_OBS = readRDS("wyoming_obs.rds")

# --------------------------------------------------------------------------------------------------------------------
# Format information to a feature string for json
stream_string = function(ID, name, link, obs, time, lon, lat){
  str = paste('{"type": "Feature", "properties": {"name": "', name[match(ID, ID)], '", "id": "', ID, '", "url": "', link[match(ID, ID)], '", "obs": "', 
              obs[match(ID, ID)], '", "time": "', time[match(ID, ID)], '", "discharge": "https://www.marisa.psu.edu/mapdata/Stream_figs/Fig_', ID, 
              '.png"}, "geometry": {"type": "Point", "coordinates": [', lon[match(ID, ID)], ',',  lat[match(ID, ID)], ']}}', sep="")
  return(str)
}
# -------------------------------------------------------------------------------------------------------------------- 
# Combine all info into one string
al_st = stream_string(AL_ID$ID, AL_ID$name, AL_ID$url, AL_OBS$obs, AL_OBS$time, 
                      AL_ID$lon, AL_ID$lat)
al_st = paste(al_st, ",", collapse="")
ak_st = stream_string(AK_ID$ID, AK_ID$name, AK_ID$url, AK_OBS$obs, AK_OBS$time, 
                      AK_ID$lon, AK_ID$lat)
ak_st = paste(ak_st, ",", collapse="")
az_st = stream_string(AZ_ID$ID, AZ_ID$name, AZ_ID$url, AZ_OBS$obs, AZ_OBS$time, 
                      AZ_ID$lon, AZ_ID$lat)
az_st = paste(az_st, ",", collapse="")
ar_st = stream_string(AR_ID$ID, AR_ID$name, AR_ID$url, AR_OBS$obs, AR_OBS$time, 
                      AR_ID$lon, AR_ID$lat)
ar_st = paste(ar_st, ",", collapse="")
ca_st = stream_string(CA_ID$ID, CA_ID$name, CA_ID$url, CA_OBS$obs, CA_OBS$time, 
                      CA_ID$lon, CA_ID$lat)
ca_st = paste(ca_st, ",", collapse="")
co_st = stream_string(CO_ID$ID, CO_ID$name, CO_ID$url, CO_OBS$obs, CO_OBS$time, 
                      CO_ID$lon, CO_ID$lat)
co_st = paste(co_st, ",", collapse="")
ct_st = stream_string(CT_ID$ID, CT_ID$name, CT_ID$url, CT_OBS$obs, CT_OBS$time, 
                      CT_ID$lon, CT_ID$lat)
ct_st = paste(ct_st, ",", collapse="")
dc_st = stream_string(DC_ID$ID, DC_ID$name, DC_ID$url, DC_OBS$obs, DC_OBS$time, 
                      DC_ID$lon, DC_ID$lat)
dc_st = paste(dc_st, ",", collapse="")
de_st = stream_string(DE_ID$ID, DE_ID$name, DE_ID$url, DE_OBS$obs, DE_OBS$time, 
                      DE_ID$lon, DE_ID$lat)
de_st = paste(de_st, ",", collapse="")
fl_st = stream_string(FL_ID$ID, FL_ID$name, FL_ID$url, FL_OBS$obs, FL_OBS$time, 
                      FL_ID$lon, FL_ID$lat)
fl_st = paste(fl_st, ",", collapse="")
ga_st = stream_string(GA_ID$ID, GA_ID$name, GA_ID$url, GA_OBS$obs, GA_OBS$time, 
                      GA_ID$lon, GA_ID$lat)
ga_st = paste(ga_st, ",", collapse="")
hi_st = stream_string(HI_ID$ID, HI_ID$name, HI_ID$url, HI_OBS$obs, HI_OBS$time, 
                      HI_ID$lon, HI_ID$lat)
hi_st = paste(hi_st, ",", collapse="")
id_st = stream_string(ID_ID$ID, ID_ID$name, ID_ID$url, ID_OBS$obs, ID_OBS$time, 
                      ID_ID$lon, ID_ID$lat)
id_st = paste(id_st, ",", collapse="")
il_st = stream_string(IL_ID$ID, IL_ID$name, IL_ID$url, IL_OBS$obs, IL_OBS$time, 
                      IL_ID$lon, IL_ID$lat)
il_st = paste(il_st, ",", collapse="")
in_st = stream_string(IN_ID$ID, IN_ID$name, IN_ID$url, IN_OBS$obs, IN_OBS$time, 
                      IN_ID$lon, IN_ID$lat)
in_st = paste(in_st, ",", collapse="")
ia_st = stream_string(IA_ID$ID, IA_ID$name, IA_ID$url, IA_OBS$obs, IA_OBS$time, 
                      IA_ID$lon, IA_ID$lat)
ia_st = paste(ia_st, ",", collapse="")
kn_st = stream_string(KN_ID$ID, KN_ID$name, KN_ID$url, KN_OBS$obs, KN_OBS$time, 
                      KN_ID$lon, KN_ID$lat)
kn_st = paste(kn_st, ",", collapse="")
ky_st = stream_string(KY_ID$ID, KY_ID$name, KY_ID$url, KY_OBS$obs, KY_OBS$time, 
                      KY_ID$lon, KY_ID$lat)
ky_st = paste(ky_st, ",", collapse="")
la_st = stream_string(LA_ID$ID, LA_ID$name, LA_ID$url, LA_OBS$obs, LA_OBS$time, 
                      LA_ID$lon, LA_ID$lat)
la_st = paste(la_st, ",", collapse="")
me_st = stream_string(ME_ID$ID, ME_ID$name, ME_ID$url, ME_OBS$obs, ME_OBS$time, 
                      ME_ID$lon, ME_ID$lat)
me_st = paste(me_st, ",", collapse="")
md_st = stream_string(MD_ID$ID, MD_ID$name, MD_ID$url, MD_OBS$obs, MD_OBS$time, 
                      MD_ID$lon, MD_ID$lat)
md_st = paste(md_st, ",", collapse="")
ma_st = stream_string(MA_ID$ID, MA_ID$name, MA_ID$url, MA_OBS$obs, MA_OBS$time, 
                      MA_ID$lon, MA_ID$lat)
ma_st = paste(ma_st, ",", collapse="")
mi_st = stream_string(MI_ID$ID, MI_ID$name, MI_ID$url, MI_OBS$obs, MI_OBS$time, 
                      MI_ID$lon, MI_ID$lat)
mi_st = paste(mi_st, ",", collapse="")
mn_st = stream_string(MN_ID$ID, MN_ID$name, MN_ID$url, MN_OBS$obs, MN_OBS$time, 
                      MN_ID$lon, MN_ID$lat)
mn_st = paste(mn_st, ",", collapse="")
ms_st = stream_string(MS_ID$ID, MS_ID$name, MS_ID$url, MS_OBS$obs, MS_OBS$time, 
                      MS_ID$lon, MS_ID$lat)
ms_st = paste(ms_st, ",", collapse="")
mo_st = stream_string(MO_ID$ID, MO_ID$name, MO_ID$url, MO_OBS$obs, MO_OBS$time, 
                      MO_ID$lon, MO_ID$lat)
mo_st = paste(mo_st, ",", collapse="")
mt_st = stream_string(MT_ID$ID, MT_ID$name, MT_ID$url, MT_OBS$obs, MT_OBS$time, 
                      MT_ID$lon, MT_ID$lat)
mt_st = paste(mt_st, ",", collapse="")
ne_st = stream_string(NE_ID$ID, NE_ID$name, NE_ID$url, NE_OBS$obs, NE_OBS$time, 
                      NE_ID$lon, NE_ID$lat)
ne_st = paste(ne_st, ",", collapse="")
nv_st = stream_string(NV_ID$ID, NV_ID$name, NV_ID$url, NV_OBS$obs, NV_OBS$time, 
                      NV_ID$lon, NV_ID$lat)
nv_st = paste(nv_st, ",", collapse="")
nh_st = stream_string(NH_ID$ID, NH_ID$name, NH_ID$url, NH_OBS$obs, NH_OBS$time, 
                      NH_ID$lon, NH_ID$lat)
nh_st = paste(nh_st, ",", collapse="")
nj_st = stream_string(NJ_ID$ID, NJ_ID$name, NJ_ID$url, NJ_OBS$obs, NJ_OBS$time, 
                      NJ_ID$lon, NJ_ID$lat)
nj_st = paste(nj_st, ",", collapse="")
nm_st = stream_string(NM_ID$ID, NM_ID$name, NM_ID$url, NM_OBS$obs, NM_OBS$time, 
                      NM_ID$lon, NM_ID$lat)
nm_st = paste(nm_st, ",", collapse="")
ny_st = stream_string(NY_ID$ID, NY_ID$name, NY_ID$url, NY_OBS$obs, NY_OBS$time, 
                      NY_ID$lon, NY_ID$lat)
ny_st = paste(ny_st, ",", collapse="")
nc_st = stream_string(NC_ID$ID, NC_ID$name, NC_ID$url, NC_OBS$obs, NC_OBS$time, 
                      NC_ID$lon, NC_ID$lat)
nc_st = paste(nc_st, ",", collapse="")
nd_st = stream_string(ND_ID$ID, ND_ID$name, ND_ID$url, ND_OBS$obs, ND_OBS$time, 
                      ND_ID$lon, ND_ID$lat)
nd_st = paste(nd_st, ",", collapse="")
oh_st = stream_string(OH_ID$ID, OH_ID$name, OH_ID$url, OH_OBS$obs, OH_OBS$time, 
                      OH_ID$lon, OH_ID$lat)
oh_st = paste(oh_st, ",", collapse="")
ok_st = stream_string(OK_ID$ID, OK_ID$name, OK_ID$url, OK_OBS$obs, OK_OBS$time, 
                      OK_ID$lon, OK_ID$lat)
ok_st = paste(ok_st, ",", collapse="")
or_st = stream_string(OR_ID$ID, OR_ID$name, OR_ID$url, OR_OBS$obs, OR_OBS$time, 
                      OR_ID$lon, OR_ID$lat)
or_st = paste(or_st, ",", collapse="")
pa_st = stream_string(PA_ID$ID, PA_ID$name, PA_ID$url, PA_OBS$obs, PA_OBS$time, 
                      PA_ID$lon, PA_ID$lat)
pa_st = paste(pa_st, ",", collapse="")
ri_st = stream_string(RI_ID$ID, RI_ID$name, RI_ID$url, RI_OBS$obs, RI_OBS$time, 
                      RI_ID$lon, RI_ID$lat)
ri_st = paste(ri_st, ",", collapse="")
sc_st = stream_string(SC_ID$ID, SC_ID$name, SC_ID$url, SC_OBS$obs, SC_OBS$time, 
                      SC_ID$lon, SC_ID$lat)
sc_st = paste(sc_st, ",", collapse="")
sd_st = stream_string(SD_ID$ID, SD_ID$name, SD_ID$url, SD_OBS$obs, SD_OBS$time, 
                      SD_ID$lon, SD_ID$lat)
sd_st = paste(sd_st, ",", collapse="")
tn_st = stream_string(TN_ID$ID, TN_ID$name, TN_ID$url, TN_OBS$obs, TN_OBS$time, 
                      TN_ID$lon, TN_ID$lat)
tn_st = paste(tn_st, ",", collapse="")
tx_st = stream_string(TX_ID$ID, TX_ID$name, TX_ID$url, TX_OBS$obs, TX_OBS$time, 
                      TX_ID$lon, TX_ID$lat)
tx_st = paste(tx_st, ",", collapse="")
ut_st = stream_string(UT_ID$ID, UT_ID$name, UT_ID$url, UT_OBS$obs, UT_OBS$time, 
                      UT_ID$lon, UT_ID$lat)
ut_st = paste(ut_st, ",", collapse="")
vt_st = stream_string(VT_ID$ID, VT_ID$name, VT_ID$url, VT_OBS$obs, VT_OBS$time, 
                      VT_ID$lon, VT_ID$lat)
vt_st = paste(vt_st, ",", collapse="")
va_st = stream_string(VA_ID$ID, VA_ID$name, VA_ID$url, VA_OBS$obs, VA_OBS$time, 
                      VA_ID$lon, VA_ID$lat)
va_st = paste(va_st, ",", collapse="")
wa_st = stream_string(WA_ID$ID, WA_ID$name, WA_ID$url, WA_OBS$obs, WA_OBS$time, 
                      WA_ID$lon, WA_ID$lat)
wa_st = paste(wa_st, ",", collapse="")
wv_st = stream_string(WV_ID$ID, WV_ID$name, WV_ID$url, WV_OBS$obs, WV_OBS$time, 
                      WV_ID$lon, WV_ID$lat)
wv_st = paste(wv_st, ",", collapse="")
wi_st = stream_string(WI_ID$ID, WI_ID$name, WI_ID$url, WI_OBS$obs, WI_OBS$time, 
                      WI_ID$lon, WI_ID$lat)
wi_st = paste(wi_st, ",", collapse="")
wy_st = stream_string(WY_ID$ID, WY_ID$name, WY_ID$url, WY_OBS$obs, WY_OBS$time, 
                      WY_ID$lon, WY_ID$lat)
wy_last = wy_st[length(wy_st)] #make sure the last feature doesn't end with a ","
wy_st = paste(wy_st[1:length(wy_st) - 1], ",", collapse="")

# -------------------------------------------------------------------------------------------------------------------- 
# Create geojson objects with the information.
# Merge geojson objects into a specific file format with data as the variable name.
json_merge = paste('Streams = {"type": "FeatureCollection","features": [', al_st, ak_st, az_st, ar_st, ca_st, co_st, ct_st, 
                   dc_st, de_st, fl_st, ga_st, hi_st, id_st, il_st, in_st, ia_st, ks_st, ky_st, la_st, me_st, md_st, ma_st, 
                   mi_st, mn_st, mi_st, mo_st, mt_st, ne_st, nv_st, nh_st, nj_st, nm_st, ny_st, nc_st, nd_st, oh_st, ok_st, 
                   or_st, pa_st, ri_st, sc_st, sd_st, tn_st, tx_st, ut_st, vt_st, va_st, wa_st, wv_st, wi_st, wy_st, wy_last, ']};', sep="")

# Export data to geojson.
cat(json_merge, file="stream_extend.js")

# --------------------------------------------------------------------------------------------------------------------

# # Export data to geojson.
# cat(json_merge, file="/home/staff/klr324/marisa.psu.edu/mapdata/weather_observations_v2.json")
# --------------------------------------------------------------------------------------------------------------------
