############################
## CREATE NLA DATASET COMBINED 2007 and 2012 observations
##  Have to align variables between survey years and give more general name if variable includes the survey year
##  ALL LAKES 2012 >=1 ha and RESAMPLED LAKES
##
##
## Data decisions with regards to combining 2007 and 2012 data
##  Issue: 30% of lakes are resampled and lead to non-independent observations
##  Tony Olsen suggested options to deal this this (see email 10/19/18)
##    See Analysis > Data exploration > Drivers_lake_hydro > data_creation_exploration_19OCT18.R for original script

##  Option 1: All NLA07 and only new NLA12 sites
##  9/27/19
## 1/6/2020 - updated datasets to include habitat and benthic macroinvert MMI
## 2/20/2020 - changed some lake origin classes based on additional research by Alan Herlihy
## 3/11/2020 - added lake hydrologic alteration potential ranking - see protcol and decision tree for details
##              Jump to line 795 for merging data
## 4/17/2020 - added observed over expected
## 4/21/2020 - updated observations where manually changed OUTLET_DAM_red2 and ELEV_use for WEST lakes
## 5/27/20 - Added Artifical Agricultural Drainage in updated LakeCat
## 6/5/20 - Full stage zmax (NLA zmax+VertDD) - this will represent the potential maximum lake depth, which may vary from what NLA field crews observe if the lake is drawndown ##
## 8/11/20 Added revized ZOE (Z2) drawdown scores - Phil calculated OE drawdown using reference natural lakes for all ecoregions - previously CENPL and WEST man-made lakes had their own reference drawdown
##
#############################

remove(list=ls())

library(dplyr)
library(ggplot2)

#############
## LOAD DATA
##  Processed NLA07 dataset Includes LakeCat variables n=1028 obs and 573 variables (phdi and lake connectivity)
##    Includes drawdown metrics standardized by regional reference expected drawdown
##
##  Processed NLA12 dataset lakes >= 4ha includes LakeCat &irrigated ag n = 951 obs w/563 vars
#############

## NLA 2007
nla07<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")
todrop<-names(nla07)%in%c("X") 
nla07<-nla07[!todrop]
names(nla07)

## NLA 2012 data (>= 1 ha lakes)
nla12<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_lkcat_wgt_26JUN19.csv")

## NLA 2012 data (>= 4 ha lakes)
#nla12<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv")
todrop<-names(nla12)%in%c("X") 
nla12<-nla12[!todrop]
names(nla12)

############
## FOR LAPTOP
## NLA 2007 n = 1028
nla07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

## NLA 2012 data (>= 1 ha lakes) n = 1038
nla12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_lkcat_wgt_26JUN19.csv")

## NLA 2012 data (>= 4 ha lakes)
#nla12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv")

#####################
## DATA PREP
##############
## REDUCE NUMBER OF VARIABLES TO KEEP IN DATASETS FOR MERGING
## MERGE DATA BY CREATING SAME COLUMNS AND THEN BINDING ROWS
## RENAME COLUMN NAMES TO MATCH BTW SURVEYS
###################
#####
## Rename columns in NLA 2007 to match NLA 2012 - "old" to "new"
names(nla07)[names(nla07)=="LAKEPERIM"]<-"PERIM_KM"
names(nla07)[names(nla07)=="Lake_Vol_m3"]<-"Volume_m3" # USE THIS LK VOLUME ESTIMATE
#names(nla07)[names(nla07)=="SLD"]<-"shorelineDevelopment"
names(nla07)[names(nla07)=="ALBERS_X"]<-"XCOORD"
names(nla07)[names(nla07)=="ALBERS_Y"]<-"YCOORD"
names(nla07)[names(nla07)=="ST"]<-"STATE"
names(nla07)[names(nla07)=="LAKENAME"]<-"NARS_NAME"
names(nla07)[names(nla07)=="SITE_TYPE"]<-"SITETYPE"
names(nla07)[names(nla07)=="WGT_NLA"]<-"WGT_ALL"

names(nla07)[names(nla07)=="BASINAREA_06_sqkm"]<-"BASINAreaSqKM"
names(nla07)[names(nla07)=="NLCD06_11_KM2_BSN"]<-"OPEN_km2"
names(nla07)[names(nla07)=="NLCD06_12_KM2_BSN"]<-"ICE_km2"
names(nla07)[names(nla07)=="NLCD06_21_KM2_BSN"]<-"DEVOPEN_km2"
names(nla07)[names(nla07)=="NLCD06_22_KM2_BSN"]<-"DEVLOW_km2"
names(nla07)[names(nla07)=="NLCD06_23_KM2_BSN"]<-"DEVMED_km2"
names(nla07)[names(nla07)=="NLCD06_24_KM2_BSN"]<-"DEVHIGH_km2"

names(nla07)[names(nla07)=="NLCD06_31_KM2_BSN"]<-"BARREN_km2"
names(nla07)[names(nla07)=="NLCD06_41_KM2_BSN"]<-"DECID_km2"
names(nla07)[names(nla07)=="NLCD06_42_KM2_BSN"]<-"CONIF_km2"
names(nla07)[names(nla07)=="NLCD06_43_KM2_BSN"]<-"MIXED_km2"
names(nla07)[names(nla07)=="NLCD06_52_KM2_BSN"]<-"SHRUBLAND_km2"
names(nla07)[names(nla07)=="NLCD06_71_KM2_BSN"]<-"GRASS_km2"

names(nla07)[names(nla07)=="NLCD06_81_KM2_BSN"]<-"PASTURE_km2"
names(nla07)[names(nla07)=="NLCD06_82_KM2_BSN"]<-"CROPS_km2"

names(nla07)[names(nla07)=="NLCD06_90_KM2_BSN"]<-"WDYWET_km2"
names(nla07)[names(nla07)=="NLCD06_95_KM2_BSN"]<-"EMHERBWET_km2"

names(nla07)[names(nla07)=="PCT06_FOREST_BSN"]<-"PCT_FOREST_BSN"
names(nla07)[names(nla07)=="PCT06_GRASS_BSN"]<-"PCT_GRASS_BSN"
names(nla07)[names(nla07)=="PCT06_WETLAND_BSN"]<-"PCT_WETLAND_BSN"
names(nla07)[names(nla07)=="PCT06_AGRIC_BSN"]<-"PCT_AGRIC_BSN"
names(nla07)[names(nla07)=="PCT06_DEVELOPED_BSN"]<-"PCT_DEVELOPED_BSN"

names(nla07)[names(nla07)=="PCT06_FOREST_BSN_logit"]<-"PCT_FOREST_BSN_logit"
names(nla07)[names(nla07)=="PCT06_GRASS_BSN_logit"]<-"PCT_GRASS_BSN_logit"
names(nla07)[names(nla07)=="PCT06_WETLAND_BSN_logit"]<-"PCT_WETLAND_BSN_logit"
names(nla07)[names(nla07)=="PCT06_AGRIC_BSN_logit"]<-"PCT_AGRIC_BSN_logit"
names(nla07)[names(nla07)=="PCT06_DEVELOPED_BSN_logit"]<-"PCT_DEVELOPED_BSN_logit"

names(nla07)[names(nla07)=="DOM_GEOL"]<-"DOMGEOL_BSN"
names(nla07)[names(nla07)=="GEOL_PT"]<-"SITE_GEOLOGY"
names(nla07)[names(nla07)=="Max_WSelev"]<-"ELEVMAX_BSN_m"
names(nla07)[names(nla07)=="Mean_WSelev"]<-"ELEVMEAN_BSN_m"

names(nla07)[names(nla07)=="phdi_nla07_WY_AVG"]<-"PHDI"
names(nla12)[names(nla12)=="phdi_nla12_WY_AVG"]<-"PHDI"

names(nla07)[names(nla07)=="RDisInEx1a"]<-"RDis_IX"

names(nla07)[names(nla07)=="PctIrrigated.AgLandWs_2007"]<-"PctIrrigated.AgLandWs"
names(nla07)[names(nla07)=="PctIrrigated.AgLandWs_2007_logit"]<-"PctIrrigated.AgLandWs_logit"
names(nla07)[names(nla07)=="PctIrrigated.AgLandCat_2007"]<-"PctIrrigated.AgLandCat"
names(nla07)[names(nla07)=="PctIrrigated.AgLandCat_2007_logit"]<-"PctIrrigated.AgLandCat_logit"


names(nla07)[names(nla07)=="PctDEVELOPED2006Cat"]<-"PctDEVELOPED_Cat"
names(nla07)[names(nla07)=="PctDEVELOPED2006Cat_logit"]<-"PctDEVELOPED_Cat_logit"
names(nla07)[names(nla07)=="PctAGR2006Cat"]<-"PctAGR_Cat"
names(nla07)[names(nla07)=="PctAGR2006Cat_logit"]<-"PctAGR_Cat_logit"


############
## CREATE VARIABLES 
# NLA 2012 PHYS HABITAT DERIVED VARS
nla12$AMFCFLTEMG <- nla12$AMFCFLOATING + nla12$AMFCEMERGENT

##############
### Reorder variables in 2007 and 2012 datasets so that the columns match up
## 2007 DATASET
myvars<- c("SITE_ID","VISIT_NO","SID","UID","YEAR","COMID","DATE_COL_iso","SAMPLE_ID","SAMPLED_PHAB",
           "dD","d18O","d_excess","E_I","RT_iso","Water_yield_m","Modeled_Water_yield_m",
           "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use","L_RT_iso",
           "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc","DDVrtDix_sc_MOD","L_DDVrtDix_sc_MOD",
           "VertDD_CONDc315","HorizDD_CONDc315","Drawdown_CONDus15",
           "LOE_VertDD_use","LOE_HorizDD_use","LOE_DDVrtDix_sc","LOE_DDHzSqrtA_sc",
           "ZLOE_VertDD_use","ZLOE_HorizDD_use","ZLOE_DDVrtDix_sc","ZLOE_DDHzSqrtA_sc",
           "LkArea_km2","L_LkAreakm2","DpthMx_use","L_DpthMx_use","sixDepth",
           "PERIM_KM","Volume_m3","SLD","ELEV_use","L_ELEV_use", #"LAKEPERIM","Lake_Vol","SLD"
           "LATdd_use","LONdd_use", "XCOORD","YCOORD", #"ALBERS_X","ALBERS_Y",
           "STATE","URBAN","Lake_Origin_use","HYDRO_TYPE_f","lk_hydro_iso",
           "NARS_NAME","ECOWSA9_2015","ECOREG_use",#"ST","LAKENAME"
           "WSA9_LO","ECOP5_2015","ECOP6_2015","RESAMPLED12","SIZE_CLASS","SITETYPE","TNT",#"SITE_TYPE",
           "WGT_ALL","RT_NLA12_2015", #"WGT_NLA",
           "NH4N_PPM","ANC","CA_PPM","CL_PPM","COLOR","COND","DOC",
           "MG_PPM","NO3N_PPM","NO3_NO2","NTL","PH_LAB","K_PPM",
           "PTL","SIO2","NA_PPM","SO4_PPM","TOC","TURB",
           "CHLA","SECMEAN","MCYST_TL_UGL",
           "L_NTL","L_PTL","L_CHLA",
           "amfcAll","amfcEmergent","amfcFloating","amfcSubmergent","amfcFltEmg",
           "bffFlat","bffGradual","bffSteep","bffVertical","bfoAngle",
           "bfxHorizDist","bfxVertHeight",
           "bsfcGravel","bsfcOrganic","bsfcSand","bsfcSilt","bsfcWood",
           "bsiSiteVariety","bsiStaVariety","bsvLdia","bsxLdia",
           "fcfcAquatic","fcfcBoulders","fcfcBrush","fcfcLedges","fcfcLiveTrees",
           "fcfcSnag","fcfcStructures",
           "hifpAny","hifpAnyCirca","hiiAll","hiiAg","hiiAgCirca",
           "hiiAllCirca","hiiNonAg","hiiNonAgCirca",
           "hipwBuildings","hipwCommercial","hipwCrops","hipwDocks","hipwLandfill",
           "hipwLawn","hipwOrchard","hipwPark","hipwPasture","hipwPowerlines",
           "hipwRoads","hipwWalls",
           "rvfcCanBig","rvfcCanSmall","rvfcGndBare","rvfcGndInundated",
           "rvfcGndNonw","rvfcGndWoody","rvfcUndNonw","rvfcUndWoody",
           "rviCanopy","rviCanUnd","rviUnderstory",
           "rviGround","rviHerbs","rviTallWood","rviTotalVeg","rviWoody",
           #"fciBig","fciRipVeg","rviLowWood","LitCvr_OE",
           "RDis_IX",#"RDisInEx1a",
           "ssfcBedrock","ssfcBoulders",
           "ssfcCobble","ssfcGravel","ssfcOrganic","ssfcOther","ssfcSand","ssfcSilt",
           "ssfcWood","ssiSiteVariety","ssiStaVariety",
           "ssvLdia","ssxLdia",
           "BASINAreaSqKM","OPEN_km2","ICE_km2",
           "DEVOPEN_km2","DEVLOW_km2","DEVMED_km2","DEVHIGH_km2",
           "BARREN_km2","DECID_km2","CONIF_km2","MIXED_km2",
           "SHRUBLAND_km2","GRASS_km2","PASTURE_km2","CROPS_km2",
           "WDYWET_km2","EMHERBWET_km2",
           "PCT_FOREST_BSN","PCT_GRASS_BSN","PCT_WETLAND_BSN","PCT_AGRIC_BSN","PCT_DEVELOPED_BSN",
           "PCT_FOREST_BSN_logit","PCT_GRASS_BSN_logit","PCT_WETLAND_BSN_logit","PCT_AGRIC_BSN_logit","PCT_DEVELOPED_BSN_logit",
           "Precip_PT","E","RH_PT","TMEAN_PT","P_WY",
           "Temp_degC_avg_yr","Precip_mm_avg_yr","Precip_mm_total_yr",
           "temp_degC_winter","temp_degC_spring","temp_degC_summer",
           "precip_mm_winter","precip_mm_spring","precip_mm_summer",
           "DpthMx_mod","L_DpthMx_mod","Zmax_source","TROPHIC_STATE",#"DOMGEOL_BSN","SITE_GEOLOGY",
           "ELEVMAX_BSN_m","ELEVMEAN_BSN_m","PHDI","POP_DEN",
           "OUTLET_DAMS","AGR_SCORE","IND_SCORE","MAN_SCORE","REC_SCORE","RES_SCORE",
           "WsAreaSqKm","CatAreaSqKm","inStreamCat", # LAKE CAT VARS
           "BFIWs","ElevWs","OmWs","PermWs","RckdepWs","WtDepWs",
           "AvgWetIndxWs","NABD_NrmStorWs","DamNrmStorWs","NABD_NIDStorWs","DamNIDStorWs",
           "RdDensWs",
           "Precip8110Ws","Tmean8110Ws","Tmax8110Ws","Tmin8110Ws",
           "PctIrrigated.AgLandWs","PctIrrigated.AgLandWs_logit","PctIrrigated.AgLandCat","PctIrrigated.AgLandCat_logit",
           "PctOw2006Cat","PctIce2006Cat","PctUrbOp2006Cat","PctUrbLo2006Cat","PctUrbMd2006Cat","PctUrbHi2006Cat","PctBl2006Cat","PctDecid2006Cat","PctConif2006Cat","PctMxFst2006Cat","PctShrb2006Cat","PctGrs2006Cat","PctHay2006Cat","PctCrop2006Cat","PctWdWet2006Cat","PctHbWet2006Cat", 
           "PctOw2006Ws","PctIce2006Ws","PctUrbOp2006Ws","PctUrbLo2006Ws","PctUrbMd2006Ws","PctUrbHi2006Ws","PctBl2006Ws","PctDecid2006Ws","PctConif2006Ws","PctMxFst2006Ws","PctShrb2006Ws","PctGrs2006Ws","PctHay2006Ws","PctCrop2006Ws","PctWdWet2006Ws","PctHbWet2006Ws",
           "PctOw2011Cat","PctIce2011Cat","PctUrbOp2011Cat","PctUrbLo2011Cat","PctUrbMd2011Cat","PctUrbHi2011Cat","PctBl2011Cat","PctDecid2011Cat","PctConif2011Cat","PctMxFst2011Cat","PctShrb2011Cat","PctGrs2011Cat","PctHay2011Cat","PctCrop2011Cat","PctWdWet2011Cat","PctHbWet2011Cat",
           "PctOw2011Ws","PctIce2011Ws","PctUrbOp2011Ws","PctUrbLo2011Ws","PctUrbMd2011Ws","PctUrbHi2011Ws","PctBl2011Ws","PctDecid2011Ws","PctConif2011Ws","PctMxFst2011Ws","PctShrb2011Ws","PctGrs2011Ws","PctHay2011Ws","PctCrop2011Ws","PctWdWet2011Ws","PctHbWet2011Ws",
           "PctDEVELOPED_Cat","PctAGR_Cat","PctDEVELOPED_Cat_logit","PctAGR_Cat_logit",
           "PctAgDrainageCat","PctAgDrainageWs","PctAgDrainageCat_logit","PctAgDrainageWs_logit",
           "WALA","L_WALA","pctGlac","bffFlat_grad","WGT_SP",
           "RVegQc15","LitCvrQc15","LitRipCvrQc15",
           "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
           "RVeg_CONDus15","LitCvr_CONDus15","LitRipCvr_CONDus15",
           "BENT_COND","MMI_BENT_NLA12","COMP_PT","DIVS_PT","FEED_PT",
           "HABT_PT","RICH_PT","TOLR_PT","TOTLNIND")
#"Class3_f","lake_type") # ADDING LAKE CONNECTIVITY TYPE (NOt available for 2012)

nla07_red <- nla07[myvars]


## NLA 2012 - Changed volume estimate to be Hollister's estimate
names(nla12)
#nla12$L_LKAREA_KM2_mod <-log10(nla12$LKAREA_KM2_mod)
# Changed variables (dropped "LkAreakm2","L_LkAreakm2", "LATdd_use", "LONdd_use") replaced with "LKAREA_KM2_mod","L_LKAREA_KM2_mod","LAT_DD83","LON_DD83",
# There are some variables in NLA 2012 which are not good (Lake area, Lake Origin)
myvars_12<- c("SITE_ID","VISIT_NO","SID","UID","YEAR","COMID","DATE_COL_iso","SAMPLE_ID","SAMPLED_PHAB",
              "dD","d18O","d_excess","E_I","RT_iso","Water_yield_m","Modeled_Water_yield_m",
              "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use","L_RT_iso",
              "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc","DDVrtDix_sc_MOD","L_DDVrtDix_sc_MOD",
              "VertDD_CONDc315","HorizDD_CONDc315","Drawdown_CONDus15",
              "LOE_VertDD_use","LOE_HorizDD_use","LOE_DDVrtDix_sc","LOE_DDHzSqrtA_sc",
              "ZLOE_VertDD_use","ZLOE_HorizDD_use","ZLOE_DDVrtDix_sc","ZLOE_DDHzSqrtA_sc",
              "LKAREA_KM2_mod","L_LKAREA_KM2_mod","DpthMx_use","L_DpthMx_use","SIXDEPTH","PERIM_KM",
              "Volume_Corrected_m3","SLD",
              "ELEV_use","L_ELEV_use","LAT_DD83","LON_DD83","XCOORD","YCOORD",
              "STATE","URBAN","LAKE_ORIGIN","HYDRO_TYPE_f","lk_hydro_iso", #"Lake_Origin_use"
              "NARS_NAME",
              "ECOWSA9_2015","ECOREG_use","WSA9_LO","ECOP5_2015","ECOP6_2015","RESAMPLED12","SIZE_CLASS","SITETYPE","TNT",
              "WGT_ALL","RT_NLA12_2015",
              "AMMONIA_N_RESULT_mgL","ANC_RESULT_ueqL",
              "CALCIUM_RESULT_mgL","CHLORIDE_RESULT_mgL","COLOR_RESULT_PtCo","COND_RESULT_uscm",
              "DOC_RESULT_mgL","MAGNESIUM_RESULT_mgL","NITRATE_N_RESULT_mgL","NITRATE_NITRITE_N_RESULT_mgL","NTL_RESULT_ugL", # NOTE - using the transformed value
              "PH_RESULT","POTASSIUM_RESULT_mgL","PTL_RESULT_ugL","SILICA_RESULT_mgL","SODIUM_RESULT_mgL","SULFATE_RESULT_mgL",
              "TOC_RESULT_mgL","TURB_RESULT_NTU",
              "CHLL_RESULT_ugL","SECCHI_m","MICL_RESULT_ugL",
              "L_NTL","L_PTL","L_CHLA",
              "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT","AMFCFLTEMG",
              "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE","BFXHORIZDIST","BFXVERTHEIGHT",
              "BSFCGRAVEL","BSFCORGANIC","BSFCSAND","BSFCSILT","BSFCWOOD",             
              "BSISITEVARIETY","BSISTAVARIETY","BSVLDIA","BSXLDIA",
              "FCFCAQUATIC_SIM","FCFCBOULDERS_SIM","FCFCBRUSH_SIM",
              "FCFCLEDGES_SIM","FCFCLIVETREES_SIM","FCFCSNAGS_SIM","FCFCSTRUCTURES_SIM",
              "HIFPANY_SYN","HIFPANYCIRCA_SYN","HIIALL_SYN","HIIAG_SYN","HIIAGCIRCA_SYN","HIIALLCIRCA_SYN","HIINONAG_SYN","HIINONAGCIRCA_SYN",
              "HIPWBUILDINGS_SYN","HIPWCOMMERCIAL_SYN","HIPWCROPS_SYN","HIPWDOCKS_SYN","HIPWLANDFILL_SYN","HIPWLAWN_SYN","HIPWORCHARD_SYN","HIPWPARK_SYN","HIPWPASTURE_SYN","HIPWPOWERLINES_SYN","HIPWROADS_SYN","HIPWWALLS_SYN",
              "RVFCCANBIG_SYN","RVFCCANSMALL_SYN","RVFCGNDBARE_SYN",
              "RVFCGNDINUNDATED_SYN","RVFCGNDNONW_SYN","RVFCGNDWOODY_SYN",
              "RVFCUNDNONW_SYN","RVFCUNDWOODY_SYN",
              "RVICANOPY_SYN","RVICANUND_SYN","RVIUNDERSTORY_SYN","RVIGROUND_SYN",
              "RVIHERBS_SYN","RVITALLWOOD_SYN","RVITOTALVEG_SYN",
              "RVIWOODY_SYN","RDis_IX",
              "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",          
              "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA",
              "BASINAreaSqKM","OPEN_11_km2","ICE_11_km2","DEVOPEN_11_km2","DEVLOW_11_km2","DEVMED_11_km2","DEVHIGH_11_km2",
              "BARREN_11_km2","DECID_11_km2","CONIF_11_km2","MIXED_11_km2","SHRUBLAND_11_km2","GRASS_11_km2",
              "PASTURE_11_km2","CROPS_11_km2","WDYWET_11_km2","EMHERBWET_11_km2",
              "PCT_FOREST11_BSN","PCT_GRASS11_BSN","PCT_WET11_BSN","PCT_AGR11_BSN","PCT_DEVELOPED11_BSN",
              "PCT_FOREST_BSN_logit","PCT_GRASS_BSN_logit","PCT_WET_BSN_logit","PCT_AGR_BSN_logit","PCT_DEVELOPED_BSN_logit",
              "Precip_PT","E_m","RH_PT","TMEAN_PT","P_WY_m",
              "Temp_degC_avg_yr","Precip_mm_avg_yr","Precip_mm_total_yr",
              "temp_degC_winter","temp_degC_spring","temp_degC_summer",
              "precip_mm_winter","precip_mm_spring","precip_mm_summer",
              "DpthMx_mod","L_DpthMx_mod","Zmax_source","TROPHIC_STATE",#"DOMGEOL_BSN","SITE_GEOLOGY",
              "ELEVMAX_BSN_m","ELEVMEAN_BSN_m","PHDI","POPDEN_BSN",
              "OUTLET_DAMS","AGR_SCORE","IND_SCORE","MAN_SCORE","REC_SCORE","RES_SCORE",
              "WsAreaSqKm","CatAreaSqKm","inStreamCat", # LAKE CAT VARS
              "BFIWs","ElevWs","OmWs","PermWs","RckdepWs","WtDepWs",
              "AvgWetIndxWs","NABD_NrmStorWs","DamNrmStorWs","NABD_NIDStorWs","DamNIDStorWs",
              "RdDensWs",
              "Precip8110Ws","Tmean8110Ws","Tmax8110Ws","Tmin8110Ws",
              "PctIrrigated.AgLandWs_2012","PctIrrigated.AgLandWs_2012_logit","PctIrrigated.AgLandCat_2012","PctIrrigated.AgLandCat_2012_logit",
              "PctOw2006Cat","PctIce2006Cat","PctUrbOp2006Cat","PctUrbLo2006Cat","PctUrbMd2006Cat","PctUrbHi2006Cat","PctBl2006Cat","PctDecid2006Cat","PctConif2006Cat","PctMxFst2006Cat","PctShrb2006Cat","PctGrs2006Cat","PctHay2006Cat","PctCrop2006Cat","PctWdWet2006Cat","PctHbWet2006Cat", 
              "PctOw2006Ws","PctIce2006Ws","PctUrbOp2006Ws","PctUrbLo2006Ws","PctUrbMd2006Ws","PctUrbHi2006Ws","PctBl2006Ws","PctDecid2006Ws","PctConif2006Ws","PctMxFst2006Ws","PctShrb2006Ws","PctGrs2006Ws","PctHay2006Ws","PctCrop2006Ws","PctWdWet2006Ws","PctHbWet2006Ws",
              "PctOw2011Cat","PctIce2011Cat","PctUrbOp2011Cat","PctUrbLo2011Cat","PctUrbMd2011Cat","PctUrbHi2011Cat","PctBl2011Cat","PctDecid2011Cat","PctConif2011Cat","PctMxFst2011Cat","PctShrb2011Cat","PctGrs2011Cat","PctHay2011Cat","PctCrop2011Cat","PctWdWet2011Cat","PctHbWet2011Cat",
              "PctOw2011Ws","PctIce2011Ws","PctUrbOp2011Ws","PctUrbLo2011Ws","PctUrbMd2011Ws","PctUrbHi2011Ws","PctBl2011Ws","PctDecid2011Ws","PctConif2011Ws","PctMxFst2011Ws","PctShrb2011Ws","PctGrs2011Ws","PctHay2011Ws","PctCrop2011Ws","PctWdWet2011Ws","PctHbWet2011Ws",
              "PctDEVELOPED2011Cat","PctAGR2011Cat","PctDEVELOPED2011Cat_logit","PctAGR2011Cat_logit", # Put in the 2011 Landuse to align with survey year
              "PctAgDrainageCat","PctAgDrainageWs","PctAgDrainageCat_logit","PctAgDrainageWs_logit",
              "WALA","L_WALA","pctGlac","bffFlat_grad","WGT_SP",
              "RVegQc15","LitCvrQc15","LitRipCvrQc15",
              "RVegQc3OE15","LitCvrQc3OE15","LitRipCvrQc3OE15",
              "RVeg_CONDus15","LitCvr_CONDus15","LitRipCvr_CONDus15",
              "BENT_COND","MMI_BENT_NLA12","COMP_PT","DIVS_PT","FEED_PT",
              "HABT_PT","RICH_PT","TOLR_PT","TOTLNIND")

nla12_red <- nla12[myvars_12]

# CHECK TO SEE THAT VARIABLES ARE LINED UP CORRECTLY
vars_07 <- names(nla07_red)
write.csv(vars_07,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/vars_07.csv")

vars_12 <- names(nla12_red)
write.csv(vars_12,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/vars_12.csv")

# FOR LAPTOP
write.csv(vars_07,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/vars_07.csv")
write.csv(vars_12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/vars_12.csv")

# Looks good

########
# Need to have variable column names completely match to perform rbind
########
var_names_07 <- names(nla07_red)

# RELABEL all of nla12_red column names to match nla07_red column names
names(nla12_red)<- var_names_07 
names(nla12_red)


################################
## CREATE SUBSETS OF LAKES THAT WERE NOT RESAMPLED - UNIQUE LAKES BTW 2007 and 2012
# RESAMPLED nla12
resamp_12<-nla12_red[nla12_red$SID %in% nla07_red$SID, c(1:345)]# n=350 1:5,28,39:40
resamp_12<-resamp_12[order(resamp_12$SID),]

# RESAMPLED nla07
resamp_07<-nla07_red[nla07_red$SID %in% nla12_red$SID, c(1:345)]# 350

# CREATE NEW COLUMN IDENTIFYING WHETHER RESAMPLED
resamp_12$RESAMPLED12_b<-"Y"
resamp_07$RESAMPLED12_b<-"Y"

# RESAMPLED classes don't match and lakes labeled as resampled here aren't found in the original dataset
#table(test$RESAMPLED12) # The subset of nla12 lakes that don't match with 2007 claims that there are 44 resampled lakes
# DRP  NO YES 
#   0 535  44

############
# CHECK TO SEE THAT RESAMPLED LAKES ARE THE SAME SID
#write.csv(resamp_07,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/nla07_resampled.csv")
#write.csv(resamp_12,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/nla12_resampled.csv")
## YES THEY MATCH

#########
## SUBSET OF UNIQUE NLA12 lakes n=688 lakes in 2012 lakes <=1ha
nla12_uniq<-nla12_red[!nla12_red$SID %in% nla07_red$SID,c(1:345)]#include all columns

# SUBSET OF UNIQUE NLA07 lakes n=678 lakes in 2007
nla07_uniq<-nla07_red[!nla07_red$SID %in% nla12_red$SID,c(1:345)]

# CREATE NEW COLUMN IDENTIFYING WHETHER RESAMPLED
nla12_uniq$RESAMPLED12_b<-"N"
nla07_uniq$RESAMPLED12_b<-"N"

#############################
## CREATE DATASETS FOR ANALYSES
##
## ALL LAKES NOT REMOVING RESAMPLED LAKES
# First rejoin NLA 2007 data with new column
nla07_full <- rbind(nla07_uniq, resamp_07)
# Then rejoin NLA 2012 (>=1ha) data with new column
nla12_full <- rbind(nla12_uniq, resamp_12)

# Now join both years together - FULL NLA DATASET WITH YEARS COMBINED n = 1979 obs (includes 350 resampled lakes each year)
nla_full <- rbind(nla07_full, nla12_full)
# ORDER BY SITE_ID
nla_full <-nla_full[order(nla_full$SITE_ID),]
tail(nla_full$SITE_ID)

#############
## WRITE FULL DATASET n = 2066 w/346 vars
write.csv(nla_full,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/a_Stata/Data/nla0712/NLA_FULL.csv")

# FOR LAPTOP
write.csv(nla_full,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/a_Stata/Data/nla0712/NLA_FULL.csv")

#######################
## READ CREATED DATASET 
nla_full <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/a_Stata/Data/nla0712/NLA_FULL.csv",stringsAsFactors = FALSE)

# FOR LAPTOP
nla_full <-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/a_Stata/Data/nla0712/NLA_FULL.csv",stringsAsFactors = FALSE)

todrop <- names(nla_full)%in% c("X")
nla_full <- nla_full[!todrop]

##########################################
##  PROCESSED AND DERIVED VARIABLES
##########################################

################
## Convert Lake area and Watershed area to m2
nla_full$LkArea_m2 <- nla_full$LkArea_km2*1000000
summary(nla_full$LkArea_m2)

nla_full$BASINAREA_m2 <- nla_full$BASINAreaSqKM* 1000000
summary(nla_full$BASINAREA_m2)


###################
## DERIVE VARIABLES
###################

## INTEGRATED LAKE SIZE METRIC ##  LAKE DEPTH (m)/sqrt LAKE AREA (m2)
nla_full$lk_size_mod <-nla_full$DpthMx_mod/sqrt(nla_full$LkArea_m2)
summary(nla_full$lk_size_mod)

##  INTEG LK SIZE Including DRAWDOWN
##  Depthmax(m)/(sqroot Lake area (m))
nla_full$litt_slope <-(nla_full$DpthMx_mod+nla_full$VertDD_use)/(sqrt(nla_full$LkArea_m2)+nla_full$HorizDD_use)

nla_full$L_litt_slope <- log10(nla_full$litt_slope)

## MODELED INFLOW (m) ##  derived from runoff, basin area, and lake area
nla_full$inflow <- (nla_full$Modeled_Water_yield_m *nla_full$BASINAREA_m2)/nla_full$LkArea_m2
summary(nla_full$inflow)

# INFLOW BASED ON ISOTOPE RUNOFF (m) - 
nla_full$inflow_iso <- (nla_full$Water_yield_m *nla_full$BASINAREA_m2)/nla_full$LkArea_m2
summary(nla_full$inflow_iso) # 

# CHANGE TO NA for 4 observation with negative inflow_iso
z_iso_inf <- nla_full%>%
  filter(inflow_iso<0)
# SID = NLA12_CA-174, NLA12_TX-129, NLA12_GA-101, NLA12_WA-141

nla_full$inflow_iso[nla_full$SID=="NLA12_CA-174"]<- NA
nla_full$inflow_iso[nla_full$SID=="NLA12_TX-129"]<- NA
nla_full$inflow_iso[nla_full$SID=="NLA12_WA-141"]<- NA
nla_full$inflow_iso[nla_full$SID=="NLA12_GA-101"]<- NA

# Plot inflow estimates against one another
plot(nla_full$inflow~nla_full$inflow_iso)
abline(lm(nla_full$inflow~nla_full$inflow_iso))
abline(0,1)
# isotopically derived inflow is smaller than modeled inflowbut maybe driven by outlier estimates

## LAKE EVAPORATION using E:I and Inflow from runoff - Renee's suggestion
#   This does not follow expected patterns with PHDI and VertDD - may be bc inflow has strong influence
nla_full$E_lk <- nla_full$inflow * nla_full$E_I
#sd(dat$E_lk, na.rm=TRUE) #45.80877
summary(nla_full$E_lk)

## LAKE EVAPORATION using E:I and Inflow from isotopically derived runoff - Renee's suggestion
nla_full$E_lk_iso <- nla_full$inflow_iso * nla_full$E_I
summary(nla_full$E_lk_iso)


##############
## CREATE DUMMY VARIABLES 

# FOR OUTLET_DAMS 
#   https://randyzwitch.com/creating-dummy-variables-data-frame-r/
#table(nla_full$OUTLET_DAMS)
#levels(nla_full$OUTLET_DAMS)
# [1] ""           "ARTIFICIAL" "NATURAL"    "NONE"  

# NAME LEVELS
#levels(nla_full$OUTLET_DAMS) <- list("NA"=c(""), "ARTIFICIAL"=c("ARTIFICIAL"),
#                                     "NATURAL"=c("NATURAL"),"NONE"=c("NONE"))
#nla_full$OUTLET_DAMS <-droplevels(nla_full$OUTLET_DAMS)

#for(level in unique(nla_full$OUTLET_DAMS)){
#  nla_full[paste("dummy", level, sep = "_")] <- ifelse(nla_full$OUTLET_DAMS == level, 1, 0)
#}
#names(nla_full)

# CONSOLIDATE CONTROL STRUCTURE CLASS (Artificial + Natural)
# Rename labels
#nla_full$OUTLET_DAMS_red<-nla_full$OUTLET_DAMS
#levels(nla_full$OUTLET_DAMS_red) <- list("NA"=c("NA"), "DAM"=c("ARTIFICIAL"),
#                                         "DAM"=c("NATURAL"),"NONE"=c("NONE"))
#nla_full$OUTLET_DAMS_red <-droplevels(nla_full$OUTLET_DAMS_red)
#table(nla_full$OUTLET_DAMS_red)
#   NA  DAM NONE 
#   50 1259  670  

# Dummy variable for reduced Outlet classes
#for(level in unique(nla_full$OUTLET_DAMS_red)){
#  nla_full[paste("dummy", level, sep = "_")] <- ifelse(nla_full$OUTLET_DAMS_red == level, 1, 0)
#}

## 

#####################
## Glaciation
nla_full$glac<- ifelse(nla_full$pctGlac>40, 1,0)
table(nla_full$glac)
# 0   1 
#1197  866 

#############
## INTERACTION TERM BETWEEN VERTICAL DD AND BANK MORPHOLOGY
nla_full$vertxbff <-nla_full$VertDD_use*nla_full$bffFlat_grad


## LAKE MORPHOMETRY VARIABLES FROM LIT
# RELATIVE DEPTH (50*zmax * sqrt (pi/LA))
nla_full$lk_size_mod2<-50*(nla_full$DpthMx_mod)*(sqrt(pi/nla_full$LkArea_m2))
nla_full$L_lk_size_mod2 <- log10(nla_full$lk_size_mod2)
summary(nla_full$lk_size_mod2)

## LAKE VOLUME - Change lake volume = 0 to NA - 7 lakes: SID=="NLA06608-0444",
#       SID="NLA12_MS-106","NLA12_MS-113","NLA12_SD-183","NLA12_WI-151"
z_vol <-nla_full%>%
  filter(Volume_m3==0)
#summary(nla_full$Volume_m3)

nla_full$Volume_m3[nla_full$SID=="NLA06608-0444"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA06608-1989"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA06608-0243"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA12_MS-106"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA12_MS-113"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA12_MS-125"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA12_SD-183"]<- NA
nla_full$Volume_m3[nla_full$SID=="NLA12_WI-151"]<- NA

summary(nla_full$Volume_m3)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#6.669e+03 2.005e+05 7.667e+05 5.614e+07 4.804e+06 2.176e+10        11 


# INDEX OF BASIN PERMANENCE (IBP) - from Kerekes 1977 "Littoral effect on basin volume"
#   Had to multiply by 1,000,000 to get values similar to other papers 
#     Some studies state IBP <0.1 have excessive shallowness and IBP=0.2 more permanent
#     Alcocer et al. 2016 bathymetry of lakes in Chiapas range from ~0.x to 7
nla_full$ibp <- nla_full$Volume_m3/(nla_full$PERIM_KM*1000000)# Seems like need to convert lake perimeter (km) to something else to get in reasonable range
summary(nla_full$ibp)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#  0.00515   0.07706   0.18227   0.70741   0.48002 129.86679        11   
nla_full$L_ibp <- log10(nla_full$ibp)
hist(nla_full$L_ibp)
summary(nla_full$L_ibp) # sd = 0.5858026

## LAKE AREA to VOLUME RATIO # There are some lakes with zero volume
nla_full$LA_LV <-nla_full$LkArea_m2/(nla_full$Volume_m3)
nla_full$L_LA_LV<-log10(nla_full$LA_LV)

##################
## RESERVOIR STORAGE CAPACITY in LAKECAT
#   Note Damxxx is different from NABD (National anthro barrier dataset)
#     More zeros in NABD (I think)
#     Asked Alan H. about different between NID and NABD 8/16/19
#     He felt that we have more confidence in NABD even tho there are more zeros (it went through QAQC to some degree)
###################
## RESERVOIR CAPACITY VARIABLES IN LAKECAT
##  Going to use NABD Normal, 30yr cumulative precipitation, and LakeCat watershed area
#     based on Alan Herlihy's code

# INFLOW by PRECIPITATION km3
# Convert cumulative precipitation from mm to km3 in watershed
nla_full$Precip8110Ws_km3 <-nla_full$Precip8110Ws/1000000 *nla_full$WsAreaSqKm
summary(nla_full$Precip8110Ws_km3)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 0.00001   0.00350   0.01598   1.92600   0.09837 268.40000         3  

# STORAGE km3
# Convert to reservoir volume in km3 in the watershed - use LakeCat Watershed area
nla_full$NABD_NrmStorWs_km3 <-nla_full$NABD_NrmStorWs*nla_full$WsAreaSqKm/1000000000
summary(nla_full$NABD_NrmStorWs_km3)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#   0.0000  0.0000  0.0000  0.3027  0.0012 84.0481       3 

## RESERVOIR CAPACITY INDEX related to PRECIPITATION 
#     Scale reservoir storage as proportion of cumulative precipitation in watershed
#   30yr avg Cumulative precipitation during year (convert from mm to m)
# Index of what proportion of annual water flow could be dampened or moderated by reservoir storage
nla_full$NABD_NrmStorWs_index <- nla_full$NABD_NrmStorWs_km3/nla_full$Precip8110Ws_km3 *100

summary(nla_full$NABD_NrmStorWs_index) # Should be %
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#     0.00      0.00      0.00     62.82      2.22 109916.71         3 

#z<-nla_full%>%filter(NABD_NrmStorWs_index>1000)
# These are lakes in the XER region with little precip and lots of modified hydrology on the landscape
# NLA06608-1295 is Mims Lake in New Mexico that is in same watershed as a reservoir fo the Rio Grande
# NLA06608-2345 is Huntington North Reservoir in UT - mountainous looking

##########
## DAM (Nat In D) - Just in case interested in other variable
nla_full$DamNrmStorWs_km3<- nla_full$DamNrmStorWs*nla_full$WsAreaSqKm/1000000000

nla_full$DamNrmStorWs_km3_index <- nla_full$DamNrmStorWs_km3/nla_full$Precip8110Ws_km3*100
summary(nla_full$DamNrmStorWs_km3_index)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#    0.000    0.000    0.000   15.039    5.584 1406.842        3 


######################
## CALCULATE WATERSHED TOPOGRAPHIC RELIEF
##  ELEV Max in watershed (m) - ELEV Lake (m)
##   Max_WSelev (in Isotope spsheet) - ELEV_use

## FIRST MANUALLY enter elevation for lakes missing lake elevation n = 20
summary(nla_full$ELEV_use)# n = 20 missing lake elevation
# SID= "NLA12_AL-101" "NLA12_AL-103" "NLA12_AL-105" "NLA12_FL-101" "NLA12_FL-102" "NLA12_FL-103" "NLA12_FL-123"
# "NLA12_MS-106" "NLA12_ND-105" "NLA12_NV-104" "NLA12_SC-104" "NLA12_SC-112" "NLA12_SC-118" "NLA12_TN-101", "NLA12_TN-108",
# "NLA12_TX-105", "NLA12_TX-110", "NLA12_VT-103" "NLA12_WY-124" "NLA12_WY-133"

nla_full$ELEV_use[nla_full$SID=="NLA12_AL-101"]<- 57.03
nla_full$ELEV_use[nla_full$SID=="NLA12_AL-103"]<- 141.76
nla_full$ELEV_use[nla_full$SID=="NLA12_AL-105"]<- 50.83
nla_full$ELEV_use[nla_full$SID=="NLA12_FL-101"]<- 4.52
nla_full$ELEV_use[nla_full$SID=="NLA12_FL-102"]<- 38.73
nla_full$ELEV_use[nla_full$SID=="NLA12_FL-103"]<- 19.85
nla_full$ELEV_use[nla_full$SID=="NLA12_FL-123"]<- 33.47
nla_full$ELEV_use[nla_full$SID=="NLA12_MS-106"]<- 82.01
nla_full$ELEV_use[nla_full$SID=="NLA12_ND-105"]<- 434.35
nla_full$ELEV_use[nla_full$SID=="NLA12_NV-104"]<- 1261
nla_full$ELEV_use[nla_full$SID=="NLA12_SC-104"]<- 162.83
nla_full$ELEV_use[nla_full$SID=="NLA12_SC-112"]<- 100.53
nla_full$ELEV_use[nla_full$SID=="NLA12_SC-118"]<- 23.16
nla_full$ELEV_use[nla_full$SID=="NLA12_TN-101"]<- 266.37
nla_full$ELEV_use[nla_full$SID=="NLA12_TN-108"]<- 192.02
nla_full$ELEV_use[nla_full$SID=="NLA12_TX-105"]<- 105.13
nla_full$ELEV_use[nla_full$SID=="NLA12_TX-110"]<- 156.97
nla_full$ELEV_use[nla_full$SID=="NLA12_VT-103"]<- 28.85
nla_full$ELEV_use[nla_full$SID=="NLA12_WY-124"]<- 2065.39
nla_full$ELEV_use[nla_full$SID=="NLA12_WY-133"]<- 3249.12

summary(nla_full$ELEV_use)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-53.27  189.50  332.24  630.49  707.89 3594.97 

# TOPOGRAPHIC RELIEF
nla_full$elev_relief_m<- nla_full$ELEVMAX_BSN_m-nla_full$ELEV_use
summary(nla_full$elev_relief_m)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-56.98   35.47   80.92  326.57  309.44 4174.32 
sd(nla_full$elev_relief_m,na.rm=T) #608.3221

hist(nla_full$elev_relief_m) # need to transform

# TRANSFORM (log10)
nla_full$L_elev_relief <- log10(nla_full$elev_relief_m) # NOTE- four (+ one resampled lake) obs with negative difference and get NA (NLA06608-0377, NLA06608-1114, NLA06608-1227,NLA12_MI-138) These lakes look like not connected to surface inflows, oxbow lake, or man-made lake perched on landscape
summary(nla_full$L_elev_relief)


############################
## RECALCULATE WALA USING MORE COMPLETE LAKE AREA FOR 2012 Lakes
# WA:LA
nla_full$WALA <-nla_full$BASINAreaSqKM/nla_full$LkArea_km2
summary(nla_full$WALA)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#0.12     9.96    30.79   303.57    95.35 52105.52        1 

# Add Log transformed WA:LA
nla_full$L_WALA<- log10(nla_full$WALA)


##########################
## TRANSFORM VARIABLES
##########################

###########
## VARIABLES with zeros that need to be log transformed
###########
# Emailed Phil and Renee - suggested to take lowest non-zero value and use that integer
# E:I log10 - need to add small constant to values to remove zeros
# E_iso log 10 - smallest non-zero = 0.0006
# RdDensity = smallest non-zero = 0.0001

############
## LOG 10 transformations
nla_full$L_EI <- log10(nla_full$E_I+0.01)
nla_full$L_runoff <- log10(nla_full$Modeled_Water_yield_m)
nla_full$L_E_lk <- log10(nla_full$E_lk + 0.0001)

nla_full$L_lk_size_mod <-log10(nla_full$lk_size_mod)
nla_full$L_inflow <- log10(nla_full$inflow)
nla_full$L_SLD <- log10(nla_full$SLD)

nla_full$L_basinarea <-log10(nla_full$BASINAreaSqKM)
nla_full$L_hiiAll <- log10(nla_full$hiiAll +0.01) # there are 107 obs with zeros
nla_full$L_hiiNonAg <-log10(nla_full$hiiNonAg +0.01) #there are 119 obs with zero values
nla_full$L_hiiAg <- log10(nla_full$hiiAg+0.01) #there are 669 obs with zero values
nla_full$L_POP_DEN <-log10(nla_full$POP_DEN)
nla_full$L_rddens <-log10(nla_full$RdDensWs +0.0001)
#nla_full$L_DamDensWs<-log10(nla_full$DamDensWs+0.0001)
nla_full$L_DamNIDStorWs <-log10(nla_full$DamNIDStorWs+1)
nla_full$L_NABD_NIDStorWs <-log10(nla_full$NABD_NIDStorWs+1)
nla_full$L_DamNrmStorWs <-log10(nla_full$DamNrmStorWs+1)
nla_full$L_NABD_NrmStorWs <-log10(nla_full$NABD_NrmStorWs+1)

nla_full$L_NABD_NrmStorWs_index<- log10(nla_full$NABD_NrmStorWs_index+0.0001)
nla_full$L_DamNrmStorWs_km3_index <- log10(nla_full$DamNrmStorWs_km3_index+0.001)

nla_full$L_Precip_PT<-log10(nla_full$Precip_PT)
nla_full$L_Precip8110Ws <- log10(nla_full$Precip8110Ws)
nla_full$L_Precip_mm_total_yr<-log10(nla_full$Precip_mm_total_yr)
nla_full$L_precip_mm_winter<-log10(nla_full$precip_mm_winter) # 2007 Dec-Feb

# Additional chem
nla_full$L_SECMEAN <- log10(nla_full$SECMEAN+0.01) # min non-zero = 0.04
nla_full$L_MCYST_TL_UGL <- log10(nla_full$MCYST_TL_UGL+0.001) # No zeros
#nla07_mod$L_CYANDENS <- log10(nla07_mod$CYANDENS+0.10) # min non-zero =0.66 #/cm2

# Nearshore habitat - some seem skewed, others are okay
#nla_full$L_fciBig<- log10(nla_full$fciBig+0.001) # min, non-zero = 0.005
#nla_full$L_fciRipVeg<-log10(nla_full$fciRipVeg+0.001)
#nla_full$L_rviLowWood<-log10(nla_full$rviLowWood+0.001)
#nla_full$L_LitCvr_OE<-log10(nla_full$LitCvr_OE+0.001) 


###############
## LAKE VOLUME - log is being treated as factor - run equation again
#   one lake has zero value
nla_full$L_VOL <-log10(nla_full$Volume_m3) # 

# Additional variables
nla_full$L_RES_SCORE <- log10(nla_full$RES_SCORE+0.001)
nla_full$L_REC_SCORE <- log10(nla_full$REC_SCORE+0.001)
nla_full$L_AGR_SCORE <- log10(nla_full$AGR_SCORE+0.001)
nla_full$L_IND_SCORE <- log10(nla_full$IND_SCORE+0.001)
nla_full$L_MAN_SCORE <- log10(nla_full$MAN_SCORE+0.001)

nla_full$L_vertxbff <- log10(nla_full$vertxbff+0.001) #interaction verticalDD*bank steepness

################
## Logit transformations for proportions
library(car)
# Convert % to proportions
nla_full$OmWs_p <-nla_full$OmWs/100
#dat$PermWs_p <- dat$PermWs/100
# Don't need to recode to 0 and 1 b/c range is within these values 
#dat$PCT_FOREST_BSN.n <- car::recode(dat$PCT06_FOREST_BSN, "0=0.0001; 1=0.999")  

# Logit transformation for % Organic Matter in soil
nla_full$OmWs_logit<-logit(nla_full$OmWs_p)

# Permeability of soils (cm/hr) - log10
nla_full$PermWs_L<-log10(nla_full$PermWs)

##########
## SQRT Transformation for Left Skewed STATSGO variables
# http://abacus.bates.edu/~ganderso/biology/bio270/homework_files/nla07_moda_Transformation.pdf
#  Sqrt(k-obs) where k is a constant(usually k=max obs value +1)

# Depth to bedrock
max(nla_full$RckdepWs,na.rm=T)# 153.1969
nla_full$RckdepWs_tran <- sqrt(154-nla_full$RckdepWs)
hist(nla_full$RckdepWs_tran)

# Depth to water table
max(nla_full$WtDepWs,na.rm=T) #183.0475
nla_full$WtDepWs_tran <- sqrt(184-nla_full$WtDepWs)
hist(nla_full$WtDepWs_tran)


## PHAB transformations
nla_full$RVegQc15.n <- car::recode(nla_full$RVegQc15, "0=0.0001; 1=0.999")  
nla_full$LitCvrQc15.n <- car::recode(nla_full$LitCvrQc15, "0=0.0001; 1=0.999") 
nla_full$LitRipCvrQc15.n <- car::recode(nla_full$LitRipCvrQc15, "0=0.0001; 1=0.999") 

nla_full$amfcEmergent.n <- car::recode(nla_full$amfcEmergent, "0=0.0001; 1=0.999") 
nla_full$amfcFloating.n <- car::recode(nla_full$amfcFloating, "0=0.0001; 1=0.999") 
nla_full$amfcSubmergent.n <- car::recode(nla_full$amfcSubmergent, "0=0.0001; 1=0.999") 
nla_full$amfcFltEmg.n <- car::recode(nla_full$amfcFltEmg, "0=0.0001; 1=0.999") 

nla_full$fcfcAquatic.n <- car::recode(nla_full$fcfcAquatic, "0=0.0001; 1=0.999") 
nla_full$fcfcBoulders.n <- car::recode(nla_full$fcfcBoulders, "0=0.0001; 1=0.999")
nla_full$fcfcBrush.n <- car::recode(nla_full$fcfcBrush, "0=0.0001; 1=0.999")
nla_full$fcfcLedges.n <- car::recode(nla_full$fcfcLedges, "0=0.0001; 1=0.999")
nla_full$fcfcLiveTrees.n <- car::recode(nla_full$fcfcLiveTrees, "0=0.0001; 1=0.999")
#nla_full$fcfcOverhang.n <- car::recode(nla_full$fcfcOverhang, "0=0.0001; 1=0.999")
nla_full$fcfcSnag.n <- car::recode(nla_full$fcfcSnag, "0=0.0001; 1=0.999")
nla_full$fcfcStructures.n <- car::recode(nla_full$fcfcStructures, "0=0.0001; 1=0.999")

nla_full$rvfcCanBig.n <- car::recode(nla_full$rvfcCanBig, "0=0.0001; 1=0.999")
nla_full$rvfcCanSmall.n <- car::recode(nla_full$rvfcCanSmall, "0=0.0001; 1=0.999")
nla_full$rvfcUndWoody.n <- car::recode(nla_full$rvfcUndWoody, "0=0.0001; 1=0.999")
nla_full$rvfcGndWoody.n <- car::recode(nla_full$rvfcGndWoody, "0=0.0001; 1=0.999")
nla_full$rvfcGndInundated.n <- car::recode(nla_full$rvfcGndInundated, "0=0.0001; 1=0.999")

nla_full$ssfcBedrock.n <- car::recode(nla_full$ssfcBedrock, "0=0.0001; 1=0.999")
nla_full$ssfcBoulders.n <- car::recode(nla_full$ssfcBoulders, "0=0.0001; 1=0.999")
nla_full$ssfcCobble.n <- car::recode(nla_full$ssfcCobble, "0=0.0001; 1=0.999")
nla_full$ssfcGravel.n <- car::recode(nla_full$ssfcGravel, "0=0.0001; 1=0.999")
nla_full$ssfcSand.n <- car::recode(nla_full$ssfcSand, "0=0.0001; 1=0.999")
nla_full$ssfcSilt.n <- car::recode(nla_full$ssfcSilt, "0=0.0001; 1=0.999")
nla_full$ssfcWood.n <- car::recode(nla_full$ssfcWood, "0=0.0001; 1=0.999")

# LOGIT Transformations
nla_full$RVegQc15_logit <- logit(nla_full$RVegQc15.n)
nla_full$LitCvrQc15_logit <- logit(nla_full$LitCvrQc15.n)
nla_full$LitRipCvrQc15_logit <- logit(nla_full$LitRipCvrQc15.n)

nla_full$amfcEmergent_logit <- logit(nla_full$amfcEmergent.n)
nla_full$amfcFloating_logit <- logit(nla_full$amfcFloating.n)
nla_full$amfcSubmergent_logit <- logit(nla_full$amfcSubmergent.n)
nla_full$amfcFltEmg_logit <- logit(nla_full$amfcFltEmg.n)

nla_full$fcfcAquatic_logit <- logit(nla_full$fcfcAquatic.n)
nla_full$fcfcBoulders_logit <- logit(nla_full$fcfcBoulders.n)
nla_full$fcfcBrush_logit <- logit(nla_full$fcfcBrush.n)
nla_full$fcfcLedges_logit <- logit(nla_full$fcfcLedges.n)
nla_full$fcfcLiveTrees_logit <- logit(nla_full$fcfcLiveTrees.n)
#nla_full$fcfcOverhang_logit <- logit(nla_full$fcfcOverhang.n)
nla_full$fcfcSnag_logit <- logit(nla_full$fcfcSnag.n)
nla_full$fcfcStructures_logit <- logit(nla_full$fcfcStructures.n)

nla_full$rvfcCanBig_logit <- logit(nla_full$rvfcCanBig.n)
nla_full$rvfcCanSmall_logit <- logit(nla_full$rvfcCanSmall.n)
nla_full$rvfcUndWoody_logit <- logit(nla_full$rvfcUndWoody.n)
nla_full$rvfcGndWoody_logit <- logit(nla_full$rvfcGndWoody.n)
nla_full$rvfcGndInundated_logit <- logit(nla_full$rvfcGndInundated.n)

nla_full$ssfcBedrock_logit <- logit(nla_full$ssfcBedrock.n)
nla_full$ssfcBoulders_logit <- logit(nla_full$ssfcBoulders.n)
nla_full$ssfcCobble_logit <- logit(nla_full$ssfcCobble.n)
nla_full$ssfcGravel_logit <- logit(nla_full$ssfcGravel.n)
nla_full$ssfcSand_logit <- logit(nla_full$ssfcSand.n)
nla_full$ssfcSilt_logit <- logit(nla_full$ssfcSilt.n)
nla_full$ssfcWood_logit <- logit(nla_full$ssfcWood.n)

##########
## Drop variables used for transformations
##########
myvars_drop <- names(nla_full)%in% c("RVegQc15.n", "LitCvrQc15.n","LitRipCvrQc15.n",
                                     "amfcEmergent.n","amfcFloating.n","amfcSubmergent.n","amfcFltEmg.n",
                                     "fcfcAquatic.n","fcfcBoulders.n","fcfcBrush.n","fcfcLedges.n","fcfcLiveTrees.n","fcfcOverhang.n","fcfcSnag.n","fcfcStructures.n",
                                     "rvfcCanBig.n","rvfcCanSmall.n","rvfcUndWoody.n","rvfcGndWoody.n","rvfcGndInundated.n",
                                     "ssfcBedrock.n","ssfcBoulders.n","ssfcCobble.n","ssfcGravel.n","ssfcSand.n","ssfcSilt.n","ssfcWood.n")

nla_full<- nla_full[!myvars_drop] # 


##################################
## CREATE NEW LAKE ORIGIN VARIABLE 
##  Alan Herlihy checked lake origin type designation by hand for WEST lakes
##  I also found some lakes that looked like they were misclassified
##  Found some lakes that should be changed to a different lake origin type
## 2/20/20
##################################
names(nla_full)

# CREATE A NEW COLUMN
nla_full$Lake_Origin_mod <-nla_full$Lake_Origin_use
table(nla_full$Lake_Origin_mod)# Looks good
#MAN_MADE  NATURAL 
#    1175      891 
table(nla_full$Lake_Origin_use)

## CHANGE SPECIFIC LAKE TYPES FROM MAN_MADE TO NATURAL
# Soda Lake(NV) NLA06608-0073
# Ashurst(AZ) NLA06608-1641 
# Topaz(NV) NLA06608-2185 
# Walker(NV) NLA06608-2889
# Waha Lake(ID) NLA12_ID-148
# Caples Lake (CA) or Twin Lakes Reservoir NLA12_CA-135

## CHANGE FROM NATURAL TO MAN_MADE
# Luce Reservoir(WY) NLA12_WY-137

nla_full$Lake_Origin_mod[nla_full$SID=="NLA06608-0073"]<- "NATURAL" # Soda
nla_full$Lake_Origin_mod[nla_full$SID=="NLA06608-1641"]<- "NATURAL" # Ashurst
nla_full$Lake_Origin_mod[nla_full$SID=="NLA06608-2185"]<- "NATURAL" # Topaz
nla_full$Lake_Origin_mod[nla_full$SID=="NLA06608-2889"]<- "NATURAL" # Walker
nla_full$Lake_Origin_mod[nla_full$SID=="NLA12_ID-148"]<- "NATURAL" # Waha Lake
nla_full$Lake_Origin_mod[nla_full$SID=="NLA12_CA-135"]<- "NATURAL" # Caples Lake
nla_full$Lake_Origin_mod[nla_full$SID=="NLA12_WY-137"]<- "MAN_MADE" # Luce Reservoir


##################################################
## DAM DATA PROCESSING - All of 48 USA
# ORIGINAL NABD dataset (n = 2247 obs) that imported as .dbf and exported as .csv
# 3/14/20 - Marc linked additional Dam and NABD data to NLA sites (deposited in public L:drive)
##            Can load .dbf file directly in R and output as .csv file rather than going through ArcMap
nabd_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_org.csv",stringsAsFactors = FALSE)

# FOR LAPTOP
nabd_all<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_org.csv",stringsAsFactors = FALSE)
#z<-nabd_all%>%filter(SID=="NLA06608-1261")%>% # This lake is listed as not having NID_ht but I had it in an earlier version
 # select(SID,YEAR,NID_height)

## MULTIPLE DAMS PER LAKE within the same year
length(unique(nabd_all$SITE_ID)) # 2066 out of 2247
# List of lakes with more than one dam observation - includes duplicates
z<-nabd_all[duplicated(nabd_all$SITE_ID),] # 181 observations

# observations with more than one dam observation per lake n = 294 observations (113 unique SITE_ID -some lakes are resampled between years tho)
mult_all<-nabd_all[nabd_all$SITE_ID %in% nabd_all$SITE_ID[duplicated(nabd_all[,c(1)])],] # based on SITE_ID
# Does same thing
#mult2 <-nabd[nabd$SITE_ID %in% z$SITE_ID,]

length(unique(mult_all$SITE_ID)) # 113 lakes

# WRITE DATASET WITH MULTIPLE DAM OBSERVATIONS TO HANDPICK AND CONSOLIDATE
write.csv(mult_all, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/LAKE_ORIGIN_ALL_multiple_dams.csv")

################################
## CLEAN UP OBSERVATIONS MANUALLY WITH MULTIPLE DAMS ON LAKE - "LAKE_ORIGIN_ALL_multiple_dams.xlsx"
##  I selected the largest dam on a lake and retained that information for each SITE_ID for each year 
##    and deleted additional dams or duplicate records
##    I noted that there were multiple dams on a lake when there were more than one NABD record
##    Exported trimmed dataset as "NABD_ALL_multi_dam_clean.csv" 

## READ MODIFIED MULTIPLE OBS DATASET (n = 113 obs)
clean_all<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_multi_dam_clean.csv",stringsAsFactors = FALSE)
names(clean_all)

# FOR LAPTOP
clean_all<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_multi_dam_clean.csv",stringsAsFactors = FALSE)
table(clean_all$ECOP5_2015)
#APPS CENPL   CPL   UMW  WEST 
#40    30    15     3    25 

###########
## SELECT DATA WITHOUT MULTIPLE DAM OBSERVATIONS (n = 1953 obs)
## Will merge this with the cleaned up multi-dam dataset
single_all<-nabd_all[!nabd_all$SITE_ID %in% nabd_all$SITE_ID[duplicated(nabd_all[,c(1)])],] # based on SITE_ID
# check
length(unique(single_all$SID))# 1626
length(unique(single_all$SITE_ID)) #1953
single_all$multi_dam<-"n"
table(single_all$multi_dam)

# Rearrange variables to match data to merge with
myvars<-c("SITE_ID","VISIT_NO","SID","UID","COMID","STATE","ECOP5_2015",
          "ECOREG_use","multi_dam","YEAR","LATdd_use","LONdd_use","XCOORD","YCOORD",
          "Dam_length","Dam_name","Dam_Name2","Dam_type","Dam_height","NIDStorM3","NID_height",
          "NrmStorM3","Condition","Owner_type","Purposes")

single_all_cl<-single_all[myvars]

# WRITE DATASET WITH SINGLE OBSERVATIONS - Add columns to make similar to the modified duplicated dataset
#write.csv(single_all, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_SINGLE_dam.csv")

## MANUALLY ADD COLUMN to make this dataset similar to the multi dam

## READ SINGLE DATASET after adding column for multi_dam between ECOREG_use and YEAR
#sing_cl_all <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_SINGLE_dam.csv")

##################
## COMBINE DAM DATASETS
nabd_all_b<-rbind(single_all_cl,clean_all) # n = 2066 obs (single=1953; multi after processing=113)

# Replace zeros with NA for observations missing dam numerical attribute data [15,19:22]
names(nabd_all_b)
summary(nabd_all_b$Dam_length)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0     420     900    1863    1688   40940    1095 

nabd_all_b[, c(15,19:22)][nabd_all_b[, c(15,19:22)] == 0] <- NA # dam length, ht, storage etc
summary(nabd_all_b$Dam_length)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.0   486.5   960.0  1957.0  1800.0 40940.0    1142  


# FOR LAPTOP
write.csv(nabd_all_b,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NABD_ALL_combined_single_multi_042420.csv")


#################
# REDUCE VARIABLES SO CAN MERGE WITH NLA FULL DATASET
myvars<-c("YEAR","SID","multi_dam", #"SITE_ID",
          "Dam_length","Dam_name","Dam_Name2","Dam_type","Dam_height","NIDStorM3","NID_height",
          "NrmStorM3","Condition","Owner_type","Purposes")

nabd_all_c<-nabd_all_b[myvars]


#########################
## PROCESS DATA for WEST LAKES 
#   There were five lakes that were missing NID attributes from Marc's US export that had attributes in the individual wmt and xer exports
#   Need to fill in the NAs with that information

## LOAD DATA of 5 WEST LAKES w/NID attributes - make sure to keep character strings as characters - not factors
#   Selected WEST lakes identified in NABD_check_042420.R with dam attribute info
w_s<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/NABD_WEST_five_tofill.csv",stringsAsFactors = FALSE)

############
## FILL IN VALUES FOR MISSING OBSERVATIONS FROM ANOTHER DATAFRAME
# R RESOURCE: https://stackoverflow.com/questions/34697032/fill-in-missing-values-nas-with-values-from-another-dataframe-in-r
# SUBSETTING: https://adv-r.hadley.nz/subsetting.html - best to avoid using which()
summary(nabd_all_c$NID_height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 5.00   20.00   40.00   56.81   70.00  525.00    1097 
table(nabd_all_c$Owner_type)
# F   L   P  PL  PS   S   U 
#146 246 360   5   9 153  48 
table(nabd_all_c$Purposes)

nabd_all_c$NID_height[is.na(nabd_all_c$NID_height)] <- w_s$NID_height[match(nabd_all_c$SID,w_s$SID)] [is.na(nabd_all_c$NID_height)]#[match(z_s$SITE_ID,w_s$SITE_ID)]#[which(is.na(z_s$NID_height))]
nabd_all_c$Dam_length[is.na(nabd_all_c$Dam_length)] = w_s$Dam_length [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Dam_length)]#[match(z_s$SITE_ID,w_s$SITE_ID)]#[which(is.na(z_s$NID_height))]
nabd_all_c$Dam_name[is.na(nabd_all_c$Dam_name)] = w_s$Dam_name [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Dam_name)]
nabd_all_c$Dam_Name2[is.na(nabd_all_c$Dam_Name2)] = w_s$Dam_Name2 [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Dam_Name2)]
nabd_all_c$Dam_type[is.na(nabd_all_c$Dam_type)] = w_s$Dam_type [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Dam_type)]
nabd_all_c$Dam_height[is.na(nabd_all_c$Dam_height)] = w_s$Dam_height [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Dam_height)]
nabd_all_c$NIDStorM3[is.na(nabd_all_c$NIDStorM3)] = w_s$NIDStorM3 [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$NIDStorM3)]
nabd_all_c$NrmStorM3[is.na(nabd_all_c$NrmStorM3)] = w_s$NrmStorM3 [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$NrmStorM3)]
nabd_all_c$Condition[is.na(nabd_all_c$Condition)] = w_s$Condition [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Condition)]
nabd_all_c$Owner_type[is.na(nabd_all_c$Owner_type)] = w_s$Owner_type [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Owner_type)]
nabd_all_c$Purposes[is.na(nabd_all_c$Purposes)] = w_s$Purposes [match(nabd_all_c$SID,w_s$SID)][is.na(nabd_all_c$Purposes)]

## CHECK
z_s<-nabd_all_c%>%
  filter(SID=="NLA06608-1261"|SID=="NLA06608-2185"|SID=="NLA12_CA-119"|SID=="NLA06608-2950"|SID=="NLA06608-4537"|SID=="NLA06608-1953"|SID=="NLA06608-1866")%>%
  select(SID,Dam_length,Dam_name, Dam_Name2,Dam_type,Dam_height,NIDStorM3,
         NID_height,NrmStorM3,Condition,Owner_type,Purposes)

summary(nabd_all_c$NID_height)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#5.0    20.0    40.0    57.9    70.0   625.0    1092


## LOOKS GOOD



###############
## CONVERT DAM HEIGHT from ft to meters
#z<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/NLA_NABD_clean.csv")

nabd_all_c$dam_ht_m<-nabd_all_c$Dam_height*0.3048
summary(nabd_all_c$Dam_height)
summary(nabd_all_c$dam_ht_m)

nabd_all_c$NID_ht_m <- nabd_all_c$NID_height*0.3048
summary(nabd_all_c$NID_ht_m)

# Create a new Dam Purpose column with just first purpose listed
# https://stackoverflow.com/questions/7723549/getting-and-removing-the-first-character-of-a-string
nabd_all_c$purpose_red<-substring(nabd_all_c$Purposes, 1,1)
head(nabd_all_c$purpose_red)


  
####################
## WEST - SUPPLEMENTED and ESTIMATED DAM HT FOR LAKES WHERE NID ATTRIBUTES NOT AVAILABLE n = 64
# https://stackoverflow.com/questions/40177132/replace-values-from-another-dataframe-by-ids

# READ in dataset with filled in dam heightsn = 64 obs

# FOR LAPTOP
west_damht<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/missing/Dam_ht_supplemented_est.csv",stringsAsFactors = FALSE)
myvars<-c("SID","NID_ht_m") #"YEAR",
west_damht_red<-west_damht[myvars]

nabd_all_c<-left_join(nabd_all_c, west_damht_red, by = c("SID")) %>% #"YEAR"
  mutate(NID_ht_m = ifelse(is.na(NID_ht_m.x), NID_ht_m.y, NID_ht_m.x))# %>% 
  #select(SITE_ID, YEAR,NID_ht_m)
summary(nabd_all_c$NID_ht_m)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#1.000   6.096  12.192  18.444  22.555 221.285    1019


#########################
## MERGE DATASETS - NLA full and PROCESSED DAM n = 2066 obs  variables
nla_all_dam <- merge(nla_full, nabd_all_c, by=c("SID","YEAR"),all.x=TRUE)
names(nla_all_dam)

# DROP NID_ht_m.x &.y
myvars_drop <- names(nla_all_dam )%in% c("NID_ht_m.x", "NID_ht_m.y")
nla_all_dam <- nla_all_dam [!myvars_drop] # 

#########################

################
## PROCESS DATA
## Dam ht relative to lake depth - to use as an indication of hydrologic alteration potential
### USE THIS - CREATE DAMHT/ZMAX variable - similar to scaled drawdown (vertical DD/lake depth)
##    Larger values indicate greater hydro alteration potential
nla_all_dam$damht_zmax <- nla_all_dam$NID_ht_m/nla_all_dam$DpthMx_mod
summary(nla_all_dam$damht_zmax)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.021   1.524   2.189   2.796   3.227  49.784    1024 

## REPLACE DAM ATTRIBUE DATA FOR Edna Lake SID=NLA06608-2450 - I think dam attribute was for Clear Lake nearby
nla_all_dam$NID_ht_m[nla_all_dam$SID=="NLA06608-2450"]<- NA
nla_all_dam$damht_zmax[nla_all_dam$SID=="NLA06608-2450"]<- NA


#####################
## FULL POOL ZMAX
##  To provide a more logical dam height to zmax estimate - we need a more reliable measure of maximum lake depth
##    the NLA zmax is an approximate max depth and can be influenced if the lake water level is drawn down at the time of sampling
##    TO address this - we propose calculating the full zmax by adding the NLA zmax and vertical DD estimate to get a more accurat measure of maximum lake basin depth
##    For a small subset of lakes we documented zmax reported in the literature. I will not add vertDD to these lakes and will only use that measure
##  6/5/20
table(nla_all_dam$Zmax_source)
#INDEX   LIT   NLA 
#   19    32  2015 

nla_all_dam$zmax_full<-nla_all_dam$DpthMx_mod

# SUBSET dataset by zmax source
## NLA max depth approximation in NLA and Index source and only lakes with VertDD estimates
nla_zmax<-nla_all_dam %>%
  filter(Zmax_source=="NLA"|Zmax_source=="INDEX") %>%
  filter(!is.na(VertDD_use))

nla_zmax$zmax_full<-nla_zmax$DpthMx_mod + nla_zmax$VertDD_use
summary(nla_zmax$zmax_full)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.500   2.900   5.400   8.029   9.760  55.600       7 

## SUBSET full dataset of remaining observations (zmax from lit and VertDD = NA) to merge this to other dataset 
##    SELECT ALL OBSERVATIONS WITH SITE_IDS THAT DO NOT match SITE_IDs from the nla_zmax dataset
lit_zmax <- nla_all_dam%>%
  filter(!SITE_ID %in% nla_zmax$SITE_ID)
summary(lit_zmax$zmax_full)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.20    7.00   17.55   36.04   51.25  200.00       2 

## COMBINE ZMAX DATASETS
nla_all_dam<-rbind(nla_zmax,lit_zmax) # n = 2066 obs (single=1953; multi after processing=113)
nla_all_dam<-nla_all_dam[order(nla_all_dam$SITE_ID),]


#######################
## CALCULATE REVISED DAMhtzmax
nla_all_dam$damht_zmax_full<-nla_all_dam$NID_ht_m/nla_all_dam$zmax_full
summary(nla_all_dam$damht_zmax_full)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.021   1.392   2.000   2.353   2.793  30.480    1026 
summary(nla_all_dam$damht_zmax)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.021   1.524   2.192   2.801   3.232  49.784    1026 
hist(nla_all_dam$damht_zmax_full)
plot(nla_all_dam$VertDD_use~nla_all_dam$damht_zmax_full)
abline(lm(nla_all_dam$VertDD_use~nla_all_dam$damht_zmax_full))
plot(nla_all_dam$VertDD_use~nla_all_dam$damht_zmax)
plot(nla_all_dam$E_I~nla_all_dam$damht_zmax_full)
abline(lm(nla_all_dam$E_I~nla_all_dam$damht_zmax_full))

######################
## CHECK FOR NAS in OUTLET_DAMS n = 52
#outlet_na<-nla_all_dam%>%
#  filter(OUTLET_DAMS=="")%>%
#  select(SID,NARS_NAME,LATdd_use,LONdd_use,Lake_Origin_mod,OUTLET_DAMS,dam_ht_m)

## WILL FILL IN "ARTIFICIAL" for a lake if there is dam_ht_m information
##  OTHERWISE WILL ASSUME "NONE" - this may not be true but it would take time to hand inspect each one

# CREATE NEW VARIABLE TO MAKE CHANGES
nla_all_dam$OUTLET_DAMS_rev<-nla_all_dam$OUTLET_DAMS

table(nla_all_dam$OUTLET_DAMS_rev)
#     ARTIFICIAL    NATURAL       NONE 
#52       1176        123        715 

# CONDITIONAL STATEMENT WHEN MISSING OUTLET_DAMS AND HAVE dam_ht_m data
nla_all_dam<-nla_all_dam%>%mutate(OUTLET_DAMS_rev = case_when(OUTLET_DAMS=="ARTIFICIAL" ~"ARTIFICIAL",
                                                              OUTLET_DAMS=="NATURAL" ~ "NATURAL",
                                                              OUTLET_DAMS=="NONE"~ "NONE",
                                                              OUTLET_DAMS=="" & !is.na(dam_ht_m) ~ "ARTIFICIAL", # for blank Outlet_dam obs, if there is dam ht data in NABD, then label as "ARTIFICIAL"
                                                              TRUE ~ "NONE") ) # For all others, lable "NONE:
                                                               
table(nla_all_dam$OUTLET_DAMS_rev)
#ARTIFICIAL    NATURAL       NONE 
#1187        123          756 
# Looks like it worked to fill in the blank observations

######################
## CONSOLIDATE OUTLET_DAM CATEGORIES NONE = NONE + NATURAL
nla_all_dam$OUTLET_DAMS_red<-factor(nla_all_dam$OUTLET_DAMS_rev)
levels(nla_all_dam$OUTLET_DAMS_red) <- list("NA"=c("NA"), "DAM"=c("ARTIFICIAL"),
                                    "NONE"=c("NATURAL"),"NONE"=c("NONE"))
nla_all_dam$OUTLET_DAMS_red <-droplevels(nla_all_dam$OUTLET_DAMS_red)
table(nla_all_dam$OUTLET_DAMS_red)
#DAM NONE 
# DAM NONE 
#1187  879   


####################
## COMBINE NLCD LAND USE CLASSES
summary(nla_all_dam$PCT_AGRIC_BSN)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#0.0000000 0.0002302 0.0742475 0.1996931 0.3442923 0.9734097         1 
summary(nla_all_dam$PCT_DEVELOPED_BSN)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#0.0000000 0.0007585 0.0198165 0.0682667 0.0621827 0.9942252         1 

# MAKE INTO A PERCENT TO BE SIMILAR TO LAKECAT
nla_all_dam$PCT_AG_URB_BSN<- (nla_all_dam$PCT_AGRIC_BSN+nla_all_dam$PCT_DEVELOPED_BSN)*100
summary(nla_all_dam$PCT_AG_URB_BSN)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   1.378  17.272  26.796  46.826  99.797       1
plot(nla_all_dam$PCT_AG_URB_BSN~nla_all_dam$hifpAny)

####################
## COMBINE LAKECAT NLCD LAND USE CLASSES in STREAM CATCHMENT
nla_all_dam$PCT_AG_URB_CAT<- nla_all_dam$PctDEVELOPED_Cat+nla_all_dam$PctAGR_Cat
summary(nla_all_dam$PCT_AG_URB_CAT)

####################
## UPDATE NLA OUTLET_DAMS_red to put "DAM" when there is NID data
# USE case_when to have conditional statements
# https://mgimond.github.io/ES218/Week03a.html

### CREATE VARIABLE INDICATING IF THERE IS NID ATTRIBUTES AVAILABLE or Attributes were supplemented or estimated
nla_all_dam$in_NID<-"n"
nla_all_dam$in_NID[!(is.na(nla_all_dam$NID_ht_m))] <-"y"
table(nla_all_dam$in_NID)
#   n    y 
# 1021 1045   

# CONDITIONAL STATEMENT TO INDICATE IF DAM PRESENT
nla_all_dam<-nla_all_dam%>%mutate(OUTLET_DAMS_red2 = case_when(OUTLET_DAMS_red=="DAM" ~"DAM",
                                               OUTLET_DAMS_red=="NONE"& in_NID=="y" ~ "DAM" ,# if other criteria | = "or"   |in_NID=="e"|in_NID=="y,s"|in_NID=="s"
                                               TRUE ~ "NONE") )
table(nla_all_dam$OUTLET_DAMS_red)
# DAM NONE 
# 1187  879 

table(nla_all_dam$OUTLET_DAMS_red2) # This adds 64 lakes that were not reported to have dams in NLA but that have dam data in NID
# DAM NONE 
#1302  764  


# CHECK TO SEE IF DOING WHAT I THINK - LOOKS GOOD
#test<-nla_all_dam%>%filter(in_NID=="y",OUTLET_DAMS_red=="NONE") %>%
#  select(in_NID,OUTLET_DAMS_red,OUTLET_DAMS_red2)

table(nla_all_dam$OUTLET_DAMS_red2,nla_all_dam$in_NID)
#          n    y
#  DAM   257 1045
#  NONE  764    0
# There are 257 lakes that have dams but are not in the NID
# There are 13 lakes that are reported to not have a dam but are in the NID - I filled these in so now zero

############# - 
## IN WEST- Manually changed n=27 lakes reported in NLA to have dam but no NID info and not visible on Google Earth
##  In othe regions, will need to see which lakes do not have dam attributes but are listed as having a dam in the NLA - 
##   and gathering dam ht or deciding if the dam outlet is not visible so can't determine where to take height and put as NONE

# In FUTURE - try to fill in values from another dataset and match based on SID
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0086"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0127"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0173"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0593"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0596"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0625"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-0641"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-1217"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-1297"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-1445"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-1793"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-2134"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-2241"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-2450"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-2833"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-3750"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-4684"]<- "NONE" 
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_AZ-118"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_CA-174"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_CA-208"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_ID-205"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_NV-141"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_UT-110"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_UT-128"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_UT-148"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_UT-400"]<- "NONE"
nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA12_WA-114"]<- "NONE"

nla_all_dam$OUTLET_DAMS_red2[nla_all_dam$SID=="NLA06608-2257"]<- "NONE" # This lake is not being treated as having NONE


check<-nla_all_dam%>%filter(RESAMPLED12_b=="Y" & ECOP5_2015=="WEST" & OUTLET_DAMS_red2=="DAM")%>%
  select(SID, YEAR,RESAMPLED12_b,ECOP5_2015,OUTLET_DAMS_red2,NID_ht_m)
summary(check$NID_ht_m)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   6.706  15.545  22.918  34.747  74.676 
# Would expect this to not have any NAs

####################
## NEED TO SUBSET DATA FOR WEST ANALYSIS AND DROP SMALL LAKES
######################
## DIVIDE FULL DATASET BY YEAR AGAIN
nla07_b <- nla_all_dam %>%
  filter(YEAR==2007)

nla12_b <- nla_all_dam %>%
  filter(YEAR==2012)

## SUBSET OF UNIQUE NLA12 lakes n=688 lakes  in 2012
nla12_uniq_b<-nla12_b[!nla12_b$SID %in% nla07_b$SID,c(1:457)]#include all columns

# SUBSET OF UNIQUE NLA07 lakes n=678 lakes in 2007
nla07_uniq_b<-nla07_b[!nla07_b$SID %in% nla12_b$SID,c(1:457)]


#############################
## CREATE DATASETS WITH UNIQUE OBSERVATIONS (DROPPING RESAMPLED LAKES FROM ONE OF SURVEY YRS)
#############
## OPTION 1) NLA07 (full) + unique NLA12 lakes n=1716 >=1ha ; >=4ha 1629
nla07_mod<-rbind(nla07_b, nla12_uniq_b) 
length(unique(nla07_mod$SID)) #1629 
nla07_mod<-nla07_mod[order(nla07_mod$SITE_ID),]
table(nla07_mod$YEAR)
#2007 2012 
#1028  688 (all lakes) 601 (>=4ha)
table(nla07_mod$RESAMPLED12_b)
# N    Y 
#1366  350

################
## OPTION 2) NLA12 (full) + unique NLA07 lakes n=1629
nla12_mod<-rbind(nla12_b, nla07_uniq_b) 
length(unique(nla12_mod$SID)) #1629
nla12_mod<- nla12_mod[order(nla12_mod$SITE_ID),]
table(nla12_mod$YEAR)
# 2007 2012 
# 678  1038 vs 951 >=4ha 
table(nla12_mod$RESAMPLED12_b)
#    N    Y 
# 1366  350 

##############
# WRITE DATASET WITH TRANSFORMATIONS
#
##############
## FULL DATASET NLA 07+12 >=1ha RESAMPLED n = 2066
write.csv(nla_all_dam, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_US.csv")
# For laptop
write.csv(nla_all_dam,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_US.csv")

## OPTION 1 >=1 ha n = 1716
write.csv(nla07_mod, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_opt1_1ha_trans_NID_US.csv")

## OPTION 2  >=1 ha n = 1716
write.csv(nla12_mod, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_opt2_1ha_trans_NID_US.csv")

# FOR LAPTOP
# OPTION 1 >=1ha
write.csv(nla07_mod, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_opt1_1ha_trans_NID_US.csv")
# OPTION 2 >=1ha
write.csv(nla12_mod, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_opt2_1ha_trans_NID_US.csv")


###########
## SIZE ADJUSTED NLA 07+12 RESAMPLED n = 1979 lakes
nla_4ha_dam<-nla_all_dam %>% 
  filter(LkArea_km2>=0.04)
length(unique(nla_4ha_dam$SID)) # n = 1629

# FOR LAPTOP
write.csv(nla_4ha_dam, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_4ha_trans_NID_US.csv")

##############
## GET VARIABLE NAMES
z<-names(nla_all_dam)

write.csv(z,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_opt1_var_list.csv")

# For laptop
write.csv(z,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_US_var_list.csv")



###########################
## READ DATASETS WITH REVISED ZLOE DRAWDOWN SCORES from Phil 8/11/20
##  Adjusted expected reference mean to be for natural lakes (rather than man-made having their own reference sites in CENPL and WEST)
##  Revised scores (Z2 - ) are identical to the previous O/E z-scores but use only the natural lakes to calculate DD O?E for CENPL and WEST

# READ FULL NLA 2007 + 2012 dataset n = 2066
nla_all_dam<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_US.csv")

# READ REVISED ZOE SCORES n = 2484
zoe<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Data/Additional_NLA_data/From_Phil_081120/dd0712_modif_zscores2020.csv")

names(zoe)
names(zoe)[names(zoe)=="site_id"]<-"SITE_ID"

# REDUCE variables
myvars<-c("SITE_ID","VISIT_NO","Z2LOE_VertDD_use","Z2LOE_HorizDD_use","Z2LOE_DDVrtDix_sc","Z2LOE_DDHzSqrtA_sc")
zoe_red<-zoe[myvars]

test<-merge(nla_all_dam,zoe_red, by=c("SITE_ID","VISIT_NO"),all.x=TRUE)

names(test)
summary(test$Z2LOE_VertDD_use) # n =90 nas
summary(test$Z2LOE_DDHzSqrtA_sc) #n = 88 nas

###############
## WRITE DATASET 8/11/20
write.csv(test,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_z2oe_US.csv")



################
## COLUMN INDICATING WHERE DAM DATA CAME FROM 8/18/20
## READ DATASET 8/18/20
nla<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_z2oe_US.csv")
table(nla$in_NID)

# READ DATA WEST SUPPLEMENTED DAM FOR LAPTOP n = 64
west_damht<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/missing/Dam_ht_supplemented_est.csv",stringsAsFactors = FALSE)
table(west_damht$YEAR)
#2007 2012 
#41   23 

## NEED TO OMITTED DAM HEIGHTS THAT WERE VISUALLY ESTIMATED IN THE WEST - LITTLE CONFIDENCE THAT ARE ACCURATE
table(west_damht$in_NID)
#e   s y,s 
#32  31   1

# CREATE NEW COLUMN INDICATING WHETHER DAM HT WAS SUPPLEMENTED OR ESTIMATED
west_damht$NID_supp<-factor(west_damht$in_NID)
levels(west_damht$NID_supp) <- list("e"= c("e"), "supp"= c("s"),
                                            "supp"= c("y,s"))
west_damht$NID_supp<- droplevels(west_damht$NID_supp)
table(west_damht$NID_supp)
#e supp 
#32   32

# REDUCE DATASET
myvars<-c("SID","NID_supp") #"YEAR","NID_ht_m"
west_damht_red<-west_damht[myvars]

###################
## JOIN DATASETS and create new column indicating where dam data came from
###   NID_supp2: nid = from NID; est = estimated from google earth; supp = supplemented from US Bureau of Reclaimation
##        none = no dam present

## MUTATE creates new column "NID_supp2" and gives labels based on conditions 
#   first - was there no dam attribute designation (this is majority of lakes)
#   second - in the merged west dataset - for lakes labeled "e" change to "est"
#   third - in merged west data set - for lakes labeled "supp" - keep as "supp"
#   fourth - all else - labels as "nid" indicating there was NID information
# n = 2066 obs

test<-left_join(nla,west_damht_red, by = c("SID"))%>%
  mutate(NID_supp2 = case_when(is.na(NID_supp) & in_NID=="n" ~ "none",
                                NID_supp=="e" ~"est",
                                NID_supp=="supp"~"supp",
                                TRUE ~ "nid"))
  
table(test$NID_supp2)
#est  nid none supp 
#38  970 1021   37 
table(test$in_NID)
#n    y 
#1021 1045 
summary(test$NID_ht_m)

## NOW CREATE DAM HEIGHT variables THAT REMOVES NID ht for lakes where it was estimated from google earth
# Create columns to preseve old estimates
test$NID_ht_m_old<-test$NID_ht_m
test$damht_zmax_old<- test$damht_zmax
test$damht_zmax_full_old<-test$damht_zmax_full
summary(test$NID_ht_m_old)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.000   6.096  12.192  18.473  22.555 221.285    1021

# CHANGE DAM HT AND OTHER VARS RELATED TO DAM HT TO NA for lakes where height was estimated (NID_supp2=="est)
test2<-mutate(test, NID_ht_m=ifelse(NID_supp2=="est", NA, NID_ht_m_old),
              damht_zmax = ifelse(NID_supp2=="est", NA, damht_zmax_old),
              damht_zmax_full = ifelse(NID_supp2=="est", NA,damht_zmax_full_old))

summary(test2$NID_ht_m)
summary(test2$damht_zmax)
summary(test2$damht_zmax_full)


#############################
## WRITE NEW DATASET that replaces estimated dam height with NA 8/18/20
write.csv(test2,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_z2oe_US.csv")


##################
## SUBSET WEST LAKES ONLY
# FULL data n = 501
WEST_full<- nla_all_dam %>%
  filter(ECOP5_2015=="WEST")

table(WEST_full$RESAMPLED12_b)
# N   Y 
#357 144  


## OPTION 1 (FULL 2007 + Unique 2012) n = 429 obs
WEST_opt1 <- nla07_mod %>%
  filter(ECOP5_2015=="WEST")
table(WEST_opt1$RESAMPLED12_b)
# N   Y 
#357  72 


## OPTION 2 (FULL 2012 + Unique 2007) n = 429
WEST_opt2 <- nla12_mod %>%
  filter(ECOP5_2015=="WEST")
table(WEST_opt2$RESAMPLED12_b)


## WRITE DATASETS WEST >=1ha
write.csv(WEST_full, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_full_WEST.csv")
write.csv(WEST_opt1, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_opt1_WEST.csv")
write.csv(WEST_opt2, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_opt2_WEST.csv")

# FOR LAPTOP
write.csv(WEST_full, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_full_WEST.csv")
write.csv(WEST_opt1, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_opt1_WEST.csv")
write.csv(WEST_opt2, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_1ha_opt2_WEST.csv")


##################
## SIZE ADJUSTED 
# WEST n = 408
WEST_4ha<-WEST_full %>%
  filter(LkArea_km2>=0.04)

# WEST Opt 1 n = 408
WEST_opt1_4ha<-WEST_opt1 %>%
  filter(LkArea_km2>= 0.04)

# WEST Opt 2 n = 408
WEST_opt2_4ha<-WEST_opt2 %>%
  filter(LkArea_km2>= 0.04)

## WRITE DATASETS WEST >=4ha
write.csv(WEST_4ha, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_4ha_full_WEST.csv")
write.csv(WEST_opt1_4ha, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_4ha_opt1_WEST.csv")
write.csv(WEST_opt2_4ha, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_4ha_opt2_WEST.csv")

# FOR LAPTOP
write.csv(WEST_4ha, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_4ha_full_WEST.csv")
write.csv(WEST_opt1_4ha, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_4ha_opt1_WEST.csv")
write.csv(WEST_opt2_4ha, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/NLA_0712_4ha_opt2_WEST.csv")



################################
## DATA EXPLORATION
## REARRANGE VARIABLES
myvarsII <- c("SITE_ID","SID","UID","YEAR","RESAMPLED12_b","NARS_NAME","STATE",
              "LkArea_km2","LATdd_use","LONdd_use","Lake_Origin_mod",#"INLET_OUTLET","INLET_OUTLET_notes",
              #"DAM","DAM_notes","Hydro_alteration","hap_rank","questionable","in_NID","nat_modif","NOTES",
              "XCOORD","YCOORD","ECOP5_2015","ECOREG_use","OUTLET_DAMS","inStreamCat",
              "HYDRO_TYPE_f","lk_hydro_iso",
              "DpthMx_mod","SLD","PERIM_KM",
              "BASINAreaSqKM","ELEV_use","ELEVMAX_BSN_m","elev_relief_m",
              "VertDD_use","HorizDD_use","E_I","d_excess","RT_iso","Drawdown_CONDus15",
              "RT_NLA12_2015","MAN_SCORE","PCT_FOREST_BSN","PCT_WETLAND_BSN",
              "PCT_AGRIC_BSN","PCT_DEVELOPED_BSN",
              "hiiAg","hiiAgCirca","hiiNonAg","hiiNonAgCirca",
              "hiiAll","hiiAllCirca","hifpAny","hifpAnyCirca",
              "PctIrrigated.AgLandWs","PctDEVELOPED_Cat","PctAGR_Cat",
              "Dam_length","Dam_name","Dam_Name2","Dam_type","Dam_height", #"nabd_multi_dam",
              "NIDStorM3","NID_height",
              "NrmStorM3","Condition","Owner_type","Purposes",
              "purpose_red","dam_ht_m","NID_ht_m","damht_zmax",
              "PctAgDrainageCat","PctAgDrainageWs") #,"nabd_Notes"

nla_all_dam_red<-nla_all_dam[myvarsII]
#write.csv(nla_all_dam_red, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NLA_0712_1ha_NABD_forexcel.csv")

# FOR LAPTOP
#write.csv(nla_all_dam_red, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ALL/NLA_0712_1ha_NABD_forexcel.csv")

#########################
## EXPLORING ARTIFICIAL DRAINAGE VARIABLE
summary(nla_all_dam$PctAgDrainageCat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.0000  0.0000  1.4748  0.0796 75.9147       3 

## COMPOSITION OF OBSERVATIONS n = 1364 obs out of 2066 66% of NLA observations
zero<-nla_all_dam_red%>%
  filter(PctAgDrainageCat==0)

not_zero<-nla_all_dam_red%>% #n = 699 obs
  filter(PctAgDrainageCat>0)

miss<-nla_all_dam_red%>% # n = 3 NLA06608-0433; NLA12_MI-138; NLA12_MN-141
  filter(is.na(PctAgDrainageCat))

# Look at Ag drainage compared to NLCD ag
plot(not_zero$PctAgDrainageCat,not_zero$PctAGR_Cat)
abline(1,1) # see if there are obs where drainage % is greater than agr %

# There are 45 observations where drainage % exceeds agriculture% in direct drainage
agr_lg<-not_zero%>%
  filter(PctAgDrainageCat>PctAGR_Cat)

plot(not_zero$PctAgDrainageCat, not_zero$elev_relief_m)
abline(500,0)


plot(not)

# WHAT IS RANGE OF TOPO RELIEF WHERE THERE IS DRAINAGE
summary(not_zero$elev_relief_m)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-0.44   27.03   54.86  201.82  120.84 4174.32 
# From plot it seems like most drainage occurs below 200 m
# Perhaps that is the topo relief range where drainage may be more prevalent and influential to hydrology

table(not_zero$Lake_Origin_mod)
#MAN_MADE  NATURAL 
#319      380
table(not_zero$ECOREG_use)
#CPL NAP NPL SAP SPL TPL UMW WMT XER 
#106  54  42  62  48 217 147   9  14 

############
## CORRELATIONS 
############
library(ggcorrplot)
# REDUCED DATASET 
z<- not_zero[c("PctAgDrainageCat","LkArea_km2","DpthMx_mod","SLD","BASINAreaSqKM",#"L_basinarea",
                  "elev_relief_m","VertDD_use", "HorizDD_use","E_I","RT_iso", #bffFlat","bffGradual","bffSteep","bffVertical",
                  "hiiAll", "damht_zmax")] #"L_lk_size_mod",

######
## ALTERNATIVE CORRELATION PLOT WITH OTHER INFO
#   Exploratory matrix plots numerical and categorical data
library(GGally)
lk_matrix<-ggpairs(z)

tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/Data exploration/Drivers_lake_hydro/Routput/LakeCat_NLA/matrix_ag_drain.tiff",
     width=7, height=7,units="in", res = 200)
print(lk_matrix)
dev.off()

###################
## Look at subset of lakes with low topographic relief to see about applying ag drainage
flat<-nla_all_dam%>% # n = 1438
  filter(elev_relief_m<201)
summary(flat$PctAgDrainageCat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0000  0.0000  0.0000  2.0737  0.2553 75.9147       1 

#####################
## SUBSET OF LAKES WITH LOW TOPO RELEIF, MODERATE LU, and LOW DRAINAGE
flat<-nla_all_dam%>% # n = 1106 lakes in flat terrain, with moderate LU, and drainage>1%
  filter(elev_relief_m<201,PCT_AG_URB_CAT<=50,PctAgDrainageCat<=10) #, damht_zmax<0.5 n = 38
summary(flat$PctAgDrainageCat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00000 0.00000 0.00000 0.44152 0.04739 9.92079 
summary(flat$damht_zmax)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.0593  1.5240  2.1102  2.5379  2.9308 18.7500     633 

#####################
## SUBSET OF LAKES WITH LOW TOPO RELEIF, MODERATE LU, and HIGH DRAINAGE
flatb<-nla_all_dam%>% # n = 31 lakes in flat terrain, with moderate LU, and drainage>1%
  filter(elev_relief_m<201,PCT_AG_URB_CAT<=50,PctAgDrainageCat>10)
summary(flatb$PctAgDrainageCat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10.02   11.38   13.51   15.48   16.84   29.53 
summary(flatb$damht_zmax)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.8333  1.7211  2.3830  2.9300  3.2902  8.7630      12 

#####################
## SUBSET OF LAKES WITH LOW TOPO RELEIF, HIGH LU, and LOW DRAINAGE
flat2<-nla_all_dam%>% # n = 232
  filter(elev_relief_m<201,PCT_AG_URB_CAT>50,PctAgDrainageCat<=10) #, damht_zmax<0.5 n = 6
summary(flat2$PctAgDrainageCat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00000 0.00000 0.04639 1.05740 0.56253 9.30144 
summary(flat2$damht_zmax)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.1293  1.7685  2.5665  3.1108  4.1194 10.6680     144 

#####################
## SUBSET OF LAKES WITH LOW TOPO RELEIF, HIGH LU, and HIGH DRAINAGE
flat2b<-nla_all_dam%>% # n = 68
  filter(elev_relief_m<201,PCT_AG_URB_CAT>50,PctAgDrainageCat>10,damht_zmax<0.5) #, damht_zmax<0.5 n = 6
summary(flat2b$PctAgDrainageCat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10.23   13.87   20.87   25.98   36.31   75.91
summary(flat2b$damht_zmax)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.1111  0.4918  1.9402  2.1892  2.1797  7.1018      48 

######################
## WHAT LAKES WITH TILE DRAINAGE ARE WE MISSING WHEN WE CUT OFF THE TOPO ELEV at 200m
hi_relief<-nla_all_dam%>%
  filter(elev_relief_m>200,PctAgDrainageCat>1)# n = only 18 obs have at least 1% drainage compared to 114 with some drainage out of 631 obs

summary(hi_relief$PctAgDrainageCat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.016   1.298   1.831   2.744   2.548   7.415

######################
## LAKES WITHOUT DAMS
no_dam<-nla_all_dam%>% # n = 712
  filter(OUTLET_DAMS_red=="NONE",PCT_AG_URB_BSN>0,PctAGR_Cat>50)

summary(no_dam$PctAgDrainageCat)
# LAKES WITH LOW AG
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.9105  0.2026 29.0000 

# Lakes with high ag
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00000  0.09287  2.97451  9.95991 13.93412 75.91472 
summary(no_dam$PctDEVELOPED_Cat)

######################
## LAKES WITHOUT DAMS & moderate LU
no_dam_mod<-nla_all_dam%>% # n = 121 
  filter(OUTLET_DAMS_red=="NONE",PctAGR_Cat>10 & PctAGR_Cat<50,PctAGR_Cat<=30) #,PctDEVELOPED_Cat>25

summary(no_dam_mod$PctAGR_Cat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10.09   14.08   18.48   19.06   23.88   29.92 

summary(no_dam_mod$PctDEVELOPED_Cat)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.6838  3.6956  3.7180 46.2963 


######################
## LAKES WITHOUT DAMS & moderate LU
no_dam_mod2<-nla_all_dam%>% # n = 90 
  filter(OUTLET_DAMS_red=="NONE",PCT_AG_URB_CAT>10 & PCT_AG_URB_CAT<50,PctAGR_Cat>30)

summary(no_dam_mod2$PctAgDrainageCat)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00000  0.05227  0.90133  2.95780  4.32211 29.00000 

summary(no_dam_mod2$PctDEVELOPED_Cat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.1915  1.5815  1.5476 12.4283

table(no_dam_mod2$Drawdown_CONDus15)
