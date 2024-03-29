#################
## Year comparisons of NLA 2007 and 2012 data
##  Create dataset of lakes that were sampled both years
##  Plot drawdown and E:I measures for each year to see changes within the same lakes
##   Plot climate variables for each year to see changes within the same lakes

##  10/11/17 - EF

## 10/17/17 - Created new dataset with both years observations joined together (not just lakes that were visited both years)
#             Using this to create histogram of d-excess and colored by year

# 12/13/17 - Updated datasets to include water residence time and watershed yield

# 1/17/18 - Added PRISM 30 yr average climate data to datasets

# 2/7/18  - Added elevation in basin

# 3/8/18 - Added scaled drawdown variables

# 5/14/18 - Added more complete datasets with NAs

# 5/29/18 - Added Palmer Drought Indicies - See Palmer_data_29MAY18.R for data processing

# 8/2/18 - updated E:I & WRT estimates

# 6/7/19 - Include Survey year precipitaiton and and evaporation

# 6/27/19 - updated lake volume and wrt for 2012 lakes and updated size adjusted 2012 dataset (n=951)

# 7/8/19 - updated zmax for NLA07=24 lakes and NLA12(szadj) = 8 lakes
#################

rm(list=ls())

###########
# Libraries
###########
library(maps)
library(mapdata)
library(scales)
library(maptools)

library(rgdal)

library(dplyr)
library(ggplot2)
library(lattice)
library(Hmisc)
library(plotrix)
library(pgirmess)
library(grid) # For Viewport
library(gridExtra)


######################
## LOAD DATA to create modified dataset
######################

# Original dataset of lakes Phil shared - This seems like it's all obs from 2007 and 2012 put together - maybe not helpful
#NLA0712_org<- read.csv("M:/Net MyDocuments/a_Water_Level/Data/nla0712_isodrawdown_20160219.csv")
#names(NLA0712_org)
#table(NLA0712_org$SID)

#length(unique(NLA0712_org$SITE_ID)) # 2292 out of 2492 obs - NOTE - SITE_IDs do not necessarily match up between sample years
#length(unique(NLA0712_org$SID))# 1901 out of 2492 obs


## NLA DATASETS PREPROCESSED FOR ANALYSIS
## NLA 2007 data - SINGLE observations only, dropped lakes with zero weights, only retained lakes "VISIT_NO"=1
# 10/30/17 - used updated dataset with corrected depth for deep lakes
# 12/13/17 - WRT and WYIELD added

# OLD - using full dataset nla07<-read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_depth.csv")
# 2/7/18 - this did not include connectivity class (n=962 obs) nla07<-read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_SINGLE_USE.csv")
# OLD NLA 07 with connectivity classes  n=874
# USE one without connectivity class n=962 obs
# 5/14/18 - Full dataset with NAs 1026 lakes (dropped missing lake depth and RT)
#5/15/18 - Full dataset with NAs 1028
# 5/29/18 - Added Palmer Drought Indices (WY averages)

# NLA07 used in pop analysis
#nla07<-read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_VISIT_1_WGT_USE.csv") #nla07<-read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_SINGLE_use.csv")

#NLA07 with Palmer drought
# 6/27/19 - updated dataset n = 1028
# 7/8/19 - updated zmax for 24 lakes
nla07 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

# OLD nla07<-read.csv("M:/Net MyDocuments/a_Water_Level/Data/Climate data/Palmer_Drought_Data/PROCESSED/NLA07_all.csv")
#names(nla07)
table(nla07$RESAMPLED12)
# DRP  NO YES 
#   2 660 366  - about 36% of lakes were resampled in 2012
#test<- nla07[which(nla07$LkArea_km2>=20),]


## NLA 2012 data - SINGLE OBSERVATION, only "VISIT_NO" =1;  
# FULL Dataset n = 1038 observations 
# OLD Full nla12<- read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_ALL_VARS_merge_transform_USE.csv")
#nla12<- read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_merge_transform_SINGLE_USE.csv")

# NLA12 dataset used for pop estimates
#nla12<- read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA_12_FULL_VISIT_1_WGT_USE.csv")
# WITH PALMER DROUGHT
# 8/2/18 - updated E:I & WRT values - had to go to Palmer_data_29MAY18.R script to add modified NLA 2012 dataset
#nla12 <- read.csv("M:/Net MyDocuments/a_Water_Level/Data/Climate data/Palmer_Drought_Data/PROCESSED/NLA12_all.csv")
#table(nla12$RESAMPLED12)
# DRP  NO YES 
#   2 644 392 
#6/7/19 Updated NLA12 dataste
# READ DATA - VISIT_NO=1; WGT_ALL>0 n = 1038
#nla12<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA_12_FULL_VISIT_1_WGT_USE_PHDI.csv")


# 6/27/19 Updated NLA 2012 full dataset
# 7/8/19 Updated zmax for 8 lakes
nla12<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_lkcat_wgt_26JUN19.csv")
todrop <- names(nla12)%in% c("X")
nla12 <- nla12[!todrop]
table(nla12$RESAMPLED12)
#DRP  NO YES 
#2 644 392


# ChangeData set - Original saved in "L:/Priv/ARM Data/NLA 2012/NLA 2012 Change Estimation/NLA2012_ChangeData_2016-08-23.orig.csv"
#  n = 2066
ch0712<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/NLA2012_ChangeData_2016-08-23.orig.csv")
head(ch0712)
myvars<- c("SITE_ID","CH0712_CAT")

table(ch0712$REVSAMP_0712)
z<-ch0712[which(ch0712$REVSAMP_0712=="Y"),] # n = 720 unique lakes
length(unique(z$SITE_ID))

############
## Process datasets
###########

##############
# NLA 2007 processing
names(nla07)

#####
## Rename columns in NLA 2007 to match NLA 2012 - "old" to "new"
names(nla07)[names(nla07)=="LAKEPERIM"]<-"PERIM_KM"
names(nla07)[names(nla07)=="Lake_Vol_m3"]<-"Volume_m3"
names(nla07)[names(nla07)=="SLD"]<-"shorlineDevelopment"
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

names(nla07)[names(nla07)=="PCT06_FOREST_BSN_logit"]<-"PCT_FOREST_BSN_logit"
names(nla07)[names(nla07)=="PCT06_GRASS_BSN_logit"]<-"PCT_GRASS_BSN_logit"
names(nla07)[names(nla07)=="PCT06_WETLAND_BSN_logit"]<-"PCT_WETLAND_BSN_logit"
names(nla07)[names(nla07)=="PCT06_AGRIC_BSN_logit"]<-"PCT_AGRIC_BSN_logit"
names(nla07)[names(nla07)=="PCT06_DEVELOPED_BSN_logit"]<-"PCT_DEVELOPED_BSN_logit"

# 2/7/18
names(nla07)[names(nla07)=="DOM_GEOL"]<-"DOMGEOL_BSN"
names(nla07)[names(nla07)=="GEOL_PT"]<-"SITE_GEOLOGY"
names(nla07)[names(nla07)=="Max_WSelev"]<-"ELEVMAX_BSN_m"
names(nla07)[names(nla07)=="Mean_WSelev"]<-"ELEVMEAN_BSN_m"

# 5/29/18 PALMER DROUGHT
names(nla07)[names(nla07)=="pdsi_nla07_WY_AVG"]<-"PDSI"
names(nla07)[names(nla07)=="phdi_nla07_WY_AVG"]<-"PHDI"
names(nla07)[names(nla07)=="pmdi_nla07_WY_AVG"]<-"PMDI"
names(nla07)[names(nla07)=="zndx_nla07_WY_AVG"]<-"ZNDX"

# 5/29/18 PALMER DROUGHT
#names(nla12)[names(nla12)=="pdsi_nla12_WY_AVG"]<-"PDSI"
names(nla12)[names(nla12)=="phdi_nla12_WY_AVG"]<-"PHDI"
#names(nla12)[names(nla12)=="pmdi_nla12_WY_AVG"]<-"PMDI"
#names(nla12)[names(nla12)=="zndx_nla12_WY_AVG"]<-"ZNDX"

#1/17/18 PRISM 30yr average climate data - Added Precip_PT


#names(nla07)[names(nla07)=="Drawdown_CONDus15"]<-"DRAWDOWN_COND"

### Reorder variables in 2007 and 2012 datasets so that the columns match up
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","DATE_COL_iso","SAMPLE_ID","SAMPLED_PHAB",
           "dD","d18O","d_excess","E_I","RT_iso","Water_yield_m",
           "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use","L_RT_iso",
           "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc","DDVrtDix_sc_MOD","L_DDVrtDix_sc_MOD",
           "VertDD_CONDc315","HorizDD_CONDc315","Drawdown_CONDus15",
           "LkArea_km2","L_LkAreakm2","DpthMx_use","L_DpthMx_use",
           "PERIM_KM","Volume_m3","shorlineDevelopment","ELEV_use","L_ELEV_use", #"LAKEPERIM","Lake_Vol","SLD"
           "LATdd_use","LONdd_use", "XCOORD","YCOORD",#"ALBERS_X","ALBERS_Y",
           "STATE","URBAN","Lake_Origin_use","HYDRO_TYPE_f","lk_hydro_iso",
           "NARS_NAME","ECOWSA9_2015",#"ST","LAKENAME"
           "WSA9_LO","ECOP5_2015","ECOP6_2015","RESAMPLED12","SIZE_CLASS","SITETYPE","TNT",#"SITE_TYPE",
           "WGT_ALL","RT_NLA12_2015", #"WGT_NLA"
           "NH4N_PPM","ANC","CA_PPM","CL_PPM","COLOR","COND","DOC",
           "MG_PPM","NO3N_PPM","NO3_NO2","NTL","PH_LAB","K_PPM",
           "PTL","SIO2","NA_PPM","SO4_PPM","TOC","TURB",
           "CHLA","SECMEAN","MCYST_TL_UGL",
           "amfcAll","amfcEmergent","amfcFloating","amfcSubmergent",
           "bffFlat","bffGradual","bffSteep","bffVertical","bfoAngle",
           "bfxHorizDist","bfxVertHeight",
           "bsfcGravel","bsfcOrganic","bsfcSand","bsfcSilt","bsfcWood",
           "bsiSiteVariety","bsiStaVariety","bsvLdia","bsxLdia",
           "fcfcAquatic","fcfcBoulders","fcfcBrush","fcfcLedges","fcfcLiveTrees",
           "fcfcSnag","fcfcStructures",
           "hifpAny","hifpAnyCirca","hiiAg","hiiAgCirca",
           "hiiAllCirca","hiiNonAg","hiiNonAgCirca",
           "hipwBuildings","hipwCommercial","hipwCrops","hipwDocks","hipwLandfill",
           "hipwLawn","hipwOrchard","hipwPark","hipwPasture","hipwPowerlines",
           "hipwRoads","hipwWalls",
           "rvfcCanBig","rvfcCanSmall","rvfcGndBare","rvfcGndInundated",
           "rvfcGndNonw","rvfcGndWoody","rvfcUndNonw","rvfcUndWoody",
           "rviCanopy","rviUnderstory",
           "rviGround","rviHerbs","rviTallWood","rviTotalVeg","rviCanUnd","rviWoody",
           "ssfcBedrock","ssfcBoulders",
           "ssfcCobble","ssfcGravel","ssfcOrganic","ssfcOther","ssfcSand","ssfcSilt",
           "ssfcWood","ssiSiteVariety","ssiStaVariety",
           "ssvLdia","ssxLdia",
           "BASINAreaSqKM","OPEN_km2","ICE_km2",
           "DEVOPEN_km2","DEVLOW_km2","DEVMED_km2","DEVHIGH_km2",
           "BARREN_km2","DECID_km2","CONIF_km2","MIXED_km2",
           "SHRUBLAND_km2","GRASS_km2","PASTURE_km2","CROPS_km2",
           "WDYWET_km2","EMHERBWET_km2",
           "PCT_FOREST_BSN_logit","PCT_GRASS_BSN_logit","PCT_WETLAND_BSN_logit","PCT_AGRIC_BSN_logit","PCT_DEVELOPED_BSN_logit",
           "Precip_PT","E","RH_PT","TMEAN_PT","P_WY",
           "Temp_degC_avg_yr","Precip_mm_avg_yr","Precip_mm_total_yr",
           "temp_degC_winter","temp_degC_spring","temp_degC_summer",
           "precip_mm_winter","precip_mm_spring","precip_mm_summer",
           "DpthMx_mod","Zmax_source","TROPHIC_STATE","DOMGEOL_BSN","SITE_GEOLOGY",
           "ELEVMAX_BSN_m","ELEVMEAN_BSN_m","PHDI","WGT_SP",
           "Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws") #"PDSI", "PMDI","ZNDX"

nla07_red <- nla07[myvars]


## NLA 2012 - Changed volume estimate to be Hollister's estimate
names(nla12)
#nla12$L_LKAREA_KM2_mod <-log10(nla12$LKAREA_KM2_mod)
# Changed variables (dropped "LkAreakm2","L_LkAreakm2", "LATdd_use", "LONdd_use") replaced with "LKAREA_KM2_mod","L_LKAREA_KM2_mod","LAT_DD83","LON_DD83",

myvars_12<- c("SITE_ID","VISIT_NO","SID","YEAR","DATE_COL_iso","SAMPLE_ID","SAMPLED_PHAB",
              "dD","d18O","d_excess","E_I","RT_iso","Water_yield_m",
              "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use","L_RT_iso",
              "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc","DDVrtDix_sc_MOD","L_DDVrtDix_sc_MOD",
              "VertDD_CONDc315","HorizDD_CONDc315","Drawdown_CONDus15",
              "LKAREA_KM2_mod","L_LKAREA_KM2_mod","DpthMx_use","L_DpthMx_use","PERIM_KM",
              "Volume_Corrected_m3","SLD", # "VolumeCorrect"old version,
              "ELEV_use","L_ELEV_use","LAT_DD83","LON_DD83","XCOORD","YCOORD",
              "STATE","URBAN","LAKE_ORIGIN","HYDRO_TYPE_f","lk_hydro_iso", # old Lake_Origin_use
              "NARS_NAME",
              "ECOWSA9_2015","WSA9_LO","ECOP5_2015","ECOP6_2015","RESAMPLED12","SIZE_CLASS","SITETYPE","TNT",
              "WGT_ALL","RT_NLA12_2015",#"WGT_ALL"
              "AMMONIA_N_RESULT_mgL","ANC_RESULT_ueqL",
              "CALCIUM_RESULT_mgL","CHLORIDE_RESULT_mgL","COLOR_RESULT_PtCo","COND_RESULT_uscm",
              "DOC_RESULT_mgL","MAGNESIUM_RESULT_mgL","NITRATE_N_RESULT_mgL","NITRATE_NITRITE_N_RESULT_mgL","NTL_RESULT_ugL", # NOTE - using the transformed value
              "PH_RESULT","POTASSIUM_RESULT_mgL","PTL_RESULT_ugL","SILICA_RESULT_mgL","SODIUM_RESULT_mgL","SULFATE_RESULT_mgL",
              "TOC_RESULT_mgL","TURB_RESULT_NTU",
              "CHLL_RESULT_ugL","SECCHI_m","MICL_RESULT_ugL",
              "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
              "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE","BFXHORIZDIST","BFXVERTHEIGHT",
              "BSFCGRAVEL","BSFCORGANIC","BSFCSAND","BSFCSILT","BSFCWOOD",             
              "BSISITEVARIETY","BSISTAVARIETY","BSVLDIA","BSXLDIA",
              "FCFCAQUATIC_SIM","FCFCBOULDERS_SIM","FCFCBRUSH_SIM",
              "FCFCLEDGES_SIM","FCFCLIVETREES_SIM","FCFCSNAGS_SIM","FCFCSTRUCTURES_SIM",
              "HIFPANY_SYN","HIFPANYCIRCA_SYN","HIIAG_SYN","HIIAGCIRCA_SYN","HIIALLCIRCA_SYN","HIINONAG_SYN","HIINONAGCIRCA_SYN",
              "HIPWBUILDINGS_SYN","HIPWCOMMERCIAL_SYN","HIPWCROPS_SYN","HIPWDOCKS_SYN","HIPWLANDFILL_SYN","HIPWLAWN_SYN","HIPWORCHARD_SYN","HIPWPARK_SYN","HIPWPASTURE_SYN","HIPWPOWERLINES_SYN","HIPWROADS_SYN","HIPWWALLS_SYN",
              "RVFCCANBIG_SYN","RVFCCANSMALL_SYN","RVFCGNDBARE_SYN",
              "RVFCGNDINUNDATED_SYN","RVFCGNDNONW_SYN","RVFCGNDWOODY_SYN",
              "RVFCUNDNONW_SYN","RVFCUNDWOODY_SYN",
              "RVICANOPY_SYN","RVICANUND_SYN","RVIGROUND_SYN",
              "RVIHERBS_SYN","RVITALLWOOD_SYN","RVITOTALVEG_SYN","RVIUNDERSTORY_SYN",
              "RVIWOODY_SYN",
              "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",          
              "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA",
              "BASINAreaSqKM","OPEN_11_km2","ICE_11_km2","DEVOPEN_11_km2","DEVLOW_11_km2","DEVMED_11_km2","DEVHIGH_11_km2",
              "BARREN_11_km2","DECID_11_km2","CONIF_11_km2","MIXED_11_km2","SHRUBLAND_11_km2","GRASS_11_km2",
              "PASTURE_11_km2","CROPS_11_km2","WDYWET_11_km2","EMHERBWET_11_km2",
              "PCT_FOREST_BSN_logit","PCT_GRASS_BSN_logit","PCT_WET_BSN_logit","PCT_AGR_BSN_logit","PCT_DEVELOPED_BSN_logit",
              "Precip_PT","E_m","RH_PT","TMEAN_PT","P_WY_m",
              "Temp_degC_avg_yr","Precip_mm_avg_yr","Precip_mm_total_yr",
              "temp_degC_winter","temp_degC_spring","temp_degC_summer",
              "precip_mm_winter","precip_mm_spring","precip_mm_summer",
              "DpthMx_mod","Zmax_source","TROPHIC_STATE","DOMGEOL_BSN","SITE_GEOLOGY",
              "ELEVMAX_BSN_m","ELEVMEAN_BSN_m","PHDI","WGT_SP", 
              "Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws") # PRISM 30yr avg #"PDSI","PMDI","ZNDX"

nla12_red <- nla12[myvars_12]

# Need to have variable column names completely match to perform rbind
var_names_07 <- names(nla07_red)

# Check to make sure variables are lined up - YES LOOKS GOOD
#vars_07 <- names(nla07_red)
#write.csv(vars_07,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/vars_07.csv")

#vars_12 <- names(nla12_red)
#write.csv(vars_12,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/vars_12.csv")

##############
# RELABEL all of nla12_red column names to match nla07_red column names
names(nla12_red)<- var_names_07 
names(nla12_red)


###########
## BIND BOTH YEARS OF DATA into long-format
# 12/13/17 - using only Single observation datasets
## 10/17/17 - Create dataset of both years (all observations - not just lakes that were sampled both years)
# Combine datasets using rbind
nla07_12<-rbind(nla07_red, nla12_red)

#clim_merge <-merge(clim07, clim12, by=c("ECOREG_use")) 
nla07_12$ECOREG_use<-factor(nla07_12$ECOWSA9_2015, labels=c("CPL","NAP","NPL","SAP","SPL","TPL","UMW","WMT","XER"))
nla07_12$ECOREG_use <- ordered(nla07_12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla07_12$YEAR<-factor(nla07_12$YEAR, labels=c("2007","2012")) 
table(nla07_12$YEAR)
# 2007 2012 
# 1028 1038 

# Make Lake_Origin_use all the same label AND Redo WSA9_LO
# 5/14/18 - updated - don't need to do this because fixed difference in data creation steps
table(nla07_12$Lake_Origin_use)
#MAN_MADE  NATURAL # Adds up to 2066 total number
#1175      891 
#levels(nla07_12$Lake_Origin_use) <- list(#
  #"MAN_MADE" = c("MAN-MADE"),
  #"NATURAL" = c("NATURAL"))

table(nla07_12$WSA9_LO) # different notation for MAN-MADE vs MAN_MADE
# RECLASSIFY ECOREGION + LAKE ORIGIN CLASS 
#nla07_12$WSA9_LO <- with(nla07_12, interaction(ECOREG_use,Lake_Origin_use, sep="_"))
table(nla07_12$WSA9_LO) # BETTER


###########
## ADD VARIABLE (NET ATMOSPHERIC WATER FLUX)
## Difference between cummulative precipitation P_WY (m) for water year 
##    and potential evapotranspiration for water year E (m) 
##    Convert PET to m * 0.001
nla07_12$P_E<- nla07_12$P_WY - nla07_12$E
summary(nla07_12$P_E)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -1.3620 -0.1194  0.1356  0.1439  0.3921  2.8410 

##############
# WRITE Row Bound dataset 12/13/17 - updated 6/7/19 (updated) 8/2/18 (updated E:I) 5/29/18 (Palmer drought); 5/15/18; 5/14/18; 2/9/18 
write.csv(nla07_12, "C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_01JUL19.csv") # OLD 02AUG18 _SINGLE_14MAY18 13DEC17

var_names<-names(nla07_12)
write.csv(var_names,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_VARIABLES_14MAY18.csv")

table(nla07_12$RESAMPLED12)
#    DRP   NO   YES 
#      4 1304  758 



##############
## LOOK AT SIZE DISTRIBUTIONS BETWEEN YEARS
## 3/6/18
##############
table(nla07$SIZE_CLASS)
table(nla12$SIZE_CLASS)

# Create new size classes
# NLA 07
nla07$size_man<-cut(nla07$LkArea_km2, breaks=c(0.04,0.1,0.5,1,5,Inf),labels=c("4-10","10-50","50-100","100-500",">500"))

plot(nla07$LkArea_km~nla07$size_man)
summary(nla07$LkArea_km,nla07$size_man)

# NLA 12
nla12$size_man<-cut(nla12$LkArea_km2, breaks=c(0.01, 0.04,0.1,0.5,1,5,Inf),labels=c("<4","4-10","10-50","50-100","100-500",">500"))
plot(nla12$LkArea_km~nla12$size_man)

table(nla07$size_man)
# 4-10   10-50  50-100 100-500    >500 
# 64     332     219     216     197 

table(nla12$size_man)
#     <4    4-10   10-50  50-100 100-500    >500 
#     87     140     394     130     158     109 



########################################
## DATASET RESAMPLED LAKES IN 2007 & 2012
#  SINGLE OBSERVATIONS (VISIT_NO =1)
# ALTERNATIVE WAY of merging data to get only lakes that were revisited - using Tom's method #########
##        11/16/17
## Using match function to bring together 2007 and 2012 data for lakes sampled both years
table(nla07_12$RESAMPLED12) # This will tell which lakes were resampled both years
#    DRP   NO  YES 
#       4 1304  758  


# SINGLE LAKES - SUBSET the merged single lake datasets into lakes that were resampled in both 2012
repeatdf_07 <- subset(nla07_12, RESAMPLED12=="YES" & YEAR==2007) #366 out of 758
repeatdf_12 <- subset(nla07_12, RESAMPLED12=="YES" & YEAR==2012) # 392 out of 758
repeatdf_07 <- subset(repeatdf_07, SID %in% repeatdf_12$SID) # retain only lakes that match SIDs between years = 348
repeatdf_12 <- subset(repeatdf_12, SID %in% repeatdf_07$SID) # retain only lakes that match SIDs between years = 348
indx<- match(repeatdf_07$SID, repeatdf_12$SID) # Vector of positions of matches of first argument in its second of match based on SID and make same order
repeatdf_12 <- repeatdf_12[indx,]
repeatdf <- droplevels(rbind(repeatdf_07, repeatdf_12)) # 696 with 348 lakes sampled both years

names(repeatdf)
#test07<-subset(nla07,SID %in% nla12$SID) 
#test12<-subset(nla12,SID %in% nla07$SID)
#table(test07$RESAMPLED12) n = 350 but 2 are to be dropped
#table(test12$RESAMPLED12)
# Order factor variables
repeatdf$ECOREG_use<-factor(repeatdf$ECOWSA9_2015, labels=c("CPL","NAP","NPL","SAP","SPL","TPL","UMW","WMT","XER"))
repeatdf$ECOREG_use <- ordered(repeatdf$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","CPL","SAP","NAP"))
repeatdf$YEAR<-factor(repeatdf$YEAR, labels=c("2007","2012")) 

table(repeatdf$YEAR)
# 2007 2012 
#  348  348 


###########
## WRITE rbind RESAMPLED SINGLE NLA07_12 LONG FORMAT DATASET - Added rows - includes revisits during the year
## LAKES that are in BOTH 2007 and 2012
##  Long format - same column names and repeated observations by lake ID
# 1/17/18 - updated using the reduced single visit datasets
# 8/2/18 - updated E:I
# 6/7/19 - updated P-E
# 7/1/19 - updated weights and 12 volume and wrt
# 7/8/19 - updated zmax and wrt for subset of lakes
###########
# SINGLE Long format
#write.csv(nla0712_single_LONG,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
write.csv(repeatdf,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")


############################
###############
## REFORMAT DATA TO BE WIDE - individual columns for 2007 and 2012 lake hydro response
#     for 1:1 PLOTS
###########
############################
# LOAD DATA
## Resampled lakes - long format (row bound)
nla07_12 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
# n = 696 observations (348 lakes resampled)



############
# SUBSET by Year 
nla07 <- subset(nla07_12, YEAR==2007) #348 lakes
nla12 <- subset(nla07_12, YEAR==2012) # 348 lakes

# Reduce number of variables
names(nla07_12)
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","E_I","RT_iso","L_RT_iso","d_excess",
           "HorizDD_use","VertDD_use","L_HorizDD_use","L_VertDD_use",
           "DDHzSqrtA_sc","DDVrtDix_sc_MOD","L_DDHzSqrtA_sc","L_DDVrtDix_sc_MOD",
           "DpthMx_mod","LkArea_km2","Volume_m3",
           "LATdd_use","LONdd_use","Lake_Origin_use","ECOREG_use",
           "Precip_mm_total_yr","temp_degC_summer","Temp_degC_avg_yr","PHDI","E","P_WY","P_E",
           "Drawdown_CONDus15")

nla07_red <-nla07[myvars]

nla12_red <- nla12[myvars]

# Merge two datasets together by SID # n = 348 obs w/53 variables
nla07_12_red <- merge(nla07_red,nla12_red, by="SID")

#indx<- match(nla07_red$SID, nla12_red $SID) # Vector of positions of matches of first argument in its second of match based on SID and make same order
#nla12_red  <- nla12_red [indx,]
#nla07_12_red <- droplevels(rbind(nla07_red, nla12_red)) # 696 with 348 lakes sampled both years

################
## WRITE WIDE-FORMATE RESAMPLED LAKES DATASET
#  Small subset of variables
################
write.csv(nla07_12_red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_01JUL19.csv") # OLD 02AUG18

table(nla12$Lake_Origin_use)

#################
## LIST OF RESAMPLED NLA LAKES 2007+2012 for P group
##  10/29/19
wide<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_01JUL19.csv")
names(wide)
myvars<-c("SITE_ID.x","YEAR.x","SITE_ID.y","YEAR.y","SID","LATdd_use.x", "LONdd_use.x","Lake_Origin_use.x","ECOREG_use.x")
red<-wide[myvars]
summary(red)

## Rename columns in NLA 2007 to match NLA 2012 - "old" to "new"
names(red)[names(red)=="LATdd_use.x"]<-"LAT"
names(red)[names(red)=="LONdd_use.x"]<-"LON"
names(red)[names(red)=="Lake_Origin_use.x"]<-"Lake_Origin_use"
names(red)[names(red)=="ECOREG_use.x"]<-"ECOREG_six"
names(red)[names(red)=="SITE_ID.x"]<-"SITE_ID_07"
names(red)[names(red)=="SITE_ID.y"]<-"SITE_ID_12"
names(red)

write.csv(red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_LAKES.csv" )

#######################
## SUMMARIZE LAKE HYDROLOGY AND CLIMATE FOR RESAMPLED LAKES 
# BY LAKE TYPE AND ECOREGION
nla07_12$eco_lo <-with(nla07_12,interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(nla07_12$eco_lo)

#sub<- group_by(nla07_12, eco_lo)

#Then summarize using grouped data
library(dplyr)
stat<-nla07_12 %>%
  group_by(eco_lo,YEAR) %>%
  summarize(mean_horz=mean(HorizDD_use,na.rm=T), mean_vert=mean(VertDD_use,na.rm=T),mean_ei=mean(E_I,na.rm=T),sd_ei=sd(E_I,na.rm=T))
summarise(sub ,mean_horz=mean(HorizDD_use, na.rm=T), mean_vert=mean(VertDD_use,na.rm=TRUE))

stat<-nla07_12 %>%
  group_by(YEAR) %>%
  summarize(mean_horz=mean(HorizDD_use,na.rm=T), mean_vert=mean(VertDD_use,na.rm=T),mean_ei=mean(E_I,na.rm=T),sd_ei=sd(E_I,na.rm=T))

# CLIMATE
stat_clim<-nla07_12 %>%
  group_by(YEAR) %>%
  summarize(mean_precip=mean(Precip_mm_total_yr,na.rm=T), mean_temp=mean(temp_degC_summer,na.rm=T),mean_phdi=mean(PHDI,na.rm=T),sd_phdi=sd(PHDI,na.rm=T))

# PRISM 30yr avg
stat_prism<-nla07_12 %>%
  group_by(YEAR) %>%
  summarize(mean_precip=mean(Precip_mm_total_yr,na.rm=T), mean_temp=mean(temp_degC_summer,na.rm=T),mean_phdi=mean(PHDI,na.rm=T),sd_phdi=sd(PHDI,na.rm=T))


#################
## Bring together datasets - row bind - *****TRY TOM"S MATCH CODE
## OLD METHOD - 
#   Multiple steps: 1) Make list of SID for 2012
#                   2) Select observations in 2007 that have same SID
#                   3) Reduce number of variables to look at - Keep lake id, location, morpho, DD, E:I, Phys habitat, chemistry, Climate
##################
# Create a vector of 2012 SID
#  1) Remove duplicates for lakes
nla12_single<-nla12_red[!duplicated(nla12_red$SITE_ID),] # Drops to 1015 from 1096
#drop unused levels?
nla12_single<-droplevels(nla12_single) 
#table(nla12_single$TROPHIC_STATE)

#  2) Create vector list of unique SIDs in 2012 dataset
SID_12 <- nla12_single$SID

#  3) Select rows that have matching SID in 2007 dataset
test<-nla07_red[(nla07_red$SID %in% SID_12),] # 366 observations (many are lakes with revisits during the sample year)
# drop unused levels
test<- droplevels(test)

#  4) Repeat - Create vector list of unique SIDs in reduced dataset
test_single <- test[!duplicated(test$SID),] # for matching NLA07 Single observations
test_single<-droplevels(test_single) 
table(test_single$Lake_Origin_use)

SID_07_12 <- test_single$SID

#  5) Select rows that have matching SID in 2012 dataset
# FOR FULL DATASET
test_2 <- nla12_red[(nla12_red$SID %in% SID_07_12),] #372 observations
table(test_2$Lake_Origin_use)
test_2 <- droplevels(test_2)

# Single observations for matching NLA 12
test_2_single <-test_2[!duplicated(test_2$SID),]
test_2_single <-droplevels(test_2_single)
table(test_2_single$Lake_Origin_use)

#  6) Combine datasets using rbind - LONG FORMAT
# Full datasets with revisits
nla07_12<-rbind(test, test_2)
#clim_merge <-merge(clim07, clim12, by=c("ECOREG_use")) 
nla07_12$ECOREG_use<-factor(nla07_12$ECOWSA9_2015, labels=c("CPL","NAP","NPL","SAP","SPL","TPL","UMW","WMT","XER"))
nla07_12$ECOREG_use <- ordered(nla07_12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","CPL","SAP","NAP"))
nla07_12$YEAR<-factor(nla07_12$YEAR, labels=c("2007","2012")) 

names(nla07_12)

cat("Number of lakes sampled in both 2007 & 2012 =", length(unique(nla07_12$SID)))

########################################
######### ALTERNATIVE WAY of merging data to get only lakes that were revisited##########
##        11/16/17
## Using match function to bring together 2007 and 2012 data for lakes sampled both years
table(nla07_12_single$RESAMPLED12)

### SINGLE LAKES ###
# SUBSET the merged single lake datasets to select only lakes that were resampled in 2012
repeatdf_07 <- subset(nla07_12_single, RESAMPLED12=="YES" & YEAR==2007)
repeatdf_12 <- subset(nla07_12_single, RESAMPLED12=="YES" & YEAR==2012)
repeatdf_07 <- subset(repeatdf_07, SID %in% repeatdf_12$SID) # retain only lakes that match SIDs between years
repeatdf_12 <- subset(repeatdf_12, SID %in% repeatdf_07$SID) # retain only lakes that match SIDs between years
indx<- match(repeatdf_07$SID, repeatdf_12$SID) # match based on SID and make same order
repeatdf_12 <- repeatdf_12[indx,]
repeatdf <- droplevels(rbind(repeatdf_07, repeatdf_12)) # 668 with 334 lakes sampled both years

names(repeatdf)

nla12_red_single <- nla12_red[!duplicated(nla12_red$SITE_ID),]
test<-merge(repeatdf,nla12_red_single,by="SID")
names(test)
vol_07<-test[,c("SITE_ID.x","SITE_ID.y","Volume_m3.x","Volume_m3.y","DpthMx_use.x","DpthMx_use.y")]
write.csv(vol_07,"M:/Net MyDocuments/a_Water_Level/Analysis/Stable_isotope_calculations/2012/Processed/NLA07_12_VOLUME_TO_MATCH.csv")

summary(nla12_red$Volume_m3)
plot(vol_07$Volume_m3.x~vol_07$Volume_m3.y)
#######################
# SINGLE datasets with revisits - LONG FORMAT

# Number of lakes sampled in both 2007 & 2012 = 335
#nla0712_single_LONG<-rbind(test_single, test_2_single)
# Order factor variables
repeatdf$ECOREG_use<-factor(repeatdf$ECOWSA9_2015, labels=c("CPL","NAP","NPL","SAP","SPL","TPL","UMW","WMT","XER"))
repeatdf$ECOREG_use <- ordered(repeatdf$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","CPL","SAP","NAP"))
repeatdf$YEAR<-factor(repeatdf$YEAR, labels=c("2007","2012")) 

names(repeatdf)

cat("Number of lakes sampled in both 2007 & 2012 =", length(unique(nla0712_single_LONG$SID)))
# Number of lakes sampled in both 2007 & 2012 = 335

table(repeatdf$YEAR)

###########
## WRITE rbind NLA07_12 DATASET - Added rows - includes revisits during the year
## LAKES that are in BOTH 2007 and 2012
##  Long format - same column names and repeated observations by lake ID
###########
#write.csv(nla07_12, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS.csv")

#nla07_12_long<- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS.csv")

# SINGLE Long format
#write.csv(nla0712_single_LONG,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
#write.csv(repeatdf,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")

###########
## CREATE merged dataset (WIDE format) with ONLY Single visits - so each lake has two measurement - one for 2007 and one for 2012
###########
test_merge <- merge(test_single, test_2_single, by="SID")
names(test_merge)
# NOTE "var.x" = 2007 and "var.y" =2012

write.csv(test_merge, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_cbind_WIDE_REVISITS_SINGLE.csv")

###########################
## Read in Merged Dataset  - for Boxplots
###########################

nla0712_merge <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_cbind_WIDE_REVISITS_SINGLE.csv")
names(nla0712_merge)

## CLEAN UP ##
#Dropping variables
todrop<-names(nla0712_merge)%in%c("X","STATE.y","URBAN.y","Lake_Origin_use.y","ECOWSA9_2015.y","WSA9_LO.y")
nla0712_merge<-nla0712_merge[!todrop]

names(nla0712_merge)

#Rename some columns
names(nla0712_merge)[names(nla0712_merge)=="STATE.x"] <- "STATE"
names(nla0712_merge)[names(nla0712_merge)=="URBAN.x"] <- "URBAN"
names(nla0712_merge)[names(nla0712_merge)=="Lake_Origin_use.x"] <- "Lake_Origin_use"
names(nla0712_merge)[names(nla0712_merge)=="ECOWSA9_2015.x"] <- "ECOWSA9_2015"
names(nla0712_merge)[names(nla0712_merge)=="WSA9_LO.x"] <-"WSA9_LO"

# Ecoregion groups so that they are plotted from West to East to match the map
## UPDATED 8/4/17 - changed order of CPL to be most east following feedback
nla0712_merge$ECOREG_use <- ordered(nla0712_merge$ECOWSA9_2015, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla0712_merge$ECOREG_use)

str(nla0712_merge$HYDRO_TYPE_f.x)
# Make into factors
nla0712_merge$HYDRO_TYPE_f.x<-factor(nla0712_merge$HYDRO_TYPE_f.x)
nla0712_merge$HYDRO_TYPE_f.y<-factor(nla0712_merge$HYDRO_TYPE_f.y)
nla0712_merge$lk_hydro_iso.x<-factor(nla0712_merge$lk_hydro_iso.x)
nla0712_merge$lk_hydro_iso.y<-factor(nla0712_merge$lk_hydro_iso.y)


nla0712_merge$HYDRO_TYPE_f.x <- ordered(nla0712_merge$HYDRO_TYPE_f.x, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))
nla0712_merge$HYDRO_TYPE_f.y <- ordered(nla0712_merge$HYDRO_TYPE_f.y, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))

nla0712_merge$lk_hydro_iso.x <- ordered(nla0712_merge$lk_hydro_iso.x, levels=c("Closed","Restricted","Flow_through"))
nla0712_merge$lk_hydro_iso.y <- ordered(nla0712_merge$lk_hydro_iso.y, levels=c("Closed","Restricted","Flow_through"))

table(nla0712_merge$lk_hydro_iso.x)

#################
## Calculate differences between 2012 and 2007
#################
# Take 2012 value (.y) minus 2007 value (.x) - so zero indicates that 2012 was no different from 2007

# Horizontal drawdown variables
nla0712_merge$HorizDD_DIFF <- nla0712_merge$HorizDD_use.y - nla0712_merge$HorizDD_use.x
nla0712_merge$L_HorizDD_DIFF <- nla0712_merge$L_HorizDD_use.y - nla0712_merge$L_HorizDD_use.x
nla0712_merge$DDHzSqrtA_sc_DIFF <- nla0712_merge$DDHzSqrtA_sc.y - nla0712_merge$DDHzSqrtA_sc.x
nla0712_merge$L_DDHzSqrtA_sc_DIFF <- nla0712_merge$L_DDHzSqrtA_sc.y - nla0712_merge$L_DDHzSqrtA_sc.x

# Vertical drawdown variables
nla0712_merge$VertDD_DIFF <- nla0712_merge$VertDD_use.y - nla0712_merge$VertDD_use.x
nla0712_merge$L_VertDD_DIFF <- nla0712_merge$L_VertDD_use.y - nla0712_merge$L_VertDD_use.x
nla0712_merge$DDVrtDix_sc_DIFF <- nla0712_merge$DDVrtDix_sc.y - nla0712_merge$DDVrtDix_sc.x
nla0712_merge$L_DDVrtDix_sc_DIFF <- nla0712_merge$L_DDVrtDix_sc.y - nla0712_merge$L_DDVrtDix_sc.x

# E:I
nla0712_merge$E_I_DIFF <- nla0712_merge$E_I.y - nla0712_merge$E_I.x

# CLIMATE
nla0712_merge$Temp_degC_avg_yr_DIFF <- nla0712_merge$Temp_degC_avg_yr.y - nla0712_merge$Temp_degC_avg_yr.x
nla0712_merge$Precip_mm_avg_yr_DIFF <- nla0712_merge$Precip_mm_avg_yr.y - nla0712_merge$Precip_mm_avg_yr.x
nla0712_merge$Precip_mm_total_yr_DIFF <- nla0712_merge$Precip_mm_total_yr.y -nla0712_merge$Precip_mm_total_yr.x
nla0712_merge$EVAP_DIFF <- nla0712_merge$E.y - nla0712_merge$E.x

nla0712_merge$temp_degC_winter_DIFF <- nla0712_merge$temp_degC_winter.y - nla0712_merge$temp_degC_winter.x
nla0712_merge$temp_degC_spring_DIFF <- nla0712_merge$temp_degC_spring.y - nla0712_merge$temp_degC_spring.x
nla0712_merge$temp_degC_summer_DIFF <- nla0712_merge$temp_degC_summer.y - nla0712_merge$temp_degC_summer.x
nla0712_merge$precip_mm_winter_DIFF <- nla0712_merge$precip_mm_winter.y - nla0712_merge$precip_mm_winter.x
nla0712_merge$precip_mm_spring_DIFF <- nla0712_merge$precip_mm_spring.y - nla0712_merge$precip_mm_spring.x
nla0712_merge$precip_mm_summer_DIFF <- nla0712_merge$precip_mm_summer.y - nla0712_merge$precip_mm_summer.x

# Volume m3
nla0712_merge$Volume_m3_DIFF <- nla0712_merge$Volume_m3.y - nla0712_merge$Volume_m3.x

####################
## WRITE dataset with differences between years - WIDE dataset
####################
write.csv(nla0712_merge,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_merge_for_deviations_USE.csv")

# WRite out variable names
test<-as.data.frame(names(nla0712_merge))

write.csv(test,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_merge_Var_Names.csv")

#################
## READ modified dataset - WIDE form with differences calculated between years 
#################
nla0712_merge<- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_merge_for_deviations_USE.csv")

names(nla0712_merge)

## CLEAN UP ##
#Dropping variables
todrop<-names(nla0712_merge)%in%c("X")
nla0712_merge<-nla0712_merge[!todrop]

# REORDER factors for boxplots
# Ecoregion groups so that they are plotted from West to East to match the map
nla0712_merge$ECOREG_use <- ordered(nla0712_merge$ECOWSA9_2015, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla0712_merge$ECOREG_use)

nla0712_merge$HYDRO_TYPE_f.x <- ordered(nla0712_merge$HYDRO_TYPE_f.x, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))
nla0712_merge$HYDRO_TYPE_f.y <- ordered(nla0712_merge$HYDRO_TYPE_f.y, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))

nla0712_merge$lk_hydro_iso.x <- ordered(nla0712_merge$lk_hydro_iso.x, levels=c("Closed","Restricted","Flow_through"))
nla0712_merge$lk_hydro_iso.y <- ordered(nla0712_merge$lk_hydro_iso.y, levels=c("Closed","Restricted","Flow_through"))


##############################
##############
## Boxplots of differences
##############
##############################
################################################
# Function to plot multiple plots in ggplot
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot<- function(...,plotlist=NULL, file, cols=1, layout=NULL){
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots<-c(list(...), plotlist)
  numPlots = length(plots)
  
  #If layout is NULL, then use'cols' to determine layout
  if(is.null(layout)){
    # Make the panel
    #ncol: Number of columns of plots
    #nrow: Number of rows needed, calculated from # of cols
    layout<- matrix(seq(1, cols* ceiling(numPlots/cols)),
                    ncol=cols, nrow = ceiling(numPlots/cols))
  }
  if(numPlots==1){
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow(layout), ncol(layout))))
    
    #Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

################
## Plot specifications
################
# Color gradient for regions
color_region<-c("#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e")
#names(color_region) <- levels(L_Horiz$ECOREG_use)
names(color_region) <- levels(nla0712_merge$ECOREG_use)

# Reduced color pallete dropping SAP for natural lakes
color_region_red<-c("#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#35978f","#01665e") # SAP=,"#80cdc1"
#names(color_region_red) <- levels(L_Horiz$ECOREG_use)

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# SET WORKING DIRECTORY FOR OUTPUT
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput")

###########
## Horizontal Drawdown variables
###########

# Log 10 TRANSFORMED Horizontal Drawdown - NOTE - 
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), 
                        aes(x=factor(ECOREG_use),y=L_HorizDD_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-2.7,2.7), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Log 10 Horizontal Drawdown")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta," Log10 Horizontal Drawdown")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=L_HorizDD_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-2.7,2.7), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)


# Horizontal (log10) DD NLA12
# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="HorizDD_log10_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


# SCALED Horizontal Drawdown by Lake Area- 
# 10/19/17 NOTE - Renee - why are all values below zero but when not scaled there are some regions that show increased DD in 2012?
plot(nla0712_merge$L_HorizDD_DIFF~nla0712_merge$L_LkAreakm2.x)
abline(lm(nla0712_merge$L_HorizDD_DIFF~nla0712_merge$L_LkAreakm2.x))

regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
                        aes(x=factor(ECOREG_use),y=L_DDHzSqrtA_sc_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-4,1), breaks=c(-4,-2,0,1)) +
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Log 10 Horizontal Drawdown")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta,
                                                  " Log10 SCALED Horizontal Drawdown")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=L_DDHzSqrtA_sc_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-4,1), breaks=c(-4,-2,0,1)) +
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="HorizDD_log10_SCALED_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

## UNTRANSFORMED Drawdown - NOTE - cannot log transform y axis because have many negative values
#nla0712_merge$HorizDD_DIF_n <- nla0712_merge$HorizDD_DIF +0.001

# 10/19/17 reduced range on axes to zoom in
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), aes(x=factor(ECOREG_use),y=HorizDD_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-100,100), breaks=c(-100,-50,0,50,100)) + #ALL DATA limits=c(-100,200), breaks=c(-400,-200,0,200,400)
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal Drawdown (m)")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta," Horizontal Drawdown (m)")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=HorizDD_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-100,100), breaks=c(-100,-50,0,50,100)) + #ALL DATA displayed limits=c(-100,200), breaks=c(-400,-200,0,200,400)
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="HorizDD_DIFFERENCE_zoom.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


###########
## Vertical Drawdown variables
###########

# Log 10 TRANSFORMED Vertical Drawdown - NOTE - 
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), aes(x=factor(ECOREG_use),y=L_VertDD_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-2,2), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Log 10 Vertical Drawdown")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta,
                                                  " Log10 Vertical Drawdown")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=L_VertDD_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-2,2), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="VertDD_log10_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

# SCALED Log 10 TRANSFORMED Vertical Drawdown - NOTE - 
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), aes(x=factor(ECOREG_use),y=L_DDVrtDix_sc_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-2.5,2.5), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Log 10 scaled Vertical Drawdown")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta,
                                                  " Log10 scaled Vertical Drawdown")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=L_DDVrtDix_sc_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-2.5,2.5), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="VertDD_log10_SCALED_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

# UNTRANSFORMED Vertical Drawdown - NOTE - 
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), aes(x=factor(ECOREG_use),y=VertDD_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-5,5), breaks=c(-5,-2.5,0,2.5,5)) + # All Data displayed limits=c(-25,15), breaks=c(-20,-10,0,10)
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical Drawdown")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta,
                                                  " Vertical Drawdown (m)")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=VertDD_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-5,5), breaks=c(-5,-2.5,0,2.5,5)) + # ALL Data limits=c(-25,15), breaks=c(-20,-10,0,10)
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="VertDD_DIFFERENCE_zoom.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

###############
## Evaporation:Inflow
###############
# E:I - NOTE - 
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), aes(x=factor(ECOREG_use),y=E_I_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-0.7,1), breaks=c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75)) +
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Evaporation:Inflow")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta,
                                                  " Evaporation:Inflow")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=E_I_DIFF)) +geom_boxplot(fill=color_region_red) + 
  scale_y_continuous(limits=c(-0.7,1), breaks=c(-0.75,-0.5,-0.25,0,0.25,0.5,0.75)) +
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="E_I_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

###########
## Climate differences between years
# 3/14/18
###########
names(nla0712_merge)

# Summary of difference in 2012 total precip from 2007
summary(nla0712_merge$Precip_mm_total_yr_DIFF)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-678.70  -89.87   33.77   42.37  175.10  837.10 
sd(nla0712_merge$Precip_mm_total_yr_DIFF)

# Summary of difference in 2012 summer temperature
summary(nla0712_merge$temp_degC_summer_DIFF)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-5.303  -2.450  -1.703  -1.861  -1.166   1.393 
sd(nla0712_merge$temp_degC_summer_DIFF)

# Summary od difference in 2012 PET
summary(nla0712_merge$EVAP_DIFF)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.02903  0.06254  0.08222  0.08045  0.10000  0.19060 
sd(nla0712_merge$EVAP_DIFF)

# BY ECOREGION
library(dplyr)
eco<- group_by(nla0712_merge, ECOREG_use)
summarise(eco, 
          mean_precip_diff=mean(Precip_mm_total_yr_DIFF), 
          mean_summer_temp_diff = mean(temp_degC_summer_DIFF),
          mean_PET_diff = mean(EVAP_DIFF))







##################################################################
###########
## Volume differences - 10/25/17 - BUT need to see if we need to estimate 2012 again using different methods
###########

# Log10 transform volume
nla0712_merge$Volume_km3_DIFF <- nla0712_merge$Volume_m3_DIFF * 0.000000001

summary(nla0712_merge$Volume_km3_DIFF)

# Volume difference between years
regionbox_mm_a<- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"), 
                        aes(x=factor(ECOREG_use),y=Volume_km3_DIFF)) +geom_boxplot(fill=color_region) + 
  #scale_y_continuous(limits=c(-.6,0.001))+#, breaks=c(-2,-1,0,1,2)) +
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Volume km3")+
  xlab(NULL)

regionbox_mm=regionbox_mm_a+ylab(expression(paste(Delta," Volume km3")))

regionbox_nat<- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use),y=Volume_km3_DIFF)) +geom_boxplot(fill=color_region_red) + 
  #scale_y_continuous(limits=c(-.6,0.001), breaks=c(-2,-1,0,1,2)) +
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox_mm, regionbox_nat, cols=2)


# Horizontal (log10) DD NLA12
# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="Volume_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

##############################################
###############
## Climate variables - just by region
###############
##############################################
# Temperature - 
regionbox_a<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=Temp_degC_avg_yr_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-6,4), breaks=c(-5,-2,0,2)) +
  ggtitle("Annual Temperature")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Mean Annual Temp (~degree~C)")+
  xlab(NULL)

regionbox=regionbox_a+ylab(expression(paste(Delta,"Mean Temp.  ",degree~C)))

# Winter
regionbox_b<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=temp_degC_winter_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-6,4), breaks=c(-5,-2,0,2)) +
  ggtitle("Winter")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Mean Annual Temp (~degree~C)")+
  xlab(NULL)

regionbox_b=regionbox_b+ylab(expression(paste(Delta,"Mean Temp.  ",degree~C)))

# Spring
regionbox_c<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=temp_degC_spring_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-6,4), breaks=c(-5,-2,0,2)) +
  ggtitle("Spring")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

#regionbox_c=regionbox_c+ylab(expression(paste(Delta,"Mean Spring Temp.  ",degree~C)))

# Summer
regionbox_d<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=temp_degC_summer_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-6,4), breaks=c(-5,-2,0,2)) +
  ggtitle("Summer")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

#regionbox_d=regionbox_d+ylab(expression(paste(Delta,"Mean Summer Temp.  ",degree~C)))

multiplot(regionbox, regionbox_b,regionbox_c, regionbox_d, cols=2)

# 
a<-viewport(width=unit(6, "inch"), height=unit(6,"inch"), x=0.5)
png(filename="Temperature_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox, regionbox_b,regionbox_c, regionbox_d, cols=2), vp=a)
dev.off()


### PRECIPITATION ##
# MEAN ANNUAL
regionbox_a<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=Precip_mm_avg_yr_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-65,60), breaks=c(-50,-25,0,25,50)) +
  ggtitle("Mean Annual Precipitation")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Mean Precip (mm)")+
  xlab(NULL)

regionbox=regionbox_a+ylab(expression(paste(Delta,"Mean Annual Precip (mm)")))

# WINTER
regionbox_b<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=precip_mm_winter_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-140,100), breaks=c(-100,-50,-25,0,25,50,100)) +
  ggtitle("Winter")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Mean Annual Precip (mm)")+
  xlab(NULL)

regionbox_b=regionbox_b+ylab(expression(paste(Delta,"Mean Precip (mm)")))

# SPRING
regionbox_c<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=precip_mm_spring_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-110,210), breaks=c(-100,-50,0,50,100,150,200)) +
  ggtitle("Spring")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

# SUMMER
regionbox_d<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=precip_mm_summer_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-180,210), breaks=c(-150,-100,-50,0,50,100,150,200)) +
  ggtitle("Summer")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab(NULL)

multiplot(regionbox, regionbox_b,regionbox_c, regionbox_d, cols=2)

# 
a<-viewport(width=unit(6, "inch"), height=unit(6,"inch"), x=0.5)
png(filename="Precipitation_DIFFERENCE.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox, regionbox_b,regionbox_c, regionbox_d, cols=2), vp=a)
dev.off()

## Cumulative water year precip
regionbox_tot<- ggplot(nla0712_merge, aes(x=factor(ECOREG_use),y=Precip_mm_total_yr_DIFF)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-680,840), breaks=c(-500,-250,0,250,500,750)) +
  ggtitle("Cumulative Annual Precipitation")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Total Precip (mm)")+
  xlab(NULL)

regionbox_tot=regionbox_tot+ylab(expression(paste(Delta,"Precip (mm)")))

a<-viewport(width=unit(6, "inch"), height=unit(6,"inch"), x=0.5)
png(filename="Precipitation_Cumulative_DIFFERENCE.png", width=7.5, height=6.5,units="in", res = 600)
print(regionbox_tot, vp=a)
dev.off()


## Evaporation (E)
# Change too mm rather than meters
nla0712_merge$EVAP_DIFF_mm <- nla0712_merge$EVAP_DIFF*1000

regionbox_evap<- ggplot(nla0712_merge, 
                       aes(x=factor(ECOREG_use),y=EVAP_DIFF_mm)) +geom_boxplot(fill=color_region) + 
  scale_y_continuous(limits=c(-30,200), breaks=c(-25,0,25,50,75,100,125,150,175,200)) +
  ggtitle("Annual Potential Evaportranspiration")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Evaporation (mm)")+
  xlab(NULL)

regionbox_evap=regionbox_evap+ylab(expression(paste(Delta,"Potential Evapotranspiration (mm)")))

a<-viewport(width=unit(6, "inch"), height=unit(6,"inch"), x=0.5)
png(filename="Evaporation_CLIMATE_DIFFERENCE.png", width=7.5, height=6.5,units="in", res = 600)
print(regionbox_evap, vp=a)
dev.off()


#############################
#############################
### XY Plots
## 10/17/17
#############################
#############################

#### Change in Horizontal DD variables vs. Change in E:I between 2012 & 2007
# Horizontal DD (m) (untransformed)
# *** NOTE ***  Cut off outliers from the graph by setting the axes limits
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=E_I_DIFF,y=HorizDD_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-100,100)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal DD (m)")+
  xlab("E:I")

regionbox_mm=g+xlab(expression(paste(Delta," Evaporation:Inflow"))) +
  ylab(expression(paste(Delta, " Horizontal drawdown (m)")))

g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=E_I_DIFF,y=HorizDD_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-100,100)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+ #"Horizontal DD (m)"
  xlab("E:I")

regionbox_nat=g+xlab(expression(paste(Delta," Evaporation:Inflow")))# +
  #ylab(expression(paste(Delta, " Horizontal drawdown (m)")))

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_horizdd_EI.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

# TRIED facet_wrap to separate by lake type - but not working...
#g <- ggplot(nla0712_merge,
#            aes(x=E_I_DIFF,y=HorizDD_DIFF,color=ECOREG_use)) +
#  #scale_y_continuous(limits=c(-100,100)) +
#  geom_point(shape=1) +
#  scale_color_manual(values=color_region,guide=FALSE)+
#  geom_smooth(method=lm, se=FALSE)+
#  theme_bw(base_size=14) + 
#  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
#        axis.text.x = element_text(family = "RMN"),
#        axis.text.y = element_text(family = "RMN"),
#        axis.title.y=element_text(family="RMN"),
#        axis.title.x=element_text(family="RMN"),
#        panel.grid.major =  element_line(colour = NA), 
#        panel.grid.minor=element_line(colour = NA),
#        panel.spacing = unit(c(1,1,0,4), "lines"))+
#  ylab("Horizontal DD (m)")+
#  xlab("E:I")

#regionbox=g+
#  facet_wrap(~ factor(Lake_Origin_use))+
#  xlab(expression(paste(Delta," Evaporation:Inflow"))) +
#  ylab(expression(paste(Delta, " Horizontal drawdown (m)")))

## Scaled HorizDD - SqrtA
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=E_I_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal DD (SCALED)")+
  xlab("E:I")

regionbox_mm=g+xlab(expression(paste(Delta," Evaporation:Inflow"))) +
  ylab(expression(paste(Delta, " Horizontal drawdown scaled by area")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=E_I_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("E:I")

regionbox_nat=g+xlab(expression(paste(Delta," Evaporation:Inflow")))# +
  #ylab(expression(paste(Delta, " Horizontal drawdown scaled by area")))

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_horizdd_SCALED_EI.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


#############
#### Change in Vertical DD variables vs. Change in E:I between 2012 & 2007
# Vertical DD (m) (untransformed)
# *** NOTE ***  Cut off outliers from the graph by setting the axes limits
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=E_I_DIFF,y=VertDD_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-25,25)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical DD (m)")+
  xlab("E:I")

regionbox_mm=g+xlab(expression(paste(Delta," Evaporation:Inflow"))) +
  ylab(expression(paste(Delta, " Vertical drawdown (m)")))

g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=E_I_DIFF,y=VertDD_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-25,25)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+ #"Vertical DD (m)"
  xlab("E:I")

regionbox_nat=g+xlab(expression(paste(Delta," Evaporation:Inflow")))# +
#ylab(expression(paste(Delta, " Horizontal drawdown (m)")))

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_vertdd_EI.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


## VERTICAL DD (SCALED)
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=E_I_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical DD scaled by depth")+
  xlab("E:I")

regionbox_mm=g+xlab(expression(paste(Delta," Evaporation:Inflow"))) +
  ylab(expression(paste(Delta, " Vertical drawdown scaled by depth")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=E_I_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+ #"Vertical DD scaled by depth"
  xlab("E:I")

regionbox_nat=g+xlab(expression(paste(Delta," Evaporation:Inflow")))# +
#ylab(expression(paste(Delta, " Horizontal drawdown (m)")))

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_vertdd_SCALED_EI.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

####################
## Drawdown vs. Climate variables
###################
#Exploratory scatterplots 
pairs(~nla0712_merge$L_DDHzSqrtA_sc_DIFF + nla0712_merge$L_DDVrtDix_sc_DIFF+ nla0712_merge$E_I_DIFF+ nla0712_merge$Temp_degC_avg_yr_DIFF +
        nla0712_merge$temp_degC_winter_DIFF + nla0712_merge$temp_degC_spring_DIFF + 
        nla0712_merge$temp_degC_summer_DIFF)

pairs(~nla0712_merge$L_DDHzSqrtA_sc_DIFF + nla0712_merge$L_DDVrtDix_sc_DIFF+nla0712_merge$E_I_DIFF+
        nla0712_merge$Precip_mm_total_yr_DIFF + nla0712_merge$Precip_mm_avg_yr_DIFF +
        nla0712_merge$precip_mm_winter_DIFF + nla0712_merge$precip_mm_spring_DIFF + nla0712_merge$precip_mm_summer_DIFF)


########
## Scaled HorizDD - SqrtA vs. Summer temperature
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=temp_degC_summer_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-4.1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal DD (SCALED)")+
  xlab("Summer Temperature")

regionbox_mm=g+xlab(expression(paste(Delta," Summer Temp.",degree~C))) +
  ylab(expression(paste(Delta, " Horizontal drawdown scaled by area")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=temp_degC_summer_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-4.1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Summer Temperature")

regionbox_nat=g+xlab(expression(paste(Delta," Summer Temp.",degree~C)))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_horizdd_SCALED_Temp_SUMMER.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


###########
## Scaled HorizDD - SqrtA vs. Summer PRECIPITATION
###########
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=precip_mm_summer_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-4.1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal DD (SCALED)")+
  xlab("Summer Precipitation")

regionbox_mm=g+xlab(expression(paste(Delta," Summer Precipitation (mm)"))) +
  ylab(expression(paste(Delta, " Horizontal drawdown scaled by area")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=precip_mm_summer_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-4.1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Summer Temperature")

regionbox_nat=g+xlab(expression(paste(Delta," Summer Precipitation (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_horizdd_SCALED_Precip_SUMMER.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


###########
## Scaled HorizDD - SqrtA vs. Difference in mean annual PET
###########
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=EVAP_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-4.1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal DD (SCALED)")+
  xlab("PET")

regionbox_mm=g+xlab(expression(paste(Delta," Mean PET (mm)"))) +
  ylab(expression(paste(Delta, " Horizontal drawdown scaled by area")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=EVAP_DIFF,y=L_DDHzSqrtA_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-4.1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("PET")

regionbox_nat=g+xlab(expression(paste(Delta," Mean PET (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_horizdd_SCALED_PET.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

########
#### CHECKING OUT VERTICAL DD vs CLIMATE VARIABLES #####
########
## Scaled VERTDD - SqrtA vs. Summer temperature
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=temp_degC_summer_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical DD (SCALED)")+
  xlab("Summer Temperature")

regionbox_mm=g+xlab(expression(paste(Delta," Summer Temp.",degree~C))) +
  ylab(expression(paste(Delta, " Vertical drawdown scaled by depth")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=temp_degC_summer_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Summer Temperature")

regionbox_nat=g+xlab(expression(paste(Delta," Summer Temp.",degree~C)))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_vertdd_SCALED_Temp_SUMMER.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()



## Scaled VERTDD - SqrtA vs. Summer precip
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=precip_mm_summer_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical DD (SCALED)")+
  xlab("Summer Precip")

regionbox_mm=g+xlab(expression(paste(Delta," Summer Precip (mm)"))) +
  ylab(expression(paste(Delta, " Vertical drawdown scaled by depth")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=precip_mm_summer_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Summer Precipitation (mm)")

regionbox_nat=g+xlab(expression(paste(Delta," Summer Precip (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_vertdd_SCALED_Precip_SUMMER.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

###########
## Scaled VERTDD - SqrtA vs. Cumulative precipitation
###########
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=Precip_mm_total_yr_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical DD (SCALED)")+
  xlab("Total Precip")

regionbox_mm=g+xlab(expression(paste(Delta," Total Precip (mm)"))) +
  ylab(expression(paste(Delta, " Vertical drawdown scaled by depth")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=Precip_mm_total_yr_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Total Precipitation (mm)")

regionbox_nat=g+xlab(expression(paste(Delta," Total Precip (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_vertdd_SCALED_Precip_TOTAL.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

###########
## Scaled VERTDD - SqrtA vs. PET (mm)
###########
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=EVAP_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical DD (SCALED)")+
  xlab("PET")

regionbox_mm=g+xlab(expression(paste(Delta," Mean PET (mm)"))) +
  ylab(expression(paste(Delta, " Vertical drawdown scaled by depth")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=EVAP_DIFF,y=L_DDVrtDix_sc_DIFF,color=ECOREG_use)) +
  scale_y_continuous(limits=c(-2.5,2.5)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("PET")

regionbox_nat=g+xlab(expression(paste(Delta," Mean PET (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_vertdd_SCALED_PET.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


###########
## Evaporation:Inflow vs. Summer temperature
###########
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=temp_degC_summer_DIFF,y=E_I_DIFF,color=ECOREG_use)) +
  #scale_y_continuous(limits=c(-1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("E:I")+
  xlab("Summer Temperature")

regionbox_mm=g+xlab(expression(paste(Delta," Summer Temp.",degree~C))) +
  ylab(expression(paste(Delta, " E:I")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=temp_degC_summer_DIFF,y=E_I_DIFF,color=ECOREG_use)) +
  #scale_y_continuous(limits=c(-1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Summer Temperature")

regionbox_nat=g+xlab(expression(paste(Delta," Summer Temp.",degree~C)))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_E_I_Temp_SUMMER.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


## E:I vs. Total Precip
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=Precip_mm_total_yr_DIFF,y=E_I_DIFF,color=ECOREG_use)) +
  #scale_y_continuous(limits=c(-1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("E:I")+
  xlab("Total precip")

regionbox_mm=g+xlab(expression(paste(Delta," Total Precip (mm)"))) +
  ylab(expression(paste(Delta, " E:I")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=Precip_mm_total_yr_DIFF,y=E_I_DIFF,color=ECOREG_use)) +
  #scale_y_continuous(limits=c(-1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("Total Precip")

regionbox_nat=g+xlab(expression(paste(Delta," Total Precip (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_E_I_TOTAL_Precip.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

##########
## E:I vs PET
g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="MAN-MADE"),
            aes(x=EVAP_DIFF,y=E_I_DIFF,color=ECOREG_use)) +
  #scale_y_continuous(limits=c(-1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("E:I")+
  xlab("PET")

regionbox_mm=g+xlab(expression(paste(Delta," Mean PET (mm)"))) +
  ylab(expression(paste(Delta, " E:I")))


g <- ggplot(subset(nla0712_merge,Lake_Origin_use=="NATURAL"),
            aes(x=EVAP_DIFF,y=E_I_DIFF,color=ECOREG_use)) +
  #scale_y_continuous(limits=c(-1,1)) +
  geom_point(shape=1) +
  scale_color_manual(values=color_region_red,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+
  xlab("PET")

regionbox_nat=g+xlab(expression(paste(Delta," Mean PET (mm)")))# +

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_E_I_PET.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()


#############
## Histograms of d-excess between years
#############
# Load Long format dataset with all observations including revisits
nla07_12<- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_OBS.csv")

names(nla07_12)
table(nla07_12$SIZE_CLASS)

# Make Year a factor
nla07_12$YEAR<-factor(nla07_12$YEAR, labels=c("2007","2012"))

# Drop small lakes from 2012 dataset to make more comparable with 2007 sample design
test<-nla07_12[which(nla07_12$YEAR=='2012' & nla07_12$LkArea_km2<0.04),] # 91 lakes are smaller than 4 ha

# Select observations that are equal to or larger than 4 ha
nla07_12 <- nla07_12[which(nla07_12$LkArea_km2>=0.04),]

## CLEAN UP ##
#Dropping variables
todrop<-names(nla07_12)%in%c("X")
nla07_12<-nla07_12[!todrop]

# REORDER factors for boxplots
# Ecoregion groups so that they are plotted from West to East to match the map
nla07_12$ECOREG_use <- ordered(nla07_12$ECOWSA9_2015, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla07_12$ECOREG_use)

nla07_12$HYDRO_TYPE_f <- ordered(nla07_12$HYDRO_TYPE_f, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))
nla07_12$HYDRO_TYPE_f <- ordered(nla07_12$HYDRO_TYPE_f, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))

nla07_12$lk_hydro_iso <- ordered(nla07_12$lk_hydro_iso, levels=c("Closed","Restricted","Flow_through"))
nla07_12$lk_hydro_iso <- ordered(nla07_12$lk_hydro_iso, levels=c("Closed","Restricted","Flow_through"))

###########
# Overlapping histogram
##########
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# SET WORKING DIRECTORY FOR OUTPUT
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput")

# http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
hist=ggplot(nla07_12, aes(x=d_excess, fill=YEAR)) +
  geom_histogram(binwidth=5, alpha=0.5, position="identity")+
  theme_bw(base_size=14)+
  theme(plot.title=element_text(family="RMN", face="plain", size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines")) +
  ylab("Frequency")+
  xlab("d-excess")

# Overlaping density plot
density=ggplot(nla07_12, aes(x=d_excess, fill=YEAR))+
  geom_density(alpha=.3)+
  theme_bw(base_size=14)+
  theme(plot.title=element_text(family="RMN", face="plain", size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines")) +
  ylab("Density")+
  xlab("d-excess")

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
tiff(filename="Hist_dexcess_YEAR_OVERLAP.tiff", width=6.5, height=5,units="in", res = 600)
print(hist, vp=a)
dev.off()

a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
tiff(filename="Hist_dexcess_YEAR_DENSITY_OVERLAP.tiff", width=6.5, height=5,units="in", res = 600)
print(density, vp=a)
dev.off()

##############
###########
## Exploratory graph to compare with L&O 2014 paper using  NLA2007
##  2012 TN vs. E:I - use the dataset with lakes <4ha dropped
##########
##############
nla12<- nla07_12[which(nla07_12$YEAR=="2012"),]
## NOTE there are five lakes with negative E:I values - which is not possible
# I can't remember why we were unable to get an E:I estimated for these lakes - going to drop them
#test<-nla12[which(nla12$E_I<0),]

# Also TN range in 2012 (size adjusted to drop really small lakes) 
#   is really small compared to 2007 data 
#   The range is the same even when we bring back the small lakes in the dataset
tapply(nla07_12$NTL, nla07_12$YEAR, summary)
# $`2007`
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   5.0   314.0   583.5  1208.0  1188.0 26100.0 

#$`2012`
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.014   0.325   0.640   1.199   1.271  54.000 

# Convert TN from mg/L to ug/L
#nla12$NTL_ug <-nla12$NTL *1000
# Create reduced dataset n=1000 observations
test<-nla12[which(nla12$E_I<0),]

nla12<-nla12[which(nla12$E_I>=0),]
nla12<-droplevels(nla12)

## Scatterplot
g <- ggplot(subset(nla12,Lake_Origin_use=="MAN-MADE"),
            aes(x=E_I,y=NTL_ug,color=ECOREG_use)) +
  scale_y_continuous(trans="log10",limits=c(NA,60000), breaks=c(30,100,300,1000,3000,10000)) +
  scale_x_continuous(limits=c(0,1.0))+
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Man-made")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("TN ug/L")+
  xlab("E:I")

regionbox_mm=g+xlab(expression(paste(" Evaporation:Inflow"))) +
  ylab(expression(paste("Total Nitrogen (ug/L)")))

g <- ggplot(subset(nla12,Lake_Origin_use=="NATURAL"),
            aes(x=E_I,y=NTL_ug,color=ECOREG_use)) +
  scale_y_continuous(trans="log10",limits=c(NA,60000),breaks=c(30,100,300,1000,3000,10000)) +
  scale_x_continuous(limits=c(0,1.0))+
  geom_point(shape=1) +
  scale_color_manual(values=color_region,guide=FALSE)+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Natural")+
  theme_bw(base_size=14) + 
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab(NULL)+ #"Horizontal DD (m)"
  xlab("E:I")

regionbox_nat=g+xlab(expression(paste(" Evaporation:Inflow")))# +
#ylab(expression(paste(Delta, " Horizontal drawdown (m)")))

multiplot(regionbox_mm, regionbox_nat, cols=2)

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="xyplot_2012_only_TN_EI.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(regionbox_mm, regionbox_nat, cols=2), vp=a)
dev.off()

#############
## VOLUME comparisons between years
#############
nla07_12$vol_km3 <- nla07_12$Volume_m3*0.00000001
nla07_12$L_vol_km3 <-log10(nla07_12$vol_km3) 
nla07_12$L_Volume_m3<- log10(nla07_12$Volume_m3)

hist_vol=ggplot(nla07_12, aes(x=L_Volume_m3, fill=YEAR)) +
  geom_histogram(binwidth=0.1, alpha=0.5, position="identity")+
  theme_bw(base_size=14)+
  theme(plot.title=element_text(family="RMN", face="plain", size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines")) +
  ylab("Frequency")+
  xlab("Volume (log 10 m^3)")

# Max depth
hist_z=ggplot(nla07_12, aes(x=L_DpthMx_use, fill=YEAR)) +
  geom_histogram(binwidth=0.1, alpha=0.5, position="identity")+
  theme_bw(base_size=14)+
  theme(plot.title=element_text(family="RMN", face="plain", size=14, hjust=0.5),
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.x=element_text(family="RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines")) +
  ylab("Frequency")+
  xlab("Max Depth (log 10)")

# Just lake volume
tiff(filename="hist_Volume_overlap_Hollister_estimate.tiff", width=6.5, height=5,units="in", res = 600)
print(hist_vol)
dev.off()

# Using multiplot
a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="hist_Volume_overlap.png", width=6.5, height=5,units="in", res = 600)
print(multiplot(hist_vol, hist_z, cols=2), vp=a)
dev.off()

# PYRAMID WAS NOT A RESAMPLED LAKE
test<- nla07_12[which(nla07_12$NARS_NAME=="Pyramid Lake"),]
droplevels(test)
table(test$NARS_NAME)

####################
## One sample t-test
##  Test with mean differnces are statistically different from zero
####################
# Create empty dataframe
# Create a data frame for test results
myvars<- c("SID","Lake_Origin_use","ECOREG_use",
           "HorizDD_use.x","L_HorizDD_use.x","L_DDHzSqrtA_sc.x","E_I.x","L_DDVrtDix_sc.x",
           "HorizDD_use.y","L_HorizDD_use.y","L_DDHzSqrtA_sc.y","E_I.y","L_DDVrtDix_sc.y")

# Differences
#myvars<- c("SID","Lake_Origin_use","ECOREG_use","HorizDD_DIFF","L_HorizDD_DIFF",
#           "L_DDHzSqrtA_sc_DIFF","VertDD_DIFF","L_VertDD_DIFF","L_DDVrtDix_sc_DIFF",
#           "E_I_DIFF")

nla07_12_merge_red <- nla0712_merge[myvars]

nla07_12_merge_red<-na.omit(nla07_12_merge_red)

## Calculate standard error of mean values se = sd(x)/sqrt(length(x))
nla07_12_merge_red$L_DDHzSqrtA_sc.x.SE <- sd(nla07_12_merge_red$L_DDHzSqrtA_sc.x)/sqrt(length(nla07_12_merge_red$L_DDHzSqrtA_sc.x))
nla07_12_merge_red$L_DDHzSqrtA_sc.y.SE <- sd(nla07_12_merge_red$L_DDHzSqrtA_sc.y)/sqrt(length(nla07_12_merge_red$L_DDHzSqrtA_sc.y))

nla07_12_merge_red$E_I.x.SE <- sd(nla07_12_merge_red$E_I.x)/sqrt(length(nla07_12_merge_red$E_I.x))
nla07_12_merge_red$E_I.y.SE <- sd(nla07_12_merge_red$E_I.y)/sqrt(length(nla07_12_merge_red$E_I.y))


#Results <- data.frame(Lake_Origin_use=NA, ECOREGION=NA,
#                      t=NA, parameter=NA, p_value=NA,
#                       estimate=NA) #conf_int=NA,

Results <- data.frame(Lake_Origin_use=NA, ECOREGION=NA,
                      HorizDD_scaled_07=NA, HorizDD_scaled_12=NA,
                      Difference=NA, StdError=NA,
                      z_Score=NA,p_value=NA) 

r=0
for(lk in unique(nla07_12_merge_red$Lake_Origin_use)){ #for(eco in unique(nla0712_merge$ECOREG_use)){
  tempdf <- droplevels(subset(nla07_12_merge_red, Lake_Origin_use==lk))
  if(nrow(tempdf)>2){  
    nlev <- length(unique(tempdf$ECOREG_use))
    for(i in 1:nlev){
      diff=tempdf$L_DDHzSqrtA_sc.y[i] - tempdf$L_DDHzSqrtA_sc.x[i]
      stderr <- sqrt(tempdf$L_DDHzSqrtA_sc.y.SE[i]^2 + tempdf$L_DDHzSqrtA_sc.x.SE[i]^2)
      tst <- abs(diff)/stderr
      pval <-2*(1-pnorm(tst))
      Results[r,1:8]<-c(lk, tempdf$ECOREG_use[i],
                        tempdf$L_DDHzSqrtA_sc.x, tempdf$L_DDHzSqrtA_sc.y,
                        diff, stderr,tst,pval)
    }
  }
}


r<- 0
for(ind in unique(p2$Indicator)){
  for(lk in unique(p2$Lake_Origin_use)){
    tempdf<-droplevels(subset(p2, Indicator==ind & Lake_Origin_use ==lk))
    if(nrow(tempdf)>1){
      nlev<- length(unique(tempdf$ECOREG_use))
      for(i in 1:(nlev-1)){
        for(j in 2:nlev){
          r<-r+1
          diff<- tempdf$Estimate[i] - tempdf$Estimate[j]
          stderr<-sqrt(tempdf$StdError[i]^2 + tempdf$StdError[j]^2)
          tst<-abs(diff)/stderr
          pval<-2*(1-pnorm(tst))
          Results[r, 1:12] <- c(ind,lk,tempdf$ECOREG_use[i],
                                tempdf$ECOREG_use[j], tempdf$NResp[i],
                                tempdf$NResp[j],tempdf$Estimate[i],
                                tempdf$Estimate[j], diff, stderr,tst,pval)
        }
      }
    }
  }
}


######## \\
# Using long dataset to do mean comparisons
# Use only single visits
########
# Read long structured dataset - SINGLE obsevations so each year has one observation
nla0712_single_LONG<- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/nla07_12_rbind_SINGLE.csv")
#nla07_12_long<- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/nla07_12_rbind_revisits.csv")

# reduce number of variables
myvars<- c("SID","YEAR","Lake_Origin_use","ECOREG_use",
           "HorizDD_use","L_HorizDD_use","L_DDHzSqrtA_sc",
           "VertDD_use","L_VertDD_use","L_DDVrtDix_sc",
           "E_I","L_DDVrtDix_sc","Temp_degC_avg_yr","Precip_mm_avg_yr",
           "Precip_mm_total_yr")

nla0712_long_red <- nla0712_single_LONG[myvars]

#nla0712_long_red<-na.omit(nla0712_long_red)

table(nla0712_long_red$Lake_Origin_use, 
      nla0712_long_red$ECOREG_use,nla0712_long_red$YEAR)

table(nla0712_long_red$YEAR)

# Drop SAP observations because no Natural lakes
nla0712_long_red<-subset(nla0712_long_red[!(nla0712_long_red$ECOREG_use%in%c("SAP")),])
nla0712_long_red<-droplevels(nla0712_long_red)



