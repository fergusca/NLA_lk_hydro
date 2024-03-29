#################
## Year comparisons of NLA 2007 and 2012 data
##    CREATING DATASET OF RESAMPLED LAKES 
##    Objective to examine difference between survey years in lake hydrology and climate variables
##    Exploratory analysis for the manuscript to see if there are associations
##    Will perform more robust analysis in future work
# 10/11/17 - EF
# 10/17/17 - Created new dataset with both years observations joined together (not just lakes that were visited both years)
#             Using this to create histogram of d-excess and colored by year
# 12/13/17 - Updated datasets to include water residence time and watershed yield
# 1/17/18 - Added PRISM 30 yr average climate data to datasets
# 2/7/18  - Added elevation in basin
# 3/8/18 - Added scaled drawdown variables
# 5/14/18 - Added more complete datasets with NAs
# 5/29/18 - Added Palmer Drought Indicies
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

## NLA07 PROCESSED DATASET
# 7/8/19 - updated zmax for 24 lakes
nla07 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

table(nla07$RESAMPLED12)
# DRP  NO YES 
#   2 660 366  - about 36% of lakes were resampled in 2012


## NLA 2012 data - PROCESSED DATASET 
# FULL Dataset n = 1038 observations 
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
names(nla07)[names(nla07)=="Lake_Vol"]<-"Volume_m3"
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
names(nla12)[names(nla12)=="phdi_nla12_WY_AVG"]<-"PHDI"


###################
## REDUCE AND REORDER VARIABLES IN DATASETS 
### For columns to match up between survey year datasets

##############
## NLA 2007
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
# Combine datasets using rbind
nla07_12<-rbind(nla07_red, nla12_red)

###################
## ORDER FACTOR VARIABLES
nla07_12$ECOREG_use<-factor(nla07_12$ECOWSA9_2015, labels=c("CPL","NAP","NPL","SAP","SPL","TPL","UMW","WMT","XER"))
nla07_12$ECOREG_use <- ordered(nla07_12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla07_12$YEAR<-factor(nla07_12$YEAR, labels=c("2007","2012")) 
table(nla07_12$YEAR)
# 2007 2012 
# 1028 1038 

table(nla07_12$Lake_Origin_use)
#MAN_MADE  NATURAL # Adds up to 2066 total number
#1175      891 


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
##  Long format - dataset is organized with the same column names and repeated observations by lake ID for each survey year
##                Each resampled lake has two rows (one for 2007 and 2012)
###########
# SINGLE Long format
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

t<-nla07_12 %>%
  filter(Drawdown_CONDus15=="LARGE") %>%
  filter(YEAR==2012) %>%
  filter(ECOP6_2015=="WEST")

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


################
## WRITE WIDE-FORMATE RESAMPLED LAKES DATASET
#  Small subset of variables
################
write.csv(nla07_12_red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_01JUL19.csv") # OLD 02AUG18


