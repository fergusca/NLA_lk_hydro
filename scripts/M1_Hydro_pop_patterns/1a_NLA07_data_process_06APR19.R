#############
### Create 2007 NLA dataset - merge existing datasets and retain subset of variables
#############

# Preliminary steps - data exploration in "NLA_iso_down_basin_2007_30JAN17.R"

## ORIGINAL DATA
##  LAKE ISOTOPE: M:\Net MyDocuments\a_Water_Level\Data\ nla0712isodrawdown_20160219.csv"
##  NARS: in a_Water_Level\Data\ and Data\NLA_2007 folders:
##    Lake information: "NLA2007_SampledLakeInformation_20091113.csv",
##    Water quality (chem + secchi): "NLA2007_WaterQuality_20091123"
##    Physical habitat: "NLA2007_PHab_Metrics_A" & "NLA2007_PHab_Metrics_B"
##    Condition: "NLA2007_Chemical_ConditionEstimates_20091123"
##    Visual: "NLA2007_VisualAssessment_20091015"

##  NLA Data NOT used: "NLA2007_PHab_IndexValues","NLA2007_Secchi_20091008"
##
##  NLA 2007 Basin data is 1992 LULC: "NLA2007_Basin_Landuse_Metrics_20061022",
##  Updated to have tabulated NLCD 2006 within NLA watershed
##
##  CLIMATE data (2007) used in stable isotope analysis in \a_Water_Level\Data\Climate data
#     "NLA_climate_data_2012.csv" (update 2/10/17)

# NOTES - 1/30 reviewed variables with Renee and Phil to decide what to retain
#         3/8 Decided to use the WaterQuality spreadsheet for chemistry & secchi
#         2/10 Added lake volume, Shoreline development index, and climate data in data Renee sent
#         3/16/17 Add other variables - Watershed area and LULC area
#         3/20/17 Added more variables after meeting with Dave Peck and Renee and Phil about flagged data
#         5/23/17 Added NLCD 2006 land use/land cover area within watershed
#         8/21/17 Modified variables to retain in exported dataset - watershed basin area, water residence time, relabeled LULC 2006 variables
#         9/11/17 Added 2006-2007 Climate data that was in Renee's E:I spreadsheet
#         9/26/17 Added max depth from literature for lakes where max depth is underestimated - created column indicating the source of max depth values
#         10/30/17 Added lake connectivity type derived using CSI LAGOS toolbox in Read et al. 2015 paper
#         12/11/17 ONLY TO the SINGLE Lake Dataset - Added Water Yield & other water balance parameters to explore & Retained only VISIT_NO = 1 for single lake dataset. This was done because we only had measurements for VISIT = 1 lakes
#         12/18/17 Updated point data output for GIS map of study extent
#         1/03/18 Composition of precip classes
#         2/7/18 Added Watershed elevation data - in clim07.csv (Renee passed on this dataset that include climate, N fertilizer, elevation, etc)
#         4/12/18 - Added calculated vertical drawdown scaled by modified lake depth
#         5/10/18 - Want to clarify number of lakes have zero weights, number of lakes with HorizDD= NA
#                   For paper, would like to state that NLA07 = 1028 lakes with weights, 44 were not assessed
#                   For the analysis, it's okay that dataset was smaller because there are lakes without DD measured etc - but need to document why there are differences
#         10/8/18 Added PHDI (average water year)
#         10/31/18 Added E:I and WRT classes
#         5/8/19 Added RDis_InEx in Physical Habitat Index file

#         6/25/19 Added LakeCat variables - processed in separate script (08MAY19)
#                 Added modified drawdown variables from Phil (2/11/19) - processed in separate script
#                 Added UPDATED POPULATION WEIGHTS (Tony emailed 6/24/19) - based on aggregated ecoregion and lake size category
#         7/5/19 Updated zmax for 24 lakes and updated volume, wrt, and water yield estimates
#######

rm(list=ls())

###########
# Libraries
###########
library(tidyverse)
library(maps)
#library(mapdata)
library(scales)
library(maptools)

library(rgdal)

library(dplyr)
library(lattice)

#library(corrr)
library(igraph)
library(corrplot)

library(caret)

############
## Load datasets
############

### Isotope and drawdown data (2007 & 2012)
isodata<-read_csv("data/NLA07/a_nla0712_isodrawdown_20160219.csv")

### Load NLA 2007 datasets - taken from NARS website

## Lake information (2007)
lk <-read_csv("data/NLA07/NLA2007_SampledLakeInformation_20091113.csv")
names(lk)

## Phys habitat absolute measures data (2007) 
habitat_a <-read_csv("data/NLA07/NLA2007_PHab_Metrics_A.csv") # 1442 and 189 vars
habitat_b <- read_csv("data/NLA07/NLA2007_PHab_Metrics_B.csv") # 1442 and 183 vars

## Physical habitat index values (2007)
hab_index <- read_csv("data/NLA07/NLA2007_PHab_IndexValues.csv")
names(hab_index)

## Recreational cond (cyano) data (2007)
recreation<-read_csv("data/NLA07/NLA2007_Recreational_ConditionEstimates_20091123.csv")
names(recreation)

## Trophic condition
trophic<- read_csv("data/NLA07/NLA2007_Trophic_ConditionEstimate_20091123.csv")

## Visual assessment
visual <-read_csv("data/NLA07/NLA2007_VisualAssessment_20091015.csv")

## Water quality - USE this one for lake water quality vars
quality <- read_csv("data/NLA07/NLA2007_WaterQuality_20091123.csv") # 1326 obs - there are some lakes with multiple samples collected (2-3)
names(quality)

## Basin LC/LU data - 1992 NLCD (in NARS and used in NLA 2007 Report)
#   We can ues a more updated LULC dataset 
LU_92<-read_csv("data/NLA07/NLA2007_Basin_Landuse_Metrics_20061022.csv")

## UPDATE - ADDED Basin Land Use/ Land Cover NLCD 2006 tabulated data
# With corrected LULC area for watersheds that overlap with interntional boundaries - Had to use the percentage values and adjust for the watershed area within the US
LU <- read_csv("data/NLA07/NLA07_NLCD06_LULC_area_sqkm_ALL.csv")

# Drop X variable
todrop <-names(LU)%in%c("...1")
LU<-LU[!todrop]


## Climate and Volume data 
## Isotope and drawdown data (2007 & 2012)
## THIS IS 30 yr AVERAGED DATA (PRISM) - VALUES ARE THE SAME IN NLA 2012
clim07<-read_csv("data/NLA07/NLA_2007_Isotope_master.csv") #1252 obs and 136 variables


## Climate data for 2006-2007 - UPDATE 9/11/17
## THIS IS DATA SPECIFIC TO THE SAMPLE YEAR - taken from Renee's E:I spreadsheet
clim06_07 <- read_csv("data/NLA07/climate_2007_all.csv")


#########
## Process datasets
#########
# Isotope - Drop all 2012 measurements - only have 2007 data
isodata_07<- isodata[which(isodata$YEAR == 2007),] # keeps 1252 records out of 2492

## Phys Habitat - Drop duplicated observations - 190 lakes had copies of the same data values
habitat_a_no<-habitat_a[!duplicated(habitat_a[,c(1,3)]),]
habitat_b_no<-habitat_b[!duplicated(habitat_b[,c(1,3)]),]

## Phys Habitat INDEX - Drop duplicated observations - 1142 obs drops 190 lakes
hab_index_no <- hab_index[!duplicated(hab_index[,c(1,3)]),] # using SITE_ID and VISIT_NO

# From meeting with Dave Peck 3/16/17 - the duplicated water quality data come from the field to do precision analysis
#  field precision was good - so can drop the duplicated sample
## Drop duplicate observations in Water Quality data by SITE_ID and VISIT_NO
quality_no<-quality[!duplicated(quality[,c(1,3)]),] #drops 74


##########
## Lakes have been resampled in NLA to evaluate precision
#   ~10% of lakes randomly selected for a second sampling later in the summer (from NLA 2007 report)
##########

## Isotope data
# Number of unique lakes
cat("Number of lakes with isotope data =", length(unique(isodata_07$SITE_ID))) # 1157 lakes and 1252 observ - need to decide what to do with repeat measurements - take median?

## Water quality data
names(quality)
cat("Number of lakes with water quality =", length(unique(quality$SITE_ID))) # 1157 lakes and 1326 observ - need to decide what to do with repeat measurements - take median?

## Physical habitat data
names(habitat_a)
cat("Number of lakes with physical habitat =", length(unique(habitat_a$SITE_ID))) #
## Number of lakes with physical habitat = 1157

## Physical habitat index
cat("Number of lakes with physical habitat index =", length(unique(hab_index$SITE_ID)))
#Number of lakes with physical habitat index = 1157

##########
## DECISIONS
#   1) What to do with resampled data
#         We will drop the duplicates - these were used to calcuate field and lab error - and precision was determined to be good

#   2) Flagged data
#         Will keep track of lakes that stand out once we analyze - if there are outliers can go back and check to see if they are flagged
###########


###################
## Create reduced datasets to merge and export and save into "mergeme" and "NLA_2007_merge" folders
# Variables selected are highlighed in excel spreadsheet NLA_2007_variable_names.xlsx
#  in folder C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)\a_Water_Level\Data\NLA_2007_merge
###################

## Lake dataset
#lk.red<-lk[c(1:2,11:12,21:22,48,50)]
names(lk)
lkvars <-c("SITE_ID","VISIT_NO","DATE_COL","REPEAT","SITE_TYPE","TNT","LON_DD","LAT_DD",
           "ALBERS_X","ALBERS_Y","FLD_LON_DD","FLD_LAT_DD","ST","STATE_NAME","EPA_REG",
           "NHDNAME","LAKENAME","AREA_CAT7","WGT_NLA","WSA_ECO3","WSA_ECO9",
           "ECO_LEV_3","ECO_L3_NAM","NUT_REG","NUTREG_NAME","ECO_NUTA","LAKE_ORIGIN",
           "AREA_HA","SIZE_CLASS","LAKEAREA","LAKEPERIM","SLD","DEPTH_X","DEPTHMAX",
           "HUC_2","HUC_8","REACHCODE","COM_ID")

lk.red<-lk[lkvars]

## Water quality dataset - ADDING FLAGS
names(quality_no)
qualvars<-c("SITE_ID","VISIT_NO","SAMPLE_CATEGORY","SAMPLE_ID_CHEM","PH_LAB","PHLAB_FLAG","COND","COND_FLAG","COND_RL_ALERT","ANC",
            "ANC_FLAG","TURB","TURB_FLAG","TURB_RL_ALERT","TOC","TOC_FLAG","TOC_RL_ALERT","DOC","DOC_FLAG","DOC_RL_ALERT",
            "NH4N_PPM","NH4_FLAG","NH4_RL_ALERT","NO3_NO2","NO3NO2_FLAG","NO3NO2_RL_ALERT","NO3N_PPM","NO3_FLAG","NO3_RL_ALERT",
            "NTL","NTL_FLAG","NTL_RL_ALERT","PTL","PTL_FLAG","PTL_RL_ALERT","CHLA","CHLA_RL_ALERT","FLAG_FLD_CHLA","COMMENT_FLD_CHLA","FLAG_LAB_CHLA",
            "CL_PPM","CL_FLAG","CL_RL_ALERT","SO4_PPM","SO4_FLAG","SO4_RL_ALERT","CA_PPM","CA_FLAG","CA_RL_ALERT",
            "MG_PPM","MG_FLAG","MG_RL_ALERT","NA_PPM","NA_FLAG","NA_RL_ALERT","K_PPM","K_FLAG","K_RL_ALERT",
            "COLOR","COLOR_FLAG","COLOR_RL_ALERT","SIO2","SIO2_FLAG","SIO2_RL_ALERT","CATSUM","ANSUM2","SOBC",
            "SECMEAN","CLEAR_TO_BOTTOM","COMMENT_SECCHI","FLAG_SECCHI","COMMENT_FLD_CHEM","COMMENT_LAB_CHEM")

qual.red<-quality_no[qualvars]
names(qual.red)

## Trophic condition
names(trophic)
trphvars<- c("SITE_ID","VISIT_NO","TSTATE_TP","TSTATE_TN","TSTATE_CHL",
             "TSTATE_SECCHI")
trophic.red<-trophic[trphvars]

## Recreational condition - for cyano & toxin data
#recreation.red<-recreation[c(1,2,28:48)]
names(recreation)
recvars<-c("SITE_ID","VISIT_NO","MCYST_TL_UGL","CYANDENS")
recreation.red<-recreation[recvars]


## Habitat metrics A & B
#habitat.red<-habitat[c(1,3,4,13:59)]
names(habitat_a_no)
hab_a_vars<- c("SITE_ID","VISIT_NO","sixDepth","sivDepth","L_sixDepth","L_sivDepth","bsfcBedrock","bsfcBoulders","bsfcCobble",
               "bsfcGravel","bsfcSand","bsfcSilt","bsfcOrganic","bsfcWood","bsiStaVariety","bsiSiteVariety","bsiStStaVariety",
               "bsxLdia","bsvLdia","amfcEmergent","amfcFloating","amfcSubmergent","amfcAll","amfcFltEmg",
               "fcfcAquatic","fcfcBoulders","fcfcBrush","fcfcLedges","fcfcLiveTrees","fcfcOverhang","fcfcSnag","fcfcStructures",
               "fciAll","fciBig","fciNatural","fciRipVeg",
               "rvfcCanBig","rvfcCanSmall","rvfcUndNonw","rvfcUndWoody","rvfcGndBare","rvfcGndInundated","rvfcGndNonw","rvfcGndWoody",               "rviCanopy","rviUnderstory",
               "rviGround","rviWoody","rviTallWood","rviHerbs","rviCanUnd","rviTotalVeg")

hab_a.red<-habitat_a_no[hab_a_vars]

names(habitat_b_no)
hab_b_vars<- c("SITE_ID","VISIT_NO","ssfcBedrock","ssfcBoulders",
               "ssfcCobble","ssfcGravel","ssfcSand","ssfcSilt","ssfcOrganic","ssfcWood","ssfcOther",
               "ssiStaVariety","ssiSiteVariety","ssiStStaVariety","ssxLdia","ssvLdia",
               "hipwBuildings","hipwCommercial","hipwRoads","hipwWalls","hipwDocks","hipwPowerlines",
               "hipwLandfill","hipwLawn","hipwPark","hipwCrops","hipwOrchard","hipwPasture",
               "hiiAll","hiiNonAg","hiiAg","hiiAllCirca","hiiNonAgCirca","hiiAgCirca","hifpAny","hifpAnyCirca",
               "bffFlat","bffGradual","bffSteep","bffVertical","bfoAngle","bfnAngle",
               "bfxHorizDist","bfxVertHeight","bfnHorizDist","bfnVertHeight","L_RtHzVrt",
               "RDisInEx1a") # Did not include Log transformed Horiz or Vertical lake level fluctuation because in the Isodata

hab_b.red<-habitat_b_no[hab_b_vars]
names(hab_b.red)

## HABITAT INDEX
names(hab_index_no)

hab_ind_vars <- c("SITE_ID","VISIT_NO","DATEPHAB","ssiBedBld","ssiNATBedBld","rviLowWood",
                  "RVegQ_7","RVegQ_8","L_RVegQ_8","LRCVQ_7A", "LRCVQ_7B","LRCVQ_7C","LRCVQ_7D","LRCVQ_8D","L_LRCVQ_8D",
                  "ElevXLat","ElevDLat",
                  "ElevXLon","RDis_InEx","RDis_IX",
                  "RvegQ_Var","RVegQ","LogRVegQ",
                  "Pre3A_L_RVegQ_8","Adj3A_L_RVegQ_8","LOE_RVQ_west",
                  "LitCvrQ_Var", "LitCvrQ", "LogLitCvrQ",
                  "Pre3A_L_LitCvrQ","Adj3A_L_LitCvrQ","LOE_LitCv_west",
                  "LitRipCVQ_Var","LitRipCVQ","LogLitRipCvQ",
                  "Pre3A_L_LRCvQ_8D","Adj3A_L_LRCvQ_8D","LOE_LitRipCv_west",
                  "RVeg_OE","LitCvr_OE","LitRipCvr_OE")

hab_index.red <-hab_index_no[hab_ind_vars]
names(hab_index.red)

## Visual assessment
names(visual)

visual_vars<- c("SITE_ID","VISIT_NO","RES_PIPES","AGR_CROPLAND","AGR_PASTURE",
                "AGR_LIVESTOCK","AGR_ORCHARDS","AGR_POULTRY","AGR_FEEDLOT",
                "AGR_WITHDRAWL","IND_INDUSTRIAL","IND_MINES","IND_OIL","IND_POWER",
                "IND_LOGGING","IND_FIRE","IND_ODORS","IND_COMMERCIAL","MAN_LEVEL_FLUCTUATIONS","RES_SCORE","REC_SCORE",
                "AGR_SCORE","IND_SCORE","MAN_SCORE","HYDRO_TYPE","OUTLET_DAMS","SWIMMABILITY",
                "LAKE_LEVEL", "LEVEL_CHANGES","TROPHIC_STATE")

visual.red<-visual[visual_vars]
names(visual.red)


## Climate - data from Isotope spreadsheet - BUT THESE ARE 30 YR PRISM AVERAGES

names(clim07)
clim_vars<- c("SITE_ID","VISIT_NO","IRT","PRT","ORT","MRT","Lake_Vol",
              "ELEV_PT","Max_WSelev","Mean_WSelev","Stdev_WSelev",
              "DOM_GEOL","GEOL_PT","Precip_WS","Precip_PT",
              "P_WY","E","RH_WS","RH_PT","RH_COLMO","RH_FW","T_FW",
              "TMAX_WS","TMEAN_WS","TMIN_WS",
              "TMAX_PT","TMEAN_PT","TMIN_PT",
              "POP_DEN","FarmFert","LvStckCon","LvStckUnC","NnFarmFert","ATM_N","Hum_N","Fert","manure","largest_source","AN")

clim.red <-clim07[clim_vars]
head(clim.red)


######################
## Climate 2006-2007 - UPDATED 6/30/18
names(clim06_07)

## Derive Season climate variables - NOTE - should decide what months to group
# Summer mean temperature and precip (OLD June-August)
# 6/30/18 - changed summer temperature to be the sample season (May - September bc missing october temp)
clim06_07$temp_degC_summer <- (clim06_07$tmean_200705 +clim06_07$tmean_200706 +clim06_07$tmean_200707
                               +clim06_07$tmean_200708 + clim06_07$tmean_200709)/5
head(clim06_07$temp_degC_summer)
summary(clim06_07$temp_degC_summer) # missing data for two observations

clim06_07$precip_mm_summer <-(clim06_07$precip_200706 + clim06_07$precip_200707 + clim06_07$precip_200708)/3
head(clim06_07$precip_mm_summer)

# Winter mean temp and precipitation (December-February)
clim06_07$temp_degC_winter <- (clim06_07$tmean_200612 +clim06_07$tmean_200701+clim06_07$tmean_200702)/3
summary(clim06_07$temp_degC_winter)

clim06_07$precip_mm_winter <-(clim06_07$precip_200612+clim06_07$precip_200701+clim06_07$precip_200702)/3
summary(clim06_07$precip_mm_winter)

# Spring mean temp and precip (March-May)
clim06_07$temp_degC_spring <- (clim06_07$tmean_200703 +clim06_07$tmean_200704 + clim06_07$tmean_200705)/3
summary(clim06_07$temp_degC_spring)

clim06_07$precip_mm_spring <-(clim06_07$precip_200703 +clim06_07$precip_200704 + clim06_07$precip_200705)/3
summary(clim06_07$precip_mm_spring)

# Rename variables
names(clim06_07)[names(clim06_07)=="Precip_PT_avg_06_07"] <- "Precip_mm_avg_yr"
names(clim06_07)[names(clim06_07)=="Precip_WY_06_07"] <- "Precip_mm_total_yr"
names(clim06_07)[names(clim06_07)=="TMEAN_PT_avg_06_07"] <- "Temp_degC_avg_yr"

# Reduce dataset to get average values for the year
myvars<- c("SITE_ID","Precip_mm_avg_yr","Precip_mm_total_yr","Temp_degC_avg_yr",
           "E_avg_06_07","RH_PT_avg_06_07","temp_degC_summer","precip_mm_summer",
           "temp_degC_winter","precip_mm_winter","temp_degC_spring","precip_mm_spring")

clim06_07_red <-clim06_07[myvars]

## Landuse NLCD 1992 3/15/17
names(LU_92)
lu_vars<-c("SITE_ID","VISIT_NO","BASINAREA_KM2","BASINAREA_LU_KM2","NLCD11_KM2_BSN","NLCD12_KM2_BSN",
           "NLCD21_KM2_BSN","NLCD22_KM2_BSN","NLCD23_KM2_BSN","NLCD24_KM2_BSN","NLCD31_KM2_BSN","NLCD32_KM2_BSN",
           "NLCD41_KM2_BSN","NLCD42_KM2_BSN","NLCD43_KM2_BSN","NLCD51_KM2_BSN","NLCD52_KM2_BSN",
           "NLCD71_KM2_BSN","NLCD72_KM2_BSN","NLCD73_KM2_BSN","NLCD74_KM2_BSN","NLCD81_KM2_BSN","NLCD82_KM2_BSN",
           "NLCD90_KM2_BSN","NLCD91_KM2_BSN","NLCD92_KM2_BSN","NLCD93_KM2_BSN","NLCD94_KM2_BSN",
           "NLCD95_KM2_BSN","NLCD96_KM2_BSN","NLCD97_KM2_BSN","NLCD98_KM2_BSN","NLCD99_KM2_BSN","LANDUSE_KM2_BSN",
           "PCT_OPENH20_BSN","PCT_ICESNOW_BSN","PCT_WATER_BSN",
           "PCT_DEVOPEN_BSN","PCT_DEVLOW_BSN","PCT_DEVMED_BSN","PCT_DEVHIGH_BSN","PCT_DEVELOPED_BSN",
           "PCT_BARREN_BSN","PCT_DECID_BSN","PCT_CONIF_BSN","PCT_MIXED_BSN","PCT_FOREST_BSN",
           "PCT_SHRUBLAND_BSN","PCT_GRASS_BSN","PCT_PASTURE_BSN","PCT_CROPS_BSN",
           "PCT_AGRIC_BSN","PCT_WDYWET_BSN","PCT_EMHERBWET_BSN","PCT_WETLAND_BSN",
           "FLAG_LU_BASIN","COMMENT_LU_BASIN")
LU_92.red<-LU_92[lu_vars]
names(LU_92.red)


### Landuse NLCD 2006
# Was going to calculate percent LULC after subtracting out lake area from Basin Area
# BUT there were negative values for some lakes - I think b/c lakes & watersheds 
#  overlapped country boundaries and there were no NLCD data outside the US
# 
# Rename columns
names(LU) <- c("SITE_ID", "Summed_area_sqkm","NLCD06_11_KM2_BSN","NLCD06_12_KM2_BSN",
               "NLCD06_21_KM2_BSN","NLCD06_22_KM2_BSN","NLCD06_23_KM2_BSN","NLCD06_24_KM2_BSN","NLCD06_31_KM2_BSN",
               "NLCD06_41_KM2_BSN","NLCD06_42_KM2_BSN","NLCD06_43_KM2_BSN","NLCD06_52_KM2_BSN",
               "NLCD06_71_KM2_BSN","NLCD06_81_KM2_BSN","NLCD06_82_KM2_BSN","NLCD06_90_KM2_BSN","NLCD06_95_KM2_BSN",
               "BASINAREA_06_sqkm", "TOTAL")
names(LU)


#######
## Write reduced datasets
#######
write_csv(isodata_07,"data_to_merge_NLA07/multi_merge07/a_isodata_07.csv" )
write_csv(lk.red,"data_to_merge_NLA07/multi_merge07/b_lk.red.csv")
write_csv(qual.red,"data_to_merge_NLA07/multi_merge07/c_qual.red.csv")
write_csv(trophic.red, "data_to_merge_NLA07/multi_merge07/d_trophic.red.csv")
write_csv(recreation.red, "data_to_merge_NLA07/multi_merge07/e_recreation.red.csv")
write_csv(hab_a.red,"data_to_merge_NLA07/multi_merge07/f_habitat_a.red.csv")
write_csv(hab_b.red,"data_to_merge_NLA07/multi_merge07/f_habitat_b.red.csv")
write_csv(hab_index.red, "data_to_merge_NLA07/multi_merge07/f_habitat_index.red.csv") # 5/8/19
write_csv(visual.red, "data_to_merge_NLA07/multi_merge07/g_visual.red.csv")
write_csv(clim.red,"data_to_merge_NLA07/multi_merge07/h_clim.red.csv")
write_csv(LU_92.red,"data_to_merge_NLA07/i_lu_1992.red.csv")
write_csv(LU, "data_to_merge_NLA07/j_lu_2006.csv") # UPDATE 8/21/17 - changed names and copied into

write_csv(clim06_07_red,"data_to_merge_NLA07/k_climate07_red.csv")


# copied and pasted data into mergeme and "NLA_2007_merge> Data" folders
# Basin Landuse saved in a separate area so not incorporated into the multi-merge function
#   Need to merge this one by hand

#########
##  Merge datasets
#########

#########
# Trying multi-merge function
#  from https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
#  from http://www.talkstats.com/showthread.php/22305-Merging-Multiple-Data-Frames
#######
# Run the function
# need to trim up datasets to not have repeat variables (except the ones we are merging on)
# Save the reduced datasets into a folder "Data"
# order datasets how we want them to be merged in the folder
# set path to folder and function will run on datasets

# Merging based on Site ID and Visit Number. I already subset data by year so the comb of the two make for a unique ID
multimerge = function(mergeme){
  filenames=list.files(path=mergeme, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y, by=c("SITE_ID","VISIT_NO"))}, datalist)
}

NLA_07_merge_a<-multimerge("data_to_merge_NLA07/multi_merge07")
#NLA_07_merge_a<- multmerge("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2007_merge/Data")
names(NLA_07_merge_a)  #1252 obs and 376 variables
table(NLA_07_merge_a$SITE_ID) #
table(NLA_07_merge_a$VISIT_NO)
#   1    2 
# 1157   95

##################
## Write Merged Dataset - first merge
write_csv(NLA_07_merge_a,"data_processed/nla07/NLA07_merge_a.csv")

####################
## READ in first MERGE dataset
####################
NLA_07_merge_a <- read_csv("data_processed/NLA07_merge_a.csv")
names(NLA_07_merge_a)
#todrop<-names(NLA_07_merge_a)%in%c("X")
#NLA_07_merge_a<-NLA_07_merge_a[!todrop]

# Count number of observations by SITE_ID
n_occur<-data.frame(table(NLA_07_merge_a$SITE_ID))
table(NLA_07_merge_a$VISIT_NO)

#########
# Manually merge NLCD 2006 Basin LU dataset
#########
j_lu.red<- read_csv("data_to_merge_NLA07/j_lu_2006.csv")
names(j_lu.red)

NLA_07_merge_b<-merge(NLA_07_merge_a, j_lu.red,by="SITE_ID")
names(NLA_07_merge_b) # 1250 Observations and 395 variables

##############
# Manually merge Climate 2006-2007 data
##############
k_climate07_red <- read_csv("data_to_merge_NLA07/k_climate07_red.csv")
names(k_climate07_red)

NLA_07_merge<-merge(NLA_07_merge_b, k_climate07_red,by="SITE_ID")
names(NLA_07_merge) # 1250 Observations and 406 variables


#########
## Clean up merged dataset
#########
#Dropping variables
#todrop<-names(NLA_07_merge_c)%in%c("X","X.1","X.x","X.y",
#                                   "X.x.1","X.x.2","X.x.3","X.y.1","X.y.2")
#NLA_07_merge<-NLA_07_merge_c[!todrop]
#names(NLA_07_merge) # 1250 obs and 385 variables

##########
## RENAME LAKE ORIGIN to match 2012
#########
levels(NLA_07_merge$Lake_Origin_use) <- list(
  "MAN_MADE" = c("MAN-MADE"),
  "NATURAL" = c("NATURAL"))
table(NLA_07_merge$Lake_Origin_use)
# MAN_MADE  NATURAL 
#  695      555

#######
## NLCD 2006 data
##  need to calculate percent values
#######

###########
## NLCD 2006 Land use/land cover %
###########
NLA_07_merge$PCT06_OPENH2O_BSN<- (NLA_07_merge$NLCD06_11_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_ICESNOW_BSN<- (NLA_07_merge$NLCD06_12_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_DEVOPEN_BSN<- (NLA_07_merge$NLCD06_21_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_DEVLOW_BSN<- (NLA_07_merge$NLCD06_22_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_DEVMED_BSN<- (NLA_07_merge$NLCD06_23_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_DEVHIGH_BSN<- (NLA_07_merge$NLCD06_24_KM2_BSN/NLA_07_merge$TOTAL)*100

NLA_07_merge$PCT06_BARREN_BSN<- (NLA_07_merge$NLCD06_31_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_DECID_BSN<- (NLA_07_merge$NLCD06_41_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_CONIF_BSN<- (NLA_07_merge$NLCD06_42_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_MIXED_BSN<- (NLA_07_merge$NLCD06_43_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_SHRUBLAND_BSN<- (NLA_07_merge$NLCD06_52_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_GRASS_BSN<- (NLA_07_merge$NLCD06_71_KM2_BSN/NLA_07_merge$TOTAL)*100

NLA_07_merge$PCT06_PASTURE_BSN<- (NLA_07_merge$NLCD06_81_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_CROPS_BSN<- (NLA_07_merge$NLCD06_82_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_WDYWET_BSN<- (NLA_07_merge$NLCD06_90_KM2_BSN/NLA_07_merge$TOTAL)*100
NLA_07_merge$PCT06_EMHERBWET_BSN<- (NLA_07_merge$NLCD06_95_KM2_BSN/NLA_07_merge$TOTAL)*100

#summary(NLA_07_merge[,c(385:401)])

plot(NLA_07_merge$NLCD06_41_KM2_BSN, NLA_07_merge$BASINAREA_06_sqkm)

#test<- NLA_07_merge[which(NLA_07_merge=="NLA06608-0384"),]
#test[,c(317,326:328,337)]


#####
## Rearrange variables
#####
# Lake info, Isotope data, Drawdown, Chemistry, Habitat, Basin LU, Visual

NLA07vars <-c("SITE_ID","VISIT_NO","SID","YEAR","DATE_COL","DATE_COL_iso","SAMPLE_ID",
              "dD","d18O","d_excess","E_I","RT",
              "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use",
              "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc",
              "VertDD_CONDc315","HorizDD_CONDc315","Drawdown_CONDus15","RTS12_15",
              "LkArea_km2","L_LkAreakm2","ELEV_use","L_ELEV_use","DpthMx_use","L_DpthMx_use","Lake_Vol", #"BASINAREA_KM2",
              "SAMPLED_PHAB","URBAN","REPEAT","RESAMPLED12","SITE_TYPE","TNT","Lake_Origin_use","ECOWSA9_2015","RT_NLA12_2015",
              "ECOP5_2015","ECOP6_2015",
              "LATdd_use","LONdd_use", "ALBERS_X","ALBERS_Y","FLD_LON_DD","FLD_LAT_DD","ST","STATE_NAME","EPA_REG", 
              "NHDNAME","LAKENAME","AREA_CAT7","WGT_NLA","WSA_ECO3","WSA_ECO9",
              "ECO_LEV_3","ECO_L3_NAM","NUT_REG","NUTREG_NAME","ECO_NUTA", 
              "AREA_HA","SIZE_CLASS","LAKEAREA","LAKEPERIM","SLD","DEPTH_X","DEPTHMAX",
              "ELEV_PT","HUC_2","HUC_8","REACHCODE","COM_ID",
              "SAMPLE_ID_CHEM","SAMPLE_CATEGORY","PH_LAB","PHLAB_FLAG","COND","COND_FLAG","COND_RL_ALERT","ANC",
              "ANC_FLAG","TURB","TURB_FLAG","TURB_RL_ALERT","TOC","TOC_FLAG","TOC_RL_ALERT","DOC","DOC_FLAG","DOC_RL_ALERT",
              "NH4N_PPM","NH4_FLAG","NH4_RL_ALERT","NO3_NO2","NO3NO2_FLAG","NO3NO2_RL_ALERT","NO3N_PPM","NO3_FLAG","NO3_RL_ALERT",
              "NTL","NTL_FLAG","NTL_RL_ALERT","PTL","PTL_FLAG","PTL_RL_ALERT","CHLA","CHLA_RL_ALERT","FLAG_FLD_CHLA","COMMENT_FLD_CHLA","FLAG_LAB_CHLA",
              "CL_PPM","CL_FLAG","CL_RL_ALERT","SO4_PPM","SO4_FLAG","SO4_RL_ALERT","CA_PPM","CA_FLAG","CA_RL_ALERT",
              "MG_PPM","MG_FLAG","MG_RL_ALERT","NA_PPM","NA_FLAG","NA_RL_ALERT","K_PPM","K_FLAG","K_RL_ALERT",
              "COLOR","COLOR_FLAG","COLOR_RL_ALERT","SIO2","SIO2_FLAG","SIO2_RL_ALERT","CATSUM","ANSUM2","SOBC","SECMEAN",
              "MCYST_TL_UGL","CYANDENS","CLEAR_TO_BOTTOM","COMMENT_SECCHI","FLAG_SECCHI","COMMENT_FLD_CHEM","COMMENT_LAB_CHEM",
              "TSTATE_TP","TSTATE_TN","TSTATE_CHL","TSTATE_SECCHI",
              "bsfcBedrock","bsfcBoulders","bsfcCobble",
              "bsfcGravel","bsfcSand","bsfcSilt","bsfcOrganic","bsfcWood","bsiStaVariety",
              "bsiSiteVariety","bsiStStaVariety","bsxLdia","bsvLdia","amfcEmergent",
              "amfcFloating","amfcSubmergent","amfcAll","amfcFltEmg","fcfcAquatic",
              "fcfcBoulders","fcfcBrush","fcfcLedges","fcfcLiveTrees","fcfcOverhang",
              "fcfcSnag","fcfcStructures","fciAll","fciBig","fciNatural","fciRipVeg",
              "rvfcCanBig","rvfcCanSmall","rvfcUndNonw","rvfcUndWoody","rvfcGndBare",
              "rvfcGndInundated","rvfcGndNonw","rvfcGndWoody","rviCanopy","rviUnderstory",
              "rviGround","rviWoody","rviTallWood","rviHerbs","rviCanUnd","rviTotalVeg",
              "ssfcBedrock","ssfcBoulders",
              "ssfcCobble","ssfcGravel","ssfcSand","ssfcSilt","ssfcOrganic",
              "ssfcWood","ssfcOther","ssiStaVariety","ssiSiteVariety","ssiStStaVariety",
              "ssxLdia","ssvLdia","hipwBuildings","hipwCommercial","hipwRoads",
              "hipwWalls","hipwDocks","hipwPowerlines","hipwLandfill","hipwLawn",
              "hipwPark","hipwCrops","hipwOrchard","hipwPasture","hiiAll","hiiNonAg","hiiAg",
              "hiiAllCirca","hiiNonAgCirca","hiiAgCirca","hifpAny","hifpAnyCirca",
              "bffFlat","bffGradual","bffSteep","bffVertical","bfoAngle","bfnAngle",
              "bfxHorizDist","bfxVertHeight","bfnHorizDist","bfnVertHeight","L_RtHzVrt",
              "RDisInEx1a","sixDepth","L_sixDepth",
              "DATEPHAB","ssiBedBld","ssiNATBedBld","rviLowWood",
              "RVegQ_7","RVegQ_8","L_RVegQ_8","LRCVQ_7A", "LRCVQ_7B","LRCVQ_7C","LRCVQ_7D","LRCVQ_8D","L_LRCVQ_8D",
              "ElevXLat","ElevDLat",
              "ElevXLon","RDis_InEx","RDis_IX",
              "RvegQ_Var","RVegQ","LogRVegQ",
              "Pre3A_L_RVegQ_8","Adj3A_L_RVegQ_8","LOE_RVQ_west",
              "LitCvrQ_Var", "LitCvrQ", "LogLitCvrQ",
              "Pre3A_L_LitCvrQ","Adj3A_L_LitCvrQ","LOE_LitCv_west",
              "LitRipCVQ_Var","LitRipCVQ","LogLitRipCvQ",
              "Pre3A_L_LRCvQ_8D","Adj3A_L_LRCvQ_8D","LOE_LitRipCv_west",
              "RVeg_OE","LitCvr_OE","LitRipCvr_OE",
              "NLCD06_11_KM2_BSN","NLCD06_12_KM2_BSN","NLCD06_21_KM2_BSN","NLCD06_22_KM2_BSN","NLCD06_23_KM2_BSN","NLCD06_24_KM2_BSN",
              "NLCD06_31_KM2_BSN","NLCD06_41_KM2_BSN","NLCD06_42_KM2_BSN","NLCD06_43_KM2_BSN",
              "NLCD06_52_KM2_BSN","NLCD06_71_KM2_BSN","NLCD06_81_KM2_BSN","NLCD06_82_KM2_BSN","NLCD06_90_KM2_BSN","NLCD06_95_KM2_BSN",
              "PCT06_OPENH2O_BSN", "PCT06_ICESNOW_BSN", "PCT06_DEVOPEN_BSN", "PCT06_DEVLOW_BSN",
              "PCT06_DEVMED_BSN","PCT06_DEVHIGH_BSN","PCT06_BARREN_BSN","PCT06_DECID_BSN",
              "PCT06_CONIF_BSN", "PCT06_MIXED_BSN", "PCT06_SHRUBLAND_BSN", "PCT06_GRASS_BSN",
              "PCT06_PASTURE_BSN","PCT06_CROPS_BSN","PCT06_WDYWET_BSN", "PCT06_EMHERBWET_BSN",
              "BASINAREA_06_sqkm", "TOTAL",
              "DOM_GEOL","GEOL_PT","Max_WSelev","Mean_WSelev","Stdev_WSelev",
              "Precip_WS","Precip_PT","P_WY","E","RH_WS","RH_PT","RH_COLMO","RH_FW","T_FW","TMAX_WS",
              "TMEAN_WS","TMIN_WS","TMAX_PT","TMEAN_PT","TMIN_PT",
              "Temp_degC_avg_yr","Precip_mm_avg_yr","Precip_mm_total_yr",
              "E_avg_06_07","RH_PT_avg_06_07","temp_degC_winter","temp_degC_spring","temp_degC_summer",
              "precip_mm_winter","precip_mm_spring","precip_mm_summer",
              "IRT","PRT","ORT","MRT",
              "RES_PIPES","AGR_CROPLAND","AGR_PASTURE",
              "AGR_LIVESTOCK","AGR_ORCHARDS","AGR_POULTRY","AGR_FEEDLOT",
              "AGR_WITHDRAWL","IND_INDUSTRIAL","IND_MINES","IND_OIL","IND_POWER",
              "IND_LOGGING","IND_FIRE","IND_ODORS","IND_COMMERCIAL","MAN_LEVEL_FLUCTUATIONS","RES_SCORE","REC_SCORE",
              "AGR_SCORE","IND_SCORE","MAN_SCORE","HYDRO_TYPE","OUTLET_DAMS","SWIMMABILITY","LAKE_LEVEL",
              "LEVEL_CHANGES","TROPHIC_STATE","POP_DEN","FarmFert","LvStckCon","LvStckUnC","NnFarmFert","ATM_N","Hum_N","Fert","manure","largest_source","AN")

test<-NLA_07_merge%>%
  select(all_of(NLA07vars))
NLA07_merge<-NLA_07_merge%>%
  select(all_of(NLA07vars)) # 1250 and 392 vars
names(NLA07_merge)

######
## Write new merged dataset
######
write_csv(NLA07_merge,"data_processed/nla07/NLA_07_merge.csv")

#######################
## Derive variables and Transformations
#######################

## Read in merged dataset
NLA_07_merge<- read_csv("data_processed/nla07/NLA_07_merge.csv")
# 1250 observation and 393 variables

# Process dataset
names(NLA_07_merge)

#########
## Lake size & depth classes based on quartile values
#########
# Drop observation missing lake depth (1 obs)
z<-NLA_07_merge[!is.na(NLA_07_merge$DpthMx_use),]

summary(NLA_07_merge$LkArea_km2)
quantile(NLA_07_merge$LkArea_km2)
#0%         25%         50%         75%        100% 
#0.04036675  0.23026332  0.65328532  2.12822412 91.32195666 
NLA_07_merge$lk_area_bin <-cut(NLA_07_merge$LkArea_km2, c(0.01, 0.23, 0.65,2.12,Inf), labels=1:4)

## Lake depth bins
quantile(z$DpthMx_use)
# 0%   25%   50%   75%  100% 
#0.50  2.80  5.60 11.25 60.30
NLA_07_merge$lk_depth_bin <- cut(NLA_07_merge$DpthMx_use, c(0.5, 2.8, 5.6, 11.25, Inf), labels=1:4)


#########
## Grouping lakes by E:I flow-through
# based on Brooks et al. 2014 - Wolfe et al. 2007
#########
##  Flowthrough E:I <0.4
##  Restricted drainage 0.4 >= E:I <1
##  Closed-basin E:I >= 1

NLA_07_merge$lk_hydro_iso [NLA_07_merge$E_I <0.4]<- "Flow_through"
NLA_07_merge$lk_hydro_iso[NLA_07_merge$E_I >=0.4 & NLA_07_merge$E_I <1] <-"Restricted"
NLA_07_merge$lk_hydro_iso[NLA_07_merge$E_I >=1] <-"Closed"
table(NLA_07_merge$lk_hydro_iso)

#       Closed Flow_through   Restricted 
#         8          933          309 


#######
## Reorder variables
#######
# from: http://stackoverflow.com/questions/4260698/r-ordering-in-boxplot

# UPDATE 7/29/17 - changed so far east is CPL
# Ecoregion groups so that they are plotted from West to East to match the map
NLA_07_merge$ECOREG_use <- ordered(NLA_07_merge$ECOWSA9_2015, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(NLA_07_merge$ECOREG_use)

# Order lk_hydro by "Flow-through", Restricted, and Closed
NLA_07_merge$lk_hydro_iso <- ordered(NLA_07_merge$lk_hydro_iso, levels=c("Closed","Restricted","Flow_through"))
table(NLA_07_merge$lk_hydro_iso)

# Preprocessing of HYDRO_TYPE to label lakes not given a designation
# https://stackoverflow.com/questions/27195956/convert-na-into-a-factor-level
library(forcats)
nla12_all<-nla12_all%>%
  mutate(HYDRO_TYPE_f=fct_na_value_to_level(HYDRO_TYPE,"Unknown"))
table(test$HYDRO_TYPE_f)

NLA_07_merge<- NLA_07_merge%>%
  mutate(HYDRO_TYPE_f=fct_na_value_to_level(HYDRO_TYPE,"Unknown"))
table(NLA_07_merge$HYDRO_TYPE_f)
NLA_07_merge$HYDRO_TYPE_f <- ordered(NLA_07_merge$HYDRO_TYPE_f, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))

table(NLA_07_merge$HYDRO_TYPE_f)
#SEEPAGE  DRAINAGE RESERVOIR 
#216       336       662 

#################
## Complile Land use/ Land cover classes
#################
NLA_07_merge$PCT06_FOREST_BSN <-NLA_07_merge$PCT06_DECID_BSN + NLA_07_merge$PCT06_CONIF_BSN + NLA_07_merge$PCT06_MIXED_BSN
NLA_07_merge$PCT06_DEVELOPED_BSN <- NLA_07_merge$PCT06_DEVLOW_BSN+ NLA_07_merge$PCT06_DEVMED_BSN + NLA_07_merge$PCT06_DEVHIGH_BSN
NLA_07_merge$PCT06_AGRIC_BSN <- NLA_07_merge$PCT06_PASTURE_BSN + NLA_07_merge$PCT06_CROPS_BSN
NLA_07_merge$PCT06_WETLAND_BSN <- NLA_07_merge$PCT06_WDYWET_BSN + NLA_07_merge$PCT06_EMHERBWET_BSN

# Make into proportion
NLA_07_merge$PCT06_FOREST_BSN <-NLA_07_merge$PCT06_FOREST_BSN/100
NLA_07_merge$PCT06_GRASS_BSN <-NLA_07_merge$PCT06_GRASS_BSN/100
NLA_07_merge$PCT06_DEVELOPED_BSN <- NLA_07_merge$PCT06_DEVELOPED_BSN/100
NLA_07_merge$PCT06_AGRIC_BSN <- NLA_07_merge$PCT06_AGRIC_BSN/100
NLA_07_merge$PCT06_WETLAND_BSN <- NLA_07_merge$PCT06_WETLAND_BSN/100


############################
## TRANSFORMATIONS - add 9/11/17
############################
#########
## Logit transformation BASIN LANDUSE vars - can't have 0s or 1s - so will add or subtract a small value to observations that are 0s or 1s
library(car)

NLA_07_merge$PCT_FOREST_BSN.n <- car::recode(NLA_07_merge$PCT06_FOREST_BSN, "0=0.0001; 1=0.999")  
summary(NLA_07_merge$PCT_FOREST_BSN.n)
NLA_07_merge$PCT_GRASS_BSN.n <- car::recode(NLA_07_merge$PCT06_GRASS_BSN, "0=0.0001; 1=0.999")
NLA_07_merge$PCT_WETLAND_BSN.n <- car::recode(NLA_07_merge$PCT06_WETLAND_BSN, "0=0.0001; 1=0.999")
NLA_07_merge$PCT_AGRIC_BSN.n <- car::recode(NLA_07_merge$PCT06_AGRIC_BSN, "0=0.0001; 1=0.999")
NLA_07_merge$PCT_DEVELOPED_BSN.n <- car::recode(NLA_07_merge$PCT06_DEVELOPED_BSN, "0=0.0001; 1=0.999")
NLA_07_merge$PCT06_PASTURE_BSN.n <- car::recode(NLA_07_merge$PCT06_PASTURE_BSN, "0=0.0001; 1=0.999")
NLA_07_merge$PCT06_CROPS_BSN.n <- car::recode(NLA_07_merge$PCT06_CROPS_BSN, "0=0.0001; 1=0.999")


# Logit transformation
NLA_07_merge$PCT06_FOREST_BSN_logit<-logit(NLA_07_merge$PCT_FOREST_BSN.n)
NLA_07_merge$PCT06_GRASS_BSN_logit<-logit(NLA_07_merge$PCT_GRASS_BSN.n)
NLA_07_merge$PCT06_WETLAND_BSN_logit<-logit(NLA_07_merge$PCT_WETLAND_BSN.n)
NLA_07_merge$PCT06_AGRIC_BSN_logit<-logit(NLA_07_merge$PCT_AGRIC_BSN.n)
NLA_07_merge$PCT06_DEVELOPED_BSN_logit<-logit(NLA_07_merge$PCT_DEVELOPED_BSN.n)

NLA_07_merge$PCT06_PASTURE_BSN_logit<-logit(NLA_07_merge$PCT06_PASTURE_BSN.n)
NLA_07_merge$PCT06_CROPS_BSN_logit<-logit(NLA_07_merge$PCT06_CROPS_BSN.n)

## LOG 10 transformation for water chemistry variables
NLA_07_merge$L_VOL <- log10(NLA_07_merge$Lake_Vol)
NLA_07_merge$L_SLD <- log10(NLA_07_merge$SLD)

NLA_07_merge$L_COND <- log10(NLA_07_merge$COND)
NLA_07_merge$L_ANC<-log10(NLA_07_merge$ANC)
NLA_07_merge$L_TURB <-log10(NLA_07_merge$TURB)
NLA_07_merge$L_DOC<-log10(NLA_07_merge$DOC)
NLA_07_merge$L_NTL<-log10(NLA_07_merge$NTL)
NLA_07_merge$L_PTL<-log10(NLA_07_merge$PTL)
NLA_07_merge$L_CHLA<-log10(NLA_07_merge$CHLA)
NLA_07_merge$L_CL_PPM<-log10(NLA_07_merge$CL_PPM)
NLA_07_merge$L_SO4_PPM<-log10(NLA_07_merge$SO4_PPM)
NLA_07_merge$L_CA_PPM<-log10(NLA_07_merge$CA_PPM)
NLA_07_merge$L_MG_PPM<-log10(NLA_07_merge$MG_PPM)
NLA_07_merge$L_NA_PPM<-log10(NLA_07_merge$NA_PPM)
NLA_07_merge$L_K_PPM<-log10(NLA_07_merge$K_PPM)
NLA_07_merge$L_SIO2<-log10(NLA_07_merge$SIO2)
NLA_07_merge$L_CATSUM<-log10(NLA_07_merge$CATSUM)
NLA_07_merge$L_ANSUM2<-log10(NLA_07_merge$ANSUM2)

##########
## Drop variables used for transformations
##########
myvars_drop <- names(NLA_07_merge)%in% c("PCT_FOREST_BSN.n", "PCT_GRASS_BSN.n","PCT_WETLAND_BSN.n",
                                         "PCT_AGRIC_BSN.n","PCT_DEVELOPED_BSN.n",
                                         "PCT06_PASTURE_BSN.n","PCT06_CROPS_BSN.n")

NLA_07_merge<- NLA_07_merge[!myvars_drop]


##########
## Create Ecoregion + Lake Origin variable

NLA_07_merge$WSA9_LO <- with (NLA_07_merge, interaction(ECOWSA9_2015,Lake_Origin_use,sep="_"))

table(NLA_07_merge$WSA9_LO)



############
## WRITE DATASET WITH TRANSFORMATIONS
############
write_csv(NLA_07_merge,"data_processed/nla07/NLA_07_transformed_fin.csv")


########################
## UPDATE TO ADD VARIABLES 
## 9/26/17 -  MAX DEPTH Decision - See Analysis\Data exploration\Max_depth_explore folder for more info
#             Populate underestimated max depth with lit values
#             Chose difference of 40 or more as criteria to change max depth
## 7/5/19 - expanded updated zmax to 24 lakes in 2007 survey - based on large discrepancy in field measure and literature documented zmax


## Read previous procesed dataset with transformations
nla07<-read_csv("data_processed/nla07/NLA_07_transformed_fin.csv")
names(nla07)

# Create dataset of lakes with underestimated max depth - see excel spreadsheet "NLA12_lakes_35m_maxdepth.xlsx"
#   Order by alphabetized SITE_ID
z<-subset(nla07,(SITE_ID %in% c("NLA06608-0021","NLA06608-0041","NLA06608-0079",
                                "NLA06608-0129","NLA06608-0181","NLA06608-0191",
                                "NLA06608-0291","NLA06608-0373","NLA06608-0495",
                                "NLA06608-0561","NLA06608-0580","NLA06608-0794",
                                "NLA06608-0870","NLA06608-0930","NLA06608-0934",
                                "NLA06608-0970","NLA06608-1045","NLA06608-1153",
                                "NLA06608-1348","NLA06608-1717","NLA06608-1818",
                                "NLA06608-1873","NLA06608-1958","NLA06608-2881")))
z[,c(1:4)]
# Entering max depth from lit - make sure put depth in twice for lakes with revists
depth_lit<- c(84,84,99.7,99.7,55,55,
              69,69,51,76,72,
              56,122,119,51,185,185,
              64,52,130,85,
              57,144,44,61,62,79,91,71)
z$DpthMx_mod <-depth_lit
head(z[,c(1,428)])

# Create column indicating source of max depth
z$Zmax_source <- "LIT"
head(z[,c(1,428,429)])

# Create column indicating source of max depth
z$Zmax_source <- "LIT"
head(z[,c(1,428,429)])

# Create dataset of all other lakes
other<-nla07[!(nla07$SITE_ID %in% c("NLA06608-0021","NLA06608-0041","NLA06608-0079",
                                    "NLA06608-0129","NLA06608-0181","NLA06608-0191",
                                    "NLA06608-0291","NLA06608-0373","NLA06608-0495",
                                    "NLA06608-0561","NLA06608-0580","NLA06608-0794",
                                    "NLA06608-0870","NLA06608-0930","NLA06608-0934",
                                    "NLA06608-0970","NLA06608-1045","NLA06608-1153",
                                    "NLA06608-1348","NLA06608-1717","NLA06608-1818",
                                    "NLA06608-1873","NLA06608-1958","NLA06608-2881")),]
# Create column that populates the updated depth
other$DpthMx_mod <-other$DpthMx_use

# Create column that indicates source of max depth
other$Zmax_source <-"NLA"
head(other[,c(1, 29,428,429)])


## Bind two dataframes together
nla07_revised <- rbind(other,z)
str(nla07_revised)
table(nla07_revised$ECOWSA9_2015)
table(nla07$ECOWSA9_2015)

# Log 10 transform max depth
nla07_revised$L_DpthMx_mod <- log10(nla07_revised$DpthMx_mod)
head(nla07_revised[,c(1,30,430)])

# Order by SITE_ID
nla07_revised <-nla07_revised [order(nla07_revised$SITE_ID),]
tail(nla07_revised[,c(1,29)])

#################
# Calculate scaled vertical drawdown using the modified lake depth
# 4/12/18
# Scaled (non-transfomred
nla07_revised$DDVrtDix_sc_MOD<-nla07_revised$VertDD_use/nla07_revised$DpthMx_mod
summary(nla07_revised$DDVrtDix_sc_MOD)

# LOG 10 transformed
nla07_revised$L_DDVrtDix_sc_MOD<-log10((nla07_revised$VertDD_use/nla07_revised$DpthMx_mod) +0.01)
summary(nla07_revised$L_DDVrtDix_sc_MOD)

###################
## WRITE MODIFIED DATASET - with modified max depth values
###################
# FULL dataset
write_csv(nla07_revised,"data_processed/nla07/NLA_07_transformed_depth.csv")


###########
## EXPLORE DATASET FOR ANALYSIS before merging modified isotope derived
###########
# Read dataset
NLA07_org<-read_csv("data_processed/nla07/NLA_07_transformed_depth.csv")

# NUMBER OF LAKES (VISIT=1) with WGT_NLA - WANT THIS TO BE ~1028
nla07_full<-NLA07_org[which(NLA07_org$WGT_NLA>0),] # 1032 lakes with weights
nla07_full<-lk_visit_one[which(lk_visit_one$WGT_NLA>0),] # 1032 lakes with weights
length(unique(nla07_full$SITE_ID))
#[1] 1033 lakes - this is close to Phil's n=1028 lakes with weights; 44(additional) were not assessed

# Number of LAKES that are NLA TARGET - 1027 - This fits! I dropped one lake missing max depth to building the lake depth classes
table(nla07_full$TNT)
# NonTarget    Target 
#        5      1123 

##################
## ADD ISOTOPE DERIVED WATER BALANCE PARAMETERS
#################
# READ IN MODIFIED isotope derived water balance parameters - e.g., Water Yield
#  n = 1158 obs with 8 vars
iso<- read_csv("data_to_merge_NLA07/i_iso_nla07_11DEC17.csv")
table(iso$VISIT_NO) #
#    1    2 
# 1157    1

# Data with both visits = 1249; iso data mostly has visit 1158

hist(iso$RT_iso)
hist(log10(iso$RT_iso+0.01)) 
# LOG TRANSFORM WRT
iso$L_RT_iso <- log10(iso$RT_iso+0.01) # 1 NA

# Merge datasets - nla07 VISIT_NO=1; with weights
nla07_revised <- merge(nla07_full, iso, by=c("SITE_ID","VISIT_NO"),all.x=TRUE)
#1033
length(unique(nla07_revised$SITE_ID)) # 1033

table(nla07_revised$SITE_TYPE)
#PROB_Lake  REF_Lake 
#1128

## SELECT TARGET LAKES 
y <- nla07_revised[which(nla07_revised$TNT=="Target"),] # n = 1123

################
# Create precip class
# 1/3/18
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
y$Precip_class_man<-cut(y$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))
## Create Precipitation + Lake Origin variable
y$PRECIP_LO <- with (y, interaction(Precip_class_man,Lake_Origin_use,sep="_"))
table(y$PRECIP_LO)

#############
# WRITE DATASET FULL NLA 2007 to cite in paper (before dropping obs for analysis)
# FOR POP ANALYSIS
#   w/ WGT_NLA>0
#   n=1123 lakes with 441 variables
#############
# All visits
write_csv(y,"data_processed/nla07/NLA_07_transformed_depth.csv")

## NLA 2007 TARGET LAKES VISIT=1 with WGT_NLA >0 # 1028 lakes - 1 lake missing depth, 1 missing RT
nla07_visit1<-y%>%
  filter(VISIT_NO==1)
write_csv(nla07_visit1,"data_processed/nla07/NLA_07_VISIT_1_WGT_USE.csv")



############################
# DATA EXPLORATION
# Look at temporal sampling
############################

## Load NLA 2007 modified data with depth corrected to merge
#   With VISIT_NO=1 and 2
#   n = 1250 obs
NLA07_org<- read_csv("data_processed/nla07/NLA_07_transformed_depth.csv")

##########
## When in the season were lakes sampled by regions
##########
str(NLA07_org$DATE_COL_iso)
# Julian date 
#library(lubridate)
# Convert date character to a Date class and specify current date format
NLA07_org$DATE_COL_iso <- as.Date(NLA07_org$DATE_COL_iso, 
                                  format="%m/%d/%Y")
str(NLA07_org$DATE_COL_iso)
NLA07_org$DATE_COL_JULIAN<-format(NLA07_org$DATE_COL_iso, "%j")

head(NLA07_org[c(5,390)]) # Looks good - checked using online julian day table

str(NLA07_org$DATE_COL_JULIAN)
# convert to numeric
NLA07_org$DATE_COL_JULIAN<-as.numeric(NLA07_org$DATE_COL_JULIAN)

summary(NLA07_org$DATE_COL_JULIAN)
# GROUP BY ECOREGION 
library(dplyr)
eco<- group_by(NLA07_org, ECOREG_use)

# MEAN LAKE HYDROLOGY
nla07_sample_date<-summarise(eco, 
                             median_juliandate=median(DATE_COL_JULIAN), 
                             mean_julian = mean(DATE_COL_JULIAN))

# Group by ecoregion and visit number
eco_visit<- group_by(NLA07_org, ECOREG_use, VISIT_NO)

# MEAN LAKE HYDROLOGY
nla07_VISIT_sample_date<-summarise(eco_visit, 
                                   median_juliandate=median(DATE_COL_JULIAN), 
                                   mean_julian = mean(DATE_COL_JULIAN),
                                   min_julian = min(DATE_COL_JULIAN),
                                   max_julian = max(DATE_COL_JULIAN))

write_csv(nla07_VISIT_sample_date,"data_processed/nla07/NLA07_sample_date_stats.csv")



####################
## LAKE CONNECTIVITY TYPE - from Read et al. 2015 - using LAGOS tools
##  10/30/17
####################

# NOTE there are 1094 observations in Read et al. whereas I have 1168 obs
# - may want to estimate connectivity on our own and see if we can fill in empty spots??

## Load data
#   Lake connectivity classes - downloaded on 10/26/17 from https://portal.lternet.edu/nis/mapbrowse?scope=knb-lter-ntl&identifier=10000&revision=1
NLA_read <- read_csv("data/NLA07/Read_et_al_2014_NLA_data.csv")

names(NLA_read) # We want Class and Class3 (condensed groupings - ISO/HW, DR, DR-UPLK)
table(NLA_read$Class)
# HW   SE   ST STLA 
#112  114  470  367

table(NLA_read$Class3)
# HWSE   ST STLA 
#  226  470  367

myvars<-c("SITE_ID","VISIT_NO.x","LAKENAME","Class","Class3")#, "ResTime")
NLA_read_red<-NLA_read[myvars]
names(NLA_read_red)
# Rename Visit number
names(NLA_read_red)[names(NLA_read_red)=="VISIT_NO.x"] <-"VISIT_NO"

table(NLA_read_red$VISIT_NO) # Only Visit 1
# 1 
# 1094

# Load NLA 2007 modified data with depth corrected n = 1028 with 442 variables
NLA07_org <- read_csv("data_processed/nla07/NLA_07_VISIT_1_WGT_USE.csv")

# Make dataset of lakes that are not in both
other<-NLA07_org[!(NLA07_org$SITE_ID %in% NLA_read_red$SITE_ID),]
# 93 observations

### Merge NLA 2007 dataset with the Read et al reduced dataset
NLA07<-merge(NLA07_org, NLA_read_red, by=c("SITE_ID","VISIT_NO"),all.x=TRUE)
# n=1063 - BUT THERE ARE DUPLICATES
table(NLA07$Class3)
# HWSE   ST STLA 
#   194  405  342  


# Rename Lake hydrology type labels
NLA07$Class3_f<-factor(NLA07$Class3, labels=c("Isolated","Drainage","Drainage-Lake"))
table(NLA07$Class3_f)
table(NLA07$Class3)
levels(NLA07$Class3_f)=c("Isolated","Drainage","Drainage-Lake")
NLA07$Class3_f <- ordered(NLA07$Class3_f, levels=c("Isolated","Drainage","Drainage-Lake"))

# Create new factor variable combining Lake_Origin_use and Class3_f
NLA07$lake_type <- with (NLA07, interaction(Class3_f,Lake_Origin_use,sep="_"))

table(NLA07$lake_type)

#     Isolated_MAN_MADE      Drainage_MAN_MADE Drainage-Lake_MAN_MADE 
#           64                    261                    217 
#     Isolated_NATURAL       Drainage_NATURAL  Drainage-Lake_NATURAL 
#           130                    144                    125 

#######
# DROP DUPLICATE OBSERVATIONS 
#######
NLA07<-NLA07[!duplicated(NLA07[,c(1,2)]),]
# n=1028

#############
## WRITE MODIFIED DATASETS - that include lake connectivity 
#############
write_csv(NLA07,"data_processed/nla07/NLA_07_transformed_CONN.csv")


##############
## ADD PALMER HYDROLOGIC DROUGHT INDEX - Average in Water Year
##  See Palmer_data_29MAY18.R for script working with original data layers
##  Marc Weber gathered values for lakes and emailed on 5/25/2018
##  10/8/18
##############

# LOAD processed NLA dataset with connectivity class
nla07<-read_csv("data_processed/nla07/NLA_07_transformed_CONN.csv")
nla07_mod<-nla07%>%
  select(!LAKENAME.y)%>%
  rename(LAKENAME=LAKENAME.x)

# LOAD processed PHDI data in Data>Climate data folder
phdi_07<- read_csv("data/NLA07/PHDI_NLA07_04JUN18.csv")
names(phdi_07)

# MERGE NLA07 and PHDI
test<-merge(nla07_mod,phdi_07,by="SITE_ID",all.x=TRUE)
test<-test[!duplicated(test[,c(1)]),]
names(test)

###############
## WRITE MODIFIED DATASET
write_csv(test,"data_processed/nla07/NLA_07_transformed_CONN_PHDI.csv")

names(test)
summary(test$hiiAll)


######################
## Create WATER ISOTOPE CLASSES
##
######################
nla07<-read_csv("data_processed/nla07/NLA_07_transformed_CONN_PHDI.csv")
names(nla07)

## E:I Classes
nla07$EI_class<-cut(nla07$E_I, breaks=c(0,0.20,0.50,Inf),labels=c("Low","Moderate","High"))

table(nla07$EI_class) 
plot(nla07$EI_class,nla07$E_I)

## WRT Classes
nla07$WRT_class<-cut(nla07$RT_iso, breaks=c(0,0.50,1.0,Inf),labels=c("Short","Moderate","Long"))
table(nla07$WRT_class)

############
## WRITE MODIFIED DATASET
write_csv(nla07,"data_processed/nla07/NLA_07_transformed_CONN_PHDI_ISO.csv")


#########################
## ADD LAKECAT VARIABLES - processed in Data>LakeCat>Scripts>Merge_NLA_LakeCat_08MAY19.R
#########################
## 6/25/19

## LOAD DATA
# NLA 2007 modified data
nla07<- read_csv("data_processed/nla07/NLA_07_transformed_CONN_PHDI_ISO.csv")

#LAKECAT compiled data for 2007 lakes
lkcat<-read_csv("data/NLA07/LakeCat_vars_nla07.csv")

## MERGE NLA07 and Reduced LakeCat dataset n=1028 w/545 variables
nla07_red<-merge(nla07,lkcat, by="SITE_ID")

### DERIVE VARIABLES
# WA:LA
nla07_red$WALA<-nla07_red$BASINAREA_06_sqkm/nla07_red$LkArea_km2
# Add Log transformed WA:LA
nla07_red$L_WALA <- log10(nla07_red$WALA)
summary(nla07_red$L_WALA)

# Consolidate glacial lithology classes
nla07_red$pctGlac <-nla07_red$PctGlacTilClayWs + nla07_red$PctGlacTilLoamWs + nla07_red$PctGlacTilCrsWs +
  nla07_red$PctGlacLakeCrsWs + nla07_red$PctGlacLakeFineWs

# Create new variable - fraction of stations with Flat and Gradual bank angle
nla07_red$bffFlat_grad <- nla07_red$bffFlat+nla07_red$bffGradual
summary(nla07_red$bffFlat_grad)
names(nla07_red)
todrop<-names(nla07_red)%in%c("X","X.x","X.y","X","X.1","X.x.1","X.y.1","X.x.2",
                              "X.y.2","X.x.3","X.y.3","X.x.4","X.y.4",
                              "VISIT_NO.y","LONdd_use.y","LATdd_use.y",
                              "...1","...1.x","X.x...3","X.y...8","X.x...16",
                              "X.y...19", "X.x...56","X.y...65","X.x...68",
                              "X.y...71","X.x...78","X.y...85","...1.y")
nla07_red<-nla07_red[!todrop]
names(nla07_red)

# rename variables
names(nla07_red)[names(nla07_red)=="VISIT_NO.x"] <- "VISIT_NO"
names(nla07_red)[names(nla07_red)=="LATdd_use.x"] <- "LATdd_use"
names(nla07_red)[names(nla07_red)=="LONdd_use.x"] <- "LONdd_use"
names(nla07_red)


####################
## ADD MODIFIED DRAWDOWN MEASURES
## Phil emailed 2/11/19
##  Processed in Data>Additional_NLA_data>From_Phil_11FEB19>Scripts>NLA0712_create_datasets_modified_DD_12FEB19.R

## LOAD DATA
# Modified Drawdown for 2007 lakes (n= 1252 obs)
dd<-read_csv("data/NLA07/NLA07_modDD_only.csv")

# MERGE NLA07 dataset with modified drawdown variables # n=1028 with 551 variables
nla07_dd<-merge(nla07_red,dd, by=c("SITE_ID","VISIT_NO"))
names(nla07_dd)

####################
## ADD UPDATED POPULATION WEIGHTS 6/25/19
####################
## NLA POPULATION WEIGHTS - UPDATED 6/24/19 - processed in Analysis>NLA_weighted_calculations>Scripts>a_UPDATED_NLA_POP_WGTS_24JUN19.R
nla07wt <- read_csv("data/NLA07/NLA07_popwts_25JUN19.csv")


#############
## MERGE with updated population weights n=1028 with 455 variables
nla07_dd_wt<-merge(nla07_dd,nla07wt,by="SITE_ID")#all.x=TRUE

names(nla07_dd_wt)

# Drop some variables
todrop <-names(nla07_dd_wt)%in%c("X.1","X.1.x","X.x","X.1.y","X.y",
                                 "...1.x", "...1.y")
nla07_dd_wt<-nla07_dd_wt[!todrop]


##################################
## WRITE DATASET and Variables
write_csv(nla07_dd_wt,"data_processed/nla07/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

# Variables Names
nla07_vars<-as.data.frame(names(nla07_dd_wt))
write_csv(nla07_vars,"data_processed/nla07/NLA07_ALL_dataset_variable_names.csv")
