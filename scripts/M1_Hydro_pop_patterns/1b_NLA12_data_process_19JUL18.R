#####################
## NLA 2012 Data Processing
## TRIMMED DOWN FOR DOCUMENTATION
## for NLA lake hydrology spatial patterns
##################### 
## DATE: 7/27/17
## 8/18/17 UPDATE
#   Added monthly climate data\
#   Factor variables for lake hydro type
#   Transformed variables

# 8/22/17
# Calculated lake shoreline development
#  SLD = PERIM_KM/ (2* sqrt(LkArea_km2 * pi))

# 8/23/17
# Added variables from Site info dataset
# After talking with Tony Olsen and Tom Kincaid on using spsurvey to infer population estimates
# Needed to add other variables: XCOORD, YCOORD, COMID2012,COMID2007, COMIDS2007, CH0712_CAT,CH0712_WGT, CH0712_USE, CAT_UNIQUE, BORD_LAKE,
# TNT, TNT_ST

# 8/24/17
# Added WSA9_LO - groups by ecoregion and lake hydro type

# 8/30/17
# Added lake volume output from lakemorpho R package - Marc Weber ran code and emailed output

## 9/10/17
# Added updated E:I values that had undergone more careful checks
# Used Source_water_selection_method_2012_26JUL17.xlsx as way to select method for input precip values
# Then copied input precip into E_I_Modeling_NLA_2012_01SEPT17_to_modify.xlsx
# Calculated E:I - took proportion value
## DROPPED some observations where it was not possible to get E:I (see EI_estimates_second_run_01SEPT17.xlsx 
#   these were revisted lakes within 2012: NLA12_GA-101 Visit 1; NLA12_WA-101 Visit 2; NLA12_WA-133 Visit 2

## Also added lake volume
# Saved data in a separate spreadsheet - E:I_nla12_10SEPT17.csv (in NLA_2012> Derived_datasets)
# Will add WRT after Runoff is calculated by Marc

## 9/11/17
# Added E:I and lake morpho output in the first merge that includes SITE_ID and VISIT_NO
# Noticed that some lakes with repeat visits had 4 observations - something was duplicating when merging

## 9/18/17
# Noticed that there were 8 duplicates for two lakes (NLA12_PA-107,NLA12_WI-128)
# Dropped the duplicated records and just kept one per SITE_ID and VISIT_NO

## 9/26/17
# Max depth - spoke with Phil 9/25/17
#  We decided that for lakes with max depth ~40m in the NLA should be checked with lit values
#  Where max depths are much deeper than the 50 m cutoff, we will manually input max depth from the lit.
#  I created two columns - one indicating the source of max depth, the other with the updated max depth to use in future analyses

## 10/16/17
# Calculated cumulative potential evapotranspiration (m) - before had average PET

## 10/23/17
# Converted TN from mg/L to ug/L to have units of measurement match 2007 dataset

## 10/30/17
# There were 6 lakes with negative E:I values - worked with Renee to correct these - 
# see "E_I_Modeling_NLA_2012_01SEPT17_to_modify.xlsx" and "a_notes_isotopic_water_balance_calcuation_process.doc" for steps taken 

# Also added Residence Time estimates (first round average values - waiting to compare with runoff estimates) 
#   Where average RT was not calculated I took precipitaiton determined RT 

## 11/1/17
# Added Population density and dam density from Landscape NLA spreadsheet

# 11/13/17 
# Created dataset of lakes equal to or greater than 4ha to compare with NLA07

## 11/15/17 - added lake morpho output from Hollister

# 12/7/17 - dropped lakes with strange E:I values

# 1/3/18 - Precipitation class composition

# 1/11/18 - For single lake dataset - only retained VISIT_NO=1

# 2/7/18 - Added watershed elevation

# 2/14/18 - created dataset of just small systems

# 4/12/18 - Calculated scaled vertical drawdown using modified max depth

# 5/10/18 -  Want to clarify number of lakes have zero weights, number of lakes with HorizDD= NA
#                   For paper, would like to state that NLA12 = 1038 lakes with weights, 
#                   For the analysis, it's okay that dataset was smaller because there are lakes without DD measured etc - but need to document why there are differences
#                   Population analysis can handle missing data - so don't drop observations
# 5/11/18 - phab has 1018 observations with VISIT=1 - we may want to merge this manually later so can get 1038
# 7/26/18 - updated E:I and RT_iso estimates
# 8/1/18 - updated E:I and RT_iso estimates - had to fix slope method and had Renee review the values
#           And have to change Lat/Long, Max depth, Area
# 9/12/18 - Updated E:I - was missing a few lakes and needed to correct flux weighted measurements from previous fix
# 10/8/18 - Added PHDI (Palmer Hydro Drought Index) Average during the survey water year
# 10/31/18 - Added Water Isotope Classes
# 4/30/19 - updated pathways to OneDrive
# 6/24/19 - Updated Lake Volume and water residence times
# 6/26/19 - updated population weights from Tony 6/24/19 - Updated weights based on aggregated ecoregions (not State) 
#             and lake size categories and adjusted weights to take into account the 2007 lakes 
#             that were evaluated but not sampled (n=1006) in addition to the sampled lakes (n=1028)
#           ALSO added Lake Cat variables and modified drawdown measures from Phil (2/11/19)
#           Use LAKE_ORIGIN in Site dataset - not Lake_Origin_use because missing 20 observations
# 7/4/19 - updated three lakes with zmax from literature; updated depth, volume, wrt, and water yield estimates

#
###################
## ORIGINAL DATA
# LAKE ISOTOPE folder M:\Net MyDocuments\a_Water_Level\Data\
#  "nla0712isodrawdown_20160219.csv"

# NARS Original data posted on https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys
##   Saved as .csv in M:\Net MyDocuments\a_Water_Level\Data\NLA_2012\NLA_2012_original_data: 
## "NLA2012_key_variables","NLA2012_Site_information","NLA2012_water_chemistry",
##  "NLA2012_phab_metrics","NLA2012_secchi","NLA2012_chlorophyll","NLA2012_toxin",
##  "NLA2012_condition","NLA2012_wide_landscape",
## "NLA2012_wide_assement_visual","NLA2012_wide_landscape"

## MERGED NLA 2012 data vars - \a_Water_Level\Data\NLA_2012\Scripts\NLA2012_data_explore_03APR17.R
## Processed data: NLA12_merge.csv (in \a_Water_Level\Data\NLA_2012\Modified_data)


# E:I estimated values - Derived from precip & lake isotope data
## - see a_Water_Level\Analysis\Stable_isotope_calculations\2012\EI_calculations
##  Data calculations (original but modified): E_I_Modeling_NLA_2012_07JUL17_to_modify.xls
##  Selection method: Source_water_selection_method_2012_26JUL17.xls
##  Processed data (first run): E_I_nla12_27JUL17.csv (in a_Water_Level\Data\NLA_2012\Derived_datasets)


## 8/6/18 E:I & RT UPDATE
## see a_Water_Level\Analysis\Stable_isotope_calculations\2012\Update_23JUL18
## Data calculations: E_I_Modeling_NLA_2012_23JUL19_to_modify.xls
## Selection method: Source_water_selection_method_2012_24JUL18.xls
## Source decision: EI_source_decision_25JUL18.xlsx  
## Processed data: E_I_nla12_modified_02AUG18.csv (in Data\NLA_2012\Derived_datasets )


# NLCD 2011 - tabulated for NLA 2012 lake watersheds - July 2017
## 
## tabulated in a_Water_Level\Analysis\Summarize_geo_data\Script\a_tabluate_NLCD_2012_AREA_sqkm_Marc.R
## Processed data: NLCD2011_NLA2012_basins_sqkm_area.csv (in \a_Water_Level\Data\NLA_2012\Derived_datasets)


# Average Climate data by lake point
## Averages calculated in a_Water_Level\Data\Climate data\Raw climate data 2012 for isotope parameter calc\Climate_data_NLA12_point_averages.xls
## Processed data: Avg_climate_lk_pt.csv (in a_Water_Level\Data\NLA_2012\Derived_datasets)
## 8/18/17 - added monthly climate data for 2011-2012

## Steps & Decisions 
# Retaining variables that were determine for the NLA 2007 data reviewed with Renee and Phil
# Tabulated NLCD land use/land cover within watersheds (NLA 2012 lakes) using Marc's code 
# E:I derived using precip isotope composition data from Gabe Bowen precip isoscape interpolated data (waterisotopes.org) 
## WRT using Volume estimate
###################################

rm(list=ls())

###########
# Libraries
###########
library(tidyverse)
library(maps)
library(mapdata)
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

## Lake isotope and drawdown data from Phil
isodata<-read_csv("data/NLA12/a_nla0712_isodrawdown_20160219.csv")
names(isodata)
# WHich lakes are missing isotope
iso_na_all<-isodata[which(is.na(isodata$d18O)),]

# Retain 2012 measurements - drop 2007 data
isodata_12<- isodata[which(isodata$YEAR == 2012),] # keeps 1240 records out of 2492

########################
## NLA 2012 data from online
#  Set working directory to load NLA 2012 data
setwd("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/NLA_2012_original_data")

# Key variable
key.vars<-read_csv("data/NLA12/NLA2012_key_variables.csv") #WGT_ALL - adjusted weight?

# Site information
site<- read_csv("data/NLA12/NLA2012_Site_information.csv")

# water chemistry
chem <- read_csv("data/NLA12/NLA2012_water_chemistry.csv")

# chl-a
chla<- read_csv("data/NLA12/NLA2012_chlorophyll.csv")

# toxin
toxin <- read_csv("data/NLA12/NLA2012_toxin.csv")

# secchi
secchi<- read_csv("data/NLA12/NLA2012_secchi.csv")

#phab
phab<-read_csv("data/NLA12/NLA2012_phab_metrics.csv") # there are 1018 VISIT=1 = maybe merge later

# condition
condition<- read_csv("data/NLA12/NLA2012_condition.csv")

# visual
visual <- read_csv("data/NLA12/NLA2012_wide_assement_visual.csv")

# landscape basin NLCD 2006 that is included in NLA 2012
lulc<- read_csv("data/NLA12/NLA2012_wide_landscape.csv")


## NOTE: water chemistry, chla are missing SITE_ID
ID12<-site[,c(1:4,68,78)] # SITE_ID and UID

# merge SITE_ID to water chem and chla datasets
chem.12<- dplyr :: inner_join(chem, ID12, by ="UID")
names(chem.12)

chla.12<- dplyr :: inner_join(chla, ID12, by = "UID")

#############
## SEE NLA1012_data_explore_03APR17.R for variable reduction of NLA datasets
#############

########################
## CLEAN UP 2012 ISOTOPE DATASET
## Manually enter isotope and d-excess for two lakes that were missing isotope info
########################
## 8/31/18 ## ADD Water Isotope and d-excess values to lakes - from Renee's email 8/8/18
#############
# Create dataset of lakes that were missing isotope values
z<-subset(isodata_12,(SITE_ID %in% c("NLA12_KS-114","NLA12_WI-129")))
head(z[,c(1:9)])
head(isodata_12[,c(1:9)])

# MANUALLY ENTERING WATER ISOTOPE DATA FOR two lakes
# Update date, dD, d18O, and d-excess
dates<- c("5/22/2012","8/7/2012")
DATE_COL_iso <-as.Date(dates,
                       format = "%m/%d/%Y")
z$DATE_COL_iso <- DATE_COL_iso
# Hydrogen
dD <-c(-17.43,-46.72)
z$dD <-dD
# Oxygen
d18O <-c(-1.87,-5.75)
z$d18O<-d18O
# d-excess
d_excess <- c(-2.47, -0.75)
z$d_excess<-d_excess

head(z[,c(1:9)])

###########
## Create dataset of all other lakes in the 2012 isotope dataset (n=1238 vs 1240)
other<-isodata_12[!(isodata_12$SITE_ID %in% c("NLA12_KS-114","NLA12_WI-129")),]
# FORMAT data column
other$DATE_COL_iso<- as.Date(other$DATE_COL_iso,
                             format="%m/%d/%Y")
head(other[,c(1:9)])

#############
## Bind two dataframes together
isodata_12_b<- rbind(other,z)
head(isodata_12_b[,c(1:9)])
tail(isodata_12_b[,c(1:9)])

# Order by SITE_ID
isodata_12_b <-isodata_12_b[order(isodata_12_b$SITE_ID),]
tail(isodata_12_b[,c(1:9)])

t<-isodata_12_b[which(isodata_12_b$SITE_ID=="NLA12_KS-114"),]


#############
## Reduce variables in dataset
##  write reduced files into folder to merge together
#############
##########
# Lake isotope data 2012 - Keep all
write_csv(isodata_12_b,"data_to_merge_NLA12/multimerge_nla12/a_lk_iso12.csv")

############
#### NLA 2012 individual datasets - reduced variables
## Site information
names(site)
site.vars <- c("SITE_ID","VISIT_NO","UID","DATE_COL","AGGR_ECO3_2015","AGGR_ECO9_2015",
               "AREA_HA","BORD_LAKE","CAT_UNIQUE","CH0712_CAT","CH0712_USE","CH0712_WGT","COMID2007","COMID2012","COMIDS2007",
               "ELEVATION","EPA_REG","EVALSTAT","HUC2","HUC8","LAKE_ORIGIN",
               "LAKE_ORIGIN12","LAT_DD83","LON_DD83","EVAL_NAME","NARS_NAME","PERIM_KM","RCHCODE","EVALSTAT","STATUS","RT_NLA12",
               "SITEID_07","SITETYPE","SIZE_CLASS","STATE","STRATUM","WGT_ALL","WGT_CAT","XCOORD","YCOORD","REF_NLA12_NUTR","RT_NLA12_BENT","RT_NLA12_ZOOP",
               "TNT")
site.red<-site[site.vars]

write_csv(site.red, "data_to_merge_NLA12/multimerge_nla12/a_site_info.csv")


## KEY variables - need to get depth at sample location (which I think is the approximate max depth of the lake)
names(key.vars) # n = 1138 obs
vars.k <- c("SITE_ID","VISIT_NO","UID","INDEX_SITE_DEPTH")
key.red <-key.vars[vars.k]

write_csv(key.red, "data_to_merge_NLA12/multimerge_nla12/j_key_zmax.csv")

########
## Water chemistry - Used the merged data (chem.12) that has SITE_ID and VISIT_NO
names(chem.12)

# Added units to column headings for variables - note - some chemistry obs did not have units indicated - BUT I think we can be confident that they are measured the same as the majority of observations
# "AMMONIA_N_UNITS","ANC_UNITS","CALCIUM_UNITS","CHLORIDE_UNITS","COLOR_UNITS","COND_UNITS","DOC_UNITS","MAGNESIUM_UNITS","NITRATE_N_UNITS","NITRATE_NITRITE_N_UNITS","NITRITE_N_UNITS","NTL_UNITS","PH_UNITS","POTASSIUM_UNITS","PTL_UNITS","SILICA_UNITS","SODIUM_UNITS","SULFATE_UNITS","TOC_UNITS","TSS_UNITS","TURB_UNITS",

chem.vars<- c("SITE_ID","VISIT_NO","SAM_CODE",
              "ALUMINUM_RESULT","AMMONIA_N_RESULT","ANC_RESULT","CALCIUM_RESULT","CHLORIDE_RESULT","COLOR_RESULT","COND_RESULT","DOC_RESULT","MAGNESIUM_RESULT","NITRATE_N_RESULT","NITRATE_NITRITE_N_RESULT","NITRITE_N_RESULT","NTL_RESULT","PH_RESULT","POTASSIUM_RESULT","PTL_RESULT","SILICA_RESULT","SODIUM_RESULT","SULFATE_RESULT","TOC_RESULT","TSS_RESULT","TURB_RESULT",
              "ALUMINUM_BATCH_ID","AMMONIA_N_BATCH_ID","ANC_BATCH_ID","CALCIUM_BATCH_ID","CHLORIDE_BATCH_ID","COLOR_BATCH_ID","COND_BATCH_ID","DOC_BATCH_ID","MAGNESIUM_BATCH_ID","NITRATE_N_BATCH_ID","NITRATE_NITRITE_N_BATCH_ID","NITRITE_N_BATCH_ID","NTL_BATCH_ID","PH_BATCH_ID","POTASSIUM_BATCH_ID","PTL_BATCH_ID","SILICA_BATCH_ID","SODIUM_BATCH_ID","SULFATE_BATCH_ID","TOC_BATCH_ID","TSS_BATCH_ID","TURB_BATCH_ID",             
              "ANC_FLAG","CALCIUM_FLAG","CHLORIDE_FLAG","COLOR_FLAG","COND_FLAG","DOC_FLAG","MAGNESIUM_FLAG","NITRATE_N_FLAG","NITRATE_NITRITE_N_FLAG","NTL_FLAG","PH_FLAG","POTASSIUM_FLAG","PTL_FLAG","SILICA_FLAG","SODIUM_FLAG","SULFATE_FLAG","TSS_FLAG","TURB_FLAG","CHEM_SAMPLE_ID")  

chem.red<-chem.12[chem.vars]
head(chem.red)


# Rename water chemistry column headings to include units
names(chem.red) <- c("SITE_ID","VISIT_NO","SAM_CODE","ALUMINUM_RESULT_mgL","AMMONIA_N_RESULT_mgL","ANC_RESULT_ueqL",
                     "CALCIUM_RESULT_mgL","CHLORIDE_RESULT_mgL","COLOR_RESULT_PtCo","COND_RESULT_uscm",
                     "DOC_RESULT_mgL","MAGNESIUM_RESULT_mgL","NITRATE_N_RESULT_mgL","NITRATE_NITRITE_N_RESULT_mgL","NITRITE_N_RESULT_mgL","NTL_RESULT_mgL",
                     "PH_RESULT","POTASSIUM_RESULT_mgL","PTL_RESULT_ugL","SILICA_RESULT_mgL","SODIUM_RESULT_mgL","SULFATE_RESULT_mgL",
                     "TOC_RESULT_mgL","TSS_RESULT_mgL","TURB_RESULT_NTU",
                     "ALUMINUM_BATCH_ID","AMMONIA_N_BATCH_ID","ANC_BATCH_ID","CALCIUM_BATCH_ID","CHLORIDE_BATCH_ID","COLOR_BATCH_ID","COND_BATCH_ID","DOC_BATCH_ID",
                     "MAGNESIUM_BATCH_ID","NITRATE_N_BATCH_ID","NITRATE_NITRITE_N_BATCH_ID","NITRITE_N_BATCH_ID","NTL_BATCH_ID","PH_BATCH_ID","POTASSIUM_BATCH_ID","PTL_BATCH_ID",
                     "SILICA_BATCH_ID","SODIUM_BATCH_ID","SULFATE_BATCH_ID","TOC_BATCH_ID","TSS_BATCH_ID","TURB_BATCH_ID",
                     "ANC_FLAG","CALCIUM_FLAG","CHLORIDE_FLAG","COLOR_FLAG","COND_FLAG","DOC_FLAG","MAGNESIUM_FLAG","NITRATE_N_FLAG","NITRATE_NITRITE_N_FLAG","NTL_FLAG",
                     "PH_FLAG","POTASSIUM_FLAG","PTL_FLAG","SILICA_FLAG","SODIUM_FLAG","SULFATE_FLAG","TSS_FLAG","TURB_FLAG","CHEM_SAMPLE_ID")

names(chem.red)

write_csv(chem.red,"data_to_merge_NLA12/multimerge_nla12/b_chem.csv")

##########
## Chla - - Used the merged data (chla.12) that has SITE_ID and VISIT_NO
# dropped units and changed variable heading to indicate units of measurement
names(chla.12)
table(chla.12$CHLL_UNITS) # ugL
table(chla.12$CHLX_UNITS)

chla.vars<-c("SITE_ID","VISIT_NO",
             "CHLL_RESULT","CHLL_BATCH_ID","CHLL_QA_FLAG")

chla.red <- chla.12[chla.vars]

# Renamed to include units
names(chla.red)[names(chla.red)=="CHLL_RESULT"]<-"CHLL_RESULT_ugL"

write_csv(chla.red,"data_to_merge_NLA12/multimerge_nla12/c_chla.csv")

##########
## Toxin
## dropped units and changed variable heading to indicate units of measurement
names(toxin)
table(toxin$MICL_UNITS) #ppb
toxin.vars<-c("SITE_ID","VISIT_NO",
              "MICL_RESULT","MICL_BATCH_ID","MICL_QA_FLAG")

toxin.red <- toxin[toxin.vars]
names(toxin.red)[names(toxin.red)=="MICL_RESULT"]<-"MICL_RESULT_ugL"
names(toxin.red)

write_csv(toxin.red,"data_to_merge_NLA12/multimerge_nla12/d_toxin.csv")

##########
## Secchi
names(secchi)

secchi.vars<- c("SITE_ID","VISIT_NO",
                "SECCHI","CLEAR_TO_BOTTOM")

secchi.red<- secchi[secchi.vars]
names(secchi.red)[names(secchi.red)=="SECCHI"]<-"SECCHI_m"

write_csv(secchi.red,"data_to_merge_NLA12/multimerge_nla12/e_secchi.csv")


## Physical habitat
names(phab)
phab.vars<-c("SITE_ID","VISIT_NO","DATE_COL",
             "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
             "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE","BFXHORIZDIST","BFXHORIZDIST_DD","BFXVERTHEIGHT","BFXVERTHEIGHT_DD",
             "BSFCBEDROCK","BSFCBOULDERS","BSFCCOBBLE","BSFCGRAVEL","BSFCORGANIC","BSFCSAND","BSFCSILT","BSFCWOOD",             
             "BSISITEVARIETY","BSISTAVARIETY","BSVLDIA","BSXLDIA",
             "FCFCAQUATIC","FCFCAQUATIC_DD","FCFCAQUATIC_LIT","FCFCAQUATIC_SIM",
             "FCFCBOULDERS","FCFCBOULDERS_DD","FCFCBOULDERS_LIT","FCFCBOULDERS_SIM",
             "FCFCBRUSH","FCFCBRUSH_DD","FCFCBRUSH_LIT","FCFCBRUSH_SIM",
             "FCFCLEDGES","FCFCLEDGES_DD","FCFCLEDGES_LIT","FCFCLEDGES_SIM",
             "FCFCLIVETREES","FCFCLIVETREES_DD","FCFCLIVETREES_LIT","FCFCLIVETREES_SIM",
             "FCFCOVERHANG","FCFCOVERHANG_DD","FCFCOVERHANG_LIT","FCFCOVERHANG_SIM",
             "FCFCSNAG","FCFCSNAGS_DD","FCFCSNAGS_LIT","FCFCSNAGS_SIM",
             "FCFCSTRUCTURES","FCFCSTRUCTURES_DD","FCFCSTRUCTURES_LIT","FCFCSTRUCTURES_SIM",
             "FCFPALL","FCFPALL_DD","FCFPALL_LIT","FCFPALL_SIM",
             "HIFPANY_SYN","HIFPANYCIRCA_SYN","HIIAG_SYN","HIIAGCIRCA_SYN","HIIALL_SYN","HIIALLCIRCA_SYN","HIINONAG_SYN","HIINONAGCIRCA_SYN",
             "HIPWAG_SYN","HIPWALL_SYN","HIPWBUILDINGS_SYN","HIPWCOMMERCIAL_SYN","HIPWCROPS_SYN","HIPWDOCKS_SYN","HIPWLANDFILL_SYN","HIPWLAWN_SYN","HIPWNONAG_SYN","HIPWORCHARD_SYN","HIPWOTHER_SYN","HIPWPARK_SYN","HIPWPASTURE_SYN","HIPWPOWERLINES_SYN","HIPWROADS_SYN","HIPWWALLS_SYN",
             "HORIZDD","VERTDD",
             "LitCvrQc3OE","LitRipCvrQc3OE","RDis_IX","RVegQc3OE",
             "RVFCCANBIG","RVFCCANBIG_DD","RVFCCANBIG_RIP","RVFCCANBIG_SYN",
             "RVFCCANSMALL","RVFCCANSMALL_DD","RVFCCANSMALL_RIP","RVFCCANSMALL_SYN",
             "RVFCGNDBARE","RVFCGNDBARE_DD","RVFCGNDBARE_RIP","RVFCGNDBARE_SYN",
             "RVFCGNDINUNDATED","RVFCGNDINUNDATED_DD","RVFCGNDINUNDATED_RIP","RVFCGNDINUNDATED_SYN",
             "RVFCGNDNONW","RVFCGNDNONW_DD","RVFCGNDNONW_RIP","RVFCGNDNONW_SYN",
             "RVFCGNDWOODY","RVFCGNDWOODY_DD","RVFCGNDWOODY_RIP","RVFCGNDWOODY_SYN",
             "RVFCUNDNONW","RVFCUNDNONW_DD","RVFCUNDNONW_RIP","RVFCUNDNONW_SYN",
             "RVFCUNDWOODY","RVFCUNDWOODY_DD","RVFCUNDWOODY_RIP","RVFCUNDWOODY_SYN",
             "RVICANOPY","RVICANOPY_DD","RVICANOPY_RIP","RVICANOPY_SYN",
             "RVICANUND","RVICANUND_DD","RVICANUND_RIP","RVICANUND_SYN",
             "RVIGROUND","RVIGROUND_DD","RVIGROUND_RIP","RVIGROUND_SYN",
             "RVIHERBS", "RVIHERBS_DD","RVIHERBS_RIP","RVIHERBS_SYN",
             "RVITALLWOOD","RVITALLWOOD_DD","RVITALLWOOD_RIP","RVITALLWOOD_SYN",
             "RVITOTALVEG","RVITOTALVEG_DD","RVITOTALVEG_RIP","RVITOTALVEG_SYN",
             "RVIUNDERSTORY","RVIUNDERSTORY_DD","RVIUNDERSTORY_RIP","RVIUNDERSTORY_SYN",
             "RVIWOODY","RVIWOODY_DD","RVIWOODY_RIP","RVIWOODY_SYN",
             "SIVDEPTH","SIXDEPTH",
             "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",          
             "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA")

phab.red<- phab[phab.vars]

write_csv(phab.red,"data_to_merge_NLA12/multimerge_nla12/f_phab.csv" )


## Visual
names(visual)
visual.vars<- c("SITE_ID","VISIT_NO","DATE_COL",
                "AGR_CROPLAND","AGR_FEEDLOT","AGR_LIVESTOCK","AGR_ORCHARDS","AGR_PASTURE","AGR_POULTRY","AGR_SCORE","AGR_STRING","AGR_WITHDRAWAL",          
                "APPEALING","BIOTIC_INTEGRITY",
                "IND_COMMERCIAL","IND_FIRE","IND_INDUSTRIAL","IND_LOGGING","IND_MINES","IND_ODORS","IND_OIL","IND_POWER","IND_SCORE","IND_STRING",              
                "HYDRO_TYPE","LAKE_LEVEL","LEVEL_CHANGE_M","LEVEL_CHANGES",
                "MAN_DRINKING_WATER","MAN_LEVEL_FLUCTUATIONS","MAN_SCORE","OUTLET_DAMS",
                "PRISTINE","RCH_AGRICULTURE","RCH_BARE_GROUND","RCH_DEVELOPMENT","RCH_FOREST", "RCH_GRASS","RCH_SHOREMODS","RCH_SHRUB","RCH_WETLAND",       
                "REC_FILMS","REC_MARINAS","REC_PARKS","REC_PRIMITIVE","REC_RESORTS","REC_SCORE","REC_STRING","REC_TRAILS","REC_TRASH",             
                "RECREATIONAL_ACTIVITY","RECREATIONAL_VALUE","RES_BRIDGES","RES_CONSTRUCTION","RES_DUMPING","RES_LAWNS","RES_PIPES","RES_RESIDENCES","RES_ROADS","RES_SCORE",             
                "RES_SEWAGE","RES_STRING","SWIMMABILITY","TROPHIC_STATE")

visual.red<- visual[visual.vars]

write_csv(visual.red,"data_to_merge_NLA12/multimerge_nla12/g_visual.csv" )


## Condition - but this is a smaller dataset than the others so may want to join later
names(condition)

cond.vars<-c("SITE_ID","VISIT_NO",
             "LAKE_ORIGIN12","BENT_COND","CHLA_COND","NTL_COND","PTL_COND","DRAWDOWN_COND",
             "LITCVR_COND","LITRIPCVR_COND","RDIS_COND","RVEG_COND",
             "SEDHG_MTH_TOP_COND","SEDHG_TOT_BTM_COND","SEDHG_TOT_TOP_COND",
             "CHLL_REC","CYNL_REC","MICL_REC","WSA9_LO")

cond.red <- condition[cond.vars]

write_csv(cond.red,"data_to_merge_NLA12/lk_condition.csv" )


## Landscape variables provided in NLA12 - only one obs per lake so merge after
names(lulc)

#2/7/18 - reduced just elevation
lulc.vars <- c("SITE_ID","DOMGEOL_BSN", "DAMCNT_BSN","DAMDEN_BSN",
               "ELEV_SITE","ELEVMAX_BSN","ELEVMEAN_BSN","ELEVMIN_BSN",
               "GNEISSAREA_BSN","GNEISSPCT_BSN","GRANITICAREA_BSN","GRANITICPCT_BSN",
               "HOUSEDEN_BSN","MAFULAREA_BSN","MAFULPCT_BSN",
               "PERMRATE_BSN","PERMRATE_SITE",
               "POPDEN_BSN","PSUM6M_BSN","PSUMPY_BSN","QTRNRYAREA_BSN","QTRNRYPCT_BSN","ROADDEN_BSN",
               "SANDPCT_BSN","SITE_GEOLOGY",
               "PMEAN_BSN","PMIN_BSN","PMAX_BSN","PIP_BSN",
               "TIP_BSN",
               "TMEAN_BSN","TMAX_BSN","TMIN_BSN",
               "TMEANSD_BSN","TMINSD_BSN","TMAXSD_BSN",
               "PMIN_PT","PMAX_PT","PMEAN_PT","PIP_PT",
               "TIP_PT","TMIN_PT","TMAX_PT","TMEAN_PT","TMEANPW_PT","TMEANPY_PT")#,)

lulc.red <- lulc[lulc.vars]

write_csv(lulc.red,"data_to_merge_NLA12/f_landscape.csv" )

############
###  Derived E:I estimates UPDATED 9/10/17
# 8/2/18 - Updated E:I & RT values
# 9/10/17 - Updated E:I values and lake volume
# 9/12/18 - Updated E:I values and lake volume
# 6/24/19 - Updated lake volume and WRT
# 7/4/19  - Updated lake volume and WRT for 3 lakes with zmax from literature OLD E_I_nla12_19JUN19.csv
EI<- read_csv("data/NLA12/E_I_nla12_04JUL19.csv")

names(EI) # n=1237 obs; 16 variables

# Reduce dataset to just important variables
myvars<- c("SITE_ID", "VISIT_NO", "UID","E_I","Water_yield_m","Modeled_Water_yield_m",
           "RT_iso","RT_modeled","Volume_Corrected_m3","Lake_precip_m","Lake_evap_m",
           "P_E_deficit_m","Wyield_P_E_m")
EI_red <-EI[myvars]

## MERGE EI dataset with Site info dataset (n=2770 obs w/54 variables)
test<- merge(site.red,EI_red,by=c("SITE_ID","VISIT_NO"),all.x=TRUE)
test2<-test[which(test$E_I>2),] # 4 observations
head(test[c(1:3,26)])
test2[c(1:3,26,37, 44,45)]
#          SITE_ID VISIT_NO  UID   NARS_NAME   WGT_ALL    TNT       E_I
#1947 NLA12_SD-146        1 8625             127.80658 Target  2.429994
#2513 NLA12_VA-R18        1 7738                    NA Target  4.275364
#2533 NLA12_WA-101        1 7179 Island Lake  92.29875 Target  2.830230
#2534 NLA12_WA-101        2 8157 Island Lake  92.29875 Target 20.108093

summary(test$E_I)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   -1.1240  0.0912  0.2082  0.3082  0.3965 20.1100    1535 

####################
# CLEAN UP E:I estimates
#   Put NAs where values are not reliable
#     for E:I, RT_iso, Modeled_Water_yield,Water_yield_m

### Observations with negative E:I
#GA-101 (VISIT_NO=1) - negative E:I 
z<-test[which(test$UID.x == 6293),]
z[c("SITE_ID","VISIT_NO","UID.x","E_I","RT_iso")] # 

# Make NAs in EI_red dataset that will be merged
EI_red$E_I[EI_red$UID==6293]<- NA
EI_red$RT_iso[EI_red$UID==6293]<- NA
EI_red$Modeled_Water_yield_m[EI_red$UID==6293]<- NA
# Checking to see if made NA - yes GA<- EI_red[which(EI_red$UID==6293),]

#WA-141 (VISIT_NO=1) - negative E:I 
z<-test[which(test$UID.x==7148),]
z[c("SITE_ID","VISIT_NO","UID.x","E_I","RT_iso")]
EI_red$E_I[EI_red$UID==7148]<- NA
EI_red$RT_iso[EI_red$UID==7148]<- NA
EI_red$Modeled_Water_yield_m[EI_red$UID==7148]<- NA
WA <-EI_red[which(EI_red$UID==7148),]


### Observations with really high E:I 
#  FOR WA-101 VISIT_NO = 2
z<-EI_red[which(EI_red$UID==8157),]
z[c("SITE_ID","VISIT_NO","UID","E_I","RT_iso")]
#           SITE_ID VISIT_NO  UID      E_I   RT_iso
#1106 NLA12_WA-101        2 8157 20.10809 22.23785
EI_red$E_I[EI_red$UID==8157]<- NA
EI_red$RT_iso[EI_red$UID==8157]<- NA
EI_red$Modeled_Water_yield_m[EI_red$UID==8157]<- NA
WA2<-EI_red[which(EI_red$UID==8157),] # Looks good

#  FOR VA-R18 VISIT_NO = 1 - E:I > 4
z<-EI_red[which(EI_red$UID==7738),]
z[c("SITE_ID","VISIT_NO","UID","E_I","RT_iso")]
#           SITE_ID VISIT_NO  UID      E_I   RT_iso
# 1090 NLA12_VA-R18        1 7738 4.275364 4.759954

EI_red$E_I[EI_red$UID==7738]<- NA
EI_red$RT_iso[EI_red$UID==7738]<- NA
#EI_red$Modeled_Water_yield_m[EI_red$UID==7738]<- NA
VA2<-EI_red[which(EI_red$UID==7738),] # Looks good

summary(EI_red$E_I) # NA=6
# n = 1237
summary(EI_red$RT_iso)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0000  0.2682  0.5188  0.8085  0.9415 27.3200      33 

write_csv(EI_red,"data_to_merge_NLA12/multimerge_nla12/h_EI_24JUN19.csv") # I copied the old one and pasted one folder back (in NLA_2012 "h_EI.csv")

###########
# Lake morpho output - 9/11/17
###########
# Lake volume and other morpho metrics - 8/30/17
vol <- read_csv("data/NLA12/NLA12_ordered_SITEID_VOLUME.csv")

# Reorder variables
myvars<- c("SITE_ID","VISIT_NO","Volume_m3","maxDepth_derived_m","meanDepth_m","surfaceArea_sqm",
           "shorelineLength_m","shorelineDevelopment","fetch_m")
vol<-vol[myvars]
names(vol)

write_csv(vol, "data_to_merge_NLA12/multimerge_nla12/i_lake_morpho_ordered.csv")


##############
## Merge reduced datasets that have SITE_ID, VISIT_NO, and UID: water chemistry, chla, toxin, secchi, physical habitat, visual
##############

## UPDATE 8/1/17 COPIED FILES CREATED IN "Modified_data" folder into Data_to_merge
## UPDATE 8/23/17 Added some variables in the "site" dataset - copied to Data_to_merge
## UPDATE 9/11/17 Added revised EI values and merged based on SITE ID and Visit NO
## UPDATE 7/26/18 Added more lakes that were missing lat/long
## UPDATE 8/1/18 Added INDEX_SITE_DEPTH from Key variable dataset - will use this for depth


#########
# Trying multi-merge function
#  from https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
#  from http://www.talkstats.com/showthread.php/22305-Merging-Multiple-Data-Frames
#######
# Run the function
# need to trim up datasets to not have repeat variables (except the ones we are merging on)
# Save the reduced datasets into a folder "Data_to_merge"
# order datasets how we want them to be merged in the folder
# set path to folder and function will run on datasets

# Merging based on Site ID and Visit Number. I already subset data by year so the comb of the two make for a unique ID
#  Added all.x=TRUE to retain NAs - this makes for a larger datasets but we can then trim
multimerge = function(mergeme){
  filenames=list.files(path=mergeme, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y, by=c("SITE_ID","VISIT_NO"),all.x=TRUE)}, datalist) #all.x=TRUE will retain NAs
}


# Site information, water chem, chla, toxin, secchi, and visual data
NLA12_merge_a<- multimerge("data_to_merge_NLA12/multimerge_nla12")
names(NLA12_merge_a)  #1269 obs and 406 variables

########
## Drop duplicates based on SITE_ID and VISIT_NO n =1238
NLA12_merge_b<-NLA12_merge_a[!duplicated(NLA12_merge_a[,c(1,2)]),] # Column 1&2 (SITE_ID & VISIT_NO)

#######################
## MANUALLY MERGE datasets that will have NAs
#######################
#Manually merge lake condition
condition <- read_csv("data_to_merge_NLA12/lk_condition.csv")
names(condition)
summary(condition)

NLA12_merge_c <-merge(NLA12_merge_b, condition, by=c("SITE_ID"),all.x=TRUE) # 1103 obs (from 1194) and 405 vars
#summary(NLA12_merge_b)
names(NLA12_merge_c) # OLD 1238 obs and 432 variables


#########
## Clean up merged dataset
#########
#Dropping variables
todrop<-names(NLA12_merge_c)%in%c("X","X.x","X.y","X","E_I.x","RT","VISIT_NO.y", "DATE_COL.x","DATE_COL.y",
                                  "LAKE_ORIGIN12.y","UID.y")
NLA12_merge_d<-NLA12_merge_c[!todrop]

names(NLA12_merge_d)

#Rename some columns
names(NLA12_merge_d)[names(NLA12_merge_d)=="VISIT_NO.x"] <- "VISIT_NO"
names(NLA12_merge_d)[names(NLA12_merge_d)=="UID.x"] <- "UID"
names(NLA12_merge_d)[names(NLA12_merge_d)=="LAKE_ORIGIN12.x"] <- "LAKE_ORIGIN12"
names(NLA12_merge_d)[names(NLA12_merge_d)=="E_I.y"] <- "E_I"
#names(NLA12_merge_d)[names(NLA12_merge_d)=="RT.y"] <- "RT"

names(NLA12_merge_d)

##########
## RENAME LAKE ORIGIN to match 2007
#########
levels(NLA12_merge_d$Lake_Origin_use) <- list(
  "MAN_MADE" = c("MAN-MADE"),
  "NATURAL" = c("NATURAL"))
table(NLA12_merge_d$Lake_Origin_use)

######
## Write NLA12 merged dataset - 1238 observations and 417 variables
write_csv(NLA12_merge_d, "data_processed/nla12/NLA12_merge.csv")


##############
## LOAD Additional DATA
##############

# MERGED NLA 2012 datasets
nla12_org<- read_csv("data_processed/nla12/NLA12_merge.csv")

# Tabulated NLCD 2011 Land use/land cover
nlcd11<- read_csv("data/NLA12/NLCD2011_NLA2012_basins_sqkm_area.csv")

# Climate - these are the (PRISM 30 yr avg?) values (not for 2011-2012 water year)
climate<-read_csv("data/NLA12/Avg_climate_lk_pt.csv")

# Climate 2011-2012 dataset
climate12_all <- read_csv("data/NLA12/climate_2012_all.csv")

# Read landscape dataset - to get elevation in basin
# 2/7/18
landscape <- read_csv("data_to_merge_NLA12/f_landscape.csv")

###########
## Modify datasets 
###########
# NLA original plus additional data
todrop<-names(nla12_org)%in%c("X","UID...400")
nla12_org<-nla12_org[!todrop]
write_csv(nla12_org,"data_processed/nla12/a_NLA12_EI_merge_24JUN19.csv") # Copied old version one folder out


# NLCD 2011 - relabel land use/cover classes
names(nlcd11)
# NEED TO MAKE SURE ORDER IS THE SAME
names(nlcd11) <- c("SITE_ID", "BASINAreaSqKM", "OPEN_11_km2","ICE_11_km2",
                   "DEVOPEN_11_km2","DEVLOW_11_km2","DEVMED_11_km2","DEVHIGH_11_km2",
                   "BARREN_11_km2","DECID_11_km2","CONIF_11_km2", "MIXED_11_km2",
                   "SHRUBLAND_11_km2","GRASS_11_km2", "PASTURE_11_km2","CROPS_11_km2",
                   "WDYWET_11_km2","EMHERBWET_11_km2")

write_csv(nlcd11,"data_to_merge_NLA12/c_NLCD2011_NLA2012_basins_sqkm_area.csv")

########
## 8/18/17 added climate data for 2011-12 water year
## 10/16/17 calculated cummulative PET (m)
## 6/30/18 Summer temperature (May - September)
#######
names(climate12_all)

## Derive Total Potential Evapotranspiration and convert from mm to meters
# Sum over rows for a subset of variables

# Cumulative PET during 2011-2012
climate12_all$E <- rowSums(climate12_all[,c("pet_201110","pet_201111","pet_201112","pet_201201","pet_201202",
                                            "pet_201203","pet_201204","pet_201205","pet_201206","pet_201207",
                                            "pet_201208","pet_201209")])
# Convert from mm to meters
climate12_all$E_m <- climate12_all$E/1000

# Average PET during the 2011-2012
climate12_all$E_avg_11_12<-rowMeans(climate12_all[,c("pet_201110","pet_201111","pet_201112","pet_201201","pet_201202",
                                                     "pet_201203","pet_201204","pet_201205","pet_201206","pet_201207",
                                                     "pet_201208","pet_201209")])

# Cumulative precipitation during OCT 2011- SEPT 2012
climate12_all$P_WY <- rowSums(climate12_all[,c("precip_201110","precip_201111","precip_201112","precip_201201","precip_201202",
                                               "precip_201203","precip_201204","precip_201205","precip_201206","precip_201207",
                                               "precip_201208","precip_201209")])
# Convert from mm to meters
climate12_all$P_WY_m <- climate12_all$P_WY/1000

## Derive Season climate variables - NOTE - should decide what months to group
# Summer mean temperature and precip (June-August)
# 6/30/18 - updated for sampling months but had to cut Oct to match 2007 (May - Sept)
climate12_all$temp_degC_summer <- (climate12_all$tmean_201205 + climate12_all$tmean_201206 + climate12_all$tmean_201207
                                   +climate12_all$tmean_201208 + climate12_all$tmean_201209)/5
head(climate12_all$temp_degC_summer)
summary(climate12_all$temp_degC_summer) # missing data for two observations

climate12_all$precip_mm_summer <-(climate12_all$precip_201206 + climate12_all$precip_201207 + climate12_all$precip_201208)/3
head(climate12_all$precip_mm_summer)

# Winter mean temp and precipitation (December-February)
climate12_all$temp_degC_winter <- (climate12_all$tmean_201112 +climate12_all$tmean_201201+climate12_all$tmean_201202)/3
summary(climate12_all$temp_degC_winter)

climate12_all$precip_mm_winter <-(climate12_all$precip_201112+climate12_all$precip_201201+climate12_all$precip_201202)/3
summary(climate12_all$precip_mm_winter)

# Spring mean temp and precip (March-May)
climate12_all$temp_degC_spring <- (climate12_all$tmean_201203 +climate12_all$tmean_201204 + climate12_all$tmean_201205)/3
summary(climate12_all$temp_degC_spring)

climate12_all$precip_mm_spring <-(climate12_all$precip_201203 +climate12_all$precip_201204 + climate12_all$precip_201205)/3
summary(climate12_all$precip_mm_spring)

# Rename variables
names(climate12_all)[names(climate12_all)=="Precip_PT_avg_11_12"] <- "Precip_mm_avg_yr"
names(climate12_all)[names(climate12_all)=="Precip_WY_11_12"] <- "Precip_mm_total_yr"
names(climate12_all)[names(climate12_all)=="TMEAN_PT_avg_11_12"] <- "Temp_degC_avg_yr"

# Reduce dataset to get average values for the year
myvars<- c("SITE_ID","Precip_mm_avg_yr","Precip_mm_total_yr","Temp_degC_avg_yr","P_WY_m",
           "E_m","E_avg_11_12","RH_PT_avg_11_12","temp_degC_summer","precip_mm_summer",
           "temp_degC_winter","precip_mm_winter","temp_degC_spring","precip_mm_spring")

climate12_red <-climate12_all[myvars]
write_csv(climate12_red,"data_to_merge_NLA12/e_climate12_red.csv")

###########
##  Landscape  ## - provided in NLA 2012
names(landscape)

summary(landscape[c(6:8)])

# Convert elevation from cm to m
landscape$ELEV_SITE_m <- landscape$ELEV_SITE*0.01
landscape$ELEVMAX_BSN_m <- landscape$ELEVMAX_BSN*0.01
landscape$ELEVMEAN_BSN_m <- landscape$ELEVMEAN_BSN*0.01
landscape$ELEVMIN_BSN_m <- landscape$ELEVMIN_BSN*0.01
summary(landscape$ELEVMAX_BSN_m)

write_csv(landscape,"data_to_merge_NLA12/f_landscape_red.csv")

###########
## I copied datasets into a new folder "Data_to_merge_final"
#   to be able to use the multi-merge function
# Saved modified nla12 and EI merged dataset directly into folder to merge [OLD -Copy and relabel NLA12_merge.csv (in modified data folder) to a_NLA12_merge.csv ]
# BUT I had to go into each file and delete the "X" column - 
#  KEEPING THE X column MAY HAVE lead to making duplicate observations 
# 10/16/17 - added the modified climate 2011-2012 reduced dataset
# 10/30/17 - corrected E:I values for some lakes and added Residence Time
# 10/5/18 - corrected E:I values for some lakes
###########


#########
# Trying multi-merge function
#  from https://www.r-bloggers.com/merging-multiple-data-files-into-one-data-frame/
#  from http://www.talkstats.com/showthread.php/22305-Merging-Multiple-Data-Frames
#######

multmerge = function(mergeme){
  filenames=list.files(path=mergeme, full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
  Reduce(function(x,y) {merge(x,y, by=c("SITE_ID"),all.x=TRUE)}, datalist)
}

NLA12_merge_e<- multmerge("data_to_merge_NLA12/multimerge_pt2")

names(NLA12_merge_e)  # 1238  obs and 500 variables 
table(NLA12_merge_e$SITE_ID) # 
table(NLA12_merge_e$VISIT_NO)
summary(NLA12_merge_e$E_m)

#######
# Clean up datasets
#########

#Rename some columns
names(NLA12_merge_e)[names(NLA12_merge_e)=="TMEAN_PT.x"] <- "TMEAN_PT" #TMEAN_PT.y has very high numbers - will use .x
names(NLA12_merge_e)[names(NLA12_merge_e)=="UID.x"] <- "UID"
names(NLA12_merge_e)[names(NLA12_merge_e)=="Volume_m3"] <- "VOL_m3_OLD"

### Check number of duplicates
# Look at the number of observations by SITE_ID - now have better values 1s and 2s (not 1s and 4s)
n_occur_b<-data.frame(table(NLA12_merge_e$SITE_ID)) # 
n_occur_b[n_occur_b$Freq>2,]
#test<-NLA12_merge_e[NLA12_merge_e$SITE_ID%in%n_occur_b$Var1[n_occur_b$Freq>2],]

###########
#  Manually ADD Hollister lake morpho output
#  LAKE VOLUME VARIABLE using derived max depth
#  11/15/17
# 7/26/18 - added more lakes
###########
# Read in morph dataset
morph<-read_csv("data/NLA12/g_morpho_hollister_23JUL18.csv")
names(morph)

#Rename some columns
names(morph)[names(morph)=="VolumeCorrect"] <- "VolumeDerivedZmax"
names(morph)[names(morph)=="MaxDepthCorrect"] <- "MaxDepthDerived"
names(morph)[names(morph)=="MeanDepthCorrect"] <- "MeanDepthDerived"

indx<-match(NLA12_merge_e$UID, morph$UID) # match basedon SID & nlaSITE_ID and make 2012 have same order as 2007
morph_ord <- morph[indx,]
names(morph_ord) 
# [1] "SITE_ID"           "VISIT_NO"          "UID"              
# [4] "nlaSITE_ID"        "COMID"             "VolumeDerivedZmax"
# [7] "FetchN"            "FetchNE"           "FetchE"           
# [10] "FetchSE"           "MaxDepthDerived"   "MeanDepthDerived" 

head(NLA12_merge_e[c(1:3,38)])
head(morph_ord[c(1:3)])
# Drop variables used to match
myvars<-c("COMID","VolumeDerivedZmax","FetchN","FetchNE","FetchE","FetchSE",
          "MaxDepthDerived","MeanDepthDerived")
morph_red<- morph_ord[myvars]

# 1238 obs with 509 variables
NLA12_merge_f <- droplevels(cbind(NLA12_merge_e, morph_red))
names(NLA12_merge_f)

#########
## Clean up merged dataset
#########
names(NLA12_merge_f)
NLA12_merge_f<-NLA12_merge_f%>%
  rename(UID=UID...38)
str(NLA12_merge_f$UID)

###############
## Reorder variables and drop some
###############

myvars<- c("SITE_ID","VISIT_NO","UID","SID","YEAR","DATE_COL_iso","SAMPLE_ID","SAMPLED_PHAB",
           "dD","d18O","d_excess","E_I","RT_iso","RT_modeled","Water_yield_m","Modeled_Water_yield_m","Lake_precip_m","Lake_evap_m","Volume_Corrected_m3",
           "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use","DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc",
           "VertDD_CONDc315","HorizDD_CONDc315","Drawdown_CONDus15",
           "RT_NLA12_2015","LkArea_km2","L_LkAreakm2","DpthMx_use","L_DpthMx_use","PERIM_KM",
           "ELEV_use","L_ELEV_use","LATdd_use","LONdd_use","XCOORD","YCOORD","STATE","URBAN","Lake_Origin_use","NARS_NAME","LAKE_ORIGIN","LAKE_ORIGIN12",
           "COMID2012","COMID2007","COMIDS2007",
           "ECOWSA9_2015","WSA9_LO","ECOP5_2015","ECOP6_2015","TNT","RESAMPLED12","SIZE_CLASS","SITETYPE",
           "WGT_ALL","WGT_CAT","CH0712_CAT","CH0712_WGT","CH0712_USE","CAT_UNIQUE","BORD_LAKE",
           "SAM_CODE","ALUMINUM_RESULT_mgL","AMMONIA_N_RESULT_mgL","ANC_RESULT_ueqL",
           "CALCIUM_RESULT_mgL","CHLORIDE_RESULT_mgL","COLOR_RESULT_PtCo","COND_RESULT_uscm",
           "DOC_RESULT_mgL","MAGNESIUM_RESULT_mgL","NITRATE_N_RESULT_mgL","NITRATE_NITRITE_N_RESULT_mgL","NITRITE_N_RESULT_mgL","NTL_RESULT_mgL",
           "PH_RESULT","POTASSIUM_RESULT_mgL","PTL_RESULT_ugL","SILICA_RESULT_mgL","SODIUM_RESULT_mgL","SULFATE_RESULT_mgL",
           "TOC_RESULT_mgL","TSS_RESULT_mgL","TURB_RESULT_NTU",
           "CHLL_RESULT_ugL","SECCHI_m","MICL_RESULT_ugL",
           "AMFCALL","AMFCEMERGENT","AMFCFLOATING","AMFCSUBMERGENT",
           "BFFFLAT","BFFGRADUAL","BFFSTEEP","BFFVERTICAL","BFOANGLE","BFXHORIZDIST","BFXHORIZDIST_DD","BFXVERTHEIGHT","BFXVERTHEIGHT_DD",
           "BSFCBEDROCK","BSFCBOULDERS","BSFCCOBBLE","BSFCGRAVEL","BSFCORGANIC","BSFCSAND","BSFCSILT","BSFCWOOD",             
           "BSISITEVARIETY","BSISTAVARIETY","BSVLDIA","BSXLDIA",
           "FCFCAQUATIC","FCFCAQUATIC_DD","FCFCAQUATIC_LIT","FCFCAQUATIC_SIM",
           "FCFCBOULDERS","FCFCBOULDERS_DD","FCFCBOULDERS_LIT","FCFCBOULDERS_SIM",
           "FCFCBRUSH","FCFCBRUSH_DD","FCFCBRUSH_LIT","FCFCBRUSH_SIM",
           "FCFCLEDGES","FCFCLEDGES_DD","FCFCLEDGES_LIT","FCFCLEDGES_SIM",
           "FCFCLIVETREES","FCFCLIVETREES_DD","FCFCLIVETREES_LIT","FCFCLIVETREES_SIM",
           "FCFCSNAG","FCFCSNAGS_DD","FCFCSNAGS_LIT","FCFCSNAGS_SIM",
           "FCFCSTRUCTURES","FCFCSTRUCTURES_DD","FCFCSTRUCTURES_LIT","FCFCSTRUCTURES_SIM",
           "HIFPANY_SYN","HIFPANYCIRCA_SYN","HIIAG_SYN","HIIAGCIRCA_SYN","HIIALL_SYN","HIIALLCIRCA_SYN","HIINONAG_SYN","HIINONAGCIRCA_SYN",
           "HIPWAG_SYN","HIPWALL_SYN","HIPWBUILDINGS_SYN","HIPWCOMMERCIAL_SYN","HIPWCROPS_SYN","HIPWDOCKS_SYN","HIPWLANDFILL_SYN","HIPWLAWN_SYN","HIPWNONAG_SYN","HIPWORCHARD_SYN","HIPWOTHER_SYN","HIPWPARK_SYN","HIPWPASTURE_SYN","HIPWPOWERLINES_SYN","HIPWROADS_SYN","HIPWWALLS_SYN","LitCvrQc3OE","LitRipCvrQc3OE","RDis_IX","RVegQc3OE",
           "RVFCCANBIG","RVFCCANBIG_DD","RVFCCANBIG_RIP","RVFCCANBIG_SYN",
           "RVFCCANSMALL","RVFCCANSMALL_DD","RVFCCANSMALL_RIP","RVFCCANSMALL_SYN",
           "RVFCGNDBARE","RVFCGNDBARE_DD","RVFCGNDBARE_RIP","RVFCGNDBARE_SYN",
           "RVFCGNDINUNDATED","RVFCGNDINUNDATED_DD","RVFCGNDINUNDATED_RIP","RVFCGNDINUNDATED_SYN",
           "RVFCGNDNONW","RVFCGNDNONW_DD","RVFCGNDNONW_RIP","RVFCGNDNONW_SYN",
           "RVFCGNDWOODY","RVFCGNDWOODY_DD","RVFCGNDWOODY_RIP","RVFCGNDWOODY_SYN",
           "RVFCUNDNONW","RVFCUNDNONW_DD","RVFCUNDNONW_RIP","RVFCUNDNONW_SYN",
           "RVFCUNDWOODY","RVFCUNDWOODY_DD","RVFCUNDWOODY_RIP","RVFCUNDWOODY_SYN",
           "RVICANOPY","RVICANOPY_DD","RVICANOPY_RIP","RVICANOPY_SYN",
           "RVICANUND","RVICANUND_DD","RVICANUND_RIP","RVICANUND_SYN",
           "RVIGROUND","RVIGROUND_DD","RVIGROUND_RIP","RVIGROUND_SYN",
           "RVIHERBS", "RVIHERBS_DD","RVIHERBS_RIP","RVIHERBS_SYN",
           "RVITALLWOOD","RVITALLWOOD_DD","RVITALLWOOD_RIP","RVITALLWOOD_SYN",
           "RVITOTALVEG","RVITOTALVEG_DD","RVITOTALVEG_RIP","RVITOTALVEG_SYN",
           "RVIUNDERSTORY","RVIUNDERSTORY_DD","RVIUNDERSTORY_RIP","RVIUNDERSTORY_SYN",
           "RVIWOODY","RVIWOODY_DD","RVIWOODY_RIP","RVIWOODY_SYN",
           "SIVDEPTH","SIXDEPTH",
           "SSFCBEDROCK","SSFCBOULDERS","SSFCCOBBLE","SSFCGRAVEL","SSFCORGANIC","SSFCOTHER","SSFCSAND","SSFCSILT","SSFCWOOD",          
           "SSISITEVARIETY","SSISTAVARIETY","SSVLDIA","SSXLDIA",
           "BASINAreaSqKM","OPEN_11_km2","ICE_11_km2","DEVOPEN_11_km2","DEVLOW_11_km2","DEVMED_11_km2","DEVHIGH_11_km2",
           "BARREN_11_km2","DECID_11_km2","CONIF_11_km2","MIXED_11_km2","SHRUBLAND_11_km2","GRASS_11_km2",
           "PASTURE_11_km2","CROPS_11_km2","WDYWET_11_km2","EMHERBWET_11_km2",
           "DOMGEOL_BSN","SITE_GEOLOGY","ELEVMAX_BSN_m","ELEVMEAN_BSN_m","ELEVMIN_BSN_m",
           "AGR_CROPLAND","AGR_FEEDLOT","AGR_LIVESTOCK","AGR_ORCHARDS","AGR_PASTURE","AGR_POULTRY","AGR_SCORE","AGR_STRING","AGR_WITHDRAWAL",          
           "APPEALING","BIOTIC_INTEGRITY",
           "IND_COMMERCIAL","IND_FIRE","IND_INDUSTRIAL","IND_LOGGING","IND_MINES","IND_ODORS","IND_OIL","IND_POWER","IND_SCORE","IND_STRING",              
           "HYDRO_TYPE","LAKE_LEVEL","LEVEL_CHANGE_M","LEVEL_CHANGES",
           "MAN_DRINKING_WATER","MAN_LEVEL_FLUCTUATIONS","MAN_SCORE","OUTLET_DAMS",
           "PRISTINE","RCH_AGRICULTURE","RCH_BARE_GROUND","RCH_DEVELOPMENT","RCH_FOREST", "RCH_GRASS","RCH_SHOREMODS","RCH_SHRUB","RCH_WETLAND",       
           "REC_FILMS","REC_MARINAS","REC_PARKS","REC_PRIMITIVE","REC_RESORTS","REC_SCORE","REC_STRING","REC_TRAILS","REC_TRASH",             
           "RECREATIONAL_ACTIVITY","RECREATIONAL_VALUE","RES_BRIDGES","RES_CONSTRUCTION","RES_DUMPING","RES_LAWNS","RES_PIPES","RES_RESIDENCES","RES_ROADS","RES_SCORE",             
           "RES_SEWAGE","RES_STRING","SWIMMABILITY","TROPHIC_STATE","DRAWDOWN_COND", "LITCVR_COND","LITRIPCVR_COND","RDIS_COND","RVEG_COND",
           "PMEAN_BSN","PMIN_BSN","PMAX_BSN","PIP_BSN",
           "TIP_BSN","TMEAN_BSN","TMIN_BSN","TMAX_BSN","TMINSD_BSN",
           "Precip_PT","E_m","RH_PT","TMEAN_PT","P_WY_m",
           "Temp_degC_avg_yr","Precip_mm_avg_yr","Precip_mm_total_yr",
           "temp_degC_winter","temp_degC_spring","temp_degC_summer",
           "precip_mm_winter","precip_mm_spring","precip_mm_summer",
           "POPDEN_BSN","ROADDEN_BSN","DAMDEN_BSN","DAMCNT_BSN",
           "VolumeDerivedZmax","FetchN","FetchE" ,"FetchSE","MaxDepthDerived","MeanDepthDerived",
           "LAT_DD83","LON_DD83","AREA_HA","INDEX_SITE_DEPTH")

NLA12_merge_g<-NLA12_merge_f%>%
  select(all_of(myvars))

names(NLA12_merge_g)

######
## Write NLA12 merged dataset - 1238 observations and 380 variables
write_csv(NLA12_merge_g, "data_processed/nla12/NLA12_ALL_VARS_merge_24JUN19.csv") # 7/26/18 copied old version into one folder back

################################
#######################
## Preprocess data - 8/18/17
## 10/5/18 - updated E:I values
#######################
################################

########
# Read in dataset - includes lake morpho output
########
nla12_all<-read_csv("data_processed/nla12/NLA12_ALL_VARS_merge_24JUN19.csv")


#######
## Reorder variables
#######
# from: http://stackoverflow.com/questions/4260698/r-ordering-in-boxplot

# Ecoregion groups so that they are plotted from West to East to match the map
nla12_all$ECOREG_use <- ordered(nla12_all$ECOWSA9_2015, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

table(nla12_all$ECOREG_use)

## Hydrologic type
table(nla12_all$Lake_Origin_use)
# MAN_MADE  NATURAL 
#   668      538 
table(nla12_all$HYDRO_TYPE)
#                      DRAINAGE RESERVOIR   SEEPAGE 
#               25       394       592       219

# Preprocessing of HYDRO_TYPE to label lakes not given a designation
# https://stackoverflow.com/questions/27195956/convert-na-into-a-factor-level
library(forcats)
nla12_all<-nla12_all%>%
  mutate(HYDRO_TYPE_f=fct_na_value_to_level(HYDRO_TYPE,"Unknown"))
table(test$HYDRO_TYPE_f)

nla12_all$HYDRO_TYPE_f <- ordered(nla12_all$HYDRO_TYPE_f, levels=c("Unknown","SEEPAGE","DRAINAGE","RESERVOIR"))

#########
## Grouping lakes by E:I flow-through
# based on Brooks et al. 2014 - Wolfe et al. 2007
#########
##  Flowthrough E:I <0.4
##  Restricted drainage 0.4 >= E:I <1
##  Closed-basin E:I >= 1

nla12_all$lk_hydro_iso [nla12_all$E_I <0.4]<- "Flow_through"
nla12_all$lk_hydro_iso[nla12_all$E_I >=0.4 & nla12_all$E_I <1] <-"Restricted"
nla12_all$lk_hydro_iso[nla12_all$E_I >=1] <-"Closed"
table(nla12_all$lk_hydro_iso)
nla12_all$lk_hydro_iso <- ordered(nla12_all$lk_hydro_iso, levels=c("Closed","Restricted","Flow_through"))

table(nla12_all$lk_hydro_iso)
# Closed   Restricted Flow_through 
#   36          266          923

### 11/15/17 - Added DEPTH classes that match 2007 data
nla12_all$DEPTH_COND<-cut(nla12_all$DpthMx_use, breaks=c(0,2,3,5,10,20,100),labels=c("< 2  ","2-3  ","3-5  ","5-10 ","10-20",">20  "))


###########
## Lake size classes based on quartile values - 
###########
summary(nla12_all$LkArea_km2)

l<-nla12_all[which(!is.na(nla12_all$LkArea_km2)),] 
quantile(l$LkArea_km2)
#              0%          25%          50%          75%         100% 
#1.033385e-02 1.121822e-01 3.161891e-01 1.092387e+00 1.254974e+03 
nla12_all$lk_area_bin <-cut(nla12_all$LkArea_km2, c(0.01, 0.11, 0.32,1.09,Inf), labels=1:4)

# look at lake size bin and max lake depth to see if increases as expect
boxplot(nla12_all$DpthMx_use~nla12_all$lk_area_bin, ylab= "Log Max depth", xlab="lake size bin",main="Lake depth by lake size classes")


## Lake depth bins
# NOT GOING TO DROP LAKES MISSING MAX DEPTH
summary(nla12_all$DpthMx_use) # 40 NAs
z<-nla12_all[which(!is.na(nla12_all$DpthMx_use)),] 

quantile(z$DpthMx_use)
#       0%  25%  50%  75% 100% 
#     0.8  2.5  4.6  8.8 58.5
nla12_all$lk_depth_bin <- cut(nla12_all$DpthMx_use, c(0.8, 2.5, 4.5, 8.55, Inf), labels=1:4)
# look at lake depth bin and lake size to see if increases as expect
boxplot(nla12_all$L_LkAreakm2~nla12_all$lk_depth_bin, ylab= "Log Lake Area (km2)", xlab="lake depth bin",main="Lake size by depth classes")
boxplot(nla12_all$DpthMx_use~nla12_all$lk_depth_bin, ylab= "Max Depth (m)", xlab="lake depth bin",main="Lake depth by depth classes")

#########
# Create land use/land cover proportions in the watershed 
#########
nla12_all$PCT_OPEN11_BSN<- nla12_all$OPEN_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_ICE11_BSN<- nla12_all$ICE_11_km2/nla12_all$BASINAreaSqKM

nla12_all$PCT_DEVOPEN11_BSN<- nla12_all$DEVOPEN_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_DEVLOW11_BSN<- nla12_all$DEVLOW_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_DEVMED11_BSN<- nla12_all$DEVMED_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_DEVHIGH11_BSN<- nla12_all$DEVHIGH_11_km2/nla12_all$BASINAreaSqKM

nla12_all$PCT_BARREN11_BSN<- nla12_all$BARREN_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_DECID11_BSN<- nla12_all$DECID_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_CONIF11_BSN<- nla12_all$CONIF_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_MIXED11_BSN<- nla12_all$MIXED_11_km2/nla12_all$BASINAreaSqKM

nla12_all$PCT_SHRUB11_BSN<- nla12_all$SHRUBLAND_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_GRASS11_BSN<- nla12_all$GRASS_11_km2/nla12_all$BASINAreaSqKM

nla12_all$PCT_PASTURE11_BSN<- nla12_all$PASTURE_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_CROPS11_BSN<- nla12_all$CROPS_11_km2/nla12_all$BASINAreaSqKM

nla12_all$PCT_WDYWET11_BSN<- nla12_all$WDYWET_11_km2/nla12_all$BASINAreaSqKM
nla12_all$PCT_EMHERBWET11_BSN<- nla12_all$EMHERBWET_11_km2/nla12_all$BASINAreaSqKM

# Create summed land use land cover
nla12_all$PCT_DEVELOPED11_BSN<- nla12_all$PCT_DEVOPEN11_BSN+nla12_all$PCT_DEVLOW11_BSN+
  nla12_all$PCT_DEVMED11_BSN+nla12_all$PCT_DEVHIGH11_BSN

nla12_all$PCT_FOREST11_BSN <-nla12_all$PCT_DECID11_BSN+nla12_all$PCT_CONIF11_BSN+nla12_all$PCT_MIXED11_BSN
nla12_all$PCT_AGR11_BSN <-nla12_all$PCT_PASTURE11_BSN+nla12_all$PCT_CROPS11_BSN
nla12_all$PCT_WET11_BSN <-nla12_all$PCT_WDYWET11_BSN+nla12_all$PCT_EMHERBWET11_BSN


########
## Calculate Shoreline Development Index (SDI)
# From Aronow 1982
# SDI = perimeter/ (2 * (sqrt(pi* lake area)))

nla12_all$SLD <- nla12_all$PERIM_KM/(2*sqrt(nla12_all$LkArea_km2 *pi))
head(nla12_all$SLD)
summary(nla12_all$SLD)

names(nla12_all)

# # Rename level in TROPHIC_STATE that was blank to "Not assessed"
nla12_all<-nla12_all%>%
  mutate(TROPHIC_STATE = fct_na_value_to_level(TROPHIC_STATE,"Not_assessed"))
table(nla12_all$TROPHIC_STATE)
#  Not_assessed      EUTROPHIC HYPEREUTROPHIC    MESOTROPHIC   OLIGOTROPHIC 
#       45            474             88            518            113 

##########
## FILL IN VALUES WHERE MISSING LAT, LONG, AREA, MAX DEPTH
# 8/1/18
## AND ADD other variables
##########

################
# MAX DEPTH Decision - 9/26/17 - Populate underestimated max depth with lit values
# Create dataset of lakes with underestimated max depth
# 7/4/19 - updated zmax for three more lakes
z<-subset(nla12_all,(SITE_ID %in% c("NLA12_WA-151","NLA12_CA-206","NLA12_MT-136",
                                    "NLA12_OR-109","NLA12_NV-109","NLA12_NY-0101","NLA12_NC-105","NLA12_MT-111")))

# Entering max depth from lit 
depth_lit<- c(158,71,119,76,109,57,57,130,200) # ordered alphabetically SITE_ID
z$DpthMx_mod <-depth_lit
head(z[,c(1,405)])

# Create column indicating source of max depth
z$Zmax_source <- "LIT"
head(z[,c(1,405,406)])

## Some lakes that were added do not have DpthMx_use but there is depth data in the Site dataset
dpth_na <-subset(nla12_all, (SITE_ID %in% c("NLA12_AL-101","NLA12_AL-103","NLA12_AL-105",
                                            "NLA12_FL-101","NLA12_FL-102","NLA12_FL-103",
                                            "NLA12_FL-123","NLA12_ND-105","NLA12_NV-101","NLA12_NV-104",
                                            "NLA12_SC-104","NLA12_SC-118","NLA12_TN-101","NLA12_TN-108",
                                            "NLA12_TX-105","NLA12_TX-110","NLA12_VT-103","NLA12_WY-124","NLA12_WY-133")))

dpth_na$DpthMx_mod<-dpth_na$INDEX_SITE_DEPTH
dpth_na$Zmax_source <-"INDEX"
test<-dpth_na[c(1:3,31,33,38,39,377:380,403,404)]


# Create dataset of all other lakes
other<-nla12_all[!(nla12_all$SITE_ID %in% c("NLA12_WA-151","NLA12_CA-206","NLA12_MT-136",
                                            "NLA12_OR-109","NLA12_NV-109","NLA12_NY-0101","NLA12_NC-105","NLA12_MT-111",
                                            "NLA12_AL-101","NLA12_AL-103","NLA12_AL-105",
                                            "NLA12_FL-101","NLA12_FL-102","NLA12_FL-103",
                                            "NLA12_FL-123","NLA12_ND-105","NLA12_NV-101","NLA12_NV-104",
                                            "NLA12_SC-104","NLA12_SC-118","NLA12_TN-101","NLA12_TN-108",
                                            "NLA12_TX-105","NLA12_TX-110","NLA12_VT-103","NLA12_WY-124","NLA12_WY-133")),]
# Create column that populates the updated depth
other$DpthMx_mod <-other$DpthMx_use

# Create column that indicates source of max depth
other$Zmax_source <-"NLA"
head(other[,c(1, 33,405,406)])

## Bind two dataframes together
nla12_revised <- rbind(other,dpth_na)
nla12_revised <- rbind(nla12_revised,z)
str(nla12_revised)
table(nla12_revised$ECOWSA9_2015)

# Dropped NAs to 20
summary((nla12_revised$DpthMx_mod))
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.800   2.600   4.600   8.059   8.875 200.000      20

# Log 10 transform max depth
nla12_revised$L_DpthMx_mod <- log10(nla12_revised$DpthMx_mod)
head(nla12_revised[,c(1,34,403)])
head(nla12_revised[,c(1,35,405)])

# Order by SITE_ID
nla12_revised <-nla12_revised [order(nla12_revised$SITE_ID),]
head(nla12_revised[,c(1,35,405)])
tail(nla12_revised[,c(1,35,405)])

############
# MISSING LAT LONG
lat_na<-nla12_revised[which(is.na(nla12_revised$LATdd_use)),] #n =32 obs
long_na<-nla12_revised[which(is.na(nla12_revised$LONdd_use)),] # n = 32
lat_na<-lat_na[,c(1:3,38,39,377:380)]
write_csv(lat_na,"data_processed/nla12/nla12_lat_long_NA.csv")

# USE LAT_DD83 and LONG_DD83 in dataset

##############
## MISSING AREA
area_na <-nla12_revised[which(is.na(nla12_revised$LkArea_km2)),] # 31 lakes
area_na <-nla12_revised[which(is.na(nla12_revised$AREA_HA)),]  # 7 obs

nla12_revised$LKAREA_KM2_mod <-nla12_revised$AREA_HA*0.01
plot(nla12_revised$LkArea_km2,nla12_revised$LKAREA_KM2_mod)

nla12_revised$L_LKAREA_KM2_mod <-log10(nla12_revised$LKAREA_KM2_mod)
# Okay to replace converted AREA_HA (into sqkm) with LkArea_km2


#################
## CLEAN UP SOME OTHER STUFF
nla12_revised$NTL_RESULT_ugL<-nla12_revised$NTL_RESULT_mgL*1000

# Calculate scaled Vert Drawdown using modified max depth variable
# Non-transformed scaled VERT DD
nla12_revised$DDVrtDix_sc_MOD<-nla12_revised$VertDD_use/nla12_revised$DpthMx_mod
summary(nla12_revised$DDVrtDix_sc_MOD)

# LOG 10 transforme scaled vert dd
nla12_revised$L_DDVrtDix_sc_MOD<-log10((nla12_revised$VertDD_use/nla12_revised$DpthMx_mod) +0.01)
summary(nla12_revised$L_DDVrtDix_sc_MOD)
# NOTE there was little difference between using the modified depth class and the NLA reported depth


################
# Create precip class
# 1/3/18
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
nla12_revised$Precip_class_man<-cut(nla12_revised$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))

## Create Precipitation + Lake Origin variable
nla12_revised$PRECIP_LO <- with (nla12_revised, interaction(Precip_class_man,Lake_Origin_use,sep="_"))
table(nla12_revised$PRECIP_LO)

# Ecoregion groups so that they are plotted from West to East to match the map
nla12_revised$ECOREG_use <- ordered(nla12_revised$ECOWSA9_2015, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


###################
## WRITE MODIFIED DATASET - with modified max depth values & transformations
###################
# 5/10/18
#6/30/18 - summer temperature
# 7/26/18
# 8/1/18 - revised EI and added lat,long
# 10/5/18 - revised E:I
# 4/30/19 - rearranged data processing and made lake with E:I>4 NA
# 6/24/19 - Updated Lake Volume and Water Residence times
# FULL dataset n=1238 obs (all visits, non targets and duplicates) with 422 variables
write_csv(nla12_revised,"data_processed/nla12/NLA12_ALL_VARS_merge_transformed_depth_24JUN19.csv") # 7/26/18 copied the old version one folder back


###########
## EXPLORE DATASET FOR ANALYSIS 
###########
# Read dataset - n=1238 w/423 variables
NLA12_org<-read_csv("data_processed/nla12/NLA12_ALL_VARS_merge_transformed_depth_24JUN19.csv")

# NUMBER OF LAKES with VISIT_NO = 1
table(NLA12_org$VISIT_NO) 
#            1    2 
#         1135  103 

lk_visit_one<-NLA12_org[which(NLA12_org$VISIT_NO==1),] # 1135

# NUMBER OF LAKES with SAMPLED_PHAB
table(lk_visit_one$SAMPLED_PHAB)
#           N  YES 
#       3   22 1110 

# NUMBER OF LAKES (VISIT=1) with WGT_ALL - WANT THIS TO BE ~1038
nla12_full<-lk_visit_one[which(lk_visit_one$WGT_ALL>0),] # 1039 lakes with weights
length(unique(nla12_full$SITE_ID))
#[1] 1039

# NUMBER OF LAKES WITH WEIGHT AND HAD PHAB SAMPLED
table(nla12_full$SAMPLED_PHAB)
#         N  YES 
#     0   20 1019  

# NUMBER OF LAKES with HORIZ_DD = NA
summary(nla12_full$HorizDD_use)
#           Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#         0.000   0.000   0.000   6.959   1.085 708.700      20 


# DROP DUPLICATE SITE_ID & VISIT_NO
z<-nla12_full[!duplicated(nla12_full[,c(1,2)]),] 
# 1039 obs and 412 variables

#########
## ONE LAKE IS MISSING KEY info 
#     Drop from dataset and use this for population estimates
#############
summary(nla12_full$WSA9_LO) 
nla12_full_b<- nla12_full[which(!is.na(nla12_full$WSA9_LO)),] # 1038 obs
nla12_full_b<-droplevels(nla12_full_b)

##########
## Transformations
##########

## Logit transformation BASIN LANDUSE vars - can't have 0s or 1s - so will add or subtract a small value to observations that are 0s or 1s
# Need to specify that we want the car package "recode" function

library(car)

nla12_full_b$PCT_FOREST11_BSN.n <- car::recode(nla12_full_b$PCT_FOREST11_BSN, "0.00=0.0001; 1.00=0.999")
summary(nla12_full_b$PCT_FOREST11_BSN.n)
summary(nla12_full_b$PCT_FOREST11_BSN)

nla12_full_b$PCT_GRASS11_BSN.n <- car::recode(nla12_full_b$PCT_GRASS11_BSN, "0=0.0001; 1=0.999")

nla12_full_b$PCT_WET11_BSN.n <- car::recode(nla12_full_b$PCT_WET11_BSN, "0=0.0001; 1=0.999")
nla12_full_b$PCT_AGR11_BSN.n <- car::recode(nla12_full_b$PCT_AGR11_BSN, "0=0.0001; 1=0.999")
nla12_full_b$PCT_DEVELOPED11_BSN.n <- car::recode(nla12_full_b$PCT_DEVELOPED11_BSN, "0=0.0001; 1=0.999")

# Logit transformation
nla12_full_b$PCT_FOREST_BSN_logit<-logit(nla12_full_b$PCT_FOREST11_BSN.n)
nla12_full_b$PCT_GRASS_BSN_logit<-logit(nla12_full_b$PCT_GRASS11_BSN.n)
nla12_full_b$PCT_AGR_BSN_logit<-logit(nla12_full_b$PCT_AGR11_BSN.n)
nla12_full_b$PCT_DEVELOPED_BSN_logit<-logit(nla12_full_b$PCT_DEVELOPED11_BSN.n)
nla12_full_b$PCT_WET_BSN_logit<-logit(nla12_full_b$PCT_WET11_BSN.n)


## LOG 10 Transformations
nla12_full_b$L_COND <- log10(nla12_full_b$COND_RESULT_uscm)
nla12_full_b$L_ANC<-log10(nla12_full_b$ANC_RESULT_ueqL)
nla12_full_b$L_TURB <-log10(nla12_full_b$TURB_RESULT_NTU)
nla12_full_b$L_DOC<-log10(nla12_full_b$DOC_RESULT_mgL)
nla12_full_b$L_NTL<-log10(nla12_full_b$NTL_RESULT_mgL)
nla12_full_b$L_PTL<-log10(nla12_full_b$PTL_RESULT_ugL)
nla12_full_b$L_CHLA<-log10(nla12_full_b$CHLL_RESULT_ugL)
nla12_full_b$L_CL_PPM<-log10(nla12_full_b$CHLORIDE_RESULT_mgL)
nla12_full_b$L_SO4_PPM<-log10(nla12_full_b$SULFATE_RESULT_mgL)
nla12_full_b$L_CA_PPM<-log10(nla12_full_b$CALCIUM_RESULT_mgL)
nla12_full_b$L_MG_PPM<-log10(nla12_full_b$MAGNESIUM_RESULT_mgL)
nla12_full_b$L_NA_PPM<-log10(nla12_full_b$SODIUM_RESULT_mgL)
nla12_full_b$L_K_PPM<-log10(nla12_full_b$POTASSIUM_RESULT_mgL)
nla12_full_b$L_SIO2<-log10(nla12_full_b$SILICA_RESULT_mgL)

nla12_full_b$L_RT_iso <-log10(nla12_full_b$RT_iso+0.01)


names(nla12_full_b)

##########
## Drop variables used for transformations
##########
myvars_drop <- names(nla12_full_b)%in% c("PCT_FOREST11_BSN.n","PCT_GRASS11_BSN.n","PCT_WET11_BSN.n",
                                         "PCT_AGR11_BSN.n","PCT_DEVELOPED11_BSN.n")

nla12_full_c <- nla12_full_b[!myvars_drop]

# n=1038 obs and 434 variables

###################
## REWRITE FILE
## NLA 2012 VISIT=1 with WGT_ALL >0 # n= 1038 with 437 variables
# 10/5/18
# 6/24/19
write_csv(nla12_full_c,"data_processed/nla12/NLA_12_FULL_VISIT_1_WGT_USE.csv")


##############
## ADD PALMER HYDROLOGIC DROUGHT INDEX - Average in Water Year
##  See Palmer_data_29MAY18.R for script working with original data layers
##  10/8/18
##############

# LOAD processed NLA dataset with connectivity class
nla12<-read_csv("data_processed/nla12/NLA_12_FULL_VISIT_1_WGT_USE.csv")

# LOAD processed PHDI data
phdi_12<- read_csv("data/NLA12/PHDI_NLA12.csv")
names(phdi_12)

# MERGE NLA07 and PHDI
test<-merge(nla12,phdi_12,by="SITE_ID",all.x=TRUE)
test<-test[!duplicated(test[,c(1)]),]
names(test)

###########
# Add Water isotope classes
# 10/31/18
##########
## E:I Classes
test$EI_class<-cut(test$E_I, breaks=c(0,0.20,0.50,Inf),labels=c("Low","Moderate","High"))

table(test$EI_class) 
plot(test$EI_class,test$E_I)

## WRT Classes
test$WRT_class<-cut(test$RT_iso, breaks=c(0,0.50,1.0,Inf),labels=c("Short","Moderate","Long"))
table(test$WRT_class)
plot(test$WRT_class,test$RT_iso)

###########
# Update lake size and depth classes using more complete observations
# 6/24/19
###########

## Lake size classes based on quartile values - LKAREA_KM2_mod
summary(test$LKAREA_KM2_mod)

l<-test[which(!is.na(test$LKAREA_KM2_mod)),] 
quantile(l$LKAREA_KM2_mod)
#          0%          25%          50%          75%         100% 
# 1.033385e-02 1.119247e-01 3.217644e-01 1.099743e+00 1.674896e+03 
test$lk_area_bin <-cut(test$LKAREA_KM2_mod, c(0.01, 0.11, 0.32,1.09,Inf), labels=1:4)

# look at lake size bin and max lake depth to see if increases as expect
boxplot(test$DpthMx_mod~test$lk_area_bin, ylab= "Log Max depth", xlab="lake size bin",main="Lake depth by lake size classes")


## Lake depth bins
# NOT GOING TO DROP LAKES MISSING MAX DEPTH
summary(test$DpthMx_mod) # 40 NAs
z<-test[which(!is.na(test$DpthMx_mod)),] 


quantile(z$DpthMx_mod)
#   0%   25%   50%   75%  100% 
#   0.8   2.5   4.5   8.6 200.0 
test$lk_depth_bin <- cut(test$DpthMx_mod, c(0.8, 2.5, 4.5, 8.6, Inf), labels=1:4)
# look at lake depth bin and lake size to see if increases as expect
boxplot(test$L_LkAreakm2~test$lk_depth_bin, ylab= "Log Lake Area (km2)", xlab="lake depth bin",main="Lake size by depth classes")
boxplot(test$DpthMx_mod~test$lk_depth_bin, ylab= "Max Depth (m)", xlab="lake depth bin",main="Lake depth by depth classes")

# SHORELINE DEVELOPMENT INDEX - using more complete lake area variable
test$SLD <- test$PERIM_KM/(2*sqrt(test$LKAREA_KM2_mod *pi))
head(test$SLD)
summary(test$SLD)


###############
## WRITE MODIFIED DATASET
write_csv(test,"data_processed/nla12/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_24JUN19.csv")


###########################
## LakeCAT VARIABLES for 2012 lakes
#  6/26/19

# READ in processed NLA12 dataset
nla12<- read_csv("data_processed/nla12/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_24JUN19.csv")

lkcat12<-read_csv("data/NLA12/LakeCat_vars_nla12.csv")

###################
## MERGE NLA 2012 (n=1038 lakes) + LakeCat variables
nla12_red<-merge(nla12,lkcat12, by="SITE_ID",all.x=TRUE) # there are 3 sites missing LakeCat data - Need to keep NAs
names(nla12_red)

#################
### DERIVE VARIABLES
#################
# WA:LA
nla12_red$WALA<-nla12_red$BASINAreaSqKM/nla12_red$LkArea_km2
# Add Log transformed WA:LA
nla12_red$L_WALA <- log10(nla12_red$WALA)
summary(nla12_red$WALA)
summary(nla12_red$L_WALA)

# Consolidate glacial lithology classes
nla12_red$pctGlac <-nla12_red$PctGlacTilClayWs + nla12_red$PctGlacTilLoamWs + nla12_red$PctGlacTilCrsWs +
  nla12_red$PctGlacLakeCrsWs + nla12_red$PctGlacLakeFineWs
nla12_red$pctGlac <-nla12_red$PctGlacTilClayWs + nla12_red$PctGlacTilLoamWs + nla12_red$PctGlacTilCrsWs +
  nla12_red$PctGlacLakeCrsWs + nla12_red$PctGlacLakeFineWs
summary(nla12_red$pctGlac)

# Create new variable - fraction of stations with Flat and Gradual bank angle
nla12_red$bffFlat_grad <- nla12_red$BFFFLAT + nla12_red$BFFGRADUAL
summary(nla12_red$bffFlat_grad)

# DROP EXTRA VARIABLES
todrop<-names(nla12_red)%in%c("X","X.x","X.y","X","X.1","X.x.1","X.y.1","X.x.2",
                              "X.y.2","X.x.3","X.y.3","X.x.4","X.y.4",
                              "VISIT_NO.y","LONdd_use.y","LATdd_use.y",
                              "...1","X.x...3","X.y...8",
                              "X.x...16","X.y...19","X.x...56",
                              "X.y...65","X.x...68","X.y...71","X.x...78",
                              "X.y...85")
nla12_red<-nla12_red[!todrop]

# rename variables
names(nla12_red)[names(nla12_red)=="VISIT_NO.x"] <- "VISIT_NO"
names(nla12_red)[names(nla12_red)=="LATdd_use.x"] <- "LATdd_use"
names(nla12_red)[names(nla12_red)=="LONdd_use.x"] <- "LONdd_use"
names(nla12_red)

# DROP EXTRA VARIABLES
todrop<-names(nla12_red)%in%c("X","X.x","X.y","X","X.1","X.x.1","X.y.1","X.x.2",
                              "X.y.2","X.x.3","X.y.3","X.x.4","X.y.4",
                              "VISIT_NO.y","LONdd_use.y","LATdd_use.y",
                              "LAT_DD83.y","LON_DD83.y")
nla12_red<-nla12_red[!todrop]

# rename variables
names(nla12_red)[names(nla12_red)=="VISIT_NO.x"] <- "VISIT_NO"
names(nla12_red)[names(nla12_red)=="LATdd_use.x"] <- "LATdd_use"
names(nla12_red)[names(nla12_red)=="LONdd_use.x"] <- "LONdd_use"
names(nla12_red)[names(nla12_red)=="LAT_DD83.x"] <- "LAT_DD83"
names(nla12_red)[names(nla12_red)=="LON_DD83.x"] <- "LON_DD83"
names(nla12_red)

####################
## ADD MODIFIED DRAWDOWN MEASURES
## Phil emailed 2/11/19
##  Processed in Data>Additional_NLA_data>From_Phil_11FEB19>Scripts>NLA0712_create_datasets_modified_DD_12FEB19.R

## LOAD DATA
# Modified Drawdown for 2012 lakes (n= 1252 obs w/19 vars)
dd<-read_csv("data/NLA12/NLA12_modDD_only.csv")

# MERGE NLA12 dataset with modified drawdown variables # n=1038 with 536 variables
nla12_dd<-merge(nla12_red,dd, by=c("SITE_ID","VISIT_NO"))
#names(nla12_dd)

####################
## ADD UPDATED POPULATION WEIGHTS 6/26/19
####################
## NLA POPULATION WEIGHTS - UPDATED 6/24/19 - processed in Analysis>NLA_weighted_calculations>Scripts>a_UPDATED_NLA_POP_WGTS_24JUN19.R
# n=1038 with 9 variables
nla12wt <- read_csv("data/NLA12/NLA12_popwts_25JUN19.csv")


#############
## MERGE with updated population weights 
nla12_dd_wt<-merge(nla12_dd,nla12wt,by="SITE_ID")#all.x=TRUE

names(nla12_dd_wt)

# Drop some variables
todrop <-names(nla12_dd_wt)%in%c("X.x","X.y","...1.x" ,"...1.y" ) #"X.1","X.1.x","X.1.y",
nla12_dd_wt<-nla12_dd_wt[!todrop]

# Rename variables
names(nla12_dd_wt)[names(nla12_dd_wt)=="WGT_CAT.x"] <- "WGT_CAT.v1"
names(nla12_dd_wt)[names(nla12_dd_wt)=="WGT_CAT.y"] <- "WGT_CAT.v2"

names(nla12_dd_wt)[names(nla12_dd_wt)=="SITETYPE.x"] <- "SITETYPE"
names(nla12_dd_wt)[names(nla12_dd_wt)=="SITETYPE.y"] <- "SITETYPE.v2"

##########
## WRITE DATASET and Variables
write_csv(nla12_dd_wt,"data_processed/nla12/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_lkcat_wgt_26JUN19.csv")

# Variables Names
nla12_vars<-as.data.frame(names(nla12_dd_wt))
write_csv(nla12_vars,"data_processed/nla12/NLA12_ALL_dataset_variable_names.csv")



##############
## NLA12 Data with lakes => 4ha in size 
#   to compare with NLA07
#   11/13/17
#   8/2/18 - UPDATED E:I & RT ESTIMATES
#   10/5/18 - UPDATED E:I
#   10/8/18 - Added PHDI (avg water year)
#   10/31/18 - Added Isotope classes
#   4/30/19 - Modified dataset
#   6/24/19 - Updated Volume and WRT
#   6/26/19 - Updated Population Weights (6/24/19)
##############
# READ DATA - VISIT_NO=1; WGT_ALL>0 n = 1038
nla12<-read_csv("data_processed/nla12/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_lkcat_wgt_26JUN19.csv")

## Reduced datasets to have lakes that are larger than 4ha or 0.04 sqkm
nla12_size_adj<-nla12[which(nla12$LKAREA_KM2_mod>=0.04),] # n = 951 lakes

######
# Write datasets - SIZE ADJUSTED NLA2012 data
#  10/5/18
#  10/8/18 - Added PHDI - kept same name
#  4/30/19 - modified full dataset slightly
# 6/16/19 - updated population weights
######
#write_csv(nla12_size_adj,"M:/Net MyDocuments/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_merge_transform_SIZE_ADJUST_USE.csv")

write_csv(nla12_size_adj,"data_processed/nla12/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv")

