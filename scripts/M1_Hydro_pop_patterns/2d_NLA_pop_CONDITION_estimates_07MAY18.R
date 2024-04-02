###################
## NLA POPULATION INFERRED CONDITION ESTIMATES
## Purpose: Calculate condition estimates for the NLA 2007 and 2012 (size adjusted) surveys
# Programmer: Tom Kincaid
# Date: October 28, 2014
# Revised: November 3, 2014
# Revised: December 9, 2014
# Revised: March 31, 2015
# Revised: April 3, 2015
# Revised: April 6, 2015
# Revised: May 19, 2015
# Revised: June 1, 2015
# Revised: July 7, 2015
# Revised: September 15, 2015
# Revised: August 22, 2016 (by Karen Blocksom)
# EMI used for NLA lake drawdown project: August 24, 2017 
# 10/10/17 - Emi Ran for 2007 Survey
# 1/10/18 - Using dataset with precipitation class
# 1/31/18 
# 2/14/18 - Updated 2007 dataset with more observations (n=962)
# 5/14/18 - Datasets with NAs
# 10/31/18 Added EI and WRT classes
# 6/26/19 - Updated NLA population weights from TOny 6/24/19


rm(list=ls())

# Load the spsurvey library
library(spsurvey)
library(tidyverse)

#######################################
################
## NLA 2007
################
# 6/26/19 -UPDATED WEIGHTS (n = 1028 with 556 variables)
condition <- read_csv("data_processed/nla07/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

######################
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
condition$Precip_class_man<-cut(condition$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))

table(condition$Precip_class_man) # Relatively balanced
#   < 500  500-900 900-1200    >1200 
#      206      324      322      176 

## Create Precipitation + Lake Origin variable
condition$PRECIP_LO <- with (condition, interaction(Precip_class_man,Lake_Origin_use,sep="_"))
table(condition$PRECIP_LO)


######################
#  SIZE CLASS 
#     Created classes based on quartiles from longformated dataset with both years
######################
summary(condition$LkArea_km2)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.04037  0.24420  0.65200  3.13800  2.04800 66.05000
condition$SIZE_man<-cut(condition$LkArea_km2, breaks=c(0,0.10,0.50,1.0,5.0,50,100),labels=c("4-10    ","10-50   ","50-100  ","100-500 ","500-5000",">5000   "))
table(condition$SIZE_man)

# Create subclass grouping depth class and lake origin
condition$SIZE_LO <- with (condition, interaction(SIZE_man,Lake_Origin_use,sep="_"))

##################
## Create DISTURBANCE + LAKE ORIGIN class
# 1/23/18
###################
summary(condition$RT_NLA12_2015)
#  R   S   T 
#113 663 252 

# Relabel classes
condition$RT_NLA12_2015 <- factor(condition$RT_NLA12_2015, labels=c("ref", 
                                                                    "mod",
                                                                    "hig"))
## Create DISTURBANCE + Lake Origin variable
condition$DISTURB_LO <- with (condition, interaction(RT_NLA12_2015,Lake_Origin_use,sep="_"))
table(condition$DISTURB_LO)

##################
## Create DISTURBANCE + ECOREGION
# 1/23/18
###################

table(condition$ECOP5_2015)
# APPS CENPL   CPL   UMW  WEST 
# 213   331   101   145   238  
table(condition$ECOP6_2015)
# May use 9 regions because we may want to separate out WMT from XER

## Create DISTURBANCE + ECOREGION (9)
condition$DISTURB_ECO <- with (condition, interaction(RT_NLA12_2015,ECOREG_use,sep="_"))
table(condition$DISTURB_ECO)
# BUT there are a lot of classes

## Create DISTURBANCE + ECOREGION (5)
condition$DISTURB_ECO5 <- with (condition, interaction(RT_NLA12_2015,ECOP5_2015,sep="_"))
table(condition$DISTURB_ECO5)

###################
## Create Lake isotope hydro connectivity type + Lake Type
## 7/12/18
###################
##  Flowthrough E:I <0.4
##  Restricted drainage 0.4 >= E:I <1
##  Closed-basin E:I >= 1

table(condition$lk_hydro_iso)
# Closed Flow_through   Restricted 
#   3          769          256

condition$lk_hydro_iso_LO <- with (condition, interaction(lk_hydro_iso, Lake_Origin_use, sep="_"))
table(condition$lk_hydro_iso_LO)
#      Closed_MAN_MADE Flow_through_MAN_MADE   Restricted_MAN_MADE 
#           1                   505                    88 
#Closed_NATURAL  Flow_through_NATURAL    Restricted_NATURAL 
#     2                   264                   168 

##############
## Create Stream Connectivity by Lake type
## 9/7/18
##############
condition$conn_LO <-with(condition, interaction(Class3_f, Lake_Origin_use, sep="_"))
table(condition$conn_LO)

condition$conn_ECO<-with(condition, interaction(Class3_f, ECOREG_use, sep="_")) 
table(condition$conn_ECO)

condition$conn_ECO_LO<-with(condition, interaction(conn_ECO, Lake_Origin_use, sep="_"))  
table(condition$conn_ECO_LO)


#############################
## Process data
# Clean up dataset
names(condition)
todrop <- names(condition)%in% c("X")
condition <- condition[!todrop]

# Recode levels of the condition class variables
levels(condition$TROPHIC_STATE) <- list(OLIGOTROPHIC="Oligotrophic",
                                        MESOTROPHIC="Mesotrophic", EUTROPHIC="Eutrophic",
                                        HYPEREUTROPHIC="Hypereutrophic", Not_assessed = "Not_Assessed")

#############################
# Calculate condition class estimates
# Using cat.analysis in spsurvey 

nr <- nrow(condition)

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=condition$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=condition$SITE_ID,
                     National=rep("All_Lakes",nrow(condition)),
                     Lake_Origin=condition$Lake_Origin_use,
                     Size=condition$SIZE_man,
                     WSA9_Ecoregions=condition$ECOWSA9_2015,
                     WSA9_by_Lake_Origin=condition$WSA9_LO,
                     Trophic=condition$TROPHIC_STATE,
                     Dist_LO=condition$DISTURB_LO,
                     conn=condition$Class3_f,
                     conn_LO=condition$conn_LO,
                     conn_ECO=condition$conn_ECO,
                     conn_ECO_LO=condition$conn_ECO_LO,
                     iso_hydro=condition$lk_hydro_iso,
                     iso_hydro_LO=condition$lk_hydro_iso_LO)

# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
# For 2007 NLA Weight = "WGT_NLA"
design <- data.frame(siteID=condition$SITE_ID,
                     wgt=condition$WGT_SP, # OLD WGT_NLA
                     xcoord=condition$ALBERS_X,
                     ycoord=condition$ALBERS_Y)

# Create the data.cat data frame, which specifies the variables to use in the
# analysis
data.cat <- data.frame(siteID=condition$SITE_ID,
                       DRAWDOWN=condition$Drawdown_CONDus15,
                       EI=condition$EI_class,
                       WRT=condition$WRT_class)

Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)

# Write results as a comma-separated value (csv) file
write_csv(Condition_Estimates, file="Routput/pop_calculations/NLA07_Condition_Estimates_26JUN19.csv")



####################
## Create datasets from CONDITION ouptput to make boxplots 
#   Use Estimate.P to show percent of lakes within subpopulations that are classified by a drawdown class (SMALL, MEDIUM, LARGE - in comparison to regional reference)
#   Have to reshape dataset to make boxplots
####################
library(reshape2)
library(dplyr)

p<-read_csv("Routput/pop_calculations/NLA07_Condition_Estimates_26JUN19.csv")#NLA07_CONTINUOUS_percentile_22DEC17.csv
names(p)
table(p$Type)
table(p$Subpopulation)
table(p$Indicator)

#########
## Subset data "Type" = "WSA9_by_Lake_Origin"
#########

p1<-subset(p,Type=="WSA9_by_Lake_Origin")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3) # Takes first three characters in Subpopulation e.g., CPL_MAN-MADE

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13) # takes characters from 5 to 13
table(p1$Lake_Origin_use)
table(p1$ECOREG_use)


## Subset by response ##
# DRAWDOWN
p2<-subset(p1,Indicator=="DRAWDOWN")
DRAWDOWN_ECO_LK<-droplevels(p2)
table(p2$Indicator)

# ECOREGION_LAKE ORIGIN
# DRAWDOWN
write_csv(DRAWDOWN_ECO_LK,"Routput/pop_calculations/NLA07_LK_ECOREG_DRAWDOWN_CONDITION_CAST_26JUN19.csv") #20MAR18

#########
## Subset data "Type" = "National" - ALL LAKES
#########

p3<-subset(p,Type=="National")
table(p3$Type)
p3<-droplevels(p3)

## Subset by response ##
# DRAWDOWN
p3<-subset(p3,Indicator=="DRAWDOWN")
DRAWDOWN_ALL<-droplevels(p3)
table(p3$Indicator)

# NATIONAL
# DRAWDOWN
write_csv(DRAWDOWN_ALL,"Routput/pop_calculations/NLA07_NATIONAL_DRAWDOWN_CONDITION_CAST_26JUN19.csv") #20MAR18


#########
## Subset data "Type" = "Lake_Origin" - Lake type
#########

p4<-subset(p,Type=="Lake_Origin")
table(p4$Type)
p4<-droplevels(p4)

## Subset by response ##
# DRAWDOWN
p4<-subset(p4,Indicator=="DRAWDOWN")
DRAWDOWN_LK_TYPE<-droplevels(p4)
table(p4$Indicator)

# LAKE ORIGIN
# DRAWDOWN
write_csv(DRAWDOWN_LK_TYPE,"Routput/pop_calculations/NLA07_LK_TYPE_DRAWDOWN_CONDITION_CAST_26JUN19.csv")




####################
## NLA 2012 SIZE-ADJUSTED
####################

rm(list=ls())

# LOAD DATA - SINGLE OBSERVATION dataset - SIZE ADJUSTED
# SIZE ADJUSTED 2012 (n = 951 lakes)
# 6/26/19 UPDATED POP WGTS
condition <- read_csv("data_processed/nla12/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv")

######################
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
condition$Precip_class_man<-cut(condition$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))

table(condition$Precip_class_man) # Relatively balanced
#      < 500  500-900 900-1200    >1200 
#       239      234      209      249

## Create Precipitation + Lake Origin variable
condition$PRECIP_LO <- with (condition, interaction(Precip_class_man,LAKE_ORIGIN,sep="_"))
table(condition$PRECIP_LO)


######################
#  SIZE CLASS 
#     Created classes based on quartiles from longformated dataset with both years
######################
summary(condition$LkArea_km2)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.04037  0.24420  0.65200  3.13800  2.04800 66.05000
condition$SIZE_man<-cut(condition$LkArea_km2, breaks=c(0,0.10,0.50,1.0,5.0,50,100),labels=c("4-10    ","10-50   ","50-100  ","100-500 ","500-5000",">5000   "))
table(condition$SIZE_man)

# Create subclass grouping depth class and lake origin
condition$SIZE_LO <- with (condition, interaction(SIZE_man,LAKE_ORIGIN,sep="_"))

##################
## Create DISTURBANCE + LAKE ORIGIN class
# 1/23/18
###################
summary(condition$RT_NLA12_2015)
#  R   S   T 
#113 663 252 

# Relabel classes
condition$RT_NLA12_2015 <- factor(condition$RT_NLA12_2015, labels=c("ref", 
                                                                    "mod",
                                                                    "hig"))
## Create DISTURBANCE + Lake Origin variable
condition$DISTURB_LO <- with (condition, interaction(RT_NLA12_2015,LAKE_ORIGIN,sep="_"))
table(condition$DISTURB_LO)

##################
## Create DISTURBANCE + ECOREGION
# 1/23/18
###################

table(condition$ECOP5_2015)
#  APPS CENPL   CPL   UMW  WEST 
# 169   293   116   131   242 
table(condition$ECOP6_2015)
# May use 9 regions because we may want to separate out WMT from XER

## Create DISTURBANCE + ECOREGION (9)
condition$DISTURB_ECO <- with (condition, interaction(RT_NLA12_2015,ECOREG_use,sep="_"))
table(condition$DISTURB_ECO)
# BUT there are a lot of classes

## Create DISTURBANCE + ECOREGION (5)
condition$DISTURB_ECO5 <- with (condition, interaction(RT_NLA12_2015,ECOP5_2015,sep="_"))
table(condition$DISTURB_ECO5)

###################
## Create Lake isotope hydro connectivity type + Lake Type
## 7/12/18
###################
##  Flowthrough E:I <0.4
##  Restricted drainage 0.4 >= E:I <1
##  Closed-basin E:I >= 1

table(condition$lk_hydro_iso)
#      Closed Flow_through   Restricted 
#       23          709          217 

condition$lk_hydro_iso_LO <- with (condition, interaction(lk_hydro_iso, LAKE_ORIGIN, sep="_"))
table(condition$lk_hydro_iso_LO)
#      Closed_MAN_MADE Flow_through_MAN_MADE   Restricted_MAN_MADE 
#           9                   426                    95 
#   Closed_NATURAL  Flow_through_NATURAL    Restricted_NATURAL 
#       14                   283                   122

#############################
## Process data
# Clean up dataset
names(condition)
todrop <- names(condition)%in% c("X")
condition <- condition[!todrop]

# Recode levels of the condition class variables
levels(condition$TROPHIC_STATE) <- list(OLIGOTROPHIC="Oligotrophic",
                                        MESOTROPHIC="Mesotrophic", EUTROPHIC="Eutrophic",
                                        HYPEREUTROPHIC="Hypereutrophic", Not_assessed = "Not_Assessed")


#############################
# Calculate condition class estimates
# Using cat.analysis in spsurvey 
#
nr <- nrow(condition)

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=condition$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=condition$SITE_ID,
                     National=rep("All_Lakes",nrow(condition)),
                     Lake_Origin=condition$LAKE_ORIGIN,
                     Size=condition$SIZE_man,
                     # Size_Class=condition$SIZE_CLASS,
                     WSA9_Ecoregions=condition$ECOWSA9_2015,
                     WSA9_by_Lake_Origin=condition$WSA9_LO,
                     Trophic=condition$TROPHIC_STATE,
                     Dist_LO=condition$DISTURB_LO,
                     iso_hydro=condition$lk_hydro_iso,
                     iso_hydro_LO=condition$lk_hydro_iso_LO)

# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
design <- data.frame(siteID=condition$SITE_ID,
                     wgt=condition$WGT_SP, # OLD Weight WGT_ALL
                     xcoord=condition$XCOORD,
                     ycoord=condition$YCOORD)

# Create the data.cat data frame, which specifies the variables to use in the
# analysis
data.cat <- data.frame(siteID=condition$SITE_ID,
                       DRAWDOWN=condition$Drawdown_CONDus15,
                       EI=condition$EI_class,
                       WRT=condition$WRT_class)

Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)


# Print results
cat("\nTrophic State Etimates:\n\n")
Condition_Estimates

###############
# 6/26/19 - SIZE ADJUSTED - n=951 lakes
# SIZE ADJUSTED
write_csv(Condition_Estimates, file="Routput/pop_calculations/NLA12_SIZADJ_Condition_Estimates_26JUN19.csv")

# USE THIS FILE TO CREATE PIE CHARTS





#############
## GET WEIGHT FOR SAP NATURAL LAKE
sap_nat <- condition%>%
  filter(ECOREG_use=="SAP")%>%
  filter(LAKE_ORIGIN=="NATURAL")%>%
  select(SITE_ID, WGT_SP)
