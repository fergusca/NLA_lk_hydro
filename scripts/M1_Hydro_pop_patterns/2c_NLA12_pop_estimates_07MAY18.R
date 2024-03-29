# File: Continuous_variable_Estimates_EF_24AUG17.R
## NLA 2012 ##
# Purpose: Calculate continuous estimates for the NLA 2012 survey
#  For Lake drawdown project - estimate drawdown measures and evaporative loss infering for the sample population
# MOdified code from Condition_Estimates.R
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
# EMI modified for NLA lake drawdown project: August 24, 2017 
# EMI modified after talking with Tom Kincaid: August 25, 2017 - don't need to specify STRATUM and popsize - unless want to force what it will sum to
# 9/18/17 - updated NLA 2012 dataset to drop duplicated observations (two lakes had 8 extra observations for some reason)
# 10/6/17 - created untransformed Horizontal dd dataset for boxplots
# 10/10/17 - Added Depth class/Lake origin subclass to estimates
# 11/07/17 - Revised NLA 2012 dataset by including lakes where we had strange E:I values - this added 5 or 6 lakes
# 11/13/17 - Loaded size adjusted 2012 data to compare with 2007 data - estimated hydrologic responses for these lakes after dropping the small lakes from cont.analysis
# 12/12/17 - Added Water Residence Time and Watershed Yield as response variables to estimate for the population
# 12/22/17 - Added Precipitation/Lake Type class
# 1/11/18 - Subpopulation - ECOREGION only after discussion with Phil and Renee
# 1/18/18 - Added PRISM 30yr avg precipitation/lake type class
# 1/24/18 - Added Disturbance classes
# 3/7/18 - Added scaled drawdown variables
# 3.27.18 - Added national subpopulation to size adjusted output
# 4/12/18 - Added scaled vertical dd using the modified max depth
# 8/1/18 - revised E:I estimates (some were missing in first analysis - updated)

# 6/24/19 - updated volume and wrt estimates
# 6/27/19 - updated population weights from Tony 6/24/19
#           USE LAKE_ORIGIN for lake type
# 7/8/19 - updated three lakes with zmax from lit and WRT


# See for R documentation of spsurvey package
# https://www.rdocumentation.org/packages/spsurvey/versions/3.3

citation(package="spsurvey")

# Load the spsurvey library
library(spsurvey)

###################
## LOAD DATA
### FULL NLA 2012 - n = 1038 observations
#nla12 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA_12_FULL_VISIT_1_WGT_USE_PHDI_lkcat_wgt_26JUN19.csv") #NLA12_merge_transform_SINGLE_USE.csv

#####################################################
## SIZE ADJUSTED 2012 dataset (lakes >= 4 ha) to compare with 2007
#  11/13/17
#  12/12/17 - added water balance parameters
#  1/11/18 - Grouped by Ecoregion only
#  1/24/18 - Grouped by Disturbance
#  2/1/18 - Grouped by Ecoregion and Lake type
# 3/27/18 - Added National scale
# 4/12/18 - Scaled Vert DD by modified depth
# 5/11/18 - Log transformed WRT and using dataset with NAs
# 8/2/18  - Updated E:I & RT estimates
# 6/24/19 - Updated Volume and RT estimates
# 6/27/19 - updated pop weights from Tony 6/24/19
#####################################################
# LOAD SIZE ADJUSTED NLA 2012 dataset n = 951 lakes

nla12 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv") # OLD C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_merge_transform_SINGLE_SIZE_ADJUST_USE_24JUN19.csv
# n=951 with 545 variables
test<-subset(nla12,UID==7769) # AL-113 Should have E:I = 0

## Process data
# Clean up dataset
names(nla12)
todrop <- names(nla12)%in% c("X")
nla12 <- nla12[!todrop]
nr <- nrow(nla12)

summary(nla12$HorizDD_use)
summary(nla12$VertDD_use)
z2 <- nla12[which(nla12$HorizDD_use>700),]
t2 <- nla12[which(nla12$VertDD_use> 44),]

######################
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
nla12$Precip_class_man<-cut(nla12$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))

table(nla12$Precip_class_man) # Relatively balanced
#    < 500  500-900 900-1200    >1200 
#     239      234      209      249  

## Create Precipitation + Lake Origin variable
nla12$PRECIP_LO <- with (nla12, interaction(Precip_class_man,LAKE_ORIGIN,sep="_"))
table(nla12$PRECIP_LO)

#####################
## Create DEPTH Class used in Renee's 2014 paper
# 10/10/17
nla12$DEPTH_COND<-cut(nla12$DpthMx_use, breaks=c(0,2,3,5,10,20,100),labels=c("< 2  ","2-3  ","3-5  ","5-10 ","10-20",">20  "))

# Create subclass grouping depth class and lake origin
nla12$DEPTH_LO <- with (nla12, interaction(DEPTH_COND,LAKE_ORIGIN,sep="_"))

table(nla12$DEPTH_LO)
names(nla12)

##################
## Create DISTURBANCE + LAKE ORIGIN class
# 1/24/18
###################
summary(nla12$RT_NLA12_2015)
#   R   S   T 
# 144 519 255 

# Relabel classes
nla12$RT_NLA12_2015 <- factor(nla12$RT_NLA12_2015, labels=c("ref", 
                                                            "mod",
                                                            "hig"))
## Create DISTURBANCE + Lake Origin variable
nla12$DISTURB_LO <- with (nla12, interaction(RT_NLA12_2015,LAKE_ORIGIN,sep="_"))
table(nla12$DISTURB_LO)

##################
## Create DISTURBANCE + ECOREGION
# 1/24/18
###################

table(nla12$ECOP5_2015)
#  APPS CENPL   CPL   UMW  WEST 
#  161   288   104   129   23656
table(nla12$ECOP6_2015)
# May use 9 regions because we may want to separate out WMT from XER

## Create DISTURBANCE + ECOREGION (9)
nla12$DISTURB_ECO <- with (nla12, interaction(RT_NLA12_2015,ECOREG_use,sep="_"))
table(nla12$DISTURB_ECO)
# BUT there are a lot of classes

## Create DISTURBANCE + ECOREGION (5)
nla12$DISTURB_ECO5 <- with (nla12, interaction(RT_NLA12_2015,ECOP5_2015,sep="_"))
table(nla12$DISTURB_ECO5)


#################
## CALCULATE POPULATION ESTIMATES
#################

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=nla12$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=nla12$SITE_ID,
                     National=rep("All_Lakes",nrow(nla12)),
                     Lake_Origin=nla12$LAKE_ORIGIN,
                     WSA9_Ecoregions=nla12$ECOWSA9_2015,
                     WSA9_by_Lake_Origin=nla12$WSA9_LO,
                     Depth=nla12$DEPTH_LO,
                     Iso_hydro=nla12$lk_hydro_iso)#,


# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
design <- data.frame(siteID=nla12$SITE_ID,
                     wgt=nla12$WGT_SP, #Old weight WGT_ALL
                     xcoord=nla12$XCOORD,
                     ycoord=nla12$YCOORD)


# Create data.cont data frame, specifies the response variables to use
data.cont <- data.frame(siteID=nla12$SITE_ID,
                        vert=nla12$VertDD_use,
                        horiz=nla12$HorizDD_use,
                        E_I=nla12$E_I,
                        WRT=nla12$RT_iso,
                        L_vert=nla12$L_VertDD_use,
                        L_horiz=nla12$L_HorizDD_use,
                        L_vert_sc=nla12$L_DDVrtDix_sc,
                        L_vert_sc_mod=nla12$L_DDVrtDix_sc_MOD,
                        L_horiz_sc=nla12$L_DDHzSqrtA_sc,
                        vert_sc=nla12$DDVrtDix_sc,
                        vert_sc_mod=nla12$DDVrtDix_sc_MOD,
                        horiz_sc=nla12$DDHzSqrtA_sc,
                        L_RT=nla12$L_RT_iso)


# Can just run this one if don't want the text output
Cont_var_Estimates <- cont.analysis(sites, subpop, design, data.cont) # dropped pop.size

# Print results
Cont_var_Estimates

## NOTE Cont_var_Estimates creates a list of dataframes: CDF = cumulative distribution function,
#   Pct = percentiles (percentiles of sample population do not have standard error values),

#######
# Write the CDF within Cont_var_Estimates 
#   These are the cumulative distribution function values 
#     Estimate.P = probability that that it is on or less than the value.
#     Estimate.U = number of lakes or less than that are that value or less than that value
write.csv(Cont_var_Estimates$CDF,file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_CONTINUOUS_SIZEADJ_Var_Estimates_27JUN19.csv", # 24JUN19 07MAR18 22DEC17.csv old 12DEC17
          row.names=FALSE)

# Write the Pct within Cont_var_Estimates  - USE this one for boxplots
write.csv(Cont_var_Estimates$Pct,file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_CONTINUOUS_SIZEADJ_percentile_27JUN19.csv", # 07MAR18 old 12DEC17
          row.names=FALSE)


####################
## Create datasets from ouptput to make boxplots using percentile values
####################
library(reshape2)
library(dplyr)

p<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_CONTINUOUS_SIZEADJ_percentile_27JUN19.csv") #22DEC17 #M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_CONTINUOUS_SIZEADJ_percentile_07MAR18.csv
names(p)
table(p$Subpopulation)


############
## Subset data "Type" = "WSA9_by_Lake_Origin" - SIZE ADJUSTED 2012 dataset
############
p1<-subset(p,Type=="WSA9_by_Lake_Origin")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3)

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13)
table(p1$Lake_Origin_use)

## Subset by response ##
# E:I
p3<-subset(p1,Indicator=="E_I")
p3<-droplevels(p3)
table(p3$Indicator)

# Untransformed Horizontal DD
p5<-subset(p1,Indicator=="horiz")
p5<-droplevels(p5)
table(p5$Indicator)

# Untransformed Vertical DD
p6<-subset(p1,Indicator=="vert")
p6<-droplevels(p6)
table(p6$Indicator)

# WRT
p7<-subset(p1,Indicator=="WRT")
p7<-droplevels(p7)
table(p7$Indicator)

# Untransformed SCALED Horizontal DD
p8<-subset(p1,Indicator=="horiz_sc")
p8<-droplevels(p8)
table(p8$Indicator)

# Untransformed Vertical DD
p9<-subset(p1,Indicator=="vert_sc")
p9<-droplevels(p9)
table(p9$Indicator)

# Untransformed SCALED Vertical DD USING MODIFIED DEPTH  - USE THIS ONE
p10<-subset(p1,Indicator=="vert_sc_mod")
p10<-droplevels(p10)
table(p10$Indicator)


#####################
### Cast dataframe to have percentile stat have separate columns ###
## SIZE ADJUSTED ## n=951
# EI
E_I <- dcast(p3, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(E_I,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_E_I_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv") # 24JUN19 18JAN18 06AUG18

# HorizDD
Horiz <- dcast(p5, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Horiz,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_HorizDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# VertDD
Vert <- dcast(p6, Type + ECOREG_use  + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Vert,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_VertDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# WRT
WRT <- dcast(p7, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(WRT,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_WRT_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# SCALED HorizDD untrans
Horiz <- dcast(p8, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Horiz,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_SCALED_HorizDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv") # 07MAR18

# SCALED VertDD untrans
Vert_sc <- dcast(p9, Type + ECOREG_use  + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Vert,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_SCALED_VertDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# SCALED Vertical DD
Vert_sc_mod <- dcast(p10, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Vert_sc_mod,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/NLA12_LK_ECOREG_SCALED_VertDD_MODIFIED_percentile_CAST_27JUN19.csv")



#############
## Plot CDFs for subpopulations and variables
#############
# Use cont.cdfplot

# Let's only plot the log transformed variables and E:I which doesn't seem to need to be transformed
# Create separate dataframe
library(dplyr)
cont_vars_red_a <- Cont_var_Estimates$CDF
cont_vars_red <- subset(cont_vars_red_a, Indicator %in% c("vert_log","horiz_log","E_I"))

# PLOTS
cont.cdfplot(pdffile="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA12/cdfs_2012_10OCT17.pdf",
             cont_vars_red, units.cdf="Percent", 
             xlbl="", ylbl="Percent", ylbl.r=NULL, legloc="BR")


