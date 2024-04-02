###########################
## TRIMMED DOWN SCRIPT TO MAKE POPULATION ESTIMATES
##  NLA 2007
#
# File: Continuous_variable_Estimates_EF_24AUG17.R
## NLA 2007 ##
# Purpose: Calculate continuous estimates for the NLA 2007 survey
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
# Emi edited for NLA 2007 data: 9/7/17
# EMI 10/6/17 - created non-transformed Horizontal drawdown dataset to create boxplots 
# EMI 10/10/17 - Created Depth class based on classes in Renee's 2014 
#                 Re ran cont.analysis with Depthclass/Lake origin as a subpopulation
# EMI 12/12/17 - Added Water residence time and watershed yield as response variables to get population estimates

# 12/23/17 - Added Precipitation/Lake type class

#1/11/18 - Cast data tables Subpopulation = Ecoregion only - after discussion with Phil and Renee

# 1/18/18 - Added PRISM 30yr avg precipitation/lake type class
# 1/29/18 - Added National as a subpopulation to get the statistics for all lakes in the dataset

# 3/5/18 - Added scaled drawdown variables following discussion wtih Phil to see if patterns match up with E:I 
#           (L_DDHzSqrtA_sc, L_DDVrtDix_sc)

# 5/9/18 - Added log transformed drawdown (not scaled)

# 9/7/18 - ran with Lake connectivity classes

# 6/25/19 - updated population weights from Tony (emailed 6/24/19)
#######
# See for R documentation of spsurvey package
# https://www.rdocumentation.org/packages/spsurvey/versions/3.3
############################

rm(list=ls())

############
# Load the spsurvey library
library(tidyverse)
library(spsurvey)
library(dplyr)
library(reshape2)

citation(package="spsurvey")

##################
## LOAD DATA
# 6/25/19 - data processed NLA 2007 lakes with updated weights
#   n=1028 w/556 variables
#   USE WGT_SP for population weights
nla07<-read_csv("data_processed/nla07/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

names(nla07)

##########
## Process data
# Clean up dataset
names(nla07)
todrop <- names(nla07)%in% c("X")
nla07 <- nla07[!todrop]
nr <- nrow(nla07)

#################
## PRISM 30yr avg PRECIPITATION CLASS - "Precip_PT" PRISM 30yr avg for lake point - have this in 2012 dataset too
# 1/18/18
summary(nla07$Precip_PT)
nla07$Precip_AVG_class_man<-cut(nla07$Precip_PT, breaks=c(75,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))

table(nla07$Precip_AVG_class_man)
# < 500  500-900 900-1200    >1200 
#  203      287      286      185 

## Create Precipitation + Lake Origin variable
nla07$PRECIP_AVG_LO <- with (nla07, interaction(Precip_AVG_class_man,Lake_Origin_use,sep="_"))
table(nla07$PRECIP_AVG_LO)
#   < 500_MAN_MADE  500-900_MAN_MADE 900-1200_MAN_MADE    >1200_MAN_MADE 
#       136               106               183               121 
#< 500_NATURAL   500-900_NATURAL  900-1200_NATURAL     >1200_NATURAL 
#         67               181               103                64 
######################
# 2007 Water year PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
nla07$Precip_class_man<-cut(nla07$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))

table(nla07$Precip_class_man) # Relatively balanced
#   < 500  500-900 900-1200    >1200 
#     206      323      321      176

## Create Precipitation + Lake Origin variable
nla07$PRECIP_LO <- with (nla07, interaction(Precip_class_man,Lake_Origin_use,sep="_"))
table(nla07$PRECIP_LO)
#    < 500_MAN_MADE  500-900_MAN_MADE 900-1200_MAN_MADE    >1200_MAN_MADE 
#         158               115               198               122 
#     < 500_NATURAL   500-900_NATURAL  900-1200_NATURAL     >1200_NATURAL 
#         48               208               123                54

#####################
## Create DEPTH Class used in Renee's 2014 paper
# 10/10/17
nla07$DEPTH_COND<-cut(nla07$DpthMx_use, breaks=c(0,2,3,5,10,20,100),labels=c("< 2  ","2-3  ","3-5  ","5-10 ","10-20",">20  "))

# Create subclass grouping depth class and lake origin
nla07$DEPTH_LO <- with (nla07, interaction(DEPTH_COND,Lake_Origin_use,sep="_"))

table(nla07$DEPTH_LO)

names(nla07)

##################
## Create DISTURBANCE + LAKE ORIGIN class
# 1/23/18
###################
summary(nla07$RT_NLA12_2015)
# R   S   T 
# 98 566 210

# Relabel classes
nla07$RT_NLA12_2015 <- factor(nla07$RT_NLA12_2015, labels=c("ref", 
                                                            "mod",
                                                            "hig"))
## Create DISTURBANCE + Lake Origin variable
nla07$DISTURB_LO <- with (nla07, interaction(RT_NLA12_2015,Lake_Origin_use,sep="_"))
table(nla07$DISTURB_LO)

##################
## Create DISTURBANCE + ECOREGION
# 1/23/18
###################

table(nla07$ECOP5_2015)
# APPS CENPL   CPL   UMW  WEST 
# 213   331   101   145   238  
table(nla07$ECOP6_2015)
# May use 9 regions because we may want to separate out WMT from XER

## Create DISTURBANCE + ECOREGION (9)
nla07$DISTURB_ECO <- with (nla07, interaction(RT_NLA12_2015,ECOREG_use,sep="_"))
table(nla07$DISTURB_ECO)
# BUT there are a lot of classes

## Create DISTURBANCE + ECOREGION (5)
nla07$DISTURB_ECO5 <- with (nla07, interaction(RT_NLA12_2015,ECOP5_2015,sep="_"))
table(nla07$DISTURB_ECO5)


##############
## Create Stream Connectivity by Lake type
## 9/7/18
##############
nla07$conn_LO <-with(nla07, interaction(Class3_f, Lake_Origin_use, sep="_"))
table(nla07$conn_LO)

nla07$conn_ECO<-with(nla07, interaction(Class3_f, ECOREG_use, sep="_")) 
table(nla07$conn_ECO)

nla07$conn_ECO_LO<-with(nla07, interaction(conn_ECO, Lake_Origin_use, sep="_"))  
table(nla07$conn_ECO_LO)

#####################
# Calculate continuous estimates weighted by sample design
####################

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=nla07$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
subpop <- data.frame(siteID=nla07$SITE_ID,
                     National=rep("All_Lakes",nrow(nla07)),
                     Lake_Origin=nla07$Lake_Origin_use,
                     WSA9_Ecoregions=nla07$ECOWSA9_2015,
                     WSA9_by_Lake_Origin=nla07$WSA9_LO,
                     conn=nla07$Class3_f,
                     conn_LO=nla07$conn_LO,
                     conn_ECO=nla07$conn_ECO,
                     conn_ECO_LO=nla07$conn_ECO_LO,
                     iso_hydro=nla07$lk_hydro_iso,
                     iso_hydro_LO=nla07$lk_hydro_iso_LO)


# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
design <- data.frame(siteID=nla07$SITE_ID,
                     wgt=nla07$WGT_SP, # OLD WEIGHTS "WGT_NLA"
                     xcoord=nla07$ALBERS_X,
                     ycoord=nla07$ALBERS_Y)


# Create data.cont data frame, specifies the response variables to use
data.cont <- data.frame(siteID=nla07$SITE_ID,
                        vert=nla07$VertDD_use,
                        horiz=nla07$HorizDD_use,
                        E_I=nla07$E_I,
                        RT=nla07$RT_iso,
                        L_vert=nla07$L_VertDD_use,
                        L_horiz=nla07$L_HorizDD_use,
                        L_vert_sc=nla07$L_DDVrtDix_sc,
                        L_horiz_sc=nla07$L_DDHzSqrtA_sc,
                        vert_sc=nla07$DDVrtDix_sc,
                        vert_sc_mod=nla07$DDVrtDix_sc_MOD,
                        horiz_sc=nla07$DDHzSqrtA_sc,
                        L_RT=nla07$L_RT_iso)


# Calculate the estimates
Cont_var_Estimates <- cont.analysis(sites, subpop, design, data.cont) # dropped pop.size


# Print results
#cat("\nTrophic State Etimates:\n\n")
Cont_var_Estimates


## NOTE Cont_var_Estimates creates a list of dataframes: CDF = cumulative distribution function,
#   Pct = percentiles (percentiles of sample population do not have standard error values),

#######
#6/25/19
# Write the CDF ESTIMATE VALUES within Cont_var_Estimates 
write.csv(Cont_var_Estimates$CDF,file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_CONTINUOUS_Var_Estimates_25JUN19.csv", # OLD 12DEC17
          row.names=FALSE)

# Write the PERCENTILE within Cont_var_Estimates - USE this one for Boxplots
write.csv(Cont_var_Estimates$Pct,file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_CONTINUOUS_percentile_25JUN19.csv",
          row.names=FALSE)


#############
## Plot CDFs for subpopulations and variables
#############
# Use cont.cdfplot

# Let's only plot the log transformed variables and E:I which doesn't seem to need to be transformed
# Create separate dataframe
library(dplyr)
# Read cdf output
cont_vars_red_a <- read_csv("Routput/pop_calculations/NLA07_CONTINUOUS_Var_Estimates_05MAR18.csv")
cont_vars_red_a <- Cont_var_Estimates$CDF
cont_vars_red <- subset(cont_vars_red_a, Indicator %in% c("vert_log","horiz_log","E_I","RT"))

# PLOTS
cont.cdfplot(pdffile="Routput/pop_calculations/figs/cdfs_2007_26SEPT18.pdf", #18SEPT17
             cont_vars_red, units.cdf="Percent", 
             xlbl="", ylbl="Percent", ylbl.r=NULL, legloc="BR")


####################
## Create datasets from ouptput to make boxplots using percentile values
#    not the estimated values because those are only useful to create cumulative distribution function plot
#    We can use the percentile values to manually create boxplots of the drawdown distribution values
####################
#########
## Subset data "Type" = "WSA9_by_Lake_Origin"
#########
# 6/25/19 - updated population weights

library(reshape2)
library(dplyr)

p<-read_csv("Routput/pop_calculations/NLA07_CONTINUOUS_percentile_25JUN19.csv")
names(p)
table(p$Type)

p1<-subset(p,Type=="WSA9_by_Lake_Origin")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3) # Takes first three characters in Subpopulation e.g., CPL_MAN-MADE

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13) # takes characters from 5 to 13
table(p1$Lake_Origin_use)
table(p1$ECOREG_use)


## Subset by response ##
# E:I
p3<-subset(p1,Indicator=="E_I")
p3<-droplevels(p3)
table(p3$Indicator)

# Horizontal DD - NON TRANSFORMED
p5<-subset(p1, Indicator=="horiz")
p5<-droplevels(p5)
table(p5$Indicator)

# Vertical DD - NON TRANSFORMED
p6<-subset(p1, Indicator=="vert")
p6<-droplevels(p6)
table(p6$Indicator)

# Water Residence Time 
p7<-subset(p1, Indicator=="RT")
p7<-droplevels(p7)
table(p7$Indicator)

# SCALED HORIZONTAL DD
p8<-subset(p1, Indicator=="horiz_sc")
p8<-droplevels(p8)
table(p8$Indicator)

# SCALED VERTICAL DD
p9<-subset(p1, Indicator=="vert_sc")
p9<-droplevels(p9)
table(p9$Indicator)

# SCALED VERTICAL DD with Modified Zmax
p10 <-subset(p1, Indicator=="vert_sc_mod")
p10<-droplevels(p10)
table(p10$Indicator)

### Cast dataframe to have percentile stat have separate columns ###
# ECOREGION_LAKE ORIGIN
# EI
E_I <- dcast(p3, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(E_I,"Routput/pop_calculations/NLA07_LK_ECOREG_E_I_percentile_CAST_25JUN19.csv") #08FEB18

# Horizontal DD - NON TRANSFORMED
Horiz <- dcast(p5, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Horiz,"Routput/pop_calculations/NLA07_LK_ECOREG_HorizDD_RAW_percentile_CAST_25JUN19.csv")

# Vertical DD - NON TRANSFORMED
Vert <- dcast(p6, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Vert,"Routput/pop_calculations/NLA07_LK_ECOREG_VertDD_RAW_percentile_CAST_25JUN19.csv")

# Water residence time
WRT <- dcast(p7, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(WRT,"Routput/pop_calculations/NLA07_LK_ECOREG_WRT_percentile_CAST_25JUN19.csv")

# SCALED Horizontal DD
Horiz_sc <- dcast(p8, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Horiz_sc,"Routput/pop_calculations/NLA07_LK_ECOREG_SCALED_HorizDD_percentile_CAST_25JUN19.csv")

# SCALED Vertical DD
Vert_sc <- dcast(p9, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Vert_sc,"Routput/pop_calculations/NLA07_LK_ECOREG_SCALED_VertDD_percentile_CAST_25JUN19.csv")

# SCALED Vertical DD
Vert_sc_mod <- dcast(p10, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write.csv(Vert_sc_mod,"Routput/pop_calculations/NLA07_LK_ECOREG_SCALED_VertDD_MOD_percentile_CAST_25JUN19.csv")
