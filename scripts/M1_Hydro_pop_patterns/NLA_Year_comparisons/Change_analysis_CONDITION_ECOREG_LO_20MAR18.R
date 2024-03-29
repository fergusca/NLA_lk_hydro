# File: Change_Estimates.R
# Purpose: Calculate change estimates for the NLA 2007 and NLA 2012 surveys
#  CONDITION CHANGES BETWEEN SURVEY YEARS
# Programmer: Tom Kincaid
# Date: December 5, 2014
# Revised: January 14, 2015
# Revised: March 31, 2015
# Revised: April 1, 2015
# Revised: May 19, 2015
# Revised: June 30, 2015
# Revised: July 8, 2015
# Revised: July 10, 2015
# Revised: September 16, 2015
# EF: March 20, 2018
###################################

rm(list=ls())


# Load the spsurvey library
library(spsurvey)

# Load dataset
change<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/NLA2012_ChangeData_2016-08-23.orig.csv")

myvars_ch<- c("SITE_ID","CH0712_USE","STATUS","CH0712_WGT","REVSAMP_0712",
              "LITCVR_COND","LITRIPCVR_COND","RDIS_COND","DRAWDOWN_COND","NTL_COND")

change_red<- change[myvars_ch]

#########
# Long-format NLA 2007 & 2012 data
#########
# 1/17/18 - updated to include WRT and WYield - single observations n=1876 
# 2/9/18 n=1964
nla07_12_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_13DEC17.csv")
names(nla07_12_all)
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","HorizDD_use","VertDD_use","E_I","RT_iso",
           "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc",
           "XCOORD","YCOORD","Lake_Origin_use","ECOREG_use","WSA9_LO","WGT_ALL","Precip_mm_total_yr",
           "Drawdown_CONDus15")
nla07_12_red <- nla07_12_all[myvars]

summary(nla07_12_red)

# Drop NAs
nla07_12_red<-nla07_12_red[which(!is.na(nla07_12_red$RT_iso)),] #23 observations missing water residence time

#####
# Look at outlier water residence time in XER
# 1/26/18
#   Looks like it's Pyramid Lake SITE_ID- NLA12_NV-109
#####
# subset by Subgroup and by indicator
z<- nla07_12_red[which(nla07_12_red$ECOREG_use=="XER"),]
z<- droplevels(z)
plot(z$RT_iso~z$WGT_ALL)

test2<- nla07_12_all[which(nla07_12_all$RT_iso>100),]
head(test2[c(2,4,5,13,44)])


########
# Merge data
########
test<- merge(nla07_12_red, change_red, by="SITE_ID") 
names(test)

table(test$CH0712_USE)
table(test$REVSAMP_0712)
