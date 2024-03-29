# File: Change_Estimates.R
# Purpose: Population inferred Calculate change estimates for the NLA 2007 and NLA 2012 surveys
#  from Tom Kincaid - This change analysis includes the variable CH0712_USE that includes categories "Include" and "Exclude"
#  The "Exclude category identifies sites in the 1-4 ha size class for NLA 2012. 
#  "Include" will take into account different size classes to make change analysis comparable between years
#   CH0712_WGT - are the weights for the two surveys and has not been adjusted to account for lake size distribution differences between the two surveys

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
# EF: November 2, 2017
# Revised: November 6, 2017 to recode levels for the WSA9_LO variable and to
#    create a revised version of the repeatdf data frame
# EF: December 13, 2017 - included WRT and Watershed yield

# 1/22/18 - DEPTH & Lake Origin inferred pop estimates
#           AREA & Lake Origin inferred pop estimates
# 2/9/18 - updated with larger NLA 07 dataset  - change by ecoregion and lake type

# 3/8/18 - Added scaled drawdown variables

#5/15/18 - using full datasets with NAs

# 7/18/18 - Put place holder in change dotplot for missing LARGE drawdown in UMW man-made and MEDIUM & LARGE dd in NAP natural lakes

# 8/6/18 - UPDATED E:I and RT estimates (more observations)

# Load the spsurvey library
rm(list=ls())

library(spsurvey)
citation(package="spsurvey")

####################
## Bring variables in NLA2012_ChangeData_2016-08-23.orig.csv 
####################
change<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/NLA2012_ChangeData_2016-08-23.orig.csv")

myvars_ch<- c("SITE_ID","CH0712_USE","STATUS","CH0712_WGT","REVSAMP_0712",
              "LITCVR_COND","LITRIPCVR_COND","RDIS_COND","DRAWDOWN_COND","NTL_COND")

change_red<- change[myvars_ch]

#########
# Long-format NLA 2007 & 2012 data
#########
# 1/17/18 - updated to include WRT and WYield - single observations n=1876 
# 2/9/18 n=1964
# 5/14/18 - FULL datasets with NAs n=2064
# 5/15/18 - FULL datasets with NAs n=2066 (revised NLA07 n=1028)
# 8/6/18  - updated E:I & RT estimates
nla07_12_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_02AUG18.csv") # nla07_12_rbind_LONG_ALL_SINGLE_14MAY18 13DEC17
names(nla07_12_all)
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","HorizDD_use","VertDD_use","E_I","RT_iso",
           "DDHzSqrtA_sc","DDVrtDix_sc","L_DDHzSqrtA_sc","L_DDVrtDix_sc","L_RT_iso","DDVrtDix_sc_MOD","L_DDVrtDix_sc_MOD",
           "XCOORD","YCOORD","Lake_Origin_use","ECOREG_use","WSA9_LO","WGT_ALL","Precip_mm_total_yr",
           "Drawdown_CONDus15")
nla07_12_red <- nla07_12_all[myvars]

summary(nla07_12_red)

# 5//14/18 - don't need to drop NAs 
#nla07_12_red<-nla07_12_red[which(!is.na(nla07_12_red$RT_iso)),] #23 observations missing water residence time

#####
# Look at outlier water residence time in XER
# 1/26/18
#   Looks like it's Pyramid Lake SITE_ID- NLA12_NV-109
#####
# subset by Subgroup and by indicator
z<- nla07_12_red[which(nla07_12_red$ECOREG_use=="XER"),]
z<- droplevels(z)
plot(z$RT_iso~z$WGT_ALL)

test2<- nla07_12_all[which(nla07_12_all$RT_iso>100),]# 2 lakes (Pyramid (2012) and Soda Lake (2007) - MV)
head(test2[c(2,4,5,13,44)])


########
# Merge data
########
test<- merge(nla07_12_red, change_red, by="SITE_ID") 
names(test)

table(test$CH0712_USE)
#Exclude Include 
# 88    1978

table(test$REVSAMP_0712)
#   N    Y 
# 1346  720

#############
# Read the file containing NLA 2007 and NLA 2012 data and retain sites of
# interest
#############
# 10/31/17 - Long-format 2007 & 2012 reduced NLA datasets
#   Subset data to keep lakes that are labeled as "Include" and part of the target sample population
#changestatus <- read.csv("L:/Priv/ARM Data/NLA 2012/NLA 2012 Change Estimation/NLA2012_ChangeData_2016-08-23.orig.csv", row.names = 1)
#read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE.csv", row.names=1)
changestatus <- test
# Keep CH0712_USE="Include" which will drop small systems
changestatus <- droplevels(subset(changestatus, CH0712_USE == "Include" &
                                    STATUS == "Target_Sampled"))
nr <- nrow(changestatus)

# Create a data frame that contains only repeat visit sites
#   BUT needs to be modified b/c inner_join may have messed up order
repeatdf_a <- subset(changestatus, REVSAMP_0712 == "Y")

###############
## DROPPING PYRAMID LAKE - to compare results
# 1/26/18
###############
# Drop Pyramid lake
changestatus <- changestatus[!(changestatus$SITE_ID %in% c("NLA12_NV-109")),]
# Drop Soda lake ?

changestatus <- droplevels(subset(changestatus, CH0712_USE == "Include" &
                                    STATUS == "Target_Sampled"))
nr <- nrow(changestatus)

###################

# Create a data frame that contains only repeat visit sites
#   BUT needs to be modified b/c inner_join may have messed up order
repeatdf_a <- subset(changestatus, REVSAMP_0712 == "Y")


# Recode levels of the condition class variables
levels(changestatus$NTL_COND) <- list(Good="Good", Fair="Fair",
                                      Poor="Poor", "Not Assessed"="Not Assessed")
levels(changestatus$LITCVR_COND) <- list(Good="Good", Fair="Fair",
                                         Poor="Poor", "Not Assessed"="Not Assessed")
levels(changestatus$LITRIPCVR_COND) <- list(Good="Good", Fair="Fair",
                                            Poor="Poor", "Not Assessed"="Not Assessed")
levels(changestatus$RDIS_COND) <- list(Good="Good", Fair="Fair",
                                       Poor="Poor", "Not Assessed"="Not Assessed")
levels(changestatus$DRAWDOWN_COND) <- list(Small="Small", Medium="Medium",
                                           Large="Large", "Not Assessed"="Not Assessed")
table(changestatus$Drawdown_CONDus15)
levels(changestatus$Drawdown_CONDus15) <- list(SMALL="Small", MEDIUM="Medium",
                                               LARGE="Large", "NOT ASSESSED"="Not Assessed")


###########
## There are unequal number of observations per year
###########
#droplevels(repeatdf_a)
table(repeatdf_a$YEAR)
# 2007 2012 
# 360  360  

# Create a data frame containing data for repeat visit sites
# Tom added 11/6/17 to get numbers to match up
repeatdf_07 <- subset(changestatus, REVSAMP_0712 == "Y" & YEAR == 2007)
repeatdf_12 <- subset(changestatus, REVSAMP_0712 == "Y" & YEAR == 2012)
repeatdf_07 <- subset(repeatdf_07, SID %in% repeatdf_12$SID)
repeatdf_12 <- subset(repeatdf_12, SID %in% repeatdf_07$SID)
indx <- match(repeatdf_07$SID, repeatdf_12$SID) # match basedon SID and make 2012 have same order as 2007
repeatdf_12 <- repeatdf_12[indx,]
repeatdf <- droplevels(rbind(repeatdf_07, repeatdf_12)) # 694 lakes sampled both years

#########
# Look to see if data are balanced between years for Ecoregion/lake type classes

#############
with(repeatdf, addmargins(table(ECOREG_use, YEAR, useNA="ifany"))) # Looks good equal
#           YEAR
# ECOREG_use 2007 2012 Sum
#CPL   38   38  76
#NAP   37   37  74
#NPL   20   20  40
#SAP   38   38  76
#SPL   47   47  94
#TPL   48   48  96
#UMW   48   48  96
#WMT   43   43  86
#XER   29   29  58
#Sum  348  348 696

with(repeatdf, addmargins(table(Lake_Origin_use, YEAR, useNA="ifany")))
#               YEAR
#Lake_Origin_use 2007 2012 Sum
#MAN_MADE  198  198 396
#NATURAL   150  150 300
#Sum       348  348 696



with(changestatus, addmargins(table(Lake_Origin_use, YEAR, useNA="ifany")))
#                              YEAR
# Lake_Origin_use 2007 2012  Sum
#MAN_MADE  594  519 1113
#NATURAL   434  410  844
#<NA>        0   20   20
#Sum      1028  949 1977
###########################
## Change analysis
###########################
summary(changestatus)
# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=changestatus$SITE_ID,
                    survey1=changestatus$YEAR == 2007,
                    survey2=changestatus$YEAR == 2012)

# Create the repeats data frame, which identifies repeat visit sites to use in
# the analysis - creates dataframe of repeat obs by SITE_ID and Year (which was matched by SID)
# NOTE - Precip and lake order class is unbalanced between years
#   Tom 1/9/18 - The change.analysis will give output but there were be more variance
#               It also may not be so good because they are not independent observations - but maybe okay??
repeats <- data.frame(
  siteID_1=repeatdf$SITE_ID[repeatdf$YEAR == 2007],
  siteID_2=repeatdf$SITE_ID[repeatdf$YEAR == 2012])


# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=changestatus$SITE_ID,
                     National=rep("National", nr),
                     Lake_Origin=changestatus$Lake_Origin_use,
                     WSA9_Ecoregions_LO=changestatus$WSA9_LO) # WSA3_Ecoregions=changestatus$AGGR_ECO3_2015,EPA_REG=changestatus$EPA_REG,SIZE_CLASS=changestatus$SIZE_CLASS)

# Create the design data frame, which identifies the stratum code, weight,
# x-coordinate, and y-coordinate for each site ID
design <- data.frame(siteID=changestatus$SITE_ID,
                     wgt=changestatus$CH0712_WGT, #CH0712_WGT
                     xcoord=changestatus$XCOORD,
                     ycoord=changestatus$YCOORD)

# Create the data.cat data frame, which specifies categorical variables to use
# in the analysis
data.cat <- data.frame(siteID=changestatus$SITE_ID,
                       Lake_Drawdown_Exposure=changestatus$Drawdown_CONDus15)#


data.cont <- data.frame(siteID=changestatus$SITE_ID,
                        vert=changestatus$VertDD_use,
                        horiz=changestatus$HorizDD_use,
                        EI=changestatus$E_I,
                        WRT=changestatus$RT_iso,
                        L_vert_sc=changestatus$L_DDVrtDix_sc,
                        L_horiz_sc=changestatus$L_DDHzSqrtA_sc,
                        vert_sc=changestatus$DDVrtDix_sc,
                        horiz_sc=changestatus$DDHzSqrtA_sc,
                        vert_sc_mod=changestatus$DDVrtDix_sc_MOD,
                        L_vert_sc_mod=changestatus$L_DDVrtDix_sc_MOD,
                        L_WRT=changestatus$L_RT_iso)

# For precip/laketype class - set repeats=NULL
Change_Estimates <- change.analysis(sites, repeats=NULL, subpop, design, data.cat,data.cont,
                                    test=c("mean","median")) # Can specify to calculate estimates for mean and median

# FROM Change analysis Vignette https://cran.r-project.org/web/packages/spsurvey/vignettes/Change_Analysis.pdf
#   and Tom's email 4/3/18
# Median in change analysis - first step is to calculate an estimate of the median for the first survey
# Then use that estimate to define two categories: 1)values that are less than or equal to estimated median; 2) values greater than estimated median
# Once categories are defined, change analysis for hte median is identical to change analysis for a categorical varialbbe - change is estimated by th edifference in category estimates
###################
# WRITE OUTPUT
#   Write results as comma-separated value (csv) files
# 2/9/18 - Dropped Pyramid Lake from this analysis
# 3/8/18 - added scaled drawdown variables
# 4/4/18 - added change analysis based on median
# 5/14/18 - Added full dataset with NAs and log transformed vars
# 5/15/18 - Full dataset with NAs and revised NLA07
# 8/7/18 - Updated E:I and RT estimates
###################
## NOTES on Output - .p = percent change; .u = count of lakes in inferred population
# First estimate is earlier time step, Second estimate is later time step
#  Difference is later time step minus earlier time step 
#    so that (0) indicates no change, (+) indicates an increase, and (-) indicates a decrease

# catsum are condition category output
write.csv(Change_Estimates$catsum, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONDITION_Estimates_14MAY18.csv") # 04APR18 08MAR18 #OLD 09FEB18

# continuous variable - mean change estimates
write.csv(Change_Estimates$contsum_mean, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONTINUOUS_MEAN_Estimates_07AUG18.csv") #NLA0712_Change_ECOREG_CONTINUOUS_MEAN_Estimates_14MAY18

# continuous variable - median change estimates
# BUT NOTE Median has a different interpretation - number that varies from median in first estimate
write.csv(Change_Estimates$contsum_median, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONTINUOUS_MEDIAN_Estimates_14MAY18.csv")


###########
### OUTPUT DROPPING PYRAMID LAKE
# 1/26/18
###########
# catsum are condition category output
#write.csv(Change_Estimates$catsum, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONDITION_Estimates_DROP_26JAN18.csv")

# continuous variable - mean change estimates
#write.csv(Change_Estimates$contsum_mean, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONTINUOUS_Estimates_DROP_26JAN18.csv")

######################
######################
# Dot plots showing change between years 
######################

library(reshape2)
library(dplyr)
library(ggplot2)

library(Hmisc)
library(plotrix)
library(pgirmess)

library(gridExtra)

# FULL DATASET
#p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONTINUOUS_Estimates_22JAN18.csv")

# Dropping outlier
# 2/9/18
# 3/8/18 - scaled variables
# 5/15/18 - full datasets
# 8/7/18
# CONTINUOUS values
t<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONTINUOUS_MEAN_Estimates_14MAY18.csv") # 

p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONTINUOUS_MEAN_Estimates_07AUG18.csv") # 14MAY18
names(p)
table(p$Subpopulation)
table(p$Type)

# Subset ECOREGION/lake type class
p1<-subset(p,Type=="WSA9_Ecoregions_LO")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3)

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13)

table(p1$Lake_Origin_use)
table(p1$ECOREG_use)


##########
## Subset by response ##
##########

# E:I
p3<-subset(p1,Indicator=="EI")
E_I<-droplevels(p3)
table(E_I$Indicator)

# Untransformed Horizontal DD
p5<-subset(p1,Indicator=="horiz")
Horiz<-droplevels(p5)
table(Horiz$Indicator)

# Untransformed Vertical DD
p6<-subset(p1,Indicator=="vert")
Vert<-droplevels(p6)
table(Vert$Indicator)

# WRT
p7<-subset(p1,Indicator=="WRT")
WRT<-droplevels(p7)
table(WRT$Indicator)

# SCALED Horizontal DD
p8<-subset(p1,Indicator=="horiz_sc")
Horiz_sc<-droplevels(p8)
table(Horiz_sc$Indicator)

# SCALED VERTICAL DD
p9<-subset(p1,Indicator=="vert_sc")
Vert_sc<-droplevels(p9)
table(Vert_sc$Indicator)

# SCALED VERTICAL DD MODIFIED
p10<-subset(p1,Indicator=="vert_sc_mod")
Vert_sc_mod<-droplevels(p10)
table(Vert_sc_mod$Indicator)


##################
## DOT PLOTS of CHANGE between 2012 and 2007 with standard error bars 
# Reorder variables
# Ecoregion groups so that they are plotted from West to East to match the map
##################
## ggplot
#https://stackoverflow.com/questions/42754661/plot-multiple-points-of-a-single-factor-side-by-side
# https://rcompanion.org/rcompanion/d_08.html
# legend labels https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
# Drop extra legend elements https://stackoverflow.com/questions/11714951/remove-extra-legends-in-ggplot2

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

## Set working director
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/Dotplot")

#########
## Reorder variables
#########
Horiz$ECOREG_use <- ordered(Horiz$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Vert$ECOREG_use <- ordered(Vert$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
E_I$ECOREG_use <- ordered(E_I$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
WRT$ECOREG_use <- ordered(WRT$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Horiz_sc$ECOREG_use <- ordered(Horiz_sc$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Vert_sc$ECOREG_use <- ordered(Vert_sc$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Vert_sc_mod$ECOREG_use <- ordered(Vert_sc_mod$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


##########
## Horizontal DD
##########
horiz <-ggplot(Horiz, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Horizontal drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nHorizontal drawdown (m)")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))
  #scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
   #                    labels=c("Natural", "Man-made"))




#########
## Vertical DD
#########
vert <-ggplot(Vert, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use,shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use,shape=Lake_Origin_use), size=2) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+ #,linetype=I("dashed")
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Vertical drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nVertical drawdown (m)")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))
  #scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
  #                     labels=c("Natural", "Man-made"))

##########
## E_I
##########
EI <-ggplot(E_I, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use,shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)), aes(colour=Lake_Origin_use,shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Evaporation:Inflow 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#legend.position = "right", #
  #legend.title=element_blank(), #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nE:I")+ #legend.text=element_text(family="RMN"))+
  xlab(NULL)+
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("MAN_MADE","NATURAL"))+
  scale_shape_manual(values=c(16,15))
  #scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
  #                     labels=c("Natural", "Man-made"))

#########
## WRT
#########
Wrt <-ggplot(WRT, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use,shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use,shape=Lake_Origin_use), size=2) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+ #,linetype=I("dashed")
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #scale_y_continuous(limits=c(-3,5))+ #, breaks=c(0,0.2,0.4,0.6,0.8,1.0,1.2)) +
  #ggtitle("Change in Vertical drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position = "none")+ #legend.position = "right",
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nWater residence time (yr)")+
  xlab(NULL)+ #
  scale_color_manual(values = c("#d95f02","#1f78b4"),labels=c("MAN_MADE","NATURAL"))+ # Can change labels to show up on legend
  scale_shape_manual(values=c(16,15))
  #scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
  #                     labels=c("NATURAL", "MAN_MADE"))# Not changing anything

##########
## SCALED Horizontal DD
##########
horiz_sc <-ggplot(Horiz_sc, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use,shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use,shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Horizontal drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \n Scaled horizontal drawdown")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))
  # scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
#                       labels=c("Natural", "Man-made"))

#########
## SCALED Vertical DD MODIFIED
#########
vert_sc_mod <-ggplot(Vert_sc_mod, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use,shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use,shape=Lake_Origin_use), size=2) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+ #,linetype=I("dashed")
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Vertical drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nScaled vertical drawdown")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))
  # scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
#                       labels=c("Natural", "Man-made"))


#####################
## PLOT change in multiplot 
#####################
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
#######################################
# MULTIPLE PLOT PANEL
# Two variables
# 2/9/18 - dropped Pyramid Lake
# 3/8/18 - scaled drawdown
# 5/14/18
# 6/8/18 - plot for ALSO 2018
# 8/6/18 - updated E:I and RT estimates
########################
# HORIZ & VERT
tiff(filename="a_Change_Inferred_ECOREGION_horiz_vert_14MAY18.tiff",width=6, height=4, units="in", res=600) # 09FEB18
multiplot(horiz,vert, cols=2)
dev.off()

# E:I & WRT
tiff(filename="a_Change_Inferred_ECOREGION_EI_WRT_07AUG18.tiff",width=6.2, height=4, units="in", res=600) # 14MAY18
multiplot(EI,Wrt,cols=2)
dev.off()

# SCALED HORIZ & VERT
tiff(filename="a_Change_Inferred_ECOREGION_SCALED_horiz_vert_14MAY18.tiff",width=6, height=4, units="in", res=600)
multiplot(horiz_sc,vert_sc_mod, cols=2)
dev.off()

# JUST VERT FOR ASLO PRESENTATION
tiff(filename="a_Change_Inferred_ECOREGION_vert_08JUN18.tiff",width=4, height=4, units="in", res=600) # 09FEB18
multiplot(vert, cols=1)
dev.off()

# JUST E:I FOR ASLO PRESENTATION
tiff(filename="a_Change_Inferred_ECOREGION_EI_08JUN18.tiff",width=4, height=4, units="in", res=600)
multiplot(EI,cols=1)
dev.off()

# JUST SCALED VERT FOR ASLO PRESENTATION
tiff(filename="a_Change_Inferred_ECOREGION_vert_SCALED_08JUN18.tiff",width=4, height=4, units="in", res=600) # 09FEB18
multiplot(vert_sc_mod, cols=1)
dev.off()



#########
# DROPPING OUTLIER
# 1/26/18
# HORIZ & VERT
tiff(filename="a_Change_Inferred_ECOREGION_horiz_vert_DROP_26JAN18.tiff",width=6, height=4, units="in", res=600)
multiplot(horiz,vert, cols=2)
dev.off()

# E:I & WRT
tiff(filename="a_Change_Inferred_ECOREGION_EI_WRT_DROP_26JAN18.tiff",width=6.2, height=4, units="in", res=600)
multiplot(EI,Wrt,cols=2)
dev.off()


# TO GET LEGEND
EI_legend <-ggplot(E_I, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)), aes(colour=Lake_Origin_use,shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Evaporation:Inflow 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in E:I")+
  xlab("Ecoregion")+
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural"))
  #scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
  #                     labels=c("Natural", "Man-made"))

tiff(filename="a_Change_inferred_legend_09JAN18.tiff", width=3, height=4, units="in", res=600)
print(EI_legend)
dev.off()



#####################
# CONDITION state
# Load population inferred condition
# 6/4/18 - update with complete NLA datasets
# 7/18/18 - added place holder for UMW manmade large; NAP natural Medium and large
p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_ECOREG_CONDITION_Estimates_14MAY18.csv") #08MAR18
names(p)
table(p$Subpopulation)
table(p$Type)


# Drop NAs
p<-p[which(!is.na(p$DiffEst.P)),]
# Drop Not Assessed
p<-p[which(!p$Category=="NOT ASSESSED"),]

# Subset ECOREGION/lake type class
DD_ECOREG_LO<-subset(p,Type=="WSA9_Ecoregions_LO")
table(DD_ECOREG_LO$Type)
DD_ECOREG_LO<-droplevels(DD_ECOREG_LO)

# Need to modify Subpopulation to pull out grouping factors
DD_ECOREG_LO$ECOREG_use<-substr(DD_ECOREG_LO$Subpopulation,1,3)

DD_ECOREG_LO$Lake_Origin_use<-substr(DD_ECOREG_LO$Subpopulation,5,13)

table(DD_ECOREG_LO$Lake_Origin_use)
table(DD_ECOREG_LO$ECOREG_use)
table(DD_ECOREG_LO$Category)

str(DD_ECOREG_LO)
DD_ECOREG_LO$Lake_Origin_use<-factor(DD_ECOREG_LO$Lake_Origin_use, labels=c("MAN_MADE","NATURAL"))

# SUBSET LAKE_ORIGIN
DD_LO<-subset(p,Type=="Lake_Origin")
table(DD_LO$Type)
DD_LO<-droplevels(DD_LO)

#############
## For dotplots
DD_LO$Category <- ordered(DD_LO$Category, levels=c("SMALL","MEDIUM","LARGE"))

names(DD_LO)
##########
## DRAWDOWN CONDITION
##########

## LAKE TYPE ONLY ##
z <- ggplot(DD_LO, aes(color=Category, y=DiffEst.P, x=Subpopulation))
DDown_lk_type <- z + geom_point(position=position_dodge(width=0.9),stat="identity", aes(colour=Category), size=2) +
  geom_hline(yintercept=0, size=I(0.2), color=I("black"))+
  geom_errorbar(aes(ymin=DiffEst.P-StdError.P, ymax=DiffEst.P+StdError.P), position=position_dodge(width=0.9), width=0.8)+
  #scale_y_continuous(limits=c(NA,100)) +
  scale_color_manual(values = c("#8da0cb","#66c2a5","#fc8d62"),labels=c("SMALL","MEDIUM","LARGE"))+
  scale_shape_discrete(breaks=c("SMALL","MEDIUM","LARGE"),
                       labels=c("SMALL","MEDIUM","LARGE"))+
  #scale_fill_manual(values = c("#8da0cb","#66c2a5","#fc8d62"))+
  theme_bw (base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#,angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        strip.text.x=element_text(family="RMN"), # To change facet ecoregion labels
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="bottom", #c(0.85,0.85)
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in\n% lakes")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+


##########
# ECOREGION + LAKE TYPE
##########
# Drawdown condition in comparison to reference values in region
# Drop NAs
DD_ECOREG_LO<-DD_ECOREG_LO[which(!is.na(DD_ECOREG_LO$Category)),]

# Change Labels for Lake_Origin_use
levels(DD_ECOREG_LO$Lake_Origin_use) <- list("Man-made"=c("MAN_MADE"), "Natural"=c("NATURAL"))
table(DD_ECOREG_LO$Lake_Origin_use)

# Order Small, Medium, Large
DD_ECOREG_LO$ECOREG_use <- ordered(DD_ECOREG_LO$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
DD_ECOREG_LO$Category <- ordered(DD_ECOREG_LO$Category, levels=c("SMALL","MEDIUM","LARGE"))


# Add fake data to keep spaces in graph where missing factors
myvars<- c("Category","DiffEst.P", "StdError.P","ECOREG_use","Lake_Origin_use")
data<-DD_ECOREG_LO[myvars]

data2<-rbind(data, data.frame(Category="LARGE", DiffEst.P=110, StdError.P=110,
                              ECOREG_use="UMW",Lake_Origin_use="Man-made"))
data3<-rbind(data2, data.frame(Category="MEDIUM", DiffEst.P=110, StdError.P=110,
                               ECOREG_use="NAP",Lake_Origin_use="Natural"))
data4<-rbind(data3, data.frame(Category="LARGE", DiffEst.P=110, StdError.P=110,
                               ECOREG_use="NAP",Lake_Origin_use="Natural"))

z <- ggplot(data4, aes(color=Category, y=DiffEst.P, x=Lake_Origin_use))

DDown_ECOREG_LO <- z + geom_point(position=position_dodge(width=0.9),stat="identity",aes(colour=Category), size=2) +
  facet_wrap(~ECOREG_use)+
  geom_hline(yintercept=0, sizeI(0.2), color=I("black"))+
  geom_errorbar(aes(ymin=DiffEst.P-StdError.P, ymax=DiffEst.P+StdError.P), position=position_dodge(width=0.9), width=0.8)+
  #ylim(-100,100)+
  #scale_y_continuous(limits=c(NA,100)) +
  scale_y_continuous(limits=c(NA,100), breaks=c(-50,0,50))+
  scale_color_manual(values = c("#8da0cb","#66c2a5","#fc8d62"),
                     labels=c("SMALL","MEDIUM","LARGE"))+
  scale_shape_discrete(breaks=c("SMALL","MEDIUM","LARGE"),
                       labels=c("SMALL","MEDIUM","LARGE"))+
  theme_bw (base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN",angle=45, hjust=1, size=12),#,angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        strip.text.x=element_text(family="RMN"), # To change facet ecoregion labels
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in\n%lakes")+ #
  xlab(NULL)#+

#######################################
# MULTIPLE PLOT PANEL
########################
# Drawdown condition
# LAKE TYPE ONLY
tiff(filename="a_Change_DD_CONDITION_LO_14MAY18.tiff",width=3, height=2.25, units="in", res=600) # 20MAR18
multiplot(DDown_lk_type, cols=1)
dev.off()

# ECOREG + LAKE TYPE
tiff(filename="a_Change_DD_CONDITION_ECOREG_LO_18JUL18.tiff",width=6.5, height=7, units="in", res=600) # old 14MAY18
multiplot(DDown_ECOREG_LO, cols=1)
dev.off()

