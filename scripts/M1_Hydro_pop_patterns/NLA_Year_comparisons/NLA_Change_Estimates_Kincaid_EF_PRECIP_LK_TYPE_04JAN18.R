# File: Change_Estimates.R
# Purpose: Calculate change estimates for the NLA 2007 and NLA 2012 surveys
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

# 1/4/18 - Precipitation aand Lake type class inferred pop estimates
 # - NOT working - gett an error message "Components in the list passed to the missing value function must be the same length.
# Load the spsurvey library
rm(list=ls())
library(spsurvey)

####################
## Bring variables in NLA2012_ChangeData_2016-08-23.orig.csv 
####################
change<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/NLA2012_ChangeData_2016-08-23.orig.csv")

myvars_ch<- c("SITE_ID","CH0712_USE","STATUS","CH0712_WGT","REVSAMP_0712",
           "LITCVR_COND","LITRIPCVR_COND","RDIS_COND","DRAWDOWN_COND","NTL_COND")

change_red<- change[myvars_ch]


####
# Merge with long formated dataset
####
# LOAD long-format 2007 & 2012 data
# 12/13/17 - updated to include WRT and WYield - single observations n=1876 
nla07_12_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_13DEC17.csv")

# Merging using inner_join function - need to have single observations to join
#  May have trouble merging because there are repeat observations within years
#   Get an error message about joining factors with different levels - coercing to character vector
test<- dplyr::inner_join(nla07_12_all,change_red, by="SITE_ID")

write.csv(test,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_CH_WGTS.csv")
# Match function to merge data - needs to have same number of observations
#index = match(nla07_12_all$SID, nla07_12$SID, nomatch=0)
#nla07_12_all$CH0712_USE<-nla07_12$CH0712_USE[index]
#test<- merge(nla07_12_all, nla07_12, by="SID") _ this made duplicates


# Create a text file for output 
#sink("Change_Estimates_20160823.txt")
#cat("Change Estimation for the NLA 2007 and NLA 2012 Surveys\n\n\n")

################
# Print the date and session information
#cat(date(), "\n\n")
#cat("Session Information:\n\n")
#sessionInfo()

#############
# Read the file containing NLA 2007 and NLA 2012 data and retain sites of
# interest
#############
# 10/31/17 - Long-format 2007 & 2012 reduced NLA datasets
#   Subset data to keep lakes that are labeled as "Include" and part of the target sample population
#changestatus <- read.csv("L:/Priv/ARM Data/NLA 2012/NLA 2012 Change Estimation/NLA2012_ChangeData_2016-08-23.orig.csv", row.names = 1)
#read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE.csv", row.names=1)
changestatus <- test
changestatus <- droplevels(subset(changestatus, CH0712_USE == "Include" &
   STATUS == "Target_Sampled"))
nr <- nrow(changestatus)

# Create a data frame that contains only repeat visit sites
#   BUT needs to be modified b/c inner_join may have messed up order
repeatdf_a <- subset(changestatus, REVSAMP_0712 == "Y")

#########
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
#########
changestatus$Precip_class_man<-cut(changestatus$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("low","med_low", "med_hi","high"))#labels=c("< 500","500-900","900-1200",">1200"))

table(changestatus$Precip_class_man) # Relatively balanced

## Create Precipitation + Lake Origin variable
changestatus$PRECIP_LO <- with (changestatus, interaction(Precip_class_man,Lake_Origin_use,sep="_"))
table(changestatus$PRECIP_LO)

# Recode levels of the Precip class / Lake Origin (PRECIP_LO) variable
levels(changestatus$PRECIP_LO) <- list(
   "low_MAN_MADE" = c("low_MAN-MADE", "low_MAN_MADE"),
   "low_NATURAL" = "low_NATURAL",
   "med_low_MAN_MADE" = c("med_low_MAN-MADE", "med_low_MAN_MADE"),
   "med_low_NATURAL" = "med_low_NATURAL",
   "med_hi_MAN_MADE" = c("med_hi_MAN-MADE", "med_hi_MAN_MADE"),
   "med_hi_NATURAL" = "med_hi_NATURAL",
   "high_MAN_MADE" = c("high_MAN-MADE", "high_MAN_MADE"),
   "high_NATURAL" = "high_NATURAL")

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
repeatdf_a<-droplevels(repeatdf_a)
table(repeatdf_a$YEAR)
# 2007 2012 
# 304  344 

# List of lakes in 2007
nla07_lk <-subset(repeatdf_a, YEAR=="2007", select=c(SID))
# List of lakes that are in 2012 but not in 2007
nla12_lk_todrop <-repeatdf_a[!(repeatdf_a$SID %in% nla07_lk$SID),] #48 observations
#sid.drop<-nla12_lk_todrop$SID
#sid.drop<-droplevels(sid.drop)

#List of lakes in 2012 
nla12_lk <-subset(repeatdf_a, YEAR=="2012", select=c(SID))
# list of lakes that are in 2007 but not 2012
nla07_lk_todrop <-repeatdf_a[!(repeatdf_a$SID %in% nla12_lk$SID),]#8 

# REDUCED DATASETS
# 1) Drop NLA_12 observations that aren't sampled in 2007
test <-repeatdf_a[!( repeatdf_a$SID%in%nla12_lk_todrop$SID),]
# 2) Drop NLA_07 observations that aren't sampled in 2012
repeatdf <- test[!(test$SID%in%nla07_lk_todrop$SID),]

# Rewrite dataframe with equal observations per year
#repeatdf<-repeatdf[!(repeatdf$SID %in% c("NLA06608-0916","NLA06608-1733","NLA06608-0591",
#                                     "NLA06608-1608","NLA06608-0359","NLA06608-2332","NLA06608-0271","NLA06608-0120","NLA06608-0332","NLA06608-0243",
#                                     "NLA06608-0727","NLA06608-0560","NLA06608-1560","NLA06608-0529","NLA06608-0593",
#                                     "NLA06608-2257","NLA06608-1354","NLA06608-1529","NLA06608-1989")),]
#test<-repeatdf[!(repeatdf$SID %in% c(sid.drop)),]
#repeatdf<- subset(repeatdf,!(SID %in% c(sid.drop)))
table(repeatdf$YEAR) # 592 - 296 per year
#repeatdf<-droplevels(repeatdf) 

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=changestatus$SITE_ID,
                    survey1=changestatus$YEAR == 2007,
                    survey2=changestatus$YEAR == 2012)

# Create a data frame containing data for repeat visit sites
# Tom added 11/6/17 to get numbers to match up
repeatdf_07 <- subset(changestatus, REVSAMP_0712 == "Y" & YEAR == 2007)
repeatdf_12 <- subset(changestatus, REVSAMP_0712 == "Y" & YEAR == 2012)
repeatdf_07 <- subset(repeatdf_07, SID %in% repeatdf_12$SID)
repeatdf_12 <- subset(repeatdf_12, SID %in% repeatdf_07$SID)
indx <- match(repeatdf_07$SID, repeatdf_12$SID) # match basedon SID and make 2012 have same order as 2007
repeatdf_12 <- repeatdf_12[indx,]
repeatdf <- droplevels(rbind(repeatdf_07, repeatdf_12))

# Create the repeats data frame, which identifies repeat visit sites to use in
# the analysis - creates dataframe of repeat obs by SITE_ID and Year (which was matched by SID)
repeats <- data.frame(
   siteID_1=repeatdf$SITE_ID[repeatdf$YEAR == 2007],
   siteID_2=repeatdf$SITE_ID[repeatdf$YEAR == 2012])


# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=changestatus$SITE_ID,
                     National=rep("National", nr),
                     Lake_Origin=changestatus$Lake_Origin_use,
                     Precip_lktype_LO=changestatus$PRECIP_LO) # WSA3_Ecoregions=changestatus$AGGR_ECO3_2015,EPA_REG=changestatus$EPA_REG,SIZE_CLASS=changestatus$SIZE_CLASS)
             
# Create the design data frame, which identifies the stratum code, weight,
# x-coordinate, and y-coordinate for each site ID
design <- data.frame(siteID=changestatus$SITE_ID,
                     wgt=changestatus$CH0712_WGT, #CH0712_WGT
                     xcoord=changestatus$XCOORD,
                     ycoord=changestatus$YCOORD)

# Create the data.cat data frame, which specifies categorical variables to use
# in the analysis
data.cat <- data.frame(siteID=changestatus$SITE_ID,
                       Lake_Drawdown_Exposure=changestatus$Drawdown_CONDus15)#,
                       #Total_Nitrogen=changestatus$NTL_COND,
                       #Shallow_Water_Habitat=changestatus$LITCVR_COND,
                       #Lake_Habitat_Complexity=changestatus$LITRIPCVR_COND,
                       #Lakeshore_Disturbance=changestatus$RDIS_COND)


data.cont <- data.frame(siteID=changestatus$SITE_ID,
                        vert=changestatus$VertDD_use,
                        horiz=changestatus$HorizDD_use,
                        #vert_log=changestatus$L_VertDD_use,
                        #horiz_log=changestatus$L_HorizDD_use,
                        EI=changestatus$E_I,
                        WRT=changestatus$RT_iso)

# Calculate the estimates
cat("\n\nCalculate change estimates\n")
sink()
if(exists("warn.df")) rm("warn.df")
Change_Estimates <- change.analysis(sites, repeats, subpop, design, data.cat,data.cont)

# Check for warning messages and print them if any exist
sink("Change_Estimates_20160823.txt", append=TRUE)
if(exists("warn.df")) {
   cat("\nWarning messages generated during the call to change.analysis:\n")
   warnprnt()
} else {
   cat("\nNo warning messages were generated during the call to change.analysis.\n")
}

# Print results
cat("\nChange Etimates:\n\n")
Change_Estimates


#write.csv(Change_Estimates$catsum, file="NLA0712_Change_Estimates_20160823.csv",
#   row.names=FALSE)
###################
# WRITE OUTPUT
#   Write results as comma-separated value (csv) files
###################
## NOTES on Output - .p = percent change; .u = count of lakes in inferred population
# First estimate is earlier time step, Second estimate is later time step
#  Difference is later time step minus earlier time step 
#    so that (0) indicates no change, (+) indicates an increase, and (-) indicates a decrease


# catsum are condition category output
write.csv(Change_Estimates$catsum, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_CONDITION_Estimates_13DEC17.csv")

# continuous variable - mean change estimates
write.csv(Change_Estimates$contsum_mean, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_CONTINUOUS_Estimates_13DEC17.csv")
# Close the output text file
#sink()

######################
## Look at Differences by SUBSETTING DATA 
#####################
## Subset data "Type" = "WSA9_by_Lake_Origin"

# 12/13/17

library(reshape2)
library(dplyr)
library(ggplot2)

library(Hmisc)
library(plotrix)
library(pgirmess)

library(gridExtra)

p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_CONTINUOUS_Estimates_13DEC17.csv")
names(p)
table(p$Subpopulation)

#
p1<-subset(p,Type=="WSA9_Ecoregions_LO")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3)

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13)

table(p1$Lake_Origin_use)
table(p1$ECOREG_use)

names(p1)
head(p1)

##########
## Subset by response ##
##########
# L_Horizontal DD
p2<-subset(p1,Indicator=="horiz_log")
L_Horiz<-droplevels(p2)
table(L_Horiz$Indicator)

# E:I
p3<-subset(p1,Indicator=="E_I")
E_I<-droplevels(p3)
table(E_I$Indicator)

# L_Vert DD
p4<-subset(p1,Indicator=="vert_log")
L_Vert<-droplevels(p4)
table(L_Vert$Indicator)

# Untransformed Horizontal DD
p5<-subset(p1,Indicator=="horiz")
Horiz<-droplevels(p5)
table(Horiz$Indicator)

# Untransformed Horizontal DD
p6<-subset(p1,Indicator=="vert")
Vert<-droplevels(p6)
table(Vert$Indicator)

# WRT
p7<-subset(p1,Indicator=="WRT")
WRT<-droplevels(p7)
table(WRT$Indicator)

# WYIELD
p8<-subset(p1,Indicator=="WYIELD")
WYIELD<-droplevels(p8)
table(WYIELD$Indicator)

##################
## DOT PLOTS of CHANGE between 2012 and 2007 with standard error bars 
# Reorder variables
# Ecoregion groups so that they are plotted from West to East to match the map
##################

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman


## Log Horizontal DD
L_Horiz$ECOREG_use <- ordered(L_Horiz$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(L_Horiz$ECOREG_use)
head(L_Horiz)

#plot(L_Horiz$DiffEst~L_Horiz$ECOREG_use)

## ggplot
#https://stackoverflow.com/questions/42754661/plot-multiple-points-of-a-single-factor-side-by-side
# https://rcompanion.org/rcompanion/d_08.html
# legend labels https://stackoverflow.com/questions/23635662/editing-legend-text-labels-in-ggplot
# Drop extra legend elements https://stackoverflow.com/questions/11714951/remove-extra-legends-in-ggplot2
L_horiz <-ggplot(L_Horiz, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5))) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("red"))+
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  ggtitle("Change in Horizontal drawdown 2012-2007")+
  theme_bw(base_size=14)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in Horizontal drawdown (log10)")+
  xlab("Ecoregion")+
  scale_color_manual(values = c("#d95f02","#1f78b4"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

## Set working director
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput")

# Horizontal (log10) DD NLA12
# Using multiplot
#a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="Change_L_horiz_eco_lktype.png", width=6.5, height=5,units="in", res = 600)
print(L_horiz)
dev.off()


##########
## E_I
##########
E_I$ECOREG_use <- ordered(E_I$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(E_I$ECOREG_use)
head(E_I)

EI <-ggplot(E_I, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)), aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2)
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
        legend.position = "right", #legend.position="none")+
        legend.title=element_blank(), #legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in \nE:I")+ #legend.text=element_text(family="RMN"))+
  xlab(NULL)+
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("MAN_MADE","NATURAL"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))
        

#a<-viewport(width=unit(6, "inch"), height=unit(4,"inch"), x=0.5)
png(filename="Change_EI_eco_lktype.png", width=6.5, height=5,units="in", res = 600)
print(EI)
dev.off()

########
## Log Vertical DD
########
L_Vert$ECOREG_use <- ordered(L_Vert$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
#L_Vert$Lake_Origin_use <- ordered(L_Vert$Lake_Origin_use, levels=c("NATURAL","MAN_MADE"))
table(L_Vert$ECOREG_use)
head(L_Vert)

L_vert <-ggplot(L_Vert, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5))) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("red"))+
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  ggtitle("Change in Vertical drawdown 2012-2007")+
  theme_bw(base_size=14)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in Vertical drawdown (log10)")+
  xlab("Ecoregion")+
  scale_color_manual(values = c("#d95f02","#1f78b4"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

png(filename="Change_L_Vert_eco_lktype.png", width=6.5, height=5,units="in", res = 600)
print(L_vert)
dev.off()

##########
## Horizontal DD
##########
Horiz$ECOREG_use <- ordered(Horiz$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(Horiz$ECOREG_use)
head(Horiz)

horiz <-ggplot(Horiz, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
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
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

png(filename="Change_Horiz_untrans_eco_lktype.png", width=6.5, height=5,units="in", res = 600)
print(horiz)
dev.off()

#########
## Vertical DD
#########
Vert$ECOREG_use <- ordered(Vert$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(Vert$ECOREG_use)
head(Vert)

vert <-ggplot(Vert, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use), size=2) + # to change size ,aes(size=2)
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
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

png(filename="Change_Vert_untrans_eco_lktype.png", width=6.5, height=5,units="in", res = 600)
print(vert)
dev.off()

#########
## WRT
#########
WRT$ECOREG_use <- ordered(WRT$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(WRT$ECOREG_use)


Wrt <-ggplot(WRT, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use), size=2) + # to change size ,aes(size=2)
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
        legend.position = "right",
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in \nWater residence time (yr)")+
  xlab("Ecoregion")+ #
  scale_color_manual(values = c("#d95f02","#1f78b4"),labels=c("MAN_MADE","NATURAL"))+ # Can change labels to show up on legend
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("NATURAL", "MAN_MADE"))# Not changing anything


#########
## Watershed Yield
#########
WYIELD$ECOREG_use <- ordered(WYIELD$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(WYIELD$ECOREG_use)


Wyield <-ggplot(WYIELD, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use), size=2) + # to change size ,aes(size=2)
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+ #,linetype=I("dashed")
  geom_errorbar(aes(ymin=DiffEst-StdError, ymax=DiffEst+StdError), width = .1,
                position=position_dodge((width=0.5)))+
  #scale_y_continuous(limits=c(-5,7))+
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
  ylab("Change in \nWatershed yield (m)")+
  xlab("Ecoregion")+ #
  scale_color_manual(values = c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

#####################
## PLOT change in multiplot - 11/29/17
#####################
# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput")

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
# 

# Two variables
tiff(filename="a_Change_Inferred_horz_vert_18DEC17.tiff",width=8, height=4, units="in", res=600)
multiplot(horiz,vert, cols=2)
dev.off()
# Plus E:I
tiff(filename="a_Change_Inferred_EI_18DEC17.tiff",width=5.4, height=4, units="in", res=600)
multiplot(EI, cols=2)
dev.off()


tiff(filename="a_Change_Inferred_horz_vert_EI_13DEC17.tiff",width=8, height=10, units="in", res=600)
multiplot(horiz,EI,Wrt,vert,Wyield, cols=2)
dev.off()

# Split into 3 groups
tiff(filename="a_Change_Inferred_horz_vert_A_13DEC17.tiff",width=8, height=3, units="in", res=600)
multiplot(horiz,vert, cols=2)
dev.off()

tiff(filename="a_Change_Inferred_EI_WYIELD_B_13DEC17.tiff",width=8, height=3, units="in", res=600)
multiplot(EI,Wyield, cols=2)
dev.off()

tiff(filename="a_Change_Inferred_WRT_C_13DEC17.tiff",width=5.3, height=3, units="in", res=600)
multiplot(Wrt, cols=1)
dev.off()

# TO GET LEGEND
EI_legend <-ggplot(E_I, aes(x=ECOREG_use, y = DiffEst, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)), aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2)
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
        legend.position = "right",
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in E:I")+
  xlab("Ecoregion")+
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("MAN_MADE","NATURAL"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

tiff(filename="a_Change_inferred_legend_29NOV17.tiff", width=4, height=3.5, units="in", res=600)
print(EI_legend)
dev.off()