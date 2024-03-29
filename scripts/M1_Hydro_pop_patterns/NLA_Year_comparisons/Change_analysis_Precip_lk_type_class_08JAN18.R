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
# EF: December 13, 2017 - included WRT and Watershed yield response variables
# EF: Janurary 4, 2018 - Precipitation aand Lake type class inferred pop estimates
# 1/9/18 - Tom Kincaid replied why code does not work with precipitation/lake type classes
#         It is because individual lakes can change classes between years and so there is an unbalanced dataset
#       A potential way around it is by setting repeat visit sites to Null in the change analysis code 

rm(list=ls())

# Load the spsurvey library
library(spsurvey)

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
# 12/13/17 - updated to include WRT and WYield - single observations n=1876 
nla07_12_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_13DEC17.csv")
names(nla07_12_all)
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","HorizDD_use","VertDD_use","E_I","RT_iso",
           "XCOORD","YCOORD","Lake_Origin_use","WGT_ALL","Precip_mm_total_yr",
           "Drawdown_CONDus15")
nla07_12_red <- nla07_12_all[myvars]

summary(nla07_12_red)

# Drop NAs
nla07_12_red<-nla07_12_red[which(!is.na(nla07_12_red$RT_iso)),] #23 observations missing water residence time


########
# Merge data
########
test<- merge(nla07_12_red, change_red, by="SITE_ID") 
names(test)

table(test$CH0712_USE)
table(test$REVSAMP_0712)


#############
# Retain sites of interest
##############
changestatus <- test
changestatus <- droplevels(subset(changestatus, CH0712_USE == "Include" &
                                    STATUS == "Target_Sampled")) # drops 85 (Exclude)
nr <- nrow(changestatus)

# Create a data frame that contains only repeat visit sites
#   BUT needs to be modified b/c inner_join may have messed up order
repeatdf_a <- subset(changestatus, REVSAMP_0712 == "Y")


#########
# PRECIPITATION CLASS ## - quartile slightly adjusted to whole values
#########
changestatus$Precip_class_man<-cut(changestatus$Precip_mm_total_yr, breaks=c(28,500,900,1200,Inf),labels=c("< 500","500-900","900-1200",">1200"))# labels=c("low","med_low", "med_hi","high")

table(changestatus$Precip_class_man) # Relatively balanced

## Create Precipitation + Lake Origin variable
changestatus$PRECIP_LO <- with (changestatus, interaction(Precip_class_man,Lake_Origin_use,sep="_"))
table(changestatus$PRECIP_LO)


##########
# Recode levels of the condition class variables
##########
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

# Create a data frame containing data for repeat visit sites
# Tom added 11/6/17 to get numbers to match up
repeatdf_07 <- subset(changestatus, REVSAMP_0712 == "Y" & YEAR == 2007)
repeatdf_12 <- subset(changestatus, REVSAMP_0712 == "Y" & YEAR == 2012)
repeatdf_07 <- subset(repeatdf_07, SID %in% repeatdf_12$SID)
repeatdf_12 <- subset(repeatdf_12, SID %in% repeatdf_07$SID)
indx <- match(repeatdf_07$SID, repeatdf_12$SID) # match basedon SID and make 2012 have same order as 2007
repeatdf_12 <- repeatdf_12[indx,]
repeatdf <- droplevels(rbind(repeatdf_07, repeatdf_12)) # 296 lakes sampled both years

#########
# Look to see if data are balanced between years for precip/lake type classes
# 1/9/18
#############
with(repeatdf, addmargins(table(Precip_class_man, YEAR, useNA="ifany")))
#                 YEAR
#Precip_class_man 2007 2012 Sum
#low       57   68 125
#med_low   89   82 171
#med_hi    90   61 151
#high      56   81 137
#Sum      292  292 584

with(repeatdf, addmargins(table(Lake_Origin_use, YEAR, useNA="ifany")))


###########################
## Change analysis
##
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
                       Lake_Drawdown_Exposure=changestatus$Drawdown_CONDus15)#


data.cont <- data.frame(siteID=changestatus$SITE_ID,
                        vert=changestatus$VertDD_use,
                        horiz=changestatus$HorizDD_use,
                        EI=changestatus$E_I,
                        WRT=changestatus$RT_iso)

# For precip/laketype class - set repeats=NULL
Change_Estimates <- change.analysis(sites, repeats=NULL, subpop, design, data.cat,data.cont)


###################
# WRITE OUTPUT
#   Write results as comma-separated value (csv) files
###################
## NOTES on Output - .p = percent change; .u = count of lakes in inferred population
# First estimate is earlier time step, Second estimate is later time step
#  Difference is later time step minus earlier time step 
#    so that (0) indicates no change, (+) indicates an increase, and (-) indicates a decrease


# catsum are condition category output
write.csv(Change_Estimates$catsum, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_CONDITION_Estimates_09JAN18.csv")

# continuous variable - mean change estimates
write.csv(Change_Estimates$contsum_mean, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_CONTINUOUS_Estimates_09JAN18.csv")

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

p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/NLA0712_Change_CONTINUOUS_Estimates_09JAN18.csv")
names(p)
table(p$Subpopulation)
table(p$Type)

# Subset Precipitation/lake type class
p1<-subset(p,Type=="Precip_lktype_LO")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
# Relabel classes
p1$Subpopulation <- factor(p1$Subpopulation, labels=c("   < 500_MAN-MADE", 
                                                      "   < 500_NATURAL",
                                                      "   >1200_MAN-MADE",
                                                      "   >1200_NATURAL",
                                                      " 500-900_MAN-MADE",
                                                      " 500-900_NATURAL",
                                                      "900-1200_MAN-MADE",
                                                      "900-1200_NATURAL"))
p1$PRECIP<-substr(p1$Subpopulation,1,8)

p1$Lake_Origin_use<-substr(p1$Subpopulation,10,17)
table(p1$PRECIP)
table(p1$Lake_Origin_use)

names(p1)
head(p1)
table(p1$Indicator)

##########
## Subset by response ##
##########
# L_Horizontal DD
#p2<-subset(p1,Indicator=="horiz_log")
#L_Horiz<-droplevels(p2)
#table(L_Horiz$Indicator)

# E:I
p3<-subset(p1,Indicator=="EI")
E_I<-droplevels(p3)
table(E_I$Indicator)

# L_Vert DD
#p4<-subset(p1,Indicator=="vert_log")
#L_Vert<-droplevels(p4)
#table(L_Vert$Indicator)

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
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput")

#########
## Reorder variables
# Precipitation class groups so that they are lowest to highest
#########

## E_I
E_I$PRECIP <- ordered(E_I$PRECIP, levels=c("   < 500"," 500-900","900-1200","   >1200"))
table(E_I$PRECIP)
head(E_I)

## Horiz
Horiz$PRECIP <- ordered(Horiz$PRECIP, levels=c("   < 500"," 500-900","900-1200","   >1200"))
table(Horiz$PRECIP)
head(Horiz)

## Vert
Vert$PRECIP <- ordered(Vert$PRECIP, levels=c("   < 500"," 500-900","900-1200","   >1200"))
table(Vert$PRECIP)
head(Vert)

## WRT
WRT$PRECIP <- ordered(WRT$PRECIP, levels=c("   < 500"," 500-900","900-1200","   >1200"))
table(WRT$PRECIP)
head(WRT)

##########
## Horizontal DD
##########
horiz <-ggplot(Horiz, aes(x=PRECIP, y = DiffEst, color=Lake_Origin_use))+
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

#########
## Vertical DD
#########
vert <-ggplot(Vert, aes(x=PRECIP, y = DiffEst, color=Lake_Origin_use))+
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

##########
## E_I
##########
EI <-ggplot(E_I, aes(x=PRECIP, y = DiffEst, color=Lake_Origin_use))+
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
        legend.position="none")+#legend.position = "right", #
        #legend.title=element_blank(), #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab("Change in \nE:I")+ #legend.text=element_text(family="RMN"))+
  xlab("Precipitation (mm)")+
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("MAN_MADE","NATURAL"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

#########
## WRT
#########
Wrt <-ggplot(WRT, aes(x=PRECIP, y = DiffEst, color=Lake_Origin_use))+
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
        legend.position = "none")+ #legend.position = "right",
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab("Change in \nWater residence time (yr)")+
  xlab("Precipitation (mm)")+ #
  scale_color_manual(values = c("#d95f02","#1f78b4"),labels=c("MAN_MADE","NATURAL"))+ # Can change labels to show up on legend
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("NATURAL", "MAN_MADE"))# Not changing anything

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
########################
# HORIZ & VERT
tiff(filename="a_Change_Inferred_PRECIP_horiz_vert_09JAN18.tiff",width=6, height=3, units="in", res=600)
multiplot(horiz,vert, cols=2)
dev.off()

# E:I & WRT
tiff(filename="a_Change_Inferred_PRECIP_EI_WRT_09JAN18.tiff",width=6.2, height=3.25, units="in", res=600)
multiplot(EI,Wrt,cols=2)
dev.off()

# TO GET LEGEND
EI_legend <-ggplot(E_I, aes(x=PRECIP, y = DiffEst, color=Lake_Origin_use))+
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
        legend.position = "bottom",
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Change in E:I")+
  xlab("Ecoregion")+
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("MAN_MADE","NATURAL"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

tiff(filename="a_Change_inferred_legend_09JAN18.tiff", width=3, height=3.5, units="in", res=600)
print(EI_legend)
dev.off()