#####################
## Population inferred values NLA 2007
##  OTHER Lake Characteristics
##  Lake area (km2); Approximate max depth, Elevation
##  Lake connectivity types
##
# Programmer: Tom Kincaid
# Date: October 28, 2014
##
# See for R documentation of spsurvey package
# https://www.rdocumentation.org/packages/spsurvey/versions/3.3
##
## 5/16/18
## 6/27/19 - updated population weights from Tony 6/24/19
## 7/16/19 - updated zmax for 24 lakes
#####################

rm(list=ls())

# Load the spsurvey library
library(tidyverse)
library(spsurvey)
library(dplyr)
library(reshape2)

citation(package="spsurvey")

###########
## LOAD DATASET - SINGLE OBSERVATION dataset
# NLA 2007 with connectivity type added on
nla07<- read_csv("data_processed/nla07/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv")

# Check to make sure right dataset - looks good - lake depth supplemented from lit
y=c("NLA06608-0021","NLA06608-0041","NLA06608-0079","NLA06608-0129",
    "NLA06608-1958","NLA06608-2881")
test<-nla07%>%
  filter(SITE_ID%in%y)%>%
  select(SITE_ID,Lake_Origin_use,DpthMx_mod,RT_iso,Lake_Vol_m3) 

table(nla07$TNT)
#Target 
#1028

## Process data
# Clean up dataset
names(nla07)
todrop <- names(nla07)%in% c("X")
nla07 <- nla07[!todrop]
nr <- nrow(nla07)

# Create new factor variable combining Lake_Origin_use and Class3_f
nla07$hydro_iso_lk <- with (nla07, interaction(lk_hydro_iso,Lake_Origin_use,sep="_"))
table(nla07$lk_hydro_iso)
table(nla07$hydro_iso_lk)

#####################
# Calculate continuous estimates weighted by sample design
####################

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=nla07$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=nla07$SITE_ID,
                     National=rep("All_Lakes",nrow(nla07)),
                     Lake_Origin=nla07$Lake_Origin_use,
                     WSA9_Ecoregions=nla07$ECOWSA9_2015,
                     WSA9_by_Lake_Origin=nla07$WSA9_LO)

subpop <- data.frame(siteID=nla07$SITE_ID,
                     National=rep("All_Lakes",nrow(nla07)),
                     Lake_Origin=nla07$Lake_Origin_use,
                     WSA9_Ecoregions=nla07$ECOWSA9_2015,
                     WSA9_by_Lake_Origin=nla07$WSA9_LO)


# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
design <- data.frame(siteID=nla07$SITE_ID,
                     wgt=nla07$WGT_SP, #WGT_NLA
                     xcoord=nla07$ALBERS_X,
                     ycoord=nla07$ALBERS_Y)


## CONTINUOUS VARIABLES
# Create data.cont data frame, specifies the response variables to use
data.cont <- data.frame(siteID=nla07$SITE_ID,
                       lkarea_km=nla07$LkArea_km2,
                        depth=nla07$DpthMx_mod,
                        SLD=nla07$SLD,
                        elev=nla07$ELEV_use,
                        vol=nla07$Lake_Vol)

## CATEGORICAL VARIABLES
# Create the data.cat data frame, which specifies the variables to use in the
# analysis
data.cat <- data.frame(siteID=nla07$SITE_ID,
                       hydro_iso=nla07$HYDRO_TYPE_f,
                       hydro_iso_lk=nla07$hydro_iso_lk,
                       hydro_conn=nla07$Class3_f,
                       hydro_conn_lk=nla07$lake_type)

# Calculate the estimates
Cont_var_Estimates <- cont.analysis(sites, subpop, design, data.cont) # dropped pop.size


## NOTE Cont_var_Estimates creates a list of dataframes: CDF = cumulative distribution function,
#   Pct = percentiles (percentiles of sample population do not have standard error values),

#######
# WRITE CONTINOUOUS PERCENTILE OUTPUT
# 6/27/19 
# 7/16/19 - updated zmax
write_csv(Cont_var_Estimates$Pct,file="Routput/pop_calculations/NLA07_CONTINUOUS_percentile_LAKE_VARS_27JUN19.csv",
          row.names=FALSE)


#####################
## RUN CATEGORICAL POPULATION ANALYSIS
#####################
#if(exists("warn.df")) rm("warn.df")
Condition_Estimates <- cat.analysis(sites, subpop, design, data.cat)


# Print results
cat("\nTrophic State Etimates:\n\n")
Condition_Estimates

# Write results as a comma-separated value (csv) file
# 6/27/19
write_csv(Condition_Estimates, file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_Condition_Estimates_LAKE_VARS_27JUN19.csv", # OLD 14FEB18 (10JAN18) NLA07_Condition_Estimates_10OCT17.csv
          row.names=FALSE)


#######################
## Cast dataset to make boxplots of lake characteristics by lake type and ecoregion
## 7/16/19 We did not end up making boxplots - but will keep the script if we want to do it later
#######################
####################
## Create datasets from ouptput to make boxplots using percentile values
#    not the estimated values because those are only useful to create cumulative distribution function plot
#    We can use the percentile values to manually create boxplots of the drawdown distribution values
####################
library(reshape2)
library(dplyr)

# CONTINUOUS LAKE VARIABLES
p<-read_csv("Routput/pop_calculations/NLA07_CONTINUOUS_percentile_LAKE_VARS_16MAY18.csv")#
names(p)
table(p$Type)
table(p$Subpopulation)
table(p$Indicator)

##########
# 5/29/18
library(reshape2)
library(dplyr)

# Subset by lake type and ecoregion
p1<-subset(p,Type=="WSA9_by_Lake_Origin")
table(p1$Type)
p1<-droplevels(p1)
# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3) # Takes first three characters in Subpopulation e.g., CPL_MAN-MADE

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13) # takes characters from 5 to 13
table(p1$Lake_Origin_use)
table(p1$ECOREG_use)

# lkarea
p3<-subset(p1,Indicator=="lkarea_km")
p3<-droplevels(p3)
table(p3$Indicator)

# depth
p5<-subset(p1, Indicator=="depth")
p5<-droplevels(p5)
table(p5$Indicator)

# vol
p6<-subset(p1, Indicator=="vol")
p6<-droplevels(p6)
table(p6$Indicator)

# SLD 
p7<-subset(p1, Indicator=="SLD")
p7<-droplevels(p7)
table(p7$Indicator)



### Cast dataframe to have percentile stat have separate columns ###
# ECOREGION_LAKE ORIGIN
# AREA
area <- dcast(p3, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write_csv(area,"Routput/pop_calculations/NLA07_LK_ECOREG_AREA_KM2_percentile_CAST_29MAY18.csv")

# DEPTH
depth <- dcast(p5, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write_csv(depth,"Routput/pop_calculations/NLA07_LK_ECOREG_DEPTH_percentile_CAST_29MAY18.csv")

# Vol
vol <- dcast(p6, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write_csv(vol,"Routput/pop_calculations/NLA07_LK_ECOREG_VOLUME_percentile_CAST_29MAY18.csv")

# SLD
SLD <- dcast(p7, Type + ECOREG_use + Lake_Origin_use + Indicator ~ Statistic, value.var="Estimate")
write_csv(SLD,"Routput/pop_calculations/NLA07_LK_ECOREG_SLD_percentile_CAST_29MAY18.csv")


###########
# Libraries
###########
library(plyr)
library(ggplot2)
library(reshape2)

library(Hmisc)
library(plotrix)
library(pgirmess)

library(gridExtra)

##############
## ORDER ECOREGIONS
area$ECOREG_use <- ordered(area$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
depth$ECOREG_use <- ordered(depth$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
vol$ECOREG_use <- ordered(vol$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
SLD$ECOREG_use <- ordered(SLD$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

####
## Clean-up datasets
####

# FOR E_I
#todrop <- names(E_I)%in% c("X")
#E_I  <- E_I [!todrop]

# Rename Subpopulation
names(area)[names(area)=="X10Pct"] <- "10Pct"
names(area)[names(area)=="X25Pct"] <- "25Pct"
names(area)[names(area)=="X50Pct"] <- "50Pct"
names(area)[names(area)=="X5Pct"] <- "5Pct"
names(area)[names(area)=="X75Pct"] <- "75Pct"
names(area)[names(area)=="X90Pct"] <- "90Pct"
names(area)[names(area)=="X95Pct"] <- "95Pct"

# Rename Subpopulation
names(depth)[names(depth)=="X10Pct"] <- "10Pct"
names(depth)[names(depth)=="X25Pct"] <- "25Pct"
names(depth)[names(depth)=="X50Pct"] <- "50Pct"
names(depth)[names(depth)=="X5Pct"] <- "5Pct"
names(depth)[names(depth)=="X75Pct"] <- "75Pct"
names(depth)[names(depth)=="X90Pct"] <- "90Pct"
names(depth)[names(depth)=="X95Pct"] <- "95Pct"

names(vol)[names(vol)=="X10Pct"] <- "10Pct"
names(vol)[names(vol)=="X25Pct"] <- "25Pct"
names(vol)[names(vol)=="X50Pct"] <- "50Pct"
names(vol)[names(vol)=="X5Pct"] <- "5Pct"
names(vol)[names(vol)=="X75Pct"] <- "75Pct"
names(vol)[names(vol)=="X90Pct"] <- "90Pct"
names(vol)[names(vol)=="X95Pct"] <- "95Pct"

names(SLD)[names(SLD)=="X10Pct"] <- "10Pct"
names(SLD)[names(SLD)=="X25Pct"] <- "25Pct"
names(SLD)[names(SLD)=="X50Pct"] <- "50Pct"
names(SLD)[names(SLD)=="X5Pct"] <- "5Pct"
names(SLD)[names(SLD)=="X75Pct"] <- "75Pct"
names(SLD)[names(SLD)=="X90Pct"] <- "90Pct"
names(SLD)[names(SLD)=="X95Pct"] <- "95Pct"


###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
setwd("Routput/figs")

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

############
## Boxplots for LAKE VARIABLES
##  Plotting Man-made and natural on same graph - not using region colors
############
# Was getting error messages when plotting values because
#  there are zero values for 10 and 25 percentiles
#  Solution: Add small value to Horiz$'10Pct' & Horiz$'25Pct' so that can take log10 value
# There are a few ways to incoroporate this
#https://github.com/tidyverse/ggplot2/issues/930
# https://stackoverflow.com/questions/38753628/ggplot-boxplot-length-of-whiskers-with-logarithmic-axis
# scale_y_log10(breaks =, limits = set to values of untransformed)
# scale_y_continous(breaks=,...) + coord_trans(y="log10")
# scale_y_continous(trans="log10", breaks=...)
############
## Lakes & reservoirs plotted on separate plots
# ggplot help 
#https://stackoverflow.com/questions/22238278/fine-tuning-ggplot2s-geom-boxplot


##########################################################
### NLA 2007 ###
############################################################
#   SUBSET by Lake_Origin_use
######################
## LAKE AREA
##  RESERVOIRS
area_man<-ggplot(subset(area, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                             middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
               fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(trans="log10",limits=c(NA,7.5),  breaks=c(0,0.10,0.5,2,6,10)) +
    scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

# NATURAL
area_lk<-ggplot(subset(area, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                           middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
              fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(trans="log10",limits=c(NA,7.5),  breaks=c(0,0.10,0.5,2,6,10)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

## LAKE DEPTH
##  RESERVOIRS
depth_man<-ggplot(subset(depth, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                 fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(trans="log10",limits=c(NA,80),  breaks=c(0,2,5,10,20,40,80)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

# NATURAL
depth_lk<-ggplot(subset(depth, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                              middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(trans="log10",limits=c(NA,80),  breaks=c(0,2,5,10,20,40,80)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

## LAKE VOLUME
##  RESERVOIRS
vol_man<-ggplot(subset(vol, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                  middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                  fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(trans="log10",limits=c(10000,100000000))+#,  breaks=c(0,2,5,10,20,40,80)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

# NATURAL
vol_lk<-ggplot(subset(vol, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                 fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_y_continuous(trans="log10",limits=c(10000,100000000))+#,  breaks=c(0,2,5,10,20,40,80)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

## LAKE SLD
##  RESERVOIRS
SLD_man<-ggplot(subset(SLD, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                              middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
 # scale_y_continuous(trans="log10",limits=c(10000,100000000))+#,  breaks=c(0,2,5,10,20,40,80)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

# NATURAL
SLD_lk<-ggplot(subset(SLD, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                            middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
               fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  #scale_y_continuous(trans="log10",limits=c(10000,100000000))+#,  breaks=c(0,2,5,10,20,40,80)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

###########################
## NLA 2007 LAKE VARIABLES MULTIPLOT FIGURES
###########################

##########
## LAKE TYPE AS COLUMN and RESPONSE AS ROW
tiff(filename="lkAREA_LO_ECOREG_USE_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(area_man, area_lk, cols=2)
dev.off()

# DEPTH
tiff(filename="DEPTH_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(depth_man, depth_lk, cols=2)
dev.off()

# VOL
tiff(filename="VOL_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(vol_man, vol_lk, cols=2)
dev.off()

# SLD
tiff(filename="SLD_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.4, units="in", res=600)
multiplot(SLD_man, SLD_lk, cols=2)
dev.off()
