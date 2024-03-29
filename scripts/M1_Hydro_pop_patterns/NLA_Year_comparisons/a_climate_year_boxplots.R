###################################
## Boxplots of climate variables by year (2007 & 2012)
## OBJECTIVE: Look at climate variables (temperature and precip) by years to see differences
#             Was 2007 wetter or dryer than 2012?
#
#  Using ggpot geom_boxplot to make boxplots (visulaizes five summary statistics:  
        # median, two hinges (lower and upper = 25th & 75th percentiles) 
        # and two whiskers (from hinge to largest value no further than 1.5*IQR from the hinge (inter-quartile range or distance between first and third quartiles)), 
        # and all outlying points individually (data beyond the end of the whiskers))
#
# 1/16/18

###################################

rm(list=ls())

library(ggplot2)

#########
# Long-format NLA 2007 & 2012 data
#########
# 12/13/17 - updated to include WRT and WYield - single observations n=1876 
# 5/29/18
# 6/30/18 - summer temperature (May-Sept)
nla_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_14MAY18.csv")
# n = 2066
length(unique(nla_all$SID)) #[1] 1716
names(nla_all)

table(nla_all$YEAR)
# 2007 2012 
# 1028 1038 

#########
# Process dataset
#########

table(nla_all$ECOREG_use)
nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla_all$ECOREG_use)

##########
## PHDI WATER YEAR DROUGHT CATEGORY
##########
nla_all$phdi_cat[nla_all$PHDI>=0 & nla_all$PHDI< 0.5] <-"NORMAL"
nla_all$phdi_cat[nla_all$PHDI<=0 & nla_all$PHDI> -0.5] <-"NORMAL"

nla_all$phdi_cat[nla_all$PHDI<=-0.5 & nla_all$PHDI> -1.0] <-"Incipient_drought"
nla_all$phdi_cat[nla_all$PHDI>=0.5 & nla_all$PHDI< 1.0] <-"Incipient_wet"

nla_all$phdi_cat[nla_all$PHDI<=-1.0 & nla_all$PHDI> -2.0] <-"MILD_drought"
nla_all$phdi_cat[nla_all$PHDI>=1.0 & nla_all$PHDI< 2.0] <-"MILD_wet"

nla_all$phdi_cat[nla_all$PHDI<=-2.0 & nla_all$PHDI> -3.0] <-"MODERATE_drought"
nla_all$phdi_cat[nla_all$PHDI>=2.0 & nla_all$PHDI< 3.0] <-"MODERATE_wet"

nla_all$phdi_cat[nla_all$PHDI<=-3.0 & nla_all$PHDI> -4.0] <-"SEVERE_drought"
nla_all$phdi_cat[nla_all$PHDI>=3.0 & nla_all$PHDI< 4.0] <-"SEVERE_wet"

nla_all$phdi_cat[nla_all$PHDI< -4.0] <-"EXTREME_drought"
nla_all$phdi_cat[nla_all$PHDI> 4.0] <-"EXTREME_wet"

table(nla_all$phdi_cat)
nla_all$phdi_cat<-factor(nla_all$phdi_cat, labels=c("EXTREME_drought","EXTREME_wet","Incipient_drought","Incipient_wet",
                                                    "MILD_drought","MILD_wet","MODERATE_drought","MODERATE_wet",
                                                    "NORMAL","SEVERE_drought","SEVERE_wet"))
plot(nla_all$E_I~ nla_all$phdi_cat)
plot(nla_all$L_VertDD_use~ nla_all$phdi_cat)

#WRITE DATASET WITH DROUGHT CATEGORIES for GIS
myvars<-c("SITE_ID","VISIT_NO","YEAR","dD","d18O","d_excess","E_I","RT_iso","Water_yield_m",
            "VertDD_use","HorizDD_use","L_VertDD_use","L_HorizDD_use","LkArea_km2","DpthMx_mod",
            "Lake_Origin_use","ECOREG_use","RT_NLA12_2015","RESAMPLED12","LATdd_use","LONdd_use","XCOORD","YCOORD","STATE",
            "PHDI","phdi_cat")
test<-nla_all[myvars]

write.csv(test,"M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_0712_for_GIS_03JUN18.csv")

## WRITE TABLE FOR GIS STUDY EXTENT MAP BY RESAMPLED AND NOT RESAMPLED LAKES
table(test$RESAMPLED12)
resamp<-subset(test, RESAMPLED12=="YES")
single<-subset(test, RESAMPLED12=="NO")

write.csv(resamp,"M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_0712_RESAMPLED_for_GIS_03JUN18.csv" )
write.csv(single,"M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_0712_SINGLE_for_GIS_03JUN18.csv" )

##################
# Summary of climate vars
# Summarize variables by a grouping class
library(dplyr)

# Standard error function
sem <- function (x){
  se <- (sd(x)/sqrt(length(x)))
  return(se)
}

# Standard error across years
sem(nla_all$Precip_mm_total_yr)
test07 <-subset(nla_all, YEAR=="2007")
# Checking to see if standard error is right by year
sem(test07$Precip_mm_total_yr) #11.84 - yes this looks right for 2007
mean(test07$Precip_mm_total_yr)

# Group by year - then summarize by year
test<- group_by(nla_all, YEAR)
summarise(test, precip_mm_tot_mean=mean(Precip_mm_total_yr), precip_mm_se=sem(Precip_mm_total_yr),
          temp_summ_mean=mean(temp_degC_summer), temp_summ_se=sem(temp_degC_summer))

# Group by year and region
test_2 <- group_by(nla_all, YEAR, ECOREG_use)
clim_reg<-summarise(test_2, precip_mm_tot_mean=mean(Precip_mm_total_yr), precip_mm_se=sem(Precip_mm_total_yr),
          temp_summ_mean=mean(temp_degC_summer), temp_summ_se=sem(temp_degC_summer))

write.csv(clim_reg, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/Climate_region_summary.csv")


###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

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

# ggplot legend code
#http://ggplot2.tidyverse.org/reference/theme.html

##########
## Boxplot by Ecoregion and Lake type
##########
# 6/4/18 - Decided to break up by lake type to be consistent with drought
# 6/30/18 - updated summer temperature

# NATURAL LAKES
lk<-subset(nla_all, Lake_Origin_use=="NATURAL")
lk<-subset(lk, ECOREG_use!="SAP")
# RESERVOIRS
man<-subset(nla_all, Lake_Origin_use=="MAN_MADE")

## PRECIPITATION - total water year (mm)

summary(nla_all$Precip_mm_total_yr)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.88  544.20  893.90  882.90 1172.00 3685.00

######
# PRECIP by LAKE TYPE
precip_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
              fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+ #"#b2df8a","#1f78b4"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        #legend.justification=c("right", "top"),
        #legend.box.just = "right",
        #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ggtitle("Natural")+
  ylab (NULL)+#("Precipitation (mm)")+ #
  xlab(NULL)#
  
# RESERVOIR
  precip_man<-ggplot(man, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
                    fill=factor(YEAR))+
    geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
    scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3500)) +
    scale_fill_manual(values = c("#808080","#E0E0E0"))+ #"#b2df8a","#1f78b4"
    theme_bw(base_size=12)+
    theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
          axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
          axis.text.y = element_text(family = "RMN"),
          axis.title.y=element_text(family="RMN"),
          panel.grid.major =  element_line(colour = NA), 
          panel.grid.minor=element_line(colour = NA),
          panel.spacing = unit(c(1,1,0,4), "lines"),
          legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
    #legend.justification=c("right", "top"),
    #legend.box.just = "right",
    #legend.margin=margin(6,6,6,6),
    #legend.title=element_blank(),
    #legend.text=element_text(family="RMN"))+
    ggtitle("Man-made")+
  ylab("Precipitation (mm)")+ 
    xlab(NULL)#
  
#######
## Temperature - Summer temperature - by LAKE TYPE
summary(nla_all$temp_degC_summer)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 9.51   20.52   22.92   22.96   25.98   36.71 

# NATURAL LAKES
temp_summer_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = temp_degC_summer),
               fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=temp_degC_summer))+#,stat="identity")+
  scale_y_continuous(limits=c(9,35),  breaks=c(10,15,20,25,30,35)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position=c(0.90,0.29),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Natural lake")+
  ylab(NULL) +#(expression(paste("Summer temperature ( ",degree,"C)")))+ #
  xlab(NULL)#

# RESERVOIRS
temp_summer_man<-ggplot(man, aes(x=factor(ECOREG_use), y = temp_degC_summer),
                    fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=temp_degC_summer))+#,stat="identity")+
  scale_y_continuous(limits=c(9,35),  breaks=c(10,15,20,25,30,35)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position=c(0.90,0.29),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        #legend.justification=c("right", "top"),
        #legend.box.just = "right",
        #legend.margin=margin(6,6,6,6),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  #ggtitle("Reservoir")+
  ylab (expression(paste("Summer temperature ( ",degree,"C)")))+ #
  xlab(NULL)#


## PET ##
summary(nla_all$E)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2600  0.5800  0.6584  0.6985  0.8075  1.4470 

PET<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = E),
                      fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=E))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Annual potential evapotranspiration (m)")+ #
  xlab(NULL)#

##############################
## DROUGHT INDICES

PDSI<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = PDSI),
            fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PDSI))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+ # green"#b2df8a" blue "#1f78b4"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("PDSI")+ #
  xlab(NULL)#

PHDI<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = PHDI),
             fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("PHDI Water Year")+ #
  xlab(NULL)#

PHDI_use<-PHDI + geom_hline(yintercept=0, linetype="dashed", color="red", size=1.5)



PMDI<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = PMDI),
             fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PMDI))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Modified PDSI")+ #
  xlab(NULL)#

ZNDX<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = ZNDX),
             fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=ZNDX))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Palmer Z Index")+ #
  xlab(NULL)#

# FOR LEGEND
ZNDX_legend<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = ZNDX),
             fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=ZNDX))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="bottom",#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  legend.title=element_blank(),
  legend.text=element_text(family="RMN"))+
  ylab("Palmer Z Index")+ #
  xlab(NULL)#


###########################
## SEPARATE OUT LAKE TYPE AND PHDI WY
###########################
lk<-subset(nla_all, Lake_Origin_use=="NATURAL")
lk<-subset(lk, ECOREG_use!="SAP")

man<-subset(nla_all, Lake_Origin_use=="MAN_MADE")

# LAKES
PHDI_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = PHDI),
             fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  scale_y_continuous(limits=c(-5.5,4.7), breaks=c(-4.0,-2.0,0,2,4)) +
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Natural lake")+
  ylab(NULL) + #("PHDI Water Year")+
  xlab(NULL)#

PHDI_lk_use<-PHDI_lk + geom_hline(yintercept=0, linetype="dashed", color="red", size=1.5)

# RESERVOIRS
PHDI_man<-ggplot(man, aes(x=factor(ECOREG_use), y = PHDI),
                fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  scale_y_continuous(limits=c(-5.5,4.7), breaks=c(-4.0,-2.0,0,2,4)) +
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Reservoir")+
  ylab ("PHDI Water Year")+ #
  xlab(NULL)#

PHDI_man_use<-PHDI_man + geom_hline(yintercept=0, linetype="dashed", color="red", size=1.5)



## PRECIPITATION BY SEASON ##
summary(nla_all$precip_mm_winter)

precip_winter<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = precip_mm_winter),
               fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=precip_mm_winter))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#b2df8a","#1f78b4"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        #legend.justification=c("right", "top"),
        #legend.box.just = "right",
        #legend.margin=margin(6,6,6,6),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab("Winter precipitation (mm)")+ #
  xlab(NULL)#

summary(nla_all$precip_mm_summer)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00   40.19   66.95   72.44   97.34  325.10

precip_summer<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = precip_mm_summer),
                      fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=precip_mm_summer))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) + #if min is zero or close to zero put "NA" as min
  scale_fill_manual(values = c("#b2df8a","#1f78b4"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        #legend.justification=c("right", "top"),
        #legend.box.just = "right",
        #legend.margin=margin(6,6,6,6),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab("Summer precipitation (mm)")+ #
  xlab(NULL)#

precip_tot<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
               fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3000)) +
  scale_fill_manual(values = c("#b2df8a","#1f78b4"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position=c(0.90,0.40),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  legend.justification=c("right", "top"),
  legend.box.just = "right",
  legend.margin=margin(6,6,6,6),
  legend.title=element_blank(),
  legend.text=element_text(family="RMN"))+
  ylab("Precipitation (mm)")+ #
  xlab(NULL)#

## TEMPERATURE BY SEASON ##
summary(nla_all$temp_degC_winter)
temp_winter<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = temp_degC_winter),
                    fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=temp_degC_winter))+#,stat="identity")+
  scale_y_continuous(limits=c(-12,37), breaks=c(-10,0,10,20,30)) +
  #scale_y_continuous(trans="log10",limits=c(NA,37),  breaks=c(-10,0,10,20)) +
  scale_fill_manual(values = c("#b2df8a","#1f78b4"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        #legend.justification=c("right", "top"),
        #legend.box.just = "right",
        #legend.margin=margin(6,6,6,6),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab(expression(paste("Winter temperature ( ",degree,"C)")))+ #
  xlab(NULL)#

## TEMPERATURE ANNUAL AVERAGE ##
summary(nla_all$Temp_degC_avg_yr)
temp_avg<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = Temp_degC_avg_yr),
                    fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Temp_degC_avg_yr))+#,stat="identity")+
  scale_y_continuous(limits=c(-12,37), breaks=c(-10,0,10,20,30)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#b2df8a","#1f78b4"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position=c(0.92,0.37),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        legend.justification=c("right", "top"),
        legend.box.just = "right",
        legend.margin=margin(6,6,6,6),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab(expression(paste("Average temperature ( ",degree,"C)")))+ #
  xlab(NULL)#

temp_summer_2<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = temp_degC_summer),
                    fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=temp_degC_summer))+#,stat="identity")+
  scale_y_continuous(limits=c(-12,37), breaks=c(-10,0,10,20,30)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#b2df8a","#1f78b4"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.40),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
        #legend.justification=c("right", "top"),
        #legend.box.just = "right",
        #legend.margin=margin(6,6,6,6),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab(expression(paste("Summer temperature ( ",degree,"C)")))+ #
  xlab(NULL)#


##########################
## OTHER METHODS to SUMMARIZE PHDI
##  5/30/18
##########################
# METHOD 1: DROUGHT SAMPLE MONTH
##########
# LOAD NLA 07+12 long format data with sample month PHDI
nla_all <-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_PHDI_MONTH_30MAY18.csv")
names(nla_all)
nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

PHDI_month<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = phdi_col),
             fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=phdi_col))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("PHDI Sample Month")+ #
  xlab(NULL)#

###########
# METHOD 2: DROUGHT SUMMER AVERAGE
###########
# LOAD NLA 07+12 long format data with average summer PHDI
nla_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_PHDI_SUMMER_30MAY18.csv")
nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
names(nla_all)

PHDI_summer<-ggplot(nla_all, aes(x=factor(ECOREG_use), y = phdi_SUMM_AVG),
                   fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=phdi_SUMM_AVG))+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("PHDI Summer Avg")+ #
  xlab(NULL)#


################
## MULTIPLOTS
################
# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")


# TOTAL PRECIP + SUMMER TEMP
tiff(filename="PRECIP_TOTAL_SUMMER_TEMP.tiff",width=7, height=6, units="in", res=600)
multiplot(precip,temp_summer, cols=1)
dev.off()

# PET
tiff(filename="PET.tiff",width=5, height=3, units="in", res=600)
multiplot(PET, cols=1)
dev.off()

# PDSI
tiff(filename="PDSI.tiff",width=5, height=3, units="in", res=600)
multiplot(PDSI, cols=1)
dev.off()

# PRECIPITATION
tiff(filename="PRECIP_VARS_YEAR.tiff",width=6.5, height=5, units="in", res=600)
multiplot(precip_winter,precip_tot,precip_summer, cols=2)
dev.off()

# TEMPERATURE
tiff(filename="TEMP_VARS_YEAR.tiff",width=6.5, height=5, units="in", res=600)
multiplot(temp_winter,temp_avg,temp_summer_2, cols=2)
dev.off()

# PALMER DROUGHT INDICES
tiff(filename="PALMER_DROUGHT.tiff",width=5, height=9, units="in", res=600)
multiplot(PDSI,PHDI,PMDI,ZNDX, cols=1)
dev.off()

tiff(filename="PALMER_Legend.tiff",width=5, height=3, units="in", res=600)
multiplot(ZNDX_legend, cols=1)
dev.off()

# PALMER PHDI
tiff(filename="PHDI_METRICS.tiff",width=5, height=9, units="in", res=600)
multiplot(PHDI_month,PHDI_summer,PHDI, cols=1)
dev.off()

# PALMER PHDI - SELECTED METHOD
# PHDI Summer Avg
tiff(filename="PHDI_summer.tiff",width=5, height=3, units="in", res=600)
multiplot(PHDI_summer, cols=1)
dev.off()


# PHDI WATER YEAR ALL
tiff(filename="PHDI_WY.tiff",width=5, height=3, units="in", res=600)
multiplot(PHDI_use, cols=1)
dev.off()



#Precip by LAKE TYPE (Natural, Man-made)
# TOTAL PRECIP
tiff(filename="PRECIP_TOTAL_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(precip_man,precip_lk, cols=2)
dev.off()

#Summer Temp by LAKE TYPE
tiff(filename="SUMMER_TEMP_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(temp_summer_man, temp_summer_lk,cols=2)
dev.off()

# PHDI WATER YEAR BY LAKE TYPE
tiff(filename="PHDI_WY_LO_2.tiff",width=7, height=3, units="in", res=600)
multiplot(PHDI_man_use,PHDI_lk_use, cols=2)
dev.off()

#####################
## LINEAR REGRESSION
#####################

# SUMMER PHDI
names(nla_all)

lm1<-lm(nla_all$L_VertDD_use~nla_all$phdi_SUMM_AVG)
#lm1<-lm(nla_all$L_DDVrtDix_sc_MOD~nla_all$phdi_SUMM_AVG)
summary(lm1)

nat_lk <-subset(nla_all,Lake_Origin_use=="NATURAL")
man<-subset(nla_all,Lake_Origin_use=="MAN_MADE")

lm1_lk<-lm(nat_lk$L_VertDD_use~nat_lk$phdi_SUMM_AVG)
#lm1_lk<-lm(nat_lk$L_DDVrtDix_sc_MOD~nat_lk$phdi_SUMM_AVG)
summary(lm1_lk)

lm1_man<-lm(man$L_VertDD_use~man$phdi_SUMM_AVG)
#lm1_man<-lm(man$L_DDVrtDix_sc_MOD~man$phdi_SUMM_AVG)
summary(lm1_man)

# SUMMER TEMP
names(nla_all)
lm1<-lm(nla_all$L_VertDD_use~nla_all$temp_degC_summer)
lm1<-lm(nla_all$L_DDVrtDix_sc_MOD~nla_all$temp_degC_summer)
summary(lm1)

lm1_lk<-lm(nat_lk$L_VertDD_use~nat_lk$temp_degC_summer)
lm1_lk<-lm(nat_lk$L_DDVrtDix_sc_MOD~nat_lk$temp_degC_summer)
summary(lm1_lk)

lm1_man<-lm(man$L_VertDD_use~man$temp_degC_summer)
lm1_man<-lm(man$L_DDVrtDix_sc_MOD~man$temp_degC_summer)
summary(lm1_man)

# PRECIP
lm1<-lm(nla_all$L_VertDD_use~nla_all$Precip_mm_total_yr)
lm1<-lm(nla_all$L_DDVrtDix_sc_MOD~nla_all$Precip_mm_total_yr)
summary(lm1)

lm1_lk<-lm(nat_lk$L_VertDD_use~nat_lk$Precip_mm_total_yr)
lm1_lk<-lm(nat_lk$L_DDVrtDix_sc_MOD~nat_lk$Precip_mm_total_yr)
summary(lm1_lk)

lm1_man<-lm(man$L_VertDD_use~man$Precip_mm_total_yr)
lm1_man<-lm(man$L_DDVrtDix_sc_MOD~man$Precip_mm_total_yr)
summary(lm1_man)


####################
## Plot L VertDD vs. Weather
## By lake type and by ecoregion
## 5/31/18
# 6/8/18 - Grand mean slope
####################
# RESERVOIRS
#############
# PLOTTING EACH REGIONAL SLOPE
g_precip_man <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                aes(x=Precip_mm_total_yr, y=L_VertDD_use, color=ECOREG_use))+
  scale_y_continuous()+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vert drawdown")+
  xlab("Precipitation (mm)")


# NATURAL LAKES
g_precip_lk <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
               aes(Precip_mm_total_yr, y=L_VertDD_use, color=ECOREG_use))+
  scale_y_continuous()+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vert drawdown")+
  xlab("Precipitation (mm)")


###########
## PHDI WY vs. VERT DD
# LOAD DATA WITH WATER YEAR
# 5/29/18
nla_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_14MAY18.csv")
names(nla_all)

# JUST 2007 data for ASLO PRESENTATION 2018 - DECIDED NOT TO USE
#nla_all<-nla_all[which(nla_all$YEAR==2007),]
#nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
#######################
# JUST PLOTTED THE GRAND MEAN SLOPE
#######################
g_man_grdmean <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                        aes(x=PHDI, y=VertDD_use+0.01))+ #, color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(aes(color=ECOREG_use),shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE, color="black")+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vert drawdown")+
  xlab("PHDI Water Yr")

# NATURAL LAKES
g_lk_grdmean <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
                       aes(PHDI, y=VertDD_use+0.01))+ #, color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(aes(color=ECOREG_use), shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE, color="black")+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.position="bottom", #)+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+
  xlab("PHDI Water Yr")


###########
## SLOPE BY ECOREGIONS
###########
# RESERVOIRS
g_man <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                aes(x=PHDI, y=VertDD_use+0.01, color=ECOREG_use))+
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vert drawdown")+
  xlab("PHDI Water YR")


# NATURAL LAKES
g_lk <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
               aes(PHDI, y=VertDD_use+0.01, color=ECOREG_use))+
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
        #legend.position="bottom", #)+
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab(NULL)+
  xlab("PHDI Water Yr")

###########
## PHDI SAMPLED MONTH XY PLOTS
# LOAD DATA WITH PHDI sampled month
# 6/3/18
nla_all <-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_PHDI_MONTH_30MAY18.csv")
names(nla_all)

nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


# RESERVOIRS
g_man_month <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                aes(x=phdi_col, y=VertDD_use+0.01, color=ECOREG_use))+
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vert drawdown")+
  xlab("PHDI sample month")


# NATURAL LAKES
g_lk_month <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
               aes(phdi_col, y=VertDD_use+0.01, color=ECOREG_use))+
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.position="bottom")+
  ylab(NULL)+
  xlab("PHDI sample month")


# PHDI SUMMER MONTHS
# RESERVOIRS
g_man_summer <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                      aes(x=phdi_SUMM_AVG, y=VertDD_use+0.01, color=ECOREG_use))+
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vert drawdown")+
  xlab("PHDI summer")


# NATURAL LAKES
g_lk_summer <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
                     aes(phdi_SUMM_AVG, y=VertDD_use+0.01, color=ECOREG_use))+
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.position="bottom")+
  ylab(NULL)+
  xlab("PHDI summer")

#############
## E:I vs. PHDI WY
#############
nla_all<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_SINGLE_14MAY18.csv")
names(nla_all)

nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

###############
# BY ECOREGIONS
###############
# RESERVOIRS
g_man_ei <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                aes(x=PHDI, y=E_I, color=ECOREG_use))+
  scale_y_continuous(limits=c(NA,1.0))+
  #scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("E:I")+
  xlab("PHDI Water Yr")


# NATURAL LAKES
g_lk_ei <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
               aes(PHDI, y=E_I, color=ECOREG_use))+
  scale_y_continuous(limits=c(NA,1.0))+
  #scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
        #legend.position="bottom", #)+
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
  ylab(NULL)+
  xlab("PHDI Water Yr")

##############
# GRAND MEAN SLOPE
##############
# RESERVOIRS
g_man_ei_grdmean <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                   aes(x=PHDI, y=E_I))+ #, color=ECOREG_use
  scale_y_continuous(limits=c(NA,1.0))+
  #scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(aes(color=ECOREG_use),shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE, color="black")+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("E:I")+
  xlab("PHDI Water Yr")


# NATURAL LAKES
g_lk_ei_grdmean <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
                  aes(PHDI, y=E_I))+ #, color=ECOREG_use
  scale_y_continuous(limits=c(NA,1.0))+
  #scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(aes(color=ECOREG_use), shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE, color="black")+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.position="bottom", #)+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+
  xlab("PHDI Water Yr")



# SUMMER E:I
# RESERVOIRS
g_man_ei_summer <- ggplot(subset(nla_all, Lake_Origin_use=="MAN_MADE"),
                   aes(x=phdi_SUMM_AVG, y=E_I, color=ECOREG_use))+
  scale_y_continuous(limits=c(NA,1.0))+
  #scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Reservoir")+
  theme_dark(base_size=14) +  # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("E:I")+
  xlab("PHDI Summer")

# NATURAL LAKES
g_lk_ei_summer <- ggplot(subset(nla_all, Lake_Origin_use=="NATURAL"),
                  aes(phdi_SUMM_AVG, y=E_I, color=ECOREG_use))+
  scale_y_continuous(limits=c(NA,1.0))+
  #scale_y_continuous(trans="log10",limits=c(NA,50),breaks=c(0.05,0.5,5,30)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  ggtitle("Lake")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.position="bottom", #)+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+
  xlab("PHDI summer")

###################
## MULTIPLOT SCATTERPLOTS
#####################

# precip
tiff(filename="xy_precip.tiff",width=7, height=3, units="in", res=600)
multiplot(g_precip_man,g_precip_lk, cols=2)
dev.off()

# PHDI WY
tiff(filename="xy_phdi.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man,g_lk, cols=2)
dev.off()

# PHDI legend
tiff(filename="xy_phdi_legend.tiff",width=7, height=5, units="in", res=600)
multiplot(g_lk, cols=1)
dev.off()

# PHDI Samplemonth
tiff(filename="xy_phdi_month.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man_month,g_lk_month, cols=2)
dev.off()

# E:I vs PHDI WY
tiff(filename="xy_phdi_EI.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man_ei,g_lk_ei, cols=2)
dev.off()

# PHDI summer vs. Vert
tiff(filename="xy_phdi_summer.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man_summer,g_lk_summer, cols=2)
dev.off()

# E:I vs PHDI SUMMER
tiff(filename="xy_phdi_EI_summer.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man_ei_summer,g_lk_ei_summer, cols=2)
dev.off()

# PHDI WY for 2007 survey only
#tiff(filename="xy_phdi_2007.tiff",width=7, height=5, units="in", res=600)
#multiplot(g_man,g_lk, cols=2)
#dev.off()

# E:I vs PHDI WY for 2007 survey only
#tiff(filename="xy_phdi_EI_2007.tiff",width=7, height=5, units="in", res=600)
#multiplot(g_man_ei,g_lk_ei, cols=2)
#dev.off()

# PHDI WY w/GRAND MEAN SLOPE
tiff(filename="xy_phdi_grandmean.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man_grdmean,g_lk_grdmean, cols=2)
dev.off()

# PHDI WY w/GRAND MEAN SLOPE
tiff(filename="xy_phdi_EI_grandmean.tiff",width=7, height=5, units="in", res=600)
multiplot(g_man_ei_grdmean,g_lk_ei_grdmean, cols=2)
dev.off()


##############################
##############################
## WEATHER BOXPLOTS
## RESAMPLED LAKES (n=348)
##
## 8/23/18
##############################
##########
# LOAD DATA
## Resampled lakes - long format (row bound)

nla07_12 <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
# n = 696 observations (348 lakes resampled)

names(nla07_12)
#########
# Process dataset
#########

table(nla07_12$ECOREG_use)
nla07_12$ECOREG_use <- ordered(nla07_12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla07_12$ECOREG_use)

###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

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

# ggplot legend code
#http://ggplot2.tidyverse.org/reference/theme.html

##########
## Boxplot by Ecoregion and Lake type
##########
# 6/4/18 - Decided to break up by lake type to be consistent with drought
# 6/30/18 - updated summer temperature

# NATURAL LAKES
lk<-subset(nla07_12, Lake_Origin_use=="NATURAL")
lk<-subset(lk, ECOREG_use!="SAP")
# RESERVOIRS
man<-subset(nla07_12, Lake_Origin_use=="MAN_MADE")

## PRECIPITATION - total water year (mm)

summary(nla07_12$Precip_mm_total_yr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#28.88  553.60  913.60  894.20 1190.00 2729.00

######
# PRECIP by LAKE TYPE
precip_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
                  fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+ #"#b2df8a","#1f78b4"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ggtitle("Natural")+
  ylab (NULL)+#("Precipitation (mm)")+ #
  xlab(NULL)#

# RESERVOIR
precip_man<-ggplot(man, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
                   fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+ #"#b2df8a","#1f78b4"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ggtitle("Man-made")+
  ylab("Precipitation (mm)")+ 
  xlab(NULL)#

#######
## Temperature - Summer temperature - by LAKE TYPE
summary(nla07_12$temp_degC_summer)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   10.54   18.65   20.90   21.27   24.11   34.55 

# NATURAL LAKES
temp_summer_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = temp_degC_summer),
                       fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=temp_degC_summer))+#,stat="identity")+
  scale_y_continuous(limits=c(9,35),  breaks=c(10,15,20,25,30,35)) +
  #scale_y_continuous(trans="log10",limits=c(9,35),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position=c(0.90,0.29),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Natural lake")+
  ylab(NULL) +#(expression(paste("Summer temperature ( ",degree,"C)")))+ #
  xlab(NULL)#

# RESERVOIRS
temp_summer_man<-ggplot(man, aes(x=factor(ECOREG_use), y = temp_degC_summer),
                        fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=temp_degC_summer))+#,stat="identity")+
  scale_y_continuous(limits=c(9,35),  breaks=c(10,15,20,25,30,35)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position=c(0.90,0.29),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Reservoir")+
  ylab (expression(paste("Summer temperature ( ",degree,"C)")))+ #
  xlab(NULL)#


############
## PHDI
###
# LAKES
PHDI_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = PHDI),
                fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  scale_y_continuous(limits=c(-5.5,4.7), breaks=c(-4.0,-2.0,0,2,4)) +
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Natural lake")+
  ylab(NULL) + #("PHDI Water Year")+
  xlab(NULL)#

PHDI_lk_use<-PHDI_lk + geom_hline(yintercept=0, linetype="dashed", color="black", size=1.5)

# RESERVOIRS
PHDI_man<-ggplot(man, aes(x=factor(ECOREG_use), y = PHDI),
                 fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  scale_y_continuous(limits=c(-5.5,4.7), breaks=c(-4.0,-2.0,0,2,4)) +
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Reservoir")+
  ylab ("PHDI Water Year")+ #
  xlab(NULL)#

PHDI_man_use<-PHDI_man + geom_hline(yintercept=0, linetype="dashed", color="black", size=1.5)

################
## MULTIPLOTS
################
# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

#Precip by LAKE TYPE (Natural, Man-made)
# TOTAL PRECIP
tiff(filename="RESAMPLE_PRECIP_TOTAL_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(precip_man,precip_lk, cols=2)
dev.off()

#Summer Temp by LAKE TYPE
tiff(filename="RESAMPLE_SUMMER_TEMP_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(temp_summer_man, temp_summer_lk,cols=2)
dev.off()


# PHDI WATER YEAR BY LAKE TYPE
tiff(filename="RESAMPLE_PHDI_WY_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(PHDI_man_use,PHDI_lk_use, cols=2)
dev.off()

###################
## Scaled drawdown vs. EI
###################

plot(nla07_12$L_DDVrtDix_sc_MOD~nla07_12$E_I)
abline(lm(nla07_12$L_DDVrtDix_sc_MOD~nla07_12$E_I))

plot(nla07_12$L_DDHzSqrtA_sc ~ nla07_12$E_I)
abline(lm(nla07_12$L_DDHzSqrtA_sc ~ nla07_12$E_I))

# Scaled vertical DD vs. EI
ggplot(nla07_12, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=Lake_Origin_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  #geom_smooth(method=lm)
  #geom_abline()+
  facet_wrap(~ Lake_Origin_use, scales="free")

# UNscaled Vert DD vs. EI - see a positive trend but magnitude isn't as great as scaled
ggplot(nla07_12, aes(x=E_I, y=L_VertDD_use, group=Lake_Origin_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  #geom_smooth(method=lm)
  #geom_abline()+
  facet_wrap(~ Lake_Origin_use, scales="free")

#######################################
## By ECOREGION ##############
# Scaled vertical DD vs. EI
## MAN_MADE LAKES ##
man_xy_plot<-ggplot(man, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

## NATURAL LAKES ##
nat_xy_plot<-ggplot(lk, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

# UNscaled Vert DD vs. EI - see a positive trend but magnitude isn't as great as scaled
## MAN_MADE
ggplot(man, aes(x=E_I, y=L_VertDD_use, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  #geom_smooth(method=lm)
  #geom_abline()+
  facet_wrap(~ ECOREG_use, scales="free")

## NATURAL
ggplot(lk, aes(x=E_I, y=L_VertDD_use, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  #geom_smooth(method=lm)
  #geom_abline()+
  facet_wrap(~ ECOREG_use, scales="free")

##########################
## BY YEAR
nla07<-nla07_12[which(nla07_12$YEAR=="2007"),]
nla12<-nla07_12[which(nla07_12$YEAR=="2012"),]

nla07$ECOREG_use <- ordered(nla07$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla12$ECOREG_use <- ordered(nla12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


## NLA07 ##
nla07_xy_plot<-ggplot(nla07, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

## NLA12 ##
nla12_xy_plot<-ggplot(nla12, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

##########################
## BY YEAR AND LAKE TYPE
nla07<-nla07_12[which(nla07_12$YEAR=="2007"),]
nla12<-nla07_12[which(nla07_12$YEAR=="2012"),]

nla07_man <-nla07[which(nla07$Lake_Origin_use=="MAN_MADE"),]
nla07_nat <-nla07[which(nla07$Lake_Origin_use=="NATURAL"),]

nla07_man$ECOREG_use <- ordered(nla07_man$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla07_nat$ECOREG_use <- ordered(nla07_nat$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


## NLA07 MAN_MADE##
nla07_man_xy_plot<-ggplot(nla07_man, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

## NLA07 MAN_MADE##
nla07_nat_xy_plot<-ggplot(nla07_nat, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#


######################
## PLOTS of Scaled vert vs. EI - for resampled lakes (pooling year together)
# MAN_MADE
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/RESAMPLE_Scaled_VERTDD_EI_MAN.tiff",width=5, height=5, units="in", res=600)
man_xy_plot
dev.off()

# NATURAL
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/RESAMPLE_Scaled_VERTDD_EI_NAT.tiff",width=5, height=5, units="in", res=600)
nat_xy_plot
dev.off()

# NLA 2007
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/RESAMPLE_Scaled_VERTDD_EI_NLA07.tiff",width=5, height=5, units="in", res=600)
nla07_xy_plot
dev.off()

# NLA 2012
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/RESAMPLE_Scaled_VERTDD_EI_NLA12.tiff",width=5, height=5, units="in", res=600)
nla12_xy_plot
dev.off()