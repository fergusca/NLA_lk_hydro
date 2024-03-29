######################
## POST ANALYSIS EXPLORATION FOR L&O REVIEWS 
## 2/11/2022
######################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)
library(tidyverse)
library(ggpubr) # statistics for comparison
library(ggpmisc) # for regression 

##################
## LOAD DATA
##################
# CONUS LOW HYDRAP (0 - 2) n = 553 lakes
low_hydrap <-read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/conus_low_hydrap.csv")
todrop<-names(low_hydrap)%in%c("X1","X1_1") 
low_hydrap<-low_hydrap[!todrop]

# By ECOREGION
west <-read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/west.csv") #n = 429
mwest <- read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/mwest.csv") #n = 482
gplains <- read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/gplains.csv") #n = 292
apps <- read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/apps.csv") #n = 324
cstplains <- read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/cstplains.csv") #n = 189

# Clean up datasets
todrop<-names(west)%in%c("X1","X1_1") 
west<-west[!todrop]
todrop<-names(mwest)%in%c("X1","X1_1") 
mwest<-mwest[!todrop]
todrop<-names(gplains)%in%c("X1","X1_1") 
gplains<-gplains[!todrop]
todrop<-names(apps)%in%c("X1","X1_1") 
apps<-apps[!todrop]
todrop<-names(cstplains)%in%c("X1","X1_1") 
cstplains<-cstplains[!todrop]


############
## COMBINE WEST AND MIDWEST DATASETS
dat<-rbind(west,gplains,mwest,apps,cstplains)

dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West","Great Plains", "Midwest","Appalachians", "Coastal Plains"))

low_hydrap$ECOREG_rev<-ordered(low_hydrap$ECOREG_rev, levels=c("West","Great Plains","Midwest","Appalachians", "Coastal Plains"))


# Subset lakes with high HydrAP ranks
dat_hi<-dat%>%
  filter(HydrAP>3)

# Subset by ecoregions
dat_1<- dat%>%
  filter(ECOREG_rev=="West"|ECOREG_rev=="Midwest")

# Make HydrAP class
dat$HydrAP_class<-cut(dat$HydrAP, breaks = c(-1,2,8),
                      labels=c("low","high"))

table(dat$HydrAP_class, dat$OUTLET_DAMS_red2)
#     DAM NONE
#low   26  527
#high 793  140

z<-dat%>%
  filter(HydrAP_class=="high")
table(z$OUTLET_DAMS_red2,z$ECOREG_rev)
#     West Midwest Great Plains Appalachians Coastal Plains
#DAM   222     127          147          205             92
#NONE   15      85           18            6             16

# % High HydrAP lakes with dams
# West = 93.6%; Midwest = 60%; Great Plains = 89%; Apps = 97%; CPL=85%

# In teh Midwest, there were 85 lakes with High HydrAP without dams
summary(z$PctDEVELOPED_Cat)
summary(z$PctAgDrainageCat)
summary(z$PctIrrigated.AgLandCat)

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman



########################
## LOOK AT Dam relationships with lake hydrologic responses
# Relationship between vertical decline and whether a dam is present in MWEST and WEST
vert_dam<-ggplot(dat_1, aes(x=factor(OUTLET_DAMS_red2), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,50),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"), #,#
        axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Vertical decline (m)"))

vert_dam
# I think drops a lot of observations because vertical decline =0 (n = 294), 37 are missing outlet dam


horiz_dam<-ggplot(dat_1, aes(x=factor(OUTLET_DAMS_red2), y = HorizDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=HorizDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,550),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text.x = element_blank(),#element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Horizontal decline (m)"))

horiz_dam

ei_dam<-ggplot(dat_1, aes(x=factor(OUTLET_DAMS_red2), y = E_I))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=E_I),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,1.00),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text.x = element_blank(),#element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Evaporation:Inflow"),
    x=expression("Dam presence"))

ei_dam

####################
## PLOT PANELS
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

# Lake hydro response by dam presence
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Review_figs/mwest_west_lk_hydro_dam.tiff",
     width=7,height=8, units="in", res=400)
grid.arrange(arrangeGrob(vert_dam,
                         horiz_dam,
                         ei_dam,
                         nrow=3))
dev.off()

###########################
## PLOT PERCENTAGES OF LAKES WITH AND WITHOUT DAMS BY ECOREGION
d<-dat%>%
  group_by(ECOREG_rev, .add=TRUE) %>%
  mutate(countT=n())%>%
  group_by(ECOREG_rev,OUTLET_DAMS_red2)%>%
  mutate(count_dam=n())%>%
  summarize(pct_dam=count_dam/countT)
dam<-d%>%
  filter(OUTLET_DAMS_red2=="DAM")
table(dam$ECOREG_rev,dam$pct_dam)

nodam<-d%>%
  filter(OUTLET_DAMS_red2=="NONE")
table(nodam$ECOREG_rev, nodam$pct_dam)

# CREATE DATAFRAME OF PERCENTAGE OF LAKES WITH dams by ecoregion
# Ecoregions
df<-data.frame(eco=c("West","Midwest","Great Plains","Appalachians","Coastal Plains"),
               DAM=c(62.2,38.6,67.8,80.9,70.9),
               NONE=c(37.8,61.4,32.2,19.1,29.1))
# Create long table for stacked bar charts
df_long<-gather(df,percentage,value,DAM,NONE)

df_long$eco<-ordered(df_long$eco, levels=c("West", "Midwest","Great Plains", "Appalachians", "Coastal Plains"))

# Stacked + percent
stacked_dam<-ggplot(df_long, aes(fill=percentage, y=value, x=eco, label=value)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5),family = 'RMN')+
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        legend.title = element_blank(),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Percentage lakes"),
    x=expression(""))

##############
## SAVE STACKED BARCHART OF DAM%
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Review_figs/dam_pct_ecoreg.tiff",
     width=5,height=5, units="in", res=300)
stacked_dam
dev.off()



####################
## DISTRIBUTIONS OF VARIABLES IN HYDRAP BY LAKE SUBSETS
# Dam height relative to lake depth
tapply(dat$damht_zmax_full,dat$ECOREG_rev,max, na.rm=T)

# For Kruskal-Wallis and pairwise difference results - see revised model script
anno <- data.frame(xstar=c(1,2,3,4,5),ystar=c(15,15,15,15,15),
                   lab=c("a","a","b","a","ab"),
                   variable=c("West","Great Plains", "Midwest","Appalachians", "Coastal Plains"))

damht<-ggplot(dat_hi, aes(x=factor(ECOREG_rev), y = damht_zmax_full))+#,fill=factor(YEAR))+
  geom_boxplot(aes(y=damht_zmax_full,fill=ECOREG_rev),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,35),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=4)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  geom_text(data=anno, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Dam ht:zmax"),
    x=expression(""))
damht
# Pairwise comparisons
pairwise.wilcox.test(dat_hi$damht_zmax_full, dat_hi$ECOREG_rev,
                     p.adjust.method = "BH")

# PCT DEVELOPED
tapply(dat_hi$PctDEVELOPED_Cat,dat_hi$ECOREG_rev,max,na.rm=T)

# For Kruskal-Wallis and pairwise difference results - see revised model script
anno2 <- data.frame(xstar=c(1,2,3,4,5),ystar=c(85,85,85,85,85),
                   lab=c("a","a","bc","b","c"),
                   variable=c("West","Great Plains","Midwest", "Appalachians", "Coastal Plains"))

urban<-ggplot(dat_hi, aes(x=factor(ECOREG_rev), y = PctDEVELOPED_Cat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(ECOREG_rev),y=PctDEVELOPED_Cat),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,100),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+# aes(shape=factor(OUTLET_DAMS_rev))+
  stat_compare_means(size=2.5,family="RMN",label.x=4.5)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  geom_text(data=anno2, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Urban"),
    x=expression(""))
urban

pairwise.wilcox.test(dat_hi$PctDEVELOPED_Cat, dat_hi$ECOREG_rev,
                     p.adjust.method = "BH")


# PCT Irrigated Ag
tapply(dat_hi$PctIrrigated.AgLandCat,dat_hi$ECOREG_rev,max,na.rm=T)

# For Kruskal-Wallis and pairwise difference results - see revised model script
anno3 <- data.frame(xstar=c(1,2,3,4,5),ystar=c(85,85,85,85,85),
                    lab=c("a","a","a","b","a"),
                    variable=c("West","Great Plains","Midwest","Appalachians", "Coastal Plains"))

irrag<-ggplot(dat_hi, aes(x=factor(ECOREG_rev), y = PctIrrigated.AgLandCat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(ECOREG_rev),y=PctIrrigated.AgLandCat),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,100),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+# aes(shape=factor(OUTLET_DAMS_rev))+
  stat_compare_means(size=2.5,family="RMN",label.x=4.3)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  geom_text(data=anno3, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Irrigated Ag"),
    x=expression(""))
irrag

pairwise.wilcox.test(dat_hi$PctIrrigated.AgLandCat, dat_hi$ECOREG_rev,
                     p.adjust.method = "BH")

# PCT DRAINAGE Ag
tapply(dat_hi$PctAgDrainageCat,dat_hi$ECOREG_rev,max,na.rm=T)

# For Kruskal-Wallis and pairwise difference results - see revised model script
anno4 <- data.frame(xstar=c(1,2,3,4,5),ystar=c(60,60,60,60,60),
                    lab=c("a","b","c","d","e"),
                    variable=c("West","Great Plains","Midwest", "Appalachians", "Coastal Plains"))
drainag<-ggplot(dat_hi, aes(x=factor(ECOREG_rev), y = PctAgDrainageCat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(ECOREG_rev),y=PctAgDrainageCat),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,70),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=4.3)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  geom_text(data=anno4, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Tile Drain"),
    x=expression(""))
drainag


pairwise.wilcox.test(dat_hi$PctAgDrainageCat, dat_hi$ECOREG_rev,
                     p.adjust.method = "BH")

# GET LEGEND
#legend <- get_legend(drainag)

# REMOVE LEGEND
#drainag <- drainag + theme(legend.position="none")

#############
# SAVE BOXPLOTS OF HYDRO ALTERATION VARABLES MWEST AND WEST 

## By HydrAP class
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Review_figs/high_hydrap_all_eco_boxplots.tiff",
     width=6,height=9, units="in", res=400)
grid.arrange(arrangeGrob(damht,
                         urban,
                         irrag,
                         drainag,
                         nrow=4))#,
             #legend,nrow=2, heights=c(5,.25))
dev.off()


###########################
## LOW HYDRAP LAKES
table(low_hydrap$OUTLET_DAMS_red2)
#DAM NONE 
#26  527
# 5% of low HydrAP ranked lakes had dams

summary(low_hydrap$damht_zmax_full)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0210  0.0738  0.1217  0.1164  0.1609  0.1936     527 
# Dam height:zmax is relatively low

##############
## HISTOGRAMS OF DISTRIBUTIONS OF VARIABLES IN HYDRAP IN LOW VS ALL US
# Row bind low HydrAp and ALL together
low_hydrap$CLASS<-"LOW"
dat$CLASS<-"US"
dat_hi<-dat%>%
  filter(HydrAP>2)

dat_all<-rbind(low_hydrap,dat_hi)

# Represent it
LU_all <- dat_all %>%
  ggplot( aes(x=PCT_AG_URB_BSN, fill=CLASS)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw(base_size=12)+
  labs(fill="")
LU_all

damht_all <- dat_all %>%
  ggplot( aes(x=damht_zmax_full, fill=CLASS)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw(base_size=12)+
  labs(fill="")
damht_all

urb_all <- dat_all %>%
  ggplot( aes(x=PctDEVELOPED_Cat, fill=CLASS)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw(base_size=12)+
  labs(fill="")
urb_all

irrag_all <- dat_all %>%
  ggplot( aes(x=PctIrrigated.AgLandCat, fill=CLASS)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw(base_size=12)+
  labs(fill="")
irrag_all

drain_all <- dat_all %>%
  ggplot( aes(x=PctAgDrainageCat, fill=CLASS)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_bw(base_size=12)+
  labs(fill="")


# For Kruskal-Wallis and pairwise difference results - see revised model script
#anno5 <- data.frame(xstar=c(1,2,3,4,5),ystar=c(18,18,18,18,18),
#                    lab=c("a","b","c","c","b"),
#                    variable=c("West","Midwest","Great Plains", "Appalachians", "Coastal Plains"))

LU_low<-ggplot(dat_all, aes(x=factor(CLASS), y = PCT_AG_URB_CAT))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(CLASS),y=PCT_AG_URB_CAT),outlier.shape=NA)+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,70),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#69b3a2", "#404080"))+ #c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=1)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #geom_text(data=anno5, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Land use"),
    x=expression(""))
LU_low
      
dam_low<-ggplot(dat_all, aes(x=factor(CLASS), y = damht_zmax_full))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(CLASS),y=damht_zmax_full),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,10),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#69b3a2", "#404080"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=1)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #geom_text(data=anno5, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("dam ht:zmax"),
    x=expression(""))
dam_low

urb_low<-ggplot(dat_all, aes(x=factor(CLASS), y = PctDEVELOPED_Cat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(CLASS),y=PctDEVELOPED_Cat),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,50),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=1)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #geom_text(data=anno5, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Urban"),
    x=expression(""))
urb_low
summary(low_hydrap$PctDEVELOPED_Cat)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.915   1.145  57.403

ag_low<-ggplot(dat_all, aes(x=factor(CLASS), y = PctAGR_Cat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(CLASS),y=PctAGR_Cat),outlier.shape=NA)+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,100),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=1)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #geom_text(data=anno5, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Agriculture"),
    x=expression(""))
ag_low
summary(low_hydrap$PctAGR_Cat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.5172 10.0078 16.8055 91.6075

drainag_low<-ggplot(dat_all, aes(x=factor(CLASS), y = PctAgDrainageCat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(CLASS),y=PctAgDrainageCat),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,100),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=1)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #geom_text(data=anno5, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Tile Drain"),
    x=expression(""))
drainag_low
summary(low_hydrap$PctAgDrainageCat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.5609  0.0347 14.9560

irrag_low<-ggplot(dat_all, aes(x=factor(CLASS), y = PctIrrigated.AgLandCat))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(CLASS),y=PctIrrigated.AgLandCat),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,100),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3)+#,aes(color=factor(OUTLET_DAMS_red2)))+
  stat_compare_means(size=2.5,family="RMN",label.x=1)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #geom_text(data=anno5, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("% Irrigated Agriculture"),
    x=expression(""))
irrag_low
summary(low_hydrap$PctIrrigated.AgLandCat)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.3634  0.0000 13.2711


###############
## SAVE PLOTS
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Review_figs/low_hydrap_LU_boxplots.tiff",
     width=4,height=7, units="in", res=200)
grid.arrange(arrangeGrob(dam_low,
                         LU_low,
                         nrow=2))#,
#legend,nrow=2, heights=c(5,.25))
dev.off()

plot(low_hydrap$L_VertDD_use~low_hydrap$damht_zmax_full)
abline(lm(low_hydrap$L_VertDD_use~low_hydrap$damht_zmax_full))
# Dam ht and vert low
damht_vert_low<-ggplot(dat, aes(x=damht_zmax_full, y=L_VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=2)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_regline_equation(aes(family="RMN"), label.x=10, label.y=1.2) +
  stat_cor(aes(family="RMN",label=..rr.label..), label.x=10, label.y=1)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        #text = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("damht:zmax"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

damht_vert_low

# LANDUSE and vert low
LU_vert_low<-ggplot(dat, aes(x=PctDEVELOPED_Cat, y=L_VertDD_use))+ #PCT_AG_URB_CAT
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=2)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_regline_equation(aes(family="RMN"), label.x=0, label.y=1.2) +
  stat_cor(aes(family="RMN",label=..rr.label..), label.x=0, label.y=1)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        #text = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("% Land use"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

LU_vert_low

# LANDUSE and EI low
LU_EI_low<-ggplot(dat, aes(x=PCT_AG_URB_CAT, y=E_I))+ #PCT_AG_URB_CAT
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  #scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=2)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_regline_equation(aes(family="RMN"), label.x=0, label.y=1.2) +
  stat_cor(aes(family="RMN",label=..rr.label..), label.x=0, label.y=1)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        #text = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("% Land use"), #Grand mean scaled 
       y=expression("evaporation:inflow"))#

LU_EI_low

# Boxplots of lake level decline and dam presence in LOW HYDRAP lakes
vert_dam_low<-ggplot(dat_all, aes(x=factor(OUTLET_DAMS_red2), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,50),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~CLASS,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"), #,#
        axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Vertical decline (m)"))

vert_dam_low


horiz_dam_low<-ggplot(dat_all, aes(x=factor(OUTLET_DAMS_red2), y = HorizDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,550),labels=function(x) format(x,scientific = FALSE))+
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~CLASS,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"), #,#
        axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Horizontal decline (m)"))

horiz_dam_low

ei_dam_low<-ggplot(dat_all, aes(x=factor(OUTLET_DAMS_red2), y = E_I))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=E_I),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,1.00),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#ef8a62","#67a9cf"))+#c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~CLASS,ncol=2)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text.x = element_blank(),#element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    y=expression("Evaporation:Inflow"),
    x=expression("Dam presence"))

ei_dam_low

##################
##HISTOGRAMS OF HUMAN VS HYDRAP IN LOW RANKED LAKES
LU_low <- low_hydrap %>%
  ggplot( aes(x=PCT_AG_URB_BSN, fill=factor(HydrAP))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#fdc086", "#beaed4","#7fc97f")) +
  theme_bw(base_size=12)+
  labs(fill="")
LU_low

damht_low <- low_hydrap %>%
  ggplot( aes(x=damht_zmax_full, fill=factor(HydrAP))) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#7fc97f")) +
  theme_bw(base_size=12)+
  labs(fill="")
damht_low

#################
## SUBSET LOW HYDRAP WITH LANDUSE
low_hydrap_lu<-low_hydrap%>%
  filter(PCT_AG_URB_CAT>0)
summary(low_hydrap_lu$PCT_AG_URB_CAT)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02445  3.22407 13.78882 18.46943 31.10893 91.60746 

cor.test(low_hydrap$L_DpthMx_mod,low_hydrap$L_inflow)
plot(low_hydrap$L_DpthMx_mod,low_hydrap$L_inflow)
abline(lm(low_hydrap$L_inflow~low_hydrap$L_DpthMx_mod))
