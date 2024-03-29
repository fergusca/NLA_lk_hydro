########################
## EXPLORATORY ANALYSIS FOR DISCUSSION
## DRIVERS OF LAKE HYDROLOGY MS
##
## 7/27/21
########################
remove(list=ls())

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)
library(tidyverse)
library(ggpubr) # statistics for comparison
#library(FSA)

##################
## LOAD DATA
##################
# CONUS LOW HYDRAP (0 - 2) n = 553 lakes
low_hydrap <-read_csv("data_processed/conus_low_hydrap.csv")
todrop<-names(low_hydrap)%in%c("X1","X1_1") 
low_hydrap<-low_hydrap[!todrop]

# By ECOREGION
west <-read_csv("data_processed/west.csv") #n = 429
mwest <- read_csv("data_processed/mwest.csv") #n = 482 #~/NLA_hydro/NLA_hydro_driver/
gplains <- read_csv("data_processed/gplains.csv") #n = 292
apps <- read_csv("data_processed/apps.csv") #n = 324
cstplains <- read_csv("data_processed/cstplains.csv") #n = 189

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

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman


#################################
## LOOK AT ANTHROPOGENIC CHARACTERISTICS OF LAKES IN WEST AND MIDWEST
table(west$OUTLET_DAMS_red2)
#DAM NONE 
#267  162

table(mwest$OUTLET_DAMS_red2)
#DAM NONE 
#186  296 

hist(west$PctIrrigated.AgLandCat_logit)
hist(mwest$PctIrrigated.AgLandCat_logit)

hist(west$PctAgDrainageCat_logit)
hist(mwest$PctAgDrainageCat_logit)

hist(west$PctDEVELOPED_Cat_logit)
hist(mwest$PctDEVELOPED_Cat_logit)

# DAM PURPOSES
table(west$purpose_red)
table(mwest$purpose_red)

# REDUCE DAM PURPOSE INTO CLASSES BASED ON EXPECTED WATER CHANGES
west$dam_purp <- west$purpose_red
mwest$dam_purp <- mwest$purpose_red


west <- west%>%
  mutate(dam_purp= recode(dam_purp,
                             I="release",H="release",C="release",S="release", R="store",N="store",F="store",P="store",
                             O="other",T="other"))

mwest <- mwest%>%
  mutate(dam_purp= recode(dam_purp,
                          I="release",H="release",C="release",S="release", R="store",N="store",F="store",P="store",
                          O="other",T="other"))

table(west$dam_purp)
#other release   store 
#4     174      23 

table(mwest$dam_purp)
#other release   store 
#9      46      75 

# CONCLUSIONS - West has more dams with purposes that are likely to release or consume water (Irrigation, Hydroelectric, Flood control, Water supply)
# Compared to the Midwest that has more dams related to storage of water (Recreation, Navigation, Fish and Wildlife Pond, and Farm pond)
##############################
## LOOK AT ANTHROPOGENIC VARIABLES RELATED TO DECLINE

# WEST
west_vert_dam<-ggplot(west, aes(x=factor(OUTLET_DAMS_red2), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,46),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #stat_pvalue_manual(dat, hide.ns = TRUE)+
  #geom_text(data=anno, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #labs(subtitle = get_test_label(res.kruskal, detail=TRUE),
  #     caption = get_dat_label(dat))+
  #facet_wrap(~ECOREG_rev,ncol=5)+
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
    y=expression("Vertical decline (m)"),
    x=expression("Dam presence"))


#############
# MIDWEST
mwest_vert_dam<-ggplot(mwest, aes(x=factor(OUTLET_DAMS_red2), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,46),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #stat_pvalue_manual(dat, hide.ns = TRUE)+
  #geom_text(data=anno, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #labs(subtitle = get_test_label(res.kruskal, detail=TRUE),
  #     caption = get_dat_label(dat))+
  #facet_wrap(~ECOREG_rev,ncol=5)+
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
    y=expression("Vertical decline (m)"),
    x=expression("Dam presence"))

#############
# G PLAINS
gplains_vert_dam<-ggplot(gplains, aes(x=factor(OUTLET_DAMS_red2), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(OUTLET_DAMS_red2),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,46),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #stat_pvalue_manual(dat, hide.ns = TRUE)+
  #geom_text(data=anno, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #labs(subtitle = get_test_label(res.kruskal, detail=TRUE),
  #     caption = get_dat_label(dat))+
  #facet_wrap(~ECOREG_rev,ncol=5)+
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
    y=expression("Vertical decline (m)"),
    x=expression("Dam presence"))

###########
## BOXPLOTS
west_vert_dam

mwest_vert_dam

# CONCLUSION - lakes with dams have greater vertical decline in the WEST but no difference in the midwest


# MIDWEST distribution of urban development and HydrAP
mwest_vert_dam<-ggplot(mwest, aes(x=factor(HydrAP), y = PctDEVELOPED_Cat_logit))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(HydrAP),y=PctDEVELOPED_Cat_logit),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,46),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #stat_pvalue_manual(dat, hide.ns = TRUE)+
  #geom_text(data=anno, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #labs(subtitle = get_test_label(res.kruskal, detail=TRUE),
  #     caption = get_dat_label(dat))+
  #facet_wrap(~ECOREG_rev,ncol=5)+
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
    y=expression("Urban logit"),
    x=expression("HydrAP"))



####################
## BIPLOTS
# DRAINAGE AGRICULTURE
west_drain_vert<-ggplot(west, aes(x=PctAgDrainageCat_logit, y=VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
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
  labs(x=expression("Logit drainage ag"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

# MIDWEST
mwest_drain_vert<-ggplot(mwest, aes(x=PctAgDrainageCat_logit, y=VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
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
  labs(x=expression("Logit drainage ag"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

##########
#Biplot drainage ag ~ vert decline
# WEST
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Biplot_daing_ag_vert_west.tiff",
     width=7.5, height=4, units="in", res=400)
west_drain_vert
dev.off()

#MIDWEST
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Biplot_daing_ag_vert_mwest.tiff",
     width=7.5, height=4, units="in", res=400)
mwest_drain_vert
dev.off()

# CONCLUSIONS
# west & midwest - no correlation of vertical decline with drainage agriculture


##########################
## URBAN CATCHMENT BIPLOTS
# WEST
west_urban_vert<-ggplot(west, aes(x=PctDEVELOPED_Cat_logit, y=VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
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
  labs(x=expression("Logit urban"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

# MIDWEST
mwest_urban_vert<-ggplot(mwest, aes(x=PctDEVELOPED_Cat_logit, y=VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
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
  labs(x=expression("Logit urban"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

##########
## SAVE BIPLOTS
#WEST
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Biplot_urban_vert_west.tiff",
     width=7.5, height=4, units="in", res=400)
west_urban_vert
dev.off()

#MIDWEST
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/Biplot_urban_vert_mwest.tiff",
     width=7.5, height=4, units="in", res=400)
mwest_urban_vert
dev.off()

# CONCLUSIONS
# WEST - no correlation between urban development and vertical decline (r = 0.01)
# MIDWEST - negative correlation between urban development and decline (r = -0.26)

##################
## IRRIGATED AGRICULTURE
# WEST
west_irrag_vert<-ggplot(west, aes(x=PctIrrigated.AgLandCat_logit, y=VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
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
  labs(x=expression("Logit irrigated agriculture"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

# WEST
mwest_irrag_vert<-ggplot(mwest, aes(x=PctIrrigated.AgLandCat_logit, y=VertDD_use))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  #xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(family="RMN", label = ..r.label..), label.x=-4, size=3)+
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
  labs(x=expression("Logit irrigated agriculture"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

# BIPLOTS
west_irrag_vert
mwest_irrag_vert

# CONCLUSIONS 
# WEST - irrigated agriculture positive correlation with vertical decline - but no strong correlation in the Midwest