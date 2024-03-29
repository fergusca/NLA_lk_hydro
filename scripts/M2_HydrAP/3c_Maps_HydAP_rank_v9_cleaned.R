###################################
## LAKE HYDRO-ALTER POTENTIAL RANKING v.8
##
## MAPS OF RANKINGS IN US ACROSS 5 ECOREGIONS
##
##
## 4/24/20

## 5/12/20 - modified ranking v6 - ignores drainage seepage lake types
## 5/28/20 - Modified ranking v6 with artificial agr drainage
## 6/2/20  - Modified ranking v7 with different LU threshold for lakes without dams
## 6/9/20  - Modified ranking v8 with different damht/zmax thresholds
## 7/1/20  - Modified ranking v9 with irrigated agriculture and redistributed bins
## 7/7/20  - Modified ranking v9 with irrigated ag moved up
## 7/17/20 - Marc added code mapping modified ecoregion boundaries
## 8/18/20 - Dropped estimated dam height from ranks


###################################

remove(list=ls())

library(maps)
library(mapdata)
library(scales)
library(maptools)
library(ggplot2)
library(ggmap)
library(dplyr)

library(sf)
library(readr)

library("rnaturalearth")
library("rnaturalearthdata")

#####################
## LOAD DATA
# ALL Lk HydAP lakes >=1ha n = 2066 v.9
d<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA0712_1ha_opt1_HydAP_v9_RESAMPLED.csv")
todrop<-names(d)%in%c("X","X.2") 
d<-d[!todrop]
#dat<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA0712_1ha_opt1_HydAP_v9.csv") # Sent to Marc "NLA0712_1ha_opt1_HydAP_v9_REDUCED.csv"


#############################
## CREATE DATASETS WITH UNIQUE OBSERVATIONS (DROPPING RESAMPLED LAKES FROM ONE OF SURVEY YRS)
#############
## OPTION 1) NLA07 (full) + unique NLA12 lakes n=1716 >=1ha ; >=4ha 1629
## DIVIDE FULL DATASET BY YEAR AGAIN
nla07_b <- d %>%
  filter(YEAR==2007)

nla12_b <- d %>%
  filter(YEAR==2012)

## SUBSET OF UNIQUE NLA12 lakes n=688 lakes  in 2012
nla12_uniq_b<-nla12_b[!nla12_b$SID %in% nla07_b$SID,c(1:471)]#include all columns

# SUBSET OF UNIQUE NLA07 lakes n=678 lakes in 2007
nla07_uniq_b<-nla07_b[!nla07_b$SID %in% nla12_b$SID,c(1:471)]

# ROW COMBINE DATASETS (all 2007 + unique 2012)
dat<-rbind(nla07_b, nla12_uniq_b) 
length(unique(dat$SID)) #1716 
dat<-dat[order(dat$SITE_ID),]
table(dat$YEAR)
#2007 2012 
#1028  688 (all lakes) 601 (>=4ha)
table(dat$RESAMPLED12_b)

## MAP
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
## GRAB WORLD DATA
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# GRAB STATE POLYGONS
state<-map_data("state")

## READ SHAPEFILE - copied and pasted ecoregion 9 shapefile on laptop
eco <-read_sf("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/GIS/ECOREG_9/Ecoregion_9.shp")

eco<-eco %>%
  mutate(ECO_5 = case_when (
    (WSA_9 %in% c('WMT','XER'))~'West',
    (WSA_9 %in% c('NPL','SPL'))~'Great Plains',
    (WSA_9 %in% c('UMW','TPL'))~'Midwest',
    (WSA_9 %in% c('NAP','SAP'))~'Appalachians',
    (WSA_9 %in% c('CPL'))~'Coastal Plains')) %>%
  group_by(ECO_5) %>%
  summarise()

###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

dat$hap_rank_9_f<-as.factor(dat$hap_rank_9)

################
## MAKE MAP
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
###############
eco<- st_transform(eco, 4269)
# Order ecoregions for gray color assignment
eco$ECO_5<-ordered(eco$ECO_5, levels=c("Great Plains","Appalachians","West","Midwest",
                                                     "Coastal Plains"))


#NLA sites
sites <-st_as_sf(dat, coords = c("LONdd_use","LATdd_use"),
                 crs=4269)

# State outlines
states<-st_as_sf(map("state",plot=FALSE, fill=TRUE, color="gray"))

# Ecoregion outlines
#ecoreg<-st_as_sf(eco,)
#library(ggrepel)

map_rank <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco, aes(fill=factor(eco$ECO_5))) +
  scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  #scale_fill_manual(values=c("grey89","grey89","grey99","grey99","grey80","grey80","grey85","grey85","grey92"))+ #
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites, aes(colour=sites$hap_rank_9_f))+
  scale_color_manual(values= c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "#404040")+ #"gray"
  geom_sf_label(data=eco, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title=element_text(family = "RMN"), # element_blank()
        #panel.border=element_blank(),#
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom")+
  labs(color = "HydrAP rank")+
  ggtitle("") #"Hydrologic alteration potential"
  
# colors http://sape.inf.usi.ch/quick-reference/ggplot2/colour
# Ecoregion scales of gray
#col_list<-c("grey85","grey85","grey80","grey80","grey99","grey99","grey86","grey86","grey92")

##
map_eco <-ggplot(data=world, color="gray90")+
  geom_sf()+
  geom_sf(data=eco, aes(fill=factor(eco$ECO_5))) +
  scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  #scale_fill_manual(values=c("grey89","grey89","grey99","grey99","grey80","grey80","grey85","grey85","grey92"))+ #
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites, aes(shape=sites$Lake_Origin_mod))+
  #scale_color_manual(values= c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "#404040")+ #"gray"
  #geom_label_repel(data=eco,aes(label=ECO_5, family="RMN"))+
  geom_sf_label(data=eco, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title= element_blank(), #  element_text(family = "RMN")
        #panel.border=element_blank(),#element_blank()
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom")+
  labs(color = "HydrAP rank")+
  ggtitle("") #"Hydrologic alteration potential"





## OPTION 1 (Full nla07 + unique nla12)
map_rank_old<- ggplot(data=world,color="gray90")+
  geom_sf()+
  geom_point(data=dat, aes(x=LONdd_use, y=LATdd_use,
                           color=hap_rank_9_f))+
  geom_sf_label(data=eco, aes(label=ECO_5,family="RMN",label.size=0.5),show.legend=F)+
  #geom_sf(data=eco, aes(group=ECO_5, fill=factor(ECO_5), color="black")) + #fill=factor(ECO_5),
  geom_sf(data=eco,fill=NA, color="black")+ #   fill=factor(ECO_5), 
  #scale_fill_manual(values=c("grey89","grey89","grey80","grey80","grey99","grey99","grey85","grey85","grey92"))+
  #scale_fill_manual(values=col_list)+
  geom_polygon(data=state, aes (x=long, y=lat, group=group), fill = NA, color="gray")+
  coord_sf(xlim = c(-127,-65), ylim= c(23, 51.0), expand = FALSE)+
  #coord_fixed (xlim=c(-114,-82.4), ylim=c(28.2,49.0))+#(1.3)+
  #scale_colour_gradient(low="yellow", high="red", na.value = "gray")+ # na.value=NA
  scale_color_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "gray")+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title=element_text(family = "RMN"), # element_blank()
        #panel.border=element_blank(),#element_blank()
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "bottom")+
  labs(color = "HydrAP rank")+
  ggtitle("") #"Hydrologic alteration potential"




#PRINT MAP
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/rank_v9/map_US_opt1_5ECOREG_rank_v9.tiff",
     width=7.5, height=6, units="in", res=300)
map_rank
dev.off()


#PRINT MAP ECOREGIONS
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/rank_v9/map_US_opt1_5ECOREG.tiff",
     width=6.5, height=5, units="in", res=300)
map_eco
dev.off()

