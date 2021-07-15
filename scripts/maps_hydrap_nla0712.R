########################
## MAPS FOR DRIVERS OF LAKE HYDROLOGY MS
##
## 5/13/21
########################
remove(list=ls())


library(ggplot2)
library(ggmap)
library(dplyr)
library(gridExtra)
library(cowplot)
library(tidyr)

library(maps)
library(mapdata)
library(scales)
library(maptools)

library(sf)
library(readr)

library("rnaturalearth")
library("rnaturalearthdata")

###########
## Plot specifications
###########
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

################
## LOAD DATA
# CONUS ALL LAKES n = 1716
dat <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/conus_NLA_opt1_1716.csv")
todrop<-names(dat)%in%c("X1","X1_1") 
dat<-dat[!todrop]

## CREATE DATASET WITH CATEGORICAL VARIABLE INDICATING WHETHER LOW HYDRO_ALT LAKE OR NOT 
dat$hydro_dist <-dat$HydrAP
dat$hydro_dist <-as.factor(dat$hydro_dist)
str(dat$hydro_dist)

# Create SUBGROUPS FOR LAKES WITH MIN HYDRO_ALTERAITO DISTURBANCES AND ALL OTHERS
levels(dat$hydro_dist) <- list("Minimal" = c("0","1","2"), "CONUS" = c("3","4","5","6","7"))
dat<-dat %>% replace_na(list(hydro_dist = "CONUS")) # will fill hydro_dist with NA to be labeled as "CONUS"

##################
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



################
## MAKE MAP
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
###############
eco<- st_transform(eco, 4269)
# Order ecoregions for gray color assignment
eco$ECO_5<-ordered(eco$ECO_5, levels=c("West", "Great Plains","Midwest",
                                       "Appalachians","Coastal Plains"))

dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West", "Great Plains","Midwest",
                                                 "Appalachians","Coastal Plains"))

eco_reduced <-eco %>%
  filter(ECO_5 == "West"|ECO_5 == "Midwest")

#eco$ECO_5<-ordered(eco$ECO_5, levels=c("Great Plains","Appalachians","West","Midwest",
#                                       "Coastal Plains"))

# MAKE HYDRAP A FACTOR
dat$HydrAP_f<-as.factor(dat$HydrAP)

# NLA sites with minimal hydro-alteration
min_hydro<- dat %>%
  filter(hydro_dist=="Minimal")

# NLA sites in West and Midwest
eco_subset <- dat %>%
  filter(ECOREG_rev=="West"|ECOREG_rev=="Midwest")

#############
# ALL NLA sites
sites <-st_as_sf(dat, coords = c("LONdd_use","LATdd_use"),
                 crs=4269)

# Min hydro alteration NLA sites
sites_min <-st_as_sf(min_hydro, coords = c("LONdd_use","LATdd_use"),
                     crs=4269)

# WEST & MIDWEST NLA sites
sites_eco <-st_as_sf(eco_subset, coords = c("LONdd_use","LATdd_use"),
                     crs=4269)

###############
# State outlines
states<-st_as_sf(map("state",plot=FALSE, fill=TRUE, color="gray"))


#####################
## MAP MIN HYDRO-ALTERATION LAKES in CONUS n = 553 
#####################

map_min <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco, aes(fill=factor(eco$ECO_5))) +
  scale_fill_manual(values=c("#bdbdbd","#969696","#d9d9d9","#737373","#f0f0f0"))+
  #scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites_min, aes(colour=sites_min$HydrAP_f))+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  scale_color_manual(values= c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "#404040")+ #"gray"
  #geom_sf_label(data=eco, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        #axis.text=element_text(family = "RMN"),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(), # remove lat long values
        axis.text.y = element_blank(),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN"),
        legend.title=element_text(family = "RMN"), # element_blank()
        #panel.border=element_blank(),#
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "none")+
  labs(color = "HydrAP rank", fill = "")+
  ggtitle("") #"Hydrologic alteration potential"


#####################
## MAP ALL CONUS n = 1716 
#####################

map_conus <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco, aes(fill=factor(eco$ECO_5))) +
  scale_fill_manual(values=c("#bdbdbd","#969696","#d9d9d9","#737373","#f0f0f0"))+
  #scale_fill_grey(start=0.3,end=0.8, guide=FALSE)+
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites, aes(colour=sites$HydrAP_f))+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  scale_color_manual(values= c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "#404040")+ #"gray"
  #geom_sf_label(data=eco, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        #axis.ticks=element_blank(),
        legend.text = element_text(family = "RMN",size=10),
        legend.title=element_text(family = "RMN"), # element_blank()
        #panel.border=element_blank(),#
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.box = "vertical",
        legend.position = "bottom")+
  labs(color = "HydrAP rank", fill = "")+
  ggtitle("") #"Hydrologic alteration potential"



#####################
## MAP WEST AND MIDWEST LAKES in CONUS n = 911 
#####################

map_west_midwest <-ggplot(data=world, color="gray90")+
  geom_sf(data=eco, aes(fill=factor(eco$ECO_5))) +
  scale_fill_manual(values=c("#bdbdbd","#ffffff","#d9d9d9","#ffffff","#ffffff"), guide=FALSE)+
  geom_sf(data=states, fill=NA, color="grey19")+
  geom_sf(data=sites_eco, aes(colour=sites_eco$HydrAP_f))+ #geom_sf(data=sites, aes(colour=sites$HydrAP_f),size = .05)+
  scale_color_manual(values= c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value = "#404040")+ #"gray"
  #geom_sf_label(data=eco_reduced, aes(label=ECO_5,family="RMN"),label.size=0.5,show.legend=F)+
  coord_sf(xlim=c(-129, -65), ylim = c(24, 51), expand = FALSE)+
  theme_bw()+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        #axis.text=element_text(family = "RMN"),
        #axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_blank(), # remove lat long values
        axis.text.y = element_blank(),
        legend.text = element_text(family = "RMN", size=12),
        legend.title=element_text(family = "RMN"), # element_blank()
        #panel.border=element_blank(),#
        # panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.background = element_rect(fill = "gray90"), #lightgray
        legend.key = element_rect(fill = "gray90", color = NA),
        legend.position = "none")+
  labs(color = "HydrAP rank")+
  ggtitle("") #"Hydrologic alteration potential"


###################
#PRINT MAPS 
## PANEL OF MINIMALLY DISTURBED AND WEST AND MIDWEST MAPS
# GET LEGEND
legend<-get_legend(map_conus)

# REMOVE LEGEND
#midwest_pct_eff_stacked_bar<-midwest_pct_eff_stacked_bar + theme(legend.position="none")

# ARRANGE MULTIPLE GRAPHS AND LEGEND - USE THIS ONE FOR MANUSCRIPT
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/map_compile_min_hydro_west_mwest.tiff",
     width=7.5, height=10, units="in", res=400)
grid.arrange(arrangeGrob(map_min + theme(legend.position = "none"),
                         map_west_midwest + theme(legend.position = "none"), nrow = 2),
             #nrow=2),
             arrangeGrob(legend, ncol=1, nrow=1),
             heights=c(4.5,1))
#grid.arrange(west_pct_eff_stacked_bar, midwest_pct_eff_stacked_bar, legend, ncol=2, nrow=2, widths=c(3.0, 2.8))
dev.off()


###########
## FULL CONUS DATASET FOR SI
## CONUS n =1716
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/map_NLA0712_1716_lks_conus.tiff",
     width=7.5, height=5, units="in", res=400)
map_conus
dev.off()

