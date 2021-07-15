#######################
## INTERACTION PLOTS
##  EFFECT SIZE CURVE USING JOHN GELDHOF'S Spreadsheet
##  Simplified biplot of VertDD ~ PHDI by Hydrap class
##  Will combine for a conceptual figure showing interaction
##
## 7/9/21
#######################
remove(list=ls())

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(cowplot)
library(GGally)
library(ggpubr)
library(tidyr)
library(FSA)

#############
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
dat<-dat %>% replace_na(list(hydro_dist = "CONUS"))

table(dat$hydro_dist)
#Minimal   CONUS 
#   553    1163 

#################
## CREATE HYDRAP BINS
## Follows bins from HydrAP paper (Ecological Indicators 2021)
dat$hydrap_bin <- dat$HydrAP

dat$hydrap_bin[dat$HydrAP <(3)]<- "Low"
dat$hydrap_bin[dat$HydrAP >=3 & dat$HydrAP <=5] <- "Moderate"
dat$hydrap_bin[dat$HydrAP > 5] <- "High"

# Bin PHDI values into drought, normal, and wet conditions
dat$PHDI_bin <- dat$PHDI
dat$PHDI_bin[dat$PHDI < (-0.5)] <- "drought"
dat$PHDI_bin[dat$PHDI >=(-0.5) & dat$PHDI <= 0.5] <- "normal"
dat$PHDI_bin[dat$PHDI > (0.5)] <- "wet"
table(dat$PHDI_bin)

## GRAND MEAN SCALED 30 yr normal precipitation
dat$Precip8110Ws_grd_sc <- scale(dat$Precip8110Ws, scale=T)

## GROUP MEAN SCALED 30 yr normal PRECIPITATION BY ECOREGION (scale = centers by mean and divides by std dev within an ecoregion group)
dat <- dat %>%
  group_by(ECOREG_rev) %>%
  mutate(Precip8110Ws_grp_sc = scale(Precip8110Ws, scale=TRUE)) # scaling is the default but just in case
summary(dat$Precip8110Ws_grp_sc)

###########
## ORDER CATEGORIES
# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                 "Appalachians","Coastal Plains"))
# ORDER HYDRAP BINS
dat$hydrap_bin <- ordered(dat$hydrap_bin, levels=c("Low", "Moderate", "High"))

#####################
## MIDWEST ORIGINAL DATA n= 482
#########
mwest<-dat %>%
  filter(ECOREG_rev=="Midwest")

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman


############
## MIDWEST - SIMPLIFIED INTERACTION PLOT (ONLY LOW AND HIGH HYDRAP)
# https://cmdlinetips.com/2021/05/tips-to-customize-text-color-font-size-in-ggplot2-with-element_text/#plotcaption
# Remove lakes missing HydrAP ranks
mwest_mod <- mwest %>%
  drop_na(HydrAP)

# Only Low and High HydrAP lakes
mwest_mod <- mwest_mod %>%
  filter(hydrap_bin=="Low" | hydrap_bin =="High")

simple_graph_mwest<-ggplot(mwest_mod, aes(x= PHDI, y=VertDD_use, color=hydrap_bin,linetype=hydrap_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,0.5),labels=function(x) format(x,scientific = FALSE))+
  xlim(-4, 4)+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+#
  #scale_linetype_manual(values=c("dashed","solid"))+
  scale_color_manual( values=c("#4682b4", "#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position=c(0.5,0.86),
        legend.direction="horizontal",
        plot.tag=element_text(family="RMN"))+
        #legend.position="bottom")+
  #annotate("text", x=c(-3,3.5),y=0.08,label=c("Drought","Wet"), family="RMN", size=5)+
  annotate("text",x=c(-3,3),y=c(0.28,0.10),label=c("More decline","Less decline"),family="RMN",color="#4682b4")+
  annotate("text",x=c(-3,3),y=c(0.115,0.28),label=c("Less decline","More decline"),family="RMN",color="#b4464b")+
  #annotate("text", x=c(3,3),y=c(0.10,0.28),label=c("Less decline","More decline"), family="RMN")+
  labs(x=expression("PHDI"), #
       y=expression("Vertical decline (m)"),
       color="HydrAP class",
       linetype="HydrAP class",
       tag="b.")#,



###########################
## EFFECT SIZE CURVE
###########################
## LOAD DATA
#   Compiled estimated partial total effects of vertdd ~ HydrAP given a level of PHDI
##   copied values from John's excel spreadsheet

# MIDWEST Vertical decline HydrAP relationships vs. PHDI
peff <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/partial_total/phdi_vertdd_midwest.csv")

# EFFECT SIZE CURVE
effect_size_mwest<-ggplot(peff, aes(x= PHDI, y=partial_total))+ #
  #scale_y_continuous(trans="log10",limits=c(NA,0.5),labels=function(x) format(x,scientific = FALSE))+
  xlim(-4, 4)+
  #geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, color="black")+
  #xlim(NA,350)+
  #scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  #stat_cor(aes(color=hydrap_bin, family="RMN"), label.x=1)+ 
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_blank(),#element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_blank(),#axis.title.x=element_text(family="RMN"),
        axis.ticks=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        #plot.margin=margin(3,3,3,15),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom",
        plot.tag=element_text(family="RMN"))+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("PHDI"), #
       y=expression("Total effect HydrAP~vertical"),
       tag="a.")#,


##########
## SAVE MIDWEST INTERACTION PLOTS TOGETHER

tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/stacked_interaction_plot_MIDWEST.tiff",
     width=5, height=6, units="in", res=400)
grid.arrange(arrangeGrob(effect_size_mwest,
                         simple_graph_mwest,
                         nrow=2))
#grid.arrange(west_pct_eff_stacked_bar, midwest_pct_eff_stacked_bar, legend, ncol=2, nrow=2, widths=c(3.0, 2.8))
dev.off()


