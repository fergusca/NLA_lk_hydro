####################
#####################
## FIGURES FOR DRIVERS OF LAKE HYDROLOGY for OREGON LAKES ASSOCATION 2021 MEETING
##
## Plotting the total effects of the Interaction model with rescaled PHDI by category class and by individual predictor

## 11/1/21 - rescaled PHDI for easier interpretation for OLA presentation 2021

# If have issue with plotting - may need to create temp folder where figures are stored in Rstudio - see error msg
#####################

###############################

remove(list=ls())

library(dplyr)
library(ggplot2)
#library(grid)
library(gridExtra)
#library(lattice)
library(cowplot)
library(GGally)
library(ggpubr)
library(tidyr)

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# SET COLOR PALATE FOR BY DRIVER CATEGORY
# For barchars with individual predictors
cbPalette <- c("#E69F00","#999999","#bdbdbd","#d9d9d9","#56B4E9","#F0E442","#009E73")# interaction= "#009E73")
cbPalette_EI <- c("#E69F00","#d9d9d9","#56B4E9","#F0E442","#009E73")# interaction= "#009E73")
cbPalette_vert <- c("#E69F00","#bdbdbd","#d9d9d9","#56B4E9","#F0E442","#009E73")# interaction= "#009E73")

cbPalette_reduced <- c("#E69F00","#999999","#56B4E9","#F0E442","#009E73") # interaction ="#009E73")


#######################
## LOAD DATA JUST WEST AND MIDWEST FOR COMPARISON
#################
## ECOREGION - HAND COMPILED TOTAL EFFECTS (Standardized with Std error) from SEM model output with bootstrap sampling
teff_ecoreg <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/compiled_total_effects/ecoregion_total_effects_scaled_PHDI.csv")

names(teff_ecoreg)
table(teff_ecoreg$predictor)

# RELABEL PREDICTOR VARIABLES
teff_ecoreg$predictor_2<-teff_ecoreg$predictor
teff_ecoreg <- teff_ecoreg%>%
  mutate(predictor_2= recode(predictor_2,
                             bffFlat_grad="flatness", BFIWs="baseflow", HydrAP="HydrAP",L_DpthMx_mod="depth",
                             L_EI="EI", L_inflow="inflow",L_LkAreakm2="area", Precip_mm_total_yr_sc="precip",
                             L_SLD="shoreline", L_VertDD_use="vert", PHDI="PHDI", Temp_degC_avg_yr="temp",
                             HydrAP_PHDI="HydrAPxPHDI"))

# ORDER PREDICTOR VARIABLES
teff_ecoreg$predictor_2 <- ordered(teff_ecoreg$predictor_2, levels=c("area","depth","shoreline","flatness","vert","EI","inflow","baseflow",
                                                                     "temp","precip","PHDI","HydrAP","HydrAPxPHDI"))
table(teff_ecoreg$predictor_2)

# CREATE CATEGORIES
teff_ecoreg$category <-teff_ecoreg$predictor
teff_ecoreg <- teff_ecoreg %>%
  mutate(category = recode(category,
                           L_LkAreakm2="morphometry", L_DpthMx_mod="morphometry", L_SLD="morphometry", bffFlat_grad="morphometry",
                           L_inflow="hydrology",BFIWs="hydrology",
                           L_VertDD_use="vertical", L_EI = "EI",
                           Temp_degC_avg_yr="climate", Precip_mm_total_yr_sc="climate", PHDI="climate",
                           HydrAP="human",HydrAP_PHDI="interaction"))

teff_ecoreg$category <- ordered(teff_ecoreg$category, levels=c("morphometry","vertical","EI","hydrology","climate","human","interaction"))
table(teff_ecoreg$category)


# RENAME RESPONSE
teff_ecoreg$response_2 <-teff_ecoreg$response
teff_ecoreg <- teff_ecoreg %>%
  mutate(response_2 = recode(response_2,
                             L_EI="EI", L_HorizDD_use="Horizontal", L_VertDD_use="Vertical"))

teff_ecoreg$response_2 <- ordered(teff_ecoreg$response_2, levels=c("EI","Vertical", "Horizontal"))
table(teff_ecoreg$response_2)

# Order Ecoregions
#teff_ecoreg$Ecoregion<-ordered(teff_ecoreg$Ecoregion, levels=c("WEST","MIDWEST"))
#table(teff_ecoreg$Ecoregion)

######################
## TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_ecoreg_total <- teff_ecoreg %>%
  filter(effect == "total")

## SUBSET BY ECOREGIONS FOR MS
## WEST & MIDWEST
teff_west_mwest <- teff_ecoreg_total %>%
  filter(ecoregion == "WEST" | ecoregion == "MIDWEST") %>%
  droplevels()

# Order Ecoregions
teff_west_mwest$ecoregion<-ordered(teff_west_mwest$ecoregion, levels=c("WEST","MIDWEST"))
table(teff_west_mwest$ecoregion)


#################
## BARCHART INDIVIDUAL PREDICTORS: WEST & MIDWEST 
# SUBSET DATA BY RESPONSE VARIABLES
teff_west_mwest_EI <- teff_west_mwest %>%
  filter(response_2 == "EI")%>%
  droplevels()

teff_west_mwest_vert <- teff_west_mwest %>%
  filter(response_2 == "Vertical")%>%
  droplevels()

teff_west_mwest_horiz <- teff_west_mwest %>%
  filter(response_2 == "Horizontal")%>%
  droplevels()


## WEST & MIDWEST E:I INDIVIDUAL TOTAL EFFECTS 
EI_mw<-ggplot(teff_west_mwest_EI ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette_EI, drop=TRUE)+
  ylim(-0.7,0.9)+
  facet_grid(vars(teff_west_mwest_EI$ecoregion), vars(teff_west_mwest_EI$response_2))+
  #facet_wrap(teff_west_mwest_EI$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",3),rep("#999999",2),rep("#56B4E9",3),rep("#F0E442",1),rep("#009E73",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        strip.text.x = element_text(family="RMN", size=12),
        strip.text.y= element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

## WEST & MIDWEST VERTICAL INDIVIDUAL TOTAL EFFECTS
vert_mw<-ggplot(teff_west_mwest_vert ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette_vert, drop=TRUE)+
  ylim(-0.7,0.9)+
  facet_grid(vars(teff_west_mwest_vert$ecoregion), vars(teff_west_mwest_vert$response_2))+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",3),rep("#56B4E9",3),rep("#F0E442",1),rep("#009E73",1))),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), #element_text(family = "RMN"),
        axis.title.y = element_blank(), #axis.title.y=element_text(family="RMN"),
        strip.text.x = element_text(family="RMN", size=12),
        strip.text.y= element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

## WEST & MIDWEST HORIZONTAL INDIVIDUAL TOTAL EFFECTS
horiz_mw<-ggplot(teff_west_mwest_horiz ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.6,0.9)+
  facet_grid(vars(teff_west_mwest_horiz$ecoregion), vars(teff_west_mwest_horiz$response_2))+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",4),rep("#56B4E9",3),rep("#F0E442",1),rep("#009E73",1))),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), #element_text(family = "RMN"),
        axis.title.y = element_blank(), #axis.title.y=element_text(family="RMN"),
        strip.text.x = element_text(family="RMN", size=12),
        strip.text.y= element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

# GET LEGEND
legend <- get_legend(EI_mw)

# REMOVE LEGEND
EI_mw <- EI_mw + theme(legend.position="none")

## WEST & MIDWEST - INDIVIDUAL STD TOTAL EFFECTS
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/figures/WEST_MWEST_std_total_effects_for_OLA21.tiff",
     width=7.5, height=8, units="in", res=400)
grid.arrange(arrangeGrob(EI_mw,
                         vert_mw,
                         horiz_mw,
                         ncol=3),
             legend,nrow=2,heights=c(3.5, .5))
dev.off()

