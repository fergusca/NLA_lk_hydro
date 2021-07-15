#####################
## FIGURES FOR DRIVERS OF LAKE HYDROLOGY MS
##
## Plotting the total effects of the base model (no interaction) by category class and by individual predictor
## 7/6/21

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
#library(tidyverse) - hard to relabel - but it is possible with recode https://dplyr.tidyverse.org/reference/recode.html
#library(PMCMRplus)

#install.packages("remotes")                    # Install remotes package
#remotes::install_github("coolbutuseless/ggpattern")
#library("ggpattern")     


###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# SET COLOR PALATE FOR BY DRIVER CATEGORY
# For barchars with individual predictors
cbPalette <- c("#E69F00","#999999","#bdbdbd","#d9d9d9","#56B4E9","#F0E442")# interaction= "#009E73")
cbPalette_EI <- c("#E69F00","#d9d9d9","#56B4E9","#F0E442")# interaction= "#009E73")
cbPalette_vert <- c("#E69F00","#bdbdbd","#d9d9d9","#56B4E9","#F0E442")# interaction= "#009E73")

#cbPalette <- c("#999999", "#F0E442", "#56B4E9", "#009E73","#E69F00", "#0072B2", "#D55E00", "#CC79A7")

cbPalette_reduced <- c("#E69F00","#999999","#56B4E9","#F0E442") # interaction ="#009E73")

# REVERSE ORDER TO HAVE COLORS ACENDING UP STACKED BARCHART
cbPalette_rev <- c("#F0E442","#56B4E9","#d9d9d9","#bdbdbd","#999999","#E69F00")

#######################
## LOAD DATA
## CONUS LOW HYDRAP (n = 553)
# HAND COMPILED TOTAL EFFECTS (Standardized with Std error) from SEM model output with bootstrap sampling
teff_low <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/compiled_total_effects/conus_low_hydrap_total_effects.csv")
#teff_low <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/Compiled_total_effects_for_R/CONUS_LOW_HYDRAP_std_effects_060321.csv") # OLD 010521

names(teff_low)
table(teff_low$predictor)


## PROCESS DATA
# RELABEL PREDICTOR VARIABLES
teff_low$predictor_2<-teff_low$predictor
teff_low <- teff_low %>%
  mutate(predictor_2= recode(predictor_2,
                             bffFlat_grad="flatness", BFIWs="baseflow", HydrAP="HydrAP",L_DpthMx_mod="depth",
                             L_EI="EI", L_inflow="inflow",L_LkAreakm2="area", Precip_mm_total_yr_sc="precip",
                             L_SLD="shoreline", L_VertDD_use="vert", PHDI="PHDI", Temp_degC_avg_yr="temp"))

table(teff_low$predictor_2)

# ORDER PREDICTOR VARIABLES
teff_low$predictor_2 <- ordered(teff_low$predictor_2, levels=c("area","depth","shoreline","flatness","vert","EI","inflow","baseflow",
                                                               "temp","precip","PHDI","HydrAP"))
table(teff_low$predictor_2)


# CREATE CATEGORIES
teff_low$category <-teff_low$predictor
teff_low <- teff_low %>%
  mutate(category = recode(category,
                           L_LkAreakm2="morphometry", L_DpthMx_mod="morphometry", L_SLD="morphometry", bffFlat_grad="morphometry",
                           L_inflow="hydrology",BFIWs="hydrology",
                           L_VertDD_use="vertical", L_EI = "EI",
                           Temp_degC_avg_yr="climate", Precip_mm_total_yr_sc="climate", PHDI="climate",
                           HydrAP="human"))

teff_low$category <- ordered(teff_low$category, levels=c("morphometry","vertical","EI","hydrology","climate","human"))
table(teff_low$category)

# RENAME AND ORDER CATEGORIES
#teff_low$category_2 <-teff_low$category
#levels(teff_low$category_2) <- list(morphometry="morph", hydrology="hydro", vertical="vert", EI ="ei", climate="climate", human="human", interaction="interaction")
#teff_low$category_2 <- ordered(teff_low$category_2, levels=c("morphometry","vertical","EI","hydrology","climate","human","interaction"))
#table(teff_low$category_2)

# SIMPLIFIED CATEGORIES FOR LEGEND
teff_low$category_2 <-teff_low$category
teff_low <- teff_low %>%
  mutate(category_2 = recode(category_2,
                             morphometry="morphometry", hydrology="hydrology", 
                             vertical="hydrology", EI="hydrology", 
                             climate="climate", human="human"))

teff_low$category_2 <- ordered(teff_low$category_2, levels=c("morphometry","hydrology","climate","human"))
table(teff_low$category_2)


# RENAME RESPONSE
teff_low$response_2 <-teff_low$response
teff_low <- teff_low %>%
  mutate(response_2 = recode(response_2,
                             L_EI="EI", L_HorizDD_use="Horizontal", L_VertDD_use="Vertical"))
#levels(teff_low$response_2) <- list(EI="L_EI", Horizontal="L_HorizDD_use", Vertical="L_VertDD_use")
teff_low$response_2 <- ordered(teff_low$response_2, levels=c("EI","Vertical", "Horizontal"))
table(teff_low$response_2)

# ORDER EFFECT
table(teff_low$effect)
teff_low$effect <- ordered(teff_low$effect, levels=c("indirect","direct","total"))

# ORDER BY RESPONSE AND DRIVER CLASS
teff_low<-teff_low[order(teff_low$response_2, teff_low$category_2),]


######################
## FIGURE OF PROPORTION OF TOTAL EFFECTS ONLY
## SUBSET DATA TO HAVE ONLY TOTAL EFFECTS
teff_total <- teff_low %>%
  filter(effect == "total")

# Set colors for x-labels - does not work with facet graphs bc will repeat from original
# https://stackoverflow.com/questions/45843759/ggplot2-coloring-axis-text-on-a-faceted-plot
#colors <- ifelse((teff_total$predictor_2 == "area"|teff_total$predictor_2 =="depth"|teff_total$predictor_2 =="shoreline"|teff_total$predictor_2 =="flatness"), "#E69F00",
#                 ifelse((teff_total$predictor_2 == "vert"|teff_total$predictor_2 == "EI"|teff_total$predictor_2 == "inflow"|teff_total$predictor_2 == "baseflow"), "#999999",
#                        ifelse((teff_total$predictor_2 == "temp"|teff_total$predictor_2 =="precip"|teff_total$predictor_2 == "PHDI"),"#56B4E9","#F0E442")))

#########################
## BARCHART CONUS LOW HYDRAP FOR INDIVIDUAL PREDICTORS
##########
# Individual plots by response
teff_total_EI <- teff_total %>%
  filter(response_2=="EI") %>%
  droplevels()

teff_total_vert <- teff_total %>%
  filter(response_2=="Vertical") %>%
  droplevels()

teff_total_horiz <- teff_total %>%
  filter(response_2=="Horizontal") %>%
  droplevels()

## CONUS E:I INDIVIDUAL TOTAL EFFECTS
EI_conus<-ggplot(teff_total_EI ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette_EI, drop=TRUE)+
  ylim(-0.6,0.9)+
  facet_wrap(teff_total_EI$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",3),rep("#999999",2),rep("#56B4E9",3),rep("#F0E442",1))),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "bottom",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

## CONUS VERTICAL INDIVIDUAL TOTAL EFFECTS
vert_conus<-ggplot(teff_total_vert ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette_vert, drop=TRUE)+
  ylim(-0.6,0.9)+
  facet_wrap(teff_total_vert$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",3),rep("#56B4E9",3),rep("#F0E442",1))),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), #element_text(family = "RMN"),
        axis.title.y = element_blank(), #axis.title.y=element_text(family="RMN"),
        strip.text.x = element_text(family="RMN", size=12),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position= "none",
        legend.key.size = unit(10, 'point'),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
  ylab("Total effects")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

## CONUS HORIZONTAL INDIVIDUAL TOTAL EFFECTS
horiz_conus<-ggplot(teff_total_horiz ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-0.6,0.9)+
  facet_wrap(teff_total_horiz$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",4),rep("#56B4E9",3),rep("#F0E442",1))),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank(), #element_text(family = "RMN"),
        axis.title.y = element_blank(), #axis.title.y=element_text(family="RMN"),
        strip.text.x = element_text(family="RMN", size=12),
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
legend <- get_legend(EI_conus)

# REMOVE LEGEND
EI_conus <- EI_conus + theme(legend.position="none")

## CONUS LOW - INDIVIDUAL STD TOTAL EFFECTS
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/CONUS_LOW_HYDRAP_std_total_effects.tiff",
     width=7, height=5.5, units="in", res=400)
grid.arrange(arrangeGrob(EI_conus,
                         vert_conus,
                         horiz_conus,
                         ncol=3),
             legend,nrow=2,heights=c(3.5, .5))
dev.off()


##############
## PROCESS CONUS DATA FOR STACKED BARCHART
## LOW HYDRAP
# CALCULATE SUM OF ABSOLUTE TOTAL EFFECT BY CATEGORY and RESPONSE
test_low<-teff_total %>%
  group_by(response_2, category) %>%
  summarise(abs_eff=sum(abs(est_std)))

# CALCULATE PERCENT OF ABSOLUTE TOTAL EFFECT BY RESPONSE & CREATE Y POSITION FOR LABEL
low_hydrap_pct_tot_eff<- test_low%>%
  group_by(response_2) %>%
  mutate(pct_total_eff = abs_eff/sum(abs_eff)*1)%>%
  mutate(lab_ypos = cumsum(pct_total_eff) - 0.5*pct_total_eff)

# CREATE DATAFRAME OF R2 by PREDICTOR VAR TO MERGE WITH DATASET
response_2 <- c("EI","Vertical","Horizontal")
r2 <-c(0.47,0.14,0.62)
r2_conus<-data.frame(response_2,r2)
str(r2_conus)

# JOIN R2 to total effects data
low_hydrap_pct_tot_eff <- low_hydrap_pct_tot_eff %>%
  inner_join(r2_conus, by="response_2")

# SCALE AS Proportion OF VARIANCE EXPLAINED IN MODEL (multiple % of total effects by R2)
low_hydrap_pct_r2 <- low_hydrap_pct_tot_eff %>%
  group_by(response_2)%>%
  mutate(pct_r2_total = pct_total_eff*r2) %>%
  mutate(lab_yposr2 = cumsum(pct_r2_total) - 0.5*pct_r2_total)


#########
# ORDER DRIVER CATEGORIES IN OPPOSITE ORDER FOR STACKED BARCHART
low_hydrap_pct_r2$category <- ordered(low_hydrap_pct_r2$category, levels=c("human","climate","hydrology","EI","vertical","morphometry"))

low_hydrap_pct_r2$response_2 <- ordered(low_hydrap_pct_r2$response_2, levels=c("EI","Vertical","Horizontal"))

#########
# LOW HYDRAP STACKED BARCHART SCALED BY R2
low_hydrap_R2_stacked_bar <- ggplot(low_hydrap_pct_r2, aes(x=response_2, y = pct_r2_total))+
  geom_col(aes(fill = category), width=0.7)+
  scale_fill_manual(values = cbPalette_rev)+
  ylim(0,1)+
  #geom_text(aes(y = lab_yposr2,label = round(pct_r2_total,2),group=response_2), color='black',size=2, check_overlap=TRUE, family="RMN")+
  #guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        #text=element_text(family="RMN", size=11),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN", size=12),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_blank(),#element_text(family="RMN"),
        legend.text = element_text(family="RMN", size = 10),
        legend.position="right")+
  labs(title="",
       x=expression(),
       y=expression(paste("Proportion total effects scaled by  ", R^{2})))# % total effects related to variance explained?? (R^2)


# LOW HYDRAP STACKED BARCHART 100%
low_hydrap_pct_eff_stacked_bar <- ggplot(low_hydrap_pct_tot_eff, aes(x=response_2, y = pct_total_eff))+
  geom_col(aes(fill = category), width=0.7)+
  scale_fill_manual(values = cbPalette_rev)+
  #geom_text(aes(label=round(pct_total_eff,0),position=position_stack(vjust = .5)))+
  geom_text(aes(y = lab_ypos,label = round(pct_total_eff,0),group=response_2), color='black',size=1, check_overlap=TRUE, family="RMN")+
  #guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN", size=12),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_blank(),#element_text(family="RMN"),
        legend.text = element_text(family="RMN", size = 8),
        legend.position="right")+
  labs(title="",
       x=expression(),
       y=expression("Percent total effects"))

#########################
## PRINT STACKED BARGRAPH
## LOW HYDRAP
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/CONUS_LOW_stacked_barchart_pct_R2.tiff",
     width=5, height=4, units="in", res=400)
low_hydrap_R2_stacked_bar
dev.off()



####################
## BARCHARTS BY ECOREGIONS

#################
## ECOREGION - HAND COMPILED TOTAL EFFECTS (Standardized with Std error) from SEM model output with bootstrap sampling
teff_ecoreg <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/compiled_total_effects/ecoregion_total_effects.csv")

names(teff_ecoreg)
table(teff_ecoreg$predictor)

# RELABEL PREDICTOR VARIABLES
teff_ecoreg$predictor_2<-teff_ecoreg$predictor
levels(teff_ecoreg$predictor_2) <- list(flatness="bffFlat_grad", baseflow="BFIWs", HydrAP="HydrAP",depth="L_DpthMx_mod",
                                              EI="L_EI", inflow="L_inflow",area="L_LkAreakm2", precip="Precip_mm_total_yr_sc",
                                              shoreline="L_SLD", vert="L_VertDD_use", PHDI="PHDI", temp="Temp_degC_avg_yr")

# ORDER PREDICTOR VARIABLES
teff_ecoreg$predictor_2 <- ordered(teff_ecoreg$predictor_2, levels=c("area","depth","shoreline","flatness","vert","EI","inflow","baseflow",
                                                                                 "temp","precip","PHDI","HydrAP"))
# CREATE CATEGORIES
teff_ecoreg$category <-teff_ecoreg$predictor
levels(teff_ecoreg$category) <- list(morphometry="L_LkAreakm2", morphometry="L_DpthMx_mod", morphometry="L_SLD", morphometry="bffFlat_grad",
                                  hydrology="L_inflow",hydrology="BFIWs",
                                  vertical = "L_VertDD_use", EI = "L_EI",
                                  climate = "Temp_degC_avg_yr", climate = "Precip_mm_total_yr_sc", climate ="PHDI",
                                  human = "HydrAP")

teff_ecoreg$category <- ordered(teff_ecoreg$category, levels=c("morphometry","vertical","EI","hydrology","climate","human"))
table(teff_low$category)


# RENAME RESPONSE
teff_ecoreg$response_2 <-teff_ecoreg$response
levels(teff_ecoreg$response_2) <- list(EI="L_EI", Horizontal="L_HorizDD_use", Vertical="L_VertDD_use")
teff_ecoreg$response_2 <- ordered(teff_ecoreg$response_2, levels=c("EI","Vertical", "Horizontal"))

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


## GREAT PLAINS, APPALACHIANS, COASTAL PLAINS
teff_apps_gpls_cpls <- teff_ecoreg_total %>%
  filter(ecoregion =="GREAT PLAINS"|ecoregion =="APPALACHIANS"|ecoregion =="COASTAL PLAINS") %>%
  droplevels()
# Order ecoregions
teff_apps_gpls_cpls$ecoregion<-ordered(teff_apps_gpls_cpls$ecoregion, levels=c("GREAT PLAINS","APPALACHIANS","COASTAL PLAINS"))
table(teff_apps_gpls_cpls$ecoregion)


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


## CONUS E:I INDIVIDUAL TOTAL EFFECTS WEST & MIDWEST
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
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",3),rep("#999999",2),rep("#56B4E9",3),rep("#F0E442",1))),
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

## CONUS VERTICAL INDIVIDUAL TOTAL EFFECTS
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
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",3),rep("#56B4E9",3),rep("#F0E442",1))),
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

## CONUS HORIZONTAL INDIVIDUAL TOTAL EFFECTS
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
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",4),rep("#56B4E9",3),rep("#F0E442",1))),
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
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/WEST_MWEST_std_total_effects.tiff",
     width=7.5, height=8, units="in", res=400)
grid.arrange(arrangeGrob(EI_mw,
                         vert_mw,
                         horiz_mw,
                         ncol=3),
             legend,nrow=2,heights=c(3.5, .5))
dev.off()



################
## STACKED BAR CHART PROCESSING FOR WEST & MIDWEST DATA
##############
## WEST DATA
test_west<-teff_west_mwest %>%
  filter(ecoregion=="WEST")%>%
  group_by(response_2, category) %>%
  summarise(abs_eff=sum(abs(est_std)))

# CALCULATE PERCENT OF ABSOLUTE TOTAL EFFECT BY RESPONSE & CREATE Y POSITION FOR LABEL
west_hydrap_pct_tot_eff<- test_west%>%
  group_by(response_2) %>%
  mutate(pct_total_eff = abs_eff/sum(abs_eff)*100)%>%
  mutate(lab_ypos = cumsum(pct_total_eff) - 0.5*pct_total_eff)

# CREATE DATAFRAME OF WEST R2 by PREDICTOR VAR TO MERGE WITH DATASET
response_2 <- c("EI","Vertical","Horizontal")
r2_west <-c(0.36,0.38,0.79)
r2_west_df<-data.frame(response_2,r2_west)
str(r2_west_df)

# JOIN R2 to total effects data
west_hydrap_pct_tot_eff <- west_hydrap_pct_tot_eff %>%
  inner_join(r2_west_df, by="response_2")

# SCALE AS PERCENT OF VARIANCE EXPLAINED IN MODEL (multiple % of total effects by R2)
west_hydrap_pct_r2 <- west_hydrap_pct_tot_eff %>%
  group_by(response_2)%>%
  mutate(pct_r2_total = pct_total_eff*r2_west) %>%
  mutate(lab_yposr2 = cumsum(pct_r2_total) - 0.5*pct_r2_total)

#########
## MIDWEST DATA
test_mwest<-teff_west_mwest %>%
  filter(ecoregion=="MIDWEST") %>%
  group_by(response_2, category) %>%
  summarise(abs_eff=sum(abs(est_std)))

# CALCULATE PERCENT OF ABSOLUTE TOTAL EFFECT BY RESPONSE & CREATE Y POSITION FOR LABEL
mwest_hydrap_pct_tot_eff<- test_mwest%>%
  group_by(response_2) %>%
  mutate(pct_total_eff = abs_eff/sum(abs_eff)*100)%>%
  mutate(lab_ypos = cumsum(pct_total_eff) - 0.5*pct_total_eff)

# CREATE DATAFRAME OF MIDWEST R2 by PREDICTOR VAR TO MERGE WITH DATASET
response_2 <- c("EI","Vertical","Horizontal")
r2_mwest <-c(0.42,0.18,0.57)
r2_mwest_df<-data.frame(response_2,r2_mwest)
str(r2_mwest_df)

# JOIN R2 to total effects data
mwest_hydrap_pct_tot_eff <- mwest_hydrap_pct_tot_eff %>%
  inner_join(r2_mwest_df, by="response_2")

# SCALE AS PERCENT OF VARIANCE EXPLAINED IN MODEL (multiple % of total effects by R2)
mwest_hydrap_pct_r2 <- mwest_hydrap_pct_tot_eff %>%
  group_by(response_2)%>%
  mutate(pct_r2_total = pct_total_eff*r2_mwest) %>%
  mutate(lab_yposr2 = cumsum(pct_r2_total) - 0.5*pct_r2_total)


#########
# ORDER DRIVER CATEGORIES IN OPPOSITE ORDER FOR STACKED BARCHART
west_hydrap_pct_r2$category <- ordered(west_hydrap_pct_r2$category, levels=c("human","climate","hydrology","EI","vertical","morphometry"))
mwest_hydrap_pct_r2$category <- ordered(mwest_hydrap_pct_r2$category, levels=c("human","climate","hydrology","EI","vertical","morphometry"))
west_hydrap_pct_r2$response_2 <- ordered(west_hydrap_pct_r2$response_2, levels=c("EI","Vertical","Horizontal"))
mwest_hydrap_pct_r2$response_2 <- ordered(mwest_hydrap_pct_r2$response_2, levels=c("EI","Vertical","Horizontal"))


# WEST STACKED BARCHART SCALED BY R2 TOTAL EFFECTS ONLY
west_R2_stacked_bar <- ggplot(west_hydrap_pct_r2, aes(x=response_2, y = pct_r2_total))+
  geom_col(aes(fill = category), width=0.7)+
  scale_fill_manual(values = cbPalette_rev)+
  ylim(0,100)+
  #geom_text(aes(y = lab_yposr2,label = round(pct_r2_total,0),group=response_2), color='black',size=3, check_overlap=TRUE, family="RMN")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN", size=12),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_blank(),#element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="none")+
  labs(title="West",
       x=expression(),
       y=expression(paste("Percent total effects scaled by  ", R^{2})))

# MIDWEST
midwest_R2_stacked_bar <- ggplot(mwest_hydrap_pct_r2, aes(x=response_2, y = pct_r2_total))+
  geom_col(aes(fill = category), width=0.7)+
  scale_fill_manual(values = cbPalette_rev)+
  ylim(0,100)+
  #geom_text(aes(y = lab_yposr2,label = round(pct_r2_total,0),group=response_2), color='black',size=3, check_overlap=TRUE, family="RMN")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN", size=12),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_blank(),#element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+
  labs(title="Midwest",
       x=expression(),
       y=expression("")) #"Percent total effects"

###################
## Multiple ggplots with common legend
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
# GET LEGEND
legend<-get_legend(midwest_R2_stacked_bar)

# REMOVE LEGEND
midwest_pct_eff_stacked_bar<-midwest_R2_stacked_bar + theme(legend.position="none")

## WEST & MIDWEST - STACKED BARCHART
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/stacked_barchart_R2_WEST_MIDWEST.tiff",
     width=7.5, height=5, units="in", res=400)
grid.arrange(arrangeGrob(west_R2_stacked_bar + theme(legend.position = "none"),
                         midwest_R2_stacked_bar + theme(legend.position = "none"),
                         nrow=1),
             legend,nrow=2,heights=c(4, .5))
#grid.arrange(west_pct_eff_stacked_bar, midwest_pct_eff_stacked_bar, legend, ncol=2, nrow=2, widths=c(3.0, 2.8))
dev.off()



#################
## BARCHART INDIVIDUAL PREDICTORS: GREAT PLAINS, APPS, CST PLAINS 

# SUBSET DATA BY RESPONSE VARIABLES
teff_plains_EI <- teff_apps_gpls_cpls %>%
  filter(response_2 == "EI")%>%
  droplevels()

teff_plains_vert <- teff_apps_gpls_cpls %>%
  filter(response_2 == "Vertical")%>%
  droplevels()

teff_plains_horiz <- teff_apps_gpls_cpls %>%
  filter(response_2 == "Horizontal")%>%
  droplevels()


## CONUS E:I INDIVIDUAL TOTAL EFFECTS GREAT PLAINS, APPS, CST PLAINS
EI_pls<-ggplot(teff_plains_EI ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette_EI, drop=TRUE)+
  ylim(-1.1,1.0)+
  facet_grid(vars(teff_plains_EI$ecoregion), vars(teff_plains_EI$response_2))+
  #facet_wrap(teff_west_mwest_EI$response_2)+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, size=12, hjust=1, colour=c(rep("#E69F00",3),rep("#999999",2),rep("#56B4E9",3),rep("#F0E442",1))),
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
        legend.text=element_text(family="RMN",size=12))+
  ylab("Total effects")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

## CONUS VERTICAL INDIVIDUAL TOTAL EFFECTS
vert_pls<-ggplot(teff_plains_vert ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette_vert, drop=TRUE)+
  ylim(-1.1,1.0)+
  facet_grid(vars(teff_plains_vert$ecoregion), vars(teff_plains_vert$response_2))+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, size=12, hjust=1, colour=c(rep("#E69F00",4),rep("#999999",3),rep("#56B4E9",3),rep("#F0E442",1))),
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

## CONUS HORIZONTAL INDIVIDUAL TOTAL EFFECTS
horiz_pls<-ggplot(teff_plains_horiz ,aes(predictor_2,est_std,fill=category)) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  geom_errorbar(aes(ymin=ci_lower, ymax=ci_upper), width=.2, # est_std-se_std
                position=position_dodge(.9))+ 
  scale_fill_manual(values=cbPalette, drop=TRUE)+
  ylim(-1.1,1.0)+
  facet_grid(vars(teff_plains_horiz$ecoregion), vars(teff_plains_horiz$response_2))+
  geom_hline(aes(yintercept = 0))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, size=12,hjust=1, colour=c(rep("#E69F00",4),rep("#999999",4),rep("#56B4E9",3),rep("#F0E442",1))),
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
legend <- get_legend(EI_pls)

# REMOVE LEGEND
EI_pls <- EI_pls + theme(legend.position="none")

## CONUS LOW - INDIVIDUAL STD TOTAL EFFECTS
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/GPLAINS_APPS_CSTPLAINS_std_total_effects.tiff",
     width=8, height=9.5, units="in", res=400)
grid.arrange(arrangeGrob(EI_pls,
                         vert_pls,
                         horiz_pls,
                         ncol=3),
             legend,nrow=2,heights=c(3.5, .5))
dev.off()


