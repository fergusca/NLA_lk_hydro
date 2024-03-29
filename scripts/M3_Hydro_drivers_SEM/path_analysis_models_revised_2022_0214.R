#########################
## PATH ANALYSIS MODELS FOR MANUSCRIPT ON DRIVERS OF LAKE HYDROLOGY
##
## Revised modeling approach to start with base model without an interaction term to examine individual drivers
##  Then compare to model with an interaction term (HydrAP*PHDI) to examine if we see evidence for human effects being modified by climate

# 7/2/21
# 2/14/22 - removing Vertical decline as a predictor or pathway between vertical and horizontal decline
#########################

remove(list=ls())

library(tidyverse)
library(lavaan)
library(psych)
library(dplyr)
#library(rockchalk)

#########################
# READ DATA

# CONUS LOW HYDRAP (0 - 2) n = 553 lakes
low_hydrap <-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/conus_low_hydrap.csv")
#low_hydrap <-read_csv("~/NLA_hydro/NLA_hydro_driver/data_processed/conus_low_hydrap.csv")
todrop<-names(low_hydrap)%in%c("X1","X1_1") 
low_hydrap<-low_hydrap[!todrop]

# By ECOREGION
west <-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/west.csv") #n = 429
mwest <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/mwest.csv") #n = 482
gplains <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/gplains.csv") #n = 292
apps <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/apps.csv") #n = 324
cstplains <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/cstplains.csv") #n = 189

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

## SCALE PHDI FOR OLA PRESENTATION - Multiple by -1 so can interpret positive values as drought
west$PHDI_inv <-west$PHDI*-1
summary(west$PHDI_inv)
summary(west$PHDI)

mwest$PHDI_inv <-mwest$PHDI*-1

#Interaction HydrAP*PHDI_inv
west$hydrap_phdiinv <-west$HydrAP*west$PHDI_inv

mwest$hydrap_phdiinv <-mwest$HydrAP*mwest$PHDI_inv

#######################################
## BASE PATH ANALYSIS MODEL TO TEST (no interaction) _ REMOVING PATH BETWEEN VERTICAL AND HORIZONAL DECLINE and adding paths to Horizon from E:I, Baseflow, and Inflow
mymodel_DD_EI_rev1 <- '
L_HorizDD_use ~ a*L_EI + l*L_DpthMx_mod + o*L_LkAreakm2 + r*L_SLD + t*bffFlat_grad +tv*L_inflow + rancho*BFIWs + z*Temp_degC_avg_yr + e*Precip_mm_total_yr_sc + d*PHDI + g*HydrAP #+ delta*hydrap_phdi ## delta*hydrap_grp_sc_precip OLD INTERACTIONS delta*hydrap_Lprecip #delta*hydrap_phdi  precip_grd_sc
L_VertDD_use ~ p*L_LkAreakm2 + m*L_DpthMx_mod + s*L_SLD + h*L_EI + v*L_inflow + y*BFIWs + j*Temp_degC_avg_yr + c*Precip_mm_total_yr_sc + b*PHDI + f*HydrAP #+ eps*hydrap_phdi # L_Precip_mm_total_yr
L_EI ~ q*L_LkAreakm2 + n*L_DpthMx_mod + u*bffFlat_grad + w*L_inflow + x*BFIWs + i*Temp_degC_avg_yr + alpha*Precip_mm_total_yr_sc + beta*PHDI + k*HydrAP #+ gamma*hydrap_phdi

# indirect effect on Horiz - area
area_horiz := a*q 

# indirect effect on Horiz - zmax
zmax_horiz := a*n

# indirect effect on Horiz - SLD
#sld_horiz := a*s

# indirect effect on Horiz - bank
bank_horiz := u*a

# indirect effect on Horiz - E:I
#ei_horiz := a*h

# indirect effect on Horiz - inflow
inflow_horiz := a*w 

# indirect effect on Horiz -baseflow
base_horiz :=a*x

# indirect effect on Horiz - Temp
temp_horiz := a*i

# indirect effect (precip via EI ~ Horiz)
precip_horiz := a*alpha

# indirect effect (PHDI via EI ~ Horiz)
phdi_horiz := a*beta

# indirect effect (HydrAP via EI~ Horiz)
hydrap_horiz := a*k

# indirect effect on Horiz - PHDI*HydrAP
#inter_horiz := a*eps + gamma*h*a

### Vert

# indirect effect on Vert - area
area_vert := h*q

# indirect effect on Vert - zmax
zmax_vert := h*n

# indirect effect on Vert - bank
bank_vert := h*u

# indirect effect on Vert -inflow
inflow_vert := h*w

# indirect effect on Vert - baseflow
base_vert := h*x

# indirect effect (Temp via E:I ~ Vert)
temp_vert := h*i 

# indirect effect on Vert - precip
precip_vert := h*alpha

# indirect effect on Vert - PHDI
phdi_vert := h*beta

# indirect effect (HydrAP via E:I ~ vert)
hydrap_vert := h*k

# indirect effect on Vert - PHDI*HydrAP
#inter_vert := h*gamma

### total effects
# Horiz
total_area_horiz := o + area_horiz
total_zmax_horiz := l + zmax_horiz
total_sld_horiz := r #+ sld_horiz
total_bank_horiz := t + bank_horiz
#total_vert_horiz := a
total_ei_horiz := a #direct effect only now 
total_inflow_horiz := tv+inflow_horiz
total_base_horiz := rancho+base_horiz
total_temp_horiz := z + temp_horiz
total_precip_horiz := e + precip_horiz
total_phdi_horiz := d + phdi_horiz
total_hydrap_horiz := g + hydrap_horiz
#total_inter_horiz := delta + inter_horiz

# Vert
total_area_vert := p + area_vert
total_zmax_vert := m + zmax_vert
total_sld_vert := s
total_bank_vert := bank_vert
total_ei_vert := h
total_inflow_vert := v + inflow_vert
total_base_vert := y + base_vert
total_temp_vert := j + temp_vert
total_precip_vert := c + precip_vert
total_phdi_vert := b + phdi_vert
total_hydrap_vert := f + hydrap_vert
#total_inter_vert := eps + inter_vert

'

#####################
## LOW HYDRAP CONUS MODEL
fit_DD_EI_min <- sem(mymodel_DD_EI_rev1 , data=low_hydrap, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_min, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_DD_EI_min) 
print(mi_min[mi_min$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_min, 'r2')
#L_HorizDD_use  L_VertDD_use          L_EI 
#        0.156         0.129         0.469 

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_min <- capture.output(summary(fit_DD_EI_min, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_min, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/CONUS_0712_LOW_HYDRO_DD_EI_NO_INTERACTION_REV1.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_DD_EI_min)
write.csv(std_parameter_se_bootstrap_min, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/CONUS_0712_LOW_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI_REV1.csv")


#############
# WEST 
fit_DD_EI_west <- sem(mymodel_DD_EI_rev1 , data=west, 
                      #group = "ECOREG_rev",
                      missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_west, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi <-modindices(fit_DD_EI_west) 
print(mi[mi$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_west, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.398         0.382         0.359 

#############
# WEST WIHTOUT INTERACTION Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_west <- capture.output(summary(fit_DD_EI_west, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_west, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/WEST_0712_HYDRO_DD_EI_NO_INTERACTION_rev1.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_west<- standardizedSolution(fit_DD_EI_west)
write.csv(std_parameter_se_bootstrap_west, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/WEST_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI_rev1.csv")



#############
# MIDWEST 
fit_DD_EI_mwest <- sem(mymodel_DD_EI_rev1 , data=mwest, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_mwest, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi <-modindices(fit_DD_EI_mwest) 
print(mi[mi$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_mwest, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#        0.573         0.177         0.418 

#############
# MIDWEST WIHTOUT INTERACTION Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_mwest <- capture.output(summary(fit_DD_EI_mwest, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_mwest, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/MIDWEST_0712_HYDRO_DD_EI_NO_INTERACTION_rev1.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_mwest<- standardizedSolution(fit_DD_EI_mwest)
write.csv(std_parameter_se_bootstrap_mwest, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/MIDWEST_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI_rev1.csv")











###############################
## TOTAL EFFECTS BAR CHARTS
#######################
## LOAD DATA
## CONUS LOW HYDRAP (n = 553)
# HAND COMPILED TOTAL EFFECTS (Standardized with Std error) from SEM model output with bootstrap sampling
teff_low <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/compiled_total_effects/conus_low_hydrap_total_effects_rev1.csv")
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
                             L_SLD="shoreline",  PHDI="PHDI", Temp_degC_avg_yr="temp")) #L_VertDD_use="vert",

table(teff_low$predictor_2)

# ORDER PREDICTOR VARIABLES
teff_low$predictor_2 <- ordered(teff_low$predictor_2, levels=c("area","depth","shoreline","flatness","EI","inflow","baseflow",
                                                               "temp","precip","PHDI","HydrAP")) #"vert",
table(teff_low$predictor_2)


# CREATE CATEGORIES
teff_low$category <-teff_low$predictor
teff_low <- teff_low %>%
  mutate(category = recode(category,
                           L_LkAreakm2="morphometry", L_DpthMx_mod="morphometry", L_SLD="morphometry", bffFlat_grad="morphometry",
                           L_inflow="hydrology",BFIWs="hydrology",
                            L_EI = "EI",
                           Temp_degC_avg_yr="climate", Precip_mm_total_yr_sc="climate", PHDI="climate",
                           HydrAP="human")) # L_VertDD_use="vertical",
 
teff_low$category <- ordered(teff_low$category, levels=c("morphometry","EI","hydrology","climate","human")) # "vertical",
table(teff_low$category)

# SIMPLIFIED CATEGORIES FOR LEGEND
teff_low$category_2 <-teff_low$category
teff_low <- teff_low %>%
  mutate(category_2 = recode(category_2,
                             morphometry="morphometry", hydrology="hydrology", 
                              EI="hydrology", 
                             climate="climate", human="human")) #vertical="hydrology",

teff_low$category_2 <- ordered(teff_low$category_2, levels=c("morphometry","hydrology","climate","human"))
table(teff_low$category_2)


# RENAME RESPONSE
teff_low$response_2 <-teff_low$response
teff_low <- teff_low %>%
  mutate(response_2 = recode(response_2,
                             L_EI="EI", L_HorizDD_use="Horizontal", L_VertDD_use="Vertical"))

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
##################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# SET COLOR PALATE FOR BY DRIVER CATEGORY
# For barchars with individual predictors
cbPalette <- c("#E69F00","#bdbdbd","#d9d9d9","#56B4E9","#F0E442")# interaction= "#009E73") #,"#999999"
cbPalette_EI <- c("#E69F00","#d9d9d9","#56B4E9","#F0E442")# interaction= "#009E73")
cbPalette_vert <- c("#E69F00","#bdbdbd","#d9d9d9","#56B4E9","#F0E442")# interaction= "#009E73")



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
vert_conus

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
horiz_conus

# GET LEGEND
legend <- get_legend(EI_conus)

# REMOVE LEGEND
EI_conus <- EI_conus + theme(legend.position="none")

## CONUS LOW - INDIVIDUAL STD TOTAL EFFECTS
# ARRANGE MULTIPLE GRAPHS AND LEGEND
# https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/CONUS_LOW_HYDRAP_std_total_effects_rev1.tiff",
     width=7, height=5.5, units="in", res=200)
grid.arrange(arrangeGrob(EI_conus,
                         vert_conus,
                         horiz_conus,
                         ncol=3),
             legend,nrow=2,heights=c(3.5, .5))
dev.off()








#############
# GREAT PLAINS
fit_DD_EI_gplains <- sem(mymodel_DD_EI , data=gplains, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_gplains, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi <-modindices(fit_DD_EI_gplains) 
print(mi[mi$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_gplains, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#       0.709         0.104         0.354  

#############
# GREAT PLAINS WIHTOUT INTERACTION Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_gplains <- capture.output(summary(fit_DD_EI_gplains, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_gplains, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/GPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_gplains<- standardizedSolution(fit_DD_EI_gplains)
write.csv(std_parameter_se_bootstrap_gplains, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/GPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")


#############
# APPALACHIANS
fit_DD_EI_apps <- sem(mymodel_DD_EI , data=apps, 
                      #group = "ECOREG_rev",
                      missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_apps, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi <-modindices(fit_DD_EI_apps) 
print(mi[mi$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_apps, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#       0.702         0.362         0.309

#############
# APPALCHIANS WIHTOUT INTERACTION Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_apps <- capture.output(summary(fit_DD_EI_apps, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_apps, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/APPS_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_apps_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_apps<- standardizedSolution(fit_DD_EI_apps)
write.csv(std_parameter_se_bootstrap_apps, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/APPS_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")


#############
# COASTAL PLAINS
fit_DD_EI_cstplains <- sem(mymodel_DD_EI , data=cstplains, 
                      #group = "ECOREG_rev",
                      missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_cstplains, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi <-modindices(fit_DD_EI_cstplains) 
print(mi[mi$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_cstplains, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#     0.622         0.180         0.559  

#############
# COASTAL PLAINS WIHTOUT INTERACTION Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_cstplains <- capture.output(summary(fit_DD_EI_cstplains, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_cstplains, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/CSTPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_apps_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_cstplains<- standardizedSolution(fit_DD_EI_cstplains)
write.csv(std_parameter_se_bootstrap_cstplains, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/CSTPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")



#############################
## MODEL WITH INTERACTIONS BTW PHDI & HYDRAP (Total precipitation was scaled for large variance by dividing by 1000)
## & DIRECT, INDIRECT, AND TOTAL EFFECTS ALL EFFECTS
##  Estimation using bootstraping 
#https://www.youtube.com/watch?v=_tTPHt4cPwI&ab_channel=MikeCrowson
## https://lavaan.ugent.be/tutorial/mediation.html
#https://lavaan.ugent.be/tutorial/tutorial.pdf

mymodel_DD_EI_int <- '
L_HorizDD_use ~ a*L_VertDD_use + l*L_DpthMx_mod + o*L_LkAreakm2 + r*L_SLD + t*bffFlat_grad + z*Temp_degC_avg_yr + e*Precip_mm_total_yr_sc + d*PHDI + g*HydrAP + delta*hydrap_phdi ## delta*hydrap_grp_sc_precip OLD INTERACTIONS delta*hydrap_Lprecip #delta*hydrap_phdi  precip_grd_sc
L_VertDD_use ~ p*L_LkAreakm2 + m*L_DpthMx_mod + s*L_SLD + h*L_EI + v*L_inflow + y*BFIWs + j*Temp_degC_avg_yr + c*Precip_mm_total_yr_sc + b*PHDI + f*HydrAP + eps*hydrap_phdi # L_Precip_mm_total_yr
L_EI ~ q*L_LkAreakm2 + n*L_DpthMx_mod + u*bffFlat_grad + w*L_inflow + x*BFIWs + i*Temp_degC_avg_yr + alpha*Precip_mm_total_yr_sc + beta*PHDI + k*HydrAP + gamma*hydrap_phdi

# indirect effect on Horiz - area
area_horiz := a*p +q*h*a

# indirect effect on Horiz - zmax
zmax_horiz := a*m + n*h*a

# indirect effect on Horiz - SLD
sld_horiz := a*s

# indirect effect on Horiz - bank
bank_horiz := u*h*a

# indirect effect on Horiz - E:I
ei_horiz := a*h

# indirect effect on Horiz - inflow
inflow_horiz := a*v + w*h*a

# indirect effect on Horiz -baseflow
base_horiz :=a*y + x*h*a

# indirect effect on Horiz - Temp
temp_horiz := a*j + i*h*a

# indirect effect (precip via Vert ~ Horiz)
precip_horiz := a*c + alpha*h*a

# indirect effect (PHDI via Vert ~ Horiz)
phdi_horiz := a*b + beta*h*a

# indirect effect (HydrAP via Vert~ Horiz)
hydrap_horiz := a*f + k*h*a

# indirect effect on Horiz - PHDI*HydrAP
inter_horiz := a*eps + gamma*h*a

### Vert

# indirect effect on Vert - area
area_vert := h*q

# indirect effect on Vert - zmax
zmax_vert := h*n

# indirect effect on Vert - bank
bank_vert := h*u

# indirect effect on Vert -inflow
inflow_vert := h*w

# indirect effect on Vert - baseflow
base_vert := h*x

# indirect effect (Temp via E:I ~ Vert)
temp_vert := h*i 

# indirect effect on Vert - precip
precip_vert := h*alpha

# indirect effect on Vert - PHDI
phdi_vert := h*beta

# indirect effect (HydrAP via E:I ~ vert)
hydrap_vert := h*k

# indirect effect on Vert - PHDI*HydrAP
inter_vert := h*gamma

### total effects
# Horiz
total_area_horiz := o + area_horiz
total_zmax_horiz := l + zmax_horiz
total_sld_horiz := r + sld_horiz
total_bank_horiz := t + bank_horiz
total_vert_horiz := a
total_ei_horiz := ei_horiz
total_inflow_horiz := inflow_horiz
total_base_horiz := base_horiz
total_temp_horiz := z + temp_horiz
total_precip_horiz := e + precip_horiz
total_phdi_horiz := d + phdi_horiz
total_hydrap_horiz := g + hydrap_horiz
total_inter_horiz := delta + inter_horiz

# Vert
total_area_vert := p + area_vert
total_zmax_vert := m + zmax_vert
total_sld_vert := s
total_bank_vert := bank_vert
total_ei_vert := h
total_inflow_vert := v + inflow_vert
total_base_vert := y + base_vert
total_temp_vert := j + temp_vert
total_precip_vert := c + precip_vert
total_phdi_vert := b + phdi_vert
total_hydrap_vert := f + hydrap_vert
total_inter_vert := eps + inter_vert

'

#####################
## LOW HYDRAP CONUS MODEL
fit_DD_EI_INT_min <- sem(mymodel_DD_EI_int , data=low_hydrap, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_min, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# Unstandardized effects
summary(fit_DD_EI_INT_min, standardized=FALSE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_INT_min <-modindices(fit_DD_EI_INT_min) 
print(mi_INT_min[mi_INT_min$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_min, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.624         0.136         0.469 

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_INT_min <- capture.output(summary(fit_DD_EI_INT_min, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_min, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/CONUS_0712_LOW_HYDRO_DD_EI_grdsc_PHDI_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_min<- standardizedSolution(fit_DD_EI_INT_min)
write.csv(std_parameter_se_bootstrap_INT_min, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/CONUS_0712_LOW_HYDRO_DD_EI_PHDI_INTERACTION_std_bootstrap_CI.csv")

##########################
## ECOREGION MODELS
##########################

############
## WEST
fit_DD_EI_INT_west <- sem(mymodel_DD_EI_int , data=west, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_west, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
miINT <-modindices(fit_DD_EI_INT_west) 
print(miINT[miINT$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_west, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.791         0.382         0.361 

#############
# WEST 
out_DD_EI_base_INT_west <- capture.output(summary(fit_DD_EI_INT_west, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_west, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/WEST_0712_PHDI_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_west<- standardizedSolution(fit_DD_EI_INT_west)
write.csv(std_parameter_se_bootstrap_INT_west, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/WEST_0712_PHDI_INTERACTION_std_bootstrap_CI.csv")


#############
# MIDWEST 
fit_DD_EI_INT_mwest <- sem(mymodel_DD_EI_int , data=mwest, 
                        #group = "ECOREG_rev",
                        missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_mwest, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
miINT <-modindices(fit_DD_EI_INT_mwest) 
print(miINT[miINT$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_mwest, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.573         0.207         0.418 

#############
# MIDWEST Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_INT_mwest <- capture.output(summary(fit_DD_EI_INT_mwest, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_mwest, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/MWEST_0712_PHDI_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_mwest<- standardizedSolution(fit_DD_EI_INT_mwest)
write.csv(std_parameter_se_bootstrap_INT_mwest, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/MWEST_0712_PHDI_INTERACTION_std_bootstrap_CI.csv")


#############
# GREAT PLAINS
fit_DD_EI_INT_gplains <- sem(mymodel_DD_EI_int , data=gplains, 
                          #group = "ECOREG_rev",
                          missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_gplains, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
miINT <-modindices(fit_DD_EI_INT_gplains) 
print(miINT[miINT$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_gplains, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#        0.714         0.115         0.358 

#############
# GREAT PLAINS Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_INT_gplains <- capture.output(summary(fit_DD_EI_INT_gplains, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_gplains, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/GPLAINS_0712_PHDI_INTERACTION.csv")

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_gplains<- standardizedSolution(fit_DD_EI_INT_gplains)
write.csv(std_parameter_se_bootstrap_INT_gplains, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/GPLAINS_0712_PHDI_INTERACTION_std_bootstrap_CI.csv")


#############
# APPALACHIANS
fit_DD_EI_INT_apps <- sem(mymodel_DD_EI_int , data=apps, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_apps, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
miINT <-modindices(fit_DD_EI_INT_apps) 
print(miINT[miINT$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_apps, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.702         0.362         0.322 

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_INT_apps <- capture.output(summary(fit_DD_EI_INT_apps, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_apps, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/APPS_0712_PHDI_INTERACTION.csv")

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_apps<- standardizedSolution(fit_DD_EI_INT_apps)
write.csv(std_parameter_se_bootstrap_INT_apps, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/APPS_0712_PHDI_INTERACTION_std_bootstrap_CI.csv")


#############
# COASTAL PLAINS
fit_DD_EI_INT_coastplains <- sem(mymodel_DD_EI_int , data=cstplains, 
                              #group = "ECOREG_rev",
                              missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_coastplains, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
miINT <-modindices(fit_DD_EI_INT_coastplains) 
print(miINT[miINT$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_coastplains, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.633         0.185         0.560

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_INT_coastplains <- capture.output(summary(fit_DD_EI_INT_coastplains, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_coastplains, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/CSTPLAINS_0712_PHDI_INTERACTION.csv")

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_coastplains<- standardizedSolution(fit_DD_EI_INT_coastplains)
write.csv(std_parameter_se_bootstrap_INT_coastplains, "~/NLA_hydro/NLA_hydro_driver/Routput/interaction_model/CSTPLAINS_0712_PHDI_INTERACTION_std_bootstrap_CI.csv")



################################
## RESCALED PHDI and INTERACTION FOR OLA PRESENTATION 11/1/21
#############################
## MODEL WITH INTERACTIONS BTW PHDI & HYDRAP (Total precipitation was scaled for large variance by dividing by 1000)
## & DIRECT, INDIRECT, AND TOTAL EFFECTS ALL EFFECTS
##  Estimation using bootstraping 
#https://www.youtube.com/watch?v=_tTPHt4cPwI&ab_channel=MikeCrowson
## https://lavaan.ugent.be/tutorial/mediation.html
#https://lavaan.ugent.be/tutorial/tutorial.pdf

mymodel_DD_EI_int_scaled <- '
L_HorizDD_use ~ a*L_VertDD_use + l*L_DpthMx_mod + o*L_LkAreakm2 + r*L_SLD + t*bffFlat_grad + z*Temp_degC_avg_yr + e*Precip_mm_total_yr_sc + d*PHDI_inv + g*HydrAP + delta*hydrap_phdiinv ## delta*hydrap_grp_sc_precip OLD INTERACTIONS delta*hydrap_Lprecip #delta*hydrap_phdi  precip_grd_sc
L_VertDD_use ~ p*L_LkAreakm2 + m*L_DpthMx_mod + s*L_SLD + h*L_EI + v*L_inflow + y*BFIWs + j*Temp_degC_avg_yr + c*Precip_mm_total_yr_sc + b*PHDI_inv + f*HydrAP + eps*hydrap_phdiinv # L_Precip_mm_total_yr
L_EI ~ q*L_LkAreakm2 + n*L_DpthMx_mod + u*bffFlat_grad + w*L_inflow + x*BFIWs + i*Temp_degC_avg_yr + alpha*Precip_mm_total_yr_sc + beta*PHDI_inv + k*HydrAP + gamma*hydrap_phdiinv

# indirect effect on Horiz - area
area_horiz := a*p +q*h*a

# indirect effect on Horiz - zmax
zmax_horiz := a*m + n*h*a

# indirect effect on Horiz - SLD
sld_horiz := a*s

# indirect effect on Horiz - bank
bank_horiz := u*h*a

# indirect effect on Horiz - E:I
ei_horiz := a*h

# indirect effect on Horiz - inflow
inflow_horiz := a*v + w*h*a

# indirect effect on Horiz -baseflow
base_horiz :=a*y + x*h*a

# indirect effect on Horiz - Temp
temp_horiz := a*j + i*h*a

# indirect effect (precip via Vert ~ Horiz)
precip_horiz := a*c + alpha*h*a

# indirect effect (PHDI via Vert ~ Horiz)
phdi_horiz := a*b + beta*h*a

# indirect effect (HydrAP via Vert~ Horiz)
hydrap_horiz := a*f + k*h*a

# indirect effect on Horiz - PHDI*HydrAP
inter_horiz := a*eps + gamma*h*a

### Vert

# indirect effect on Vert - area
area_vert := h*q

# indirect effect on Vert - zmax
zmax_vert := h*n

# indirect effect on Vert - bank
bank_vert := h*u

# indirect effect on Vert -inflow
inflow_vert := h*w

# indirect effect on Vert - baseflow
base_vert := h*x

# indirect effect (Temp via E:I ~ Vert)
temp_vert := h*i 

# indirect effect on Vert - precip
precip_vert := h*alpha

# indirect effect on Vert - PHDI
phdi_vert := h*beta

# indirect effect (HydrAP via E:I ~ vert)
hydrap_vert := h*k

# indirect effect on Vert - PHDI*HydrAP
inter_vert := h*gamma

### total effects
# Horiz
total_area_horiz := o + area_horiz
total_zmax_horiz := l + zmax_horiz
total_sld_horiz := r + sld_horiz
total_bank_horiz := t + bank_horiz
total_vert_horiz := a
total_ei_horiz := ei_horiz
total_inflow_horiz := inflow_horiz
total_base_horiz := base_horiz
total_temp_horiz := z + temp_horiz
total_precip_horiz := e + precip_horiz
total_phdi_horiz := d + phdi_horiz
total_hydrap_horiz := g + hydrap_horiz
total_inter_horiz := delta + inter_horiz

# Vert
total_area_vert := p + area_vert
total_zmax_vert := m + zmax_vert
total_sld_vert := s
total_bank_vert := bank_vert
total_ei_vert := h
total_inflow_vert := v + inflow_vert
total_base_vert := y + base_vert
total_temp_vert := j + temp_vert
total_precip_vert := c + precip_vert
total_phdi_vert := b + phdi_vert
total_hydrap_vert := f + hydrap_vert
total_inter_vert := eps + inter_vert

'

############
## WEST - MODEL WITH INTERACTION AND SCALED PHDI FOR OLA PRESENTATION 2021
fit_DD_EI_INT_scaled_west <- sem(mymodel_DD_EI_int_scaled , data=west, 
                          #group = "ECOREG_rev",
                          missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_scaled_west, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
miINT_sc <-modindices(fit_DD_EI_INT_scaled_west) 
print(miINT_sc[miINT_sc$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_scaled_west, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.791         0.382         0.361 

#############
# WEST 
out_DD_EI_base_INT_scaled_west <- capture.output(summary(fit_DD_EI_INT_scaled_west, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_scaled_west, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/Routput/interaction_model/WEST_0712_PHDI_scaled_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_scaled_west<- standardizedSolution(fit_DD_EI_INT_scaled_west)
write.csv(std_parameter_se_bootstrap_INT_scaled_west, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/Routput/interaction_model/WEST_0712_PHDI_scaled_INTERACTION_std_bootstrap_CI.csv")


#############
# MIDWEST 
fit_DD_EI_INT_scaled_mwest <- sem(mymodel_DD_EI_int_scaled , data=mwest, 
                           #group = "ECOREG_rev",
                           missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_INT_scaled_mwest, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
miINT_sc <-modindices(fit_DD_EI_INT_scaled_mwest) 
print(miINT_sc[miINT_sc$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_INT_scaled_mwest, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.573         0.207         0.418 

#############
# MIDWEST Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_INT_scaled_mwest <- capture.output(summary(fit_DD_EI_INT_scaled_mwest, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_INT_scaled_mwest, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/Routput/interaction_model/MWEST_0712_PHDI_scaled_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_INT_scaled_mwest<- standardizedSolution(fit_DD_EI_INT_scaled_mwest)
write.csv(std_parameter_se_bootstrap_INT_scaled_mwest, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/NLA_project_repository/NLA_hydro_driver/Routput/interaction_model/MWEST_0712_PHDI_scaled_INTERACTION_std_bootstrap_CI.csv")


##########################################
### REVISED MODEL REMOVING VERTICAL DECLINE ALTOGETHER
#######################################
## BASE PATH ANALYSIS MODEL TO TEST (no interaction) _ REMOVING PATH BETWEEN VERTICAL AND HORIZONAL DECLINE and adding paths to Horizon from E:I, Baseflow, and Inflow
mymodel_DD_EI_rev2 <- '
L_HorizDD_use ~ a*L_EI + l*L_DpthMx_mod + o*L_LkAreakm2 + r*L_SLD + t*bffFlat_grad +tv*L_inflow + rancho*BFIWs + z*Temp_degC_avg_yr + e*Precip_mm_total_yr_sc + d*PHDI + g*HydrAP #+ delta*hydrap_phdi ## delta*hydrap_grp_sc_precip OLD INTERACTIONS delta*hydrap_Lprecip #delta*hydrap_phdi  precip_grd_sc
#L_VertDD_use ~ p*L_LkAreakm2 + m*L_DpthMx_mod + s*L_SLD + h*L_EI + v*L_inflow + y*BFIWs + j*Temp_degC_avg_yr + c*Precip_mm_total_yr_sc + b*PHDI + f*HydrAP #+ eps*hydrap_phdi # L_Precip_mm_total_yr
L_EI ~ q*L_LkAreakm2 + n*L_DpthMx_mod + u*bffFlat_grad + w*L_inflow + x*BFIWs + i*Temp_degC_avg_yr + alpha*Precip_mm_total_yr_sc + beta*PHDI + k*HydrAP #+ gamma*hydrap_phdi

# indirect effect on Horiz - area
area_horiz := a*q 

# indirect effect on Horiz - zmax
zmax_horiz := a*n

# indirect effect on Horiz - SLD
#sld_horiz := a*s

# indirect effect on Horiz - bank
bank_horiz := u*a

# indirect effect on Horiz - E:I
#ei_horiz := a*h

# indirect effect on Horiz - inflow
inflow_horiz := a*w 

# indirect effect on Horiz -baseflow
base_horiz :=a*x

# indirect effect on Horiz - Temp
temp_horiz := a*i

# indirect effect (precip via EI ~ Horiz)
precip_horiz := a*alpha

# indirect effect (PHDI via EI ~ Horiz)
phdi_horiz := a*beta

# indirect effect (HydrAP via EI~ Horiz)
hydrap_horiz := a*k

# indirect effect on Horiz - PHDI*HydrAP
#inter_horiz := a*eps + gamma*h*a

### total effects
# Horiz
total_area_horiz := o + area_horiz
total_zmax_horiz := l + zmax_horiz
total_sld_horiz := r #+ sld_horiz
total_bank_horiz := t + bank_horiz
#total_vert_horiz := a
total_ei_horiz := a #direct effect only now 
total_inflow_horiz := tv+inflow_horiz
total_base_horiz := rancho+base_horiz
total_temp_horiz := z + temp_horiz
total_precip_horiz := e + precip_horiz
total_phdi_horiz := d + phdi_horiz
total_hydrap_horiz := g + hydrap_horiz
#total_inter_horiz := delta + inter_horiz

'

#####################
## LOW HYDRAP CONUS MODEL
fit_DD_EI_min2 <- sem(mymodel_DD_EI_rev2 , data=low_hydrap, 
                     #group = "ECOREG_rev",
                     missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_min2, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_DD_EI_min) 
print(mi_min[mi_min$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_min, 'r2')
#L_HorizDD_use          L_EI 
#        0.157         0.469 


## WEST MODEL
fit_DD_EI_WEST2 <- sem(mymodel_DD_EI_rev2 , data=west, 
                      #group = "ECOREG_rev",
                      missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_WEST2, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_DD_EI_WEST2) 
print(mi_min[mi_min$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_WEST2, 'r2')
# L_HorizDD_use       L_EI 
#       0.395         0.359

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_min <- capture.output(summary(fit_DD_EI_min2, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_min, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/CONUS_0712_LOW_HYDRO_DD_EI_NO_INTERACTION_REV1.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_DD_EI_min2)
write.csv(std_parameter_se_bootstrap_min, "~/NLA_hydro/NLA_hydro_driver/Routput/base_model/CONUS_0712_LOW_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI_REV1.csv")

