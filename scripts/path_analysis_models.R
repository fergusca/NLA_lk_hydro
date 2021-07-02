#########################
## PATH ANALYSIS MODELS FOR MANUSCRIPT ON DRIVERS OF LAKE HYDROLOGY
##
## Revised modeling approach to start with base model without an interaction term to examine individual drivers
##  Then compare to model with an interaction term (HydrAP*PHDI) to examine if we see evidence for human effects being modified by climate

# 7/2/21
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

#######################################
## BASE PATH ANALYSIS MODEL TO TEST (no interaction)
mymodel_DD_EI <- '
L_HorizDD_use ~ a*L_VertDD_use + l*L_DpthMx_mod + o*L_LkAreakm2 + r*L_SLD + t*bffFlat_grad + z*Temp_degC_avg_yr + e*Precip_mm_total_yr_sc + d*PHDI + g*HydrAP #+ delta*hydrap_phdi ## delta*hydrap_grp_sc_precip OLD INTERACTIONS delta*hydrap_Lprecip #delta*hydrap_phdi  precip_grd_sc
L_VertDD_use ~ p*L_LkAreakm2 + m*L_DpthMx_mod + s*L_SLD + h*L_EI + v*L_inflow + y*BFIWs + j*Temp_degC_avg_yr + c*Precip_mm_total_yr_sc + b*PHDI + f*HydrAP #+ eps*hydrap_phdi # L_Precip_mm_total_yr
L_EI ~ q*L_LkAreakm2 + n*L_DpthMx_mod + u*bffFlat_grad + w*L_inflow + x*BFIWs + i*Temp_degC_avg_yr + alpha*Precip_mm_total_yr_sc + beta*PHDI + k*HydrAP #+ gamma*hydrap_phdi

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
fit_DD_EI_min <- sem(mymodel_DD_EI , data=low_hydrap, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_min, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi_min <-modindices(fit_DD_EI_min) 
print(mi_min[mi_min$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_min, 'r2')
#L_HorizDD_use  L_VertDD_use          L_EI 
#         0.623         0.129         0.469 

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_min <- capture.output(summary(fit_DD_EI_min, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_min, "~/NLA_hydro/NLA_hydro_driver/Routput/CONUS_0712_LOW_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_min<- standardizedSolution(fit_DD_EI_min)
write.csv(std_parameter_se_bootstrap_min, "~/NLA_hydro/NLA_hydro_driver/Routput/CONUS_0712_LOW_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")


#############
# WEST 
fit_DD_EI_west <- sem(mymodel_DD_EI , data=west, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_west, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi <-modindices(fit_DD_EI_west) 
print(mi[mi$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_west, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#        0.788         0.382         0.359 

#############
# WEST WIHTOUT INTERACTION Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_west <- capture.output(summary(fit_DD_EI_west, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_west, "~/NLA_hydro/NLA_hydro_driver/Routput/WEST_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_west<- standardizedSolution(fit_DD_EI_west)
write.csv(std_parameter_se_bootstrap_west, "~/NLA_hydro/NLA_hydro_driver/Routput/WEST_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")



#############
# MIDWEST 
fit_DD_EI_mwest <- sem(mymodel_DD_EI , data=mwest, 
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
write.csv(out_DD_EI_base_mwest, "~/NLA_hydro/NLA_hydro_driver/Routput/MIDWEST_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_mwest<- standardizedSolution(fit_DD_EI_mwest)
write.csv(std_parameter_se_bootstrap_mwest, "~/NLA_hydro/NLA_hydro_driver/Routput/MIDWEST_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")


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
write.csv(out_DD_EI_base_gplains, "~/NLA_hydro/NLA_hydro_driver/Routput/GPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_gplains<- standardizedSolution(fit_DD_EI_gplains)
write.csv(std_parameter_se_bootstrap_gplains, "~/NLA_hydro/NLA_hydro_driver/Routput/GPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")


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
write.csv(out_DD_EI_base_apps, "~/NLA_hydro/NLA_hydro_driver/Routput/APPS_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_apps_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_apps<- standardizedSolution(fit_DD_EI_apps)
write.csv(std_parameter_se_bootstrap_apps, "~/NLA_hydro/NLA_hydro_driver/Routput/APPS_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")


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
write.csv(out_DD_EI_base_cstplains, "~/NLA_hydro/NLA_hydro_driver/Routput/CSTPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION.csv") #summ_apps_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_cstplains<- standardizedSolution(fit_DD_EI_cstplains)
write.csv(std_parameter_se_bootstrap_cstplains, "~/NLA_hydro/NLA_hydro_driver/Routput/CSTPLAINS_0712_HYDRO_DD_EI_NO_INTERACTION_std_bootstrap_CI.csv")






#############################
## MODEL WITH INTERACTIONS BTW PHDI & HYDRAP (Total precipitation was scaled for large variance by dividing by 1000)
## & DIRECT, INDIRECT, AND TOTAL EFFECTS ALL EFFECTS
##  Estimation using bootstraping 
#https://www.youtube.com/watch?v=_tTPHt4cPwI&ab_channel=MikeCrowson
## 2/5/21
## https://lavaan.ugent.be/tutorial/mediation.html
#https://lavaan.ugent.be/tutorial/tutorial.pdf

mymodel_DD_EI7 <- '
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
fit_DD_EI_7_min <- sem(mymodel_DD_EI7 , data=low_hydrap, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI_7_min, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi7_min <-modindices(fit_DD_EI_7_min) 
print(mi7_min[mi7_min$mi >3.0,])

# GET R^2
inspect(fit_DD_EI_7_min, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.624         0.136         0.469 

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_7_min <- capture.output(summary(fit_DD_EI_7_min, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_7_min, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/summ_CONUS_0712_LOW_HYDRO_DD_EI_grand_scaled_PHDI_interaction.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_7_min<- standardizedSolution(fit_DD_EI_7_min)
write.csv(std_parameter_se_bootstrap_7_min, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/summ_CONUS_0712_LOW_HYDRO_DD_EI_grand_scaled_PHDI_int_std_bootstrap_CI.csv")

##########################
## ECOREGION MODELS
##########################

############
## WEST
fit_DD_EI7_west <- sem(mymodel_DD_EI7 , data=west, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI7_west, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #

# request modification indices greater than 3.0 - from Grace USGS materials
mi7 <-modindices(fit_DD_EI7_west) 
print(mi7[mi7$mi >3.0,])

# GET R^2
inspect(fit_DD_EI7_west, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.791         0.382         0.361 

#############
# WEST 
out_DD_EI_base_7_west <- capture.output(summary(fit_DD_EI7_west, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_7_west, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/WEST_0712_HYDRO_DD_EI_PHDI_interaction.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_7_west<- standardizedSolution(fit_DD_EI7_west)
write.csv(std_parameter_se_bootstrap_7_west, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/WEST_0712_HYDRO_DD_EI_PHDI_int_std_bootstrap_CI.csv")


#############
# MIDWEST 
fit_DD_EI7_mwest <- sem(mymodel_DD_EI7 , data=mwest, 
                        #group = "ECOREG_rev",
                        missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI7_mwest, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi7 <-modindices(fit_DD_EI7_mwest) 
print(mi7[mi7$mi >3.0,])

# GET R^2
inspect(fit_DD_EI7_mwest, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.573         0.207         0.418 

#############
# MIDWEST Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_7_mwest <- capture.output(summary(fit_DD_EI7_mwest, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_7_mwest, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/MIDWEST_0712_HYDRO_DD_EI_PHDI_interaction.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_7_mwest<- standardizedSolution(fit_DD_EI7_mwest)
write.csv(std_parameter_se_bootstrap_7_mwest, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/MIDWEST_0712_HYDRO_DD_EI_PHDI_int_std_bootstrap_CI.csv")


#############
# GREAT PLAINS
fit_DD_EI7_gplains <- sem(mymodel_DD_EI7 , data=gplains, 
                          #group = "ECOREG_rev",
                          missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI7_gplains, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi7 <-modindices(fit_DD_EI7_gplains) 
print(mi7[mi7$mi >3.0,])

# GET R^2
inspect(fit_DD_EI7_gplains, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#        0.714         0.115         0.358 

#############
# GREAT PLAINS Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_7_gplains <- capture.output(summary(fit_DD_EI7_gplains, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_7_gplains, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/GPLAINS_0712_HYDRO_DD_EI_grand_scaled_PHDI_interaction.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_7_gplains<- standardizedSolution(fit_DD_EI7_gplains)
write.csv(std_parameter_se_bootstrap_7_gplains, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/GPLAINS_0712_HYDRO_DD_EI_grand_scaled_PHDI_int_std_bootstrap_CI.csv")


#############
# APPALACHIANS
fit_DD_EI7_apps <- sem(mymodel_DD_EI7 , data=apps, 
                       #group = "ECOREG_rev",
                       missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI7_apps, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi7 <-modindices(fit_DD_EI7_apps) 
print(mi7[mi7$mi >3.0,])

# GET R^2
inspect(fit_DD_EI7_apps, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.702         0.362         0.322 

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_7_apps <- capture.output(summary(fit_DD_EI7_apps, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_7_apps, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/APPS_0712_HYDRO_DD_EI_PHDI_interaction.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_7_apps<- standardizedSolution(fit_DD_EI7_apps)
write.csv(std_parameter_se_bootstrap_7_apps, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/APPS_0712_HYDRO_DD_EI_PHDI_int_std_bootstrap_CI.csv")


#############
# COASTAL PLAINS
fit_DD_EI7_coastplains <- sem(mymodel_DD_EI7 , data=coastplains, 
                              #group = "ECOREG_rev",
                              missing="ML", se="bootstrap") #, group="Lake_Origin_mod")
summary(fit_DD_EI7_coastplains, standardized=TRUE, fit.measures=TRUE, modindices=F)#, modindices=T) #


# request modification indices greater than 3.0 - from Grace USGS materials
mi7 <-modindices(fit_DD_EI7_coastplains) 
print(mi7[mi7$mi >3.0,])

# GET R^2
inspect(fit_DD_EI7_coastplains, 'r2')
# L_HorizDD_use  L_VertDD_use          L_EI 
#         0.633         0.185         0.560

#############
# Export R output
#https://www.r-bloggers.com/export-r-output-to-a-file/
out_DD_EI_base_7_coastplains <- capture.output(summary(fit_DD_EI7_coastplains, standardized=TRUE, fit.measures=TRUE)) #, modindices=T
write.csv(out_DD_EI_base_7_coastplains, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/CSTPLAINS_0712_HYDRO_DD_EI_PHDI_interaction.csv") #summ_WEST_07_sat

# Standardized estimates of bootstrap model
std_parameter_se_bootstrap_7_coastplains<- standardizedSolution(fit_DD_EI7_coastplains)
write.csv(std_parameter_se_bootstrap_7_coastplains, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/a_Project_lake_drawdown_effects/Structural_equation_model_project/Routput/SEM/SEM_results_for_MS/CSTPLAINS_0712_HYDRO_DD_EI_PHDI_int_std_bootstrap_CI.csv")

