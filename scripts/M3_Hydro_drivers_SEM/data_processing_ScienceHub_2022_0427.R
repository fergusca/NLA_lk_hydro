##################
## LAKE HYDRO DRIVERS DATA PROCESSING
##  FOR UPLOADING TO SCIENCEHUB

## 4/27/22
###################

library(dplyr)

############
## LOAD PROCESSED NLA 0712 data
## Includes 2007 and 2012 lake data taking all obs from 2007 and only new sites visited in 2012
##  SO have a unique observation per lake n = 1716
dat<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/conus_NLA_opt1_1716.csv")

# CREATE SOME VARIABLES
## Follows bins from HydrAP paper (Ecological Indicators 2021)
dat$hydrap_bin <- dat$HydrAP

dat$hydrap_bin[dat$HydrAP <(3)]<- "Low"
dat$hydrap_bin[dat$HydrAP >=3 & dat$HydrAP <=5] <- "Moderate"
dat$hydrap_bin[dat$HydrAP > 5] <- "High"

## GRAND MEAN SCALED 30 yr normal precipitation
dat$Precip8110Ws_grd_sc <- scale(dat$Precip8110Ws, scale=T)

## GROUP MEAN SCALED 30 yr normal PRECIPITATION BY ECOREGION (scale = centers by mean and divides by std dev within an ecoregion group)
dat <- dat %>%
  group_by(ECOREG_rev) %>%
  mutate(Precip8110Ws_grp_sc = scale(Precip8110Ws, scale=TRUE)) # scaling is the default but just in case
summary(dat$Precip8110Ws_grp_sc)


##############
## CREATE A DATASUBSET OF VARIABLES USED IN THE LAKE HYDRO DRIVERS SEM MODELS

dat_red <- dat %>%
  select(SITE_ID,VISIT_NO,YEAR,DATE_COL_iso,ECOREG_rev,
         E_I,VertDD_use,HorizDD_use,
         LkArea_km2,DpthMx_mod,SLD,bffFlat_grad,
         inflow,BFIWs,
         Temp_degC_avg_yr,Precip_mm_total_yr,Precip_mm_total_yr_sc,PHDI,
         Precip8110Ws,Precip8110Ws_grp_sc,
         HydrAP,hydrap_bin,hydrap_phdi,
         OUTLET_DAMS_red2,damht_zmax_full,
         PctDEVELOPED_Cat,PCT_AG_URB_CAT,PctAgDrainageCat, 
         PctIrrigated.AgLandCat)%>%
  rename(DATE_COL = DATE_COL_iso,
         ECOREG_5 = ECOREG_rev,
         VertDD_m = VertDD_use,
         HorizDD_m = HorizDD_use,
         DpthMx_m = DpthMx_mod,
         OUTLET_DAMS = OUTLET_DAMS_red2,
         damht_zmax = damht_zmax_full)

# Transformed vars: L_EI,L_VertDD_use,L_HorizDD_use,L_LkAreakm2,L_DpthMx_mod,L_SLD,L_inflow,PctDEVELOPED_Cat_logit,PctAgDrainageCat_logit,PctIrrigated.AgLandCat_logit

# SAVE reduced dataset
write.csv(dat_red,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/NLA_0712_for_SciHub.csv",row.names = F)

# WRITE csv of column names for metadata
var_names<-colnames(dat_red)
write.csv(var_names,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/NLA_project_repository/NLA_hydro_driver/data_processed/var_names_SciHub.csv",row.names = F)

