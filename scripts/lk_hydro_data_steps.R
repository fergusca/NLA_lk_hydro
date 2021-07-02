#########################
## PROCESS DATA FOR PATH ANALYSIS FOR MANUSCRIPT ON DRIVERS OF LAKE HYDROLOGY
##
## 
## 7/2/21 
#########################

remove(list=ls())

library(tidyverse)
library(dplyr)

#######################
## READ FULL DATASET NLA 2007 + 2012 all observations n = 2066
##  Data used to rank lakes by potential anthropogenic hydrologic alteration
##    data were processed and created in C:\Users\Owner\Dropbox\z_EmiFergus\a_Work_computer\a_Water_Level\Analysis\a_Lake_managed_class\Rscripts\NLA0712_data_creation_between_years_USE.R

# Relative path from working directory in C:/Users/Owner/Documents
d <- read_csv("~/NLA_hydro/NLA_lk_hydro/data/NLA0712_1ha_opt1_HydrAP_all.csv")
# Absolute path to original data
#d<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA0712_1ha_opt1_HydAP_v9_RESAMPLED.csv")
todrop<-names(d)%in%c("X","X.2") 
d<-d[!todrop]

## RENAME HydrAP
## Rename columns - "old" to "new"
names(d)[names(d)=="hap_rank_9"]<-"HydrAP"

# Calculate variables of interest
# Lake area/ depth
d$LA_zmax <-d$LkArea_m2/d$DpthMx_mod
#hist(log10(d$LA_zmax))
d$L_LA_zmax <-log10(d$LA_zmax)

# Dynamic sedimentation ratio {Hakanson & Jansson dynamic sediment ratio (1983) (sqrt lake area/mean depth)}
#  Estimated area of lake bottom subject to resuspension of sediments
d$dsr <- sqrt(d$LkArea_m2)/d$DpthMx_mod
d$L_dsr <-log10(d$dsr)

# Transformed lake evaporation estimate from runoff
d$L_E_lk_modeled<-log10(d$E_lk+0.001)
#hist(d$L_E_lk_modeled)

# Transformed lake evaporaiton estimate from isotope
d$L_E_lk_iso <-log10(d$E_lk_iso+0.001)
#hist(d$L_E_lk_iso)
#summary(d$L_E_lk_iso)

#######################
## CREATE DATASET WITH One observation per lake - dropping resampled lakes in 2012

##########
## OPTION 1) NLA07 (full) + unique NLA12 lakes n = 1716 >=1ha ; 1629 >=4ha 
## DIVIDE FULL DATASET BY YEAR AGAIN
nla07_b <- d %>%
  filter(YEAR==2007)

nla12_b <- d %>%
  filter(YEAR==2012)

## SUBSET OF UNIQUE NLA12 lakes n=688 lakes  in 2012
nla12_uniq_b<-nla12_b[!nla12_b$SID %in% nla07_b$SID,c(1:478)]#include all columns

# SUBSET OF UNIQUE NLA07 lakes n=678 lakes in 2007
nla07_uniq_b<-nla07_b[!nla07_b$SID %in% nla12_b$SID,c(1:478)]

# ROW COMBINE DATASETS (all 2007 + unique 2012)
dat<-rbind(nla07_b, nla12_uniq_b) 
length(unique(dat$SID)) #1716 
dat<-dat[order(dat$SITE_ID),]
table(dat$YEAR)
#2007 2012 
#1028  688 

# INTERACTION TERM HYDRAP*PHDI
# SCALE VARIABLES OF INTEREST AND THEN TAKE PRODUCT OF THEM
# Grand mean scaled PHDI
dat$PHDI_sc <-scale(dat$PHDI)
#dat$HydrAP_sc <- scale(dat$HydrAP)

dat$hydrap_grand_sc_phdi <- dat$PHDI_sc*dat$HydrAP
summary(dat$hydrap_grand_sc_phdi)

## NON_SCALED PHDI
dat$hydrap_phdi <- dat$PHDI*dat$HydrAP
#summary(dat$hydrap_phdi)
#hist(dat$hydrap_phdi)

## TRANSFORM VARIABLES
dat$L_damht_zmax_full <-log10(dat$damht_zmax_full)
hist(dat$L_damht_zmax_full)


# GRAND MEAN SCALED PRECIPITATION FOR MODIFIED DATASET (subtract observation from grand mean and divide by std dev)
dat$precip_grd_sc = scale(dat$Precip_mm_total_yr)
summary(dat$precip_grd_sc)

# GROUP MEAN SCALED PRECIPITATION BY ECOREGION (scale = centers by mean and divides by std dev within an ecoregion group)
dat <- dat %>%
  group_by(ECOREG_rev) %>%
  mutate(precip_grp_sc = scale(Precip_mm_total_yr, scale=TRUE)) # scaling is the default but just in case
summary(dat$precip_grp_sc)

# INTERACTION VARIABLE HydrAP* GROUP SCALED Precipitation (non-transformed precip in interaction)
dat$hydrap_grp_sc_precip <- dat$HydrAP*dat$precip_grp_sc#d$Precip_mm_total_yr
summary(dat$hydrap_grp_sc_precip)
hist(dat$hydrap_grp_sc_precip)

# INTERACTION VARIABLE HydrAP* GRAND MEAN SCALED Precipitation (non-transformed precip in interaction)
dat$hydrap_grand_sc_precip <- dat$HydrAP*dat$precip_grd_sc#d$Precip_mm_total_yr
summary(dat$hydrap_grand_sc_precip)
hist(dat$hydrap_grand_sc_precip)

#################
## DIFFERENCE IN SUVERY YEAR PRECIPITATION AND LONG_TERM MEAN
dat$precip_diff_30yr = dat$Precip_mm_total_yr - dat$Precip8110Ws
summary(dat$precip_diff_30yr)
hist(dat$precip_diff_30yr)

## VARIANCE IS HIGH FOR TOTAL PRECIPITATION - RESCALE TO HELP WITH MODEL CONVERGENCE (MORESO WITH CFA) (Kline book)
var(dat$Precip_mm_total_yr, na.rm=T) # 177399.4
dat$Precip_mm_total_yr_sc<- dat$Precip_mm_total_yr/1000
var(dat$Precip_mm_total_yr_sc, na.rm=T) # 0.1773994

var(dat$PHDI, na.rm=T) #4.102192
var(dat$L_VertDD_use, na.rm=T) #0.2503395


##############################
## LOW HYDRAP LAKES 0-2 n = 553
low_hydrap <- dat %>%
  filter(HydrAP<3)
table(low_hydrap$HydrAP)
# 0   1   2 
#144 151 258 


##################
## SUBSET BY ECOREGION (AGGREGATED 5)

# WEST n = 429 obs
west<-dat %>%
  filter(ECOREG_rev=="West")

# MIDWEST n = 482 obs
mwest<-dat %>%
  filter(ECOREG_rev=="Midwest")

# GREAT PLAINS n = 292 obs
gplains<-dat %>%
  filter(ECOREG_rev=="Great Plains")

# APPALACHIANS n = 324 obs
apps<-dat %>%
  filter(ECOREG_rev=="Appalachians")

# COASTAL PLAINS n = 189 obs
coastplains<-dat %>%
  filter(ECOREG_rev=="Coastal Plains")

#######################
## WRITE PROCESSED DATA FILES FOR SEM ANALYSIS
write.csv(low_hydrap, "~/NLA_hydro/NLA_lk_hydro/data_processed/conus_low_hydrap.csv")

write.csv(west, "~/NLA_hydro/NLA_lk_hydro/data_processed/west.csv")
write.csv(mwest, "~/NLA_hydro/NLA_lk_hydro/data_processed/mwest.csv")
write.csv(gplains, "~/NLA_hydro/NLA_lk_hydro/data_processed/gplains.csv")
write.csv(apps, "~/NLA_hydro/NLA_lk_hydro/data_processed/apps.csv")
write.csv(coastplains, "~/NLA_hydro/NLA_lk_hydro/data_processed/cstplains.csv")



