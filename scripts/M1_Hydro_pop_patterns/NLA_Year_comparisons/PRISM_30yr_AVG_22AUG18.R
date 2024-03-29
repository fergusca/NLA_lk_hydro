#####################
## PRISM 30 yr average values
## NLA 2007
##
##  Summarized the 30 yr average values by lake type and ecoregion 
##    for the NLA 2007 lakes using site information (not inferred to population)
##
## 8/22/18
##
#####################

rm(list=ls())

library(ggplot2)

# LOAD DATA
# NLA 2007 data including NAs n=1028 lakes
nla07 <- read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_VISIT_1_WGT_USE.csv") # Dropping obs NLA_07_transformed_SINGLE_USE.csv
## Process data
# Clean up dataset
names(nla07)
todrop <- names(nla07)%in% c("X")
nla07 <- nla07[!todrop]
nr <- nrow(nla07)

###############
# RESAMPLED LAKES IN 2007 and 2012
# LOAD ALL LAKES THAT WERE RESAMPLED 2007 and 2012 - LONG Format
nla07_12 <-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
table(nla07_12$RESAMPLED12)

repeatdf_07 <-subset(nla07_12, YEAR==2007) # n = 348

# DATASET OF RESAMPLED NLA 2007 lakes
nla07_resamp <- subset(nla07, SID %in% repeatdf_07$SID) # n = 348

repeatdf_12 <-subset(nla07_12, YEAR==2012)# n = 348
names(repeatdf_12)



#######
## 30 yr average mean annual precipitaiton (mm) for the lake site point
#######
# By ecoregion and lake origin
#precip<-tapply(nla07$Precip_PT, nla07$WSA9_LO, summary) # only giving the same values for each group
#precip<- aggregate(nla07$Precip_PT, list(nla07$WSA9_LO),mean)

# By ECOREGION
precip<- aggregate(nla07_resamp$Precip_PT, list(nla07_resamp$ECOREG_use),mean)
names(precip)[names(precip)=="Group.1"] <-"Ecoregion"
names(precip)[names(precip)=="x"] <-"Mean Precip(mm)"
write.csv(precip, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_30YR_AVG_PRECIP.csv")

precip_sd<- aggregate(nla07_resamp$Precip_PT, list(nla07_resamp$ECOREG_use),sd)
names(precip_sd)[names(precip_sd)=="Group.1"] <-"Ecoregion"
names(precip_sd)[names(precip_sd)=="x"] <-"SD Precip(mm)"
write.csv(precip_sd, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_30YR_AVG_PRECIP_SD.csv")

#######
## 30 yr average mean annual temperature (degC) for the lake site point
#######
# By ECOREGION
temp<- aggregate(nla07_resamp$TMEAN_PT, list(nla07_resamp$ECOREG_use),mean)
names(temp)[names(temp)=="Group.1"] <-"Ecoregion"
names(temp)[names(temp)=="x"] <-"Mean Temp (degC)"
write.csv(temp, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_30YR_AVG_TEMP.csv")

temp_sd<- aggregate(nla07_resamp$TMEAN_PT, list(nla07_resamp$ECOREG_use),sd)
names(temp_sd)[names(temp_sd)=="Group.1"] <-"Ecoregion"
names(temp_sd)[names(temp_sd)=="x"] <-"SD Temp (degC)"
write.csv(temp_sd, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_30YR_AVG_TEMP_SD.csv")

#######
## Survey year precipitation
#######
# By ecoregion and lake origin
#precip<-tapply(nla07$Precip_PT, nla07$WSA9_LO, summary) # only giving the same values for each group
#precip<- aggregate(nla07$Precip_PT, list(nla07$WSA9_LO),mean)

## NLA 2007 ##
# By ECOREGION
precip_07<- aggregate(repeatdf_07$Precip_mm_total_yr, list(repeatdf_07$ECOREG_use),mean)
names(precip_07)[names(precip_07)=="Group.1"] <-"Ecoregion"
names(precip_07)[names(precip_07)=="x"] <-"Mean Precip(mm)"
write.csv(precip_07, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_WY_PRECIP.csv")

precip_07_sd<- aggregate(repeatdf_07$Precip_PT, list(repeatdf_07$ECOREG_use),sd)
names(precip_07_sd)[names(precip_07_sd)=="Group.1"] <-"Ecoregion"
names(precip_07_sd)[names(precip_07_sd)=="x"] <-"SD Precip(mm)"
write.csv(precip_07_sd, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_WY_PRECIP_SD.csv")

# TEMPERATURE
temp_07<- aggregate(repeatdf_07$TMEAN_PT, list(repeatdf_07$ECOREG_use),mean)
names(temp_07)[names(temp_07)=="Group.1"] <-"Ecoregion"
names(temp_07)[names(temp_07)=="x"] <-"Mean Temp (degC)"
write.csv(temp_07, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_TEMP.csv")

temp_07_sd<- aggregate(repeatdf_07$TMEAN_PT, list(repeatdf_07$ECOREG_use),sd)
names(temp_07_sd)[names(temp_07_sd)=="Group.1"] <-"Ecoregion"
names(temp_07_sd)[names(temp_07_sd)=="x"] <-"SD Temp (degC)"
write.csv(temp_07_sd, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA07_TEMP_SD.csv")

## NLA 2012 ##
# By ECOREGION
precip_12<- aggregate(repeatdf_12$Precip_mm_total_yr, list(repeatdf_07$ECOREG_use),mean)
names(precip_12)[names(precip_12)=="Group.1"] <-"Ecoregion"
names(precip_12)[names(precip_12)=="x"] <-"Mean Precip(mm)"
write.csv(precip_12, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA12_WY_PRECIP.csv")

precip_12_sd<- aggregate(repeatdf_12$Precip_PT, list(repeatdf_07$ECOREG_use),sd)
names(precip_12_sd)[names(precip_12_sd)=="Group.1"] <-"Ecoregion"
names(precip_12_sd)[names(precip_12_sd)=="x"] <-"SD Precip(mm)"
write.csv(precip_12_sd, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA12_WY_PRECIP_SD.csv")

# TEMPERATURE
temp_12<- aggregate(repeatdf_12$TMEAN_PT, list(repeatdf_12$ECOREG_use),mean)
names(temp_12)[names(temp_12)=="Group.1"] <-"Ecoregion"
names(temp_12)[names(temp_12)=="x"] <-"Mean Temp (degC)"
write.csv(temp_12, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA12_TEMP.csv")

temp_12_sd<- aggregate(repeatdf_12$TMEAN_PT, list(repeatdf_12$ECOREG_use),sd)
names(temp_12_sd)[names(temp_12_sd)=="Group.1"] <-"Ecoregion"
names(temp_12_sd)[names(temp_12_sd)=="x"] <-"SD Temp (degC)"
write.csv(temp_12_sd, "M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/NLA12_TEMP_SD.csv")

## ALL RESAMPLED LAKES BY SURVEY YEAR
summary(repeatdf_07$Precip_mm_total_yr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 28.88  599.10  925.80  877.70 1143.00 2041.00 
sd(repeatdf_07$Precip_mm_total_yr)
#[1] 378.2753

summary(repeatdf_07$temp_degC_summer)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 12.64   19.97   21.97   22.28   24.92   34.55 

sd(repeatdf_07$temp_degC_summer)
#[1] 3.628881

########
# 2012
summary(repeatdf_12$Precip_mm_total_yr)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   48.62  518.40  906.00  910.70 1243.00 2729.00
sd(repeatdf_12$Precip_mm_total_yr)
#[1] 462.8191

summary(repeatdf_12$temp_degC_summer)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   10.54   17.68   19.98   20.27   23.04   31.88

sd(repeatdf_12$temp_degC_summer)
# [1] 3.936969