###############################
## HydrAP ranks v.9 for all NLA lake sites (VISIT_NO=1)
##  Assigned ranks for all NLA lakes n = 2066 (2007 = 1028 lk; 2012 = 1038 lk)

## 8/11/20 - added revised ZOE drawdown
## 8/28/20 - replaced estimated damht (based on GoogleEarth) with NA

remove(list=ls())

library(dplyr)
library(ggplot2)
library(GGally)
library(ggpubr) # for kruskal wallis test

########################
## READ DATA OPTION all observations from 2007 and 2012
#   n = 2066 obs 
#   Using dataset where ranks were assigned in excel - want to replicate in R

# FOR LAPTOP 
#  NLA 2007+2012 >=1ha Resampled lakes n = 2066
dat<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_z2oe_US.csv",stringsAsFactors = FALSE)
#dat<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/SEM/Data/nla0712/nla0712_1ha_trans_NID_US.csv",stringsAsFactors = FALSE)

todrop<-names(dat)%in%c("X","X.1") 
dat<-dat[!todrop]

dat$PCT_DEVELOPED_BSN<-dat$PCT_DEVELOPED_BSN*100

table(dat$RESAMPLED12_b)
#N    Y 
#1366  700 
table(dat$YEAR)

################
## ADD A NULL VARIABLE FOR RANKING
dat$hap_rank_9 <- "na"


############################
## HYDAP RANKING v. 9b - revised to have irrigated ag up higher in decision tree

##########################
## Lakes rHydAP = 0 - no dams and no land use in watershed 

# Lakes without dams with outlet control structures and with zero LU in the watershed 
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" & dat$PCT_AG_URB_BSN <= 0] <- "0"

table(dat$hap_rank_9)
#0   na 
# 157 1909 

#########################
## Lakes rHydAP = 1 - no dams and minimal land use in direct drainage

##  rHydAP = 1 (LU = 0-10%) 
dat$hap_rank_9[!dat$hap_rank_9==0 & dat$OUTLET_DAMS_red2=="NONE" & dat$PCT_AG_URB_BSN <= 10] <- "1"

###################
## Lakes with some land use
## rHydAP = 2 (Agr & Urb 10-30%) n = 134
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" &  #!dat$hap_rank_9==0 & !dat$hap_rank_9==1 & 
                 dat$PCT_AG_URB_BSN > 10 & dat$PCT_AG_URB_BSN <=30] <- "2"

## rHydAP = 3 (Agr+Urb >30% AND IRRIG AG<=10 AND Ag Drain > 25% ) n = 11
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" & #!dat$hap_rank_9==0 & !dat$hap_rank_9==1 & !dat$hap_rank_9==2 & 
                 dat$PCT_AG_URB_BSN>30 &
                 dat$PctIrrigated.AgLandCat<=10 &
                 dat$PctAgDrainageCat>25] <- "3"

## rHydAP = 5 (Agr+Urb >30% AND IRRIG AG > 10 ) n = 15 
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" & #!dat$hap_rank_9==0 & !dat$hap_rank_9==1 & !dat$hap_rank_9==2 & 
                 dat$PCT_AG_URB_BSN>30 &
                 dat$PctIrrigated.AgLandCat>10] <- "5"


## rHydAP = 4 (Agr+Urb >30% AND IRRIG AG <10 AND Ag Drain<= 25% AND TOTAL AG Adj>50 ) n = 86
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" & #
                 dat$PCT_AG_URB_BSN>30 &
                 dat$PctIrrigated.AgLandCat<=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctAGR_Cat>50] <- "4"

## rHydAP = 2 (Agr+Urb >30% AND IRRIG AG <10 AND Ag Drain<= 25% AND TOTAL AG Adj<50 AND Urb Adj<=25% ) n =149  
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" & #
                 dat$PCT_AG_URB_BSN>30 &
                 dat$PctIrrigated.AgLandCat<=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctAGR_Cat<=50 &
                 dat$PctDEVELOPED_Cat<=25] <- "2"

## rHydAP = 3 (Agr+Urb >30% AND IRRIG AG <10 AND Ag Drain<= 25% AND TOTAL AG Adj<50 AND Urb Adj<=25% ) n =51  
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="NONE" & #
                 dat$PCT_AG_URB_BSN>30 &
                 dat$PctIrrigated.AgLandCat<=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctAGR_Cat<=50 &
                 dat$PctDEVELOPED_Cat>25] <- "3"


table(dat$hap_rank_9)
#   0    1    2    3    4    5   na 
# 157  189  283   62   86   15 1274  

##########################
## 
##  LOW TOPO RELIEF <=200 m and LOW LU <=50% in direct drainage  rHydAP = 2-4 
############
##  rHydAP = 2 (dam ht/zmax =< 0.2)  n =11
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full<=0.2] <- "2"

##########
##  rHydAP = 3 (dam ht/zmax 0.2-0.5) n =9
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>0.2 & dat$damht_zmax_full<=0.5 ] <- "3"

##########
##  rHydAP = 4 (dam ht/zmax 0.5-1.5) n =81
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5 ] <- "4"

##########
##  rHydAP = 5 (dam ht/zmax 1.5-2.5) n = 99
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>1.5 & dat$damht_zmax_full<=2.0 ] <- "5"

##  rHydAP = 6 (dam ht/zmax >2.5) n = 87
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>2.0 & dat$damht_zmax_full<=2.5] <- "6"

##  rHydAP = 7 (dam ht/zmax >2.5) n = 113
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>2.5 ] <- "7"

table(dat$hap_rank_9)
#0   1   2   3   4   5   6   7  na 
#157 189 294  71 167 114  87 113 874 



##########################
## 
##  LOW TOPO RELIEF <=200 m and HIGH LU >50% & High IRRIGA AGin direct drainage  rHydAP = 2-6 
############
##  rHydAP = 5 (dam ht/zmax =< 0.5) n = 2
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat >10 &
                 dat$damht_zmax_full<=0.5] <- "5"

##  rHydAP = 6 (dam ht/zmax = 0.5-1.5) n = 1
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat >10 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5] <- "6"

##  rHydAP = 7 (dam ht/zmax >1.5) n = 8
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat >10 &
                 dat$damht_zmax_full>1.5] <- "7"

table(dat$hap_rank_9)
# 0   1   2   3   4   5   6   7  na 
#157 189 294  71 167 116  88 121 863  

############
## Non-Irrigated agriculture & High Agr Drainage
##  rHydAP = 3 (dam ht/zmax <=0.5) n = 2
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat>25 &
                 dat$damht_zmax_full<=0.5] <- "3"

##  rHydAP = 4 (dam ht/zmax = 0.5-1.5) n = 0
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat>25 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5] <- "4"

##  rHydAP = 5 (dam ht/zmax >1.5 ) #n = 8
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat>25 &
                 dat$damht_zmax_full>1.5] <- "5" # 

table(dat$hap_rank_9)
# 0   1   2   3   4   5   6   7  na 
#157 189 294  73 167 124  88 121 853 

###############
## NON-IRRIGATED AG & LOW Ag DRAINAGE & LOW URB
##  rHydAP = 4 (dam ht/zmax <=0.5) n = 22
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat<=25 &
                 dat$damht_zmax_full<=0.5] <- "4"

##  rHydAP = 5 (dam ht/zmax = 0.5-1.5) n = 12
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat<=25 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5] <- "5"

##  rHydAP = 6 (dam ht/zmax 1.5-2.5 ) #n = 48
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat<=25 &
                 dat$damht_zmax_full>1.5 & dat$damht_zmax_full<=2.5] <- "6" # 

##  rHydAP =7  (dam ht/zmax >2.5 ) #n = 62
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat<=25 &
                 dat$damht_zmax_full>2.5 ] <- "7" #

table(dat$hap_rank_9)
#  0   1   2   3   4   5   6   7  na 
#157 189 294  73 189 136 136 183 709  

################
## HIGH URBAN 
## rHydAP = 3 n = 1
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat>25 &
                 dat$damht_zmax_full<=0.5] <- "3"

##  rHydAP = 4 (dam ht/zmax = 0.5-1.5) n = 3
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat>25 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5] <- "4"

##  rHydAP = 5 (dam ht/zmax >1.5 ) #n = 15
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m<=200 & dat$PCT_AG_URB_BSN > 50 &
                 dat$PctIrrigated.AgLandCat <=10 &
                 dat$PctAgDrainageCat<=25 &
                 dat$PctDEVELOPED_Cat>25 &
                 dat$damht_zmax_full>1.5] <- "5" # 

table(dat$hap_rank_9)
#  0   1   2   3   4   5   6   7  na 
#157 189 294  74 192 151 136 183 690   



########################
##  MOD TOPO RELIEF 200-700m - 
########################  
##  rHydAP = 2 (dam ht/zmax =< 0.2)  # n = 9
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full<=0.2] <- "2"

##  rHydAP = 4 (dam ht/zmax 0.2-0.5)  # n=10
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>0.2 & dat$damht_zmax_full<=0.5] <- "4"

##  rHydAP = 5 (dam ht/zmax 0.5-1.5)  # n=36
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN <= 50 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5] <- "5"

###########
##  rHydAP = 6 (dam ht/zmax 1.5-2.5) n = 68
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN <=50 &
                 dat$damht_zmax_full>1.5 & dat$damht_zmax_full<=2.5] <- "6"

###########
##  rHydAP = 7 (dam ht/zmax >2.5) n = 62
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN <=50 &
                 dat$damht_zmax_full>2.5 ] <- "7"

table(dat$hap_rank_9)
#  0   1   2   3   4   5   6   7  na 
#157 189 303  74 202 187 204 245 505 


###################
##  rHydAP = 5 (dam ht/zmax =< 0.5)  n = 0
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN>50 &
                 dat$damht_zmax_full<=0.5] <- "5"

###########
##  rHydAP = 6 (dam ht/zmax  0.5-1.5) n = 11
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN >50 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5 ] <- "6"
###########
##  rHydAP = 7 (dam ht/zmax >1.5) n = 7
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 200 & dat$elev_relief_m <= 700 &
                 dat$PCT_AG_URB_BSN >50 &
                 dat$damht_zmax_full>1.5 ] <- "7"

table(dat$hap_rank_9)
#  0   1   2   3   4   5   6   7  na 
#157 189 303  74 202 187 215 252 487      

########################
##  HIGH TOPO RELIEF >700m - Dropped the land use classes because have same rankings
########################  
##  rHydAP = 2 (dam ht/zmax =< 0.2)  # n = 11
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 700 & #dat$PCT_AG_URB_CAT <= 50 &
                 dat$damht_zmax_full<=0.2] <- "2"

##  rHydAP = 4 (dam ht/zmax 0.2- 0.5)  # n = 8
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 700 & #dat$PCT_AG_URB_CAT <= 50 &
                 dat$damht_zmax_full>0.2 & dat$damht_zmax_full<=0.5] <- "4"

###########
##  rHydAP = 5 (dam ht/zmax  0.5-1.5) n = 50
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m >700 & #dat$PCT_AG_URB_CAT <=50 &
                 dat$damht_zmax_full>0.5 & dat$damht_zmax_full<=1.5 ] <- "5"
###########
##  rHydAP = 6 (dam ht/zmax >1.5) n = 74
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 700 & #dat$PCT_AG_URB_CAT <= 50 &
                 dat$damht_zmax_full>1.5 & dat$damht_zmax_full<=2.5 ] <- "6"

###########
##  rHydAP = 7 (dam ht/zmax >2.5) n = 72
dat$hap_rank_9[dat$OUTLET_DAMS_red2=="DAM" & 
                 dat$elev_relief_m > 700 & #dat$PCT_AG_URB_CAT <= 50 &
                 dat$damht_zmax_full>2.5] <- "7"

table(dat$hap_rank_9)
#  0   1   2   3   4   5   6   7  na 
#157 189 314  74 210 237 289 324 272 



##############################
## PROCESS DATA
# CHANGE hap_rank_9 "na" to NA
# https://www.statmethods.net/input/missingdata.html
dat$hap_rank_9[dat$hap_rank_9=="na"] <- NA

#table(dat$hap_rank_9)

# Integer to numeric
dat$hap_rank_9<-as.numeric(dat$hap_rank_9)


###########
## LOG10 transform observed/expected habitat condition variables
dat$L1_RVegQc3OE15 <- log10(dat$RVegQc3OE15+0.1)
dat$L1_LitCvrQc3OE15 <- log10(dat$LitCvrQc3OE15+0.1)
dat$L1_LitRipCvrQc3OE15 <- log10(dat$LitRipCvrQc3OE15+0.1)

names(dat)

############################
## REVISE ECOREGIONS - GROUP NPL and SPL & GROUP TPL and UMW
# RENAME LEVELS
dat$ECOREG_rev<-dat$ECOREG_use
dat$ECOREG_rev<-as.factor(dat$ECOREG_rev)
levels(dat$ECOREG_rev) <- list("West"=c("WMT","XER"),"Great Plains"=c("NPL","SPL"), 
                               "Midwest"=c("TPL","UMW"),
                               "Appalachians"=c("SAP","NAP"),"Coastal Plains"=c("CPL"))
dat$ECOREG_rev <-droplevels(dat$ECOREG_rev)

# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                 "Appalachians","Coastal Plains"))
table(dat$ECOREG_rev)

########################
## WRITE DATASET WITH RANKINGS
write.csv(dat,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA0712_1ha_opt1_HydAP_v9_RESAMPLED.csv")

##########################
## WRITE DATASETS BY YEAR FOR POPULATION ESTIMATES n = 2066
d<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA0712_1ha_opt1_HydAP_v9_RESAMPLED.csv")
table(d$hap_rank_9,d$ECOREG_rev)

todrop<-names(d)%in%c("X","X.2") 
d<-d[!todrop]

# 2007 n = 1028
nla07<-d%>%
  filter(YEAR=="2007")

# 2012 n = 1038
nla12 <- d%>%
  filter(YEAR=="2012")

# 2007
write.csv(nla07,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA2007_n1028_HydAP_v9.csv")

# 2012
write.csv(nla12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA2012_n1038_HydAP_v9_RESAMPLED.csv")
