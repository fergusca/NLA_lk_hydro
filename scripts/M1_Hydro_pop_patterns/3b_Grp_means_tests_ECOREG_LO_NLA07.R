###############
## Group means comparison tests
## For NLA 2007 continuous sample population values
##
## Spoke with Tom Kincaid 9/21/17
## The NLA is not a equally probability dataset - so many standard stats tests are not appropriate
## Rather they hand calculate z-scores 
##  using the population mean estimates and standard error values from spsurvey

## z = (|mean1 - mean2|)/[sqrt{(stderror1 ^2)+ stderror2^2)}]
##  Look up p-value using pnorm function
##  Multiply by 2 to get two tail p-value 
## NOTE - may be doing multiple group comparisons - use a lower p significance value to be more conservative (e.g., 0.01)

## 2/2/18 - SUBPOPULATION = ECOREGION and LAKE TYPE
# 2/8/18 - updated with larger 2007 sample set

# 3/22/18 - Look at differences in Median (50Pct) (this will be more comparable with figure box plots)

# 4/12/18 - using scaled drawdown

# 5/14/18 - Using the more full dataset that includes NAs - have more observations for E:I and WRT than before

# 7/9/19 - UPDATED POPULATION WEIGHTS and DATASETS
# Looking to see if there are significant differences among ecoregion groups

#################

rm(list=ls())

#############
## LOAD DATA - Percentile spsurvey output (from Continuous_variable_Estimates_EF_NLA07_07SEPT17.R)
#############
library(reshape2)
library(dplyr)

#p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_CONTINUOUS_percentile_05MAR18.csv")
p<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_CONTINUOUS_percentile_25JUN19.csv")

names(p)
table(p$Type)
table(p$Subpopulation)
table(p$Indicator)

#########
## Subset data "Type" = "WSA9_by_Lake_Origin"
#########
# 2/1/18
p1<-subset(p,Type=="WSA9_by_Lake_Origin")
table(p1$Type)
p1<-droplevels(p1)

# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3) # Takes first three characters in Subpopulation e.g., CPL_MAN-MADE

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13) # takes characters from 5 to 13
table(p1$Lake_Origin_use)
table(p1$ECOREG_use)

#
p1$ECOREG_LO<-p1$Subpopulation
#Order Ecoregions west to east
p1$ECOREG_use <- ordered(p1$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


# Subset data by central tendancy Statistic 
# Median
p2 <-subset(p1,Statistic=="50Pct")
# Pull out Mean statistic
p2 <-subset(p1,Statistic=="Mean")

############
## Scaled metrics 
# 4/12/18
# 5/10/18 - log transformed
# 5/15/18 - Full datasets 

p<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_CONTINUOUS_percentile_SCALED_VERT_NA_10MAY18.csv")#12APR18; NLA07_CONTINUOUS_percentile_22DEC17.csv
#############
# Subset by lake type and ecoregion
p1<-subset(p,Type=="WSA9_by_Lake_Origin")
table(p1$Type)
p1<-droplevels(p1)
# Need to modify Subpopulation to pull out grouping factors
p1$ECOREG_use<-substr(p1$Subpopulation,1,3) # Takes first three characters in Subpopulation e.g., CPL_MAN-MADE

p1$Lake_Origin_use<-substr(p1$Subpopulation,5,13) # takes characters from 5 to 13
table(p1$Lake_Origin_use)
table(p1$ECOREG_use)
#
p1$ECOREG_LO<-p1$Subpopulation
#Order Ecoregions west to east
p1$ECOREG_use <- ordered(p1$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

table(p1$Indicator)

# subset data by central tendancy statistic
p2 <-subset(p1,Statistic=="Mean")

#################
## Mean test function - from Tom 9/22/17, modified by Emi
#################

p2$Indicator <- as.character(p2$Indicator)
p2$ECOREG_use <-as.character(p2$ECOREG_use)
p2$Lake_Origin_use <- as.character(p2$Lake_Origin_use)
p2$ECOREG_LO <-as.character(p2$ECOREG_LO)

# Create a data frame for test results
Results <- data.frame(Indicator=NA,GROUP_1=NA,GROUP_2=NA,  # TWo ecoregion classes
                      NResp_1=NA, NResp_2=NA, Estimate_1=NA,
                      Estimate_2=NA, Difference=NA, StdError=NA, z_Score=NA, 
                      p_Value=NA)

# Calculate confidence bounds, zscores, and pvalues by looping through
# Indicators, Lake Origin, and Ecoregions

r<- 0
for(ind in unique(p2$Indicator)){
  #for(lk in unique(p2$Lake_Origin_use)){
  tempdf<-droplevels(subset(p2, Indicator==ind)) # & Lake_Origin_use ==lk
  if(nrow(tempdf)>1){
    nlev<- length(unique(tempdf$ECOREG_LO))
    for(i in 1:(nlev-1)){
      for(j in 2:nlev){
        r<-r+1
        diff<- tempdf$Estimate[i] - tempdf$Estimate[j]
        stderr<-sqrt(tempdf$StdError[i]^2 + tempdf$StdError[j]^2)
        tst<-abs(diff)/stderr
        pval<-2*(1-pnorm(tst))
        Results[r, 1:11] <- c(ind,tempdf$ECOREG_LO[i],tempdf$ECOREG_LO[j], #lk,
                              tempdf$NResp[i],
                              tempdf$NResp[j],tempdf$Estimate[i],
                              tempdf$Estimate[j], diff, stderr,tst,pval)
      }
    }
  }
}



# 3/22/18 - Median - does not give p-values because cannot estimate std error from single value unlike mean
# 7/9/19 - Updated
write.csv(Results,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/Group_means_tests_NLA07_LO_ECOREG_09JUL19.csv")


