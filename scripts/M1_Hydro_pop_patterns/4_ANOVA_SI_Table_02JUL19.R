#############################
## TWO-WAY ANOVA - to partition variance among lake type and ecoregions
##  Test interaction between lake type and ecoregion
##  On lake drawdown and E:I and WRT
##
## Will perform ANOVA on the sample site data (not population inferred) for each year separately
##   Look for variation in vertical, horizontal drawdown, E:I, and WRT
##       L_HorizDD_use; L_VertDD_use ; E_I; RT (2007) RT_iso (2012)
##     BY Lake_Origin_use; ECOREG_use
##  For JAWRA Supporting INformation Table
##
##  7/2/19
#############################

rm(list=ls())

########
## LOAD DATA 
########
## NLA 2007 n = 1028 observations w/556 variables

nla07 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv") # OLD NLA_07_VISIT_1_WGT_USE.csv 
nla07$ECOREG_use <- ordered(nla07$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

## NLA 2012 - Size adjusted # n=951
nla12 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv") # OLD NLA12_merge_transform_SINGLE_SIZE_ADJUST_USE.csv
nla12$ECOREG_use <- ordered(nla12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


###########################
###########
## ANOVA
#https://www.statmethods.net/stats/anova.html
###########
############
## NLA 2007
############
library(car)
leveneTest(nla07$L_HorizDD_use, interaction(nla07$Lake_Origin_use, nla07$ECOREG_use),
           center=median)
# There is significant variance among groups
lk.anova.2007 <- aov(L_HorizDD_use~Lake_Origin_use*ECOREG_use, data=nla07)
summary(lk.anova.2007)

#                              Df Sum Sq Mean Sq F value   Pr(>F)    
#Lake_Origin_use                1   3.91   3.914   20.68 6.12e-06 ***
#  ECOREG_use                   8  42.12   5.265   27.82  < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   7   9.67   1.382    7.30 1.51e-08 ***
#  Residuals                  943 178.46   0.189                     

# Two-way Interaction Plot
#str(nla07[c(39,355)])
#nla07$ECOREG_use<-factor(nla07$ECOREG_use)
#interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$L_HorizDD_use)
interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$L_HorizDD_use,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Log Horiz",
                 main="Interaction plot")

interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$L_VertDD_use,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Log Vert",
                 main="Interaction plot")

interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$E_I,
                 type="b", col=c(1:2),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="E:I",
                 main="Interaction plot")
interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$L_RT_iso,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Water residence time",
                 main="Interaction plot")


interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$L_DDHzSqrtA_sc,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="SCALED log Horiz",
                 main="Interaction plot")

interaction.plot(nla07$ECOREG_use, nla07$Lake_Origin_use, nla07$L_DDVrtDix_sc_MOD,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="SCALED log Vert",
                 main="Interaction plot")

# Vertical drawdown
anova.vert.2007 <-aov(L_VertDD_use~Lake_Origin_use*ECOREG_use, data=nla07)
summary(anova.vert.2007)
#                             Df Sum Sq Mean Sq F value   Pr(>F)    
# Lake_Origin_use              1  10.39  10.388  67.335 7.48e-16 ***
#  ECOREG_use                   8  35.66   4.458  28.894  < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   7   7.34   1.048   6.795 6.91e-08 ***
#  Residuals                  942 145.32   0.154     

anova.EI.2007 <-aov(E_I~Lake_Origin_use*ECOREG_use, data=nla07)
summary(anova.EI.2007)
#                              Df Sum Sq Mean Sq F value  Pr(>F)    
#Lake_Origin_use               1   4.77   4.769  140.88 < 2e-16 ***
#  ECOREG_use                    8  11.38   1.422   42.01 < 2e-16 ***
#  Lake_Origin_use:ECOREG_use    7   2.44   0.348   10.28 1.7e-12 ***
#  Residuals                  1011  34.22   0.034 

anova.WRT.2007 <-aov(L_RT_iso~Lake_Origin_use*ECOREG_use, data=nla07)
summary(anova.WRT.2007)
#                             Df Sum Sq Mean Sq F value Pr(>F)    
#Lake_Origin_use               1  30.14  30.138 145.919 <2e-16 ***
#ECOREG_use                    8  30.21   3.776  18.283 <2e-16 ***
#Lake_Origin_use:ECOREG_use    7   1.60   0.228   1.105  0.358    
#Residuals                  1004 207.36   0.207 

# SCALED HORIZONTAL DRAWDOWN -log10
anova.horiz.sc.2007 <-aov(L_DDHzSqrtA_sc~Lake_Origin_use*ECOREG_use, data=nla07)
summary(anova.horiz.sc.2007)
#                            Df Sum Sq Mean Sq F value  Pr(>F)    
#Lake_Origin_use              1    5.8   5.754   5.702 0.01715 *  
#  ECOREG_use                   8  131.1  16.383  16.235 < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   7   25.9   3.693   3.659 0.00066 ***
#  Residuals                  943  951.6   1.009           


# SCALED VERTICAL DRAWDOWN - log10
anova.vert.sc.2007 <-aov(L_DDVrtDix_sc_MOD~Lake_Origin_use*ECOREG_use, data=nla07)
summary(anova.vert.sc.2007)
#                              Df Sum Sq Mean Sq F value   Pr(>F)    
#  Lake_Origin_use              1  12.37  12.369  48.237 7.04e-12 ***
#  ECOREG_use                   8  36.76   4.595  17.918  < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   7  13.94   1.991   7.764 3.75e-09 ***
#  Residuals                  941 241.29   0.256                


############
## NLA 2012
############
anova.horiz.2012 <- aov(L_HorizDD_use~Lake_Origin_use*ECOREG_use, data=nla12)
summary(anova.horiz.2012)
#                            Df Sum Sq Mean Sq F value  Pr(>F)    
# Lake_Origin_use              1  26.95  26.954 130.268 < 2e-16 ***
#  ECOREG_use                   8  42.81   5.351  25.860 < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   8  13.72   1.715   8.288 6.9e-11 ***
#  Residuals                  913 188.91   0.207  

anova.vert.2012 <- aov(L_VertDD_use~Lake_Origin_use*ECOREG_use, data=nla12)
summary(anova.vert.2012)

#                              Df Sum Sq Mean Sq F value   Pr(>F)    
#  Lake_Origin_use              1  30.75  30.749  157.41  < 2e-16 ***
#  ECOREG_use                   8  32.15   4.019   20.57  < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   8  12.02   1.502    7.69 5.28e-10 ***
#  Residuals                  912 178.16   0.195 

anova.EI.2012 <- aov(E_I~Lake_Origin_use*ECOREG_use, data=nla12)
summary(anova.EI.2012)

#                            Df Sum Sq Mean Sq F value   Pr(>F)    
#Lake_Origin_use              1   1.18  1.1750  18.379 2.00e-05 ***
#  ECOREG_use                   8  13.71  1.7132  26.797  < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   8   2.15  0.2691   4.209 5.73e-05 ***
#  Residuals                  911  58.24  0.0639 

anova.WRT.2012 <- aov(L_RT_iso~Lake_Origin_use*ECOREG_use, data=nla12)
summary(anova.WRT.2012)
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
#Lake_Origin_use              1  10.31  10.310  53.722 5.17e-13 ***
#ECOREG_use                   8  12.05   1.506   7.848 3.13e-10 ***
#Lake_Origin_use:ECOREG_use   8   1.09   0.137   0.712    0.681    
#Residuals                  894 171.56   0.192   

# SCALED HORIZONTAL DRAWDOWN -log10
anova.horiz.sc.2012 <-aov(L_DDHzSqrtA_sc~Lake_Origin_use*ECOREG_use, data=nla12)
summary(anova.horiz.sc.2012)
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
#Lake_Origin_use              1  10.29  10.290  53.845 4.87e-13 ***
# ECOREG_use                   8  12.12   1.514   7.924 2.42e-10 ***
# Lake_Origin_use:ECOREG_use   8   1.09   0.137   0.715    0.679    
#Residuals                  894 170.85   0.191            

# SCALED VERTICAL DRAWDOWN - log10
anova.vert.sc.2012 <-aov(L_DDVrtDix_sc_MOD~Lake_Origin_use*ECOREG_use, data=nla12)
summary(anova.vert.sc.2012)
#                             Df Sum Sq Mean Sq F value   Pr(>F)    
#Lake_Origin_use              1  41.06   41.06  129.77  < 2e-16 ***
#  ECOREG_use                   8  48.48    6.06   19.15  < 2e-16 ***
#  Lake_Origin_use:ECOREG_use   8  15.57    1.95    6.15 9.58e-08 ***
#  Residuals                  907 286.95    0.32      

#########
# NLA 2012 Interaction Plots
interaction.plot(nla12$ECOREG_use, nla12$Lake_Origin_use, nla12$L_HorizDD_use,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Log horizontal dd",
                 main="Interaction plot")

interaction.plot(nla12$ECOREG_use, nla12$Lake_Origin_use, nla12$L_VertDD_use,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Log vertical dd",
                 main="Interaction plot")

interaction.plot(nla12$ECOREG_use, nla12$Lake_Origin_use, nla12$E_I,
                 type="b", col=c(1:2),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="E:I",
                 main="Interaction plot")

interaction.plot(nla12$ECOREG_use, nla12$Lake_Origin_use, nla12$L_RT_iso,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Log Water residence time",
                 main="Interaction plot")

interaction.plot(nla12$ECOREG_use, nla12$Lake_Origin_use, nla12$L_DDHzSqrtA_sc,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="SCALED log horizontal dd",
                 main="Interaction plot")

interaction.plot(nla12$ECOREG_use, nla12$Lake_Origin_use, nla12$L_DDVrtDix_sc_MOD,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="SCALED log vertical dd",
                 main="Interaction plot")


############################
## POSTHOC TUKEY TEST TWO-WAY PAIRWISE COMPARISON TEST
# https://rpubs.com/aaronsc32/post-hoc-analysis-tukey

# NLA 2007
lk.anova.2007 <- aov(L_HorizDD_use~Lake_Origin_use*ECOREG_use, data=nla07)
tukey.test.horiz <- TukeyHSD(lk.anova.2007)
#                                  diff         lwr           upr     p adj
#NATURAL:WMT-MAN_MADE:WMT  -0.494205726 -0.756537068 -0.231874383 0.0000000
#NATURAL:XER-MAN_MADE:XER  -0.218438404 -0.732953868  0.296077061 0.9915365

# WMT man-made lakes have significantly different mean horizontal dd compared to natural lakes in WMT
# NLA 07 Vert
anova.vert.2007 <-aov(L_VertDD_use~Lake_Origin_use*ECOREG_use, data=nla07)
tukey.test.vert <- TukeyHSD(anova.vert.2007 )
#                                  diff         lwr           upr     p adj
#NATURAL:WMT-MAN_MADE:WMT  -0.526256946 -0.76409410 -0.2884197885 0.0000000