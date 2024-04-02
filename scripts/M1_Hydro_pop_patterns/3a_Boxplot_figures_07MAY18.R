#############
## Create boxplots of distribution of sample population values
##  Using percentile output from spsurvey
## Modified code from Tom Kincaid 8/30/17
## Plotting lake and reservoirs together
## Plot Lake type side by side
## Population estimates by ECOREGION 
## SEPARATE BY YEAR AND BY LAKE TYPE 

# 2/1/18
# 2/8/18 - NLA 2007 - used updated dataset (more samples) that calculated population values
# 3/5/18 - Added Scaled Drawdown variables
# 4/12/18 - Added Scaled Vert DD using Modified lake depth
# 6/30/18 - Changed order Natural, Man-made - DECIDED NOT TO DO THIS
# 7/16/18 - Added a place holder for Natural SAP lakes
# 7/26/18 - Updated E:I and WRT
# 8/2/18  - Updated E:I and WRT after Renee checked 
# 8/6/18 - Added SIZE ADJUSTED NLA 2012
# 7/8/19 - updated WRT and depth classes
##############

rm(list=ls())

###########
# Libraries
###########
library(tidyverse)
library(plyr)
library(ggplot2)
library(reshape2)

library(Hmisc)
library(plotrix)
library(pgirmess)

library(gridExtra)


###########
# Load data - Read in modified datasets that originally came from spsurvey cont.analysis percentile output
#             Datasets have been subset & cast to make percentiles variables
###########

### NLA 2007 ###
# E:I
E_I <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_E_I_percentile_CAST_25JUN19.csv") # Old 18JAN18M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA07/NLA07_LK_ECOREG_E_I_percentile_CAST_08FEB18.csv

# NLA 2007 NON-TRANSFORMED Horizontal DD
Horiz <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_HorizDD_RAW_percentile_CAST_25JUN19.csv")

# NLA 2007 NON-TRANSFORMED Vert DD 
Vert <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_VertDD_RAW_percentile_CAST_25JUN19.csv")

# NLA 2007 WATER RESIDENCE TIME 
WRT <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_WRT_percentile_CAST_25JUN19.csv")

# NLA 2007 NON-TRANSFORMED Horizontal DD
Horiz_sc <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_SCALED_HorizDD_percentile_CAST_25JUN19.csv")

# NLA 2007 NON-TRANSFORMED Vert DD 
Vert_sc <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_SCALED_VertDD_percentile_CAST_25JUN19.csv")

# 4/12/18 - MOdified VertDD
Vert_sc_mod <- read_csv("Routput/pop_calculations/NLA07_LK_ECOREG_SCALED_VertDD_MOD_percentile_CAST_25JUN19.csv")


################
### NLA 2012 SIZE ADJUSTED ###
##  new 6/27/19 updated population weights
##  Old 6/24/19
################
# NLA12 EI
E_I_12_adj <- read_csv("Routput/pop_calculations/NLA12_E_I_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv") # 24JUN19 18JAN18
# NLA 2012 Untransformed Horizontal Drawdown
Horiz_12_adj <- read_csv("Routput/pop_calculations/NLA12_HorizDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")
# NLA 2012 Untransformed Vertical Drawdown
Vert_12_adj <- read_csv("Routput/pop_calculations/NLA12_VertDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")
# NLA 2012 WRT
WRT_12_adj <- read_csv("Routput/pop_calculations/NLA12_WRT_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# NLA 2012 untrans SCALED HORIZDD
Horiz_sc_12_adj <- read_csv("Routput/pop_calculations/NLA12_SCALED_HorizDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# NLA 2012 untrans SCALED VERTDD
Vert_sc_12_adj <- read_csv("Routput/pop_calculations/NLA12_SCALED_VertDD_SIZEADJ_LK_ECO_percentile_CAST_27JUN19.csv")

# NLA 2012 untrans SCALED VERTDD - MODIFIED DEPTH
# 4/12/18
Vert_sc_12_adj_MOD<-read_csv("Routput/pop_calculations/NLA12_LK_ECOREG_SCALED_VertDD_MODIFIED_percentile_CAST_27JUN19.csv")


###########
## ORDER ECOREGIONS
###########
## E_I
E_I$ECOREG_use <- ordered(E_I$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
E_I_12_adj$ECOREG_use <- ordered(E_I_12_adj$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

## Horiz DD - non-transformed
Horiz$ECOREG_use <- ordered(Horiz$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Horiz_12_adj$ECOREG_use <- ordered(Horiz_12_adj$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

## Vert DD - non-transformed
Vert$ECOREG_use <- ordered(Vert$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Vert_12_adj$ECOREG_use <- ordered(Vert_12_adj$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

## WRT 
WRT$ECOREG_use <- ordered(WRT$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
WRT_12_adj$ECOREG_use <- ordered(WRT_12_adj$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

## SCALED Horiz DD
Horiz_sc$ECOREG_use <- ordered(Horiz_sc$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Horiz_sc_12_adj$ECOREG_use <- ordered(Horiz_sc_12_adj$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

# SCALED Vert DD - MODIFIED DEPTH
Vert_sc_mod$ECOREG_use <- ordered(Vert_sc_mod$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Vert_sc_12_adj_MOD$ECOREG_use <- ordered(Vert_sc_12_adj_MOD$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

####
## Clean-up datasets
####

# FOR E_I
todrop <- names(E_I)%in% c("X")
E_I  <- E_I [!todrop]

# Rename Subpopulation
names(E_I)[names(E_I)=="X10Pct"] <- "10Pct"
names(E_I)[names(E_I)=="X25Pct"] <- "25Pct"
names(E_I)[names(E_I)=="X50Pct"] <- "50Pct"
names(E_I)[names(E_I)=="X5Pct"] <- "5Pct"
names(E_I)[names(E_I)=="X75Pct"] <- "75Pct"
names(E_I)[names(E_I)=="X90Pct"] <- "90Pct"
names(E_I)[names(E_I)=="X95Pct"] <- "95Pct"


# NLA12 SIZE ADJUSTED DATASET
todrop <- names(E_I_12_adj)%in% c("X")
E_I_12_adj  <- E_I_12_adj [!todrop]

# Rename Subpopulation
names(E_I_12_adj)[names(E_I_12_adj)=="X10Pct"] <- "10Pct"
names(E_I_12_adj)[names(E_I_12_adj)=="X25Pct"] <- "25Pct"
names(E_I_12_adj)[names(E_I_12_adj)=="X50Pct"] <- "50Pct"
names(E_I_12_adj)[names(E_I_12_adj)=="X5Pct"] <- "5Pct"
names(E_I_12_adj)[names(E_I_12_adj)=="X75Pct"] <- "75Pct"
names(E_I_12_adj)[names(E_I_12_adj)=="X90Pct"] <- "90Pct"
names(E_I_12_adj)[names(E_I_12_adj)=="X95Pct"] <- "95Pct"


# FOR Horiz - non transformed
todrop <- names(Horiz )%in% c("X")
Horiz  <- Horiz [!todrop]

# Rename Subpopulation
names(Horiz)[names(Horiz)=="X10Pct"] <- "10Pct"
names(Horiz)[names(Horiz)=="X25Pct"] <- "25Pct"
names(Horiz)[names(Horiz)=="X50Pct"] <- "50Pct"
names(Horiz)[names(Horiz)=="X5Pct"] <- "5Pct"
names(Horiz)[names(Horiz)=="X75Pct"] <- "75Pct"
names(Horiz)[names(Horiz)=="X90Pct"] <- "90Pct"
names(Horiz)[names(Horiz)=="X95Pct"] <- "95Pct"

## NLA12 SIZE ADJUSTED ##
todrop <- names(Horiz_12_adj )%in% c("X")
Horiz_12_adj  <- Horiz_12_adj [!todrop]

# Rename Subpopulation
names(Horiz_12_adj)[names(Horiz_12_adj)=="X10Pct"] <- "10Pct"
names(Horiz_12_adj)[names(Horiz_12_adj)=="X25Pct"] <- "25Pct"
names(Horiz_12_adj)[names(Horiz_12_adj)=="X50Pct"] <- "50Pct"
names(Horiz_12_adj)[names(Horiz_12_adj)=="X5Pct"] <- "5Pct"
names(Horiz_12_adj)[names(Horiz_12_adj)=="X75Pct"] <- "75Pct"
names(Horiz_12_adj)[names(Horiz_12_adj)=="X90Pct"] <- "90Pct"
names(Horiz_12_adj)[names(Horiz_12_adj)=="X95Pct"] <- "95Pct"


# FOR Vert - non transformed
names(Vert)
todrop <- names(Vert )%in% c("X")
Vert  <- Vert [!todrop]

# Rename Subpopulation
names(Vert)[names(Vert)=="X10Pct"] <- "10Pct"
names(Vert)[names(Vert)=="X25Pct"] <- "25Pct"
names(Vert)[names(Vert)=="X50Pct"] <- "50Pct"
names(Vert)[names(Vert)=="X5Pct"] <- "5Pct"
names(Vert)[names(Vert)=="X75Pct"] <- "75Pct"
names(Vert)[names(Vert)=="X90Pct"] <- "90Pct"
names(Vert)[names(Vert)=="X95Pct"] <- "95Pct"

## NLA12 SIZE ADJUSTED
todrop <- names(Vert_12_adj )%in% c("X")
Vert_12_adj  <- Vert_12_adj [!todrop]

# Rename Subpopulation
names(Vert_12_adj)[names(Vert_12_adj)=="X10Pct"] <- "10Pct"
names(Vert_12_adj)[names(Vert_12_adj)=="X25Pct"] <- "25Pct"
names(Vert_12_adj)[names(Vert_12_adj)=="X50Pct"] <- "50Pct"
names(Vert_12_adj)[names(Vert_12_adj)=="X5Pct"] <- "5Pct"
names(Vert_12_adj)[names(Vert_12_adj)=="X75Pct"] <- "75Pct"
names(Vert_12_adj)[names(Vert_12_adj)=="X90Pct"] <- "90Pct"
names(Vert_12_adj)[names(Vert_12_adj)=="X95Pct"] <- "95Pct"


# FOR WRT
todrop <- names(WRT)%in% c("X")
WRT  <- WRT [!todrop]

# Rename Subpopulation
names(WRT)[names(WRT)=="X10Pct"] <- "10Pct"
names(WRT)[names(WRT)=="X25Pct"] <- "25Pct"
names(WRT)[names(WRT)=="X50Pct"] <- "50Pct"
names(WRT)[names(WRT)=="X5Pct"] <- "5Pct"
names(WRT)[names(WRT)=="X75Pct"] <- "75Pct"
names(WRT)[names(WRT)=="X90Pct"] <- "90Pct"
names(WRT)[names(WRT)=="X95Pct"] <- "95Pct"


## NLA12 SIZE ADJUSTED
todrop <- names(WRT_12_adj )%in% c("X")
WRT_12_adj  <- WRT_12_adj [!todrop]

# Rename Subpopulation
names(WRT_12_adj)[names(WRT_12_adj)=="X10Pct"] <- "10Pct"
names(WRT_12_adj)[names(WRT_12_adj)=="X25Pct"] <- "25Pct"
names(WRT_12_adj)[names(WRT_12_adj)=="X50Pct"] <- "50Pct"
names(WRT_12_adj)[names(WRT_12_adj)=="X5Pct"] <- "5Pct"
names(WRT_12_adj)[names(WRT_12_adj)=="X75Pct"] <- "75Pct"
names(WRT_12_adj)[names(WRT_12_adj)=="X90Pct"] <- "90Pct"
names(WRT_12_adj)[names(WRT_12_adj)=="X95Pct"] <- "95Pct"


# FOR SCALED Horiz
## NLA 2007
todrop <- names(Horiz_sc )%in% c("X")
Horiz_sc  <- Horiz_sc [!todrop]

# Rename Subpopulation
names(Horiz_sc)[names(Horiz_sc)=="X10Pct"] <- "10Pct"
names(Horiz_sc)[names(Horiz_sc)=="X25Pct"] <- "25Pct"
names(Horiz_sc)[names(Horiz_sc)=="X50Pct"] <- "50Pct"
names(Horiz_sc)[names(Horiz_sc)=="X5Pct"] <- "5Pct"
names(Horiz_sc)[names(Horiz_sc)=="X75Pct"] <- "75Pct"
names(Horiz_sc)[names(Horiz_sc)=="X90Pct"] <- "90Pct"
names(Horiz_sc)[names(Horiz_sc)=="X95Pct"] <- "95Pct"

## NLA12 SIZE ADJUSTED
todrop <- names(Horiz_sc_12_adj )%in% c("X")
Horiz_sc_12_adj  <- Horiz_sc_12_adj [!todrop]

# Rename Subpopulation
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X10Pct"] <- "10Pct"
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X25Pct"] <- "25Pct"
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X50Pct"] <- "50Pct"
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X5Pct"] <- "5Pct"
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X75Pct"] <- "75Pct"
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X90Pct"] <- "90Pct"
names(Horiz_sc_12_adj)[names(Horiz_sc_12_adj)=="X95Pct"] <- "95Pct"


# FOR SCALED Vert - MODIFIED DEPTH
## NLA07
names(Vert_sc_mod)
todrop <- names(Vert_sc_mod )%in% c("X")
Vert_sc_mod  <- Vert_sc_mod [!todrop]

# Rename Subpopulation
names(Vert_sc_mod)[names(Vert_sc_mod)=="X10Pct"] <- "10Pct"
names(Vert_sc_mod)[names(Vert_sc_mod)=="X25Pct"] <- "25Pct"
names(Vert_sc_mod)[names(Vert_sc_mod)=="X50Pct"] <- "50Pct"
names(Vert_sc_mod)[names(Vert_sc_mod)=="X5Pct"] <- "5Pct"
names(Vert_sc_mod)[names(Vert_sc_mod)=="X75Pct"] <- "75Pct"
names(Vert_sc_mod)[names(Vert_sc_mod)=="X90Pct"] <- "90Pct"
names(Vert_sc_mod)[names(Vert_sc_mod)=="X95Pct"] <- "95Pct"

## NLA12 SIZE ADJUSTED
names(Vert_sc_12_adj_MOD)
todrop <- names(Vert_sc_12_adj_MOD )%in% c("X")
Vert_sc_12_adj_MOD  <- Vert_sc_12_adj_MOD [!todrop]

# Rename Subpopulation
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X10Pct"] <- "10Pct"
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X25Pct"] <- "25Pct"
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X50Pct"] <- "50Pct"
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X5Pct"] <- "5Pct"
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X75Pct"] <- "75Pct"
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X90Pct"] <- "90Pct"
names(Vert_sc_12_adj_MOD)[names(Vert_sc_12_adj_MOD)=="X95Pct"] <- "95Pct"


###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/BOXPLOTS")

################################################
# Function to plot multiple plots in ggplot
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot<- function(...,plotlist=NULL, file, cols=1, layout=NULL){
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots<-c(list(...), plotlist)
  numPlots = length(plots)
  
  #If layout is NULL, then use'cols' to determine layout
  if(is.null(layout)){
    # Make the panel
    #ncol: Number of columns of plots
    #nrow: Number of rows needed, calculated from # of cols
    layout<- matrix(seq(1, cols* ceiling(numPlots/cols)),
                    ncol=cols, nrow = ceiling(numPlots/cols))
  }
  if(numPlots==1){
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(nrow(layout), ncol(layout))))
    
    #Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


############
## Boxplots for Horizontal DD - 
##  Plotting Man-made and natural on same graph - not using region colors
############

##########################################################
### NLA 2007 ###
############################################################

## ADDING 0.5 m to the values because the level of detecting horizontal drawdown is about 1 meter (this should be the number to adjust the low values to)
##      this is a way just to get rid the space at the lower end of the plot so can see differences better
summary(Horiz)
Horiz$mod_10pct <- Horiz$`10Pct`+0.5#Horiz$mod_10pct <- Horiz$`10Pct`+0.5
summary(Horiz$mod_10pct) 
Horiz$mod_25pct <- Horiz$`25Pct`+0.5 #Horiz$mod_25pct <- Horiz$`25Pct`+0.5
summary(Horiz$mod_25pct)
Horiz$mod_50pct <- Horiz$`50Pct`+0.5
Horiz$mod_75pct <- Horiz$`75Pct`+0.5
Horiz$mod_90pct <- Horiz$`90Pct`+0.5

#   SUBSET by Lake_Origin_use
######################
## RESERVOIRS
horiz_man<-ggplot(subset(Horiz,Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct, # Changed x=factor(ECOREG_use)
                                                                 middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                  fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+ #middle=`50Pct`, upper=`75Pct`, ymax=`90Pct` # 
  scale_y_continuous(trans="log10",limits=c(NA,70),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

###############
## Natural Lakes
# Add a place holder for SAP region 7/16/18
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "Mean","Std..Deviation","Variance","mod_10pct","mod_25pct","mod_50pct",
          "mod_75pct", "mod_90pct")
data <- Horiz[myvars]
data2<- rbind(data, data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                               Lake_Origin_use="NATURAL",Indicator="horiz",
                               Mean=0, Std..Deviation=0, Variance=0, mod_10pct=1000, mod_25pct=1000,
                               mod_50pct=1000, mod_75pct=1000, mod_90pct=1000))

# Updated boxplot with SAP 7/16/18
horiz_lk<-ggplot(subset(data2,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct, # Changed x=factor(ECOREG_use)
                                                               middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                 fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+ #middle=`50Pct`, upper=`75Pct`, ymax=`90Pct` # 
  scale_y_continuous(trans="log10",limits=c(NA,70),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+#"#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#35978f","#01665e"))+ # Dropped SAP color code "#80cdc1"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+


#############
## Vertical DD - Non-transformed
#############
## Add 0.0001 to all percentile estimates so they all shift up by same amount
Vert$mod_10pct <- Vert$`10Pct`+0.05
summary(Vert$mod_10pct)
Vert$mod_25pct <- Vert$`25Pct`+0.05
summary(Vert$mod_25pct)

Vert$mod_50pct <- Vert$`50Pct`+0.05
Vert$mod_75pct <- Vert$`75Pct`+0.05
Vert$mod_90pct <- Vert$`90Pct`+0.05
summary(Vert$mod_90pct)
summary(Vert)

## MAN-MADE
vert_man<-ggplot(subset(Vert,Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct,
                                                               middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                 fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,4),breaks=c(0.05,0.10,0.25,0.5,1.0,1.75,3)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position="bottom",
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Vertical Drawdown (m)"
  xlab(NULL)

# NATURAL
# Add a place holder for SAP region 7/16/18
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "Mean","Std..Deviation","Variance","mod_10pct","mod_25pct","mod_50pct",
          "mod_75pct", "mod_90pct")
data <- Vert[myvars]
data2<- rbind(data, data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                               Lake_Origin_use="NATURAL",Indicator="vert",
                               Mean=0, Std..Deviation=0, Variance=0, mod_10pct=1000, mod_25pct=1000,
                               mod_50pct=1000, mod_75pct=1000, mod_90pct=1000))


vert_lk<-ggplot(subset(data2,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct,
                                                              middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,4),breaks=c(0.05,0.10,0.25,0.5,1.0,1.75,3)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # SAP ="#80cdc1"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position="bottom",
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Vertical Drawdown (m)"
  xlab(NULL)


############
## Boxplots for E:I
############
summary(E_I)

# MAN-MADE
EI_man<-ggplot(subset(E_I, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                             middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
               fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

# NATURAL 7/16/18
# Add a place holder for SAP region
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "10Pct","25Pct","50Pct","75Pct","90Pct",
          "Mean","Std..Deviation","Variance")
data <- E_I[myvars]
# Fake data
data2<-data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                  Lake_Origin_use="NATURAL",Indicator="E_I",
                  `10Pct`=1000, `25Pct`=1000,
                  `50Pct`=1000, `75Pct`=1000, `90Pct`=1000,
                  Mean=0, Std..Deviation=0, Variance=0)
# See if the names are the same between datasets - No
names(data)==names(data2) # not the same for 10Pct
# Create vector of data column names
data_names <- names(data)
# Rename data2 columns to match data columns
names(data2)<- data_names 
data3<-rbind(data,data2)

EI_lk<-ggplot(subset(data3, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                             middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
              fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+#"#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)


#############
## Water Residence Time (yr)
#############
summary(WRT) # 
WRT$mod_10pct <- WRT$`10Pct`+0.01
WRT$mod_25pct <- WRT$`25Pct`+0.01
WRT$mod_50pct <- WRT$`50Pct`+0.01
WRT$mod_75pct <- WRT$`75Pct`+0.01
WRT$mod_90pct <- WRT$`90Pct`+0.01
summary(WRT$mod_90pct)

## MAN-MADE
wrt_man<-ggplot(subset(WRT, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct, 
                                                              middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),
                fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct, 
                   middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(0.01,6),  breaks=c(0.01,0.05,0.50,5)) + #S
  #  scale_y_continuous(limits=c(0,7), breaks=c(0,2,4,6)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+ #
  xlab(NULL)


## NATURAL
# Add a place holder for SAP region 7/16/18
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "Mean","Std..Deviation","Variance","mod_10pct","mod_25pct","mod_50pct",
          "mod_75pct", "mod_90pct")
data <- WRT[myvars]
data2<- rbind(data, data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                               Lake_Origin_use="NATURAL",Indicator="RT",
                               Mean=0, Std..Deviation=0, Variance=0, mod_10pct=1000, mod_25pct=1000,
                               mod_50pct=1000, mod_75pct=1000, mod_90pct=1000))


wrt_lk<-ggplot(subset(data2, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct, 
                                                              middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),
               fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct, 
                   middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(0.01,6),  breaks=c(0.01,0.05,0.50,5)) + #S
  #  scale_y_continuous(limits=c(0,7), breaks=c(0,2,4,6)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # SAP = "#80cdc1",
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+ #
  xlab(NULL)



############
## Boxplots for SCALED HORIZONTAL DD
############
summary(Horiz_sc)

# MAN-MADE
horiz_sc_man<-ggplot(subset(Horiz_sc, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                        middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                     fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,151),  breaks=c(0,0.05,0.5,5,50)) +
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"scaled horizontal dd"
  xlab(NULL)

# NATURAL 7/16/18
# Add a place holder for SAP region
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "10Pct","25Pct","50Pct","75Pct","90Pct",
          "Mean","Std..Deviation","Variance")
data <- Horiz_sc[myvars]
# Fake data
data2<-data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                  Lake_Origin_use="NATURAL",Indicator="horiz_sc",
                  `10Pct`=1000, `25Pct`=1000,
                  `50Pct`=1000, `75Pct`=1000, `90Pct`=1000,
                  Mean=0, Std..Deviation=0, Variance=0)
# See if the names are the same between datasets - No
names(data)==names(data2) # not the same for 10Pct
# Create vector of data column names
data_names <- names(data)
# Rename data2 columns to match data columns
names(data2)<- data_names 
data3<-rbind(data,data2)


horiz_sc_lk<-ggplot(subset(data3, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                    fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,151),  breaks=c(0,0.05,0.5,5,50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Scaled horizontal"
  xlab(NULL)


############
## Boxplots for SCALED VERTICAL DD - MODIFIED DEPTH
# 4/12/18
############
summary(Vert_sc_mod)

# MAN-MADE
vert_sc_mod_man<-ggplot(subset(Vert_sc_mod, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                              middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                        fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,1.2),  breaks=c(0,0.01,0.1,0.50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"scaled vertical dd"
  xlab(NULL)


# NATURAL 7/16/18
# Add a place holder for SAP region
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "10Pct","25Pct","50Pct","75Pct","90Pct",
          "Mean","Std..Deviation","Variance")
data <- Vert_sc_mod[myvars]
# Fake data
data2<-data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                  Lake_Origin_use="NATURAL",Indicator="vert_sc_mod",
                  `10Pct`=1000, `25Pct`=1000,
                  `50Pct`=1000, `75Pct`=1000, `90Pct`=1000,
                  Mean=0, Std..Deviation=0, Variance=0)
# See if the names are the same between datasets - No
names(data)==names(data2) # not the same for 10Pct
# Create vector of data column names
data_names <- names(data)
# Rename data2 columns to match data columns
names(data2)<- data_names 
data3<-rbind(data,data2)

vert_sc_mod_lk<-ggplot(subset(data3, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                      middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                       fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,1.2),  breaks=c(0,0.01,0.1,0.50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Scaled vertical"
  xlab(NULL)



###########################
## 2007 MULTIPLOT FIGURES
###########################

##########
## LAKE TYPE AS COLUMN and RESPONSE AS ROW
# 2007 
tiff(filename="a_HORIZ_LO_ECOREG_USE_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(horiz_man, horiz_lk, cols=2)
dev.off()

# VERTICAL DRAWDOWN
tiff(filename="a_VERT_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(vert_man, vert_lk, cols=2)
dev.off()

# E:I
tiff(filename="a_EI_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(EI_man, EI_lk, cols=2)
dev.off()

# WRT
tiff(filename="a_WRT_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.4, units="in", res=600)
multiplot(wrt_man, wrt_lk, cols=2)
dev.off()

# SCALED HORIZONTAL DD
tiff(filename="a_SCALED_HORIZ_LO_ECOREG_USE_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(horiz_sc_man, horiz_sc_lk, cols=2)
dev.off()

# # SCALED VERTICAL DRAWDOWN - MODIFIED DEPTH
# 4/12/18
tiff(filename="a_SCALED_VERT_MODIFIED_LO_ECOREG_percentile_NLA07.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(vert_sc_mod_man, vert_sc_mod_lk, cols=2)
dev.off()

#############################################
## NLA 2012 - Size Adjusted
#############################################
# 8/6/18 UPDATED with EI: and RT estimates
# 6/24/19 UPDATED VOLUME and RT estimates
# 6/27/19 UPDATED POP WGTS
# 7/8/19 UPDATED WRT with Zmax from lit for 3 lakes

#############
## HORIZONTAL DD
#############

## NLA12 SIZE ADJUSTED
Horiz_12_adj$mod_10pct <- Horiz_12_adj$`10Pct`+0.5
Horiz_12_adj$mod_25pct <- Horiz_12_adj$`25Pct`+0.5
Horiz_12_adj$mod_50pct <- Horiz_12_adj$`50Pct`+0.5
Horiz_12_adj$mod_75pct <- Horiz_12_adj$`75Pct`+0.5
Horiz_12_adj$mod_90pct <- Horiz_12_adj$`90Pct`+0.5

summary(Horiz_12_adj)

## Man-made
summary(Horiz_12_adj)
horiz_12_adj_man<-ggplot(subset(Horiz_12_adj,Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct,
                                                                               middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                         fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,70),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
        legend.position="none")+
  ylab(NULL)+ #"Horizontal Drawdown (m)"
  xlab(NULL)

### NATURAL
# Add a place holder for SAP region 7/16/18
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "Mean","Std..Deviation","Variance","mod_10pct","mod_25pct","mod_50pct",
          "mod_75pct", "mod_90pct")
data <- Horiz_12_adj[myvars]
data2<- rbind(data, data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                               Lake_Origin_use="NATURAL",Indicator="horiz",
                               Mean=0, Std..Deviation=0, Variance=0, mod_10pct=1000, mod_25pct=1000,
                               mod_50pct=1000, mod_75pct=1000, mod_90pct=1000))

horiz_12_adj_lk<-ggplot(subset(data2,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct,
                                                                      middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                        fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,70),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # Dropped SAP "#80cdc1"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        #legend.title=element_blank(),
        #legend.text=element_text(family="RMN"))+
        legend.position="none")+
  ylab(NULL)+ #"Horizontal Drawdown (m)"
  xlab(NULL)


###########
## VERTICAL DD
###########
# MAN_MADE
## Add 0.0001 to all percentile estimates so they all shift up by same amount
Vert_12_adj$mod_10pct <- Vert_12_adj$`10Pct`+0.05
summary(Vert_12_adj$mod_10pct)
Vert_12_adj$mod_25pct <- Vert_12_adj$`25Pct`+0.05
summary(Vert_12_adj$mod_25pct)

Vert_12_adj$mod_50pct <- Vert_12_adj$`50Pct`+0.05
Vert_12_adj$mod_75pct <- Vert_12_adj$`75Pct`+0.05
Vert_12_adj$mod_90pct <- Vert_12_adj$`90Pct`+0.05
summary(Vert_12_adj$mod_90pct)
summary(Vert_12_adj)

## MAN-MADE
vert_12_adj_man<-ggplot(subset(Vert_12_adj,Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct,
                                                                             middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                        fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,14.6),breaks=c(0.05,0.25,1.0,5,10)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position="bottom",
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Vertical Drawdown (m)"
  xlab(NULL)

# NATURAL
# 7/16/18 Add place holder for SAP
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "Mean","Std..Deviation","Variance","mod_10pct","mod_25pct","mod_50pct",
          "mod_75pct", "mod_90pct")
data <- Vert_12_adj[myvars]
data2<- rbind(data, data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                               Lake_Origin_use="NATURAL",Indicator="vert",
                               Mean=0, Std..Deviation=0, Variance=0, mod_10pct=1000, mod_25pct=1000,
                               mod_50pct=1000, mod_75pct=1000, mod_90pct=1000))

vert_12_adj_lk<-ggplot(subset(data2,Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct,
                                                                     middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),
                       fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct,
                   middle=mod_50pct, upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,14.6),breaks=c(0.05,0.25,1.0,5,10)) + #SIZE ADJUSTED ylim=15 otherwise ylim=7
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # SAP ="#80cdc1"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position="bottom",
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Vertical Drawdown (m)"
  xlab(NULL)


###########
## E:I
###########
# MAN-MADE
EI_12_adj_man<-ggplot(subset(E_I_12_adj, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                           middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                      fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(limits=c(0,1.24), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)

# NATURAL 7/16/18
# Add a place holder for SAP region
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "10Pct","25Pct","50Pct","75Pct","90Pct",
          "Mean","Std..Deviation","Variance")
data <- E_I_12_adj[myvars]
# Fake data
data2<-data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                  Lake_Origin_use="NATURAL",Indicator="E_I",
                  `10Pct`=1000, `25Pct`=1000,
                  `50Pct`=1000, `75Pct`=1000, `90Pct`=1000,
                  Mean=0, Std..Deviation=0, Variance=0)
# See if the names are the same between datasets - No
names(data)==names(data2) # not the same for 10Pct
# Create vector of data column names
data_names <- names(data)
# Rename data2 columns to match data columns
names(data2)<- data_names 
data3<-rbind(data,data2)

summary(E_I_12_adj)
EI_12_adj_lk<-ggplot(subset(data3, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                    middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                     fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(limits=c(0,1.24), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Evaporation:Inflow"
  xlab(NULL)


#############
## Water Residence Time (yr)
#############
summary(WRT) # 
WRT_12_adj$mod_10pct <- WRT_12_adj$`10Pct`+0.01
WRT_12_adj$mod_25pct <- WRT_12_adj$`25Pct`+0.01
WRT_12_adj$mod_50pct <- WRT_12_adj$`50Pct`+0.01
WRT_12_adj$mod_75pct <- WRT_12_adj$`75Pct`+0.01
WRT_12_adj$mod_90pct <- WRT_12_adj$`90Pct`+0.01
summary(WRT_12_adj$mod_90pct)

## MAN-MADE
wrt_12_adj_man<-ggplot(subset(WRT_12_adj, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct, 
                                                                            middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),
                       fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct, 
                   middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(0.01,6),  breaks=c(0.01,0.05,0.50,5)) + #S
  #  scale_y_continuous(limits=c(0,7), breaks=c(0,2,4,6)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+ #
  xlab(NULL)


## NATURAL
# 7/16/18 - place holder for SAP
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "Mean","Std..Deviation","Variance","mod_10pct","mod_25pct","mod_50pct",
          "mod_75pct", "mod_90pct")
data <- WRT_12_adj[myvars]
data2<- rbind(data, data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                               Lake_Origin_use="NATURAL",Indicator="RT",
                               Mean=0, Std..Deviation=0, Variance=0, mod_10pct=1000, mod_25pct=1000,
                               mod_50pct=1000, mod_75pct=1000, mod_90pct=1000))

wrt_12_adj_lk<-ggplot(subset(data2, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=mod_10pct, lower=mod_25pct, 
                                                                     middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),
                      fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=mod_10pct, lower=mod_25pct, 
                   middle=mod_50pct,upper=mod_75pct, ymax=mod_90pct),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(0.01,6),  breaks=c(0.01,0.05,0.50,5)) + #S
  #  scale_y_continuous(limits=c(0,7), breaks=c(0,2,4,6)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # SAP = "#80cdc1",
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+ #
  xlab(NULL)



############
## Boxplots for SCALED HORIZONTAL DD
## SIZE ADJUSTED NLA 2012
############
summary(Horiz_sc_12_adj)

# MAN-MADE
horiz_sc_12_adj_man<-ggplot(subset(Horiz_sc_12_adj, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                                      middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                            fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,0.1),  breaks=c(0,0.001,0.01,0.1)) +
  #scale_y_continuous(limits=c(0,1.0), breaks=c(0,0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"scaled horizontal dd"
  xlab(NULL)

# NATURAL 7/16/18
# Add a place holder for SAP region
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "10Pct","25Pct","50Pct","75Pct","90Pct",
          "Mean","Std..Deviation","Variance")
data <- Horiz_sc_12_adj[myvars]
# Fake data
data2<-data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                  Lake_Origin_use="NATURAL",Indicator="horiz_sc",
                  `10Pct`=1000, `25Pct`=1000,
                  `50Pct`=1000, `75Pct`=1000, `90Pct`=1000,
                  Mean=0, Std..Deviation=0, Variance=0)
# See if the names are the same between datasets - No
names(data)==names(data2) # not the same for 10Pct
# Create vector of data column names
data_names <- names(data)
# Rename data2 columns to match data columns
names(data2)<- data_names 
data3<-rbind(data,data2)

horiz_sc_12_adj_lk<-ggplot(subset(data3, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                          middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                           fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,0.1),  breaks=c(0,0.001,0.01,0.1)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Scaled horizontal"
  xlab(NULL)




############
## Boxplots for SCALED VERTICAL DD - USING MODIFIED DEPTH
############
summary(Vert_sc_12_adj_MOD)

# MAN-MADE
vert_sc_12_adj_MOD_man<-ggplot(subset(Vert_sc_12_adj_MOD, Lake_Origin_use=="MAN_MADE"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                                            middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                               fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,1.15),  breaks=c(0,0.01,0.1,1)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"scaled vertical dd"
  xlab(NULL)

# NATURAL 7/16/18
# Add a place holder for SAP region
myvars<-c("Type","ECOREG_use","Lake_Origin_use","Indicator",
          "10Pct","25Pct","50Pct","75Pct","90Pct",
          "Mean","Std..Deviation","Variance")
data <- Vert_sc_12_adj_MOD[myvars]
# Fake data
data2<-data.frame(Type="WSA9_by_Lake_Origin", ECOREG_use="SAP",
                  Lake_Origin_use="NATURAL",Indicator="vert_sc_mod",
                  `10Pct`=1000, `25Pct`=1000,
                  `50Pct`=1000, `75Pct`=1000, `90Pct`=1000,
                  Mean=0, Std..Deviation=0, Variance=0)
# See if the names are the same between datasets - No
names(data)==names(data2) # not the same for 10Pct
# Create vector of data column names
data_names <- names(data)
# Rename data2 columns to match data columns
names(data2)<- data_names 
data3<-rbind(data,data2)

vert_sc_12_adj_MOD_lk<-ggplot(subset(data3, Lake_Origin_use=="NATURAL"), aes(x=factor(ECOREG_use), ymin=`10Pct`, lower=`25Pct`, 
                                                                             middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),
                              fill=factor(ECOREG_use))+
  geom_boxplot(aes(fill=factor(ECOREG_use),ymin=`10Pct`, lower=`25Pct`, 
                   middle=`50Pct`,upper=`75Pct`, ymax=`90Pct`),stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,1),  breaks=c(0,0.01,0.1,1)) +
  scale_fill_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                               "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+ # "#80cdc1" SAP
  theme_bw(base_size=12)+ #14
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(NULL)+ #"Scaled vertical"
  xlab(NULL)

###########################
## 2012 SIZE ADJUSTED MULTIPLOT FIGURES
###########################

##########
## LAKE TYPE AS COLUMN and RESPONSE AS ROW
# 2012
tiff(filename="a_HORIZ_LO_ECOREG_USE_percentile_SIZE_ADJ_NLA12.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(horiz_12_adj_man, horiz_12_adj_lk, cols=2)
dev.off()

# VERTICAL DRAWDOWN
tiff(filename="a_VERT_LO_ECOREG_percentile_SIZE_ADJ_NLA12.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(vert_12_adj_man, vert_12_adj_lk, cols=2)
dev.off()

# E:I
tiff(filename="a_EI_LO_ECOREG_percentile_SIZE_ADJ_NLA12.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(EI_12_adj_man, EI_12_adj_lk, cols=2)
dev.off()

# WRT
tiff(filename="a_WRT_LO_ECOREG_percentile_SIZE_ADJ_NLA12.tiff",width=6.5, height=2.4, units="in", res=600)
multiplot(wrt_12_adj_man, wrt_12_adj_lk, cols=2)
dev.off()

# SCALED HORIZDD
tiff(filename="a_SCALED_HORIZ_LO_ECOREG_USE_percentile_SIZE_ADJ_NLA12.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(horiz_sc_12_adj_man, horiz_sc_12_adj_lk, cols=2)
dev.off()

# VERTICAL DRAWDOWN - MODIFIED DEPTH
tiff(filename="a_SCALED_VERT_LO_ECOREG_percentile_SIZE_ADJ_MODIFIED_NLA12.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(vert_sc_12_adj_MOD_man, vert_sc_12_adj_MOD_lk, cols=2)
dev.off()

