#######################
## NLA RESAMPLED LAKES in 2007 & 2012
##
## Look at change in hydrologic response variables using the sampled lakes
##  This is in response to Alan Herlihy's concerns with the change in the NLA 
##  sample frames between years. Have less confidence in change betwen years bc the pool of lakes that were selected from is so different
##   and changes could be attributed to this and not differences in weather
##
##  8/1/18
##  8/6/18 - Updated with E:I and RT 
##  8/7/18 - paired t-tests
##  8/22/18 - paired t-tests of weather attributes
######################

rm(list=ls())

###########
# Libraries
###########
library(dplyr)
library(ggplot2)

##########
# LOAD DATA
## Resampled lakes - long format (row bound)

nla07_12 <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
# n = 696 observations (348 lakes resampled)

###########
## REFORMAT DATA TO BE WIDE - individual columns for 2007 and 2012 lake hydro response
#     for 1:1 PLOTS
###########

# SUBSET by Year 
nla07 <- subset(nla07_12, YEAR==2007) #348 lakes
nla12 <- subset(nla07_12, YEAR==2012) # 348 lakes

# Reduce number of variables
names(nla07_12)
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","E_I","RT_iso","L_RT_iso","d_excess",
           "HorizDD_use","VertDD_use","L_HorizDD_use","L_VertDD_use",
           "DDHzSqrtA_sc","DDVrtDix_sc_MOD","L_DDHzSqrtA_sc","L_DDVrtDix_sc_MOD",
           "LATdd_use","LONdd_use","Lake_Origin_use","ECOREG_use",
           "Precip_mm_total_yr","temp_degC_summer","Temp_degC_avg_yr","PHDI")

nla07_red <-nla07[myvars]

nla12_red <- nla12[myvars]

# Merge two datasets together by SID
nla07_12_red <- merge(nla07_red,nla12_red, by="SID")

#indx<- match(nla07_red$SID, nla12_red $SID) # Vector of positions of matches of first argument in its second of match based on SID and make same order
#nla12_red  <- nla12_red [indx,]
#nla07_12_red <- droplevels(rbind(nla07_red, nla12_red)) # 696 with 348 lakes sampled both years

################
## WRITE WIDE-FORMATE RESAMPLED LAKES DATASET
################
write.csv(nla07_12_red,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_02AUG18.csv")


############
## Look to see lake connectivity types
############

nla07_full <- read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN.csv") # "M:/Net MyDocuments/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_VISIT_1_WGT_USE.csv"
table(nla07_full$Class3_f)
#     Drainage Drainage-Lake      Isolated 
# 400           328           178
table(nla07_full$lake_type)
#Drainage-Lake_MAN_MADE  Drainage-Lake_NATURAL      Drainage_MAN_MADE 
#205                    123                    257 
#Drainage_NATURAL      Isolated_MAN_MADE       Isolated_NATURAL 
#143                     52                    126 

nla07_resamp_SID <-nla07$SID 

#table(horiz$YEAR)
nla07_conn <- nla07_full[(nla07_full$SID %in% nla07_resamp_SID),] 
table(nla07_conn$Class3_f)
#     Drainage Drainage-Lake      Isolated 
#     136           113            58 

# Count number of observations by connectivity type and ecoregion
library(dplyr)

conn_eco <-nla07_conn %>% 
  group_by(ECOREG_use, Class3_f) %>%
  summarise(n=n())


######################
# LOAD DATA

rsmpl <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_02AUG18.csv")
names(rsmpl)

todrop <- names(rsmpl)%in% c("X")
rsmpl <- rsmpl[!todrop]
table(rsmpl$Lake_Origin_use.x)

########################

###########################################
## PLOT HYDROLOGIC RESPONSE VARIABLE
###########################################

# PLOT SPECIFICATIONS
############
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED")

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Order ecoregions
rsmpl$ECOREG_use.x <- ordered(rsmpl$ECOREG_use.x, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

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


###################
## SOME MODIFICATIONS TO PLOT LOG10 VALUES

# ADDING SAME VALUES THAT WERE ADDED TO THE POPULATION ESTIMATE BOXPLOTS
## ADDING 0.5 m to the values because the level of detecting horizontal drawdown is about 1 meter (this should be the number to adjust the low values to)
##      this is a way just to get rid the space at the lower end of the plot so can see differences better
rsmpl$HorizDD_use.x<-rsmpl$HorizDD_use.x+0.5
rsmpl$HorizDD_use.y<-rsmpl$HorizDD_use.y+0.5

rsmpl$VertDD_use.x<-rsmpl$VertDD_use.x+0.05
rsmpl$VertDD_use.y<-rsmpl$VertDD_use.y+0.05

rsmpl$RT_iso.x<-rsmpl$RT_iso.x+0.01
rsmpl$RT_iso.y<-rsmpl$RT_iso.y+0.01
####################
## Plot 2012 vs 2007 hydrologic response variables
## By lake type (MAYBE? by ecoregion)
## 8/2/18
####################
## VERTICAL DRAWDOWN - LOG 10 TRANSFORMED
# RESERVOIRS
vert_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                aes(x=VertDD_use.x, y=VertDD_use.y,color=ECOREG_use.x))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  scale_x_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
 # ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Vertical DD \n2012")+
  xlab("Vertical DD 2007") #"Log Vertical DD 2007"


# NATURAL LAKES
vert_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
               aes(x=VertDD_use.x, y=VertDD_use.y, color=ECOREG_use.x))+ #, color=ECOREG_use.x
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
#  ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("Vertical DD 2007")
#vert_lk2<-vert_lk + expand_limits(x=-0.1, y=-0.10)

#################################
## HORIZONTAL DRAWDOWN - LOG 10 TRANSFORMED
# RESERVOIRS
horiz_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                   aes(x=HorizDD_use.x, y=HorizDD_use.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,710),breaks=c(1,10,100,500)) + 
  scale_x_continuous(trans="log10",limits=c(NA,710),breaks=c(1,10,100,500)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
 # ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Horizontal DD \n2012")+
  xlab("Horizontal DD 2007")


# NATURAL LAKES
horiz_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
                  aes(HorizDD_use.x, y=HorizDD_use.y, color=ECOREG_use.x))+ #, color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,100),breaks=c(1,10,50)) + 
  scale_x_continuous(trans="log10",limits=c(NA,100),breaks=c(1,10,50)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
#  ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("Horizontal DD 2007")

#################################
## E:I
# RESERVOIRS
EI_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                    aes(x=E_I.x, y=E_I.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  scale_y_continuous()+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
 # ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("E:I \n2012")+
  xlab("E:I 2007")


# NATURAL LAKES
EI_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
                   aes(x=E_I.x, y=E_I.y, color=ECOREG_use.x))+ #, color=ECOREG_use
  scale_y_continuous()+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
 # ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("E:I 2007")


#################################
## Water Residence Time
# RESERVOIRS
RT_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                 aes(x=RT_iso.x, y=RT_iso.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,20),breaks=c(0.1, 1,10)) + 
  scale_x_continuous(trans="log10",limits=c(NA,20),breaks=c(0.1,1,10)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Residence Time \n2012")+
  xlab("Residence Time 2007")


# NATURAL LAKES
RT_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
                aes(x=RT_iso.x, y=RT_iso.y, color=ECOREG_use.x))+ #, color=ECOREG_use
  #scale_y_continuous()+
  scale_y_continuous(trans="log10",limits=c(NA,20),breaks=c(0.1, 1,10)) + 
  scale_x_continuous(trans="log10",limits=c(NA,20),breaks=c(0.1,1,10)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("Residence Time 2007")

##########
## FOR LEGEND
##########
ECO_legend <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                 aes(x=L_RT_iso.x, y=L_RT_iso.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  scale_y_continuous()+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="bottom",
        legend.text=element_text(family="RMN"),
        legend.title=element_blank())+
  ylab("Log Residence Time \n2012")+
  xlab("Log Residence Time 2007")

###########################
## RESAMPLED 1:1 MULTIPLOT FIGURES
###########################

##########
## LAKE TYPE AS COLUMN and RESPONSE AS ROW
tiff(filename="HORIZ_ECO_log_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(horiz_man, horiz_lk, cols=2)
dev.off()

tiff(filename="VERT_ECO_log_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(vert_man, vert_lk, cols=2)
dev.off()

tiff(filename="E_I_ECO_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(EI_man, EI_lk, cols=2)
dev.off()

tiff(filename="RT_ECO_log_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(RT_man, RT_lk, cols=2)
dev.off()

tiff(filename="ECO_LEGEND_02AUG18.tiff",width=4, height=2.25, units="in", res=600)
multiplot(ECO_legend, cols=1)
dev.off()

#################################
## PLOT WEATHER VARIABLES BETWEEN YEARS
#################################
## PHDI
# RESERVOIRS
PHDI_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                 aes(x=PHDI.x, y=PHDI.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  scale_y_continuous(limits=c(NA,3),breaks=c(-5,-2.5,0,2.5))+
  scale_x_continuous(limits=c(NA,3),breaks=c(-5,-2.5,0,2.5))+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  # ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("PHDI \n2012")+
  xlab("PHDI 2007")


# NATURAL LAKES
PHDI_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
                aes(x=PHDI.x, y=PHDI.y, color=ECOREG_use.x))+ #, color=ECOREG_use
  scale_y_continuous(limits=c(NA,3),breaks=c(-5,-2.5,0,2.5))+
  scale_x_continuous(limits=c(NA,3),breaks=c(-5,-2.5,0,2.5))+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  # ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("PHDI 2007")


##########
## Precipitaiton in Water Year (mm)
# RESERVOIRS
precip_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                   aes(x=Precip_mm_total_yr.x, y=Precip_mm_total_yr.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  scale_y_continuous(limits=c(NA,2750),breaks=c(500,1000,1500,2000,2500))+
  scale_x_continuous(limits=c(NA,2750),breaks=c(500,1000,1500,2000,2500))+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  # ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab("Precipitation (mm) \n2012")+
  xlab("Precipitation (mm) 2007")


# NATURAL LAKES
precip_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
                  aes(x=Precip_mm_total_yr.x, y=Precip_mm_total_yr.y, color=ECOREG_use.x))+ #, color=ECOREG_use
  scale_y_continuous(limits=c(NA,2750),breaks=c(500,1000,1500,2000,2500))+
  scale_x_continuous(limits=c(NA,2750),breaks=c(500,1000,1500,2000,2500))+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  # ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("Precipitation mm 2007")


###############
## SUMMER TEMPERATURE (C)
## MAN_MADE
temp_man <- ggplot(subset(rsmpl, Lake_Origin_use.x=="MAN_MADE"),
                     aes(x=temp_degC_summer.x, y=temp_degC_summer.y, color=ECOREG_use.x))+ # ,color=ECOREG_use
  scale_y_continuous(limits=c(NA,35),breaks=c(10,20,30))+
  scale_x_continuous(limits=c(NA,35),breaks=c(10,20,30))+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  # ggtitle("Man-made")+
  theme_dark(base_size=14) +  # To change to white background - "theme_bw" OR change to dark "theme_dark"
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(expression(atop(paste("Summer temperature ( ", degree,"C)"), " \n 2012")))+
  xlab("Summer temperature 2007")


# NATURAL LAKES
temp_lk <- ggplot(subset(rsmpl, Lake_Origin_use.x=="NATURAL"),
                    aes(x=temp_degC_summer.x, y=temp_degC_summer.y, color=ECOREG_use.x))+ #, color=ECOREG_use
  scale_y_continuous(limits=c(NA,35),breaks=c(10,20,30))+
  scale_x_continuous(limits=c(NA,35),breaks=c(10,20,30))+
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  # ggtitle("Natural")+
  theme_dark(base_size=14) + # To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ylab(NULL)+
  xlab("Summer temperature 2007")

#####################
## MULTIPLOT WEATHER ATTRIBUTES
#####################
tiff(filename="PHDI_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(PHDI_man, PHDI_lk, cols=2)
dev.off()

tiff(filename="Precip_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(precip_man, precip_lk, cols=2)
dev.off()

tiff(filename="Temp_02AUG18.tiff",width=6.5, height=2.25, units="in", res=600)
multiplot(temp_man, temp_lk, cols=2)
dev.off()

##############################################################
## Paired t-test - mean of paired differences significantly different from zero?
##  t-tests http://rcompanion.org/handbook/I_04.html
## 8/7/18
# 8/22/18 - Weather variables
################
## LOAD LONG FORMAT RESAMPLED LAKES
dat <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
names(dat)

myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","E_I","RT_iso","L_RT_iso","d_excess",
           "HorizDD_use","VertDD_use","L_HorizDD_use","L_VertDD_use",
           "LATdd_use","LONdd_use","Lake_Origin_use","ECOREG_use",
           "Precip_mm_total_yr","temp_degC_summer","Temp_degC_avg_yr","PHDI")

dat_red <-dat[myvars]

# Modify YEAR variable to order from 2012 to 2007
table(dat_red$YEAR)
dat_red$YEAR_m <-factor(dat_red$YEAR, labels=c("2007","2012"))
table(dat_red$YEAR_m)

# Relabel Year so that 2012 will be first
levels(dat_red$YEAR_m) <- list("a_2012" = c("2012"),"b_2007"=c("2007"))


dat_red <- dat_red[order(dat_red$YEAR_m,dat_red$SID),]
head(dat_red$SID)
tail(dat_red$YEAR_m)

#library(psych)
#headTail(dat_red)

# Check number of paired observations
xtabs(~SID + YEAR, data=dat_red)
#               YEAR
#SID             2007 2012
#NLA06608-0002    1    1
#NLA06608-0004    1    1
#NLA06608-0006    1    1
#NLA06608-0010    1    1

# Looks alright

# Histogram of difference in data
before = dat_red$L_HorizDD_use[dat_red$YEAR=="2007"]
after = dat_red$L_HorizDD_use[dat_red$YEAR=="2012"]
difference = after - before

x=difference
library(rcompanion)
plotNormalHistogram(x, xlab="difference (2012 - 2007)")

#Barplot of differences
# summarize by ecoregion
#test <-group_b(dat_red, ECOREG_use)
#test$
#summarise(test, mean_horz = )
#barplot(difference, col="dark gray",
#        xlab="Observation",
 #       ylab="Difference (2012 - 2007)",
#        names.arg=)


#######################
## PAIRED t-test
#######################

table(dat_red$YEAR_m)
# a_2012 b_2007 
#   348    348

### Log Horizontal DD - # There are 19 obs in 2007 missing horizontal dd
##    BUT HAVE TO MODIFIY DATASET bc MISSING OBSERVATIONS AND NOT EQUAL BETWEEN YEARS
horiz_na<-dat_red[which(is.na(dat_red$L_HorizDD_use)),]
h_na_SID <-horiz_na$SID 

#table(horiz$YEAR)
horiz <- dat_red[!(dat_red$SID %in% h_na_SID),] 
table(horiz$YEAR_m)

t.test(L_HorizDD_use ~ YEAR_m,
       data=horiz,
       paired = TRUE)

horiz.nat <- horiz[which(horiz$Lake_Origin_use=="NATURAL"),]
t.test(L_HorizDD_use ~ YEAR_m,
       data=horiz.nat,
       paired = TRUE)

t.test(HorizDD_use ~ YEAR_m,
       data=horiz,
       paired = TRUE)
# n.s.

t.test(VertDD_use ~ YEAR_m,
       data=horiz,
       paired = TRUE)
#t = -2.387, df = 328, p-value = 0.01755
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.54845837 -0.05287904
#sample estimates:
#  mean of the differences 
#-0.3006687

## Paired t-test
#data:  L_HorizDD_use by YEAR_m
#t = -3.6092, df = 328, p-value = 0.0003551
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.17676629 -0.05204857
#sample estimates:
#  mean of the differences 
#-0.1144074


#### Log Vertical DD
# SAME AS WITH HORIZONTAL DD

t.test(L_VertDD_use ~ YEAR_m,
       data=horiz,
       paired = TRUE)

#	data:  L_VertDD_use by YEAR_m
#t = -10.423, df = 328, p-value < 2.2e-16
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.3581915 -0.2444512
#sample estimates:
#  mean of the differences 
#-0.3013214 

### E:I
summary(dat_red$E_I)
t.test(E_I ~ YEAR_m,
       data=dat_red,
       paired = TRUE)

#	Paired t-test
# data:  E_I by YEAR
#t = 0.067607, df = 347, p-value = 0.9461
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.01841752  0.01972876
#sample estimates:
#  mean of the differences 
#0.0006556183  


# Log WRT
summary(dat_red$L_RT_iso) # 17 NAs
## MODIFY DATSET
rt_na <- dat_red[which(is.na(dat_red$L_RT_iso)),] # 17
rt_na_SID <- rt_na$SID

rt <- dat_red[!(dat_red$SID %in% rt_na_SID),]

t.test(L_RT_iso ~ YEAR_m,
       data=rt,
       paired = TRUE,
       na.action=na.omit) # na.action=na.omit doesn't work because has to be missing both years

t.test(RT_iso ~ YEAR_m,
       data=rt,
       paired = TRUE,
       na.action=na.omit) # na.action=na.omit doesn't work because has to be missing both years
#t = -1.9748, df = 330, p-value = 0.04913
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2359520335 -0.0004546235
#sample estimates:
#  mean of the differences 
#-0.1182033 


# 	Paired t-test
#data:  L_RT_iso by YEAR_m
#t = -1.9333, df = 330, p-value = 0.05406
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.1120203065  0.0009735836
#sample estimates:
#  mean of the differences 
#-0.05552336 


########################
## Perform Paired t-tests by Lake Types 
########################
# https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories

library(dplyr)
library(broom)

table(dat_red$Lake_Origin_use)


############
## HORIZONTAL DD
# Order by year and SID
horiz = horiz[order(horiz$YEAR_m, horiz$SID),]

dt_result = df %>% group_by(lk_type) %>% do(tidy(t.test(horiz~year, data=., paired=TRUE)))

horiz_result = horiz %>% group_by(Lake_Origin_use) %>% do(tidy(t.test(L_HorizDD_use ~ YEAR_m,
                                                                      data=., paired=TRUE)))

write.csv(horiz_result,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_horiz.csv")

############
## VERTICAL DD
# Order by year and SID
# Use horiz dataset bc removed NAs
#horiz = horiz[order(horiz$YEAR, horiz$SID),]

vert_result = horiz %>% group_by(Lake_Origin_use) %>% do(tidy(t.test(L_VertDD_use ~ YEAR,
                                                                      data=., paired=TRUE)))

write.csv(vert_result,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_vert.csv")


############
## E:I
# Order by year and SID
dat_red = dat_red[order(dat_red$YEAR, dat_red$SID),]

EI_result = dat_red %>% group_by(Lake_Origin_use) %>% do(tidy(t.test(E_I ~ YEAR,
                                                                     data=., paired=TRUE)))

write.csv(EI_result,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_EI.csv")


############
## WRT
# Order by year and SID
rt = rt[order(rt$YEAR, rt$SID),]

WRT_result = rt %>% group_by(Lake_Origin_use) %>% do(tidy(t.test(L_RT_iso ~ YEAR,
                                                                     data=., paired=TRUE)))

write.csv(WRT_result,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_WRT.csv")

###############################
#############################
## GROUP BY LAKE TYPE AND BY ECOREGION
##  PAIRED t-tests
#############################

## HORIZONTAL DD
# Order by year and SID
horiz = horiz[order(horiz$YEAR_m, horiz$SID),]

horiz_result_eco = horiz %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(L_HorizDD_use ~ YEAR_m,
                                                                      data=., paired=TRUE)))

write.csv(horiz_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_horiz.csv")


# Trying out non-transformed
horiz_result_eco_nt = horiz %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(HorizDD_use ~ YEAR_m,
                                                                               data=., paired=TRUE)))
write.csv(horiz_result_eco_nt,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_horiz_NT.csv")

############
## VERTICAL DD
# Order by year and SID
# Use horiz dataset bc removed NAs
#horiz = horiz[order(horiz$YEAR, horiz$SID),]

vert_result_eco = horiz %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(L_VertDD_use ~ YEAR_m,
                                                                     data=., paired=TRUE)))

write.csv(vert_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_vert.csv")

############
## E:I
# Order by year and SID
dat_red = dat_red[order(dat_red$YEAR_m, dat_red$SID),]

EI_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(E_I ~ YEAR_m,
                                                                     data=., paired=TRUE)))

write.csv(EI_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_EI.csv")


############
## WRT
# Order by year and SID
rt = rt[order(rt$YEAR_m, rt$SID),]

WRT_result_eco = rt %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(L_RT_iso ~ YEAR_m,
                                                                 data=., paired=TRUE)))

write.csv(WRT_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_WRT.csv")

################################
## WEATHER ATTRIBUTE DIFFERENCES BETWEEN SURVEY YEARS 
##  FOR LAKES RESAMPLED BOTH YEARS
################################
## 
summary(dat_red$Precip_mm_total_yr)
hist(dat_red$Precip_mm_total_yr)

summary(dat_red$temp_degC_summer)
hist(dat_red$temp_degC_summer)

summary(dat_red$PHDI)
hist(dat_red$PHDI)


# Order by year and SID
dat_red = dat_red[order(dat_red$YEAR_m, dat_red$SID),]

#########
# WY PRECIPITATION
precip_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(Precip_mm_total_yr ~ YEAR_m,
                                                                                     data=., paired=TRUE)))

write.csv(precip_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_PRECIPITATION.csv")

#########
# SUMMER TEMP
temp_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(temp_degC_summer ~ YEAR_m,
                                                                                        data=., paired=TRUE)))

write.csv(temp_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_TEMPERATURE.csv")

#########
# MEAN TEMP FULL YEAR
mean_temp_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(Temp_degC_avg_yr ~ YEAR_m,
                                                                                      data=., paired=TRUE)))

write.csv(mean_temp_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_MEAN_TEMPERATURE.csv")

########
# PHDI
phdi_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(PHDI ~ YEAR_m,
                                                                                      data=., paired=TRUE)))

write.csv(phdi_result_eco,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_PHDI.csv")


############################
## Look at resgression slopes and see if different from one using SE of the slope

# Using wide format data (rsmpl)
# Create column combining Lake Origin and Ecoreg

rsmpl <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_02AUG18.csv")
names(rsmpl)

todrop <- names(rsmpl)%in% c("X")
rsmpl <- rsmpl[!todrop]

rsmpl$ECO_LO <- with (rsmpl, interaction(ECOREG_use.x,Lake_Origin_use.x,sep="_"))
table(rsmpl$ECO_LO)


library(lme4)
# FROM https://www.r-bloggers.com/getting-the-most-of-mix-models-with-random-slopes/

# HORIZONTAL DD RANDOM SLOPES
m1 <-lmer(L_HorizDD_use.y~L_HorizDD_use.x + (1+ L_HorizDD_use.x|ECO_LO), data=rsmpl)
summary(m1)

# Extract fixed effects
a=fixef(m1)

# extract random effects  - random intercept and slopes (L_HorizDD_use.x)
b=ranef(m1, condVar=TRUE)
b

# extract variances of random effects
qq <- attr(b[[1]], "postVar")
qq


e=(sqrt(qq))
e

e=e[2,2,] # access the slope which is stored in column 2 in b[[1]]

# Calculate CIs
liminf = (b[[1]][2]+a[2]) - (e*2)

mean_=(b[[1]][2] +a[2])

limsup = (b[[1]][2] + a[2]) + (e*2)

#########
# VERTICAL DD RANDOM SLOPES
m2 <-lmer(L_VertDD_use.y~L_VertDD_use.x + (1+ L_VertDD_use.x|ECO_LO), data=rsmpl)
summary(m2)

# Extract fixed effects
av=fixef(m2)
# extract random effects  - random intercept and slopes (L_HorizDD_use.x)
bv=ranef(m2, condVar=TRUE)
# extract variances of random effects
qqv <- attr(bv[[1]], "postVar")
ev=(sqrt(qqv))
ev=ev[2,2,] # access the slope which is stored in column 2 in b[[1]]

# Calculate CIs
liminfv = (bv[[1]][2]+av[2]) - (ev*2)

mean_v=(bv[[1]][2] +av[2])

limsupv = (bv[[1]][2] + av[2]) + (ev*2)

###########
## E:I
m3 <-lmer(E_I.y~E_I.x + (1+ E_I.x|ECO_LO), data=rsmpl)
summary(m3)

# Extract fixed effects
ae=fixef(m3)

# extract random effects  - random intercept and slopes (deviation from mean effect)
be=ranef(m3, condVar=TRUE)
be
# extract variances of random effects
qqe <- attr(be[[1]], "postVar")


ee=(sqrt(qqe))
ee=ee[2,2,] # access the slope which is stored in column 2 in b[[1]]

# Calculate CIs
liminfe = (be[[1]][2]+ae[2]) - (ee*2)

mean_e=(be[[1]][2] +ae[2])

limsupe = (be[[1]][2] + ae[2]) + (ee*2)


############
## L_WRT
m4 <-lmer(L_RT_iso.y~L_RT_iso.x + (1+ L_RT_iso.x|ECO_LO), data=rsmpl)
summary(m4)

# Extract fixed effects
aw=fixef(m4)
aw
# extract random effects  - random intercept and slopes (L_HorizDD_use.x)
bw=ranef(m4, condVar=TRUE)
bw
# extract variances of random effects
qqw <- attr(bw[[1]], "postVar")

ew=(sqrt(qqw))

ew=ew[2,2,] # access the slope which is stored in column 2 in b[[1]]

# Calculate CIs
liminfw = (bw[[1]][2]+aw[2]) - (ew*2)

mean_w=(bw[[1]][2] +aw[2])

limsupw = (bw[[1]][2] + aw[2]) + (ew*2)


#############
# Plot betas and errors
#############

### HORIZONTAL DD ###
dotchart(mean_$L_HorizDD_use.x, labels=rownames(mean_), cex=0.5,xlim=c(-0.2,1.10), xlab = "betas",main="Random slope coefficients: HORIZONTAL DD")

# Add CIs
for(i in 1:nrow(mean_)) {
  lines(x=c(liminf[i,1], limsup[i,1]), y = c(i,i))
}
abline(v=1.0, col="red")

### VERTICAL DD ##
dotchart(mean_v$L_VertDD_use.x, labels=rownames(mean_v), cex=0.5,xlim=c(-0.2,1.10), xlab = "betas",main="Random slope coefficients: VERTICAL DD")

# Add CIs
for(i in 1:nrow(mean_v)) {
  lines(x=c(liminfv[i,1], limsupv[i,1]), y = c(i,i))
}
abline(v=1.0, col="red")


### E_I ##
dotchart(mean_e$E_I.x, labels=rownames(mean_e), cex=0.5,xlim=c(0.2,1.30), xlab = "betas",main="Random slope coefficients: E:I")

# Add CIs
for(i in 1:nrow(mean_e)) {
  lines(x=c(liminfe[i,1], limsupe[i,1]), y = c(i,i))
}
abline(v=1.0, col="red")

### Log WRT ##
dotchart(mean_w$L_RT_iso.x, labels=rownames(mean_w), cex=0.5,xlim=c(0,1.10), xlab = "betas",main="Random slope coefficients: WRT")

# Add CIs
for(i in 1:nrow(mean_w)) {
  lines(x=c(liminfw[i,1], limsupw[i,1]), y = c(i,i))
}
abline(v=1.0, col="red")

# Export random slopes of L_HORIZDD
#tiff(filename="M:/Net MyDocuments/a_Water_Level/a_Project_lake_drawdown_patterns/Writing/Revisions/Revisions_external_review_06AUG18/RESAMPLED_LAKES/random_slopes_HORIZ.tiff",
#     width=6, height=4, units="in", res=200)
#horiz
#dev.off()


# Final plot
plot(rsmpl$L_HorizDD_use.y~rsmpl$L_HorizDD_use.x, col = rsmpl$ECO_LO, las = 1)
# Plot each random slope
#abline(a = b[[1]][1,1] + a[1], b=mean_$HorizDD_use.x[1], col = "black")



#m1 <-lmList(L_HorizDD_use.y~L_HorizDD_use.x |ECO_LO, data=rsmpl)
#str(m1)# structure of attribures
#se_slope <-summary(m1)$coef[[4]] # 0.0350

# For variance components
#https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-standard-errors-for-variance-components-from-mixed-models/
library(nlme)
horiz_wide<-rsmpl[!is.na(rsmpl$L_HorizDD_use.x),]
droplevels(horiz_wide)
m2<- lme(L_HorizDD_use.y~L_HorizDD_use.x, data=horiz_wide, random = ~ 1 + L_HorizDD_use.x|ECO_LO)
summary(m2)
intervals(m2)

# See random effects by Ecoregion + lake type
random.effects(m2)
# m2$coefficients$random # Same thing as above

