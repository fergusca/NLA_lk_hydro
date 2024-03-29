#########################
## Change in Lake hydrology and weather between survey years
##  NLA 2012 - NLA 2007 (using resampled lakes)
##
##
##  Plots for STOTEN revisions
##
## 11/15/18

## 12/6/18 - Climate boxplots for resampled lakes and 30yr averages

#########################

remove(list=ls())

###########
# Libraries
###########
library(dplyr)
library(ggplot2)


#################
## READ modified dataset - WIDE form with differences calculated between years 
##  n=348 lakes
#################
nla0712<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_02AUG18.csv")

names(nla0712)
#Dropping variables
todrop<-names(nla0712)%in%c("X","Lake_Origin_use.y","ECOREG_use.y")
nla0712<-nla0712[!todrop]

names(nla0712)

#Rename some columns
names(nla0712)[names(nla0712)=="Lake_Origin_use.x"] <- "Lake_Origin_use"
names(nla0712)[names(nla0712)=="ECOREG_use.x"] <- "ECOREG_use"

# Ecoregion groups so that they are plotted from West to East to match the map
## UPDATED 8/4/17 - changed order of CPL to be most east following feedback
nla0712$ECOREG_use <- ordered(nla0712$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla0712$ECOREG_use)

# WMT XER NPL SPL TPL UMW SAP NAP CPL 
# 43  29  20  47  48  48  38  37  38

#################
## Calculate differences between 2012 and 2007
#################
# Take 2012 value (.y) minus 2007 value (.x) - so zero indicates that 2012 was no different from 2007

# Horizontal drawdown variables
nla0712$HorizDD_DIFF <- nla0712$HorizDD_use.y - nla0712$HorizDD_use.x
nla0712$L_HorizDD_DIFF <- nla0712$L_HorizDD_use.y - nla0712$L_HorizDD_use.x
nla0712$DDHzSqrtA_sc_DIFF <- nla0712$DDHzSqrtA_sc.y - nla0712$DDHzSqrtA_sc.x
nla0712$L_DDHzSqrtA_sc_DIFF <- nla0712$L_DDHzSqrtA_sc.y - nla0712$L_DDHzSqrtA_sc.x

# Vertical drawdown variables
nla0712$VertDD_DIFF <- nla0712$VertDD_use.y - nla0712$VertDD_use.x
nla0712$L_VertDD_DIFF <- nla0712$L_VertDD_use.y - nla0712$L_VertDD_use.x
nla0712$DDVrtDix_sc_DIFF <- nla0712$DDVrtDix_sc_MOD.y - nla0712$DDVrtDix_sc_MOD.x
nla0712$L_DDVrtDix_sc_DIFF <- nla0712$L_DDVrtDix_sc_MOD.y - nla0712$L_DDVrtDix_sc_MOD.x

# E:I
nla0712$E_I_DIFF <- nla0712$E_I.y - nla0712$E_I.x

# CLIMATE
nla0712$Precip_mm_total_yr_DIFF <- nla0712$Precip_mm_total_yr.y -nla0712$Precip_mm_total_yr.x
nla0712$temp_degC_summer_DIFF <- nla0712$temp_degC_summer.y - nla0712$temp_degC_summer.x
nla0712$Temp_degC_avg_yr_DIFF <- nla0712$Temp_degC_avg_yr.y - nla0712$Temp_degC_avg_yr.x
nla0712$PHDI_DIFF <- nla0712$PHDI.y - nla0712$PHDI.x


z<-tapply(nla0712$PHDI.x, nla0712$ECOREG_use,summary)
z2<-tapply(nla0712$PHDI.y, nla0712$ECOREG_use,summary)

capture.output(z, file="M:/Net MyDocuments/a_Water_Level/a_Project_lake_drawdown_patterns/Figures/PHDI_NLA07.txt")
capture.output(z2, file="M:/Net MyDocuments/a_Water_Level/a_Project_lake_drawdown_patterns/Figures/PHDI_NLA12.txt")

# NLA 2007 PHDI
summary(nla0712$PHDI.x)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-4.32500 -1.86700  0.26380 -0.08553  1.57400  4.05400

# NLA 2012 PHDI
summary(nla0712$PHDI.y)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.9350 -1.7020 -0.4454 -0.4506  0.9840  4.0080

# PLOT SPECIFICATIONS
############
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED")

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

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

############################
## CHANGE IN SCALED VERTICAL DRAWDOWN and PHDI - LOG 10 TRANSFORMED
# MAN_MADE
vert_man <- ggplot(subset(nla0712, Lake_Origin_use=="MAN_MADE"),
                   aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF,color=ECOREG_use))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  #geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"Vertical DD")))+
  xlab(expression(paste(Delta,"PHDI"))) #"Log Vertical DD 2007"


# NATURAL LAKES
vert_lk <- ggplot(subset(nla0712, Lake_Origin_use=="NATURAL"),
                  aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF, color=ECOREG_use))+ #, color=ECOREG_use.x
  #scale_y_continuous()+
 # scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
 # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"Vertical DD")))+
  xlab(expression(paste(Delta,"PHDI")))
#vert_lk2<-vert_lk + expand_limits(x=-0.1, y=-0.10)


############
## CHANGE VERT vs PHDI not scaled
# MAN_MADE
vert_man <- ggplot(subset(nla0712, Lake_Origin_use=="MAN_MADE"),
                   aes(x=PHDI_DIFF, y=VertDD_DIFF,color=ECOREG_use))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  #geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"Vertical DD")))+
  xlab(expression(paste(Delta,"PHDI"))) #"Log Vertical DD 2007"


# NATURAL LAKES
vert_lk <- ggplot(subset(nla0712, Lake_Origin_use=="NATURAL"),
                  aes(x=PHDI_DIFF, y=VertDD_DIFF, color=ECOREG_use))+ #, color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"Vertical DD")))+
  xlab(expression(paste(Delta,"PHDI")))


############
## CHANGE HORIZ vs PHDI not scaled
# MAN_MADE
horiz_man <- ggplot(subset(nla0712, Lake_Origin_use=="MAN_MADE"),
                   aes(x=PHDI_DIFF, y=HorizDD_DIFF,color=ECOREG_use))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  #geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"Horizontal DD")))+
  xlab(expression(paste(Delta,"PHDI"))) #"Log Vertical DD 2007"


# NATURAL LAKES
horiz_lk <- ggplot(subset(nla0712, Lake_Origin_use=="NATURAL"),
                  aes(x=PHDI_DIFF, y=HorizDD_DIFF, color=ECOREG_use))+ #, color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"Horizontal DD")))+
  xlab(expression(paste(Delta,"PHDI")))

#########################
## CHANGE IN E:I and PHDI
# MAN_MADE
EI_man <- ggplot(subset(nla0712, Lake_Origin_use=="MAN_MADE"),
                   aes(x=PHDI_DIFF, y=E_I_DIFF,color=ECOREG_use))+ # ,color=ECOREG_use
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,30),breaks=c(0.05,0.25,1.0,5,20)) + 
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  #geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI"))) #"Log Vertical DD 2007"


# NATURAL LAKES
EI_lk <- ggplot(subset(nla0712, Lake_Origin_use=="NATURAL"),
                  aes(x=PHDI_DIFF, y=E_I_DIFF, color=ECOREG_use))+ #, color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
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
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI")))


#######################
##
## Scatterplots by ecoregion and lake type
## USE THIS ONE FOR STOTEN SUPPLEMENTARY MATERIAL
## 11/16/18
##
##########

############
# All LAKES - COLORED BY LAKE TYPE - USE THIS ONE ###
EI <- ggplot(nla0712,
                aes(x=PHDI_DIFF, y=E_I_DIFF,color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  #  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
  #                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~ECOREG_use)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #  ggtitle("Natural")+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI")))


vert <- ggplot(nla0712,
                  aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  #  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
  #                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  facet_wrap(~ECOREG_use)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #  ggtitle("Natural")+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position="bottom")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"PHDI")))

##############
# E:I vs PHDI
tiff(filename="DELTA_EI_PHDI_ALL.tiff",width=6, height=4, units="in", res=600)
EI
dev.off()

tiff(filename="DELTA_VERT_PHDI_ALL.tiff",width=6, height=4.2, units="in", res=600)
vert
dev.off()

##############
## SEPARATE PLOTS BY LAKE TYPE - DONT USE
# make separate datasets by lake type
nla_lk <-nla0712[which(nla0712$Lake_Origin_use=="NATURAL"),]
nla_man <- nla0712[which(nla0712$Lake_Origin_use=="MAN_MADE"),]

# NATURAL LAKES
EI_lk <- ggplot(nla_lk,
                aes(x=PHDI_DIFF, y=E_I_DIFF))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("black"))+
#  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
#                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE,colour="black")+
  facet_wrap(~ECOREG_use)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #  ggtitle("Natural")+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ggtitle("Natural")+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI")))


# MANMADE LAKES
EI_man<- ggplot(nla_man,
                aes(x=PHDI_DIFF, y=E_I_DIFF))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("black"))+
  #  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
  #                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE,colour="black")+
  facet_wrap(~ECOREG_use)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #  ggtitle("Natural")+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ggtitle("Man-made")+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI")))

##################
## SCALED VERTICAL DRAWDOWN
# NATURAL LAKES
vert_lk <- ggplot(nla_lk,
                aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("black"))+
  #  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
  #                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE,colour="black")+
  facet_wrap(~ECOREG_use)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #  ggtitle("Natural")+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ggtitle("Natural")+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"PHDI")))


# Man-made LAKES
vert_man <- ggplot(nla_man,
                  aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("black"))+
  #  scale_color_manual(values = c("#8c510a","#bf812d","#dfc27d","#f6e8c3",
  #                                "#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e"))+
  geom_smooth(method=lm, se=FALSE,colour="black")+
  facet_wrap(~ECOREG_use)+
  # geom_abline( slope=1, intercept=0,size=1)+ # 1:1 Line
  #  ggtitle("Natural")+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        #panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  ggtitle("Man-made")+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"PHDI")))





###########################
## RESAMPLED DELTA plots
###########################

##########
## LAKE TYPE AS COLUMN and RESPONSE AS ROW

# E:I vs PHDI
tiff(filename="DELTA_EI_PHDI_NAT.tiff",width=5, height=4, units="in", res=600)
EI_lk
dev.off()

tiff(filename="DELTA_EI_PHDI_MAN.tiff",width=5, height=4, units="in", res=600)
EI_man
dev.off()

# Log10Scaled Vert DD vs PHDI
tiff(filename="DELTA_logVert_scale_PHDI_NAT.tiff",width=5, height=4, units="in", res=600)
vert_lk
dev.off()

tiff(filename="DELTA_logVert_scale_PHDI_MAN.tiff",width=5, height=4, units="in", res=600)
vert_man
dev.off()

#tiff(filename="DELTA_VERT_PHDI.tiff",width=6.5, height=2.25, units="in", res=600)
#multiplot(vert_man, vert_lk, cols=2)
#dev.off()


##########################
## MAP WHERE RESAMPLED LAKES ARE

nla07_12 <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
names(nla07_12)


library(maps)
library(mapdata)
library(scales)
library(maptools)
library(ggplot2)
library(ggmap)

####################
## MAPS using library(map) base maps
state<-map_data("state")

gg1<-ggplot()+
  geom_polygon(data=state, aes(x=long, y=lat, group=group),
               fill=NA, color="black")+
  coord_fixed(1.3)

## NATURAL LAKES NLA 2007
gg1+geom_point(data=nla07_12, aes(x=LONdd_use, y=LATdd_use,
                                   color=YEAR))+ #Drawdown_CONDus15
  coord_fixed(1.3)+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.position = "bottom")

#################
## MAP PHDI by YEAR
nla0712<-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_02AUG18.csv")

names(nla0712)
#Dropping variables
todrop<-names(nla0712)%in%c("X","Lake_Origin_use.y","ECOREG_use.y")
nla0712<-nla0712[!todrop]

names(nla0712)

#Rename some columns
names(nla0712)[names(nla0712)=="Lake_Origin_use.x"] <- "Lake_Origin_use"
names(nla0712)[names(nla0712)=="ECOREG_use.x"] <- "ECOREG_use"

####################
## MAP PHDI
state<-map_data("state")

gg1<-ggplot()+
  geom_polygon(data=state, aes(x=long, y=lat, group=group),
               fill=NA, color="black")+
  coord_fixed(1.3)

## NLA PHDI 2007
gg1+geom_point(data=nla0712, aes(x=LONdd_use.x, y=LATdd_use.x,
                                  color=PHDI.x))+ #2007
  coord_fixed(1.3)+
  scale_colour_gradient(low="red",high="white")+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.position = "bottom")

## NLA PHDI 2012
gg1+geom_point(data=nla0712, aes(x=LONdd_use.x, y=LATdd_use.x,
                                 color=PHDI.y))+ #2007
  coord_fixed(1.3)+
  scale_colour_gradient(low="red",high="white")+
  theme_bw()+
  theme(axis.text=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.title=element_blank(),
        legend.position = "bottom")


########################################
#########################################
## BOXPLOTS CLIMATE VARIABLES RESAMPLED LAKES
## BY LAKE TYPE AND YEAR
## 12/6/18
#########################################
##########
# LOAD DATA

## Resampled lakes - long format (row bound)
nla07_12 <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
# n = 696 observations (348 lakes resampled)
names(nla07_12)
length(unique(nla07_12$SID)) # USE SID for UNIQUE LAKE CODE

# 30yr PRISM data (Merged NLA2007 + lakeCat dataset with 30yr PRISM average)
nla07_lkcat<- read.csv("M:/Net MyDocuments/a_Water_Level/Data/NLA_LakeCat/nla07_lkcat.csv")
names(nla07_lkcat)

# REDUCE dataset to PRISM 30yr avg and SID
nla07_lkcat_red<-nla07_lkcat[,c("SID","Temp_degC_avg_yr","Precip_mm_total_yr","Precip8110Ws","Tmax8110Ws","Tmean8110Ws","Tmin8110Ws")]

# try and merge datatogether
test<-merge(nla07_12,nla07_lkcat_red, by="SID")

# Subset data for variables of interest - SID, Total Precip, Mean temperature
survey_yr<-test[,c("SID","YEAR","Lake_Origin_use","ECOREG_use","Precip_mm_total_yr","Temp_degC_avg_yr")]
longterm_clim<-test[,c("SID","YEAR","Lake_Origin_use","ECOREG_use","Precip8110Ws","Tmean8110Ws")]

# SELECT JUST 2007 year data from longterm_clim
longterm_clim<-longterm_clim[which(longterm_clim$YEAR=="2007"),]
# RENAME LONGTERM PRISM (30yr avg) YEAR to "30yr_AVG"
longterm_clim$YEAR="30yr_AVG"
table(longterm_clim$YEAR)

# RENAME COLUMNS to match survey year names
names(longterm_clim)[names(longterm_clim)=="Precip8110Ws"] <- "Precip_mm_total_yr"
names(longterm_clim)[names(longterm_clim)=="Tmean8110Ws"] <- "Temp_degC_avg_yr"

### COMBINE REDUCED CLIMATE DATASETS
clim_yr_avg<-rbind(survey_yr,longterm_clim)

## RESAMPLED LAKES (n=348) - See Climate boxplot.R code for original ## 8/23/18
##############################



#########
# Process dataset
#########

table(clim_yr_avg$ECOREG_use)
clim_yr_avg$ECOREG_use <- ordered(clim_yr_avg$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(clim_yr_avg$ECOREG_use)

###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

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

# ggplot legend code
#http://ggplot2.tidyverse.org/reference/theme.html

##########
## Boxplot by Ecoregion and Lake type
##########

# NATURAL LAKES
lk<-subset(clim_yr_avg, Lake_Origin_use=="NATURAL")
lk<-subset(lk, ECOREG_use!="SAP")
# RESERVOIRS
man<-subset(clim_yr_avg, Lake_Origin_use=="MAN_MADE")

## PRECIPITATION - total water year (mm)

summary(nla07_12$Precip_mm_total_yr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#28.88  553.60  913.60  894.20 1190.00 2729.00

######
# PRECIP by LAKE TYPE
precip_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
                  fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0","#FFD700"))+ #"#FF0000"red "#b2df8a","#1f78b4"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ggtitle("Natural")+
  ylab (NULL)+#("Precipitation (mm)")+ #
  xlab(NULL)#

# RESERVOIR
precip_man<-ggplot(man, aes(x=factor(ECOREG_use), y = Precip_mm_total_yr),
                   fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Precip_mm_total_yr))+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(25,3700),  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0","#FFD700"))+ #"#b2df8a","#1f78b4"
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ggtitle("Man-made")+
  ylab("Precipitation (mm)")+ 
  xlab(NULL)#

#######
## Temperature - MEAN temperature - by LAKE TYPE
#summary(nla07_12$Temp_degC_avg_yr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   10.54   18.65   20.90   21.27   24.11   34.55 

# NATURAL LAKES
temp_mean_lk<-ggplot(lk, aes(x=factor(ECOREG_use), y = Temp_degC_avg_yr),
                       fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Temp_degC_avg_yr))+#,stat="identity")+
 # scale_y_continuous(limits=c(9,26.9),  breaks=c(10,15,20,25)) +
  #scale_y_continuous(trans="log10",limits=c(9,35),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0","#FFD700"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position=c(0.90,0.29),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Natural lake")+
  ylab(NULL) +#(expression(paste("Mean temperature ( ",degree,"C)")))+ #
  xlab(NULL)#

# RESERVOIRS
temp_mean_man<-ggplot(man, aes(x=factor(ECOREG_use), y = Temp_degC_avg_yr),
                        fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Temp_degC_avg_yr))+#,stat="identity")+
  #scale_y_continuous(limits=c(9,35),  breaks=c(10,15,20,25,30,35)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0","#FFD700"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+ # legend.position=c(0.90,0.29),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Reservoir")+
  ylab (expression(paste("Mean temperature ( ",degree,"C)")))+ #
  xlab(NULL)#


## GET LEGEND
legend<-ggplot(man, aes(x=factor(ECOREG_use), y = Temp_degC_avg_yr),
                      fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=Temp_degC_avg_yr))+#,stat="identity")+
  #scale_y_continuous(limits=c(9,35),  breaks=c(10,15,20,25,30,35)) +
  #scale_y_continuous(trans="log10",limits=c(9,37),  breaks=c(10,15,30)) +
  scale_fill_manual(values = c("#808080","#E0E0E0","#FFD700"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"))+
    #ggtitle("Reservoir")+
  ylab (expression(paste("Mean temperature ( ",degree,"C)")))+ #
  xlab(NULL)#




############
## PHDI - DON"T HAVE 30 YR AVG PHDI
###
lk_prsm<-nla07_12[nla07_12$Lake_Origin_use=="NATURAL",]
man_prsm<- nla07_12[nla07_12$Lake_Origin_use=="MAN_MADE",]

# LAKES
PHDI_lk<-ggplot(lk_prsm, aes(x=factor(ECOREG_use), y = PHDI),
                fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  scale_y_continuous(limits=c(-5.5,4.7), breaks=c(-4.0,-2.0,0,2,4)) +
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Natural lake")+
  ylab(NULL) + #("PHDI Water Year")+
  xlab(NULL)#

PHDI_lk_use<-PHDI_lk + geom_hline(yintercept=0, linetype="dashed", color="black", size=1.5)

# RESERVOIRS
PHDI_man<-ggplot(man_prsm, aes(x=factor(ECOREG_use), y = PHDI),
                 fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=PHDI))+#,stat="identity")+
  scale_y_continuous(limits=c(-5.5,4.7), breaks=c(-4.0,-2.0,0,2,4)) +
  #scale_y_continuous(trans="log10",limits=c(NA,500),  breaks=c(1,10,100,250,500)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #legend.justification=c("right", "top"),
  #legend.box.just = "right",
  #legend.margin=margin(6,6,6,6),
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  #ggtitle("Reservoir")+
  ylab ("PHDI Water Year")+ #
  xlab(NULL)#

PHDI_man_use<-PHDI_man + geom_hline(yintercept=0, linetype="dashed", color="black", size=1.5)

################
## MULTIPLOTS
################
# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

#Precip by LAKE TYPE (Natural, Man-made)
# TOTAL PRECIP
tiff(filename="RESAMPLE_PRECIP_TOTAL_LO_AVG.tiff",width=7, height=3, units="in", res=600)
multiplot(precip_man,precip_lk, cols=2)
dev.off()

#MEAN Temp by LAKE TYPE
tiff(filename="RESAMPLE_MEAN_TEMP_LO_AVG.tiff",width=7, height=3, units="in", res=600)
multiplot(temp_mean_man, temp_mean_lk,cols=2)
dev.off()

#LEGEND
tiff(filename="LEGEND_YR_AVG.tiff",width=7, height=3, units="in", res=600)
multiplot(legend)
dev.off()

# PHDI WATER YEAR BY LAKE TYPE - NOTE: DOES NOT HAVE 30yr AVG
tiff(filename="RESAMPLE_PHDI_WY_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(PHDI_man_use,PHDI_lk_use, cols=2)
dev.off()

########################
## ANOVA by YEAR
# Anova - linear regression wiht ecoregion as predictor variable
summary(lm(lk$Temp_degC_avg_yr~lk$YEAR))

# Bonferroni - NATURAL
pairwise.t.test(lk$Temp_degC_avg_yr,lk$YEAR, 
                paired=FALSE, p.adjust.method ="bonferroni")
 
#           2007    2012   
#2012      0.31736 -      
#30yr_AVG  4e-08   0.00011

# NATURAL LAKE ANOVA with interaction between YEAR and ECOREGION
lk.anova <- aov(Temp_degC_avg_yr~YEAR*ECOREG_use, data=lk)
summary(lk.anova)

#                   Df Sum Sq Mean Sq F value Pr(>F)    
#  YEAR              2    555   277.4  66.535 <2e-16 ***
#  ECOREG_use        7   5120   731.5 175.453 <2e-16 ***
#  YEAR:ECOREG_use  14     56     4.0   0.952  0.502    
#Residuals       426   1776     4.2           


interaction.plot(lk$ECOREG_use, lk$YEAR, lk$Temp_degC_avg_yr)
interaction.plot(lk$ECOREG_use, lk$YEAR, lk$Temp_degC_avg_yr,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Temperature",
                 main="Interaction plot")

# MAN-MADE LAKE ANOVA with interaction between YEAR and ECOREGION
man.anova <- aov(Temp_degC_avg_yr~YEAR*ECOREG_use, data=man)
summary(man.anova)
#                 Df Sum Sq Mean Sq F value Pr(>F)    
#  YEAR              2    948   474.2  52.332 <2e-16 ***
#  ECOREG_use        8   5139   642.3  70.886 <2e-16 ***
#  YEAR:ECOREG_use  16     86     5.4   0.595  0.889    
#Residuals       567   5138     9.1                      

interaction.plot(man$ECOREG_use, man$YEAR, man$Temp_degC_avg_yr)
interaction.plot(man$ECOREG_use, man$YEAR, man$Temp_degC_avg_yr,
                 type="b", col=c(1:2),
                 fun= function(x)mean(x, na.rm=TRUE),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24),
                 xlab="Ecoregions",
                 ylab="Temperature",
                 main="Interaction plot")
plot(TukeyHSD(man.anova))
