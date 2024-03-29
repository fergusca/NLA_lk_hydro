####################
## Change between years
## RESAMPLED LAKES ONLY - NOT POPULATION INFERRED VALUES
## 
## 1/17/18
###################

rm(list=ls())

library(dplyr)
library(plyr)

###########
# LOAD DATA
# Resampled lakes, single observations, long format
# n = 592 observations for 296 lakes sampled in 2007 and 2012
# Data created in "a_data_yr_comparison_2007_2012.R"

nla_all <-read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")

names(nla_all)
table(nla_all$YEAR)

#########
### Preprocess dataset ##
#########
todrop<-names(nla_all)%in%c("X") 
nla_all<-nla_all[!todrop]

# Order Ecoregions
nla_all$ECOREG_use <- ordered(nla_all$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla_all$ECOREG_use)

# Ecoregion + Lake Type
table(nla_all$WSA9_LO) #17 classes - looks good
##################

#######
# Subset dataset by year and then cbind together - WIDE Format dataset
nla_07<- nla_all[which(nla_all$YEAR == 2007),] # 296 obs
nla_12 <- nla_all[which(nla_all$YEAR== 2012),] # 296 obs

# Simplify datasets for now
myvar <- c("SITE_ID","VISIT_NO","SID","YEAR","HorizDD_use","VertDD_use","E_I","RT_iso",
           "LkArea_km2","DpthMx_use","DpthMx_mod","LATdd_use","LONdd_use","XCOORD","YCOORD","STATE","Lake_Origin_use",
           "ECOREG_use","WSA9_LO","SIZE_CLASS",
           "Precip_PT","Precip_mm_total_yr","Precip_mm_avg_yr","precip_mm_winter","precip_mm_spring","precip_mm_summer",
           "Temp_degC_avg_yr","temp_degC_winter","temp_degC_spring","temp_degC_summer")

# Reduced 2007
nla07_red <- nla_07[myvar]

#Reduced 2012
nla12_red <-nla_12[myvar]

# Order by SID
nla07_red <- nla07_red[order(nla07_red$SID),]

nla12_red <- nla12_red[order(nla12_red$SID),]

## RENAME COLUMNS with 2012 data
names(nla12_red)[names(nla12_red)=="HorizDD_use"] <- "HorizDD_use_12"
names(nla12_red)[names(nla12_red)=="VertDD_use"] <- "VertDD_use_12"
names(nla12_red)[names(nla12_red)=="E_I"] <- "E_I_12"
names(nla12_red)[names(nla12_red)=="RT_iso"] <- "RT_iso_12"

names(nla12_red)[names(nla12_red)=="Precip_mm_total_yr"] <- "Precip_mm_total_yr_12"
names(nla12_red)[names(nla12_red)=="Precip_mm_avg_yr"] <- "Precip_mm_avg_yr_12"
names(nla12_red)[names(nla12_red)=="precip_mm_winter"] <- "precip_mm_winter_12"
names(nla12_red)[names(nla12_red)=="precip_mm_spring"] <- "precip_mm_spring_12"
names(nla12_red)[names(nla12_red)=="precip_mm_summer"] <- "precip_mm_summer_12"

names(nla12_red)[names(nla12_red)=="Temp_degC_avg_yr"] <- "Temp_degC_avg_yr_12"
names(nla12_red)[names(nla12_red)=="temp_degC_winter"] <- "temp_degC_winter_12"
names(nla12_red)[names(nla12_red)=="temp_degC_spring"] <- "temp_degC_spring_12"
names(nla12_red)[names(nla12_red)=="temp_degC_summer"] <- "temp_degC_summer_12"

names(nla12_red)
# Double check order
#vars_07 <- nla07_red$SID
#write.csv(vars_07,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/vars_07_resample.csv")

#vars_12 <- nla12_red$SID
#write.csv(vars_12,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/vars_12_resample.csv")

# LOOKS GOOD - OKAY TO CBIND DATA
nla_wide<-cbind(nla07_red,nla12_red)
names(nla_wide)

summary(nla_wide)

# Drop lakes that are missing WRT in 2012 (n= 4)
nla_wide<-nla_wide[which(!is.na(nla_wide$RT_iso_12)),] #drops 81 obs
summary(nla_wide) # 

################
## DATA EXPLORATION
## Alan H. suggested plotting two years against one another to see how correlated they are to one another
## 
#################

# Horizontal DD
plot(nla_wide$HorizDD_use~nla_wide$HorizDD_use_12)
abline(lm(nla_wide$HorizDD_use~nla_wide$HorizDD_use_12))
abline(a=0,b=1,col="red")

# Vertical DD
plot(nla_wide$VertDD_use~nla_wide$VertDD_use_12)
abline(lm(nla_wide$VertDD_use~nla_wide$VertDD_use_12))
abline(a=0,b=1,col="red")

# E:I
plot(nla_wide$E_I~nla_wide$E_I_12)
abline(lm(nla_wide$E_I~nla_wide$E_I_12))
abline(a=0,b=1,col="red")

# WRT
plot(nla_wide$RT_iso~nla_wide$RT_iso_12)
abline(lm(nla_wide$RT_iso~nla_wide$RT_iso_12))
abline(a=0,b=1,col="red")

########
## Calculate change for each of the four hydrology variables
# Take 2012 value and subtract away 2007 value to see how drawdown has changed since 2007
########
nla_wide$horiz_diff <- nla_wide$HorizDD_use_12 - nla_wide$HorizDD_use
nla_wide$vert_diff <- nla_wide$VertDD_use_12 - nla_wide$VertDD_use
nla_wide$EI_diff <- nla_wide$E_I_12 - nla_wide$E_I
nla_wide$RT_iso_diff <- nla_wide$RT_iso_12 - nla_wide$RT_iso

nla_wide$Precip_mm_total_yr_diff <- nla_wide$Precip_mm_total_yr_12 - nla_wide$Precip_mm_total_yr
nla_wide$temp_degC_summer_diff <- nla_wide$temp_degC_summer_12 - nla_wide$temp_degC_summer

###########
# Write WIDE dataset with change values calculated
# 1/22/18 - to plot change values for resampled lakes
########
write.csv(nla_wide,"M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_cbind_WIDE_difference_REVISITS.csv")

############
## Summarize change values by ECOREGION + LAKE ORIGIN
# http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
###########

## Function that summarizes data
#   Gives count, mean, standard deviation, standard error of mean, and confidence interval
#   data: a dataframe
#   measurevar: the name of a column that contains the variable to be sumarized
#   groupvars: a vector containing names of columns that contain grouping variables
#   na.rm: a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the confidence interval (default = 95%)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, 
                      conf.interval=.95, .drop=TRUE){
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function(x, na.rm=FALSE){
    if(na.rm) sum(!is.na(x))
    else length(x)
  }
  
  # This does the summary. For each group's dataframe, return a vector with N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun=function (xx, col){
                   c(N = length2(xx[[col]], na.rm=na.rm),
                     mean = mean (xx[[col]], na.rm=na.rm),
                     sd = sd (xx[[col]], na.rm=na.rm)
                     )
                 },
                 measurevar
                 )
  #Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N) # calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval
  # e.g., if conf.interval = .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se*ciMult
  
  return(datac)
}

## Apply function to Hydrology Variables ##

# HORIZONTAL DD
HORIZ<-summarySE(data=nla_wide, measurevar = "horiz_diff", groupvars=c("WSA9_LO"))

VERT<-summarySE(data=nla_wide, measurevar = "vert_diff", groupvars=c("WSA9_LO"))

EI<-summarySE(data=nla_wide, measurevar = "EI_diff", groupvars=c("WSA9_LO"))

RT<-summarySE(data=nla_wide, measurevar = "RT_iso_diff", groupvars=c("WSA9_LO"))

#cdata<-ddply(nla_wide, c("WSA9_LO"), summarise,
#             N = length(SID),
##             horiz_mean=mean(horiz_diff),
#             vert_mean= mean(Vert_diff),
#             EI_mean=mean(EI_diff),
#             RT_mean=mean(RT_iso_diff))

######################
# Dot plots showing change between years
######################

library(reshape2)
library(dplyr)
library(ggplot2)

library(Hmisc)
library(plotrix)
library(pgirmess)

library(gridExtra)
names(HORIZ)

#####
# Need to modify Subpopulation to pull out grouping factors
####
######
## HORIZ
#######
HORIZ$ECOREG_use<-substr(HORIZ$WSA9_LO,1,3)
table(HORIZ$ECOREG_use)

HORIZ$Lake_Origin_use<-substr(HORIZ$WSA9_LO,5,13)
table(HORIZ$Lake_Origin_use)

# Order Ecoregions
HORIZ$ECOREG_use <- ordered(HORIZ$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

#######
# VERT
#######
VERT$ECOREG_use<-substr(VERT$WSA9_LO,1,3)
table(VERT$ECOREG_use)

VERT$Lake_Origin_use<-substr(VERT$WSA9_LO,5,13)
table(VERT$Lake_Origin_use)

# Order Ecoregions
VERT$ECOREG_use <- ordered(VERT$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

#######
# EVAPORATION:INFLOW
#######
EI$ECOREG_use<-substr(EI$WSA9_LO,1,3)
table(EI$ECOREG_use)

EI$Lake_Origin_use<-substr(EI$WSA9_LO,5,13)
table(EI$Lake_Origin_use)

# Order Ecoregions
EI$ECOREG_use <- ordered(EI$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

#######
# WRT
#######
RT$ECOREG_use<-substr(RT$WSA9_LO,1,3)
table(RT$ECOREG_use)

RT$Lake_Origin_use<-substr(RT$WSA9_LO,5,13)
table(RT$Lake_Origin_use)

# Order Ecoregions
RT$ECOREG_use <- ordered(RT$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

#############
# CHANGE DOT PLOTS
##########
## Horizontal DD
##########

horiz <-ggplot(HORIZ, aes(x=ECOREG_use, y = horiz_diff, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=horiz_diff-se, ymax=horiz_diff+se), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Horizontal drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nHorizontal drawdown (m)")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))


##########
## Vertical DD
##########

vert <-ggplot(VERT, aes(x=ECOREG_use, y = vert_diff, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=vert_diff-se, ymax=vert_diff+se), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Vertical drawdown 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nVertical drawdown (m)")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

##########
## E:I
##########

EI_change <-ggplot(EI, aes(x=ECOREG_use, y = EI_diff, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=EI_diff-se, ymax=EI_diff+se), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in E:I 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nEvaporation: Inflow")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))


##########
## WRT
##########

WRT <-ggplot(RT, aes(x=ECOREG_use, y = RT_iso_diff, color=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=RT_iso_diff-se, ymax=RT_iso_diff+se), width = .1,
                position=position_dodge((width=0.5)))+
  #ggtitle("Change in WRT 2012-2007")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Change in \nWater residence time (yr)")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_discrete(breaks=c("NATURAL","MAN_MADE"),
                       labels=c("Natural", "Man-made"))

##########
## PLOT SPECIFICATIONS
##########
# Set working directory for output
setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/Dotplot")

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
#######################################
# MULTIPLE PLOT PANEL
# 

# Two variables
tiff(filename="a_Change_RESAMPLED_HORIZ_VERT_17JAN18.tiff",width=6.5, height=4, units="in", res=600)
multiplot(horiz,vert, cols=2)
dev.off()

# Plus E:I & WRT
tiff(filename="a_Change_RESAMPLED_EI_17JAN18.tiff",width=6.5, height=4, units="in", res=600)
multiplot(EI_change,WRT, cols=2)
dev.off()



#############
# Change in mean values by Ecoregion and lake type
#   Get mean values by ecoregion and lake type class "WSA9_LO"
#############

# Summarize variables by a grouping class
eco_lo<-group_by(nla_all, WSA9_LO, YEAR)

summarise(eco_lo,mean_horiz=mean(HorizDD_use), mean_vert=mean(VertDD_use),
          mean_EI=mean(E_I), mean_RT=mean(RT_iso))

test<- ddply(nla_all, ~WSA9_LO, summarise,mean_horiz=mean(HorizDD_use), mean_vert=mean(VertDD_use),
          mean_EI=mean(E_I), mean_RT=mean(RT_iso))

#####################
#############
# Plot change values 
# to see if change in hydrology is related to change in climate
#############
#####################
nla_diff <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_cbind_WIDE_difference_REVISITS.csv")

todrop<-names(nla_diff)%in%c("X") 
nla_diffl<-nla_diff[!todrop]

# Order Ecoregions
nla_diff$ECOREG_use <- ordered(nla_diff$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
table(nla_diff$ECOREG_use)


#####
## Ecoregion
#   1/22/18
#####
# Set region colors
color_region<-c("#8c510a","#bf812d","#dfc27d","#f6e8c3","#f5f5f5","#c7eae5","#80cdc1","#35978f","#01665e")
names(color_region) <- levels(nla_diff$ECOREG_use)
summary(nla_diff)

# HORIZONTAL DRAWDOWN
p=ggplot(nla_diff, aes(x=Precip_mm_total_yr_diff, y=horiz_diff, color=ECOREG_use)) +
  geom_point(shape=16)+
  scale_color_manual(values=color_region, guide=FALSE)+
  scale_y_continuous(limits=c(NA,705), breaks=c(-400,-100,0,100,250,500,700))+
  scale_x_continuous(limits=c(NA,840), breaks=c(-680,-500,-250,0,250,500,700))+
  #scale_shape_manual(values=c(16,21))+ # Sets point shape
  #geom_abline(intercept=10, slope=8,linetype=1)+ # Add GMWL (b=10, m=8)
  theme_bw(base_size=14)+
  theme(axis.text.x = element_text(family = "RMN"), # , hjust=1  angle=45, #plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major=  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Horizontal")+
  xlab("Precipitation (mm)")

# Add delta and permil to axis titles
horiz=p+xlab(expression(paste(Delta," Precipitation (mm)")))+ #for isotope xlab(expression(paste(delta^{18},"O(\u2030)")) #(delta unicode = u0394)
  ylab(expression(paste(Delta," Horizontal drawdown (m)")))+
  theme(legend.position="none")#+


# VERTICAL DRAWDOWN
p_vert=ggplot(nla_diff, aes(x=Precip_mm_total_yr_diff, y=vert_diff, color=ECOREG_use)) +
  geom_point(shape=16)+
  scale_color_manual(values=color_region, guide=FALSE)+
  scale_y_continuous(limits=c(NA,14), breaks=c(-20,-10,-5,0,5,10))+
  scale_x_continuous(limits=c(NA,840), breaks=c(-680,-500,-250,0,250,500,700))+
  #scale_shape_manual(values=c(16,21))+ # Sets point shape
  #geom_abline(intercept=10, slope=8,linetype=1)+ # Add GMWL (b=10, m=8)
  theme_bw(base_size=14)+
  theme(axis.text.x = element_text(family = "RMN"), # , hjust=1  angle=45, #plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major=  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Vertical")+
  xlab("Precipitation (mm)")

# Add delta and permil to axis titles
vert=p_vert+xlab(expression(paste(Delta," Precipitation (mm)")))+ #for isotope xlab(expression(paste(delta^{18},"O(\u2030)")) #(delta unicode = u0394)
  ylab(expression(paste(Delta," Vertical drawdown (m)")))+
  theme(legend.position="none")#


# E:I
p_EI=ggplot(nla_diff, aes(x=Precip_mm_total_yr_diff, y=EI_diff, color=ECOREG_use)) +
  geom_point(shape=16)+
  scale_color_manual(values=color_region, guide=FALSE)+
  scale_y_continuous(limits=c(NA,1), breaks=c(-1,-0.5,-0.25,0,0.25,0.5,1))+
  scale_x_continuous(limits=c(NA,840), breaks=c(-680,-500,-250,0,250,500,700))+
  #scale_shape_manual(values=c(16,21))+ # Sets point shape
  #geom_abline(intercept=10, slope=8,linetype=1)+ # Add GMWL (b=10, m=8)
  theme_bw(base_size=14)+
  theme(axis.text.x = element_text(family = "RMN"), # , hjust=1  angle=45, #plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major=  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("Evaporation: Inflow")+
  xlab("Precipitation (mm)")

# Add delta and permil to axis titles
EI=p_EI+xlab(expression(paste(Delta," Precipitation (mm)")))+ #for isotope xlab(expression(paste(delta^{18},"O(\u2030)")) #(delta unicode = u0394)
  ylab(expression(paste(Delta," Evaporation: Inflow")))+
  theme(legend.position="none")#

# WRT
p_WRT=ggplot(nla_diff, aes(x=Precip_mm_total_yr_diff, y=RT_iso_diff, color=ECOREG_use)) +
  geom_point(shape=16)+
  scale_color_manual(values=color_region, guide=FALSE)+
  scale_y_continuous(limits=c(NA,5), breaks=c(-5,-2.5,0,2.5,5))+
  scale_x_continuous(limits=c(NA,840), breaks=c(-680,-500,-250,0,250,500,700))+
  #scale_shape_manual(values=c(16,21))+ # Sets point shape
  #geom_abline(intercept=10, slope=8,linetype=1)+ # Add GMWL (b=10, m=8)
  theme_bw(base_size=14)+
  theme(axis.text.x = element_text(family = "RMN"), # , hjust=1  angle=45, #plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major=  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"))+
  ylab("WRT")+
  xlab("Precipitation (mm)")

# Add delta and permil to axis titles
WRT=p_WRT+xlab(expression(paste(Delta," Precipitation (mm)")))+ #for isotope xlab(expression(paste(delta^{18},"O(\u2030)")) #(delta unicode = u0394)
  ylab(expression(paste(Delta," Water residence time (yr)")))+
  theme(legend.position="none")#

#######################################
# MULTIPLE PLOT PANEL
# Two variables
tiff(filename="a_Change_BIPLOT_RESAMPLED_HORIZ_VERT_22JAN18.tiff",width=6.5, height=4, units="in", res=600)
multiplot(horiz,vert, cols=2)
dev.off()

# Plus E:I & WRT
tiff(filename="a_Change_BIPLOT_RESAMPLED_EI_22JAN18.tiff",width=6.5, height=4, units="in", res=600)
multiplot(EI,WRT, cols=2)
dev.off()