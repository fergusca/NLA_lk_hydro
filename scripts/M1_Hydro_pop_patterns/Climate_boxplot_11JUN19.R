#############################
## CLIMATE BOXPLOTS for JAWRA
##  Distributions of weather attributes by survey year and 30yr avg
##  Using full lake datasets (long format)
##
##  6/11/19
##  7/1/19 - updated datasets with new population weights
#############################

rm(list=ls())

###########
# Libraries
###########


######################
## LOAD DATA to create modified dataset
######################

##############
## LONG FORMAT 2007 + 2012 all lakes (n = 2066 obs) 205 variables (NLA+lkCat+pop wgts)
nla07_12 <-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_ALL_01JUL19.csv")
length(unique(nla07_12$SID)) # 1716
table(nla07_12$YEAR)
# 2007 2012 
# 1028 1038 

names(nla07_12)

###############
## REDUCED DATASET VARIABLES
#   Variables of interest - SID, Total Precip, Mean temperature
survey_yr<-nla07_12[,c("SID","YEAR","Lake_Origin_use","ECOREG_use","Precip_mm_total_yr","Temp_degC_avg_yr","PHDI")]

longterm_clim<-nla07_12[,c("SID","YEAR","Lake_Origin_use","ECOREG_use","Precip8110Ws","Tmean8110Ws","PHDI")] #"Tmax8110Ws","Tmin8110Ws"

###############
## TRIM LONG-TERM CLIMATE DATASET
# We will have repeat observations in the long-term climate data because of resampled lakes - need to remove
length(unique(longterm_clim$SID)) # n = 1716
longterm_clim_red <- longterm_clim[!duplicated(longterm_clim$SID),] # n = 1716

#####################
# RENAME LONGTERM PRISM (30yr avg) YEAR to "30yr_AVG"
longterm_clim_red$YEAR="30yr_AVG"
table(longterm_clim_red$YEAR)

# RENAME COLUMNS to match survey year names
names(longterm_clim_red)[names(longterm_clim_red)=="Precip8110Ws"] <- "Precip_mm_total_yr"
names(longterm_clim_red)[names(longterm_clim_red)=="Tmean8110Ws"] <- "Temp_degC_avg_yr"

names(survey_yr)
names(longterm_clim_red)

### COMBINE SURVEY YEAR AND LONG-TERM CLIMATE DATA
dat<-rbind(survey_yr,longterm_clim_red)

table(dat$YEAR)
#    2007     2012 30yr_AVG 
#   1028     1038     1716 



################
## ORDER ECOREGIONS
dat$ECOREG_use <- ordered(dat$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
#nla07_12$YEAR<-factor(nla07_12$YEAR, labels=c("2007","2012")) 
table(dat$YEAR)
#    2007     2012 30yr_AVG 
#     1028     1035     1714

table(dat$Lake_Origin_use)
# MAN_MADE  NATURAL # MISSING 40 obs? 
#   2127     1610

names(dat)

################
## SUMMARY MEANS of climate variables and 30 yr avg by Year
# PRISM 30yr avg
stat_prism<-dat %>%
  group_by(YEAR) %>%
  summarize(mean_precip=mean(Precip_mm_total_yr,na.rm=T), mean_temp=mean(Temp_degC_avg_yr,na.rm=T),mean_phdi=mean(PHDI,na.rm=T),sd_phdi=sd(PHDI,na.rm=T))



###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
setwd("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

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
lk_prsm<-dat[(dat$Lake_Origin_use=="NATURAL"),]
lk<-subset(dat, Lake_Origin_use=="NATURAL")
lk<-subset(lk, ECOREG_use!="SAP")
# RESERVOIRS
man<-subset(dat, Lake_Origin_use=="MAN_MADE")

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
## PHDI - USE Dataset and drop "30yr_AVG" from YEAR
###
lk_prsm<-dat[dat$Lake_Origin_use=="NATURAL",]
lk_prsm <- lk_prsm[lk_prsm$YEAR %in% c("2007","2012"),]
lk_prsm<-subset(lk_prsm, ECOREG_use!="SAP")
table(lk_prsm$Lake_Origin_use)

man_prsm<- dat[dat$Lake_Origin_use=="MAN_MADE",]
man_prsm <- man_prsm[man_prsm$YEAR %in% c("2007","2012"),]
table(man_prsm$YEAR)
table(man_prsm$Lake_Origin_use)

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
setwd("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

#Precip by LAKE TYPE (Natural, Man-made)
# TOTAL PRECIP
tiff(filename="ALL_PRECIP_TOTAL_LO_AVG.tiff",width=7, height=3, units="in", res=600)
multiplot(precip_man,precip_lk, cols=2)
dev.off()

#MEAN Temp by LAKE TYPE
tiff(filename="ALL_MEAN_TEMP_LO_AVG.tiff",width=7, height=3, units="in", res=600)
multiplot(temp_mean_man, temp_mean_lk,cols=2)
dev.off()

#LEGEND
tiff(filename="LEGEND_YR_AVG.tiff",width=7, height=3, units="in", res=600)
multiplot(legend)
dev.off()

# PHDI WATER YEAR BY LAKE TYPE - NOTE: DOES NOT HAVE 30yr AVG
tiff(filename="ALL_PHDI_WY_LO.tiff",width=7, height=3, units="in", res=600)
multiplot(PHDI_man_use,PHDI_lk_use, cols=2)
dev.off()

