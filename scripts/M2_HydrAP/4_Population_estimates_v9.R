####################################
### POPULATION ESTIMATES OF HYDRAP v.9
# Purpose: Calculate condition estimates for the NLA 2007 & 2012 surveys
##          Population estimates conducted separately by year
# Programmer: Tom Kincaid
##
# See for R documentation of spsurvey package
# https://www.rdocumentation.org/packages/spsurvey/versions/3.3
##
## 6/29/20
## 7/1/20 
## 7/7/20
## 7/16/20  - Population estimates by YEAR
## 7/25/20 - Population estimates of NLA 2012 lakes >=4 ha
## 8/7/20 - Population estimates of dam purpose and HydrAP rank

rm(list=ls())

# Load the spsurvey library
library(spsurvey)

##################
## LOAD DATA
# NLA 2007 with HydrAP ranks v.9 n = 1028
dat07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA2007_n1028_HydAP_v9.csv")
table(dat07$hap_rank_9)
#0   1   2   3   4   5   6   7 
#74  90 139  38 101 144 165 156 

# NLA 2012 with HydrAP ranks v.9 n = 1038
dat12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA2012_n1038_HydAP_v9_RESAMPLED.csv")
table(dat12$hap_rank_9)
#0   1   2   3   4   5   6   7 
#83  99 175  36 109  93 124 168  

# DROP SMALL LAKES n = 951 - run population estimates to see if there are differences between yeasr because of increased target population
dat12<-dat12%>%
  filter(LkArea_km2>=0.04)

## ALL LAKES OPT 1 lakes >=1ha lake HydAP v. 9 n = 1716 
#dat<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA0712_1ha_opt1_HydAP_v9.csv")

############################
## REVISE ECOREGIONS - GROUP NPL and SPL & GROUP TPL and UMW
# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
dat07$ECOREG_rev<-ordered(dat07$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                 "Appalachians","Coastal Plains"))
dat12$ECOREG_rev<-ordered(dat12$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                     "Appalachians","Coastal Plains"))
table(dat07$ECOREG_rev)

# Create subclass grouping Aggregatd ecoregion and Lake origin
dat07$ECO_LO <- with (dat07, interaction(ECOREG_rev,Lake_Origin_mod,sep="_"))
dat12$ECO_LO <- with (dat12, interaction(ECOREG_rev,Lake_Origin_mod,sep="_"))

dat07$ECO9_LO <- with (dat07, interaction(ECOREG_use,Lake_Origin_mod,sep="_"))
dat12$ECO9_LO <- with (dat12, interaction(ECOREG_use,Lake_Origin_mod,sep="_"))

###################
## DAM PURPOSE (Primary)
dat07$dam_purp<-dat07$purpose_red
levels(dat07$dam_purp)<-list("release"=c("I","H","C","S"),
                             "store"=c("R","N","F","P"),
                             "other"=c("O","T"))
table(dat07$dam_purp)

# 2012
dat12$dam_purp<-dat12$purpose_red
levels(dat12$dam_purp)<-list("release"=c("I","H","C","S"),
                             "store"=c("R","N","F","P"),
                             "other"=c("O","T"))
###########
## CREATE SUBGROUP OF DAM PURPOSE AND ECOREGION
dat07$dam_eco <- with (dat07, interaction(ECOREG_rev,dam_purp,sep="_"))
dat12$dam_eco <- with (dat12, interaction(ECOREG_rev,dam_purp,sep="_"))

############################
### POPULATION ESTIMATES 2007
#############################
# Calculate condition class estimates
# Using cat.analysis in spsurvey 

nr <- nrow(dat07)

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=dat07$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=dat07$SITE_ID,
                     National=rep("All_Lakes",nrow(dat07)),
                     Lake_Origin=dat07$Lake_Origin_mod,
                     WSA5_Ecoregions=dat07$ECOREG_rev,
                     WSA9_Ecoreg=dat07$ECOREG_use,
                     WSA5_LO=dat07$ECO_LO,
                     WSA9_LO=dat07$ECO9_LO,
                     Trophic=dat07$TROPHIC_STATE)
                     #dam_purp=dat07$dam_purp)

# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
# For 2007 NLA Weight = "WGT_NLA"
design <- data.frame(siteID=dat07$SITE_ID,
                     wgt=dat07$WGT_SP, # OLD WGT_NLA
                     xcoord=dat07$XCOORD,
                     ycoord=dat07$YCOORD)

# Create the data.cat data frame, which specifies the variables to use in the
# analysis
data.cat <- data.frame(siteID=dat07$SITE_ID,
                       HydrAP=dat07$hap_rank_9,
                       SIZE=dat07$SIZE_CLASS,
                       dam_purp=dat07$dam_purp)

# Calculate the estimates
HydrAP_07_Estimates <- cat.analysis(sites, subpop, design, data.cat)



# Print results
HydrAP_07_Estimates

# Write results as a comma-separated value (csv) file
write.csv(HydrAP_07_Estimates, file="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/Hydro_v9_2007_pop_estimates.csv", # 
          row.names=FALSE)

############################
### POPULATION ESTIMATES 2012 n = 1038
#############################
# Calculate condition class estimates
# Using cat.analysis in spsurvey 

nr <- nrow(dat12)

# Create the sites data frame, which identifies sites to use in the analysis
sites <- data.frame(siteID=dat12$SITE_ID,
                    Use=rep(TRUE, nr))

# Create the subpop data frame, which defines populations and subpopulations for
# which estimates are desired
subpop <- data.frame(siteID=dat12$SITE_ID,
                     National=rep("All_Lakes",nrow(dat12)),
                     Lake_Origin=dat12$Lake_Origin_mod,
                     WSA5_Ecoregions=dat12$ECOREG_rev,
                     WSA9_Ecoreg=dat12$ECOREG_use,
                     WSA5_LO=dat12$ECO_LO,
                     WSA9_LO=dat12$ECO9_LO,
                     Trophic=dat12$TROPHIC_STATE)

# Create the design data frame, which identifies the weight, x-coordinate, and
# y-coordinate for each site ID
# 
design <- data.frame(siteID=dat12$SITE_ID,
                     wgt=dat12$WGT_SP, # OLD WGT_NLA
                     xcoord=dat12$XCOORD,
                     ycoord=dat12$YCOORD)

# Create the data.cat data frame, which specifies the variables to use in the
# analysis
data.cat <- data.frame(siteID=dat12$SITE_ID,
                       HydrAP=dat12$hap_rank_9,
                       SIZE=dat12$SIZE_CLASS,
                       dam_purp=dat12$dam_purp)

# Calculate the estimates
HydrAP_12_Estimates <- cat.analysis(sites, subpop, design, data.cat)



# Print results
HydrAP_12_Estimates

# Write results as a comma-separated value (csv) file
write.csv(HydrAP_12_Estimates, file="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/Hydro_v9_2012_pop_estimates.csv", # 
          row.names=FALSE)

# Write results for NLA 2012 (>=4ha) as a comma-separated value (csv) file
write.csv(HydrAP_12_Estimates, file="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/Hydro_v9_2012_4ha_pop_estimates.csv", # 
          row.names=FALSE)

####################
## Create datasets from CONDITION ouptput to make boxplots 
#   Use Estimate.P to show percent of lakes within subpopulations that are classified by a drawdown class (SMALL, MEDIUM, LARGE - in comparison to regional reference)
#   Have to reshape dataset to make boxplots
####################
library(reshape2)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(cowplot)

########################
# 2007 POPULATION ESTIMATES
p07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/Hydro_v9_2007_pop_estimates.csv")#
names(p07)
table(p07$Type)
table(p07$Subpopulation)
table(p07$Indicator)

## Subset by response ##
# 5 Aggregated ECOREGIONS
p107<-subset(p07,Type=="WSA5_Ecoregions")
table(p107$Type)
p107<-droplevels(p107)

p207<-subset(p107,Indicator=="HydrAP")
HydrAP_eco_07<-droplevels(p207)
table(p207$Indicator)

# HYDRAP
write.csv(HydrAP_eco_07,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2007.csv")

# Dam purpose
p107b<-subset(p107,Indicator=="dam_purp")
HydrAP_eco_07_dam<-droplevels(p107b)

write.csv(HydrAP_eco_07_dam, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_DAM_2007.csv")

#########
## Subset data "Type" = "National" - ALL LAKES
#########

p307<-subset(p07,Type=="National")
table(p307$Type)
p307<-droplevels(p307)

## Subset by response ##
# HydrAP
p307<-subset(p307,Indicator=="HydrAP")
HydrAP_natl_07<-droplevels(p307)
table(p307$Indicator)

# NATIONAL
write.csv(HydrAP_natl_07,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_NATL_2007.csv")


#########
## Subset data "Type" = "Lake_Origin" - Lake type
#########

p407<-subset(p07,Type=="Lake_Origin")
table(p407$Type)
p407<-droplevels(p407)

## Subset by response ##
# HydrAP
p407<-subset(p407,Indicator=="HydrAP")
HydrAP_LO_07<-droplevels(p407)
table(p407$Indicator)

# LAKE ORIGIN
write.csv(HydrAP_LO_07,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_LO_2007.csv")

## SUBSET BY ECOREGION AND LAKE ORIGIN##
p507<-subset(p07,Type=="WSA5_LO")
table(p507$Type)
p507<-droplevels(p507)

p607<-subset(p507,Indicator=="HydrAP")
HydrAP_ecoLO_07<-droplevels(p607)
table(p607$Indicator)
table(p607$Subpopulation)

# HYDRAP
write.csv(HydrAP_ecoLO_07,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5LO_2007.csv")



#######################
# 2012 POPULATION ESTIMATES
p12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/Hydro_v9_2012_pop_estimates.csv")#
names(p12)

## Subset by response ##
# 5 Aggregated ECOREGIONS
p112<-subset(p12,Type=="WSA5_Ecoregions")
table(p112$Type)
p112<-droplevels(p112)

p212<-subset(p112,Indicator=="HydrAP")
HydrAP_eco_12<-droplevels(p212)
table(p212$Indicator)

# HYDRAP
write.csv(HydrAP_eco_12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2012.csv")

#########
## Subset data "Type" = "National" - ALL LAKES
#########

p312<-subset(p12,Type=="National")
table(p312$Type)
p312<-droplevels(p312)

## Subset by response ##
# HydrAP
p312<-subset(p312,Indicator=="HydrAP")
HydrAP_natl_12<-droplevels(p312)
table(p312$Indicator)

# NATIONAL
write.csv(HydrAP_natl_12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_NATL_2012.csv")

#########
## Subset data "Type" = "Lake_Origin" - Lake type
#########
p412<-subset(p12,Type=="Lake_Origin")
table(p412$Type)
p412<-droplevels(p412)

## Subset by response ##
# HydrAP
p412<-subset(p412,Indicator=="HydrAP")
HydrAP_LO_12<-droplevels(p412)
table(p412$Indicator)

# LAKE ORIGIN
write.csv(HydrAP_LO_12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_LO_2012.csv")

######################
## SUBSET BY ECOREGION AND LAKE ORIGIN >= 1ha##
p512<-subset(p12,Type=="WSA5_LO")
table(p512$Type)
p512<-droplevels(p512)

p612<-subset(p512,Indicator=="HydrAP")
HydrAP_ecoLO_12<-droplevels(p612)
table(p612$Indicator)
table(p612$Subpopulation)

# HYDRAP
write.csv(HydrAP_ecoLO_12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5LO_2012.csv")



#######################
# 2012 lakes >=4ha  POPULATION ESTIMATES
p12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/Hydro_v9_2012_4ha_pop_estimates.csv")#
names(p12)

## Subset by response ##
# 5 Aggregated ECOREGIONS
p112<-subset(p12,Type=="WSA5_Ecoregions")
table(p112$Type)
p112<-droplevels(p112)

p212<-subset(p112,Indicator=="HydrAP")
HydrAP_eco_12<-droplevels(p212)
table(p212$Indicator)

# HYDRAP 5 Ecoregion (adjusted target population)
write.csv(HydrAP_eco_12,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2012_4ha.csv")




####################
## READ DATASETS

## LAKE ORIGIN
HydrAP_LO_07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_LO_2007.csv")
# FULL 2012
HydrAP_LO_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_LO_2012.csv")

## ECOREGION
HydrAP_eco_07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2007.csv")
# FULL 2012
HydrAP_eco_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2012.csv")

# >=4ha 2012(adjusted target population)
HydrAP_eco_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2012_4ha.csv")

## ECOREGION LAKE ORIGIN 
HydrAP_ecoLO_07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5LO_2007.csv")
HydrAP_ecoLO_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5LO_2012.csv")


###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
#d$ECOREG_rev<-ordered(d$ECOREG_rev, levels=c("West","Great Plains","Midwest",
#                                             "Appalachians","Coastal Plains"))

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


#####################################
## STACKED BAR CHART
#https://www.datanovia.com/en/blog/how-to-create-a-ggplot-stacked-bar-chart-2/
# BAR CHART
########################
## LAKE ORIGIN 2007
HydrAP_LO_07<-HydrAP_LO_07%>%
  filter(!Category=="Total")

HydrAP_LO_07$Category<-as.factor(HydrAP_LO_07$Category)

HydrAP_LO_07$Category<-ordered(HydrAP_LO_07$Category, levels=c("7","6","5","4",
                                                         "3","2","1","0"))
HydrAP_LO_07$Subpopulation<-ordered(HydrAP_LO_07$Subpopulation, levels=c("NATURAL","MAN_MADE"))

# Arrange/sort and compute cumulative sums - to position labels on barchart
df1_07 <- HydrAP_LO_07 %>%
  group_by(Subpopulation) %>% #ECOP5_2015
  arrange(Subpopulation, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df1

## NEED TO REVERSE ORDER OF COLORS AND ORDER OF RANKS TO ASCEND MOVING UP BARCHART
p_lo_07 <- ggplot(df1_07, aes(x=Subpopulation, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black", size=3, check_overlap = TRUE,family="RMN")+
  #annotate(geom="text", x=1, y=100, label="0.3",size=5)+ # rank = 6 is 0.3%
  annotate(geom="text", x=1, y=101.5, label="1.0",size=3,family="RMN")+
  annotate(geom="text", x=2, y=8.2, label="1.5",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(title="2007", #\nAn example
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    x=expression(),# "HydAP rank v.5"
    y=expression("Percent population"))  

########################
## LAKE ORIGIN 2012
HydrAP_LO_12<-HydrAP_LO_12%>%
  filter(!Category=="Total")

HydrAP_LO_12$Category<-as.factor(HydrAP_LO_12$Category)

HydrAP_LO_12$Category<-ordered(HydrAP_LO_12$Category, levels=c("7","6","5","4",
                                                               "3","2","1","0"))
HydrAP_LO_12$Subpopulation<-ordered(HydrAP_LO_12$Subpopulation, levels=c("NATURAL","MAN_MADE"))

# Arrange/sort and compute cumulative sums - to position labels on barchart
df1_12 <- HydrAP_LO_12 %>%
  group_by(Subpopulation) %>% #ECOP5_2015
  arrange(Subpopulation, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df1

## NEED TO REVERSE ORDER OF COLORS AND ORDER OF RANKS TO ASCEND MOVING UP BARCHART
p_lo_12 <- ggplot(df1_12, aes(x=Subpopulation, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black", size=3, check_overlap = TRUE,family="RMN")+
  #annotate(geom="text", x=2, y=3.5, label="3.3",size=3,family="RMN")+
  annotate(geom="text", x=1, y=101.5, label="0.7",size=3,family="RMN")+
  annotate(geom="text", x=2, y=3, label="3.3",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="right")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(title="2012", #\nAn example
       #subtitle="using font Decima WE",
       #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
       fill="HydrAP rank",
       x=expression(),# "HydAP rank v.5"
       y=expression())#"Percent population"  

###################
## Multiple ggplots with common legend
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
# GET LEGEND
legend<-get_legend(p_lo_12)

# REMOVE LEGEND
p_lo_12<-p_lo_12 + theme(legend.position="none")

## MULTIPLE GRAPHS, LEGEND ON RIGHT
#LO_fig<-grid.arrange(p_lo_07, p_lo_12, legend, ncol=3, widths=c(2.5, 2.3, 0.8))

#########################
## PRINT BARGRAPH
## LAKE ORIGIN # 2007+2012
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_lk_origin_0712_bargraph.tiff",
     width=6.5, height=4, units="in", res=300)
grid.arrange(p_lo_07, p_lo_12, legend, ncol=3, widths=c(2.5, 2.3, 0.8))
dev.off()


###################################
########################
## ECOREGION 5
## 2007
HydrAP_eco_07<-HydrAP_eco_07%>%
  filter(!Category=="Total")

HydrAP_eco_07$Category<-as.factor(HydrAP_eco_07$Category)

HydrAP_eco_07$Category<-ordered(HydrAP_eco_07$Category, levels=c("7","6","5","4",
                                                           "3","2","1","0"))
HydrAP_eco_07$Subpopulation<-ordered(HydrAP_eco_07$Subpopulation, levels=c("West","Great Plains","Midwest",
                                                                     "Appalachians","Coastal Plains"))
# Arrange/sort and compute cumulative sums - to position labels on barchart
df2_07 <- HydrAP_eco_07 %>%
  group_by(Subpopulation) %>% #ECOP5_2015
  arrange(Subpopulation, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df2

p_eco_07 <- ggplot(df2_07, aes(x=Subpopulation, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE,family="RMN")+
  #annotate(geom="text", x=1,y=69, label="2.0",size=3,family="RMN")+
  annotate(geom="text", x=3, y=96, label="2.4",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2007",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    fill="HydrAP rank",
    x=expression(),# "HydAP rank v.5"
    y=expression("% Population 2007"))  

########################
## 2012
HydrAP_eco_12<-HydrAP_eco_12%>%
  filter(!Category=="Total")

HydrAP_eco_12$Category<-as.factor(HydrAP_eco_12$Category)

HydrAP_eco_12$Category<-ordered(HydrAP_eco_12$Category, levels=c("7","6","5","4",
                                                                 "3","2","1","0"))
HydrAP_eco_12$Subpopulation<-ordered(HydrAP_eco_12$Subpopulation, levels=c("West","Great Plains","Midwest",
                                                                           "Appalachians","Coastal Plains"))
# Arrange/sort and compute cumulative sums - to position labels on barchart
df2_12 <- HydrAP_eco_12 %>%
  group_by(Subpopulation) %>% #ECOP5_2015
  arrange(Subpopulation, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df2

p_eco_12 <- ggplot(df2_12, aes(x=Subpopulation, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  #annotate(geom="text", x=3, y=100.5, label="4.0",size=3,family="RMN")+
  #annotate(geom="text", x=5, y=100.5, label="5.3",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="right")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2012",
       #subtitle="using font Decima WE",
       #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
       fill="HydrAP rank",
       x=expression(),# 
       y=expression("% Population 2012"))  #"Percent population"

#########################
## PRINT BARGRAPH
# ECOREGION

###################
## Multiple ggplots with common legend - Ecoregion graphs too big
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
# GET LEGEND
legend<-get_legend(p_eco_12)
# REMOVE LEGEND
p_eco_12<-p_eco_12 + theme(legend.position="none")

# Create a blank plot
#blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
#  cowplot::theme_nothing()

## MULTIPLE GRAPHS, LEGEND ON RIGHT
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_ecoregion_0712_bargraph.tiff",
     width=5.5, height=8, units="in", res=300)
grid.arrange(p_eco_07, p_eco_12, legend,ncol=2,nrow=2, 
             layout_matrix=cbind(c(1,2),c(3,3)),
             widths=c(4,0.8),heights=c(4,4.5))
dev.off()

#grid.arrange(p_eco_07, p_eco_12, legend,blankPlot,ncol=2,nrow=2, 
#             layout_matrix=rbind(c(1,3),c(2,4)),
#             widths=c(4,0.8),heights=c(4,4))


# 2007
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2007_ecoregion_bargraph.tiff",
     width=5.5, height=4, units="in", res=200)
p_eco_07
dev.off()

# 2012
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2012_ecoregion_bargraph.tiff",
     width=5.5, height=4, units="in", res=200)
p_eco_12
dev.off()

# 2012 size adjusted
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2012_4ha_ecoregion_bargraph.tiff",
     width=5.5, height=4, units="in", res=300)
p_eco_12
dev.off()


######################################
## ECOREGION + LAKE ORIGIN 2007
HydrAP_ecoLO_07<-HydrAP_ecoLO_07%>%
  filter(!Category=="Total")

## CREATE COLUMNS FOR LAKE TYPE AND ECOREGION
HydrAP_ecoLO_07$LO<-HydrAP_ecoLO_07$Subpopulation
HydrAP_ecoLO_07$ECO5<-HydrAP_ecoLO_07$Subpopulation
levels(HydrAP_ecoLO_07$LO) <- list( "NATURAL"=c("Appalachians_NATURAL","Coastal Plains_NATURAL","Great Plains_NATURAL","Midwest_NATURAL","West_NATURAL"),
                                         "MAN_MADE"=c("Appalachians_MAN_MADE","Coastal Plains_MAN_MADE","Great Plains_MAN_MADE","Midwest_MAN_MADE","West_MAN_MADE"))

levels(HydrAP_ecoLO_07$ECO5) <- list("Appalachians"=c("Appalachians_NATURAL","Appalachians_MAN_MADE"),
                                     "Coastal Plains"=c("Coastal Plains_NATURAL","Coastal Plains_MAN_MADE"),
                                    "Great Plains"=c("Great Plains_NATURAL","Great Plains_MAN_MADE"),
                                    "Midwest"=c("Midwest_NATURAL","Midwest_MAN_MADE"),
                                    "West"=c("West_NATURAL","West_MAN_MADE"))

table(HydrAP_ecoLO_07$ECO5)

HydrAP_ecoLO_07$Category<-as.factor(HydrAP_ecoLO_07$Category)

HydrAP_ecoLO_07$Category<-ordered(HydrAP_ecoLO_07$Category, levels=c("7","6","5","4",
                                                                 "3","2","1","0"))
HydrAP_ecoLO_07$ECO5<-ordered(HydrAP_ecoLO_07$ECO5, levels=c("West","Great Plains","Midwest",
                                                                           "Appalachians","Coastal Plains"))
# Arrange/sort and compute cumulative sums - to position labels on barchart
# FIRST NATURAL
df2_07 <- HydrAP_ecoLO_07 %>%
  filter(LO=="NATURAL") %>%
  group_by(ECO5) %>% #ECOP5_2015
  arrange(ECO5, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df2

p_ecolo_nat_07 <- ggplot(df2_07, aes(x=ECO5, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  #scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+ # OLD ORDER
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  #annotate(geom="text", x=1, y=100.5, label="0.7",size=3,family="RMN")+
  annotate(geom="text", x=2, y=100, label="0.7",size=3,family="RMN")+
  annotate(geom="text", x=3, y=100.5, label="1.4",size=3,family="RMN")+
  #annotate(geom="text", x=3, y=101.8, label="1.3",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2012",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    fill="HydrAP rank",
    x=expression(),# 
    y=expression("% Natural lake population 2007"))  #"Percent population"


# MANMADE 2007
df3_07 <- HydrAP_ecoLO_07 %>%
  filter(LO=="MAN_MADE") %>%
  group_by(ECO5) %>% #ECOP5_2015
  arrange(ECO5, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.

p_ecolo_man_07 <- ggplot(df3_07, aes(x=ECO5, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  #scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  #annotate(geom="text", x=1, y=101.8, label="0.7",size=3,family="RMN")+
  #annotate(geom="text", x=2, y=99.5, label="3.4",size=3,family="RMN")+
  #annotate(geom="text", x=3, y=101.8, label="1.3",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x =element_blank(), #axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2012",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    fill="HydrAP rank",
    x=expression(),# 
    y=expression("% Man-made lake population 2007"))  #"Percent population"


######################################
## ECOREGION + LAKE ORIGIN 2012
HydrAP_ecoLO_12<-HydrAP_ecoLO_12%>%
  filter(!Category=="Total")

## CREATE COLUMNS FOR LAKE TYPE AND ECOREGION
HydrAP_ecoLO_12$LO<-HydrAP_ecoLO_12$Subpopulation
HydrAP_ecoLO_12$ECO5<-HydrAP_ecoLO_12$Subpopulation
levels(HydrAP_ecoLO_12$LO) <- list( "NATURAL"=c("Appalachians_NATURAL","Coastal Plains_NATURAL","Great Plains_NATURAL","Midwest_NATURAL","West_NATURAL"),
                                    "MAN_MADE"=c("Appalachians_MAN_MADE","Coastal Plains_MAN_MADE","Great Plains_MAN_MADE","Midwest_MAN_MADE","West_MAN_MADE"))

levels(HydrAP_ecoLO_12$ECO5) <- list("Appalachians"=c("Appalachians_NATURAL","Appalachians_MAN_MADE"),
                                     "Coastal Plains"=c("Coastal Plains_NATURAL","Coastal Plains_MAN_MADE"),
                                     "Great Plains"=c("Great Plains_NATURAL","Great Plains_MAN_MADE"),
                                     "Midwest"=c("Midwest_NATURAL","Midwest_MAN_MADE"),
                                     "West"=c("West_NATURAL","West_MAN_MADE"))

table(HydrAP_ecoLO_12$ECO5)

HydrAP_ecoLO_12$Category<-as.factor(HydrAP_ecoLO_12$Category)

HydrAP_ecoLO_12$Category<-ordered(HydrAP_ecoLO_12$Category, levels=c("7","6","5","4",
                                                                     "3","2","1","0"))
HydrAP_ecoLO_12$ECO5<-ordered(HydrAP_ecoLO_12$ECO5, levels=c("West","Great Plains","Midwest",
                                                             "Appalachians","Coastal Plains"))
# Arrange/sort and compute cumulative sums - to position labels on barchart
df2_12 <- HydrAP_ecoLO_12 %>%
  filter(LO=="NATURAL") %>%
  group_by(ECO5) %>% #ECOP5_2015
  arrange(ECO5, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df2

p_ecolo_nat_12 <- ggplot(df2_12, aes(x=ECO5, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  #scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  #annotate(geom="text", x=1, y=101.8, label="0.7",size=3,family="RMN")+
  #annotate(geom="text", x=2, y=99.5, label="3.4",size=3,family="RMN")+
  #annotate(geom="text", x=3, y=101.8, label="1.3",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1), #axis.text.x = element_blank(), #
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2012",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    fill="HydrAP rank",
    x=expression(),# 
    y=expression("% Natural lake population 2012"))  #"Percent population"

## MANMADE 2012
df3_12 <- HydrAP_ecoLO_12 %>%
  filter(LO=="MAN_MADE") %>%
  group_by(ECO5) %>% #ECOP5_2015
  arrange(ECO5, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df2

p_ecolo_man_12 <- ggplot(df3_12, aes(x=ECO5, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  #scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  #annotate(geom="text", x=1, y=101.8, label="0.7",size=3,family="RMN")+
  #annotate(geom="text", x=2, y=99.5, label="3.4",size=3,family="RMN")+
  #annotate(geom="text", x=3, y=101.8, label="1.3",size=3,family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),# element_blank(), #axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2012",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    fill="HydrAP rank",
    x=expression(),# 
    y=expression("% Man-made lake population 2012"))  #"Percent population"

#########################
## PRINT BARGRAPH
###################
## Multiple ggplots with common legend - Ecoregion graphs too big
#http://www.sthda.com/english/wiki/wiki.php?id_contents=7930

### 2007 Natural and Man_made (LONG FIGURE)
# GET LEGEND
legend<-get_legend(p_ecolo_man_07)
# REMOVE LEGEND
p_ecolo_man_07<-p_ecolo_man_07 + theme(legend.position="none")

## MULTIPLE GRAPHS, LEGEND ON RIGHT
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2007_ecoregion_LO__bargraph.tiff",
     width=7.5, height=4, units="in", res=300)
grid.arrange(p_ecolo_nat_07, p_ecolo_man_07, legend,ncol=2,nrow=2, 
             layout_matrix=cbind(c(1,2),c(3,3)),
             widths=c(4,0.8),heights=c(4,4.5))
dev.off()

###########################
### 2012 Natural and Man_made
# GET LEGEND
legend<-get_legend(p_ecolo_man_12)
# REMOVE LEGEND
p_ecolo_man_12<-p_ecolo_man_12 + theme(legend.position="none")

## MULTIPLE GRAPHS, LEGEND ON RIGHT
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2012_ecoregion_LO__bargraph.tiff",
     width=7.5, height=4, units="in", res=300)
grid.arrange(p_ecolo_nat_12, p_ecolo_man_12, legend,ncol=2,nrow=2, 
             layout_matrix=cbind(c(1,2),c(3,3)),
             widths=c(4,0.8),heights=c(4,4.5))
dev.off()

#############################
## FOUR PANEL HYDRAP % BY ECOREGION, LAKE TYPE, YEAR
# GET LEGEND
legend<-get_legend(p_ecolo_man_12)
# REMOVE LEGEND
p_ecolo_man_12<-p_ecolo_man_12 + theme(legend.position="none")

tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_ecoregion_LO_4panel_0712_bargraph.tiff",
     width=7.5, height=8, units="in", res=300)
grid.arrange(p_ecolo_nat_07, p_ecolo_man_07,p_ecolo_nat_12, p_ecolo_man_12, legend,ncol=2,nrow=3, 
             layout_matrix=cbind(c(1,3,5),c(2,4,5)),
             widths=c(4,4),heights=c(4,4.5,0.8))
dev.off()

# ECOREGION Natural lake
# 2007
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2007_ecoregion_natural_bargraph.tiff",
     width=5.5, height=4, units="in", res=300)
p_ecolo_nat_07
dev.off()

# 2012
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2012_ecoregion_natural_bargraph.tiff",
     width=5.5, height=4, units="in", res=300)
p_ecolo_nat_12
dev.off()






## NATIONAL - IGNORE
HydrAP_natl<-HydrAP_natl%>%
  filter(!Category=="Total")

p <- ggplot(HydrAP_natl, aes(x=Type, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  #geom_text(aes(y = lab_ypos, label = Estimate.P, group =Subpopulation), color = "black")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="right")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    x=expression(),#
    y=expression("Percent population"))  
# NATIONAL
#tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_national_bargraph.tiff",
#     width=5, height=4, units="in", res=200)
#p
#dev.off()


##############3
## ALTERNATIVE CONNECTED LINE PLOT
ggplot(HydrAP_natl, aes(x=Category, y=Estimate.P)) + #colour=Category
  geom_errorbar(aes(ymin=Estimate.P-StdError.P, ymax=Estimate.P+StdError.P), width=.1) +
  geom_point(size=3) +
  #scale_color_manual(values = c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4"),na.value="gray")+
  geom_line()  


## ALTERNATIVE CONNECTED LINE PLOT
ggplot(HydrAP_natl, aes(x=Category, y=Estimate.U)) + #colour=Category
  geom_errorbar(aes(ymin=Estimate.U-StdError.U, ymax=Estimate.U+StdError.U), width=.1) +
  geom_point(size=3) +
  #scale_color_manual(values = c("#d73027","#fc8d59","#fee090","#ffffbf","#e0f3f8","#91bfdb","#4575b4"),na.value="gray")+
  geom_line()  



