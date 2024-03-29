###################################
## BAR CHARTS FOR MANUSCRIPT
## HYDRAP v.9 % POPULATION
##
## 8/16/20
###################################

rm(list=ls())

# LIBRARIES
library(reshape2)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(cowplot)


####################
## READ DATASETS - POPULATION ESTIMATES
#  Population output processed to select population summaries by subset e.g., ecoregion or ecoregion+lake origin

#################
## LAKE ORIGIN
HydrAP_LO_07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_LO_2007.csv")
# FULL 2012
HydrAP_LO_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_LO_2012.csv")

##############
## 5 ECOREGION
# 2007
HydrAP_eco_07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2007.csv")
# FULL 2012
HydrAP_eco_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2012.csv")

###############
## ECOREGION LAKE ORIGIN 
HydrAP_ecoLO_07<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5LO_2007.csv")
HydrAP_ecoLO_12<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5LO_2012.csv")

##############
## NATIONAL
HydrAP_07 <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_NATL_2007.csv")
HydrAP_12 <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_NATL_2012.csv")

###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman



#################################################
## LAKE ORIGIN 2007
HydrAP_LO_07<-HydrAP_LO_07%>%
  filter(!Category=="Total")

HydrAP_LO_07$Category<-as.factor(HydrAP_LO_07$Category)

HydrAP_LO_07$Category<-ordered(HydrAP_LO_07$Category, levels=c("7","6","5","4",
                                                               "3","2","1","0"))
# Lake Orign Type - Subpopulation
# Rename labels
HydrAP_LO_07$lk_origin<-factor(HydrAP_LO_07$Subpopulation, labels=c("Man-made","Natural"))
table(HydrAP_LO_07$lk_origin)
HydrAP_LO_07$lk_origin <- ordered(HydrAP_LO_07$lk_origin, levels=c("Natural","Man-made"))

#HydrAP_LO_07$Subpopulation<-ordered(HydrAP_LO_07$Subpopulation, levels=c("NATURAL","MAN_MADE"))


# Arrange/sort and compute cumulative sums - to position labels on barchart
df1_07 <- HydrAP_LO_07 %>%
  group_by(lk_origin) %>% #ECOP5_2015
  arrange(lk_origin, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df1



## NEED TO REVERSE ORDER OF COLORS AND ORDER OF RANKS TO ASCEND MOVING UP BARCHART
p_lo_07 <- ggplot(df1_07, aes(x=lk_origin, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =lk_origin), color = "black", size=3, check_overlap = TRUE,family="RMN")+
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
        strip.text = element_text(family = "RMN", size=12),
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
# Lake Orign Type - Subpopulation
# Rename labels
HydrAP_LO_12$lk_origin<-factor(HydrAP_LO_12$Subpopulation, labels=c("Man-made","Natural"))
table(HydrAP_LO_12$lk_origin)
HydrAP_LO_12$lk_origin <- ordered(HydrAP_LO_12$lk_origin, levels=c("Natural","Man-made"))

#HydrAP_LO_12$Subpopulation<-ordered(HydrAP_LO_12$Subpopulation, levels=c("NATURAL","MAN_MADE"))

# Arrange/sort and compute cumulative sums - to position labels on barchart
df1_12 <- HydrAP_LO_12 %>%
  group_by(lk_origin) %>% #ECOP5_2015
  arrange(lk_origin, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df1

## NEED TO REVERSE ORDER OF COLORS AND ORDER OF RANKS TO ASCEND MOVING UP BARCHART
p_lo_12 <- ggplot(df1_12, aes(x=lk_origin, y = Estimate.P))+
  geom_col(aes(fill = Category), width = 0.7)+
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =lk_origin), color = "black", size=3, check_overlap = TRUE,family="RMN")+
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
     width=7.5, height=5, units="in", res=300)
grid.arrange(p_lo_07, p_lo_12, legend, ncol=3, widths=c(3.0, 2.8, 0.9))
dev.off()


##################################################################
########################
## ECOREGION 5 ONLY - stacked by year
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
  annotate(geom="text", x=1,y=69.5, label="1.3",size=3,family="RMN")+
  #annotate(geom="text", x=1,y=68, label="1.8",size=3,family="RMN")+
  #annotate(geom="text", x=3, y=96, label="2.4",size=3,family="RMN")+
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
  #annotate(geom="text", x=3, y=96, label="0.8",size=3,family="RMN")+
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
     width=6.5, height=10.5, units="in", res=300)
grid.arrange(p_eco_07, p_eco_12, legend,ncol=2,nrow=2, 
             layout_matrix=cbind(c(1,2),c(3,3)),
             widths=c(4.5,0.8),heights=c(5,5.5))
dev.off()



##########################################
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
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+ # OLD ORDER
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  #annotate(geom="text", x=1, y=100.5, label="0.7",size=3,family="RMN")+
  annotate(geom="text", x=2, y=98.6, label="3.5",size=3,family="RMN")+
  annotate(geom="text", x=3, y=100.2, label="1.4",size=3,family="RMN")+
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
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
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
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  annotate(geom="text", x=2, y=100.3, label="0.5",size=3,family="RMN")+
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
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+ # NEW ORDER
  scale_fill_manual(values = c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4"),na.value="gray")+
  geom_text(aes(y = lab_ypos, label = round(Estimate.P,1), group =Subpopulation), color = "black",size=3, check_overlap = TRUE, family="RMN")+
  annotate(geom="text", x=2, y=9.9, label="2.2",size=3,family="RMN")+
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
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(#title="2012",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    fill="HydrAP rank",
    x=expression(),# 
    y=expression("% Man-made lake population 2012"))  #"Percent population"



## FOR LEGEND
HydrAP_ecoLO_12_for_legend<-HydrAP_ecoLO_12
HydrAP_ecoLO_12_for_legend$Category<-ordered(HydrAP_ecoLO_12_for_legend$Category, levels=c("0","1","2","3","4","5","6","7"))

df3_12_legend <- HydrAP_ecoLO_12_for_legend %>%
  filter(LO=="MAN_MADE") %>%
  group_by(ECO5) %>% #ECOP5_2015
  arrange(ECO5, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.

p_ecolo_man_12_legend <- ggplot(df3_12_legend, aes(x=ECO5, y = Estimate.P))+
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

#############################
## FOUR PANEL HYDRAP % BY ECOREGION, LAKE TYPE, YEAR
# GET LEGEND
legend<-get_legend(p_ecolo_man_12_legend)
# REMOVE LEGEND
#p_ecolo_man_12<-p_ecolo_man_12 + theme(legend.position="none")

tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_ecoregion_LO_4panel_0712_bargraph.tiff",
     width=7.5, height=8, units="in", res=300)
grid.arrange(p_ecolo_nat_07, p_ecolo_man_07,p_ecolo_nat_12, p_ecolo_man_12, legend,ncol=2,nrow=3, 
             layout_matrix=cbind(c(1,3,5),c(2,4,5)),
             widths=c(4,4),heights=c(4,4.5,0.8))
dev.off()


###############################
## NLA 2012 >=4ha
## READ DATA
# >=4ha 2012(adjusted target population)
HydrAP_eco_12_4ha<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydrAP_ECO5_2012_4ha.csv")


HydrAP_eco_12_4ha<-HydrAP_eco_12%>%
  filter(!Category=="Total")

HydrAP_eco_12_4ha$Category<-as.factor(HydrAP_eco_12_4ha$Category)

HydrAP_eco_12_4ha$Category<-ordered(HydrAP_eco_12_4ha$Category, levels=c("7","6","5","4",
                                                                 "3","2","1","0"))
HydrAP_eco_12_4ha$Subpopulation<-ordered(HydrAP_eco_12_4ha$Subpopulation, levels=c("West","Great Plains","Midwest",
                                                                           "Appalachians","Coastal Plains"))
# Arrange/sort and compute cumulative sums - to position labels on barchart
df2_12_4ha <- HydrAP_eco_12_4ha %>%
  group_by(Subpopulation) %>% #ECOP5_2015
  arrange(Subpopulation, desc(Category)) %>%
  mutate(lab_ypos = cumsum(Estimate.P) - 0.5 * Estimate.P) # calculate the cumulative sum of len for each dose category. Used as the y coordinates of labels. To put the label in the middle of the bars, we'll use cumsum(len) - 0.5 * len.
#df2

p_eco_12_4ha <- ggplot(df2_12_4ha, aes(x=Subpopulation, y = Estimate.P))+
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



# 2012 size adjusted
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_2012_4ha_ecoregion_bargraph.tiff",
     width=5.5, height=5, units="in", res=300)
p_eco_12_4ha
dev.off()
