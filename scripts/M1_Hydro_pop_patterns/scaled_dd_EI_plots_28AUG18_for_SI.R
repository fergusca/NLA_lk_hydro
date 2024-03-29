###########################
## ASSOCIATIONS BETWEEN SCALED DRAWDOWN AND E:I
##
## USING NLA SAMPLE SITES (NOT POPULATION INFERRED)
## 8/28/18
##
## 7/1/19
##########################

rm(list=ls())

library(ggplot2)
library(ggpubr) # for correlation coeff in ggplot

##############
# LOAD DATA #
##############
## NLA 2007 ##
# n = 1028 observations with 555 variables
nla07 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2007_merge/NLA07_processed_data_USE/NLA_07_transformed_CONN_PHDI_ISO_lkcat_WGTS.csv") # NLA_07_VISIT_1_WGT_USE.csv Dropping obs NLA_07_transformed_SINGLE_USE.csv

# Clean up dataset
todrop <- names(nla07)%in% c("X")
nla07 <- nla07[!todrop]

###########
# SIZE ADJUSTED NLA 2012 dataset
###########
nla12 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Data/NLA_2012/Processed_data_TO_USE/NLA12_SINGLE_SIZE_ADJUST_lkcat_wgt_USE_26JUN19.csv") #NLA12_merge_transform_SINGLE_SIZE_ADJUST_USE.csv
# n=951 with 545 variables
test<-subset(nla12,UID==7769) # AL-113 Should have E:I = 0
#test <-subset(nla12,SID=="NLA06608-2193")

# Clean up dataset
names(nla12)
todrop <- names(nla12)%in% c("X")
nla12 <- nla12[!todrop]


###########
## Distributions of lakes by drawdown characteristics
##  FOR RESULTS SECTION in JAWRA MS

## NLA07 proportion of lakes with zero vertical and horizontal drawdown
# SEE THE CUMULATIVE DISTRIBUTION CONTINUOUS POPULATION OUTPUT
##  FOR PROPORTION OF INFERRED POPULATION WITH ZERO DRAWDOWN
#zero<-nla07[which(nla07$VertDD_use==0),]
#summary(zero$VertDD_use)
#summary(zero$HorizDD_use)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0000  0.0000  0.0000  0.1527  0.0000 10.1400
# drop lakes without zero horizontal dd
#zero_all <- zero[which(zero$HorizDD_use==0),] #n = 107
#107/1028 # 10% of lakes sampled in 2007 had zero measured

###############
## SELECT OBSERVATIONS WITH MAX DRAWDOWN BY LAKE TYPE
#https://nsaunders.wordpress.com/2013/02/13/basic-r-rows-that-contain-the-maximum-value-of-a-variable/

## MAX VERTICAL # Man-made = 40.0 m; Natural = 3.82 m
df.agg<-aggregate(VertDD_use~Lake_Origin_use,nla07,max)
max_vert<-merge(df.agg, nla07)

# MAX HORIZONTAL # Man-made = 545.66 m; Natural = 321.50 m
df.agg.h<-aggregate(HorizDD_use~Lake_Origin_use,nla07,max)
max_horiz<-merge(df.agg.h, nla07)

###########
## Plot specifications
###########

## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

# Set working directory for output
#setwd("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS")

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


##########################
## SEPARATE BY LAKE TYPE
nla07_man <-nla07[which(nla07$Lake_Origin_use=="MAN_MADE"),]
nla07_nat <-nla07[which(nla07$Lake_Origin_use=="NATURAL"),]

nla07_man$ECOREG_use <- ordered(nla07_man$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla07_nat$ECOREG_use <- ordered(nla07_nat$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


##############################
##############################
## BIPLOTS OF E:I AND SCALED DD BY LAKE TYPE AND PANELS FOR ECOREGION
library(ggplot2)
# Drop WA lake with high E:I ~2.83
### Replace E:I with NA for observation with high E:I in 2012 (NLA12_WA-101) SID =NLA06608-2193
nla12$E_I[nla12$SID=="NLA06608-2193"]<- NA
summary(nla12$E_I)
summary(nla07$E_I)

###################
## NATIONAL BIPLOTS BY LAKE TYPE
# Specify coefficient font size
your_font_size<- 5

###########
## 2007
ei_nla07_all <- ggplot(nla07,
                       aes(x=E_I, y=L_DDVrtDix_sc_MOD, color=Lake_Origin_use, shape=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2)+
  scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) +
  #geom_point(shape=16)+
  #scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  #facet_wrap(~ECOREG_use)+
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
        #legend.text=element_text(family="RMN"),
        legend.position="none")+
  ggtitle(NULL)+
  ylab("Log Vertical DD (scaled) 2007")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("E:I") + #(expression(paste(Delta,"E:I"))) +
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

## DROP missing E:I observations n = 3
nla12_red <-nla12[which(!is.na(nla12$E_I)),]

ei_nla12_all <- ggplot(nla12_red,
                       aes(x=E_I, y=L_DDVrtDix_sc_MOD, color=LAKE_ORIGIN, shape=LAKE_ORIGIN))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(aes(colour=LAKE_ORIGIN, shape=LAKE_ORIGIN),size=2)+
  scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) + #geom_point(shape=16)+
  #scale_color_manual(values=c("red","black"))+
  #scale_y_continuous(trans="log10")+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  #facet_wrap(~ECOREG_use)+
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
  ylab("Log Vertical DD (scaled) 2012")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("E:I") + #(expression(paste(Delta,"E:I")))
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

# WRITE FIGS BOTH YEARS ON SAME FILE
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/BIPLOT_ABS_VALUES/EI_scVERT_biplot_NATIONA_NLA0712.tiff",
     width=4, height=7, units="in", res=600)
multiplot(ei_nla07_all,ei_nla12_all)
dev.off()







################
## BIPLOTS WITH ECOREGION AS PANELS
# ORDER ECOREGIONS
nla07$ECOREG_use <- ordered(nla07$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla12$ECOREG_use <- ordered(nla12$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


# Specify coefficient font size
your_font_size<- 2.5

ei_nla07 <- ggplot(nla07,
                   aes(x=E_I, y=L_DDVrtDix_sc_MOD, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  facet_wrap(~ECOREG_use)+
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
        #legend.text=element_text(family="RMN"),
        legend.position="none")+
  ggtitle(NULL)+
  ylab("Log Vertical DD (scaled) 2007")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("E:I") + #(expression(paste(Delta,"E:I"))) +
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))


ei_nla12 <- ggplot(nla12,
                   aes(x=E_I, y=L_DDVrtDix_sc_MOD, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  facet_wrap(~ECOREG_use)+
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
  ylab("Log Vertical DD (scaled)")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("E:I") + #(expression(paste(Delta,"E:I")))
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))
library(dplyr)
nla12 %>% 
  group_by(ECOREG_use,Lake_Origin_use) %>%
  summarise(min=min(L_DDVrtDix_sc_MOD),mean=mean(L_DDVrtDix_sc_MOD),max=max(L_DDVrtDix_sc_MOD))
##############
## PRINT PLOTS

# Scaled Vert vs E:I NLA 2007
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/BIPLOT_ABS_VALUES/EI_scVERT_biplot_NLA07.tiff",
     width=6, height=4.55, units="in", res=600)
ei_nla07
dev.off()

# Scaled Vert vs E:I NLA 2012
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/BIPLOT_ABS_VALUES/EI_scVERT_biplot_NLA12.tiff",
     width=6, height=4.5, units="in", res=600)
ei_nla12
dev.off()

# BOTH YEARS ON SAME FILE
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/BIPLOT_ABS_VALUES/EI_scVERT_biplot_NLA0712.tiff",
     width=6.5, height=9, units="in", res=600)
multiplot(ei_nla07,ei_nla12)
dev.off()


###################
## E:I vs Depth
ei_zmax <- ggplot(nla07,
                  aes(x=E_I, y=L_DpthMx_mod, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  #facet_wrap(~ECOREG_use)+
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
  ylab("Log max depth")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("E:I") + #(expression(paste(Delta,"E:I")))
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

## E:I vs Area
ei_area <- ggplot(nla07,
                  aes(x=E_I, y=L_LkAreakm2, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  #facet_wrap(~ECOREG_use)+
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
  ylab("Log area")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("E:I") + #(expression(paste(Delta,"E:I")))
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))






## NLA07 MAN_MADE##
nla07_man_xy_plot<-ggplot(nla07_man, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

## NLA07 NATURAL##
nla07_nat_xy_plot<-ggplot(nla07_nat, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#


######################
## NLA 2007 PLOTS of Scaled vert vs. EI - for SAMPLED LAKES

# MAN_MADE
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/Scaled_VERTDD_EI_NLA07_MAN.tiff",width=5, height=5, units="in", res=600)
nla07_man_xy_plot
dev.off()

# NATURAL
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/Scaled_VERTDD_EI_NLA07_NAT.tiff",width=5, height=5, units="in", res=600)
nla07_nat_xy_plot
dev.off()


##########################
## SEPARATE BY LAKE TYPE
nla12_man <-nla12[which(nla12$Lake_Origin_use=="MAN_MADE"),]
nla12_nat <-nla12[which(nla12$Lake_Origin_use=="NATURAL"),]

nla12_man$ECOREG_use <- ordered(nla12_man$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
nla12_nat$ECOREG_use <- ordered(nla12_nat$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))


## NLA12 MAN_MADE##
nla12_man_xy_plot<-ggplot(nla12_man, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#

## NLA12 NATURAL ##
# DROP SAP
nla12_nat_red<-nla12_nat[!(nla12_nat$ECOREG_use %in% c("SAP")),]

nla12_nat_xy_plot<-ggplot(nla12_nat_red, aes(x=E_I, y=L_DDVrtDix_sc_MOD, group=ECOREG_use))+
  geom_point() +
  stat_smooth(method="lm",se=FALSE)+
  facet_wrap(~ ECOREG_use, scales="free")+
  theme_bw(base_size=12)+
  theme(strip.background = element_rect(fill="white"),
        strip.text=element_text(family="RMN"),
        axis.text.x=element_text(family = "RMN"),
        axis.text.y=element_text(family = "RMN"),
        axis.title.y=element_text(family = "RMN"),
        axis.title.x=element_text(family = "RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA))+
  ylab("Scaled log vertical drawdown") +
  xlab("E:I")#


######################
## NLA 2012 PLOTS of Scaled vert vs. EI - for SAMPLED LAKES

# MAN_MADE
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/Scaled_VERTDD_EI_NLA12_MAN.tiff",width=5, height=5, units="in", res=600)
nla12_man_xy_plot
dev.off()

# NATURAL
tiff(filename="M:/Net MyDocuments/a_Water_Level/Analysis/NLA_weighted_calculations/R_output/NLA_ALL/Scaled_VERTDD_EI_NLA12_NAT.tiff",width=5, height=5, units="in", res=600)
nla12_nat_xy_plot
dev.off()

######################
## SPEARMAN CORRELATION - used for when the two variables change together but not necessarily at a constant rate (https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/regression/supporting-topics/basics/a-comparison-of-the-pearson-and-spearman-correlation-methods/)
# 2007
cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla07,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.14 p<0.0001

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla07_nat,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.29 p<0.0001

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla07_man,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.13 p <0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=npl_man,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.58 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=tpl_man,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.36 p <0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=tpl_nat,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# n.s.

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=umw_nat,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.27 p<0.01



## By ECOREGION (grouping lake types)
CPL<-nla07[which(nla07$ECOREG_use=="CPL"),]
TPL<-nla07[which(nla07$ECOREG_use=="TPL"),]
NPL <-nla07[which(nla07$ECOREG_use=="NPL"),]
SPL <-nla07[which(nla07$ECOREG_use=="SPL"),]
UMW <-nla07[which(nla07$ECOREG_use=="UMW"),]
SAP <-nla07[which(nla07$ECOREG_use=="SAP"),]
NAP <-nla07[which(nla07$ECOREG_use=="NAP"),]
XER <-nla07[which(nla07$ECOREG_use=="XER"),]
WMT <-nla07[which(nla07$ECOREG_use=="WMT"),]

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=CPL,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.22 p<0.05

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NPL,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.39 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=TPL,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.21 p<0.05

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=UMW,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.25 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SPL,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SAP,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=XER,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=WMT,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NAP,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

## By ECOREGION AND LAKE TYPE 2007
CPL_n<-nla07_nat[which(nla07_nat$ECOREG_use=="CPL"),]
TPL_n<-nla07_nat[which(nla07_nat$ECOREG_use=="TPL"),]
NPL_n <-nla07_nat[which(nla07_nat$ECOREG_use=="NPL"),]
SPL_n <-nla07_nat[which(nla07_nat$ECOREG_use=="SPL"),]
UMW_n <-nla07_nat[which(nla07_nat$ECOREG_use=="UMW"),]
SAP_n <-nla07_nat[which(nla07_nat$ECOREG_use=="SAP"),]
NAP_n <-nla07_nat[which(nla07_nat$ECOREG_use=="NAP"),]
XER_n <-nla07_nat[which(nla07_nat$ECOREG_use=="XER"),]
WMT_n <-nla07_nat[which(nla07_nat$ECOREG_use=="WMT"),]

CPL_m<-nla07_man[which(nla07_man$ECOREG_use=="CPL"),]
TPL_m<-nla07_man[which(nla07_man$ECOREG_use=="TPL"),]
NPL_m <-nla07_man[which(nla07_man$ECOREG_use=="NPL"),]
SPL_m <-nla07_man[which(nla07_man$ECOREG_use=="SPL"),]
UMW_m <-nla07_man[which(nla07_man$ECOREG_use=="UMW"),]
SAP_m <-nla07_man[which(nla07_man$ECOREG_use=="SAP"),]
NAP_m <-nla07_man[which(nla07_man$ECOREG_use=="NAP"),]
XER_m <-nla07_man[which(nla07_man$ECOREG_use=="XER"),]
WMT_m <-nla07_man[which(nla07_man$ECOREG_use=="WMT"),]

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=CPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# NAT n.s.
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man rho=0.58 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=TPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man rho = 0.36 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=UMW_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat rho = 0.27 p <0.01
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SAP_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=XER_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=WMT_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NAP_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns




##########
# 2012
##########
cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.11 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla12_nat,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.18 p<0.001

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla12_man,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.14 p<0.01

## By ECOREGION (grouping lake types)
CPL_12<-nla12[which(nla12$ECOREG_use=="CPL"),]
TPL_12<-nla12[which(nla12$ECOREG_use=="TPL"),]
NPL_12 <-nla12[which(nla12$ECOREG_use=="NPL"),]
SPL_12 <-nla12[which(nla12$ECOREG_use=="SPL"),]
UMW_12 <-nla12[which(nla12$ECOREG_use=="UMW"),]
SAP_12 <-nla12[which(nla12$ECOREG_use=="SAP"),]
NAP_12 <-nla12[which(nla12$ECOREG_use=="NAP"),]
XER_12 <-nla12[which(nla12$ECOREG_use=="XER"),]
WMT_12 <-nla12[which(nla12$ECOREG_use=="WMT"),]

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=CPL_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho = 0.21 p<0.05

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NPL_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=TPL_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=UMW_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SPL_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SAP_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=XER_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=WMT_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# rho=0.33 p <0.0001

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NAP_12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns


## By ECOREGION AND LAKE TYPE 2012
CPL_n<-nla12_nat[which(nla12_nat$ECOREG_use=="CPL"),]
TPL_n<-nla12_nat[which(nla12_nat$ECOREG_use=="TPL"),]
NPL_n <-nla12_nat[which(nla12_nat$ECOREG_use=="NPL"),]
SPL_n <-nla12_nat[which(nla12_nat$ECOREG_use=="SPL"),]
UMW_n <-nla12_nat[which(nla12_nat$ECOREG_use=="UMW"),]
SAP_n <-nla12_nat[which(nla12_nat$ECOREG_use=="SAP"),]
NAP_n <-nla12_nat[which(nla12_nat$ECOREG_use=="NAP"),]
XER_n <-nla12_nat[which(nla12_nat$ECOREG_use=="XER"),]
WMT_n <-nla12_nat[which(nla12_nat$ECOREG_use=="WMT"),]

CPL_m<-nla12_man[which(nla12_man$ECOREG_use=="CPL"),]
TPL_m<-nla12_man[which(nla12_man$ECOREG_use=="TPL"),]
NPL_m <-nla12_man[which(nla12_man$ECOREG_use=="NPL"),]
SPL_m <-nla12_man[which(nla12_man$ECOREG_use=="SPL"),]
UMW_m <-nla12_man[which(nla12_man$ECOREG_use=="UMW"),]
SAP_m <-nla12_man[which(nla12_man$ECOREG_use=="SAP"),]
NAP_m <-nla12_man[which(nla12_man$ECOREG_use=="NAP"),]
XER_m <-nla12_man[which(nla12_man$ECOREG_use=="XER"),]
WMT_m <-nla12_man[which(nla12_man$ECOREG_use=="WMT"),]

############
## By lake type and ecoregion
cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=CPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# NAT rho = 0.5353722; p <0.01
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man rho= 0.4150467 p<0.01

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=TPL_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=UMW_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SPL_n,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns p<0.1
# Man rho = 0.236441; p<0.05

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=SAP_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=XER_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=WMT_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat rho = 0.24; p<0.05
# Man rho = 0.40; p<0.001

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=NAP_m,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
# Nat ns
# Man ns



################
## NATIONAL CORRELATION
cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla07,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
#(rho=0.145, p-value <0.0001)

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
#(rho=0.10, p-value <0.01)

###
## BY LAKE TYPE
cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla07_nat,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
#(rho=0.29, p-value <0.0001)

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla07_man,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
#(rho=0.13, p-value <0.01)

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla12_nat,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
#(rho=0.18, p-value <0.001)

cor.test(~E_I + L_DDVrtDix_sc_MOD,
         data=nla12_man,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)
#(rho=0.13, p-value <0.01)






## linear regression 
## NLA 2007
summary(lm(nla07$L_DDVrtDix_sc_MOD~nla07$E_I)) 
#Residual standard error: 0.557 on 956 degrees of freedom
#(70 observations deleted due to missingness)
#Multiple R-squared:  0.02536,	Adjusted R-squared:  0.02434 
#F-statistic: 24.88 on 1 and 956 DF,  p-value: 7.258e-07

# By lake type
# NATURAL 2007
summary(lm(nla07_nat$L_DDVrtDix_sc_MOD~nla07_nat$E_I)) 
#Residual standard error: 0.49 on 410 degrees of freedom
#(22 observations deleted due to missingness)
#Multiple R-squared:  0.1074,	Adjusted R-squared:  0.1052 
#F-statistic: 49.35 on 1 and 410 DF,  p-value: 8.983e-12

# MAN-MADE 2007
summary(lm(nla07_man$L_DDVrtDix_sc_MOD~nla07_man$E_I)) 
#Residual standard error: 0.5706 on 544 degrees of freedom
#(48 observations deleted due to missingness)
#Multiple R-squared:  0.02513,	Adjusted R-squared:  0.02334 
#F-statistic: 14.03 on 1 and 544 DF,  p-value: 0.0001996

# NLA 2012
summary(lm(nla12$L_DDVrtDix_sc_MOD~nla12$E_I)) 
# Residual standard error: 0.6397 on 921 degrees of freedom
#(8 observations deleted due to missingness)
#Multiple R-squared:  0.03783,	Adjusted R-squared:  0.03678 
#F-statistic: 36.21 on 1 and 921 DF,  p-value: 2.557e-09

# By lake type
# NATURAL 2012
summary(lm(nla12_nat$L_DDVrtDix_sc_MOD~nla12_nat$E_I)) 
#Residual standard error: 0.3889 on 407 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.1154,	Adjusted R-squared:  0.1132 
#F-statistic: 53.08 on 1 and 407 DF,  p-value: 1.672e-12

# MAN-MADE 2012
summary(lm(nla12_man$L_DDVrtDix_sc_MOD~nla12_man$E_I)) 
#Residual standard error: 0.7227 on 512 degrees of freedom
#(5 observations deleted due to missingness)
#Multiple R-squared:  0.04846,	Adjusted R-squared:  0.0466 
#F-statistic: 26.07 on 1 and 512 DF,  p-value: 4.645e-07

###########
# NLA 2007 separated by ecoregion
# MAN-MADE LAKES
wmt_man<-nla07_man[which(nla07_man$ECOREG_use=="WMT"),]
xer_man<-nla07_man[which(nla07_man$ECOREG_use=="XER"),]
npl_man<-nla07_man[which(nla07_man$ECOREG_use=="NPL"),]
spl_man<-nla07_man[which(nla07_man$ECOREG_use=="SPL"),]
tpl_man<-nla07_man[which(nla07_man$ECOREG_use=="TPL"),]
umw_man<-nla07_man[which(nla07_man$ECOREG_use=="UMW"),]
sap_man<-nla07_man[which(nla07_man$ECOREG_use=="SAP"),]
nap_man<-nla07_man[which(nla07_man$ECOREG_use=="NAP"),]
cpl_man<-nla07_man[which(nla07_man$ECOREG_use=="CPL"),]

# NATURAL LAKES
wmt_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="WMT"),]
xer_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="XER"),]
npl_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="NPL"),]
spl_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="SPL"),]
tpl_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="TPL"),]
umw_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="UMW"),]
sap_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="SAP"),]
nap_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="NAP"),]
cpl_nat<-nla07_nat[which(nla07_nat$ECOREG_use=="CPL"),]


##############
## MAN-MADE LAKES - NLA 2007
##############
summary(lm(wmt_man$L_DDVrtDix_sc_MOD~wmt_man$E_I)) 
# Multiple R-squared:  0.01925,	Adjusted R-squared:  0.005039 
# F-statistic: 1.355 on 1 and 69 DF,  p-value: 0.2485

summary(lm(xer_man$L_DDVrtDix_sc_MOD~xer_man$E_I)) 
#Multiple R-squared:  0.000286,	Adjusted R-squared:  -0.01442 
#F-statistic: 0.01945 on 1 and 68 DF,  p-value: 0.8895

summary(lm(npl_man$L_DDVrtDix_sc_MOD~npl_man$E_I)) 
#Multiple R-squared:  0.2861,	Adjusted R-squared:  0.2575 
#F-statistic: 10.02 on 1 and 25 DF,  p-value: 0.004048

summary(lm(spl_man$L_DDVrtDix_sc_MOD~spl_man$E_I)) 
# Multiple R-squared:  0.01541,	Adjusted R-squared:  0.005852 
# F-statistic: 1.612 on 1 and 103 DF,  p-value: 0.207

summary(lm(tpl_man$L_DDVrtDix_sc_MOD~tpl_man$E_I)) 
#Multiple R-squared:  0.05877,	Adjusted R-squared:  0.04451 
#F-statistic: 4.121 on 1 and 66 DF,  p-value: 0.04639

summary(lm(umw_man$L_DDVrtDix_sc_MOD~umw_man$E_I)) 
# Multiple R-squared:  0.002215,	Adjusted R-squared:  -0.1641 
# F-statistic: 0.01332 on 1 and 6 DF,  p-value: 0.9119

summary(lm(sap_man$L_DDVrtDix_sc_MOD~sap_man$E_I)) 
#Multiple R-squared:  0.04633,	Adjusted R-squared:  0.03742 
#F-statistic: 5.198 on 1 and 107 DF,  p-value: 0.0246

summary(lm(nap_man$L_DDVrtDix_sc_MOD~nap_man$E_I)) 
# Multiple R-squared:  0.01234,	Adjusted R-squared:  -0.01759 
# F-statistic: 0.4124 on 1 and 33 DF,  p-value: 0.5252

summary(lm(cpl_man$L_DDVrtDix_sc_MOD~cpl_man$E_I)) 
# Multiple R-squared:  0.0412,	Adjusted R-squared:  0.0224 
# F-statistic: 2.191 on 1 and 51 DF,  p-value: 0.1449

##############
## NATURAL LAKES - NLA 2007
##############
summary(lm(wmt_nat$L_DDVrtDix_sc_MOD~wmt_nat$E_I)) 
# Multiple R-squared:  0.04178,	Adjusted R-squared:  0.02607 
# F-statistic:  2.66 on 1 and 61 DF,  p-value: 0.1081

summary(lm(xer_nat$L_DDVrtDix_sc_MOD~xer_nat$E_I)) 
#Multiple R-squared:  0.122,	Adjusted R-squared:  0.01224 
#F-statistic: 1.112 on 1 and 8 DF,  p-value: 0.3225

summary(lm(npl_nat$L_DDVrtDix_sc_MOD~npl_nat$E_I)) 
#Multiple R-squared:  0.1156,	Adjusted R-squared:  0.08955 
# F-statistic: 4.443 on 1 and 34 DF,  p-value: 0.0425

summary(lm(spl_nat$L_DDVrtDix_sc_MOD~spl_nat$E_I)) 
# Multiple R-squared:  0.2183,	Adjusted R-squared:  0.1582 
# F-statistic: 3.631 on 1 and 13 DF,  p-value: 0.07908

summary(lm(tpl_nat$L_DDVrtDix_sc_MOD~tpl_nat$E_I)) 
#Multiple R-squared:  0.08552,	Adjusted R-squared:  0.07053 
# F-statistic: 5.704 on 1 and 61 DF,  p-value: 0.02003

summary(lm(umw_nat$L_DDVrtDix_sc_MOD~umw_nat$E_I)) 
# Multiple R-squared:  0.08701,	Adjusted R-squared:  0.08019 
# F-statistic: 12.77 on 1 and 134 DF,  p-value: 0.0004902

summary(lm(nap_nat$L_DDVrtDix_sc_MOD~nap_nat$E_I)) 
# Multiple R-squared:  2.309e-05,	Adjusted R-squared:  -0.0185 
# F-statistic: 0.001247 on 1 and 54 DF,  p-value: 0.972

summary(lm(cpl_nat$L_DDVrtDix_sc_MOD~cpl_nat$E_I)) 
# Multiple R-squared:  0.03379,	Adjusted R-squared:  0.002618 
# F-statistic: 1.084 on 1 and 31 DF,  p-value: 0.3059

######################
## SPEARMAN RANK CORRELATION 
##  MODELED WRT vs. ISOTOPE WRT
#####################
# https://rcompanion.org/rcompanion/e_02.html
nla12$RT_modeled

cor.test(~RT_iso + RT_modeled,
         data=nla12,
         method="spearman",
         continuity=FALSE,
         conf.level=0.95)

#Spearman's rank correlation rho

#data:  RT_iso and RT_modeled
#S = 30249000, p-value < 2.2e-16
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho 
#0.747684 

summary(nla12$RT_modeled)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#0.00000  0.05979  0.20020  0.80210  0.76790 51.31000       18 

summary(nla12$RT_iso)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.0000   0.1544   0.4120   0.8586   0.9267 134.5000       35 