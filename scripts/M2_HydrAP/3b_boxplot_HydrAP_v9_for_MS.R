##################################
## LAKE HYD ALTERATION RANK v.9 
##  BOXPLOTS of LAKE HYDROLOGIC RESPONSE BY RANKING
##  FOR MANUSCRIPT 
##
##   FIVE ECOREGIONS
## CLEANED UP VERSION FOR HYDRAP MANUSCRIPT
##
## 8/11/20 - added revised ZOE drawdown - Phil emailed using natural reference drawdown for all ecoregions
###################################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(GGally)
library(ggpubr)

#####################
## LOAD DATA 
# 2007 n =1028
nla<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Data/ECOREGION/ALL_ECO/NLA2007_n1028_HydAP_v9.csv")

############################
## REVISE ECOREGIONS - GROUP NPL and SPL & GROUP TPL and UMW
# RENAME LEVELS
nla$ECOREG_rev<-nla$ECOREG_use
levels(nla$ECOREG_rev) <- list("West"=c("WMT","XER"),"Great Plains"=c("NPL","SPL"), 
                               "Midwest"=c("TPL","UMW"),
                               "Appalachians"=c("SAP","NAP"),"Coastal Plains"=c("CPL"))
nla$ECOREG_rev <-droplevels(nla$ECOREG_rev)

# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
nla$ECOREG_rev<-ordered(nla$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                 "Appalachians","Coastal Plains"))
table(nla$ECOREG_rev)

# ORDER LAKE ORIGIN
nla$Lake_Origin_mod<-ordered(nla$Lake_Origin_mod, levels=c("NATURAL","MAN_MADE"))


###########
## Plot specifications
###########

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

###############################
## BOXPLOTS OF SAMPLED VALUES BY 5 ECOREGIONS
###############################

##################
# LAKE MORPHOLOGY AND RANKS
## LAKE DEPTH
# Default method = "kruskal.test" for multiple groups
zmax<-ggplot(nla, aes(x=factor(hap_rank_9), y = DpthMx_mod))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hap_rank_9),y=DpthMx_mod),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,200),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST 
  facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_wrap(Lake_Origin_mod~ECOREG_rev, ncol=5)+
  #facet_wrap(~ECOREG_use)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"),
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    #x=expression("HydrAP rank"),
    y=expression("Lake maximum depth m"))

###################
## LAKE AREA
lkarea<-ggplot(nla, aes(x=factor(hap_rank_9), y = LkArea_km2))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hap_rank_9),y=LkArea_km2),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,1500),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_jitter(width=0.1,alpha=0.3)+#, aes(shape=factor(Lake_Origin_mod  alpha=0.2,
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST
  #geom_hline(yintercept = mean(nla$LkArea_km2),linetype=2)+
  facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_wrap(Lake_Origin_mod~ECOREG_rev, ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    x=expression(""), #HydrAP rank
    y=expression("Lake area" ~km^2))


##############
## WALA
wala<-ggplot(nla, aes(x=factor(hap_rank_9), y = WALA))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hap_rank_9),y=WALA),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,52105),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3 )+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST
  facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_blank(), #
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    x= expression("HydrAP rank"),
    y=expression("Watershed:lake area"))


###################
## PLOTS OF RANKINGS

# 3PLOT WALA + ZMAX + LK AREA NLA 2007
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/rank_v9/3plot_wala_zmax_lkarea_2007_rank9.tiff",
     width=9, height=8, units="in", res=300)
multiplot(lkarea,zmax,wala) 
dev.off()


####################################
# SCALED ZOE DRAWDOWN
## log(OBSERVED) - log(EXPECTED) scaled water-level drawdown - i.e., (log(O/E))
##  That has been standardized by subtracting from the mean and dividing by the standard deviation

# VERTICAL scaled to depth
z2sc_VertDD<-ggplot(nla, aes(x=factor(hap_rank_9), y = Z2LOE_DDVrtDix_sc))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hap_rank_9),y=Z2LOE_DDVrtDix_sc),outlier.shape=NA)+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,4),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_hline(yintercept = 0, colour='red')+
  geom_jitter(width=0.1, alpha=0.2)+
  facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_grid(Lake_Origin_mod~ECOREG_rev)+
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST
  #facet_wrap(~ECOP5_2015,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    x=expression(""), #HydrAP rank
    y=expression("ZLog(Obs-Exp) Vertical scaled"))


##################
# SCALED Z2OE HORIZONTAL DD
z2sc_HorizDD<-ggplot(nla, aes(x=factor(hap_rank_9), y = Z2LOE_DDHzSqrtA_sc))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hap_rank_9),y=Z2LOE_DDHzSqrtA_sc),outlier.shape=NA)+#,stat="identity")+
  #scale_y_continuous(trans="log10",limits=c(NA,6),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_hline(yintercept = 0, colour="red")+
  geom_jitter(width=0.1, alpha=0.2)+
  facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_grid(~Lake_Origin_mod~ECOREG_rev)+
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST
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
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    x=expression("HydrAP rank"),
    y=expression("ZLog(Obs-Exp) Horizontal scaled"))


##################
# WRT
WRT<-ggplot(nla, aes(x=factor(hap_rank_9), y = RT_iso+0.1))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hap_rank_9),y=RT_iso+0.1),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,30),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST
  scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.2)+
  facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_grid(Lake_Origin_mod~ECOREG_rev)+
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
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    x=expression("HydrAP rank"),
    y=expression("Water residence time (yr)"))


#################################
## PRINT FIVE ECOREGION Comparing revised and original PLOTS
#################################

# NLA 2007 - SCALED REVSIED Z2 OE
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/rank_v9/Z2OEDD_scaled_2007_5ECO_rank9.tiff",
     width=8.5, height=5, units="in", res=300)
multiplot(z2sc_VertDD,z2sc_HorizDD)
dev.off()

## WATER RESIDENCE TIME
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/rank_v9/WRT_2007_5ECO_rank9.tiff",
     width=8.5, height=2.5, units="in", res=300)
WRT
dev.off()