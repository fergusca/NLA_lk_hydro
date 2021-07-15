############################
##DRIVERS OF LAKE HYDROLOGY MS
## DATA EXPLORATION FOR SUPPORTING INFORMATION
#######################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lattice)
library(cowplot)
library(GGally)
library(ggpubr)
library(tidyr)
library(FSA)

#############
## LOAD DATA
# CONUS ALL LAKES n = 1716
dat <- read.csv("~/NLA_hydro/NLA_hydro_driver/data_processed/conus_NLA_opt1_1716.csv")
todrop<-names(dat)%in%c("X1","X1_1") 
dat<-dat[!todrop]

## CREATE DATASET WITH CATEGORICAL VARIABLE INDICATING WHETHER LOW HYDRO_ALT LAKE OR NOT 
dat$hydro_dist <-dat$HydrAP
dat$hydro_dist <-as.factor(dat$hydro_dist)
str(dat$hydro_dist)

# Create SUBGROUPS FOR LAKES WITH MIN HYDRO_ALTERAITO DISTURBANCES AND ALL OTHERS
levels(dat$hydro_dist) <- list("Minimal" = c("0","1","2"), "CONUS" = c("3","4","5","6","7"))
dat<-dat %>% replace_na(list(hydro_dist = "CONUS"))

table(dat$hydro_dist)
#Minimal   CONUS 
#   553    1163 

#################
## CREATE HYDRAP BINS
## Follows bins from HydrAP paper (Ecological Indicators 2021)
dat$hydrap_bin <- dat$HydrAP

dat$hydrap_bin[dat$HydrAP <(3)]<- "Low"
dat$hydrap_bin[dat$HydrAP >=3 & dat$HydrAP <=5] <- "Moderate"
dat$hydrap_bin[dat$HydrAP > 5] <- "High"

# Bin PHDI values into drought, normal, and wet conditions
dat$PHDI_bin <- dat$PHDI
dat$PHDI_bin[dat$PHDI < (-0.5)] <- "drought"
dat$PHDI_bin[dat$PHDI >=(-0.5) & dat$PHDI <= 0.5] <- "normal"
dat$PHDI_bin[dat$PHDI > (0.5)] <- "wet"
table(dat$PHDI_bin)

## GRAND MEAN SCALED 30 yr normal precipitation
dat$Precip8110Ws_grd_sc <- scale(dat$Precip8110Ws, scale=T)

## GROUP MEAN SCALED 30 yr normal PRECIPITATION BY ECOREGION (scale = centers by mean and divides by std dev within an ecoregion group)
dat <- dat %>%
  group_by(ECOREG_rev) %>%
  mutate(Precip8110Ws_grp_sc = scale(Precip8110Ws, scale=TRUE)) # scaling is the default but just in case
summary(dat$Precip8110Ws_grp_sc)

##############
## COMBINE MOUNTAINOUS AREAS
mtn_part <- dat %>%
  filter(ECOREG_use=="XER"|ECOREG_use=="WMT"|ECOREG_use=="NAP") %>%
  drop_na(HydrAP) %>%
  droplevels()

#Remove NA obs and SAP low hydrap
SAP <- dat %>%
  filter(ECOREG_use=="SAP" & hydrap_bin!= "Low") %>%
  drop_na(HydrAP) %>%
  droplevels()

mtn <- bind_rows(mtn_part,SAP)
###########
## ORDER CATEGORIES
# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                 "Appalachians","Coastal Plains"))
# ORDER HYDRAP BINS
dat$hydrap_bin <- ordered(dat$hydrap_bin, levels=c("Low", "Moderate", "High"))

##################
## DATA SUBSETS
##################
## LOW HYDRAP LAKES (0-2)
#########
low_hydrap <- dat %>%
  filter(HydrAP<3)
table(low_hydrap$HydrAP)

#####################
## MIDWEST n= 482
#########
mwest<-dat %>%
  filter(ECOREG_rev=="Midwest")

#####################
## WEST n= 429
#########
west<-dat %>%
  filter(ECOREG_rev=="West")

#######################
## SUMMARY STATISTICS FOR LOW HYDRAP LAKES
#https://stackoverflow.com/questions/25759891/dplyr-summarise-each-with-na-rm/48224648

low_hydrap_summary <- dat %>%
  group_by(hydro_dist) %>%
  summarise_at(c("VertDD_use","HorizDD_use","E_I"),funs(mean,median,sd,min,max),na.rm=TRUE
  )

###################
## PLOTTING SPECIFICATIONS
#####################
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

######################
## FOR BOXPLOTS OF LAKE HYDROLOGIC CHARACTERISTIC BY ECOREGION
## Pairwise comparison test - Dunn's test for Kruskal-Wallis test
# https://rcompanion.org/rcompanion/d_06.html
#OLDhttps://www.datanovia.com/en/lessons/kruskal-wallis-test-in-r/#multiple-pairwise-comparisons
#OLDhttps://cran.r-project.org/web/packages/PMCMR/vignettes/PMCMR.pdf
# VERTDD
vert_out<- dunnTest(VertDD_use ~ ECOREG_rev,
              data=dat,
              method="bonferroni")
#                 Comparison           Z      P.unadj        P.adj
#1  Appalachians - Coastal Plains   1.8818470 5.985679e-02 5.985679e-01
#2    Appalachians - Great Plains  -3.4197876 6.267005e-04 6.267005e-03
#3  Coastal Plains - Great Plains  -4.7428198 2.107637e-06 2.107637e-05
#4         Appalachians - Midwest   2.8502824 4.368043e-03 4.368043e-02
#5       Coastal Plains - Midwest   0.3082457 7.578954e-01 1.000000e+00
#6         Great Plains - Midwest   6.5334635 6.426587e-11 6.426587e-10
#7            Appalachians - West  -7.7219510 1.145625e-14 1.145625e-13
#8          Coastal Plains - West  -8.3234932 8.540817e-17 8.540817e-16
#9            Great Plains - West  -3.9064207 9.367338e-05 9.367338e-04
#10                Midwest - West -11.7262993 9.345597e-32 9.345597e-31

# CONCLUSION: Mean vertdd is significantly diffrent among each ecoregion except between the Coastal Plains 
# and Appalachians and Midwest and Coastal Plains

# HORIZDD
horiz_out<-dunnTest(HorizDD_use ~ ECOREG_rev,
                    data=dat,
                    method="bonferroni")
#                      Comparison           Z      P.unadj        P.adj
#1  Appalachians - Coastal Plains   0.7105032 4.773922e-01 1.000000e+00
#2    Appalachians - Great Plains  -5.1452248 2.672007e-07 2.672007e-06
#3  Coastal Plains - Great Plains  -5.0461831 4.507235e-07 4.507235e-06
#4         Appalachians - Midwest   1.8026473 7.144363e-02 7.144363e-01
#5       Coastal Plains - Midwest   0.7077813 4.790811e-01 1.000000e+00
#6         Great Plains - Midwest   7.4062874 1.298842e-13 1.298842e-12
#7            Appalachians - West  -8.1286405 4.341316e-16 4.341316e-15
#8          Coastal Plains - West  -7.4323871 1.066551e-13 1.066551e-12
#9            Great Plains - West  -2.4636863 1.375164e-02 1.375164e-01
#10                Midwest - West -11.0532259 2.114766e-28 2.114766e-27                  

# CONCLUSION: Mean Horizontal is significantly different among ecoregions except the Appalachians and Coastal Plains and Midwest
#             and The West and the Great Plains

# E:I
ei_out<-dunnTest(E_I ~ ECOREG_rev,
                 data=dat,
                 method="bonferroni")
#                      Comparison          Z      P.unadj        P.adj
#1  Appalachians - Coastal Plains  -8.896018 5.788621e-19 5.788621e-18
#2    Appalachians - Great Plains -14.330649 1.407986e-46 1.407986e-45
#3  Coastal Plains - Great Plains  -3.664622 2.477039e-04 2.477039e-03
#4         Appalachians - Midwest -13.692790 1.121199e-42 1.121199e-41
#5       Coastal Plains - Midwest  -1.974568 4.831719e-02 4.831719e-01
#6         Great Plains - Midwest   2.328234 1.989969e-02 1.989969e-01
#7            Appalachians - West  -1.730722 8.350135e-02 8.350135e-01
#8          Coastal Plains - West   7.860237 3.834078e-15 3.834078e-14
#9            Great Plains - West  13.548464 8.090582e-42 8.090582e-41
#10                Midwest - West  12.883210 5.595885e-38 5.595885e-37  

# CONCLUSION: Mean E:I is significantly different among ecoregions except between the West and Appalachaisn and 
##            Midwest, Coastal Plains, and Great Plains

#########
# ADD LETTERS to BOXPLOTS - see crayfish code https://www.r-bloggers.com/adding-different-annotation-to-each-facet-in-ggplot/
# alternative site to check out https://cran.r-project.org/web/packages/PMCMR/vignettes/PMCMR.pdf
anno <- data.frame(xstar=c(1,2,3,4,5),ystar=c(35,30,30,30,30),
                   lab=c("a","b","c","d","cd"))
vert_boxplot<-ggplot(dat, aes(x=factor(ECOREG_rev), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(ECOREG_rev),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,46),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=4)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  #stat_pvalue_manual(dat, hide.ns = TRUE)+
  geom_text(data=anno, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #labs(subtitle = get_test_label(res.kruskal, detail=TRUE),
  #     caption = get_dat_label(dat))+
  #facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
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
    y=expression("Vertical decline (m)"),
    x=NULL)

##############
anno2 <- data.frame(xstar=c(1,2,3,4,5),ystar=c(270,270,270,270,270),
                    lab=c("a","a","b","b","b"))

horiz_boxplot<-ggplot(dat, aes(x=factor(ECOREG_rev), y = HorizDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(ECOREG_rev),y=HorizDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,546),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=4)+ # KRUSKAL WALLIS GRP MEAN TEST 
  geom_text(data=anno2, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  #facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
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
    y=expression("Horizontal decline (m)"),
    x=NULL)

###################
# E:I
anno3 <- data.frame(xstar=c(1,2,3,4,5),ystar=c(1.4,1.4,1.4,1.4,1.4),
                    lab=c("a","b","bc","a","c"))

EI_boxplot<-ggplot(dat, aes(x=factor(ECOREG_rev), y = E_I))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(ECOREG_rev),y=E_I),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,1.5),labels=function(x) format(x,scientific = FALSE))+ #trans="log10",,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#a6611a","#dfc27d","#f5f5f5","#80cdc1","#018571"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=4)+ # KRUSKAL WALLIS GRP MEAN TEST 
  geom_text(data=anno3, aes(x=xstar, y= ystar,label=lab),family="RMN")+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
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
    y=expression("Evaporation:Inflow"),
    x=NULL)

# SAVE FIGURE LAKE HYDROLOGY BOXPLOTS BY ECOREGION - KRUSKAL_WALLIS TEST AND DUNN's PostHoc Pairwise test
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_boxplot_eco_lk_hydro.tiff",
     width=5, height=8, units="in", res=300)
grid.arrange(vert_boxplot, horiz_boxplot, EI_boxplot,ncol=1,nrow=3)
dev.off()


#################################
## COMPARING LAKE SUBSETS - MIN HYDRO-ALTERATION vs REST OF CONUS
###################
## NON PARAMETRIC t-test - WILCOXON RANK SUM
#http://www.sthda.com/english/wiki/unpaired-two-samples-wilcoxon-test-in-r

vertdd <- wilcox.test(L_VertDD_use ~ hydro_dist, data = dat,
                      exact = FALSE) #W = 235301, p-value = 2.629e-10

horizdd <- wilcox.test(L_HorizDD_use ~ hydro_dist, data = dat,
                       exact = FALSE) #W = 248901, p-value = 1.447e-06

EI <- wilcox.test(L_EI ~ hydro_dist, data = dat,
                  exact = FALSE) #W = 383103, p-value = 7.191e-11

## BOXPLOTS OF LAKE HYDRO CHARACERISTICS BY MIN AND REST HYDRAP SUBSETS
## 3/29/21

vert_min_boxplot<-ggplot(dat, aes(x=factor(hydro_dist), y = VertDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hydro_dist),y=VertDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,46),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  scale_fill_manual(values = c("#5ab4ac","#d8b365"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2,method = "wilcox.test", paired = FALSE)+ # label="p.format",nonparametric test of independence for Non-paired observations
  #facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
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
    y=expression("Vertical decline (m)"),
    x=NULL)

horiz_min_boxplot<-ggplot(dat, aes(x=factor(hydro_dist), y = HorizDD_use))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hydro_dist),y=HorizDD_use),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,546),labels=function(x) format(x,scientific = FALSE))+
  scale_fill_manual(values = c("#5ab4ac","#d8b365"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2,method = "wilcox.test", paired = FALSE)+ #
  #facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_blank(),#axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
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
    y=expression("Horizontal decline (m)"),
    x=NULL)

EI_min_boxplot<-ggplot(dat, aes(x=factor(hydro_dist), y = E_I))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(hydro_dist),y=E_I),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(limits=c(NA,1.5),labels=function(x) format(x,scientific = FALSE))+
  scale_fill_manual(values = c("#5ab4ac","#d8b365"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.5,family="RMN",label.x=2,method = "wilcox.test", paired = FALSE)+ #
  #facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_text(family="RMN"), #element_blank(),#
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
    y=expression("Evaporation:Inflow"),
    x=NULL)

################
## MULTIPLE GRAPHS
# LAKE HYDROLOGY FOR SUBSET OF LAKES WITH MIN HYDRO ALTERATION AND ALL OTHER CONUS LAKES
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_boxplot_lk_hydro_min_hydro_conus.tiff",
     width=3.5, height=8, units="in", res=300)
grid.arrange(vert_min_boxplot, horiz_min_boxplot, EI_min_boxplot,ncol=1,nrow=3)
dev.off()



############
## CONUS INTERACTION WITH PHDI

# ORDER REVISED FIVE ECOREGIONS - already in order from west to east but if want to make sure
dat$ECOREG_rev<-ordered(dat$ECOREG_rev, levels=c("West","Great Plains","Midwest",
                                                               "Appalachians","Coastal Plains"))
dat$HydrAP_f <- as.factor(dat$HydrAP)

# Remove observations missing HydrAP ranks for graph
dat_mod <-dat %>%
  drop_na(HydrAP)

##############
## VERTICAL DD AND HydrAP
vert_phdi_conus<-ggplot(dat_mod, aes(x=PHDI, y=VertDD_use, color= hydrap_bin))+ #, shape=hydrap_bin))+
  geom_point(alpha=0.5) +
  xlim(-4.0, 4.0)+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  facet_wrap(~ECOREG_rev, ncol=5)+
  scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(color=hydrap_bin, family="RMN", label = ..r.label..), label.x=0, size=3)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        #text = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.title = element_text(family="RMN"),
        legend.text = element_text(family="RMN"),
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("PHDI"), #Grand mean scaled 
       y=expression("Vertical decline (m)"))#

##########
## SAVE CONUS MIN HYDRAP INTERACTION BIPLOT 
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_vert_phdi_interaction_CONUS.tiff",
     width=7.5, height=4, units="in", res=400)
vert_phdi_conus
dev.off()

##############
## MIDwEST INTERACTION PLOT BINNING HYDRAP CLASSES
# COMPLETE GRAPH WITH dots
vert_phdi_hydrap_mwest<-ggplot(mwest, aes(x= PHDI, y=VertDD_use, color=hydrap_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,5),labels=function(x) format(x,scientific = FALSE))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #xlim(NA,350)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  #stat_cor(aes(color=hydrap_bin, family="RMN"), label.x=1)+ 
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("PHDI"), #
       y=expression("Vertical decline (m)"))#,

############
## MIDWEST - SIMPLIFIED INTERACTION PLOT (ONLY LOW AND HIGH HYDRAP)
# Remove lakes missing HydrAP ranks
mwest_mod <- mwest %>%
  drop_na(HydrAP)

# Only Low and High HydrAP lakes
mwest_mod <- mwest_mod %>%
  filter(hydrap_bin=="Low" | hydrap_bin =="High")

simple_mwest<-ggplot(mwest_mod, aes(x= PHDI, y=VertDD_use, color=hydrap_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,0.5),labels=function(x) format(x,scientific = FALSE))+
  xlim(-4, 4)+
  #geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #xlim(NA,350)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  #stat_cor(aes(color=hydrap_bin, family="RMN"), label.x=1)+ 
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("PHDI"), #
       y=expression("Vertical decline (m)"))#,

##########
## SAVE MIDWEST INTERACTION BIPLOT 
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_interaction_MIDWEST.tiff",width=5, height=4, units="in", res=400)
vert_phdi_hydrap_mwest
dev.off()

## SAVE MIDWEST SIMPLIFIED INTERACTION BIPLOT 
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_SIMPLE_interaction_MIDWEST.tiff",width=5, height=4, units="in", res=400)
simple_mwest
dev.off()

###########################
# MIDWEST BIN BY DROUGHT
vert_phdi_bin_mwest<-ggplot(mwest, aes(x= HydrAP, y=VertDD_use, color=PHDI_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,5),labels=function(x) format(x,scientific = FALSE))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #xlim(NA,350)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  scale_color_manual(name="PHDI class", values=c("#d73027", "#fee090","#4575b4"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  stat_cor(aes(color=PHDI_bin, family="RMN"), label.x=4)+ # Just correlation ,label = ..r.label..
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("HydrAP"), #
       y=expression("Vertical decline (m)"))#,

## SAVE MIDWEST BINNED BY PHDI INTERACTION BIPLOT 
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_PHDI_bin_interaction_MIDWEST.tiff",
     width=5, height=4, units="in", res=300)
vert_phdi_bin_mwest
dev.off()

###############################
# WEST COMPLETE GRAPH WITH dots
vert_phdi_hydrap_west<-ggplot(west, aes(x= PHDI, y=VertDD_use, color=hydrap_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #xlim(NA,350)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  #stat_cor(aes(color=hydrap_bin, family="RMN"), label.x=1)+ 
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("PHDI"), #
       y=expression("Vertical decline (m)"))#,

# WEST BIN BY DROUGHT
vert_phdi_bin_west<-ggplot(west, aes(x= HydrAP, y=VertDD_use, color=PHDI_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #xlim(NA,350)+
  #facet_wrap(~ECOREG_rev, ncol=5)+
  scale_color_manual(name="PHDI class", values=c("#d73027", "#fee090","#4575b4"),na.value= "#999999")+ #"#2c7bb6", "#fdae61","#d7191c"
  #stat_cor(aes(color=hydrap_bin, family="RMN"), label.x=1)+ 
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("HydrAP"), #
       y=expression("Vertical decline (m)"))#,

## SAVE WEST INTERACTION BIPLOT 
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_interaction_WEST.tiff",width=5, height=4, units="in", res=400)
vert_phdi_hydrap_west
dev.off()

## SAVE WEST BINNED BY PHDI INTERACTION BIPLOT 
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_PHDI_bin_interaction_WEST.tiff",
     width=5, height=4, units="in", res=300)
vert_phdi_bin_west
dev.off()



##########################
## CLIMATE PLOTS

####################
## BOXPLOTS LONG_TERM SCALED PRECIPITATION DEVIATION FROM MEAN BY ECOREGION
##########################
## GROUP MEAN SCALED 30 yr Norm Precip by Ecoregions and HydrAP class

conus_30yr_precip_grp_sc <- ggplot(dat_mod, aes(x=hydrap_bin, y=Precip8110Ws_grp_sc))+
  geom_boxplot(aes(fill=factor(hydrap_bin),y=Precip8110Ws_grp_sc),outlier.shape=NA)+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(size=2.0,family="RMN",label.x=1.3)+ # label="p.format",KRUSKAL WALLIS GRP MEAN TEST 
  scale_fill_manual(name="HydrAP class",values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+
  facet_wrap(~ECOREG_rev,ncol=5)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
  labs(x=expression("HydrAP class"), #
       y=expression("Scaled 30 yr normal mean precipitation (mm)"))

##########
## PRINT BOXPLOT GROUP SCALED
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Boxplot_GRPSCALED_30yr_precip_hydrap_ECOREGION.tiff",
     width=7, height=4, units="in", res=400)
conus_30yr_precip_grp_sc
dev.off()


## POST-HOC PAIRWISE TEST
# WEST
west <- dat%>%
  filter(ECOREG_rev=="West")

posthoc.kruskal.dunn.test(x=west$Precip8110Ws_grp_sc, g=west$hydrap_bin, p.adjust.method="bonferroni")
# All pairwise comparisons are different p <0.05

# G PLAINS
gplains <-dat %>%
  filter(ECOREG_rev=="Great Plains")

posthoc.kruskal.dunn.test(x=gplains$Precip8110Ws_grp_sc, g=gplains$hydrap_bin, p.adjust.method="bonferroni")
# Low and Moderate are not different

# MIDWEST
mwest <- dat %>%
  filter(ECOREG_rev=="Midwest")

posthoc.kruskal.dunn.test(x=mwest$Precip8110Ws_grp_sc, g=mwest$hydrap_bin, p.adjust.method="bonferroni")
# All are different

# APPALACHIANS
apps <- dat %>%
  filter(ECOREG_rev=="Appalachians")

posthoc.kruskal.dunn.test(x=apps$Precip8110Ws_grp_sc, g=apps$hydrap_bin, p.adjust.method="bonferroni")
# Moderate is not different from low or high

# COASTAL PLAINS
cstplns <- dat %>%
  filter(ECOREG_rev=="Coastal Plains")

posthoc.kruskal.dunn.test(x=cstplns$Precip8110Ws_grp_sc, g=cstplns$hydrap_bin, p.adjust.method="bonferroni")
# moderate and high are not different

#####################
# EXAMINE INTERACTIONS IN MOUNTAINOUS SUB ECOREGIONS
# ORDER ECOREGIONS - already in order from west to east but if want to make sure
mtn$ECOREG_use<-ordered(mtn$ECOREG_use, levels=c("WMT","XER","NAP","SAP"))
# ORDER HYDRAP BINS
mtn$hydrap_bin <- ordered(mtn$hydrap_bin, levels=c("Low", "Moderate", "High"))


vert_phdi_mtn<- ggplot(mtn, aes(x= PHDI, y=VertDD_use, color=hydrap_bin))+ #, shape=hydrap_bin))+
  scale_y_continuous(trans="log10",limits=c(NA,45),labels=function(x) format(x,scientific = FALSE))+
  geom_point(alpha=0.5) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE)+
  #xlim(NA,350)+
  facet_wrap(~ECOREG_use, ncol=4)+
  scale_color_manual(name="HydrAP class", values=c("#4682b4", "#b4af46","#b4464b"),na.value= "#999999")+#name="PHDI class", values=c("#d73027", "#fee090","#4575b4"),na.value= "#999999")+ 
  stat_cor(aes(color=hydrap_bin, family="RMN", label = ..r.label..), label.x=0, size=3)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN", face="plain", size = 12, hjust=0.5),
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
        legend.position="bottom")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  labs(x=expression("PHDI"), #
       y=expression("Vertical decline (m)"))#,

##########
## PRINT MOUNTAINOUS INTERACTION BIPLOTS
tiff(filename="~/NLA_hydro/NLA_hydro_driver/figures/SI_Biplot_interaction_MOUNT.tiff",
     width=7, height=4, units="in", res=300)
vert_phdi_mtn
dev.off()