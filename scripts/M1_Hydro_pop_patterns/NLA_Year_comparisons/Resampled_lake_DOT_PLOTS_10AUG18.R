#########################
## RESAMPLED LAKES 2007 & 2012
## Change in lake hydrology response variables
##  Dot Plots - using paired t-test mean difference and confidence intervals - similar to the population estimate plots but
#########################
## 8/10/18
# 6/7/19
# 7/1/19 - updated datasets
# 7/8/19 - updated zmax and wrt on subset of data


rm(list=ls())

###########
# Libraries
###########
library(dplyr)
library(ggplot2)
library(ggpubr)


##############
## LOAD DATA
##############

## Original Long-format RESAMPLED lakes (n = 696 observations of 348 lakes)
#   updated WRT for 2012 lakes

nla07_12 <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
# n = 696 observations (348 lakes resampled)
length(unique(nla07_12$SID))

##############
## PAIRED t-tests
myvars<- c("SITE_ID","VISIT_NO","SID","YEAR","E_I","RT_iso","L_RT_iso","d_excess",
           "HorizDD_use","VertDD_use","L_HorizDD_use","L_VertDD_use",
           "LATdd_use","LONdd_use","Lake_Origin_use","ECOREG_use",
           "Precip_mm_total_yr","temp_degC_summer","Temp_degC_avg_yr","PHDI","P_E")

dat_red <-nla07_12[myvars]

# Modify YEAR variable to order from 2012 to 2007
table(dat_red$YEAR)
dat_red$YEAR_m <-factor(dat_red$YEAR, labels=c("2007","2012"))
table(dat_red$YEAR_m)

# Relabel Year so that 2012 will be first
levels(dat_red$YEAR_m) <- list("a_2012" = c("2012"),"b_2007"=c("2007"))

############
## NEED TO MODIFY DATASET TO DROP lakes missing measurements for both years

### Log Horizontal DD - # There are 19 obs in 2007 missing horizontal dd
##    BUT HAVE TO MODIFIY DATASET bc SOME OBSERVATIONS MISSING ONE YEAR BUT NOT OTHER
##      NEED TO HAVE THEM BE EQUAL 
horiz_na<-dat_red[which(is.na(dat_red$L_HorizDD_use)),]
h_na_SID <-horiz_na$SID 

#table(horiz$YEAR)
horiz <- dat_red[!(dat_red$SID %in% h_na_SID),] 
table(horiz$YEAR_m)

# Log Vert is same as horizontal
#  E:I
ei_na <-dat_red[which(is.na(dat_red$E_I)),]
ei_na_SID <- ei_na$SID
ei <-dat_red[!(dat_red$SID %in% ei_na_SID),]

## LOG WRT 
rt_na <- dat_red[which(is.na(dat_red$L_RT_iso)),] # 17
rt_na_SID <- rt_na$SID
rt <- dat_red[!(dat_red$SID %in% rt_na_SID),]
table(rt$YEAR_m)

#############################
## GROUP BY LAKE TYPE AND BY ECOREGION
##  PAIRED t-tests
#############################
# https://stats.stackexchange.com/questions/168378/applying-two-sample-t-test-comparing-multiple-groups-in-two-categories

library(dplyr)
library(broom)

## HORIZONTAL DD
# Order by year and SID
horiz = horiz[order(horiz$YEAR_m, horiz$SID),]

horiz_result_eco = horiz %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(L_HorizDD_use ~ YEAR_m,
                                                                                     data=., paired=TRUE)))
write.csv(horiz_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_horiz.csv")


# Trying out non-transformed
horiz_result_eco_nt = horiz %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(HorizDD_use ~ YEAR_m,
                                                                                        data=., paired=TRUE)))
write.csv(horiz_result_eco_nt,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_horiz_NT.csv")

############
## VERTICAL DD
# Order by year and SID
# Use horiz dataset bc removed NAs
#horiz = horiz[order(horiz$YEAR, horiz$SID),]

vert_result_eco = horiz %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(L_VertDD_use ~ YEAR_m,
                                                                                    data=., paired=TRUE)))

write.csv(vert_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_vert.csv")

############
## E:I
# Order by year and SID
ei = ei[order(ei$YEAR_m, ei$SID),]

EI_result_eco = ei %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(E_I ~ YEAR_m,
                                                                                    data=., paired=TRUE)))

write.csv(EI_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_EI.csv")

# National E:I t-test between years
EI_result_nat = ei %>% do(tidy(t.test(E_I ~ YEAR_m,
                                      data=., paired=TRUE)))
#  estimate statistic p.value parameter conf.low conf.high method  alternative
#     <dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl> <chr>   <chr>      
#1  0.00951     0.842   0.400       346  -0.0127    0.0317 Paired~ two.sided                                                                                

# ??SE = 0.022
#0.0317-0.00951
#0.00951-0.022
############
## WRT
# Order by year and SID
rt = rt[order(rt$YEAR_m, rt$SID),]

WRT_result_eco = rt %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(L_RT_iso ~ YEAR_m,
                                                                                data=., paired=TRUE)))

write.csv(WRT_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_WRT.csv")

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

write.csv(precip_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_PRECIPITATION.csv")

#National
precip_result_nat = dat_red %>% do(tidy(t.test(Precip_mm_total_yr ~ YEAR_m,
                                               data=., paired=TRUE)))
#estimate statistic p.value parameter conf.low conf.high method  alternative
#<dbl>     <dbl>   <dbl>     <dbl>    <dbl>     <dbl> <chr>   <chr>      
#1     33.0      2.63 0.00905       347     8.26      57.7 Paired~ two.sided                                                                       

#########
# SUMMER TEMP
temp_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(temp_degC_summer ~ YEAR_m,
                                                                                      data=., paired=TRUE)))

write.csv(temp_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_TEMPERATURE.csv")
# National
temp_result_nat = dat_red %>% do(tidy(t.test(temp_degC_summer ~ YEAR_m,
                                                                                      data=., paired=TRUE)))
#  estimate statistic   p.value parameter conf.low conf.high method
#1    -2.01     -42.3 4.09e-139       347    -2.11     -1.92 Paire~

#########
# MEAN TEMP FULL YEAR
mean_temp_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(Temp_degC_avg_yr ~ YEAR_m,
                                                                                           data=., paired=TRUE)))

write.csv(mean_temp_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_MEAN_TEMPERATURE.csv")

# National
mean_temp_result_nat = dat_red %>% do(tidy(t.test(Temp_degC_avg_yr ~ YEAR_m,
                                                  data=., paired=TRUE)))
                                                                                           
#  estimate statistic  p.value parameter conf.low conf.high method alternative
#1    -1.01     -21.6 5.55e-66       347    -1.11    -0.922 Paire~ two.sided
########
# PHDI
phdi_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(PHDI ~ YEAR_m,
                                                                                      data=., paired=TRUE)))

write.csv(phdi_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_PHDI.csv")

# National
phdi_result_nat = dat_red %>% do(tidy(t.test(PHDI ~ YEAR_m,
                                             data=., paired=TRUE)))
                                                                                      
phdi_result_nat
# estimate statistic p.value parameter conf.low conf.high method  alternative
#1   -0.357     -2.98 0.00305       347   -0.592    -0.122 Paired~ two.sided 


########
# P - E
PE_result_eco = dat_red %>% group_by(Lake_Origin_use,ECOREG_use) %>% do(tidy(t.test(P_E ~ YEAR_m,
                                                                                    data=., paired=TRUE)))

write.csv(PE_result_eco,"C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_P_E.csv")



################
# Paired t-test results using log10 transformed variables (drawdown & WRT); E:I not transformed

Horiz <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_horiz.csv")

Vert <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_vert.csv")
  
E_I <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_EI.csv")

WRT <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_WRT.csv")

#########
## Reorder variables
#########
Horiz$ECOREG_use <- ordered(Horiz$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
Vert$ECOREG_use <- ordered(Vert$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
E_I$ECOREG_use <- ordered(E_I$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
WRT$ECOREG_use <- ordered(WRT$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

############
# PLOT SPECIFICATIONS
############
setwd("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED")

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



##########
## LOG 10 Horizontal DD
##########

# MODIFICATION
#   Dropping Man-made UMW lakes because only n=2 lakes

horiz_umw_man<-Horiz[which(Horiz$ECOREG_use=="UMW" & Horiz$Lake_Origin_use=="MAN_MADE"),]
# Create factor combining ecoregion and lake origin
Horiz$ECO_LO <- with (Horiz, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(Horiz$ECO_LO)

Horiz_mod<-Horiz[!(Horiz$ECO_LO %in% c("UMW_MAN_MADE")),]

horiz <-ggplot(Horiz_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
                position=position_dodge((width=0.5)))+
  #scale_y_continuous(limits=c(NA,1.5),breaks=-1,0,1)+
  #ggtitle("Change in Horizontal drawdown 2012-2007")+
  #CANNOT USE log axis bc have negative values scale_y_continuous(trans="log10",limits=c(NA,200),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
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
  ylim(-1.8,1.2)+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(expression(paste(Delta , "Horizontal drawdown (log10)")))+ #"Change in \nHorizontal drawdown (log10)"
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))



##########
## LOG10 Vertical DD
##########
# DROPPING UMW_MAN_MADE 
# Create factor combining ecoregion and lake origin
Vert$ECO_LO <- with (Vert, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(Vert$ECO_LO)

Vert_mod<-Vert[!(Vert$ECO_LO %in% c("UMW_MAN_MADE")),]

vert <-ggplot(Vert_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
  ylab(expression(paste(Delta , "Vertical drawdown (log10)")))+ #"Change in \nVertical drawdown (log10)"
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))

##########
## E:I
##########
# DROPPING UMW_MAN_MADE 
# Create factor combining ecoregion and lake origin
E_I$ECO_LO <- with (E_I, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(E_I$ECO_LO)

E_I_mod<-E_I[!(E_I$ECO_LO %in% c("UMW_MAN_MADE")),]

EI <-ggplot(E_I_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
  ylab(expression(paste(Delta ,"Evaporation: Inflow")))+ # "Change in \nEvaporation: Inflow"
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))


##########
## WRT
##########
# DROPPING UMW_MAN_MADE 
# Create factor combining ecoregion and lake origin
WRT$ECO_LO <- with (WRT, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(WRT$ECO_LO)

WRT_mod<-WRT[!(WRT$ECO_LO %in% c("UMW_MAN_MADE")),]

wrt <-ggplot(WRT_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position= c(0.8,0.2))+#"bottom")+
  #legend.text=element_text(family="RMN"))+
  #legend.position="none")+
  ylab(expression(paste(Delta , "Water residence time (log10)")))+ #"Change in \nWater residence time (log10)"
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15),labels=c("Man-made","Natural"))



### TRY USING NON TRANSFORMED RESULTS ###
## BEST NOT TO USE because the differences do not lead to hte same conclusions
# Nontransformed Horizontal data
#Horiz_nt <- read.csv("M:/Net MyDocuments/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_horiz_NT.csv")
#Horiz_nt$ECOREG_use <- ordered(Horiz_nt$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

#horiz_nt <-ggplot(Horiz_nt, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
#  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
#  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
#  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
#                position=position_dodge((width=0.5)))+
  #ggtitle("Change in Horizontal drawdown 2012-2007")+
#  theme_bw(base_size=12)+
#  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
#        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
#        axis.text.y = element_text(family = "RMN"),
#        axis.title.y=element_text(family="RMN"),
#        axis.title.x=element_text(family="RMN"),
#        panel.grid.major =  element_line(colour = NA), 
#        panel.grid.minor=element_line(colour = NA),
#        panel.spacing = unit(c(1,1,0,4), "lines"),
#        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
#  ylab("Change in \nHorizontal drawdown (m)")+
#  xlab(NULL)+ #"Ecoregion"
#  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
#  scale_shape_manual(values=c(16,15))


#######################################
# MULTIPLE PLOT PANEL
# Two variables
# 8/10/18
########################
# HORIZ & VERT
tiff(filename="RESAMPLED_CHANGE_LOG_HORIZ_VERT_01JUL19.tiff",width=6, height=4, units="in", res=600) # 07JUN1909FEB18
multiplot(horiz,vert, cols=2)
dev.off()

# E:I & WRT
tiff(filename="RESAMPLED_CHANGE_EI_LOGWRT_01JUL19.tiff",width=6.2, height=4, units="in", res=600) # 14MAY18
multiplot(EI,wrt,cols=2)
dev.off()

## ALL PLOTS 8/6/19
tiff(filename="RESAMPLED_CHANGE_HYDRO_ALL_06AUG19.tiff",width=6.5, height=7, units="in", res=600, compression="lzw") # 07JUN1909FEB18
multiplot(horiz,EI,vert,wrt, cols=2)
dev.off()

##############################
## PAIRED t-test results WEATHER ATTRIBUTES
## See if mean difference between survey years is significantly different from zeron
##############################
# Paired t-test results NOT TRANSFORMED weather
precip <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_PRECIPITATION.csv")
temp <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_TEMPERATURE.csv")
mean_temp<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_MEAN_TEMPERATURE.csv")
phdi <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_PHDI.csv")
pe <- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/PAIRED_ttest_lk_type_ECO_P_E.csv")

#########
## Reorder variables
#########
precip$ECOREG_use <- ordered(precip$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
temp$ECOREG_use <- ordered(temp$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
mean_temp$ECOREG_use <- ordered(mean_temp$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
phdi$ECOREG_use <- ordered(phdi$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))
pe$ECOREG_use <- ordered(pe$ECOREG_use, levels=c("WMT","XER","NPL","SPL","TPL","UMW","SAP","NAP","CPL"))

########
## PRECIPITATION
# DROPPING UMW_MAN_MADE 
# Create factor combining ecoregion and lake origin
precip$ECO_LO <- with (precip, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(precip$ECO_LO)

precip_mod<-precip[!(precip$ECO_LO %in% c("UMW_MAN_MADE")),]

PRECIP <-ggplot(precip_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
  ylab(expression(paste(Delta ,"Precipitation (mm)")))+ #"Change in Precipitation (mm)"
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))



########
## SUMMER TEMP
# Create factor combining ecoregion and lake origin
temp$ECO_LO <- with (temp, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(temp$ECO_LO)

temp_mod<-temp[!(temp$ECO_LO %in% c("UMW_MAN_MADE")),]

TEMP <-ggplot(temp_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position= c(0.8,0.2))+#"bottom")+
        #legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(expression(paste(Delta, "Summer temperature (",~degree*C,")")))+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural"))


########
## MEAN TEMP
# Create factor combining ecoregion and lake origin
mean_temp$ECO_LO <- with (mean_temp, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(mean_temp$ECO_LO)

mean_temp_mod<-mean_temp[!(mean_temp$ECO_LO %in% c("UMW_MAN_MADE")),]

MEAN_TEMP <-ggplot(mean_temp_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position= c(0.8,0.2))+#"bottom")+
       #legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab(expression(paste(Delta,"Mean temperature (",~degree*C,")")))+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural"))

########
## PHDI
# Create factor combining ecoregion and lake origin
phdi$ECO_LO <- with (phdi, interaction(ECOREG_use,Lake_Origin_use,sep="_"))
table(phdi$ECO_LO)

phdi_mod<-phdi[!(phdi$ECO_LO %in% c("UMW_MAN_MADE")),]

PHDI <-ggplot(phdi_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
  ylab(expression(paste(Delta, "PHDI")))+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))


########
## NET ATMOSPHERIC HYDRO FLUX (P - E)
# Create factor combining ecoregion and lake origin
pe$ECO_LO <- with (pe, interaction(ECOREG_use,Lake_Origin_use,sep="_"))

pe_mod<-pe[!(pe$ECO_LO %in% c("UMW_MAN_MADE")),]

PE <-ggplot(pe_mod, aes(x=ECOREG_use, y = estimate, color=Lake_Origin_use, shape=Lake_Origin_use))+
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2) + # to change size ,aes(size=2); drop legend for aes elements, show.legend=F
  geom_hline(yintercept = 0, size = I(0.2), color = I("black"))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width = .1,
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
  ylab("Change in (P - E) water year")+
  xlab(NULL)+ #"Ecoregion"
  scale_color_manual(values = c("#d95f02","#1f78b4"), labels=c("Man-made","Natural"))+
  scale_shape_manual(values=c(16,15))

#######################################
# MULTIPLE PLOT PANEL
# Two variables
# 8/22/18
########################
# Precip & MEAN Temperature
tiff(filename="RESAMPLED_CHANGE_PRECIP_MEAN_TEMP_01JUL19.tiff",width=6, height=4, units="in", res=600) # 09FEB18
multiplot(PRECIP,MEAN_TEMP, cols=2)
dev.off()

#OLD
# Precip & SUMMER Temperature
tiff(filename="RESAMPLED_CHANGE_PRECIP_TEMP_22AUG18.tiff",width=6, height=4, units="in", res=600) # 09FEB18
multiplot(PRECIP,TEMP, cols=2)
dev.off()

# PHDI
tiff(filename="RESAMPLED_CHANGE_PHDI_01JUL19.tiff",width=3, height=4, units="in", res=600) # 14MAY18
multiplot(PHDI,cols=2)
dev.off()

# PHDI & P-E
tiff(filename="RESAMPLED_CHANGE_PHDI_PE_07JUN19.tiff",width=6, height=4, units="in", res=600) # 14MAY18
multiplot(PHDI, PE, cols=2)
dev.off()

###########
## ALL CLIMATE FOR SURVEY YEAR 8/6/19
# LZW Compression for JAWRA upload
tiff(filename="RESAMPLED_CHANGE_ALL_CLIMATE_06AUG19.tiff",width=6.5, height=7, units="in", res=600, compression="lzw") # 09FEB18
multiplot(PRECIP,PHDI,MEAN_TEMP, cols=2)
dev.off()


##################################
### BIPLOTS OF DELTA LAKE HYDRO VS DELTA CLIMATE
##    FOR SUPPLEMENTAL INFORMATION
### 6/7/19
## 7/1/19 - updated datasets - revised 2012 WRT
##################################
# To get correlation coefficients in panels
# stat_cor https://rpkgs.datanovia.com/ggpubr/reference/stat_cor.html
# other https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/
# change font size https://stackoverflow.com/questions/48550525/ggpubr-change-font-size-of-stat-compare-means-kruskal-wallis-p-values

#################
## READ modified dataset - WIDE form with differences calculated between years 
##  n=348 lakes
#################
nla0712<-read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_RESAMPLED_WIDE_for_PLOTS_01JUL19.csv") #02AUG18

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
nla0712$PE_DIFF <- nla0712$P_E.y - nla0712$P_E.x


z<-tapply(nla0712$PHDI.x, nla0712$ECOREG_use,summary)
z2<-tapply(nla0712$PHDI.y, nla0712$ECOREG_use,summary)

capture.output(z, file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/a_Project_lake_drawdown_patterns/Figures/PHDI_NLA07.txt")
capture.output(z2, file="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/a_Project_lake_drawdown_patterns/Figures/PHDI_NLA12.txt")

#######################
##
## Scatterplots by ecoregion and lake type
## CHange in Lake hydrology vs. Change in climate
## USE THIS ONE FOR SUPPLEMENTARY MATERIAL
## 11/16/18
## 6/7/19
## 7/1/19
##########

############
# All LAKES - COLORED BY LAKE TYPE - SEPARATE ECOREGION PANELS#

# Drop UMW Man-made lakes
nla0712$ECO_LO<-with(nla0712, interaction(ECOREG_use, Lake_Origin_use, sep="_"))  

nla0712_mod<-nla0712[!(nla0712$ECO_LO %in% c("UMW_MAN_MADE")),]

# Set font size for correlation coefficients and p-value
your_font_size<-2.5

EI <- ggplot(nla0712_mod,
             aes(x=PHDI_DIFF, y=E_I_DIFF,color=Lake_Origin_use,shape=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2)+
  scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  scale_shape_manual(values=c(16,15)) +
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
  xlab(expression(paste(Delta,"PHDI"))) +
  stat_cor(method="spearman", size=your_font_size,label.x=-0.5, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))


vert <- ggplot(nla0712_mod,
               aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF, color=Lake_Origin_use,shape=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2)+
  scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  scale_shape_manual(values=c(16,15)) +
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
  xlab(expression(paste(Delta,"PHDI")))+
  stat_cor(method="spearman", size=your_font_size,label.x=-0.5, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))



##############################
## COMBINING ALL OBSERVATIONS TOGETHER - NOT BREAKING UP BY ECOREGION
your_font_size<- 4
EI_all <- ggplot(nla0712,
             aes(x=PHDI_DIFF, y=E_I_DIFF,color=Lake_Origin_use, shape=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2)+
  scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  scale_shape_manual(values=c(16,15)) +
  geom_smooth(method=lm, se=FALSE)+
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
         legend.position="none")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI")))+
  stat_cor(method="spearman", size=your_font_size,label.x=0.5, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))


vert_all <- ggplot(nla0712,
               aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF, color=Lake_Origin_use, shape=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(position=position_dodge((width=0.5)),aes(colour=Lake_Origin_use, shape=Lake_Origin_use),size=2)+
  scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) +
    geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
#  facet_wrap(~ECOREG_use)+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position="bottom")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"PHDI")))+
  stat_cor(method="spearman", size=your_font_size,label.x=0.5, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

## Correlation between delta vert and delta E:I
VERT_EI_all <- ggplot(nla0712,
                 aes(x=L_DDVrtDix_sc_DIFF, y=E_I_DIFF,color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+
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
        legend.position="none")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  stat_cor(method="spearman", size=your_font_size,label.x=0.5, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))



##################
## EXPORT GRAPHS
##################
# E:I  and scaled Vert vs PHDI BY ECOREGION
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/DELTA_EI_PHDI_ECO.tiff",width=6, height=4, units="in", res=600)
EI
dev.off()

tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/DELTA_VERT_PHDI_ECO.tiff",width=6, height=4.2, units="in", res=600)
vert
dev.off()


## EI & scaled Vert vs PHDI GROUPED TOGETHER
# BOTH YEARS ON SAME FILE
tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/DELTA_VERT_EI_PHDI_ALL.tiff",
     width=4, height=7, units="in", res=600)
multiplot(EI_all,vert_all)
dev.off()

#tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/DELTA_EI_PHDI_ALL.tiff",width=6, height=4, units="in", res=600)
#EI_all
#dev.off()

#tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/DELTA_VERT_PHDI_ALL.tiff",width=6, height=4.2, units="in", res=600)
#vert_all
#dev.off()


#####################
## DELTA LAKE HYDRO vs DELTA P-E
#####################

## COMBINING ALL OBSERVATIONS TOGETHER - NOT BREAKING UP BY ECOREGION
EI_PE_all <- ggplot(nla0712,
                 aes(x=PE_DIFF, y=E_I_DIFF,color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+
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
        legend.position="none")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"(P-E)")))+
  stat_cor(method="spearman", size=your_font_size,label.x=0.2, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))


vert_PE_all <- ggplot(nla0712,
                   aes(x=PE_DIFF, y=L_DDVrtDix_sc_DIFF, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  #  facet_wrap(~ECOREG_use)+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position="bottom")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"(P-E)")))+
  stat_cor(method="spearman", size=your_font_size,label.x=0.5, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

## BY ECOREGION
your_font_size<- 2.5
EI_PE <- ggplot(nla0712_mod,
                    aes(x=PE_DIFF, y=E_I_DIFF,color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+
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
        legend.position="none")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"(P-E)"))) +
  stat_cor(method="spearman", size=your_font_size,label.x=0, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))


vert_PE <- ggplot(nla0712_mod,
                      aes(x=PE_DIFF, y=L_DDVrtDix_sc_DIFF, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
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
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position="bottom")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"(P-E)")))+
  stat_cor(method="spearman", size=your_font_size,label.x=0, #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

######################
## SUMMARIZE CHANGE IN VERTICAL DRAWDOWN

nla0712%>%
  group_by(ECO_LO) %>%
  filter(!is.na(HorizDD_use.x)) %>%
  summarise(mean_horz07 = mean(HorizDD_use.x),mean_horz12 = mean(HorizDD_use.y),
            mean_vert07 = mean(VertDD_use.x),mean_vert12 = mean(VertDD_use.y),
            mean_vertdiff=mean(VertDD_DIFF))

####################################
## Create E:I and WRT ISOTOPE classes

## Take mean of EI and WRT between years and create summarizing variable
nla0712$EI_avg <- (nla0712$E_I.x + nla0712$E_I.y)/2
nla0712$RT_iso_avg <- (nla0712$RT_iso.x + nla0712$RT_iso.y)/2

nla0712$EI_class<-cut(nla0712$EI_avg, breaks=c(0,0.20,0.50,Inf),labels=c("Low","Moderate","High"))

table(nla0712$EI_class) 
#Low Moderate     High 
#176      120       47
plot(nla0712$L_DDVrtDix_sc_DIFF~nla0712$EI_class)

## WRT Classes
nla0712$WRT_class<-cut(nla0712$RT_iso_avg, breaks=c(0,0.50,1.0,Inf),labels=c("Short","Moderate","Long"))
table(nla0712$WRT_class)

#Short Moderate     Long 
#148       94       88 

plot(nla0712$L_DDVrtDix_sc_DIFF~nla0712$WRT_class)

## PHDI relationship with lake hydro by isotope class
EI_iso <- ggplot(nla0712,
                 aes(x=PHDI_DIFF, y=E_I_DIFF,color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  #scale_y_continuous()+
  #scale_y_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) + 
  #scale_x_continuous(trans="log10",limits=c(NA,2),breaks=c(0.01, 0.05,0.25,1.0)) +
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+
  facet_wrap(~EI_class)+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        legend.position="none")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"E:I")))+
  xlab(expression(paste(Delta,"PHDI")))


vert_iso <- ggplot(nla0712,
                   aes(x=PHDI_DIFF, y=L_DDVrtDix_sc_DIFF, color=Lake_Origin_use))+ #,, color=ECOREG_use color=ECOREG_use.x
  geom_point(shape=16)+
  scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  facet_wrap(~WRT_class)+
  theme_bw(base_size=14) + # To change to dark background theme_dark(base_size=14) To change to white background - theme_bw
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN"),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text=element_text(family="RMN"),
        legend.title=element_blank(),
        legend.text=element_text(family="RMN"),
        legend.position="bottom")+
  ggtitle(NULL)+
  ylab(expression(paste(Delta,"Log Vertical DD (scaled)")))+
  xlab(expression(paste(Delta,"PHDI")))