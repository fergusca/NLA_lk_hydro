###########################
## SAMPLE DATE BETWEEN SURVEY YEAR COMPARISON
##
##  7/9/19
###########################
rm(list=ls())

library(ggplot2)
library(ggpubr) # for correlation coeff in ggplot


#############
## LOAD DATA 
##  RESAMPLED LAKES LONG FORMAT - lakes sampled 2007 & 2012
#n = 348 lakes resampled in both surveys

nla0712_merge<- read.csv("C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Data/nla07_12_rbind_LONG_REVISITS_SINGLE.csv")
names(nla0712_merge)

## CLEAN UP ##
#Dropping variables
todrop<-names(nla0712_merge)%in%c("X")
nla0712_merge<-nla0712_merge[!todrop]


########################
## RESAMPLED LAKES
##  SEE WHETHER SAMPLE TIME DURING SEASON CHANGED IN ONE DIRECTION BY SURVEY YEAR
##

table(nla0712_merge$YEAR)
# 2007 2012 
#  348  348

# Specify date format
str(nla0712_merge$DATE_COL_iso)
nla0712_merge$date_j<-as.Date(nla0712_merge$DATE_COL_iso, 
                              format = "%m/%d/%Y") # Note capital Y for 2007 vs 07

# JULIAN DATE - day of year number
nla0712_merge$date_j<-format(nla0712_merge$date_j, "%j")
str(nla0712_merge$date_j)

nla0712_merge$date_j <-as.numeric(nla0712_merge$date_j)
summary(nla0712_merge$date_j)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#129.0   184.0   205.0   205.4   227.0   275.0
# Min is 5/10 and max is 10/3, median = 7/25

head(nla0712_merge$date_j)

###############
## SUBSET RESAMPLED LAKES BY YEAR
nla07_revisit <- nla0712_merge[which(nla0712_merge$YEAR == 2007),]
nla12_revisit <- nla0712_merge[which(nla0712_merge$YEAR==2012),]

### MERGE TOGETHER
repeatdf_wide <- merge(nla07_revisit,nla12_revisit, by="SID")

#############
## Calculate difference in sample dates (2012-2007)
repeatdf_wide$date_j.diff <- repeatdf_wide$date_j.y-repeatdf_wide$date_j.x

hist(repeatdf_wide$date_j.diff)
# DIFFERENCE IN SAMPLING DATES FOR MS
summary(repeatdf_wide$date_j.diff)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-118.00  -35.00   -8.00  -11.08   15.00   95.00 
sd(repeatdf_wide$date_j.diff) #[1] 36.80589


###############
## SAMPLING DATES
# 2007
summary(nla07_revisit$date_j) #(5/30 - 10/2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#150.0   192.8   207.0   210.9   231.2   275.0 
sd(nla07_revisit$date_j) # 26.39198

# 2012
summary(nla12_revisit$date_j) # 129 (5/9 - 9/28)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   129.0   177.0   199.5   199.8   222.0   271.0
sd(nla12_revisit$date_j) # 31.20856


#############
# Cumulative distribution plot
#https://rpubs.com/tgjohnst/cumulative_plotting
plot(ecdf(repeatdf_wide[,"date_j.diff"]))
plot(ecdf(repeatdf_wide$date_j.diff), cex=1)

z<-repeatdf_wide[which(repeatdf_wide$date_j.diff<35),]#260
z2 <-repeatdf_wide[which(repeatdf_wide$date_j.diff<(-35)),]#33
(260-33)/348*100 #65%
z3<-repeatdf_wide[which(repeatdf_wide$date_j.diff>89),]#7 obs
z4<-repeatdf_wide[which(repeatdf_wide$date_j.diff<(-89)),]#1 obs
8/348*100 # 2.29%

# Difference in sample day (day number in calendar yr) is distributed around zero
# There does not seem to be a tendancy for lakes to be sampled 
#  at different times during the season
# Most observations fall around 0 indicating that lakes 
#  were sampled around roughly the same time of year in 2007 and 2012

###########
## PHIL suggested looking at the ccorrelation between time and difference in water level

## Calculate change in drawdown between yrs in resampled lakes
repeatdf_wide$L_DDHzSqrtA_sc.diff <- repeatdf_wide$L_DDHzSqrtA_sc.y - repeatdf_wide$L_DDHzSqrtA_sc.x
repeatdf_wide$L_DDVrtDix_sc_MOD.diff <- repeatdf_wide$L_DDVrtDix_sc_MOD.y - repeatdf_wide$L_DDVrtDix_sc_MOD.x
repeatdf_wide$E_I.diff <- repeatdf_wide$E_I.y - repeatdf_wide$E_I.x
repeatdf_wide$RT_iso.diff <- repeatdf_wide$RT_iso.y - repeatdf_wide$RT_iso.x

# Look at scaled drawdown (log) values between years; log horiz_sc is always positive - because 2012 is negative value and when take difference we get a positive value
#   Better to use untransformed scaled drawdown
head(repeatdf_wide[c(1,21,24, 205,225,228,409,410,411,412)])

plot(repeatdf_wide$L_DDHzSqrtA_sc.diff~repeatdf_wide$date_j.diff)
repeatdf_wide$DDHzSqrtA_sc.diff <- repeatdf_wide$DDHzSqrtA_sc.y - repeatdf_wide$DDHzSqrtA_sc.x
repeatdf_wide$DDVrtDix_sc_MOD.diff <- repeatdf_wide$DDVrtDix_sc_MOD.y - repeatdf_wide$DDVrtDix_sc_MOD.x


# HISTOGRAM OF SAMPLING JULIAN DATE BY SURVEY YEAR
#http://www.sthda.com/english/wiki/ggplot2-histogram-easy-histogram-graph-with-ggplot2-r-package
#install.packages("devtools")
#library(devtools)
#install_github("easyGgplot2", "kassambara")
#library(easyGgplot2)
#ggplot2.histogram(data=repeatdf, xName="date_j",
#                  groupName='YEAR', legendPosition="top",
#                  alpha=0.5, addDensity=True,
#                  addMeanLine=TRUE, meanLineColor="white", meanLineSize=1.5)

# https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
nla0712_merge$YEAR_f <-factor(nla0712_merge$YEAR, levels=c(2007,2012))
hist_jdate<-ggplot(nla0712_merge, aes(date_j, fill = YEAR_f)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), binwidth=5,position = 'identity')

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

###################
## NATIONAL BIPLOTS BY LAKE TYPE
# Specify coefficient font size
your_font_size<- 5


###########
## scaled horizontal dd (untransformed) vs. difference in sample date
sc_horiz <- ggplot(repeatdf_wide,
                       aes(x=date_j.diff, y=L_DDHzSqrtA_sc.diff))+#, color=Lake_Origin_use.x, shape=Lake_Origin_use.x))+ #,, color=ECOREG_use color=ECOREG_use.x
  #geom_point(aes(colour=Lake_Origin_use.x, shape=Lake_Origin_use.x),size=2)+
  #scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  #scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) +
  geom_point(shape=16)+
  #scale_color_manual(values=c("red","black"))+
  geom_smooth(method=lm, se=FALSE)+ # ,colour="black"
  #facet_wrap(~ECOREG_use)+
  #scale_y_continuous(trans="log10",limits=c(NA,70),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
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
  ylab("Log(DDhoriz_2012/DDhoriz_2007)")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("Difference julian date (2012-2007)") + #(expression(paste(Delta,"E:I"))) +
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

## change in scaled Vertical dd (untransformed) vs. difference in sample date
sc_vert <- ggplot(repeatdf_wide,
                   aes(x=date_j.diff, y=L_DDVrtDix_sc_MOD.diff))+#, color=Lake_Origin_use.x, shape=Lake_Origin_use.x))+ #,, color=ECOREG_use color=ECOREG_use.x
  #geom_point(aes(colour=Lake_Origin_use.x, shape=Lake_Origin_use.x),size=2)+
  #scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  #scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) +
  geom_point(shape=16)+
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
  ylab("Log(DDvert_2012/DDvert_2007)")+ #Log(DDvert_2012/DDvert_2007) (expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("Difference julian date (2012-2007)") + #(expression(paste(Delta,"E:I"))) +
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

# Correlation
cor(repeatdf_wide$L_DDVrtDix_sc_MOD.diff,repeatdf_wide$date_j.diff, use="complete.obs", method="spearman")
# rho=0.115795 p = 0.04
cor.test(repeatdf_wide$L_DDVrtDix_sc_MOD.diff,repeatdf_wide$date_j.diff, use="complete.obs", method="spearman")

## EI vs. difference in sample date
sc_EI <- ggplot(repeatdf_wide,
                   aes(x=date_j.diff, y=E_I.diff))+#, color=Lake_Origin_use.x, shape=Lake_Origin_use.x))+ #,, color=ECOREG_use color=ECOREG_use.x
  #geom_point(aes(colour=Lake_Origin_use.x, shape=Lake_Origin_use.x),size=2)+
  #scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  #scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) +
  geom_point(shape=16)+
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
  ylab("Change in E:I")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("Change in julian date") + #(expression(paste(Delta,"E:I"))) +
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))

## WRT vs. difference in sample date
# drop lakes with large change in WRT
repeatdf_red<-repeatdf_wide[which(repeatdf_wide$RT_iso.diff<10),] #drops 14 obs
sc_WRT <- ggplot(repeatdf_red,
                aes(x=date_j.diff, y=RT_iso.diff))+#, color=Lake_Origin_use.x, shape=Lake_Origin_use.x))+ #,, color=ECOREG_use color=ECOREG_use.x
  #geom_point(aes(colour=Lake_Origin_use.x, shape=Lake_Origin_use.x),size=2)+
  #scale_color_manual(values=c("#d95f02","#1f78b4"),labels=c("Man-made","Natural"))+ # c("red","black")
  #scale_shape_manual(values=c(16,15), labels=c("Man-made","Natural")) +
  geom_point(shape=16)+
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
  ylab("Change in WRT")+ #(expression(paste(Delta,"Log Vertical DD (scaled)")))
  xlab("Change in julian date") + #(expression(paste(Delta,"E:I"))) +
  stat_cor(method="spearman", size=your_font_size,label.x.npc=0.65,label.y.npc="top", #..p.label..,sep= "~','~")))
           aes(label = paste( ..r.label.., cut(..p..,
                                               breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
                                               labels=c("'****'","'***'","'**'","'*'", "'ns'")),
                              sep= "~")))


#############################
## BOX PLOTS Julian date by survey year for resampled lakes

day_sampled<-ggplot(subset(nla0712_merge), aes(x=factor(YEAR), y=date_j),
                  fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(YEAR),y=date_j))+ #middle=`50Pct`, upper=`75Pct`, ymax=`90Pct` # 
  #scale_y_continuous(trans="log10",limits=c(NA,70),  breaks=c(0,0.5,1,2.5,5,10,25,50)) +
  scale_fill_manual(values = c("#808080","#E0E0E0"))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+
  #legend.title=element_blank(),
  #legend.text=element_text(family="RMN"))+
  ylab("Julian day")+ #"Horizontal Drawdown (m)"
  xlab(NULL)#+

###############
## WRITE FIGURES
###############

tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/HIST_Julian_date.tiff",
     width=4, height=4, units="in", res=200)
hist_jdate
dev.off()

tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/Diff_Julian_date_scaled_DD.tiff",
     width=6, height=8, units="in", res=200)
multiplot(sc_vert,sc_horiz) #,sc_EI,sc_WRT
dev.off()

tiff(filename="C:/Users/EFergus/OneDrive - Environmental Protection Agency (EPA)/a_Water_Level/Analysis/Year_comparisons/Routput/PLOTS/RESAMPLED/Julian_date_boxplot.tiff",
     width=6, height=8, units="in", res=200)
day_sampled
dev.off()
