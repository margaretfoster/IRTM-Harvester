### This script identifies
## a few drill-down examples to 

rm(list=ls())

loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}
packs <- c("ggplot2", "RColorBrewer",
           "latex2exp", "reshape2", "ggridges",
           "rlang", "stargazer", 
           "xtable", "dplyr")

loadPkg(packs)

thetasPath  <-"./Results/"

load(paste0(thetasPath, "all-Thetas.Rds"))

all_df$country <- as.factor(all_df$country)

length(levels(all_df$country)) ## 32 countries

##%%%%%%%%%%%%%%%%%%%
##%%%%%%%%%%%%%%%%%%%
## Black and white visualizations for 
## IRTM-Harvester paper

## specific-drill downs


all_df[which(all_df$urbrur==1), "urbrur"] <- "Urban"
all_df[which(all_df$urbrur==2), "urbrur"] <- "Rural"

country= "Algeria"

dfsubset = subset(all_df, 
                  country == "Algeria")

table(dfsubset$round)
table(dfsubset$urbrur)


gg1 <-  ggplot(dfsubset) +
  geom_density_ridges(aes(x=Theta1,
                        y=urbrur, 
                        fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'darkgray')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle=paste0(country, " Rounds 5 (2013) and 6 (2015), By Urbanization")) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(dfsubset$urbrur)))+
  theme_bw() + theme(legend.position = 'none')+ 
  scale_fill_grey()+
  facet_grid(~round)

gg1

ggsave(gg1, 
       file="Algeria_Example_UrbanRuralDivide.png",
       width=12,
       height=6,
       units='in')

## What is the mean of R6?
mean(dfsubset[which(dfsubset$round=="R6"), "Theta1"])
median(dfsubset[which(dfsubset$round=="R6"), "Theta1"])

## Illustration of drill-down