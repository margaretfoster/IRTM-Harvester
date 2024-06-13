## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Tailored to Uganda Round 6 (based on Nigeria script)
## Specific to THETA NxKxS array and Afrobarometer demographic data
## Prepare space:
rm(list=ls())

#######
## Load needed packages, install if needed:
#####

loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}

packs <- c("ggplot2", "RColorBrewer", "readxl",
           "latex2exp", "reshape2", "ggridges",
           "rlang", "stargazer", "stringr")

loadPkg(packs)

####################
## Load Data

setwd("~/Dropbox/IRTM-Harvester/code") ## customize to project directory

##dataPath <- "~/Dropbox/Research/Interventions/Theory-IRT/afrobarometer/Results/" ## THETAS

dataPath <- "./Code_All_Countries/"## THETAS

thetasPath  <-"../Results/"
abdata <- "../Afrobarometer-data/"

load(paste0(abdata, "dfWithABEthnicGroupsR6.Rdata")) ## Loads a 466 x 2 df with names
## of ethnic groups and their AB codes
load(paste0(abdata,"ugandaR6RawAB.Rdata")) ## made by breakdownABRounds.R
## of ethnic groups and their AB codes

ugandaRegions <-  read_excel(paste0(abdata, "Uganda_district_to_region.xlsx"))


###########################
## ROUND 6
###########################

load(paste0(dataPath, "UgandaUganda_R6_direct_THETA.RData")) ## Thetas

ABRound <- ugandaR6


dim(ABRound)## 2400 x 360
dim(THETA)

## Amend THETAS to the survey data:
anchors <- 1:6 ## anchor entries
meanThetas<- apply(THETA, c(1,2), mean) ## take mean
meanThetas <- meanThetas[-anchors,] ## remove anchors
dim(meanThetas)[1] == dim(ABRound)[1] ## verify same length

### Merge data:

mergedABThetas <- cbind(ABRound, meanThetas)
colnames(mergedABThetas) <- c(colnames(ABRound),
                              c("Theta1", "Theta2", "Theta3"))


#################
##  Add in human-readable ethnic group names
#######################
egv <- "q87" ## Merged R6, ethnic group question is q87

colnames(mergedABThetas) 

table(mergedABThetas[,egv])

colnames(AB.ethnic.groups) ## mapping of ethnic group code to names
AB.ethnic.groups$ethnicGroupsR6 <- as.numeric(
  AB.ethnic.groups$ethnicGroupsR6)

#### Merge the ethnic group code with the
## data frame of written-out group names

ABThetasR6 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR6")

dim(ABThetasR6)
colnames(ABThetasR6)

## Regions:

## Region:

## check that the district spellings are the same: 
## And the wikipedia list covers all AB districts
wikidistricts <- as.character(ugandaRegions$District)
abdistricts <- str_to_title(as.character(ABThetasR6$locationlevel2))
setdiff(abdistricts, wikidistricts) ## set diff: in x but not y

## Format consistency:

ABThetasR6$locationlevel2 <-  str_to_title(ABThetasR6$locationlevel2) 

ABThetasR6[which(ABThetasR6$locationlevel2 == "Ssembabule"), 
           "locationlevel2"] <- "Sembabule"

abdistricts <- ABThetasR6$locationlevel2
setdiff(abdistricts, wikidistricts) ## set diff: in x but not y

ABThetasR6 <- merge(x=ABThetasR6,
                    y=ugandaRegions,
                    by.x="locationlevel2",
                    by.y="District")

colnames(ABThetasR6)[which(colnames(ABThetasR6)=="Region")] <- "wikiregion"

table(ABThetasR6$wikiregion)


## Theta locations:
### write out for 
writeout <- c("dateintr", "latitude", "longitude",
              "wikiregion", "urbrur",
              "Theta1", "Theta2", "Theta3")

geoThetasR6 <- ABThetasR6[,writeout]
geoThetasR6$country <- "Uganda"
geoThetasR6$round <- 6

## This is location to make maps with:
#save(geoThetasR6,
#     file=paste0(dataPath,
#                 "UgandaR6geolocatedThetas.Rdata" ))

## Code for ethnic groups summary
egsummary <-as.data.frame(table(ABThetasR6$V2))
colnames(egsummary) <- c("groupname", "R6Freq")
#save(egsummary, file="UgandaR6egsummary.Rdata")

ABThetasR6$V2 <- as.factor(ABThetasR6$V2)

dim(ABThetasR6) ##2400 x 364


regsummary <-as.data.frame(table(ABThetasR6$wikiregion))
colnames(regsummary) <- c("regionname", "R6freq")

#save(regsummary, file="ugandar6regionsummary.Rdata")


#######

colnames(ABThetasR6)[colnames(ABThetasR6)=="V2"] <- "groupname"
ABThetasR6$wikiregion <- as.factor(ABThetasR6$wikiregion)
ABThetasR6$groupname <- as.factor(ABThetasR6$groupname)


##########################                                                                                                                                 
## Write location sub-data to map                                                                                                                          
cols <- c("Theta1", "Theta2" , "Theta3" ,
          "groupname",  "wikiregion", "urbrur",
          "latitude", "longitude")

R6map <- ABThetasR6[,cols]
R6map$round <- 6

#save(R6map, file=paste0(dataPath,"UgandaR6mapdat.Rdata"))


#####################
## Plots
#####################
#####################
### By Ethnic Group
#####################

## THETA 1
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta1,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Uganda, Round 6 (2014), By Ethnic Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r6-theta1_group_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Uganda, Round 6 (2014), By Ethnic Group') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r6-theta2_group_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Uganda, Round 6 (2014), By Ethnic Group') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r6-theta3_group_mean_posterior.png',
       width = 10, height=5)


#########################
### By Region
########################

## THETA 1
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta1,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 1',
          subtitle='Uganda, Round 6 (2014), By Region') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupnames)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r6-theta1_region_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 2',
          subtitle='Uganda, Round 6 (2014), By Region') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$wikiregion)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r6-theta2_region_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 3',
          subtitle='Uganda, Round 6 (2014), By Region') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r6-theta3_region_mean_posterior.png',
       width = 10, height=5)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Template code for using an Urban-Rural facet wrap to drill-down
## Urban-Rural
## Codes for urban or rural sampling unit
## 1=urban, 2=rural
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Recode for ease of interpretation:

ABThetasR6[which(ABThetasR6$urbrur==1), "urbrur"] <- "urban"
ABThetasR6[which(ABThetasR6$urbrur==2), "urbrur"] <- "rural"


ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta1,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R6-theta1_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R6-theta2_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 3
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R6-theta3_urbrur_mean_posterior.png',
       width = 10, height=5)
