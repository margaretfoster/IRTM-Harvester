## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Tailored to Uganda Round 5 (based on Nigeria script)
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

packs <- c("ggplot2", "RColorBrewer",
           "latex2exp", "reshape2", "ggjoy",
           "rlang", "stargazer",
           "readxl")

loadPkg(packs)

####################
## Load Data
###################
####################
## Load Data

setwd("~/Dropbox/IRTM-Harvester/code") ## customize to project directory

##dataPath <- "~/Dropbox/Research/Interventions/Theory-IRT/afrobarometer/Results/" ## THETAS

dataPath <- "./Code_All_Countries/"## THETAS

thetasPath  <-"../Results/"
abdata <- "../Afrobarometer-data/"

load(paste0(abdata, "dfWithABEthnicGroupsR5.Rdata")) ## Loads a 466 x 2 df with names

load(paste0(abdata,"ugandaR5RawAB.Rdata")) ## made by breakdownABRounds.R
## of ethnic groups and their AB codes

load(paste0(dataPath, "UgandaUganda_R5_direct_THETA.RData")) ## Thetas

ugandaRegions <- read_excel(paste0(abdata, "Uganda_district_to_region.xlsx"))

###########################
## ROUND 5
###########################


ABRound <- ugandaR5

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



##################
## Ethnic Group Variables
## Add in names of ethnic groups and regions
## Uganda ethnic group question: 84

egv <- "q84"

table(mergedABThetas[,egv])

colnames(AB.ethnic.groups)

AB.ethnic.groups$ethnicGroupsR5 <- as.numeric(
  AB.ethnic.groups$ethnicGroupsR5)


ABThetasR5 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x="q84",
                    by.y="ethnicGroupsR5")

dim(mergedABThetas)
dim(ABThetasR5)

egsummary <-as.data.frame(table(ABThetasR5$V2))
colnames(egsummary) <- c("groupname", "R5Freq")

#save(egsummary,
#     file=paste0("./Data/Intermediate_Data/", "UgandaR5egsummary.Rdata"))

### Region:

colnames(ABThetasR5)


## check that the district spellings are the same: 
## And the wikipedia list covers all AB districts
## In R5, location level 1 seems to be districts
wikidistricts <- as.character(ugandaRegions$District)
abdistricts <- as.character(ABThetasR5$locationlevel1)

setdiff(abdistricts, wikidistricts) ## set diff: in x but not y
## Siroko, Ssembabule, Agogo
## Standardize a few spelling differences:

ABThetasR5[which(ABThetasR5$locationlevel1 == "Ssembabule"), "locationlevel1"] <- "Sembabule"
ABThetasR5[which(ABThetasR5$locationlevel1 == "Siroko"), "locationlevel1"] <- "Sironko"
ABThetasR5[which(ABThetasR5$locationlevel1 == "Agogo"), "locationlevel1"] <-  "Agago"

abdistricts <- as.character(ABThetasR5$locationlevel1)
setdiff(abdistricts, wikidistricts) ## set diff: in x but not y 

ABThetasR5 <- merge(x=ABThetasR5,
                    y=ugandaRegions,
                    by.x="locationlevel1",
                    by.y="District")

colnames(ABThetasR5)[which(colnames(ABThetasR5)=="Region")] <- "wikiregion"

table(ABThetasR5$wikiregion)

###########################
## Write out geolocated thetas

writeout <- c("dateintr", "latitude", "longitude",
              "wikiregion", "urbrur",
              "Theta1", "Theta2", "Theta3")                                                                                                                                                                                                                                             
geoThetasR5 <- ABThetasR5[,writeout]
geoThetasR5$country <- "Uganda"
geoThetasR5$round <- 5   
#save(geoThetasR5,
#     file=paste0(dataPath,
#                 "UgandaR5geolocatedThetas.Rdata" ))

## not using all.x=TRUE, because that gets four rows with an ethnic group
## code of "17", which is not a code that the AB used 
##so seems like a transcription error or junk data.
##Easier to remove it now than to excise it later

ABThetasR5$V2 <- as.factor(ABThetasR5$V2)

dim(ABThetasR5) ##2400 x 364

regsummary <-as.data.frame(table(ABThetasR5$wikiregion))
colnames(regsummary) <- c("regionname", "R5freq")

#save(regsummary,
#    file="ugandar5regionsummary.Rdata")

################################
## Format for graphing aesthetics
##############################
colnames(ABThetasR5)[colnames(ABThetasR5)=="V2"] <- "groupname"
ABThetasR5$wikiregion <- as.factor(ABThetasR5$wikiregion)
ABThetasR5$groupname <- as.factor(ABThetasR5$groupname)

table(ABThetasR5$groupname)
table(ABThetasR5$regionnames)

##########################                                                                                                                                 
## Write location sub-data to map                                                                                                                          
cols <- c("Theta1", "Theta2" , "Theta3" ,
          "groupname",  "wikiregion", "urbrur",
          "latitude", "longitude")

R5map <- ABThetasR5[,cols]
R5map$round <- 5

#save(R5map, file=paste0(dataPath,"UgandaR5mapdat.Rdata"))


#####################
## plotting
#####################
### THETA 1

#########################
### Plotting: By Ethnic Group
########################

## THETA 1
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta1,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Uganda, Round 5 (2012), By Ethnic Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$groupname)))+
  theme_bw() + theme(legend.position = 'none') +
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r5-theta1_group_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Uganda, Round 5 (2012), By Ethnic Group') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$groupname)))+
  theme_bw() + theme(legend.position = 'none') +
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r5-theta2_group_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Uganda, Round 5 (2012), By Ethnic Group') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$groupname)))+
  theme_bw() + theme(legend.position = 'none')+ 
  scale_fill_grey(start = 0, end = .9)


ggsave('Uganda_r5-theta3_group_mean_posterior.png',
       width = 10, height=5)


#########################
### Plotting: By Region
########################

## THETA 1
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta1,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 1',
          subtitle='Uganda, Round 5 (2012), By Region') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$wikiregion)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)


ggsave('Uganda_r5-theta1_region_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta2,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 2',
          subtitle='Uganda, Round 5 (2012), By Region') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$wikiregion)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)


ggsave('Uganda_r5-theta2_region_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta3,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 3',
          subtitle='Uganda, Round 5 (2012), By Region') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  scale_fill_grey(start = 0, end = .9)


ggsave('Uganda_r5-theta3_region_mean_posterior.png',
       width = 10, height=5)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Template code for using an Urban-Rural facet wrap to drill-down
## Urban-Rural
## Codes for urban or rural sampling unit
## 1=urban, 2=rural
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Recode for ease of interpretation:

ABThetasR5[which(ABThetasR5$urbrur==1), "urbrur"] <- "urban"
ABThetasR5[which(ABThetasR5$urbrur==2), "urbrur"] <- "rural"


ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta1,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R5-theta1_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 2
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta2,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R5-theta2_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 3
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta3,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R5-theta3_urbrur_mean_posterior.png',
       width = 10, height=5)

