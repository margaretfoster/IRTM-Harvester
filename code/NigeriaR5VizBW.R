## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Tailored to Nigeria Round 5 (based on Nigeria script)
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
           "rlang", "stargazer")

loadPkg(packs)

####################
## Load Data
###################

## Paths: 

setwd("~/Dropbox/IRTM-Harvester/code") ## customize to project directory

dataPath <- "./Code_All_Countries/"## THETAS

thetasPath  <-"../Results/"
abdata <- "../Afrobarometer-data/"

## Data Objects:

load(paste0(abdata, "dfWithABEthnicGroupsR5.Rdata")) ## Loads a 466 x 2 df with names

regions <- read_csv(paste0(abdata, "Data/NigeriaRegionsKey.csv")) ## loads a small csv with names of
## Nigeria's regions and their AB codes for each of the AB rounds

load(paste0(dataPath, "NigeriaNigeria_R5_direct_THETA.RData")) ## Thetas

load(paste0(abdata, "nigeriaR5RawAB.Rdata")) ## made by breakdownABRounds.R

###########################
## ROUND 5
###########################

ABRound <- nigeriaR5

rm(nigeriaR5)

dim(THETA)

summary(ABRound$dateintr) ## 2012-2013, but AB pages says R5 was 2,400 interviews 
## in 2013

dim(ABRound)## 2400
dim(THETA) ## 2406 

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
## Nigeria ethnic group question: 84

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
#     file="NigeriaR5egsummary.Rdata")

## not using all.x=TRUE, because that gets four rows with an ethnic group
## code of "17", which is not a code that the AB used, so seems like a transcription error or junk data. Easier to remove it now than to excise it later

ABThetasR5$V2 <- as.factor(ABThetasR5$V2)

dim(ABThetasR5) ##2397 x 364

## Standardize region name:

ABThetasR5 <- merge(x=ABThetasR5,
                    y=regions,
                    by.x="region",
                    by.y="Code_R5",
                    all.x=TRUE)

colnames(ABThetasR5)


##regsummary <-as.data.frame(table(ABThetasR5$ABName))
##colnames(regsummary) <- c("regionname", "R5freq")
##save(regsummary,
##     file="nigeriar5regionsummary.Rdata")

################################
## Write out geolocated Thetas
###############################

writeout <- c("dateintr", "latitude", "longitude",  "urbrur",
              "ABName", "Zones", "Divide",
              "Theta1", "Theta2", "Theta3")

geoThetasR5 <- ABThetasR5[,writeout]
geoThetasR5$country <- "Nigeria"
geoThetasR5$round <- 5

#save(geoThetasR5,
 #    file=paste0(dataPath,
#                 "NigeriaR5geolocatedThetas.Rdata" ))


################################
## Format for graphing aesthetics
##############################
colnames(ABThetasR5)[colnames(ABThetasR5)=="V2"] <- "groupname"

table(ABThetasR5$groupname)

##########################                                                                                                                                 
## Write location sub-data to map                                                                                                                          
cols <- c("dateintr", "latitude", "longitude",  "urbrur",
          "ABName", "Zones", "Divide",
          "Theta1", "Theta2", "Theta3",
          "groupname")

R5map <- ABThetasR5[,cols]
R5map$round <- 5

#save(R5map, file=paste0(dataPath,"NigeriaR5mapdat.Rdata"))

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
          subtitle='Nigeria, Round 5 (2012), By Ethnic Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r5-theta1_group_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Nigeria, Round 5 (2012), By Ethnic Group') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r5-theta2_group_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Nigeria, Round 5 (2012), By Ethnic Group') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR5$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r5-theta3_group_mean_posterior.png',
       width = 10, height=5)


#########################
### Plotting: By Region
########################

## THETA 1
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta1,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 1',
          subtitle='Nigeria, Round 5 (2012), By Region') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+ 
  facet_wrap(~Divide, scales="free")+
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)


ggsave('Nigeria_r5-theta1_region_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta2,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 2',
          subtitle='Nigeria, Round 5 (2012), By Region') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date()))+
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free")+
  scale_y_discrete(limits=rev)


ggsave('Nigeria_r5-theta2_region_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR5) + 
  geom_density_ridges(aes(x=Theta3,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 3',
          subtitle='Nigeria, Round 5 (2012), By Region') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free")+
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r5-theta3_region_mean_posterior.png',
       width = 10, height=5)

##%%%%%%%%%%%%%%%
## By North-South
##%%%%%%%%%%%%%%%

## North/South Theta 1 (Satisfaction with Status Quo)
ggplot(ABThetasR5) +
  geom_density_ridges(aes(x=Theta1,
                          y=Divide,
                          fill=Divide), color='white') +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 1',
          subtitle="Nigeria, Round 5, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR5$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R5-theta1_north-south_mean_posterior.png',
       width = 10, height=5)


ggplot(ABThetasR5) +
  geom_density_ridges(aes(x=Theta2,
                          y=Divide,
                          fill=Divide), color='white') +
  ## scale_fill_brewer(palette='Spectral') +
  ## scale_y_discrete(labels=unique(ABThetasR6$V2)) +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 2',
          subtitle="Nigeria, Round 5, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR5$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R5-theta2_north-south_mean_posterior.png',
       width = 10, height=5)


## Theta 3 by north-south:

ggplot(ABThetasR5) +
  geom_density_ridges(aes(x=Theta3,
                          y=Divide,
                          fill=Divide), color='white') +
  ## scale_fill_brewer(palette='Spectral') +
  ## scale_y_discrete(labels=unique(ABThetasR6$V2)) +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 3',
          subtitle="Nigeria, Round 5, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR5$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R5-theta3_north-south_mean_posterior.png',
       width = 10, height=5)
