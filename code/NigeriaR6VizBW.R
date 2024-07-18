## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Tailored to Nigeria Round 6 (based on Nigeria script)
## Specific to THETA NxKxS array and Afrobarometer demographic data
## Prepare space:## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

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

load(paste0(abdata, "dfWithABEthnicGroupsR6.Rdata")) ## Loads a 466 x 2 df with names

regions <- read.csv(paste0(abdata, "Data/NigeriaRegionsKey.csv")) ## loads a small csv with names of
## Nigeria's regions and their AB codes for each of the AB rounds

load(paste0(dataPath, "NigeriaNigeria_R6_direct_THETA.RData")) ## Thetas

load(paste0(abdata, "nigeriaR6RawAB.Rdata")) ## made by breakdownABRounds.R

###########################
## ROUND 6
###########################

ABRound <- nigeriaR6
rm(nigeriaR6)

dim(ABRound)## 2400 x 260
dim(THETA) ## 2406 

## Append THETAS to the survey data:
anchors <- 1:6 ## anchor entries
meanThetas<- apply(THETA, c(1,2), mean) ## take mean
meanThetas <- meanThetas[-anchors,] ## remove anchors
dim(meanThetas)[1] == dim(ABRound)[1] ## verify same length

### Merge data:

mergedABThetas <- cbind(ABRound, meanThetas)
colnames(mergedABThetas) <- c(colnames(ABRound),
                              c("Theta1", "Theta2", "Theta3"))


#################
## Ethnic Groups
## Merged R6, ethnic group question is q87

egv <- "q87"

colnames(mergedABThetas)

table(mergedABThetas[,egv])

colnames(AB.ethnic.groups)
AB.ethnic.groups$ethnicGroupsR6 <- as.numeric(
  AB.ethnic.groups$ethnicGroupsR6)

ABThetasR6 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR6")

dim(ABThetasR6)
colnames(ABThetasR6)

### Write out the summary dataframe of number of ethnic groups
##  So that you can merge them all together

## Code for ethnic groups summary
egsummary <-as.data.frame(table(ABThetasR6$V2))
colnames(egsummary) <- c("groupname", "R6Freq")
##save(egsummary, file="NigeriaR6egsummary.Rdata")

## not using all.x=TRUE, because that gets four rows with an ethnic group
## code of "17", which is not a code that the AB used, so seems like a transcription error or junk data.
## Easier to remove it now than to excise it later

ABThetasR6$V2 <- as.factor(ABThetasR6$V2)

dim(ABThetasR6) ##2397 x 364

ABThetasR6 <- merge(x=ABThetasR6,
                    y=regions,
                    by.x="region",
                    by.y="Code_R5", ## region code seems to be stabilized
                    all.x=TRUE)

colnames(ABThetasR6)

dim(ABThetasR6) ## 2397 x 370


#######
colnames(ABThetasR6)[colnames(ABThetasR6)=="V2"] <- "groupname"
ABThetasR6$groupname <- as.factor(ABThetasR6$groupname)

#########################
## Write out geolocated Thetas
## (commented for replication)

writeout <- c("dateintr", "latitude", "longitude", "urbrur",
              "ABName", "Zones", "Divide", 
              "Theta1", "Theta2", "Theta3")

geoThetasR6 <- ABThetasR6[,writeout]
geoThetasR6$country <- "Nigeria"
geoThetasR6$round <- 6

head(geoThetasR6$dateintr) ## Character in format day-abrev month- yy

geoThetasR6$dateintr <- as.Date(geoThetasR6$dateintr, format = "%d-%b-%y")

#save(geoThetasR6,
#    file=paste0(dataPath,
#                 "NigeriaR6geolocatedThetas.Rdata" ))


##########################                                                                                                                                 
## Write location sub-data to map                                                                                                                          
writeout <- c("dateintr", "latitude", "longitude", "urbrur",
              "ABName", "Zones", "Divide", "groupname",
              "Theta1", "Theta2", "Theta3")

R6map <- ABThetasR6[,writeout]
R6map$round <- 6

#save(R6map,
#     file=paste0(dataPath,"NigeriaR6mapdat.Rdata"))


#####################
## plotting
#####################
### THETA 1

#########################
### Plotting: By Ethnic Group
########################

## THETA 1
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta1,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Nigeria, Round 6 (2014), By Ethnic Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r6-theta1_group_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Nigeria, Round 6 (2014), By Ethnic Group') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r6-theta2_group_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Nigeria, Round 6 (2014), By Ethnic Group') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r6-theta3_group_mean_posterior.png',
       width = 10, height=5)


#########################
### Plotting: By Region
########################

## THETA 1
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta1,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 1',
          subtitle='Nigeria, Round 6 (2014), By Region') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free")+
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r6-theta1_region_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 2',
          subtitle='Nigeria, Round 6 (2014), By Region') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free")+
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r6-theta2_region_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 3',
          subtitle='Nigeria, Round 6 (2014), By Region') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free")+
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r6-theta3_region_mean_posterior.png',
       width = 10, height=5)


##%%%%%%%%%%%%%%%
## By North-South
##%%%%%%%%%%%%%%%

## North/South Theta 1 (Satisfaction with Status Quo)
ggplot(ABThetasR6) +
  geom_density_ridges(aes(x=Theta1,
                          y=Divide,
                          fill=Divide), color='white') +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 1',
          subtitle="Nigeria, Round 6, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR6$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R6-theta1_north-south_mean_posterior.png',
       width = 10, height=5)


ggplot(ABThetasR6) +
  geom_density_ridges(aes(x=Theta2,
                          y=Divide,
                          fill=Divide), color='white') +
  ## scale_fill_brewer(palette='Spectral') +
  ## scale_y_discrete(labels=unique(ABThetasR6$V2)) +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 2',
          subtitle="Nigeria, Round 6, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR6$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R6-theta2_north-south_mean_posterior.png',
       width = 10, height=5)


## Theta 3 by north-south:

ggplot(ABThetasR6) +
  geom_density_ridges(aes(x=Theta3,
                          y=Divide,
                          fill=Divide), color='white') +
  ## scale_fill_brewer(palette='Spectral') +
  ## scale_y_discrete(labels=unique(ABThetasR6$V2)) +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 3',
          subtitle="Nigeria, Round 6, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR6$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R6-theta3_north-south_mean_posterior.png',
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
                          y=Divide, 
                          fill=Divide), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Nigeria, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$Divide)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R6-theta1_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=Divide, 
                          fill=Divide), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Nigeria, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$Divide)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R6-theta2_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 3
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=Divide, 
                          fill=Divide), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Nigeria, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$Divide)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_R6-theta3_urbrur_mean_posterior.png',
       width = 10, height=5)
