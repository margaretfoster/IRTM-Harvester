## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Tailored to Nigeria Round 4 (based on Nigeria script)
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

load(paste0(abdata, "dfWithABEthnicGroupsR4.Rdata")) ## Loads a 466 x 2 df with names

regions <- read_csv(paste0(abdata, "Data/NigeriaRegionsKey.csv")) ## loads a small csv with names of
## Nigeria's regions and their AB codes for each of the AB rounds

load(paste0(dataPath, "NigeriaNigeria_R4_direct_THETA.RData")) ## Thetas

load(paste0(abdata, "nigeriaR4RawAB.Rdata")) ## made by breakdownABRounds.R

###########################
## ROUND 4
###########################

ABRound <- nigeriaR4

rm(nigeriaR4)

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


### Add in names of ethnic groups and regions
## Ethnic group question is q79 in the merged R4 data

egv <- "q79"

dim(AB.ethnic.groups) ## 426 x 2

r4EGroups <- unique(mergedABThetas[,egv]) ## 25

r4EGroups[!(r4EGroups %in% AB.ethnic.groups$ethnicGroups)]

df <- unique(mergedABThetas[,egv]) ## 25

comp <- AB.ethnic.groups$"ethnicGroupsR4"

unique(df)

setdiff(df, comp)

colnames(AB.ethnic.groups)

ABThetasR4 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR4")

dim(mergedABThetas)
dim(ABThetasR4)

## Code for ethnic groups summary

egsummary <-as.data.frame(table(ABThetasR4$V2))

colnames(egsummary) <- c("groupname", "R4Freq")
#save(egsummary, file="NigeriaR4egsummary.Rdata")

## not using all.x=TRUE, because that gets four rows with an ethnic group
## code of "17", which is not a code that the AB used, so seems like a transcription error or junk data. Easier to remove it now than to excise it later

ABThetasR4$V2 <- as.factor(ABThetasR4$V2)

dim(ABThetasR4) 

ABThetasR4 <- merge(x=ABThetasR4,
                    y=regions,
                    by.x="region",
                    by.y="Code_R4",
                    all.x=TRUE)

colnames(ABThetasR4)

dim(ABThetasR4) ## 2324 x 365

### Write out the region summary table:
##regsummary <-as.data.frame(table(ABThetasR4$ABName))
##colnames(regsummary) <- c("regionname", "R4freq")
##save(regsummary, file="nigeriar4regionsummary.Rdata")


############ Format for plotting

colnames(ABThetasR4)[colnames(ABThetasR4)=="V2"] <- "groupname"

##########################                                                                                                                                 
## Write location sub-data to map                                                                                                                          
cols <- c("Theta1", "Theta2" , "Theta3" , "urbrur",
          "groupname",  "ABName",
          "latitude", "longitude", "Divide")

R4map <- ABThetasR4[,cols]
R4map$round <- 4
##save(R4map, file=paste0(dataPath,"NigeriaR4mapdat.Rdata"))

##########################
## write out geolocated Thetas
#########################

writeout <- c("dateintr", "latitude", "longitude", "urbrur",
              "ABName", "Zones", "Divide",
              "Theta1", "Theta2", "Theta3")

geoThetasR4 <- ABThetasR4[,writeout]
geoThetasR4$country <- "Nigeria"
geoThetasR4$round <- 4

#save(geoThetasR4,
#     file=paste0(dataPath,
#                 "NigeriaR4geolocatedThetas.Rdata" ))

#####################
## plotting
#####################
## Cleanup:

ABThetasR4$ABName <- as.factor(ABThetasR4$ABName)

#########################
### Plotting: By Ethnic Group
########################

## THETA 1
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta1,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Nigeria, Round 4 (2008), By Ethnic Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta1_group_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Nigeria, Round 4 (2008), By Ethnic Group') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta2_group_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Nigeria, Round 4 (2008), By Ethnic Group') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta3_group_mean_posterior.png',
       width = 10, height=5)


#########################
### Plotting: By Region
########################

## THETA 1

ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta1,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 1',
          subtitle='Nigeria, Round 4 (2008), By Region') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free") +
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta1_region_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 2',
          subtitle='Nigeria, Round 4 (2008), By Region') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free") +
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta2_region_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=ABName, 
                          fill=ABName), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Mean Theta 3',
          subtitle='Nigeria, Round 4 (2008), By Region') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  theme_bw() + theme(legend.position = 'none')+
  facet_wrap(~Divide, scales="free") +
  scale_y_discrete(limits=rev)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta3_region_mean_posterior.png',
       width = 10, height=5)


##%%%%%%%%%%%%%%%
## By North-South
##%%%%%%%%%%%%%%%

## North/South Theta 1 (Satisfaction with Status Quo)
ggplot(ABThetasR4) +
  geom_density_ridges(aes(x=Theta1,
                          y=Divide,
                          fill=Divide), color='white') +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 1',
          subtitle="Nigeria, Round 4, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR4$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta1_north-south_mean_posterior.png',
       width = 10, height=5)


ggplot(ABThetasR4) +
  geom_density_ridges(aes(x=Theta2,
                          y=Divide,
                          fill=Divide), color='white') +
  ## scale_fill_brewer(palette='Spectral') +
  ## scale_y_discrete(labels=unique(ABThetasR6$V2)) +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 2',
          subtitle="Nigeria, Round 4, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR4$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta2_north-south_mean_posterior.png',
       width = 10, height=5)


## Theta 3 by north-south:

ggplot(ABThetasR4) +
  geom_density_ridges(aes(x=Theta3,
                          y=Divide,
                          fill=Divide), color='white') +
  ## scale_fill_brewer(palette='Spectral') +
  ## scale_y_discrete(labels=unique(ABThetasR6$V2)) +
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution Mean Theta 3',
          subtitle="Nigeria, Round 4, By North - South Divide") +
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(as.factor(ABThetasR4$Divide))))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Nigeria_r4-theta3_north-south_mean_posterior.png',
       width = 10, height=5)

