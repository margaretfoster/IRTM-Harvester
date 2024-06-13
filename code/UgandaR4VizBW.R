## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Tailored to Uganda Round 4 (based on Nigeria script)
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

setwd("~/Dropbox/IRTM-Harvester/code") ## customize to project directory

##dataPath <- "~/Dropbox/Research/Interventions/Theory-IRT/afrobarometer/Results/" ## THETAS

dataPath <- "./Code_All_Countries/"## THETAS

thetasPath  <-"../Results/"
abdata <- "../Afrobarometer-data/"

load(paste0(abdata, "dfWithABEthnicGroupsR4.Rdata")) ## Loads a 466 x 2 df with names
## of ethnic groups and their AB codes
load(paste0(abdata,"ugandaR4RawAB.Rdata")) ## made by breakdownABRounds.R
## of ethnic groups and their AB codes

ugandaRegions <-  read_excel(paste0(abdata, "Uganda_district_to_region.xlsx"))

###########################
## ROUND 4
###########################

load(paste0(dataPath, "UgandaUganda_R4_direct_THETA.RData")) ## Thetas

ABRound <- ugandaR4


dim(ABRound)## 2431 x 309 x 360
dim(THETA) ## 2437 x 3 x 100

## Amend THETAS to the survey data:
anchors <- 1:6 ## anchor entries
meanThetas<- apply(THETA, c(1,2), mean) ## take mean
meanThetas <- meanThetas[-anchors,] ## remove anchors
dim(meanThetas)[1] == dim(ABRound)[1] ## verify same length


### Merge data:

mergedABThetas <- cbind(ABRound, meanThetas)
colnames(mergedABThetas) <- c(colnames(ABRound),
                              c("Theta1", "Theta2", "Theta3"))


colnames(mergedABThetas) ##Doesn't show up in View, but the thetas are there
## Note now we have thetas & districts

### Add in names of ethnic groups and regions
## Ethnic group question is q79 in the merged R4 data

egv <- "q79"

dim(AB.ethnic.groups) ## 426 x 2

AB.ethnic.groups$"ethnicGroupsR4" <- as.numeric(
  AB.ethnic.groups$"ethnicGroupsR4")

df <- unique(mergedABThetas[,egv]) ## 25

comp <- AB.ethnic.groups$"ethnicGroupsR4"

unique(df)

setdiff(df, comp) ## no missing ethnic groups


ABThetasR4 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR4")

## Region:

## check that the district spellings are the same: 
## And the wikipedia list covers all AB districts
wikidistricts <- as.character(ugandaRegions$District)
abdistricts <- as.character(ABThetasR4$district)
setdiff(abdistricts, wikidistricts) ## set diff: in x but not y

ABThetasR4 <- merge(x=ABThetasR4,
                    y=ugandaRegions,
                    by.x="district",
                    by.y="District")

colnames(ABThetasR4)[which(colnames(ABThetasR4)=="Region")] <- "wikiregion"

table(ABThetasR4$wikiregion)

table(ABThetasR4$urbrur)

## Code for ethnic groups summary
egsummary <-as.data.frame(table(ABThetasR4$V2))

colnames(egsummary) <- c("groupname", "R4Freq")
#save(egsummary, file="UgandaR4egsummary.Rdata")

############################
## write out the geolocated thetas

writeout <- c("dateintr", "latitude", "longitude",
              "wikiregion",  "urbrur",
              "Theta1", "Theta2", "Theta3")

geoThetasR4 <- ABThetasR4[,writeout]
geoThetasR4$country <- "Uganda"
geoThetasR4$round <- 4                                                                                                                                        

#save(geoThetasR4,
#     file=paste0(dataPath,
#                 "UgandaR4geolocatedThetas.Rdata"))

## not using all.x=TRUE, because that gets four rows with an ethnic group
## code of "17", which is not a code that the AB used, so seems like a transcription error or junk data. Easier to remove it now than to excise it later

### Write out the region summary table:
regsummary <-as.data.frame(table(ABThetasR4$wikiregion))
colnames(regsummary) <- c("regionname", "R4freq")
#save(regsummary, file="ugandar4regionsummary.Rdata")


############ Format for plotting

colnames(ABThetasR4)

colnames(ABThetasR4)[colnames(ABThetasR4)=="V2"] <- "groupname"

ABThetasR4$wikiregion <- as.factor(ABThetasR4$wikiregion)
ABThetasR4$groupname <- as.factor(ABThetasR4$groupname)

table(ABThetasR4$groupname)
table(ABThetasR4$wikiregion)

##########################                                                                                                                                 
## Write location sub-data to map                                                                                                                          
cols <- c("Theta1", "Theta2" , "Theta3" ,
          "groupname",  "wikiregion", "urbrur",
          "latitude", "longitude")

R4map <- ABThetasR4[,cols]
R4map$round <- 4

#save(R4map, file=paste0(dataPath,"UgandaR4mapdat.Rdata"))


#####################
## plotting
#####################
### THETA 1

#########################
### Plotting: By Ethnic Group
########################

print(Sys.Date())

## THETA 1
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta1,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Uganda, Round 4 (2008), By Ethnic Group') +
  labs(caption= print(Sys.Date())) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r4-theta1_group_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Uganda, Round 4 (2008), By Ethnic Group') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r4-theta2_group_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Uganda, Round 4 (2008), By Ethnic Group') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r4-theta3_group_mean_posterior.png',
       width = 10, height=5)


#########################
### Plotting: By Region
########################

## THETA 1
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta1,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 1',
          subtitle='Uganda, Round 4 (2008), By Region') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$wikiregion)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r4-theta1_region_mean_posterior.png',
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 2',
          subtitle='Uganda, Round 4 (2008), By Region') + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$wikiregion)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r4-theta2_region_mean_posterior.png',
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Region Mean Theta 3',
          subtitle='Uganda, Round 4 (2008), By Region') + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$wikiregion)))+
  theme_bw() + theme(legend.position = 'none')+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_r4-theta3_region_mean_posterior.png',
       width = 10, height=5)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Template code for using an Urban-Rural facet wrap to drill-down
## Urban-Rural
## Codes for urban or rural sampling unit
## 1=urban, 2=rural
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Recode for ease of interpretation:

ABThetasR4[which(ABThetasR4$urbrur==1), "urbrur"] <- "urban"
ABThetasR4[which(ABThetasR4$urbrur==2), "urbrur"] <- "rural"


ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta1,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R4-theta1_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R4-theta2_urbrur_mean_posterior.png',
       width = 10, height=5)

## Theta 3
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=wikiregion, 
                          fill=wikiregion), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle='Uganda, Round 6 (2014), By Urban/Rural Group') + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$wikiregion)))+
  theme_bw() + theme(legend.position = 'none') +
  facet_wrap(~urbrur)+
  scale_fill_grey(start = 0, end = .9)

ggsave('Uganda_R4-theta3_urbrur_mean_posterior.png',
       width = 10, height=5)
