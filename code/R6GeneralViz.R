## Takes an array of THETAS estimated from Interventions IRT Model
## Produces a visualization of the distribution of the thetas.

## Goal to make it general for any country
##

## Prepare space:
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
           "latex2exp", "reshape2", "ggridges",
           "rlang")

loadPkg(packs)

####################
## Load Data
###################

setwd("~/Dropbox/IRTM-Harvester/code") ## customize to project directory

dataPath <- "./Code_All_Countries/"## THETAS
thetasPath  <-"./Results/"
abdata <- "~/Dropbox/Afrobarometer-data/"

## small helper df of ethnic groups and their AB codes
load(paste0(abdata, "dfWithABEthnicGroupsR6.Rdata")) ## Loads a df with names

load(paste0(dataPath, paste0(country, country, "_R6_direct_THETA.RData"))) ## Thetas

load(paste0(abdata, country, "R6RawAB.Rds")) ## made by breakdownABRounds.R

###########################
## Round 6
###########################

ABRound <- tmp ## name change for more modular code

dim(ABRound)## 
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


##------Ethnic Group Information
##------------------------------
### Add premade key of  names of ethnic groups and regions

### Add in names of ethnic groups and regions
## Ethnic group question is q79 in the merged R6 data

egv <-  "q87"

AB.ethnic.groups$"ethnicGroupsR6" <- as.numeric(
  AB.ethnic.groups$"ethnicGroupsR6")

df <- unique(mergedABThetas[,egv]) ## what enthic groups are in the data

comp <- AB.ethnic.groups$"ethnicGroupsR6" ## from the master list

setdiff(df, comp) ## Adjust so that anything not in the list
## gets recoded for Missing Value in the key:


colnames(AB.ethnic.groups)

ABThetasR6 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR6")

ABThetasR6$V2 <- as.factor(ABThetasR6$V2)

dim(mergedABThetas)
dim(ABThetasR6)

## ----Code if want to customize ethnic group
## If uncommented, saves a data file with the 
## enthic groups for each country:

##egsummary <-as.data.frame(table(ABThetasR6$V2))
##colnames(egsummary) <- c("groupname", "R6Freq")
##save(egsummary, file=paste0(country,"R6egsummary.Rdata"))

## ---- Write in regions if needed:
## ------
## This is where you'd uncomment and load location key datasets

## ##load(paste0(country"RegionLabels.RData")
## Loads a table that maps AB codes onto the
## politically-salient regions/region names
## (Not always the same as the AB )


##ABThetasR6 <- merge(x=ABThetasR6,
##                    y=countryRL,
##                    by.x="region",
##                    by.y="regionlabels")

##dim(ABThetasR6) ## 2397 x 365

### Write out the region summary table:
## regsummary <-as.data.frame(table(ABThetasR6$regionnames))
## colnames(regsummary) <- c("regionname", "R6freq")
## save(regsummary, file=paste0(country, "R6regionsummary.Rdata"))

##----------
## Save gelocated thetas
## (For modeling)

writeout <- c("dateintr", "latitude", "longitude",
              "urbrur", 
              "Theta1", "Theta2", "Theta3")

geoThetas <- ABThetasR6[,writeout]
geoThetas$country <- country
geoThetas$round <- 6

save(geoThetas,
     file=paste0(thetasPath,
                 country, "R6geolocatedThetas.Rds" ))


############ Format for plotting

colnames(ABThetasR6)[colnames(ABThetasR6)=="V2"] <- "groupname"
ABThetasR6$groupname <- as.factor(ABThetasR6$groupname)

table(ABThetasR6$groupname)
table(ABThetasR6$urbrur)

##########################      
## If uncommented, writes location sub-data to map                                                                                                                          
##
## cols <- c("Theta1", "Theta2" , "Theta3" ,
##          "groupname",  "regionnames","urbrur",
##          "latitude", "longitude")

##R6map <- ABThetasR6[,cols]
## R6map$round <- 4
## save(R6map, file=paste0(country, "R6mapdat.Rdata"))


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
          subtitle=paste0(country, "Round 6 (2014-2015), By Ethnic Group")) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country, "_R6-theta1_group_mean_posterior.png"),
              width = 10, height=5)

## THETA 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle=paste0(country, " Round 6 (2014-2015), By Ethnic Group")) + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country, "_R6-theta2_group_mean_posterior.png"),
              width = 10, height=5)

## THETA 3

ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle=paste0(country, "Round 6 (2014-2015), By Ethnic Group")) + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$groupname)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country, "-theta3_group_mean_posterior.png"),
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
                          y=urbrur, 
                          fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle=paste0(country, ', Round 6 (2014-2015), By Urban/Rural')) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$regionnames)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country,"_R6-theta1_urbrur_mean_posterior.png"),
       width = 10, height=5)

## Theta 2
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta2,
                          y=urbrur, 
                          fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle=paste0(country, ', Round 6 (2014-2015), By Urban/Rural')) + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date() )) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$regionnames)))+
  theme_bw() + theme(legend.position = 'none')


ggsave(paste0(country,"_R6-theta2_urbrur_mean_posterior.png"),
       width = 10, height=5)


## Theta 3
ggplot(ABThetasR6) + 
  geom_density_ridges(aes(x=Theta3,
                          y=urbrur, 
                          fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle=paste0(country, ', Round 6 (2014-2015), By Urban/Rural')) + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR6$regionnames)))+
  theme_bw() + theme(legend.position = 'none')


ggsave(paste0(country,"_R6-theta3_urbrur_mean_posterior.png"),
       width = 10, height=5)
