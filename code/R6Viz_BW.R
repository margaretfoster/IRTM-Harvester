## Script to produce the theta visualizations 
## For IRT-M Harvester 

## Customized for jus

rm(list= ls())
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
thetasPath  <-"../Results/"
abdata <- "../Afrobarometer-data/"

## small helper df of ethnic groups and their AB codes
load(paste0(abdata, "dfWithABEthnicGroupsR6.Rdata")) ## Loads a df with names

## Declare countries:

countries <-c("Nigeria", "Uganda", "Kenya")


image_data_prep <- function(country){
  
  load(paste0(dataPath, paste0(country, country, "_R6_direct_THETA.RData"))) ## Thetas
  load(paste0(abdata, country, "R6RawAB.Rds")) ## made by breakdownABRounds.R

###########################
## Round 6
###########################

  ABRound <- tmp ## name change for more modular code
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

  AB.ethnic.groups$"ethnicGroupsR6" <- as.numeric(AB.ethnic.groups$"ethnicGroupsR6")
  df <- unique(mergedABThetas[,egv]) ## what enthic groups are in the data
  comp <- AB.ethnic.groups$"ethnicGroupsR6" ## from the master list
  setdiff(df, comp) ## Adjust so that anything not in the list
## gets recoded for Missing Value in the key:
  
  ABThetasR6 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR6")
  
  ABThetasR6$V2 <- as.factor(ABThetasR6$V2)

## ----Code if want to customize ethnic group
## If uncommented, saves a data file with the 
## ethnic groups for each country:

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

  print(colnames(ABThetasR6))
  writeout <- c("dateintr", "latitude", "longitude",
              "urbrur", "V2",
              "Theta1", "Theta2", "Theta3")

  geoThetas <- ABThetasR6[,writeout]
  geoThetas$country <- country
  geoThetas$round <- 6

  return(geoThetas)
}

############ Format for plotting

## By ethnic group:

def_plotbw_EG <- function(geoThetas, theta, countryname){
  
  colnames(geoThetas)[colnames(geoThetas)=="V2"] <- "groupname"
  geoThetas$groupname <- as.factor(geoThetas$groupname)
  
  gg <- ggplot(geoThetas) + 
  geom_density_ridges(aes(x=get(theta),
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle(paste0('Posterior Distribution of Group Mean ',theta),
          subtitle=paste0(countryname, " Round 6 (2014-2015), By Ethnic Group")) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(geoThetas$groupname)))+
  theme_bw() + theme(legend.position = 'none') +
  scale_fill_grey(start = 0, end = .9)

  print(gg)
  
  ggsave(paste0(countryname, "_bw_R6-", theta, "_group_mean_posterior.png"),
              width = 10, height=5)

}


## Nigeria, R6 Theta 1 by Ethnic Group

geoThetasN <- image_data_prep(countries[1])

def_plotbw_EG(geoThetasN, "Theta1", countryname=countries[1])

## Uganda: Location and Ethnic Group

## Uganda, R6 Thetas by Ethnic Group
geoThetasU <- image_data_prep(countries[2])

def_plotbw_EG(geoThetasU, "Theta1", countryname = countries[2])

def_plotbw_EG(geoThetasU, "Theta2", countries[2])
def_plotbw_EG(geoThetasU, "Theta3", countries[2])
##
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Template code for using an Urban-Rural facet wrap to drill-down
## Urban-Rural
## Codes for urban or rural sampling unit
## 1=urban, 2=rural
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

