##Temporary file to debug save/load issues

## Prepare space:
#######
## Load needed packages, install if needed:
#####

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
           "rlang", "stargazer")

loadPkg(packs)

####################
## Load Data
###################

country='Botswana'

abdata <- "~/Dropbox/Afrobarometer-data/"

dataPath <- "/Users/Promachos/Dropbox (Personal)/Research/Interventions/Theory-IRT/afrobarometer/Code_All_Countries/"## THETAS
thetasPath  <-"~/Dropbox/Research/Interventions/Theory-IRT/afrobarometer/Results/"
auxData <- "~/Dropbox/Research/Interventions/Theory-IRT/data-viz/Interventions-Country-Thetas/AB-All/"

load(paste0(auxData, "dfWithABEthnicGroupsR4.Rdata")) ## Loads a df with names
## of ethnic groups and their AB codes

load("/Users/Promachos/Dropbox (Personal)/Research/Interventions/Theory-IRT/afrobarometer/Code_All_Countries/Malawi/tstBTS_THETA.RData")
##load(paste0(dataPath, paste0(country, country, "_R4_direct_THETA.RData"))) ## Thetas

load(paste0(abdata, country, "R4RawAB.Rds")) ## made by breakdownABRounds.R ##

###########################
## ROUND 4
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
## Ethnic group question is q79 in the merged R4 data

egv <- "q79"

AB.ethnic.groups$"ethnicGroupsR4" <- as.numeric(
  AB.ethnic.groups$"ethnicGroupsR4")

df <- unique(mergedABThetas[,egv]) ## what enthic groups are in the data

comp <- AB.ethnic.groups$"ethnicGroupsR4" ## from the master list

setdiff(df, comp) ## Adjust so that anything not in the list
## gets recoded for Missing Value in the key:


colnames(AB.ethnic.groups)

ABThetasR4 <- merge(x=mergedABThetas,
                    y=AB.ethnic.groups,
                    by.x=egv,
                    by.y="ethnicGroupsR4")

ABThetasR4$V2 <- as.factor(ABThetasR4$V2)

dim(mergedABThetas)
dim(ABThetasR4)

## ----Code if want to customize ethnic group
## If uncommented, saves a data file with the 
## enthic groups for each country:

##egsummary <-as.data.frame(table(ABThetasR4$V2))
##colnames(egsummary) <- c("groupname", "R4Freq")
##save(egsummary, file=paste0(country,"R4egsummary.Rdata"))

## ---- Write in regions if needed:
## ------
## This is where you'd uncomment and load location key datasets

## ##load(paste0(country"RegionLabels.RData")
## Loads a table that maps AB codes onto the
## politically-salient regions/region names
## (Not always the same as the AB )


##ABThetasR4 <- merge(x=ABThetasR4,
##                    y=countryRL,
##                    by.x="region",
##                    by.y="regionlabels")

##dim(ABThetasR4) ## 2397 x 365

### Write out the region summary table:
## regsummary <-as.data.frame(table(ABThetasR4$regionnames))
## colnames(regsummary) <- c("regionname", "R4freq")
## save(regsummary, file=paste0(country, "R4regionsummary.Rdata"))

##----------
## Save gelocated thetas
## (For modeling)

writeout <- c("dateintr", "latitude", "longitude",
              "urbrur", 
              "Theta1", "Theta2", "Theta3")

geoThetasR4 <- ABThetasR4[,writeout]
geoThetasR4$country <- country
geoThetasR4$round <- 4

save(geoThetasR4,
     file=paste0(thetasPath,
                 country, "R4geolocatedThetas.Rdata" ))


############ Format for plotting

colnames(ABThetasR4)[colnames(ABThetasR4)=="V2"] <- "groupname"
ABThetasR4$groupname <- as.factor(ABThetasR4$groupname)

table(ABThetasR4$groupname)
table(ABThetasR4$urbrur)

##########################      
## If uncommented, writes location sub-data to map                                                                                                                          
##
## cols <- c("Theta1", "Theta2" , "Theta3" ,
##          "groupname",  "regionnames","urbrur",
##          "latitude", "longitude")

##R4map <- ABThetasR4[,cols]
## R4map$round <- 4
## save(R4map, file=paste0(country, "R4mapdat.Rdata"))


#####################
## plotting
#####################
### THETA 1

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
          subtitle=paste0(country," Round 4 (2008), By Ethnic Group")) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country, "_R4-theta1_group_mean_posterior.png"),
       width = 10, height=5)

## THETA 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle=paste0(country, " Round 4 (2008 -2009), By Ethnic Group")) + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country, "_R4-theta2_group_mean_posterior.png"),
       width = 10, height=5)

## THETA 3

ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=groupname, 
                          fill=groupname), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle=paste0(country, "Round 4 (2008), By Ethnic Group")) + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date())) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$groupname)))+
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

ABThetasR4[which(ABThetasR4$urbrur==1), "urbrur"] <- "urban"
ABThetasR4[which(ABThetasR4$urbrur==2), "urbrur"] <- "rural"


ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta1,
                          y=urbrur, 
                          fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 1',
          subtitle=paste0(country, ', Round 4 (2008), By Urban/Rural')) + 
  xlab(TeX('$\\theta_1$')) + ylab('')+
  labs(caption= print(Sys.Date() )) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$regionnames)))+
  theme_bw() + theme(legend.position = 'none')

ggsave(paste0(country,"_R4-theta1_urbrur_mean_posterior.png"),
       width = 10, height=5)

## Theta 2
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta2,
                          y=urbrur, 
                          fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 2',
          subtitle=paste0(country, ', Round 4 (2008), By Urban/Rural')) + 
  xlab(TeX('$\\theta_2$')) + ylab('')+
  labs(caption= print(Sys.Date() )) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$regionnames)))+
  theme_bw() + theme(legend.position = 'none')


ggsave(paste0(country,"_R4-theta2_urbrur_mean_posterior.png"),
       width = 10, height=5)


## Theta 3
ggplot(ABThetasR4) + 
  geom_density_ridges(aes(x=Theta3,
                          y=urbrur, 
                          fill=urbrur), color='white') + 
  geom_vline(xintercept=0,linetype='dotted', col = 'red')+
  ggtitle('Posterior Distribution of Group Mean Theta 3',
          subtitle=paste0(country, ', Round 4 (2008), By Urban/Rural')) + 
  xlab(TeX('$\\theta_3$')) + ylab('')+
  labs(caption= print(Sys.Date()  )) + 
  scale_y_discrete(limits = rev(levels(ABThetasR4$regionnames)))+
  theme_bw() + theme(legend.position = 'none')


ggsave(paste0(country,"_R4-theta3_urbrur_mean_posterior.png"),
       width = 10, height=5)