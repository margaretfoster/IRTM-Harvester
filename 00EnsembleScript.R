###############################
## IRT-M AB and Event analysis
## for all countries
##############################


rm(list=ls())
## Declare paths:

## USERS: Update these paths for your computer!

setwd("~/Dropbox/IRTM-Harvester/") 

dataPath2 <- "./Afrobarometer-data/" ## raw data
irtmPath <- "./code/" ## for IRM code

## Groundhog code for the package libraries:
library(groundhog)

packs <- c("tidyverse", "ggmap",
           "ggplot2", "maps",
           #"rgdal",
           "viridis",
           'pscl', "MASS", "boot", ## for Zero-inflated negative binomial
           "lmtest", "sandwich", "broom") ## for clustered standard errors

groundhog_day <- "2023-09-01" ##

groundhog.library(packs, groundhog_day)


##%%%%%%%%%%%%%%%%%%%%%%
## Part One:
## Produce IRT-M Estimates
## For countries in AB Rounds 4, 5, 6
##%%%%%%%%%%%%%%%%%%%%%%

## Extract Countries of Interest from Raw AB Data:

setwd(dataPath2)

source("breakdownABRounds.R") ## Takes the raw AB data and creates country-round subsets

source("AB_EthnicGroupsR4.R") ##creates a small DF with ethnic group sorting
source("AB_EthnicGroupsR5.R") ##creates a small DF with ethnic group sorting
source("AB_EthnicGroupsR6.R") ##creates a small DF with ethnic group sorting

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Estimate country-level IRTM models:
setwd(irtmPath)
source("master_ensemble.R") 

source("LambdasDistribution.R")
## In: Lambda and M-matrices created by IRTM
## Out: Data frame of lambda loadings per specified theta 


## This script directs to run_[country]_R[round].R scripts
## To customize the countries that are running, comment out
## the "countries" line in master_file
##%%%%%%%%%%%%%%%%%%%%%%
## Part Two: Visualization and Analysis
##%%%%%%%%%%%%%%%%%%%%%%

## individual country plots:
source("masterThetaViz.R") ## generates visualizations for all countries
## by urban/rural; calls the R4, R5, R6 General Viz scripts
## out: R4:R6 plots for all selected countries; [Country]GeolocatedThetas.Rds

source("AssembleGridYearThetas.R") ## Attach thetas to PRIO Grid
## out: all_df.Rds (all theta estimates, unit is respondent)
## out: gridyear_Thetas.Rds: unit is PRIO-grid year, summary statistics of thetas

## Ensemble plot:
source("allViz.R")
## out: plot of all countries (y-axis) and distribution of theta(s)

source("allAnalysis.R") ## Analysis of correlations between Thetas and events

################
## Validation
###############

## Inspect Lambda loadings
## (Ensure that lambdas are loading on sensible questions)

source("LambdasDistribution.R")
##In: IRT-M output (Lambdas and M-matrix [from package] for each country)
##Out: T[N]_LambdaLoadings.Rds, a dataframe of country-AB round question loadings for 
## selected Theta dimension.

