rm(list=ls())

## Customize for your machine:
setwd("/Users/Promachos/Dropbox (Personal)/IRTM-Harvester/code")

library(groundhog)

packs <- c("tidyverse", "ggmap",
           "ggplot2", "maps",
           #"rgdal",
           "viridis",
           'pscl', "MASS", "boot", ## for Zero-inflated negative binomial
           "lmtest", "sandwich", "broom") ## for clustered standard errors

groundhog.library(packs, "2023-09-30")

dataPath <- "./Data/"
thetasPath  <-"./Results/"

## loads Grids and Theta summaries
load(paste0(thetasPath, "gridyear-Thetas-All.Rds"))

## helper to convert long-lat to PRIO grid system:
source("coordsToPrioGrid.R") 

## Data:

acled <-  read.csv(paste0(dataPath, "acled_africa.csv"), sep=";") ## 334,188 -- has field for PRIO grid
##ged <- read.csv(paste0(dataPath, "GEDEvent_v22_1.csv")) ### 293,634; uses PRIO-Grid
##scad <- read.csv(paste0(dataPath, "SCAD2018Africa_Final.csv")) ## 17,644; needs PRIO-post-processing

## Merge in Thetas by PRIO grid:
grids <- read.csv(paste0(dataPath, "priogrid_v1_01.csv"), sep=";") ## PRIO Grids

## Acled:
## Largest dataset, but actors are combined, which creates more information about activities
## but harder to pair with a theory of non-government mobilization

## mapping:

## Interaction codes indicating
## State only: 
## 10 (sole military), 11 (military vs military), 17 (military vs civilians)

state <- c(10, 11, 17)

## Codes with military-organized rebel/militias
## 12 (military vs rebels); 13 (military vs political militia); 14 (military vs communal militia)
## 20 (sole rebel action); 22 (rebels vs rebels); 23 (rebels vs political militia); 24 (rebels vs communal militia)
## 27 (rebel vs civilians); 28 (rebel vs others); 30 (sole political militia); 33 (political militia vs political militia);
# #34 (political militia vs communal militia);  37 (political militia vs civilians); 38 political militia vs others);
## 40 (sole communal militia action); 44 (communal militia vs communal militia)
## 47 (communal militia vs civilians); 48 (communal militia vs other)

#18 
org_non_state <-  c(12, 13, 14,
                    20, 22, 23, 24, 27, 28, 
                    30, 33, 34, 37, 38, 
                    40, 44, 47, 48)

## Codes with non-government operations:

## protests/riots
## 15 (military vs rioters); 16 (military vs protesters); 25 (rebels vs rioters); 26 (rebels vs protesters)
## 35 (political militia vs rioters); 36 (political militia vs protesters); 45 (communal militia vs rioters);
## 46 communal militia vs protesters
## 50 (sole rioter action); 55 (rioters vs rioters); 56 (rioters vs protesters); 57 (rioters vs civilians)'
## 58 (rioters vs others); 60 (sole protester action); 66 (protester vs protesters); 68 (protester vs other)

## 16
non_org_non_state <- c(15, 16, 
                       25, 26, 
                       35, 36, 
                       45, 46,
                       50, 55, 
                       56, 57,
                       58, 60, 
                       66, 68)



## Add "Political Violence; Demonstrations" section to "Political violence'"

acled[which(acled$disorder_type=="Political violence; Demonstrations"),
      c("disorder_type")] <- "Political violence"

### Summarize events to the grid-interaction level:
acled_grids <- label_grid(acled) %>% ## attaches events to a PRIO grid
  dplyr::group_by(interaction, year, grid) %>% ## separate model for each "interaction" event
  dplyr::summarise(num_events=n()) 

## Remove the unneeded data:
rm(acled)
rm(grids)
rm(grid)


## Create an overarching grouping for interactions:
acled_grids$event_grouping <- "other"
acled_grids[which(acled_grids$interaction %in% state),"event_grouping"] <- "state"
acled_grids[which(acled_grids$interaction %in% org_non_state),"event_grouping"] <- "org_non_state"
acled_grids[which(acled_grids$interaction %in% non_org_non_state),"event_grouping"] <- "non_org_non_state"

table(acled_grids$event_grouping)

## merge in theta
acled_thetas <-merge(x=acled_grids, 
                     y=gridyear_Thetas,
                     by.x=c("grid", "year"), 
                     by.y= c("grid", "int.year"),
                     all=TRUE)

## Cells with thetas and no violence:

## Cells with no recorded violent or non-violent events
acled_thetas[is.na(acled_thetas$disorder_type), "disorder_type"]  <- "None" ## Type of event
acled_thetas[is.na(acled_thetas$num_events), "num_events"]  <- 0 ## number of events

## Interview rounds:
acled_thetas$int.round <- NA
acled_thetas[which(acled_thetas$year==2008), "int.round"] <- 4
acled_thetas[which(acled_thetas$year==2009), "int.round"] <- 4
acled_thetas[which(acled_thetas$year==2012), "int.round"] <- 5
acled_thetas[which(acled_thetas$year==2013), "int.round"] <- 5
acled_thetas[which(acled_thetas$year==2014), "int.round"] <- 6
acled_thetas[which(acled_thetas$year==2015), "int.round"] <- 6

## Crop to years around the interviews:
acled_thetas <- acled_thetas[which(acled_thetas$year > 2007 &
                                     acled_thetas$year < 2016),]

table(acled_thetas$year)

## Add in 0s for grid-years without violence or interviews:
acled_thetas_Lag = expand.grid(
  grid = sort(unique(acled_thetas$grid)), ## group for index
  int.round = seq(from = 4, ## Expand the dataframe so that there is an entry for each grid-round
                  to = 6)) %>% ## 
  ## merge the scaffold into the data: 
  left_join(acled_thetas, by = c("grid", "int.round")) %>%  ## Add location, event, and theta estimates
  arrange(grid, int.round) %>%
  group_by(grid)

## Add in 0s for interaction codes in grid with no events:
acled_thetas[is.na(acled_thetas$interaction), c("interaction")] <- 0

## 
acled_thetas_Lag <- acled_thetas_Lag %>%
  group_by(grid) %>% ## group by location 
  mutate( ## Create lags for previous value
    T1.q25.prevround = lag(T1.quant.25, order_by = int.round),
    T1.mean.prevround = lag(T1.mean,  order_by = int.round),
    T1.var.prevround = lag(T1.var,  order_by = int.round),
    T1.q75.prevround = lag(T1.quant.75, order_by = int.round),
    T2.q25.prevround = lag(T2.quant.25, order_by = int.round),
    T2.mean.prevround = lag(T2.mean, order_by = int.round),
    T2.q75.prevround = lag(T2.quant.75, order_by = int.round),
    T3.q25.prevround = lag(T3.quant.25, order_by = int.round),
    T3.mean.prevround = lag(T3.mean, order_by = int.round),
    T3.q75.prevround = lag(T3.quant.75, order_by = int.round),
  ) %>%  ## Also fill in location  metadata:
  fill(c(urbrur, country),
       .direction =c("updown"))

### Check the data for one grid:
## unit is year-interaction
## with number of events for each of the different 
## interaction codes

print(acled_thetas[which(acled_thetas$grid==80317),])


print(dim(acled_thetas)) ## 31288

save(acled_thetas, 
     file=paste0(dataPath, "acled_thetas.RData" ))
