## This script to take data with lat-long coorinates
## and fit it to the PRIO grids

##rm(list=ls())

loadPkg=function(toLoad){
 	for(lib in toLoad){
            if(!(lib %in% installed.packages()[,1])){ 
                install.packages(lib, repos='http://cran.rstudio.com/') }
            suppressMessages( library(lib, character.only=TRUE) )
 	}
}

packs <- c("tidyverse", "ggmap", "dplyr")

loadPkg(packs)

codePath <- "~/Dropbox/Research/Interventions/Theory-IRT/mjf-merges/meltt/data-prep/"

##Load PRIO Grid csv file:
## In the same directory as this script
gids <- read.csv(paste0(codePath,"priogrid_v1_01.csv"),
                 sep=";")

print(paste0("Grids loaded, dimension is: ",dim(gids))) ## ~4M observations.

#################
## Begin Miranda's code:

grid <- gids %>%
  as_tibble() %>%
  dplyr::select(
    'gid',
    # centroid of longitude & latitude for each grid cell
    'xcoord', 
    'ycoord'
  ) %>%
  distinct() %>%
  mutate(
    min_long = xcoord - .25,
    max_long = xcoord + .25,
    min_lat  = ycoord - .25,
    max_lat  = ycoord + .25
  ) %>%
  dplyr::select(
    -'xcoord', -'ycoord'
  ) %>%
  rename(grid = gid)



half_round <- function(x) { ceiling(x * 2) / 2 }

label_grid <- function(df) {
  df %>% 
    mutate(
      max_long = half_round(longitude), ## round the lat-long
      max_lat  = half_round(latitude) ## to match PRIO
    ) %>%
    left_join(
      grid, ## merge in the PRIO grid
      by = c('max_long', 'max_lat')
    ) %>%
    dplyr::select(
      -min_lat, -min_long, -max_lat, -max_long
    )
} 

