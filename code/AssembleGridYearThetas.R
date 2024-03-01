## This script to associate PRIO Grid ID with geolocated Thetas

## Iterate through the list of country-rounds
## (From Master_file)

rm(list=ls())

setwd("~/Dropbox/IRTM-Harvester/code/Code_All_Countries/")
thetasPath  <-"../Results/"

## Helper function to convert long/lat to PRIO grids
source("../coordsToPrioGrid.R")

## master list:
## Comment for whichever set of countries wanted

countries <- c('Algeria', 'Botswana', 
               'BurkinaFaso','Burundi','Cameroon',
               'CapeVerde', 'CoteDIvoire', 'Egypt', 'Gabon', 'Ghana',
               'Guinea', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar',
               'Malawi', 'Mauritius', 'Morocco', 'Mozambique', 'Namibia',
               'Niger', 'Nigeria','Sao_Tome_and_Principe', 'Senegal',
               'SierraLeone', 'South_Africa', 'Sudan', 'Swaziland',
               'Tanzania', 'Togo', 'Tunisia', 'Uganda', 'Zambia', 'Zimbabwe')

## ID the AB survey rounds for each country:

country_each_round <- list(
  'Algeria'               = c(                  'R5', 'R6'),
  'Botswana'              = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Burkina Faso'           = c(            'R4',       'R6'),
  'Burundi'               = c(                        'R6'),
  'Cameroon'              = c(                        'R6'),
  'Cape Verde'             = c('R2',             'R5', 'R6'),
  'CoteDIvoire'           = c(                        'R6'),
  'Egypt'                 = c(                  'R5', 'R6'),
  'Gabon'                 = c(                        'R6'),
  'Ghana'                 = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Guinea'                = c(                        'R6'),
  'Kenya'                 = c('R2', 'R4','R5', 'R6'),
  'Lesotho'               = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Liberia'               = c(            'R4', 'R5', 'R6'),
  'Madagascar'            = c(            'R4',       'R6'),
  'Malawi'                = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Mauritius'             = c(                  'R5', 'R6'),
  'Morocco'               = c(                  'R5', 'R6'),
  'Mozambique'            = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Namibia'               = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Niger'                 = c(                        'R6'),
  'Nigeria'               = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Sao_Tome_and_Principe' = c(                        'R6'),
  'Senegal'               = c(            'R4',       'R6'),
  'SierraLeone'           = c(                  'R5', 'R6'),
  'South_Africa'          = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Sudan'                 = c(                  'R5', 'R6'),
  'Swaziland'             = c(                  'R5', 'R6'),
  'Tanzania'              = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Togo'                  = c(                        'R6'),
  'Tunisia'               = c(                  'R5', 'R6'),
  'Uganda'                = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Zambia'                = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'Zimbabwe'              = c('R2', 'R3', 'R4', 'R5', 'R6'))

focus <-   c("R4", "R5", "R6") ## Identify desired rounds
all_df <-  data.frame() ## Initialize empty dataframe



set.seed(30923)

for (country in countries){
  for (r in country_each_round[[country]]){
    if(r %in% focus){
      ## Load the geolocated thetas for the round
      ## convert to PID
      ## take the average for a year-PID
      ## write to a large dataframe
        
      load(paste0(thetasPath, ## loads the GeoThetas
                 country, r, "geolocatedThetas.Rds" ))

      thetasGrid <- label_grid(geoThetas) ## calls a helper-function
  
      thetasGrid$int.year <- as.numeric(format(
      as.Date(thetasGrid$dateintr,
              format="%d-%b-%y"),
      "%Y"))
    
      thetasGrid$round <- r
      thetasGrid$country <- country
    
      all_df <- rbind(all_df, thetasGrid)

    print(paste0("Finished: ", country, " round ", r))
    }else{ ## end if
      print(paste0(country, " ", r, " is not covered"))
    } ## end else    
  }  ## end country_each_round loop
  } ## end country loop
  

gridyear_Thetas  <- all_df %>%
  group_by(int.year, grid, urbrur,  country) %>%
  summarise(T1.quant.25 = quantile(Theta1, .25),
            T1.mean = mean(Theta1),
            T1.quant.75 = quantile(Theta1, .75),
            T1.var=var(Theta1),
            T2.quant.25 = quantile(Theta2, .25),
            T2.mean =  mean(Theta2),
            T2.quant.75 = quantile(Theta2, .75),
            T2.var=var(Theta2),
            T3.quant.25 = quantile(Theta3, .25),
            T3.mean = mean(Theta3),
            T3.quant.75 = quantile(Theta3, .75),
            T3.var=var(Theta3),
            num.thetas = n())


summary(gridyear_Thetas$T1.var)

## Take the prio grid average for each country-round
## add to a large dataframe of year-PID-theta averages

save(all_df,
     file=paste0(thetasPath, "all-Thetas.Rds"))

save(gridyear_Thetas,
     file=paste0(thetasPath, "gridyear-Thetas-All.Rds"))

print("Thetas Attached to PRIO Grid")
