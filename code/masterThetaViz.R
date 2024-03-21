## Script to churn through the Theta viz
## scripts

rm(list=ls())

setwd("~/Dropbox/IRTM-Harvester/code/") ## customize to project directory

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

set.seed(30923)

## For each country and for each round:
## Pass the country name into the appropriate visualization script:
## run the model on all countries and all rounds
## Note that there is a some hand-adjusting of the Afrobarometer 
## subsets for the countries with a space
for (country in countries){
  for (round in country_each_round[[country]]){
    
    ## declare variables to pass in:
    ## country name, round, ethnicity question, year

    print(paste0("Making Theta Plots for: ", country, " ", round))
  
    ## Passing R2 (2003-2004) and R3 (2005) for now:
    ## B/c ethnic group mapping not ready yet
    
    if(round=="R4"){
      source("R4GeneralViz.R")
    }
    
    if(round=="R5"){
    source("R5GeneralViz.R")
    }
    
    if(round=="R6"){
      source("R6GeneralViz.R")
    }
    else{print("Not a covered round")}
    print(paste0("Finished Round ", country," ", round, "!"))
  }
}
