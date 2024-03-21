## This script batches code to run the IRTM package on each
## country-AB round pair that we have.

rm(list=ls())

setwd("~/Dropbox/Afrobarometer-data/")
getwd()

## master list:
## Comment for whichever set of countries wanted

countries <- c('Algeria', 'Botswana', 'BurkinaFaso','Burundi','Cameroon',
              'CapeVerde', 'CoteDIvoire', 'Egypt', 'Gabon', 'Ghana',
             'Guinea', 'Kenya', 'Lesotho', 'Liberia', 'Madagascar',
               'Malawi', 'Mauritius',
               'Morocco', 
               'Mozambique', 'Namibia',
               'Niger', 'Nigeria','Sao_Tome_and_Principe', 'Senegal',
               'SierraLeone', 'South_Africa', 'Sudan', 'Swaziland',
               'Tanzania', 'Togo', 'Tunisia', 'Uganda', 'Zambia', 'Zimbabwe')


## ID the AB survey rounds for each country:
country_each_round <- list(
  'Algeria'               = c(                  'R5', 'R6'),
  'Botswana'              = c('R2', 'R3', 'R4', 'R5', 'R6'),
  'BurkinaFaso'           = c(            'R4',       'R6'),
  'Burundi'               = c(                        'R6'),
  'Cameroon'              = c(                        'R6'),
  'CapeVerde'             = c('R2',             'R5', 'R6'),
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


## Path-to-code:


##country_each_round

set.seed(30923)

## run the model on all countries and all rounds
for (country in countries){
  for (round in country_each_round[[country]]){
    print(paste0("Running IRT-M for: ", country, " ", round))
    file_name <- paste0('run_', country, '_', round, '.R') ##this is what calls IRT-M for each country-round pair
    pathToCode <-"./"
    setwd(paste0(pathToCode,'Code_All_Countries/', country,'/'))
    path_name <- paste0(pathToCode,'Code_All_Countries/', country, '/', file_name)
    
    if (!file.exists(path_name)) {
     # print(path_name)
      print(getwd())
      print(country)
      print(round)
      stop('File does not exist!!!')
    }
    source(path_name)
    print(paste0("Finished Round ", country," ", round, "!"))
  }
}


