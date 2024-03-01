## Script takes Lambda and M-matrix coding object for 
## IRTM analysis of Afrobarometer country rounds
## Produces a dataframe with questions & their lambda loadings & variances


## load one sample lambda:
rm(list=ls())

dataPath <- "/Users/Promachos/Dropbox (Personal)/IRTM-Harvester/code/Code_All_Countries/"
savePath <- "/Users/Promachos/Dropbox (Personal)/IRTM-Harvester/code/Results/"

   
lambda_loadings <- function(country, round, theta){
  library(dplyr)

  ## Load data
  load(paste0(dataPath, country, country, "_", round, "_direct_Tagged_M.Rds"))
  load(paste0(dataPath,  country, country, "_", round, "_direct_LAMBDA.RData"))

  #### Take simulation average of the Lambdas
  ## Only columns that start with "V" 
  
  theta_one <- as.data.frame(LAMBDA[,theta,]) %>%
    rowwise() %>%
    mutate(mean = mean(c_across(starts_with("V")), na.rm = TRUE),
           var = var(c_across(starts_with("V")), na.rm = TRUE)) %>%
    ungroup()

  t_one <- as.data.frame(cbind(question= as.character(Mdata$question), 
                  mean= round(as.numeric(theta_one$mean),3),
                   variance= round(as.numeric(theta_one$var),3),
               country=country,
               round=round)) ## round for readability
  ## Sort in descending:
  t_one <- t_one %>%
    arrange(desc(mean))

  return(t_one)
}


## In Algeria R5, the question that most heavily loads on Theta 1 is 65:
## Which is: "How well or badly would you say the current government is handling the following matters..."
t_one_AlgeriaR5 <- lambda_loadings(country="Algeria", round="R5", theta= 1)

## Top loading: 7.1.2: What direction is the country going in (right direction?)
## 65: How well or badly is the government handling...
## 59.1.3 How much do you trust [The president/ a lot]
t_one_UgandaR5 <- lambda_loadings(country="Uganda", round="R5", theta=1)


## Top loading: 63.n.1: a response that indicates ruling party is best at handling economic and social problems
## 51.1.3: "a lot of trust" in the President (51.1.3) or ruling party (51.6.3)
## 3.1.2: country is going in right direction
t_one_UgandaR6 <- lambda_loadings(country="Uganda", round="R6", theta=1)

print(t_one[1:5, c("question")])


### %%%%%%%%%%%%%
## For all countries:


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

###

lambda_t1 <- data.frame()

for (country in countries){
  print(country)
  dataPath <- "/Users/Promachos/Dropbox (Personal)/IRTM-Harvester/code/Code_All_Countries/"
  for (round in country_each_round[[country]]){
    load(paste0(dataPath, country, country, "_", round, "_direct_Tagged_M.Rds"))
    load(paste0(dataPath,  country, country, "_", round, "_direct_LAMBDA.RData"))
    tmp <- lambda_loadings(country=country, 
                                       round=round,
                                       theta=1)
  lambda_t1 <- rbind(lambda_t1, tmp)
  }
}

save(lambda_t1, file = paste0(savePath, "T1_LambdaLoadings.Rds"))
