## This script connects data processed through IRT-M and R
## into an SQL databse.
## It:
## 0) Makes a connection to a local PostgreSQL database called "harvesterab"
##  connecting as the system user
## 1) Ingests .Rdata objects with Afrobarometer survey and
## Geolocated Thetas for each country - round.
## 2) Creates a primary key for the row, based on row number and country-round
## (b/c structure of the dfs is that each row of survey and Thetas object for a given 
## country-round are assumed to be the same survey respondent)
## 3) Writes the data table to the SQL server


rm(list=ls())

packs <- c("DBI", ## Database interface package
           "odbc", ## works with dbi to connect to odbc databases
           "dplyr", ## dbplyr connects with dplyr to interface with databases
           "dbplyr",
           "RPostgreSQL",  
           "getPass") ## works like readline(),but masks input


loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib,
                      repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}


loadPkg(packs)

## DB Connection:
## configured to connect to a locally-hosted 
## PostgreSQL database, called "harvesterab"

drv <- dbDriver("PostgreSQL")

port = 5433 #Or the port you use!
db = "harvesterab"

## The following connection is configured for the system user, so don't need DB pw.

## But if I did, we'd want to make sure that we didn't store them in plain text:

#username = readline(prompt = "Enter username: ")
#password = rstudioapi::askForPassword("Database password")


con <- dbConnect(drv, 
                 host = "localhost", 
                 port = port, 
                 dbname = db, 
                 user = username) 
#password = password)

con ## PostgreSQL connection

dbListTables(con) ## so far nothing in the connection

##%%%%%%%%%%%%%%%%%
##Taking pre-convereted data over:
## Writing Nigeria to SQL:

setwd("~/Dropbox/IRTM-Harvester/code") ## customize to project directory

dataPath <- "./Code_All_Countries/"## THETAS

thetasPath  <-"./Results/"
abdata <- "../Afrobarometer-data/"

## Function to create a primary key
##  (from round and country)
## and write data to a preexisting SQL connection

write_df_to_sql <- function(round, 
                            country, 
                            dataobj, 
                            table_name){
  
  ## Data object needs to be a dataframe:
  ifelse(is.data.frame(dataobj) ==FALSE, 
         return(print("data object should be a dataframe!")),
         print("data object is a df!"))
  
  pkey = paste0(country, "_", 
              round,"_",
              1:nrow(dataobj))
  dataobj[,c('pkey')] <- pkey
##
  DBI::dbWriteTable(value = dataobj,
                  conn= con,
                  name= table_name)

  return_message <- paste0("DB tables are now: ",
                           dbListTables(con))
return(return_message)
}

## Nigeria R4:
## Nigeria Thetas R4, Survey R4


load(paste0(dataPath, "NigeriaR4geolocatedThetas.Rdata")) ## Thetas x AB
load(paste0(abdata, "nigeriaR4RawAB.Rdata")) ## made by breakdownABRounds.R

write_df_to_sql(dataobj=nigeriaR4, 
                round=4,
                country="nigeria", 
                table_name="nigeria_ab_r4")

rm(nigeriaR4)

write_df_to_sql(dataobj=geoThetasR4, 
                round=4,
                country="nigeria", 
                table_name="nigeria_gt_r4")

rm(geoThetasR4)

## Nigeria 
## Nigeria Thetas R5, Survey R5


write_df_to_sql(dataobj=nigeriaR5, 
                round=5,
                country="nigeria", 
                table_name="nigeria_ab_r5")

rm(nigeriaR5)

write_df_to_sql(dataobj=geoThetasR5, 
                round=5,
                country="nigeria", 
                table_name="nigeria_gt_r5")

rm(geoThetasR5)

## Nigeria R6:
## Nigeria Thetas R5, Survey R5

#load(paste0(dataPath, "NigeriaR6geolocatedThetas.Rdata")) ## Thetas x AB
#load(paste0(abdata, "nigeriaR6RawAB.Rdata")) ## made by breakdownABRounds.R


write_df_to_sql(dataobj=nigeriaR6, 
                round=6,
                country="nigeria", 
                table_name="nigeria_ab_r6")

rm(nigeriaR6)

write_df_to_sql(dataobj=geoThetasR6, 
                round=6,
                country="nigeria", 
                table_name="nigeria_gt_r6")

rm(geoThetasR6)

## Uganda

## Uganda 
## Uganda Thetas R4, Survey R4

load(paste0(dataPath, "UgandaR4geolocatedThetas.Rdata")) ## Thetas x AB
load(paste0(abdata, "ugandaR4RawAB.Rdata")) ## made by breakdownABRounds.R

write_df_to_sql(dataobj=ugandaR4, 
                round=4,
                country="Uganda", 
                table_name="uganda_ab_r4")

rm(ugandaR4)

write_df_to_sql(dataobj=geoThetasR4, 
                round=4,
                country="Uganda", 
                table_name="uganda_gt_r4")

rm(geoThetasR4)

## Uganda 
## Uganda Thetas R5, Survey R5

load(paste0(dataPath, "UgandaR5geolocatedThetas.Rdata")) ## Thetas x AB
load(paste0(abdata, "ugandaR5RawAB.Rdata")) ## made by breakdownABRounds.R

write_df_to_sql(dataobj=ugandaR5, 
                round=5,
                country="Uganda", 
                table_name="uganda_ab_r5")

rm(ugandaR5)

write_df_to_sql(dataobj=geoThetasR5, 
                round=5,
                country="Uganda", 
                table_name="uganda_gt_r5")

rm(geoThetasR5)


## Uganda 
## Uganda Thetas R6, Survey R6

load(paste0(dataPath, "UgandaR6geolocatedThetas.Rdata")) ## Thetas x AB
load(paste0(abdata, "ugandaR6RawAB.Rdata")) ## made by breakdownABRounds.R

write_df_to_sql(dataobj=ugandaR6, 
                round=6,
                country="Uganda", 
                table_name="uganda_ab_r6")

rm(ugandaR6)

write_df_to_sql(dataobj=geoThetasR6, 
                round=6,
                country="Uganda", 
                table_name="uganda_gt_r6")

rm(geoThetasR6)

##%%%%%%%%%%%%%%%
## Write Lambdas to SQL table
##%%%%%%%%%%%%%%%%

## Note that this has question ID, but not text
## will think about whether to pull that out of the data too

load(paste0(thetasPath, "T1_LambdaLoadings.Rds")) ## Thetas x AB

DBI::dbWriteTable(value = lambda_t1,
                  conn= con,
                  name= "ab_theta1_loadings")


dbListTables(con)

dbDisconnect(con) ## This is important!!