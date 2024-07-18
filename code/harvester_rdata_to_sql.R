rm(list=ls())

## Customize for your machine:
setwd("/Users/Promachos/Dropbox (Personal)/IRTM-Harvester/code")



packs <- c("DBI", ## Database interface package
           "odbc", ## works with dbi to connect to odbc databases
           "dplyr", ## dbplyr connects with dplyr to interface with databases
           "dbplyr",
           "RPostgreSQL",  
           "getPass") ## works like readline(),but masks input
 
## Load packages"
#library(groundhog) ## better for version control

#groundhog.library(packs,  #having trouble with groundhog and odbc
#                  include.suggests = TRUE, 
#                  "2024-06-15", tolerate.R.version='4.3.3')

loadPkg=function(toLoad){
  for(lib in toLoad){
    if(! lib %in% installed.packages()[,1])
    {install.packages(lib,
                      repos='http://cran.rstudio.com/')}
    suppressMessages( library(lib, character.only=TRUE))}}


loadPkg(packs)


## Load R Data:

dataPath <- "./Data/"
thetasPath  <-"./Results/"

load(paste0(dataPath, 'acled_thetas.Rdata'))

## csv version
write.csv(acled_thetas, 
          file=paste0(dataPath, "acled_thetas.csv"))

## SQL Connection:
drv <- dbDriver("PostgreSQL")

port = 5432
db = "harvesterab"
## username = readline(prompt = "Enter database username: ", mask = "*")

username = 'Promachos'
#password = rstudioapi::askForPassword("Database password")

#password = getPass(msg = "Enter database password: ",forcemask = TRUE)

## configured to connect to a locally-hosted 
## PostgreSQL database, called "harvesterab"

con <- dbConnect(drv, 
                 host = "localhost", 
                 port = port, 
                 dbname = db, 
                 user = username) 
                 #password = password)

con ## PostgresSQL connection

dbListTables(con) ## so far nothing in the connection
 
## write the data:
## ACLED Thetas:

DBI::dbWriteTable(value = acled_thetas,
                  conn= con,
                  name="acled_thetas")

## Processed Nigeria Data:
