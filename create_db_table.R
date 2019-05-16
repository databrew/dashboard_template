# only to run once to set up database
library(tidyverse)
library(ggplot2)
library(RPostgreSQL)

# source scripts
source('credentials_connect.R')
source('credentials_extract.R')

# define whether creating locally or remotely 
local <- TRUE
if(local){
  credentials <- credentials_extract(credentials_file = 'credentials/credentials_local.yaml', 
                                     all_in_file = TRUE)
} else {
  credentials <- credentials_extract(credentials_file = 'credentials/credentials.yaml', 
                                     all_in_file = TRUE)
}

# make sure a db is created in psql called dashboarddb
# created by CREATE DATABASE dashboarddb

# create a connection object with credentials
co <- credentials_connect(options_list = credentials)

# create table
df <- tibble(person = c('joe', 'ben'),
             letter = c('a', 'b'),
             number = c(1,2))

# write to the table to the database to which we are connected
dbWriteTable(conn = co, 
             name = 'table1', 
             value = df, 
             row.names = FALSE,
             overwrite = TRUE)

# disconnect from the db
dbDisconnect(co)
