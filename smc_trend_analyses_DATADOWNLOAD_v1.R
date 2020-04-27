library(tidyverse)
library(DBI) # needed to connect to data.dfbase
library(dbplyr) # needed to connect to data.dfbase
library(RPostgreSQL) # needed to connect to our data.dfbase
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(tidyverse)

# con is short for connection
# Create connection to the data.dfbase
con <- dbConnect(
  PostgreSQL(),
  host = "192.168.1.17",
  dbname = 'smc',
  user = 'smcread',
  password = '1969$Harbor' # if we post to github, we might want to do rstudioapi::askForPassword()
)

lu_station.df <- dbGetQuery(con, ' 
                      SELECT * FROM
                      sde.lu_stations
                            ') 
csci.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_csci_core')

asci.df<- dbGetQuery(con, '
                     SELECT * FROM
                     sde.analysis_asci')

gis.df<-dbGetQuery(con, '
                     SELECT * FROM
                     sde.tblgismetrics')


save.image("data/smc_trends.Rdata")
