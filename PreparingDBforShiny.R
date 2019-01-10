library(shiny)
library(tidyverse)
library(datasets)
library(plyr)
library(DBI)
library(RMySQL)
library(lubridate)
library(ggplot2)

library(sas7bdat)

#### R code from combining download files into dataframes that were uploaded as tables to MySQL DB
## 2017 ACS
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_pums_csv_2017&prodType=document 
# https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2017.pdf?#
## 2008-2017 FARS
# ftp://ftp.nhtsa.dot.gov/fars/ 


# ###
# con <- dbConnect(MySQL(),  
#                  user='ktmaurer', 
#                  password='pas$18Maurer',
#                  dbname='pums',
#                  host='10.34.225.221')
# ?db_write_table()
# dbWriteTable()
# 
# PUMS <- rbind(read_csv("ss16pusa.csv"),read_csv("ss16pusb.csv"))
# # dbWriteTable(con, "acs_pums_2017", PUMS)
# 
# query = dbSendQuery(con,"SELECT COUNT(AGEP) FROM acs_pums_2017")
# pums_db <- tbl(con, "acs_pums_2017")
# 
# pums_db %>%
#   group_by(ST) %>%
#   summarize()
#
#
FARS_list <- list(
  dat2008 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FSAS2008/accident.sas7bdat"),
  dat2009 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FSAS2009/accident.sas7bdat"),
  dat2010 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FSAS2010/accident.sas7bdat"),
  dat2011 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FSAS2011/accident.sas7bdat"),
  dat2012 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FSAS2012/accident.sas7bdat"),
  dat2013 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FARS2013NationalSAS/accident.sas7bdat"),
  dat2014 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FARS2014NationalSAS/accident.sas7bdat"),
  dat2015 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FARS2015NationalSAS/accident.sas7bdat"),
  dat2016 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FARS2016NationalSAS/accident.sas7bdat"),
  dat2017 = read.sas7bdat("C:/Users/maurerkt/Documents/Databases/FARS/FARS2017NationalSAS/accident.sas7bdat")
)

dat1 <- NULL
for(year in 2013:2014){
  dat1 <- rbind(dat1, read.sas7bdat(paste0("C:/Users/maurerkt/Downloads/FARS",year,"NationalSAS/accident.sas7bdat")))
}

dat2 <- NULL
for(year in 2015:2017){
  dat2 <- rbind(dat2, read.sas7bdat(paste0("C:/Users/maurerkt/Downloads/FARS",year,"NationalSAS/accident.sas7bdat")))
}
FARS_all <- FARS_list[[1]]
for(i in 2:10){
  all_cols <- unique(names(FARS_all),names(FARS_list[[i]]))
  in_all <- all_cols[all_cols %in% names(FARS_all)  & all_cols %in% names(FARS_list[[i]])]
  FARS_all <- rbind(select(FARS_all, in_all),select(FARS_list[[i]], in_all))
}
head(FARS_all)
# # dbWriteTable(con, "FARS", FARS_all)


#------------------------------------------------------------------------------------
dbinfo <- list(acs_pums_2017=NULL,
               FARS=NULL)

con <- dbConnect(MySQL(), 
                 user='ktmaurer', 
                 password='pas$18Maurer', 
                 dbname='pums',
                 host='10.34.225.221')

### Working with ACS data: prepping for shiny app
# dbGetQuery(con,"ALTER TABLE `FARS` CHANGE COLUMN `row_names` `row_names` INT ")
# dbGetQuery(con,"ALTER TABLE FARS ADD PRIMARY KEY (row_names(11))")
dbGetQuery(con,"ALTER TABLE `acs_pums_2017` CHANGE COLUMN `row_names` `row_names` INT ")
# dbGetQuery(con,"ALTER TABLE acs_pums_2017 ADD PRIMARY KEY (row_names(7))")
# dbGetQuery(con,"ALTER TABLE acs_pums_2017 ADD KEY (ST(3))")
# dbGetQuery(con,"ALTER TABLE acs_pums_2017 ADD KEY (SEX(1))")

dbGetQuery(con,"SHOW columns FROM acs_pums_2017")
dbGetQuery(con,"SELECT DISTINCT YEAR FROM FARS")


### Set attributes needed to run sampling functions quicker
# number of rows
dbinfo$acs_pums_2017$N <- as.numeric(dbGetQuery(con,"SELECT COUNT(*) FROM acs_pums_2017"))
dbinfo$FARS$N <- as.numeric(dbGetQuery(con,"SELECT COUNT(*) FROM FARS"))
# Index columns
dbinfo$acs_pums_2017$idx <- "row_names"
dbinfo$FARS$idx <- "row_names"
# stratification choices
dbinfo$acs_pums_2017$stratchoices <- c(State="ST",Sex="SEX")
dbinfo$FARS$stratchoices <-  c(Year="YEAR", Month="MONTH" , 'Day of Week'="DAY_WEEK" , State="STATE" , 
                               # 'Weather Conditions'="WEATHER" , 'National Highway'="NHS" , 
                               'Drunk Driver'="DRUNK_DR")
# Data dictionary url
dbinfo$acs_pums_2017$url <- "row_names"
dbinfo$FARS$url <- "row_names"
# variable name labels 
dbinfo$acs_pums_2017$keeper_cols <- c("State (FIPS coded)"= "ST",
                                      "Individual Income (USD)"="PINCP",
                                      "Age"="AGEP",
                                      "Sex (1=Male, 2=Female)"="SEX",
                                      "Employer Provides Health Insurance (1=Yes,2=No)"="HINS1",
                                      "Marital Status"="MAR",
                                      "Marraige Year"="MARHYP",
                                      "Employment Status"="ESR")
dbinfo$FARS$keeper_cols <- c('Number of Fatalities In Crash'='FATALS',
                             'Number of Persons Involved'='PERSONS',
                             'Number of Vehicle Involved'='VE_TOTAL',
                             'State (FIPS coded)'='STATE','County (FIPS coded)'='COUNTY',
                             'GPS Latitude'='LATITUDE',  'GPS Longitude'='LONGITUD',
                             'Crash Year'='YEAR', 'Crash Month'='MONTH',
                             'Crash Day'='DAY','Crash Hour'='HOUR',
                             'Day of Week'='DAY_WEEK',
                             'Drunk Drivers in Crash'='DRUNK_DR',
                             'National Highway System'='NHS',
                             'Atmospheric Condition'='WEATHER')
# Indeces for 

dbinfo

#----------------------------
SRS <- function(n, dbtabname, seed=NA){
  # select row numbers to draw
  if(!is.na(seed)) set.seed(seed)
  SRSindex <- sample(1:dbinfo[[dbtabname]]$N,n,replace = FALSE)
  # run query to pull draws
  sampall <- dbGetQuery(con,  sprintf("select * from %s WHERE %s in (%s)",dbtabname,dbinfo[[dbtabname]]$idx, paste(SRSindex, collapse=",")))
  return(sampall)
}

sample_data <- SRS(10,"acs_pums_2017")
dplyr::select(sample_data,one_of(dbinfo$FARS$keeper_cols))


#-------------------------------
StratSampler <- function(nper, dbtabname, seed, stratcol){
  # randomly select row numbers from within groups
  set.seed(seed)
  allsampindex <- NULL
  for (i in groupnames){
    groupidxs <- grpidx[which(grpidx[,1]==i) , 2]    
    if(nper >= length(groupidxs)){
      allsampindex <- c(allsampindex, groupidxs)}else{
        allsampindex <- c(allsampindex, sample(groupidxs, nper, replace = FALSE))
      }
  }
  # query to pull draws
  sampall <- dbGetQuery(con1,  sprintf("select * from %s WHERE %s in (%s) ORDER BY %s",dbtabname,indexcol, paste(allsampindex, collapse=","),stratcol))
  # clean up variable types (all keys set to factors)
  keysdat <-  dbinfolist$keylist[[infolistnum]]
  for (j in 1:length(sampall[1,])){
    if (is.character(sampall[,j]) | names(sampall)[j] %in% keysdat) sampall[,j] <- as.factor(as.character(sampall[,j]))
  }
  rm(grpidx)
  return(sampall)
}

