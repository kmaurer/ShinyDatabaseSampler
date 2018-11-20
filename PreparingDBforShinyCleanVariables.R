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


###
con <- dbConnect(MySQL(),
                 user='ktmaurer',
                 password='pas$18Maurer',
                 dbname='shinyDBSampler',
                 host='10.34.225.221')
?db_write_table()
dbWriteTable()

setwd("C:\\Users\\maurerkt\\Documents\\Databases\\pums")
fips <- read.csv("C:\\Users\\maurerkt\\Documents\\GitHub\\ShinyDatabaseSampler\\StateCountyFIPS.csv")
fips_states <- unique(fips[,c("StateName","StateFIPS")])
PUMS <- rbind(read_csv("psam_pusa.csv"),read_csv("psam_pusb.csv"))
pums_cleaned <- PUMS %>%
  select(one_of(c("ST","PINCP","AGEP","SEX","HINS1","MAR","JWMNP","WKHP")))%>%
  mutate(ST=as.numeric(ST))%>%
  left_join(y=fips_states,by=c("ST"="StateFIPS")) %>%
  select(-ST) %>%
  mutate_at(c("PINCP","AGEP","JWMNP","WKHP"), as.numeric) %>%
  mutate(SEX = factor(SEX,levels=1:2,labels=c("Male","Female")),
         HINS1 = factor(HINS1, levels=1:2, labels=c("Yes","No")),
         MAR = factor(MAR, levels=1:5, c("Married","Widowed","Divorced","Separated","Never Married"))) %>%
  dplyr::rename(state = StateName, 
                sex = SEX,
                age = AGEP,
                income = PINCP,
                work_hours = WKHP,
                commute_mins = JWMNP,
                marital = MAR,
                emp_hlth = HINS1) %>%
  arrange(state, sex) %>%
  select(state,sex,age,marital,income,emp_hlth,commute_mins,work_hours) 
# dbWriteTable(con, "acs2017clean", pums_cleaned)

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

fars_clean <- FARS_all %>%
  left_join(fips, by=c("STATE"="StateFIPS", "COUNTY"="CountyFIPS")) %>%
  select(one_of(c("StateName","CountryName","LATITUDE","LONGITUD",
                  "YEAR","MONTH","DAY","HOUR","DAY_WEEK",
                  "FATALS","PERSONS","VE_TOTAL",
                  "DRUNK_DR","WEATHER"))) %>%
  mutate(MONTH = factor(MONTH, levels=1:12, labels=month.abb),
         DAY_WEEK = factor(DAY_WEEK,levels=c(1:7,9), labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat","Unknown")),
         WEATHER = factor(WEATHER,levels=c(1:12,98,99),labels=c("Clear","Rain","Sleet","Snow","Fog",
                                                                "Severe Crosswind","Sandstorm","Other","Unknown",
                                                                "Cloudy","Blowing Snow","Freezing Rain","Unknown","Unknown"))) %>%
  arrange(YEAR,MONTH,DAY,HOUR,StateName,CountryName) %>%
  dplyr::rename(state=StateName, county=CountryName,
         lat=LATITUDE, long=LONGITUD,
         year=YEAR,month=MONTH,day=DAY,
         weekday=DAY_WEEK,fatalities=FATALS,
         people=PERSONS,vehicles=VE_TOTAL,
         drunk_drivers=DRUNK_DR,weather=WEATHER)

str(fars_clean)
head(fars_clean)
# dbWriteTable(con, "fars2008_2017clean", fars_clean)


#------------------------------------------------------------------------------------
dbinfo <- list(acs2017clean=NULL,
               fars2008_2017clean=NULL)

con <- dbConnect(MySQL(), 
                 user='ktmaurer', 
                 password='pas$18Maurer', 
                 dbname='shinyDBSampler',
                 host='10.34.225.221')

### Working with ACS data: prepping for shiny app
# dbGetQuery(con,"ALTER TABLE `fars2008_2017clean` CHANGE COLUMN `row_names` `row_names` INT ")
# dbGetQuery(con,"ALTER TABLE fars2008_2017clean ADD PRIMARY KEY (row_names(11))")
# dbGetQuery(con,"ALTER TABLE `acs2017clean` CHANGE COLUMN `row_names` `row_names` INT")
# dbGetQuery(con,"ALTER TABLE acs2017clean ADD PRIMARY KEY (row_names(11))")
# dbGetQuery(con,"ALTER TABLE acs2017clean ADD KEY (ST(3))")
# dbGetQuery(con,"ALTER TABLE acs2017clean ADD KEY (SEX(1))")

dbGetQuery(con,"SHOW columns FROM acs2017clean")
dbGetQuery(con,"SHOW columns FROM fars2008_2017clean")
dbGetQuery(con,"SELECT DISTINCT YEAR FROM fars2008_2017clean")


### Set attributes needed to run sampling functions quicker
# number of rows
dbinfo$acs2017clean$N <- as.numeric(dbGetQuery(con,"SELECT COUNT(*) FROM acs2017clean"))
dbinfo$fars2008_2017clean$N <- as.numeric(dbGetQuery(con,"SELECT COUNT(*) FROM fars2008_2017clean"))
# Index columns
dbinfo$acs2017clean$idx <- "row_names"
dbinfo$fars2008_2017clean$idx <- "row_names"
# stratification choices
dbinfo$acs2017clean$stratchoices <- c(State="state",Sex="sex")
dbinfo$fars2008_2017clean$stratchoices <-  c(Year="year", Month="month" , 'Day of Week'="weekday" , State="state" , 
                               # 'Weather Conditions'="WEATHER" , 'National Highway'="NHS" , 
                               'Drunk Drivers'="drunk_drivers")
# Data dictionary url
dbinfo$acs2017clean$url <- "row_names"
dbinfo$fars2008_2017clean$url <- "row_names"
# variable name labels 
dbinfo$acs2017clean$keeper_cols <- c("State"= "state",
                                      "Individual Income (USD)"="income",
                                      "Age"="age",
                                      "Sex"="sex",
                                      "Employer Provides Health Insurance"="emp_hlth",
                                      "Marital Status"="MAR",
                                      "Marraige Year"="MARHYP",
                                      "Employment Status"="ESR")
dbinfo$fars2008_2017clean$keeper_cols <- c('Number of Fatalities In Crash'='FATALS',
                             'Number of Persons Involved'='PERSONS',
                             'Number of Vehicle Involved'='VE_TOTAL',
                             'State (FIPS coded)'='STATE','County (FIPS coded)'='COUNTY',
                             'GPS Latitude'='LATITUDE',  'GPS Longitude'='LONGITUD',
                             'Crash Year'='YEAR', 'Crash Month'='MONTH',
                             'Crash Day'='DAY','Crash Hour'='HOUR',
                             'Day of Week'='DAY_WEEK',
                             'Number of Drunk Drivers in Crash'='DRUNK_DR',
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
  sampall <- dbGetQuery(con,  sprintf("select * from %s WHERE row_names in (%s)",dbtabname, paste(SRSindex, collapse=",")))
  return(sampall)
}
dbtabname="fars2008_2017clean"
sample_data <- SRS(10,"fars2008_2017clean")
