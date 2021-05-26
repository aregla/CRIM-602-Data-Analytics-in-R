#Assignment: crime, movie, and weather data 
#Name: Alejandra Regla-Vargas 
#Date: November 18, 2020 


#load packages 
library(sqldf)
library(lubridate)

#set working directory 
setwd("~/Desktop/Courses_Fall 2020/SOC 605/Data ")

# connect to the Chicago crime data
con <- dbConnect(SQLite(), dbname="chicagocrime.db")
dbListTables(con)


#1.1 Describe the relationship between movie revenue and crime 
#create temp tables of crime count and movie tab 
res <- dbSendQuery(con, "
                      CREATE TEMPORARY TABLE movie_tab AS
                      SELECT SUM(gross) AS gross,
                             date
                      FROM movie
                      WHERE CAST(STRFTIME('%w',date) AS INTEGER)>=5
                      GROUP BY date")
dbClearResult(res)


res <- dbSendQuery(con, "
                      CREATE TEMPORARY TABLE crime_tab AS
                      SELECT COUNT(*) AS count,
                             date(date) AS date0
                      FROM crime
                      WHERE (CAST(STRFTIME('%w',date) AS INTEGER)>=5)  AND
                            (CAST(STRFTIME('%H',date) AS INTEGER)>=18) AND
                            (CAST(STRFTIME('%Y',date) AS INTEGER)>=2010)
                      GROUP BY date0")
dbClearResult(res)

#merge crime and revenue 
res <- dbSendQuery(con, "
                      SELECT movie_tab.gross,
                             crime_tab.count,
                             movie_tab.date
                      FROM crime_tab
                         INNER JOIN movie_tab
                         ON crime_tab.date0=movie_tab.date")
crime.tab.sql <- fetch(res, n = -1)
dbClearResult(res)
plot(count~gross,data=crime.tab.sql)

#The plot shows an increase in crime between income levels 1 and 2; however, as income increases, the crime count decreases 

##############################

#1.2 Describe the relationship between movie revenue and crime type 

#temp table of theft 
res <- dbSendQuery(con, "
                      CREATE TEMPORARY TABLE theft_tab AS
                      SELECT COUNT(*) AS count,
                      DATE(Date) AS date0
                      FROM crime
                      
                      INNER JOIN iucr ON 
                      crime.iucr=iucr.iucr
                      WHERE DATE(Date) >= '2001-01-01' AND
                      iucr.PrimaryType='THEFT'
                      GROUP BY Date")

res <- dbSendQuery(con, "select * from theft_tab")
dbFetch(res, n= 10)

#temp table of revenue and theft 
res <- dbSendQuery(con, "
                      SELECT movie_tab.gross,
                             theft_tab.count,
                             movie_tab.date
                      FROM theft_tab
                         INNER JOIN movie_tab
                         ON theft_tab.date0=movie_tab.date")
theft.tab.sql <- fetch(res, n = -1)
dbClearResult(res)
plot(count~gross,data=theft.tab.sql)

###################

#temp table of burglary
res <- dbSendQuery(con, "
                      CREATE TEMPORARY TABLE burg_tab AS
                      SELECT COUNT(*) AS count,
                      DATE(Date) AS date0
                      FROM crime
                      
                      INNER JOIN iucr ON 
                      crime.iucr=iucr.iucr
                      WHERE DATE(Date) >= '2001-01-01' AND
                      iucr.PrimaryType='BURGLARY'
                      GROUP BY Date")

res <- dbSendQuery(con, "select * from burg_tab")
dbFetch(res, n= 10)

#temp table of revenue and burglary  
res <- dbSendQuery(con, "
                      SELECT movie_tab.gross,
                             burg_tab.count,
                             burg_tab.date
                      FROM burg_tab
                         INNER JOIN movie_tab
                         ON burg_tab.date0=movie_tab.date")
burg.tab.sql <- fetch(res, n = -1)
dbClearResult(res)
plot(count~gross,data=burg.tab.sql)


#2.a load weather data to SQL
load("/Users/alejandraregla-vargas/Desktop/Courses_Fall 2020/SOC 605/Data /weatherChicago.RData")
if(dbExistsTable(con, "weather")) dbRemoveTable(con, "weather")
dbWriteTable(con, "weather", weatherChicago, row.names=FALSE)

dbListTables(con)
res <- dbSendQuery(con, "select * from weather")
dbFetch(res, n= 10)

#merge weather and crime 
res <- dbSendQuery(con, "
                  CREATE TEMPORARY TABLE crimeweather AS
                   SELECT   weather.DATE,
                             weather.PRCP,
                             weather.SNOW,
                             weather.SNWD
                             crime_tab.count
                      FROM weather
                         INNER JOIN crime_tab on
                         weather.DATE = crime_tab.date0")

crimeweather.sql <- fetch(res, n = -1)

res <- dbSendQuery(con, "select * from crimeweather")
dbFetch(res, n= 10)

#plot weather type and crime 
plot(count~PRCP, data= weather_crimes.sql)

#plot weather type and crime 
plot(count~SNOW, data= weather_crimes.sql)

#plot weather type and crime 
plot(count~SNWD, data= weather_crimes.sql)


#temp table of weather and burglary  
res <- dbSendQuery(con, "
                      SELECT 
                      weather.date
                      weather.PRCP,
                             burg_tab.count,
                             burg_tab.date
                      FROM weather
                         INNER JOIN weather.PRCP
                         ON burg_tab.date0=weather.PRCP")
burg.tab.sql <- fetch(res, n = -1)
dbClearResult(res)
plot(count~gross,data=burg.tab.sql)


#temp table of weather and theft 
res <- dbSendQuery(con, "
CREATE TEMPORARY TABLE weather_theft AS
                      SELECT weather.PRCP,
                            weather.date
                            weather.SNOW
                            weather.SNWD
                             theft_tab.count,
                      FROM theft_tab
                         INNER JOIN theft_tab ON
                         weather.date=theft_tab.date0")

theft.tab.sql <- fetch(res, n = -1)
dbClearResult(res)
plot(count~gross,data=theft.tab.sql)


#temp table of revenue and burglary  
res <- dbSendQuery(con, "
CREATE TEMPORARY TABLE weather_burg AS
                      SELECT weather.PRCP,
                            weather.date
                            weather.SNOW
                            weather.SNWD
                             burg_tab.count,
                      FROM burg_tab
                         INNER JOIN burg_tab ON
                         weather.date=burg_tab.date0")
