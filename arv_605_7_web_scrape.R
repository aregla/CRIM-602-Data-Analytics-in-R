#Question 1: Scrape data

#load packages 
install.packages("sqldf")
install.packages("lubridate")
install.packages("doParallel")
library(sqldf)
library(lubridate)
library(leaflet)
library(ggmap)
library(hexbin)
library(httr)
library("doParallel")
#specify dates 
dates.list <- seq(ymd("2010-01-01"), ymd("2019-01-10"), by="days")

#results 
results <- vector("list", length(dates.list))

#data scrape loop 
#specify dates 
dates.list <- seq(ymd("2010-01-01"), ymd("2019-10-01"), by="days")

#results 
results <- vector("list", length(dates.list))

cl <- makeCluster(4)
registerDoParallel(cl)

timeStart <- Sys.time() # record the starting time
results <- 
  foreach(i.date=1:length(dates.list)) %dopar%
  {
   
    url.text <- paste0("http://www.boxofficemojo.com/date/",
                       dates.list[i.date])
    
    a <- scan(url.text,what="",sep="")
    
    #locate data in table 
    a <- paste(a, collapse=" ")
    a <- strsplit(a, "</tr>")[[1]]
    a <- a[-c(1,length(a))]
    a <- strsplit(a, "</td>")
    
    # get movie names
    data0 <- data.frame(movie = sapply(a, function(x) gsub("<[^>]*>","", x[3]) ))
    
    #get movie gross 
    data0$gross <- sapply(a, function(x) gsub("<[^>]*>|[$,]","", x[4]) )
    
    #add date 
    data0$date <- dates.list[i.date]
    
    return(data0)
  }

timeEnd <- Sys.time()
timeEnd-timeStart
stopCluster(cl)

movie.data.mojo <- do.call(rbind,results)
movie.data.mojo

setwd("~/Desktop/Courses_Fall 2020/SOC 605/Data ")
save(movie.data.mojo,file="movie revenue mojo.RData",compress=TRUE)

#Question 2: Compare data frames 
install.packages("compareDF")
library("compareDF")
install.packages("janitor")
library(janitor)
compare_df_cols(movie.data, movie.data.mojo)

#Variable types match (e.g., gross = numeric)

#Check for missing data 
is.na(movie.data)
is.na(movie.data.mojo)

#Compare movie names 
install.packages("data.table")
library("data.table")
data.table(movie.data$movie)
data.table(movie.data.mojo$movie)

#movie.data.mojo has cleaner movie titles 

#Confirm that max matches across data frames 
movie.data[which.max(movie.data$gross),]
movie.data.mojo[which.max(movie.data.mojo$gross),]

#In both data sets, Avatar has the highest daily gross 

#Compare gross variable by years using graphs 
movie.gross.one <- ggplot(movie.data, aes(x=date, y=gross)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
movie.gross.one

movie.gross.two <- ggplot(movie.data.mojo, aes(x=date, y=gross)) +
  geom_line( color="#69b3a2") + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
movie.gross.two

#Both graphs include the same type of distribution peaks 

#Conclusion: I would select the movie mojo data, given the 
#cleanliness of the movie variable 