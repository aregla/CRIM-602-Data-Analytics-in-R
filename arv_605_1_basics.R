#Assignment: Introduction to R
#Name: Alejandra Regla-Vargas 
#Date: September 13, 2020 

#Question 1 
setwd("~/Desktop/Coursework-year-3/SOC 605/Week 1")
data <- read.csv("leoka-feloniously killed.csv")

#Question 2 
data[1:3,]

#Question 3 
data[data$Area=="Ohio", "X2007"]

#Question 4 
subseteast <- subset(data, grepl("East North Central", Region2))
sum(subseteast$X2012)

#Question 5
data$row_sum = rowSums(data[,c(4,5,6,7,8,9,10,11,12,13)])

#Question 6 
which.max(data$row_sum)
data[38,]

aggregate(row_sum~Area, data=data, max)

#Question 7 Print the region (Region1) having the largest number of officers killed (Hint: review aggregate
max(data$Region1)
aggregate(row_sum~Region1, data=data, max)

#Question 8 Find all the areas that have had no officers killed between 2004 and 2013
aggregate(row_sum~Area, data=data, min)

#Question 9 In which year and in which area were nine officers killed?
max(data$X2004)
max(data$X2005)
max(data$X2006)
max(data$X2007)
max(data$X2008)
max(data$X2009)
max(data$X2010)
max(data$X2011)
max(data$X2012)
max(data$X2013)

data$Area[which.max(data$X2007)]



which.max(data[,c(4,5,6,7,8,9,10,11,12,13)])

#Question 10 Sort the regions (Region1) from smallest to largest in terms of the number of officers killed in 2013
newsorts <- data[order(data$Region1, data$X2013),]

#Question 11 Create a bar chart showing the number of officers killed by region (Region 1)
barplot(data$row_sum, names.arg = data$Region1, cex.names=0.5, horiz=TRUE)

#Question 12 Plot the total number of officers killed by year (Hint: consider using colSums())
barplot(colSums(data[,4:13]))

#Question 13
reshape(data,
        idvar="Area",
        v.names="count",
        varying=c("X2004","X2005","X2006","X2007","X2008","X2009","X2010", "X2011","X2012","X2013"),
        direction="long",
        times=2004:2013,
        timevar="year")


PercChange(data, data$X2013, GroupVar, NewVar= 'Percent change', slideBy = -1, type = "percent", ...)
