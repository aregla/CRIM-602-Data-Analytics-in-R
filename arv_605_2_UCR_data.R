#Assignment 2 
#Date: September 20, 2020 
#Name: Alejandra Regla-Vargas
#Course: SOC605 

setwd("~/Desktop/Coursework-year-3/SOC 605/Week 1")
load("/Users/alejandra/Desktop/Coursework-year-3/SOC 605/Week 1/Data/UCR/ICPSR_35021/DS0001/35021-0001-Data.rda")

#1 Which ten cities have the highest homicide rate per 100,000 residents?

#data frame with variables of interest 
ucr <- data.frame(ORI   =as.character(da35021.0001$V3),
                  AGENCY=as.character(da35021.0001$V29),
                  AREA  =as.character(da35021.0001$V26),
                  POP   =da35021.0001$V14,
                  MONTHS=as.character(da35021.0001$V12),
                  STATE=as.character(da35021.0001$V2),
                  stringsAsFactors=FALSE)

#create murder variable 
var.lookup <- attributes(da35021.0001)$variable.labels
var.names <- names(var.names)
var.names <- grep("ACT NUM MURDER",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$murder <- rowSums(da35021.0001[,var.names])

#create a subset of population > 0 
ucr_sub <- subset(ucr, POP > 0)

# create murder rate
murder_rate <- ucr_sub$murder / ucr_sub$POP * 100000
head(murder_rate)

#add murder rate column 
ucr_sub$murder_rate <- ucr_sub$murder / ucr_sub$POP * 100000

#order murder rate, descending order
ucr_sort <- ucr_sub[order(-ucr_sub$murder),]

#print ten cities 
ucr_sort[1:10,]

#2 Which ten cities with more than 100,000 residents have the highest homicide rate per 100,000 residents?
ucr_sort2 <- subset(ucr_sort, POP > 100000)

#print ten cities 
ucr_sort2[1:10,]

#3 Which ten cities have the most costly crime burden? Use the cost of crime numbers shown in the cost of crime slides

#add crime categories (e.g., robbery, serious assault, burglary) 
var.names <- grep("ACT NUM MURDER",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$murder <- rowSums(da35021.0001[,var.names])

var.names <- grep("ACT NUM RAPE",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$rape <- rowSums(da35021.0001[,var.names])

var.names <- grep("ACT NUM ROBBRY",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$robbery <- rowSums(da35021.0001[,var.names])

# aggravated assault are #assaults - #simple assaults
var.names <- grep("ACT NUM ASSLT",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$assault <- rowSums(da35021.0001[,var.names])
var.names <- grep("ACT # SIMPLE ASSLT",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$assault <- ucr$assault - rowSums(da35021.0001[,var.names])

var.names <- grep("ACT # BURGLARY",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$burglary <- rowSums(da35021.0001[,var.names])

var.names <- grep("ACT # LARCENY",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$larceny <- rowSums(da35021.0001[,var.names])

var.names <- grep("ACT # VHC THEFT",var.lookup,value=TRUE)
var.names <- names(var.names)
ucr$gta <- rowSums(da35021.0001[,var.names])

#create a subset of population > 0 from UCR data 
ucr_sub3 <- subset(ucr, POP > 0)

#create cost variables
ucr_sub3$murder_cost <- 86000000 / ucr_sub3$murder
ucr_sub3$rape_cost <- 220000 / ucr_sub3$rape
ucr_sub3$robbery_cost <- 67000 / ucr_sub3$robbery
ucr_sub3$assault_cost <- 87000 / ucr_sub3$assault
ucr_sub3$burglary_cost <- 13000 / ucr_sub3$burglary 
ucr_sub3$larceny_cost <- 2100 / ucr_sub3$larceny
ucr_sub3$gta_cost <- 9100 / ucr_sub3$gta

#total cost variable 
ucr_sub3$crime_total <- rowSums(ucr_sub3[,15:19])

#order by cost of crime 
ucr_crimesort <- ucr_sub3[order(ucr_sub3$crime_total),]

#print 
ucr_crimesort[1:10,]





