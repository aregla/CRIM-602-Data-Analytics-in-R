#Assignment: Hot Spot Map_Los Angeles 
#Name: Alejandra Regla-Vargas 


#load packages
library(data.table) #faster way to read large dataset
library(tidyverse) #load dplyr, tidyr and ggplot
library(ggmap) #use to read map
library(maps) #map tools kits
library(mapdata) #read the map data
library(lubridate) #date manuplation
library(ggrepel) #better label
library(varhandle) #load the function unfactor

#set working directory 
setwd("~/Desktop/Courses_Fall 2020/SOC 605/Hot spot data")

#load data 
crime_la <- as.data.frame(fread("Crime_Data_from_2010_to_2019.csv", na.strings = c("NA")))
glimpse(crime_la)

#clean data 
#select relevant variables (subset of data)
crime_la_selected <- select(crime_la, `DATE OCC`, `TIME OCC`, 
                            `AREA`, `Crm Cd Desc`, `Vict Age`,
                            `Vict Sex`, `Vict Descent`, `Premis Desc`,
                            `Weapon Desc`, `Status Desc`, LAT, LON)

#remove time from date occurred
crime_la_selected$`DATE OCC` <- as.character(as.Date(mdy_hms(crime_la_selected$`DATE OCC`)))
glimpse(crime_la_selected)

#select only 2017 and 2018
crime_selected_years <- filter(crime_la_selected, `DATE OCC` 
>= as_date("2017-01-01"), `DATE OCC` <= as_date("2017-12-30"))

#remove these data frames to same memory
rm(crime_la, crime_la_selected, location) #remove these data frames to same memory
glimpse(crime_selected_years)


#separate date into year, month and day.
crime_selected_years$year <- year(crime_selected_years$`DATE OCC`)
crime_selected_years$month <- month(crime_selected_years$`DATE OCC`)
crime_selected_years$days <- day(crime_selected_years$`DATE OCC`)

#recode the variables in
crime_selected_years$`Vict Sex` <- recode(crime_selected_years$`Vict Sex`, 'F' = 'Female', 'M' = 'Male', 'X' = 'Unknown')

crime_selected_years$`Vict Descent` <- recode(crime_selected_years$`Vict Descent`, "A" = "Other Asian", "B" = "Black", "C" = "Chinese", "D" = "Cambodian", "F" = "Filipino", "G" = "Guamanian", "H" = "Hispanci/Latin/Mexican", 'I' = "American Indian/Alaskan Native", "J" = "Japanese", "K" = "Korean", "L" = "Laotian", "O" = "Other", "P" = "Pacific Islander", "S" = "Somoan", "U" = "Hawaiian", "V" = "Vietnamese", "W" = "White", "X" = "Unknown", "Z" = "Asian Indian")

#convert the character into factor
character_vars <- lapply(crime_selected_years, class) == "character"
crime_selected_years[, character_vars] <- lapply(crime_selected_years[, character_vars], as.factor)

glimpse(crime_selected_years)

#visualize frequency of each crime 
year_2017 <- crime_selected_years %>%
  filter(year == "2017")

group <- year_2017 %>%
  group_by(`Crm Cd Desc`) %>%
  summarise(total = n()) %>%
  distinct() %>%
  top_n(20)

group %>%
  ggplot(aes(reorder(`Crm Cd Desc`, total), y = total)) +
  geom_col(fill = "red") +
  geom_label_repel(aes(label = total), size = 2.5) +
  coord_flip() +
  labs(title = "Top 20 Crime Commited in 2017", 
       x = "Crime Description", 
       y = "Total")

#hotspot map of most frequent crime in Los Angeles 
#get the map of LA
ggmap::register_google(key = "******************")

la.map <- ggmap(get_map(c(-118.243685, 34.052234),
                             scale="auto",source="stamen"))
la.map

#unfactor LAT and LON variable
year_2017$LAT <- unfactor(year_2017$LAT)
year_2017$LON <- unfactor(year_2017$LON)

#select relevant variables
mapping <- year_2017 %>%
  select(`Crm Cd Desc`, LON, LAT) %>%
  filter(`Crm Cd Desc` == 'BATTERY - SIMPLE ASSAULT') %>%
  na.omit()

#mapping
la.map + geom_density_2d(aes(x = LON, y = LAT), data = mapping) +
  stat_density2d(data = mapping, 
                 aes(x = LON, y = LAT, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red", 

                                                                                                                              guide = FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)




