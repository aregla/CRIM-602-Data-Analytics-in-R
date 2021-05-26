#Assignment #10: Philadelphia Officer-Involved Shootings 

#load packages 
library(lubridate)
library(pdftools)
library(jsonlite)
library(ggmap)
library(sf)

#set working directory 
setwd("~/Desktop/Courses_Fall 2020/SOC 605/Data ")

#load data
load("PPD OIS.RData")

# 1. Examine geo coding errors 
sort(table(gc.ois$loctype))

# a. Examine subset(gc.ois, loctype=="StreetName")
#Response: In this example, we can use the pdf description to ascertain missing information
#e.g., address number

#locate street name errors 
i <- which(gc.ois$loctype=="StreetName")
gc.ois[i,]
ois$text[i]

#Error #1, missing address number; use pdf description to ascertain lat, lon in 
#Google maps 
i <- which(ois$id=="12-20")
gc.ois[i,]
ois$text[i]
a <- geocodeARCGIS("2080 Bridge St, Philadelphia, PA")
gc.ois$geometry[i] <- st_point(as.numeric(a$candidates$location[1,]))

#Error #2, misspelling 
i <- which(ois$id=="09-24")
gc.ois[i,]
ois$text[i]
a <- geocodeARCGIS("16th and Wharton St, Philadelphia, PA")

# b. Examine subset(gc.ois, loctype %in% c("city","Locality"))
#Response: In this example, the address is withheld to avoid the personal 
#information of the officer involved in the shooting. Therefore, we are unable to correct this error.
subset(gc.ois, loctype %in% c("city","Locality"))

# c. Spot check other places in gc.ois, "yes" date format 
#Response: In this example, the (Norris Street, North 11th Street) was missing
#the the direction (west)

#Example #1 
sort(table(gc.ois$loctype))
i <- which(gc.ois$loctype=="yes")
gc.ois[i,]
ois$text[i]

i <- gc.ois[188,]
i <- which(ois$id=="12-43")
gc.ois[i,]
ois$text[i]
a <- geocodeARCGIS("1000 W Norris Street, Philadelphia, PA")

#Example #2, type: secondary, coded as neighborhood; did not capture street number
i <- which(gc.ois$loctype=="secondary")
gc.ois[i,]
ois$text[i]
i <- gc.ois[76,]
i <- which(ois$id=="15-22")
a <- geocodeARCGIS("6300 Ogontz Ave, Philadelphia, PA")


#2. Identify office involved shootings that resulted the offender being transported
#to HUP

# 2.1 create hospital variable
gc.ois$hospital <- NA

#locate variables names within pdfs 
a <- as.factor(grep("Hospital", ois$text))
b <- as.factor(grep("hospital", ois$text))
c <- as.factor(grep("Presby", ois$text))
d <- as.factor(grep("University", ois$text))

#0 entries 
grep("hup", ois$text)
grep("HUP", ois$text)

#stuff relevant cases into variable + add colors 
gc.ois$hospital[a] <- "Hospital"
gc.ois$hospital[b] <- "hospital"
gc.ois$hospital[c] <- "Presby"
gc.ois$hospital[d] <- "University"

#remove quotations 
gc.ois$hosp <-as.factor(gc.ois$hospital)

#2.2 Create a map marking the location of HUP , officer-involved shootings, and
#location of other shootings 

#HUP 
a <- geocodeARCGIS("3400 Spruce Street, Philadelphia, PA 19104 ")

#x and y coordinates
a$candidates$location[1,]

#Presby 
b <- geocodeARCGIS("3800 Powelton Avenue, Philadelphia, PA 19104 ")

#x and y coordinates 
b$candidates$location[1,]

#produce map 
leaflet(gc.ois) %>%
  addTiles() %>%
  addCircleMarkers(radius=3,
                   stroke=FALSE,
                   col=~hospCol,
                   fillOpacity = 1,
                   popup = ~paste0(addressGeo)) %>%
  #hup marker
  addCircleMarkers(lng = a$candidates$location[1,]$x,
                   lat = a$candidates$location[1,]$y,
                   radius=3,
                   color=rgb(0,0,1),
                   fillOpacity = 1,
                   popup= "HUP") %>%
  
  #presby marker 
  addCircleMarkers(lng = b$candidates$location[1,]$x,
                   lat = b$candidates$location[1,]$y,
                   radius=3,
                   color=rgb(0,0,1),
                   fillOpacity = 1,
                   popup= "Presby")

