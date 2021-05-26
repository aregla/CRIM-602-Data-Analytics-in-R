#Assignment #3: Working with NCVS data 
#Date: September 29, 2020 
#Alejandra Regla-Vargas 

#Set working directory 
setwd("~/Desktop/Coursework-year-3/SOC 605/Week 1")

#Load data, variables, exclude outside us data 
# identification information
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2012/ICPSR_34650/DS0001/34650-0001-Data.rda")
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2013/ICPSR_35164/DS0001/35164-0001-Data.rda")
data.addr12 <- da34650.0001
data.addr13 <- da35164.0001
# household records
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2012/ICPSR_34650/DS0002/34650-0002-Data.rda")
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2013/ICPSR_35164/DS0002/35164-0002-Data.rda")
data.hh12 <- da34650.0002
data.hh13 <- da35164.0002
# person records
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2012/ICPSR_34650/DS0003/34650-0003-Data.rda")
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2013/ICPSR_35164/DS0003/35164-0003-Data.rda")
data.pers12 <- da34650.0003
data.pers13 <- da35164.0003
# incident records
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2012/ICPSR_34650/DS0004/34650-0004-Data.rda")
load("~/Desktop/Coursework-year-3/SOC 605/Week 1/Data/NCVS2013/ICPSR_35164/DS0004/35164-0004-Data.rda")
data.inc12 <- da34650.0004
data.inc13 <- da35164.0004

#remove original datasets with longer names 
rm(da34650.0001,da34650.0002,da34650.0003,da34650.0004,
   da35164.0001,da35164.0002,da35164.0003,da35164.0004)

# create a 2012 data year incident data frame
# incidents in 2012 and 2013 are all in one 
data.inc <- rbind(data.inc12, data.inc13)
table(data.inc$V4015) # year crime occured
data.inc <- subset(data.inc, V4015==2012)

# exclude crime happening outside US
#    V4022 - IN WHAT CITY, TOWN, VILLAGE.
#    is.na is variable 4022 missing?
data.inc <- subset(data.inc, (V4022!="(1) Outside U.S.") |
                     is.na(V4022))
table(data.inc$V4022)

i <- which(data.inc$V4019=="(2) No (is series)")
# create a "date year" weight
data.inc$WGTVICDY <- data.inc$WGTVICCY
data.inc$WGTVICDY[i] <- with(data.inc, WGTVICDY[i] * V4016[i])

# tabulate total weight by crime type to estimate count
aggregate(WGTVICDY~V4529, data=data.inc, sum)
class(data.inc$V4529)

############1 With the NCVS, describe the context of assaults.

#create subset assault variable 
data.inc_rape <- subset(data.inc, V4529=="(01) Completed rape")

#Where did they occur? 
aggregate(WGTVICDY~V4529+V4022, data=data.inc_rape, sum)

#When did they occur?
aggregate(WGTVICDY~V4529+YEARQ, data=data.inc_rape, sum)

#Who was the offender?
aggregate(WGTVICDY~V4529+V4236, data=data.inc_rape, sum)

#What sort of weapons were used?

#Did offender have a weapon? 
aggregate(WGTVICDY~V4529+V4049, data=data.inc_rape, sum)

aggregate(WGTVICDY~V4529+V4051, data=data.inc_rape, sum)
aggregate(WGTVICDY~V4529+V4052, data=data.inc_rape, sum)
aggregate(WGTVICDY~V4529+V4053, data=data.inc_rape, sum)
aggregate(WGTVICDY~V4529+V4054, data=data.inc_rape, sum)
aggregate(WGTVICDY~V4529+V4054, data=data.inc_rape, sum)
aggregate(WGTVICDY~V4529+V4056, data=data.inc_rape, sum)
aggregate(WGTVICDY~V4529+V4057, data=data.inc_rape, sum)

#The offender had a knife 

# Were the police called?
# Reported to police 
aggregate(WGTVICDY~V4529+V4399, data=data.inc_rape, sum)

#Called police or guard 
aggregate(WGTVICDY~V4529+V4156, data=data.inc_rape, sum)

# How many victims used firearms defensively?
aggregate(WGTVICDY~V4529+V4144, data=data.inc_rape, sum)

#2 Estimate the number of crimes by race of victim by crime type

#fix factor levels
i <- sapply(data.pers12, levels)
i <- i[!sapply(i,is.null)]
i <- sapply(i, function(x) all(substring(x,1,1)=="("))
var.fix <- names(i)[i]
for(xj in var.fix)
{
  data.pers12[,xj] <- gsub("\\(([0-9]+)\\).*","\\1",
                           data.pers12[,xj])
  data.pers12[,xj] <- as.numeric(data.pers12[,xj])
}

#create person data frame
data.pers <- rbind(data.pers12,data.pers13)
data.pers <- subset(data.pers, YEARQ>=2012.1 & YEARQ<=2013.2)

#merge data 
data.inc$IDPER.YEARQ  <- with(data.inc,
                              paste(IDPER,YEARQ,sep="|"))
data.pers$IDPER.YEARQ <- with(data.pers,
                              paste(IDPER,YEARQ,sep="|"))

#match records
i <- match(data.inc$IDPER.YEARQ, data.pers$IDPER.YEARQ)
data.inc[1:3,1:5]
data.pers[i[1:3],1:5]

# merge demographic data to incident file 
data.inc$race <- data.pers$V3023A[i]


#Show a table with the estimated total number of victims by race and crime type
a <- aggregate(WGTVICDY~V4529+ race,
          data=data.inc,
          FUN=sum,
          na.action = na.omit)



#Find crime types that disproportionately affect black victims
temp <- reshape(data=a,
                timevar="race",
                idvar = "V4529",
                direction="wide")

#crete better titles
names(temp) <- c("crime","white", "black", "aindian", "asian") 

#change n/as to 0s
temp$white[is.na(temp$white)] <- 0
temp$black[is.na(temp$black)] <- 0
temp$aindian[is.na(temp$aindian)] <- 0
temp$asian[is.na(temp$asian)] <- 0

#percentage columns 
temp$white   <- with(temp, 100*white/  sum(white))
temp$black <- with(temp, 100*black/sum(black))
temp$aindian <- with(temp, 100*aindian/sum(aindian))
temp$asian <- with(temp, 100*asian/sum(asian))

#Attempted robbery with injury from minor assault, verbal threat of sexual assault, and verbal threat of assault are disproportiantely affeting blacks 




