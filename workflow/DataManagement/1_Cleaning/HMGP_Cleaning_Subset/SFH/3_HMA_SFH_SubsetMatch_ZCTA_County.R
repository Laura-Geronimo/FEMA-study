###Subsetting to ZCTAs and Counties that match###

##Setup####
getwd()
setwd('G:/My Drive/Academic/Research')

#Libraries####
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)

#Importing data
myHMA6 <- read.csv("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA_SFH/myHMA6.csv")
US_ZCTA <-get_acs("zcta",variables = "B25077_001", 
                  year=2019)
US_County <-get_acs("county",variables = "B25077_001", 
                  year=2019)

#Cleaning####
names(myHMA6)
myHMA6 <- myHMA6[,c(-1)]

names(US_ZCTA)
head(US_ZCTA)
US_ZCTA$GZCTA <- str_pad(US_ZCTA$GEOID,5,pad="0")
US_ZCTA$GZCTA  <- str_c('G',US_ZCTA$GZCTA )
US_ZCTA <- US_ZCTA[,c(6)]


names(US_County)
head(US_County)
US_County <- US_County[,c(1)]


##Only retaining ZCTAs that match with Census pulll####
names(myHMA6)
range(myHMA6$GZCTA)

US_ZCTA$myZflag <- 1
myHMA7 <- left_join(myHMA6,US_ZCTA, by ="GZCTA", copy=F)
names(myHMA7)
colSums(is.na(myHMA7))
table(is.na(myHMA7$myZflag)) #619
myHMA7$myZflag[is.na(myHMA7$myZflag)]<-0
table(myHMA7$myZflag)

seeMissingZips <- subset(myHMA7, myZflag==0)
table(seeMissingZips$propertyAction3) #loosing 434 Acquisitions, and 83 elevations

myHMA8 <- subset(myHMA7, myZflag==1)

##Only retaining ZCTAs that match with County Census pulll####
names(myHMA8)

####cleaning county in HMGP####
colSums(is.na(myHMA8))

names(myHMA8)
SeeCnty <- myHMA8[,c(6,7,8,9)]
head(SeeCnty)
colSums(is.na(SeeCnty))

range(SeeCnty$p2countyCode, na.rm = T) 
table(is.na(SeeCnty$p2countyCode)) #7 missing
head(SeeCnty)
SeeNAs <- SeeCnty[rowSums(is.na(SeeCnty))>0,] ##22 missing
table(SeeNAs$p2state) 
uniqueCO <- distinct(SeeNAs) #17 distinct missing. 


myHMA8$CntyID <- myHMA8$p2countyCode
table(is.na(myHMA8$CntyID)) #7
myHMA8$CntyID <- str_pad(myHMA8$CntyID , 3, pad="0")
head(myHMA8$CntyID)


head(myHMA8$stateNumberCode)
table(is.na(myHMA8$stateNumberCode)) #15
range(myHMA8$stateNumberCode)
myHMA8$stateNumberCode <- str_pad(myHMA8$stateNumberCode, 2, pad="0")

myHMA8$CntyID <- str_c(myHMA8$stateNumberCode , myHMA8$CntyID)
range(myHMA8$CntyID, na.rm=T)

head(myHMA8$CntyID)

table(is.na(myHMA8$CntyID)) #22 missing

###only retaining counties that match us pull###
names(US_County)
US_County$myCflag <- 1
head(US_County)
names(US_County)[names(US_County)=="GEOID"] <- "CntyID"
myHMA9 <- left_join(myHMA8,US_County, by ="CntyID", copy=F)
names(myHMA9)
colSums(is.na(myHMA9))
table(is.na(myHMA9$myCflag)) #1048
myHMA9$myCflag [is.na(myHMA9$myCflag )]<-0
table(myHMA9$myCflag )

seeMissingCnty <- subset(myHMA9, myCflag==0 | CntyID=="01000")
table(seeMissingCnty$propertyAction3) #loosing 100 elevations, 329 acquisitions, 
names(seeMissingCnty)
seeMissingCnty2 <- seeMissingCnty[,c(6,7,8,9)]
seeMissingCnty2 <- distinct(seeMissingCnty2) #18

#checking which have most that are feasible to interpolate
names(seeMissingCnty)
seeMissingCnty3 <- seeMissingCnty[,c("county","p2county","p2countyCode","stateNumberCode","p2state","propertyAction3")]
colSums(is.na(seeMissingCnty3))
seeMissingCnty3$p2county[seeMissingCnty3$p2county=="Statewide"]<-NA
seeMissingCnty3$county[seeMissingCnty3$county==""]<-NA
seeMissingCnty3$p2countyCode[seeMissingCnty3$p2countyCode==0]<-NA

table(seeMissingCnty3$propertyAction3)
seeMissingCnty3$Elev <- 0
seeMissingCnty3$Elev[seeMissingCnty3$propertyAction3=="Elevation"]<-1
seeMissingCnty3$Acqui <- 0
seeMissingCnty3$Acqui[seeMissingCnty3$propertyAction3=="Acquisition"]<-1
table(seeMissingCnty3$Acqui)
table(seeMissingCnty3$Elev)

#subsetting to elevs and acquis
interp1 <- subset(seeMissingCnty3, Elev==1 | Acqui==1)
sum(interp1$Elev)
sum(interp1$Acqui)

#Aggregating to see where most are
head(interp1)
colSums(is.na(interp1))
names(interp1)
interp2 <- interp1[,c("county","p2state","Acqui","Elev")]
head(interp2)
interp3<- na.omit(interp2)
interp3$cntySt <- str_c(interp3$county, interp3$p2state)
MissingElev <- aggregate(interp3$Elev~interp3$cntySt, FUN=sum) #2 over 10 
MissingAcqui <- aggregate(interp3$Acqui~interp3$cntySt, FUN=sum) #7 over 10

#collecting names
names(myHMA9)
cntyNames <- myHMA9[,c("CntyID","county","p2county","p2countyCode","stateNumberCode", "p2state" )]
cntyNames <- distinct(cntyNames)

#Interpolating missing cnty ids into my CntyID for rejoin####
myHMA8b <- myHMA8
colSums(is.na(myHMA8b))
head(myHMA8b$CntyID)

table(is.na(myHMA8b$CntyID)) #22 missing

#Grabbing CntyID for places with over 10 elevations, using cntyNames file
myHMA8b$CntyID2 <- 0
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Livingston", "22063", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Alaska" & myHMA8b$county=="Wade Hampton (CA)", "02270", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Alaska" & myHMA8b$county=="Yukon-Koyukuk (CA)", "02290", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Mississippi" & myHMA8b$county=="Harrison", "28047", myHMA8b$CntyID2)

#additional from prior analyis
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Orleans", "22071", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Jefferson", "22051", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="St. Tammany", "22103", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Terrebonne", "22109", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Terrebonne", "22109", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="St. Bernard", "22107", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Jefferson Davis", "22053", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Vermilion", "22113", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="Iberia", "22045", myHMA8b$CntyID2)

table(myHMA8b$CntyID2)

#Grabbing CntyID for places with over 10 Acquisitions, using cntyNames file
#pulling PR from here: https://www.cccarto.com/fipscodes/puertorico/
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="New Jersey" & myHMA8b$county=="Middlesex", "34023", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="New Jersey" & myHMA8b$county=="Union", "34039", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="New Jersey" & myHMA8b$county=="Bergen", "34003", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="New Jersey" & myHMA8b$county=="Cumberland", "34011", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="New Jersey" & myHMA8b$county=="Monmouth", "34025", myHMA8b$CntyID2)


myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Louisiana" & myHMA8b$county=="E. Baton Rouge", "22033", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Illinois" & myHMA8b$county=="Menard", "17129", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="New York" & myHMA8b$county=="Delaware", "36025", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Virginia" & myHMA8b$county=="Rockbridge", "51163", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Kentucky" & myHMA8b$county=="Pendleton", "21191", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Pennsylvania" & myHMA8b$county=="Lycoming", "42081", myHMA8b$CntyID2)
#myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Illinois" & myHMA8b$county=="De Witt", "17129", myHMA8b$CntyID2)


myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Puerto Rico" & myHMA8b$county=="Patillas", "72000", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Puerto Rico" & myHMA8b$county=="Fajardo", "72053", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Puerto Rico" & myHMA8b$county=="Ponce", "72113", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Puerto Rico" & myHMA8b$county=="Aguas Buenas", "72007", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Puerto Rico" & myHMA8b$county=="Jayuya", "72073", myHMA8b$CntyID2)
myHMA8b$CntyID2 <- ifelse(myHMA8b$p2state=="Puerto Rico" & myHMA8b$county=="San Lorenze", "72000", myHMA8b$CntyID2)


table(myHMA8b$CntyID) 
table(is.na(myHMA8b$CntyID)) #1436 NAs --> converting to 0s
myHMA8b$CntyID[myHMA8b$CntyID==0 | myHMA8b$CntyID=="01000"]<-0
myHMA8b$CntyID[is.na(myHMA8b$CntyID)]<-0

table(is.na(myHMA8b$p2countyCode)) #converting to 0s
myHMA8b$p2countyCode[is.na(myHMA8b$p2countyCode)]<-0

myHMA8b$CntyID3 <- ifelse(myHMA8b$p2countyCode==0, myHMA8b$CntyID2,myHMA8b$CntyID)
table(myHMA8b$CntyID3) #Now 33 0s --> converting back to NAs
myHMA8b$CntyID3[myHMA8b$CntyID3==0]<-NA
table(is.na(myHMA8b$CntyID3)) #38


myHMA8b$CntyID <- myHMA8b$CntyID3
table(myHMA8b$CntyID) 
table(is.na(myHMA8b$CntyID))

seeNAs <- myHMA8b[is.na(myHMA8b$CntyID),]
table(seeNAs$propertyAction3) #33 buyouts, 5 elevations

#rejoining based on new CntyIDs ####
head(US_County)
class(US_County$CntyID)
head(myHMA8b$CntyID)
class(myHMA8b$CntyID)

myHMA9b <- left_join(myHMA8b,US_County, by="CntyID", copy=F)
names(myHMA9b)
colSums(is.na(myHMA9b))
table(is.na(myHMA9b$myCflag)) #220 
myHMA9b$myCflag[is.na(myHMA9b$myCflag )]<-0
table(myHMA9b$myCflag )

seeMissingCnty <- subset(myHMA9b, myCflag ==0)
table(seeMissingCnty$propertyAction3) #now only missing 45 buyouts, 37 elevs

###############################

#Subsetting to counties that match ACS geographies####

myHMA9c <- subset(myHMA9b, myCflag==1)
names(myHMA9c)
table(myHMA9c$propertyAction3) #now have 7494 elevations, 20838  buyouts
table(myHMA9c$p2state)

#column reduction
names(myHMA9c)
myHMA10 <- myHMA9c %>% 
  select(-CntyID2, -CntyID3, -myZflag, -myCflag) #taking out CntyID2, CntyID3, myCFlag
names(myHMA10)
colSums(is.na(myHMA10))
head(myHMA10)

##Writing out files
path1 <- ("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA")
write.csv(myHMA10, file.path(path1, "myHMA10.csv"), row.names=TRUE)







