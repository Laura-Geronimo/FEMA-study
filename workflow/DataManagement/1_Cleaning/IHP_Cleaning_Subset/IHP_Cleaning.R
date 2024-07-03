#Exploring and cleaning IA --> IHP

#Ok to run 07/03/24 from other directory (Note: slow processing time)


getwd()
setwd('C:/Users/lgero/Box/Research/FEMA_project')

##Libraries
#install.packages(c("httr", "jsonlite"))
#install.packages("rjson")
#install.packages("httr")
#install.packages("data.table")
#install.packages("sqldf")
library(data.table)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(rjson)
library(httr)
library(sqldf)
library(tibble)



##Importing data####
IHP_V1 <- fread('./Data/Archive/IHP/IndividualsAndHouseholdsProgramValidRegistrations.csv', header=TRUE)
state <- read.csv('./Data/Edited/Census/StateNameAbbrCode.csv')


##Examining Data####
names(IHP_V1)
dim(IHP_V1)
head(IHP_V1)
range(IHP_V1$declarationDate)
table(IHP_V1$ownRent)
table(IHP_V1$primaryResidence)
table(is.na(IHP_V1$ihpAmount)) #none
?lapply


###Dropping columns####
IHP_V2 <- IHP_V1[,c(1,2,3,4,5,6,7,16,17,18,24,25,28,32,45,46,51,52,60,62,64)]
colSums(is.na(IHP_V2)) #none
head(IHP_V2)
table(IHP_V2$residenceType)
#rm(IHP_V1)


##Subsetting to Flood Damaged####
#table(IHP_V2$floodDamage) #2403116 with damage
#IHP_V3 <- subset(IHP_V2, floodDamage==1) 

##Examining numbers with flood damage >1

IHP_V3 <- subset(IHP_V2, floodDamageAmount>0)
range(IHP_V3$floodDamageAmount)
#hist(IHP_V3$floodDamageAmount)

##Examining and cleaning IHP data####
table(IHP_V3$incidentType)
IHP_V4<-subset(IHP_V3,  incidentType!="Fire"
               & incidentType!="Volcano"
               & incidentType!="Earthquake")

colSums(is.na(IHP_V4))
IHP_V5 <- IHP_V4

#formatting GZCTA
head(IHP_V5$damagedZipCode)
range(IHP_V5$damagedZipCode)
IHP_V5$GZCTA<-str_pad(IHP_V5$damagedZipCode,5, pad="0")
IHP_V5$GZCTA <- str_c('G',IHP_V5$GZCTA)
head(IHP_V5$GZCTA)
names(IHP_V5)

##Counting Counties
CountCounty <-distinct(data.frame(IHP_V5$county)) #1369


#Examining sums and adjusting for inflation####
#original sum
names(IHP_V5)
sum(IHP_V5$ihpAmount)#11904499322
sum(IHP_V5$fipAmount)#126977400
sum(IHP_V5$haAmount)#9302208045
sum(IHP_V5$onaAmount)#2602291278
sum(IHP_V5$rpfvl)#20111801274 #FEMA-determined value of disaster-caused damage to real property components, including floors, walls, access roads and bridges, electrical, plumbing, HVAC, etc. Note: IHP does not address land damage.
sum(IHP_V5$ppfvl)#4876869256 #	FEMA-determined value of disaster-caused damage to personal property components, including appliances, furniture, etc. Note: IHP does not address land damage.
sum(IHP_V5$floodDamageAmount)#23943687104 #FEMA-determined amount of damage to real and personal property due to flood damage, in U.S. dollars
sum(IHP_V5$rentalAssistanceAmount) #2772470701
sum(IHP_V5$repairAmount) #5863443866
sum(IHP_V5$replacementAmount) #262941077


#adjusting for inflation#####
##Adjusting remaining $$ for inflation####
#inflation multipliers
names(IHP_V5)
head(IHP_V5$declarationDate)
IHP_V5$DD <- substr(IHP_V5$declarationDate,1,4)
IHP_V5$DD <- as.numeric(IHP_V5$DD)
table(IHP_V5$DD)
DD <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
CPI_M <-c(1.53,1.47,1.46,1.42,1.39,1.35,1.30,1.27,1.22,1.22,1.19,1.17,1.14,1.12,1.10,1.10,1.09,1.06,1.04,1.02,1.00,0.99,0.92)
CPI_M <- data.frame(DD, CPI_M)

IHP_V6 <- left_join(IHP_V5, CPI_M, by="DD", copy=FALSE)
colSums(is.na(IHP_V6))


names(IHP_V5)
names(IHP_V6)
IHP_V6$ihpAmountAdj <- IHP_V6$ihpAmount * IHP_V6$CPI_M  #KEEP
IHP_V6$fipAmountAdj <- IHP_V6$fipAmount * IHP_V6$CPI_M
IHP_V6$haAmountAdj <- IHP_V6$haAmount * IHP_V6$CPI_M    #KEEP
IHP_V6$onaAmountAdj <- IHP_V6$onaAmount * IHP_V6$CPI_M  #KEEP
IHP_V6$rpfvlAdj <- IHP_V6$rpfvl * IHP_V6$CPI_M          #KEEP    
IHP_V6$ppfvlAdj <- IHP_V6$ppfvl * IHP_V6$CPI_M          #KEEP
IHP_V6$floodDamageAmountAdj <- IHP_V6$floodDamageAmount * IHP_V6$CPI_M  #KEEP
IHP_V6$rentalAssistanceAmountAdj <- IHP_V6$rentalAssistanceAmount * IHP_V6$CPI_M #KEEP
IHP_V6$repairAmountAdj <- IHP_V6$repairAmount * IHP_V6$CPI_M                     #KEEP
IHP_V6$replacementAmountAdj <- IHP_V6$replacementAmount * IHP_V6$CPI_M           #KEEP

colSums(is.na(IHP_V6))

##Dropping columns
names(IHP_V6)
IHP_V6b <- IHP_V6[,c(-11:-16,-18:-21)]
names(IHP_V6b)
dim(IHP_V6b)

#writing out files#####
path2 <- 'C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/IHP/Produced/FullSet/PropertyLevel'
write.csv(IHP_V6b, file.path(path2, "IHP_V6_FS.csv"), row.names=TRUE)


