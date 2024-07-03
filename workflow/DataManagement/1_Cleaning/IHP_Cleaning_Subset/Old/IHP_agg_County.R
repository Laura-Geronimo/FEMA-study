## Aggregating to county level

#WORKING

#setup
getwd()
setwd('G:/My Drive/Academic/Research')

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
IHP_V10 <- read.csv('./Data/FEMA/IA/Produced/FS/PropLevel/IHP_V10_FS.csv') #2322406



##Cleaning###
names(IHP_V6)
head(IHP_V6)
range(IHP_V6$declarationDate)
IHP_V7<- IHP_V6[,c(-1)]
colSums(is.na(IHP_V7)) #none missing, but no county code

names(us_county)
head(us_county)
us_county1 <- us_county[,c(2,5,6)]
head(us_county1)
range(us_county1$STATEFP)
names(us_county1)[names(us_county1)=="STATEFP"]<-"stateFIPS"


head(state)
names(state)[names(state)=="ï..State"]<-"stateName"
names(state)[names(state)=="FIPS"]<-"stateFIPS"

##managing County names and join####
#have to do this because there is no county code in the IHP file

#joining county and state data & cleaning####
us_county2 <- left_join(us_county1, state, by="stateFIPS", copy=F)
head(us_county2)
names(us_county2)[names(us_county2)=="GEOID"]<-"countyFIPS"


#creating My_countyName in county file
head(us_county2)
names(us_county2)
us_county2$My_countyNamePR <- str_c(us_county2$NAME," Municipio,"," ",us_county2$stateAbr)
us_county2$My_countyNameLA <- str_c(us_county2$NAME," Parish,"," ",us_county2$stateAbr)
us_county2$My_countyNameOther <- str_c(us_county2$NAME," County,"," ",us_county2$stateAbr)
us_county2$My_countyName <- NA
us_county2$My_countyName <- ifelse(us_county2$stateAbr=="PR", 
                                   print(us_county2$My_countyNamePR), 
                                   print(us_county2$My_countyNameOther))
us_county2$My_countyName2 <- ifelse(us_county2$stateAbr=="LA", 
                                   print(us_county2$My_countyNameLA), 
                                   print(us_county2$My_countyName))


us_county3 <- us_county2[,c(1,2,10)]
names(us_county3)[names(us_county3)=="My_countyName2"]<-"My_countyName"
head(us_county3)

#creating My_countyName in IHP file
head(IHP_V7)
names(IHP_V7)
IHP_V7$My_countyName <- sub("\\).*","",IHP_V7$county)
IHP_V7$My_countyName <- sub("\\(","",IHP_V7$My_countyName)
IHP_V7$My_countyName <- str_c(IHP_V7$My_countyName,", ",IHP_V7$damagedStateAbbreviation)
colSums(is.na(IHP_V7))

#examining My_countyName in IHP file
ihp_My_countyName <- IHP_V7[,c(4,5,26)]
ihp_My_countyName <- distinct(ihp_My_countyName)

#ISSUE DUPLICATES####
#Issue when joining on My_countyName -
#IHP_V8 <- left_join(IHP_V7, us_county2, by="My_countyName",copy=F)  ###Generating duplicates
#colSums(is.na(IHP_V8))

##see duplicates
#dup <- subset(IHP_V8, duplicated(IHP_V8))

##Workaround attempt 1: aggregate on My_countyName and then join?####
names(IHP_V7)

#renaming some of the duplicate counties based on prior work below
IHP_V7$My_countyName[IHP_V7$My_countyName=="Baltimore, MD"] <- "Baltimore County, MD"
IHP_V7$My_countyName[IHP_V7$My_countyName=="Fairfax, VA"] <- "Fairfax County, VA"
IHP_V7$My_countyName[IHP_V7$My_countyName=="Franklin, VA"] <- "Franklin County, VA"
IHP_V7$My_countyName[IHP_V7$My_countyName=="Richmond, VA"] <- "Richmond County, VA"
IHP_V7$My_countyName[IHP_V7$My_countyName=="Roanoke, VA"] <- "Roanoke County, VA"
IHP_V7$My_countyName[IHP_V7$My_countyName=="St. Louis, MO"] <- "St. Louis County, MO"


##Counting Counties
CountCounty <-distinct(data.frame(IHP_V7$My_countyName)) #2133

###Aggregating Vars of interest by My_countyName####
##Creating unique My_countyName####

head(IHP_V7$My_countyName)
My_countyName <- data.frame(unique(IHP_V7$My_countyName)) #2133 - 
names(My_countyName)[names(My_countyName)=="unique.IHP_V7.My_countyName."] <- "My_countyName"

##examining My_countyName


#Aggregating potential vars of interest by My_countyName####
sum(IHP_V7$ihpAmountAdj)#14018466822
sum(IHP_V7$haAmountAdj) #10838650752
sum(IHP_V7$onaAmountAdj) #3179816071
sum(IHP_V7$floodDamageAmountAdj) #29419330736
sum(IHP_V7$rpfvlAdj) #24760661306
sum(IHP_V7$ppfvlAdj) #5947500567
sum(IHP_V7$rentalAssistanceAmountAdj) #3289467883
sum(IHP_V7$repairAmountAdj) #6674197323
sum(IHP_V7$replacementAmountAdj) #336952640

#Aggregating sums by My_countyName
names(IHP_V7)
IHP_AmountAdjMy_countyName <- aggregate(IHP_V7$ihpAmountAdj~IHP_V7$My_countyName, FUN=sum) 
IHP_haAmountAdjMy_countyName <- aggregate(IHP_V7$haAmountAdj~IHP_V7$My_countyName, FUN=sum)
IHP_onaAmountAdjMy_countyName <- aggregate(IHP_V7$onaAmountAdj~IHP_V7$My_countyName, FUN=sum)
IHP_fldDamAmountAdjMy_countyName <- aggregate(IHP_V7$floodDamageAmountAdj~IHP_V7$My_countyName, FUN=sum) #14111 My_countyNames
IHP_rpfvlAdjMy_countyName <- aggregate(IHP_V7$rpfvlAdj~IHP_V7$My_countyName, FUN=sum) 
IHP_ppfvlAdjMy_countyName <- aggregate(IHP_V7$ppfvlAdj~IHP_V7$My_countyName, FUN=sum) 
IHP_rentalAssistanceAmountAdjMy_countyName <- aggregate(IHP_V7$rentalAssistanceAmountAdj~IHP_V7$My_countyName, FUN=sum) 
IHP_repairAmountAdjMy_countyName <- aggregate(IHP_V7$repairAmountAdj~IHP_V7$My_countyName, FUN=sum) 
IHP_replacementAmountAdjMy_countyName <- aggregate(IHP_V7$replacementAmountAdj~IHP_V7$My_countyName, FUN=sum) 



#IHP_AmountAdjMy_countyName####
head(IHP_AmountAdjMy_countyName)
names(IHP_AmountAdjMy_countyName)[names(IHP_AmountAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_AmountAdjMy_countyName)[names(IHP_AmountAdjMy_countyName)=="IHP_V7$ihpAmountAdj"] <- "IHP_AmountAdjMy_countyName"
sum(IHP_V7$ihpAmountAdj)#14018466822
sum(IHP_AmountAdjMy_countyName$IHP_AmountAdjMy_countyName)#14018466822 good

#IHP_haAmountAdjMy_countyName####
head(IHP_haAmountAdjMy_countyName)
names(IHP_haAmountAdjMy_countyName)[names(IHP_haAmountAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_haAmountAdjMy_countyName)[names(IHP_haAmountAdjMy_countyName)=="IHP_V7$haAmountAdj"] <- "IHP_haAmountAdjMy_countyName"
sum(IHP_V7$haAmountAdj) #10838650752
sum(IHP_haAmountAdjMy_countyName$IHP_haAmountAdjMy_countyName)#10838650752 good

#IHP_onaAmountAdjMy_countyName####
head(IHP_onaAmountAdjMy_countyName)
names(IHP_onaAmountAdjMy_countyName)[names(IHP_onaAmountAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_onaAmountAdjMy_countyName)[names(IHP_onaAmountAdjMy_countyName)=="IHP_V7$onaAmountAdj"] <- "IHP_onaAmountAdjMy_countyName"
sum(IHP_V7$onaAmountAdj) #3179816071
sum(IHP_onaAmountAdjMy_countyName$IHP_onaAmountAdjMy_countyName)#3179816071 good

#IHP_fldDamAmountAdjMy_countyName####
head(IHP_fldDamAmountAdjMy_countyName)
names(IHP_fldDamAmountAdjMy_countyName)[names(IHP_fldDamAmountAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_fldDamAmountAdjMy_countyName)[names(IHP_fldDamAmountAdjMy_countyName)=="IHP_V7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjMy_countyName"
sum(IHP_V7$floodDamageAmountAdj) #29419330736
sum(IHP_fldDamAmountAdjMy_countyName$IHP_fldDamAmountAdjMy_countyName) #29419330736 good



#IHP_rpfvlAdjMy_countyName####
head(IHP_rpfvlAdjMy_countyName)
names(IHP_rpfvlAdjMy_countyName)[names(IHP_rpfvlAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_rpfvlAdjMy_countyName)[names(IHP_rpfvlAdjMy_countyName)=="IHP_V7$rpfvlAdj"] <- "IHP_rpfvlAdjMy_countyName"
sum(IHP_V7$rpfvlAdj) #24760661306
sum(IHP_rpfvlAdjMy_countyName$IHP_rpfvlAdjMy_countyName) #24760661306 good

#IHP_ppfvlAdjMy_countyName####
head(IHP_ppfvlAdjMy_countyName)
names(IHP_ppfvlAdjMy_countyName)[names(IHP_ppfvlAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_ppfvlAdjMy_countyName)[names(IHP_ppfvlAdjMy_countyName)=="IHP_V7$ppfvlAdj"] <- "IHP_ppfvlAdjMy_countyName"
sum(IHP_V7$ppfvlAdj) #5947500567
sum(IHP_ppfvlAdjMy_countyName$IHP_ppfvlAdjMy_countyName) #5947500567 good


#IHP_rentalAssistanceAmountAdjMy_countyName####
head(IHP_rentalAssistanceAmountAdjMy_countyName)
names(IHP_rentalAssistanceAmountAdjMy_countyName)[names(IHP_rentalAssistanceAmountAdjMy_countyName)=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_rentalAssistanceAmountAdjMy_countyName)[names(IHP_rentalAssistanceAmountAdjMy_countyName)=="IHP_V7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjMy_countyName"
sum(IHP_V7$rentalAssistanceAmountAdj) #3289467883
sum(IHP_rentalAssistanceAmountAdjMy_countyName$IHP_rentalAssistanceAmountAdjMy_countyName) #3289467883 good

#IHP_repairAmountAdjMy_countyName ####
head(IHP_repairAmountAdjMy_countyName )
names(IHP_repairAmountAdjMy_countyName )[names(IHP_repairAmountAdjMy_countyName )=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_repairAmountAdjMy_countyName )[names(IHP_repairAmountAdjMy_countyName )=="IHP_V7$repairAmountAdj"] <- "IHP_repairAmountAdjMy_countyName"
sum(IHP_V7$repairAmountAdj) #6674197323
sum(IHP_repairAmountAdjMy_countyName $IHP_repairAmountAdjMy_countyName ) #6674197323 good

#IHP_replacementAmountAdjMy_countyName ####
head(IHP_replacementAmountAdjMy_countyName )
names(IHP_replacementAmountAdjMy_countyName )[names(IHP_replacementAmountAdjMy_countyName )=="IHP_V7$My_countyName"] <- "My_countyName"
names(IHP_replacementAmountAdjMy_countyName )[names(IHP_replacementAmountAdjMy_countyName )=="IHP_V7$replacementAmountAdj"] <- "IHP_replacementAmountAdjMy_countyName"
sum(IHP_V7$replacementAmountAdj) #336952640
sum(IHP_replacementAmountAdjMy_countyName $IHP_replacementAmountAdjMy_countyName ) #336952640good


##Joining####
IHP_V9a <- left_join(IHP_AmountAdjMy_countyName, IHP_haAmountAdjMy_countyName, by="My_countyName", copy=FALSE)
names(IHP_V7)
IHP_V9 <- left_join(IHP_V9a,IHP_onaAmountAdjMy_countyName, by="My_countyName", copy=FALSE)
IHP_V10 <- left_join(IHP_V9, IHP_fldDamAmountAdjMy_countyName, by="My_countyName", copy=FALSE)
IHP_V11 <- left_join(IHP_V10, IHP_rpfvlAdjMy_countyName, by="My_countyName", copy=FALSE)
IHP_V12 <- left_join(IHP_V11, IHP_ppfvlAdjMy_countyName , by="My_countyName", copy=FALSE)
IHP_V13 <- left_join(IHP_V12, IHP_rentalAssistanceAmountAdjMy_countyName, by="My_countyName", copy=FALSE)
IHP_V14 <- left_join(IHP_V13, IHP_repairAmountAdjMy_countyName, by="My_countyName", copy=FALSE)
IHP_V15 <- left_join(IHP_V14, IHP_replacementAmountAdjMy_countyName, by="My_countyName", copy=FALSE)

names(IHP_V15)
colSums(is.na(IHP_V15))
head(IHP_V15) 
dim(IHP_V15)
