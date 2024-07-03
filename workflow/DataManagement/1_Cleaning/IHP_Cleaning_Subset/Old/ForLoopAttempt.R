## Attempr following this guidance:
#https://stackoverflow.com/questions/46369721/r-create-new-column-in-data-frame-based-on-conditional

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
library(USAboundaries)
library(USAboundariesData)


##Importing data####
IHP_V6 <- read.csv('./Data/FEMA/IA/Produced/FS/IHP_V6_FS.csv') #2336406
state <- read.csv('./Data/Census/StateNameAbbrCode.csv')
us_county <- read.csv('./Data/Census/Boundaries_and_CW/tl_2017_us_county/tl_2017_us_county.csv')



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
#for some reason this for loop is not working consistently
head(us_county2)
names(us_county2)
us_county2$My_countyName <- NA

for(i in 1:length(us_county2$stateAbr)){
  if(us_county2$stateAbr=="PR"){
    us_county2$My_countyName[i]<- 'PR_Test'
  }
  else if(us_county2$stateAbr=="LA"){
    us_county2$My_countyName[i]<- 'LA_Test'
  }
  else{
    us_county2$MycountyName[i]<- 'Other_test'
  }
}


for(i in 1:length(us_county2$stateAbr)){
  if(us_county2$stateAbr=="PR"){
    us_county2$My_countyName[i]<-(str_c(us_county2$NAME, " Municipio," , " ", us_county2$stateAbr))#add [i] here
  }
  else if(us_county2$stateAbr=="LA"){
    us_county2$My_countyName[i]<-(str_c(us_county2$NAME, " Parish," , " ", us_county2$stateAbr)) 
  }else{
    us_county2$My_countyName[i]<-(str_c(us_county2$NAME, " County," , " ", us_county2$stateAbr))
  }
}


us_county2$My_countyNamePR <- str_c(us_county2$NAME," Municipio,"," ",us_county2$stateAbr)
us_county2$My_countyNameLA <- str_c(us_county2$NAME," Parish,"," ",us_county2$stateAbr)
us_county2$My_countyNameOther <- str_c(us_county2$NAME," County,"," ",us_county2$stateAbr)
us_county2$My_countyName <- NA

for(i in 1:length(us_county2$My_countyName)){
  if(us_county2$stateAbr=="PR"){
    us_county2$My_countyName[i]<-us_county2$My_countyNamePR
  }
  else if(us_county2$stateAbr=="LA"){
    us_county2$My_countyName[i]=="PR"<- us_county2$My_countyNameLA #add [i] here
  }else{
    us_county2$My_countyName[i]==us_county2$My_countyNameOther#add [i] here
  }
}

head(us_county2)
colSums(is.na(us_county2))
us_county2 <- us_county2[,c(1,2,6)]
us_county3 <- distinct(us_county2)
