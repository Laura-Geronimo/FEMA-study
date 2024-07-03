## Aggregating to county level


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
names(state)[names(state)=="�..State"]<-"stateName"
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
us_county2$My_countyName <- str_c(us_county2$NAME," County,"," ",us_county2$stateAbr)
colSums(is.na(us_county2))
us_county2 <- us_county2[,c(1,2,6)]
us_county3 <- distinct(us_county2)

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
IHP_V8 <- left_join(IHP_V7, us_county2, by="My_countyName",copy=F)  ###Generating duplicates
colSums(is.na(IHP_V8))

##see duplicates
dup <- subset(IHP_V8, duplicated(IHP_V8))

##Workaround attempt 1: aggregate on My_countyName and then join?####



#Fixing us_county database to match names in IHP database (or vice versa - depending)
us_county2$My_countyName[us_county2$My_countyName=="Kenai Peninsula County, AK"] <- "Kenai Peninsula Borough, AK"
us_county2$My_countyName[us_county2$My_countyName=="Kodiak Island County, AK"] <- "Kodiak Island Borough, AK"
us_county2$My_countyName[us_county2$My_countyName=="Kodiak Island County, AK"] <- "Kodiak Island Borough, AK"

IHP_V8$My_countyName[IHP_V8$My_countyName=="Norton, VA"] <- "Norton County, VA"

#post test
IHP_V9 <- left_join(IHP_V8, us_county2, by="My_countyName",copy=F)
colSums(is.na(IHP_V9))

#see NAs####
seeNAs <- IHP_V9[rowSums(is.na(IHP_V9))>0,]
names(seeNAs)
seeNAs <- seeNAs[,c(4,5,26)]
seeNAs <- distinct(seeNAs) #214

#####################################################################

##Integrating cnty Data from ZCTAinCSC (GIS produced)####
#calling these My for missing - my baseline
names(ZCTAinCSC)
ZCTAinCSC <- ZCTAinCSC[,c(-5,-6)]
names(ZCTAinCSC)[names(ZCTAinCSC)=="�..GZCTA"]<-"GZCTA"
names(ZCTAinCSC)[names(ZCTAinCSC)=="cntyfips"]<-"My_cntyfips"
names(ZCTAinCSC)[names(ZCTAinCSC)=="cntyname"]<-"My_cntyname"
names(ZCTAinCSC)[names(ZCTAinCSC)=="st_fips"]<-"My_st_fips"
names(ZCTAinCSC)[names(ZCTAinCSC)=="st_abbr"]<-"My_st_abbr"
names(ZCTAinCSC)[names(ZCTAinCSC)=="ctystate"]<-"My_ctystate"
names(ZCTAinCSC)[names(ZCTAinCSC)=="st_name"]<-"My_st_name"
IHP_Cnty1 <- left_join(IHP_V9, ZCTAinCSC, by="GZCTA", copy=FALSE)

#analyzing missing data on cntyFIPs
colSums(is.na(IHP_Cnty1)) #missing 486251
names(IHP_Cnty1)
seeNAs1 <- IHP_Cnty1[,c(8,21,23)]
seeNAs2 <- seeNAs1[rowSums(is.na(seeNAs1))>0,]
seeNAs2 <- distinct(seeNAs2) #1596 GZCTAs missing county data
seeNAs3 <- IHP_Cnty1[,c(21,23)]
seeNAs3 <- distinct(seeNAs3) #249 distinct county states

##Seeking to pull in data by using original 'county' as reference
  #and matching to cnty state names in AllCSC data


##Creating new cnty names to join in CSC data by name####
names(IHP_Cnty1)
IHP_Cnty2 <- IHP_Cnty1
head(IHP_Cnty2)
IHP_Cnty2$My_countyName <- sub("\\).*","",IHP_Cnty2$county)
IHP_Cnty2$My_countyName2 <- sub("\\(","",IHP_Cnty2$My_countyName)
IHP_Cnty2$My_countyName2 <- str_c(IHP_Cnty2$My_countyName2,","," ",IHP_Cnty2$damagedStateAbbreviation)
IHP_Cnty2$My_countyName <- IHP_Cnty2$My_countyName2
head(IHP_Cnty2)
colSums(is.na(IHP_Cnty2))
names(IHP_Cnty2)
IHP_Cnty2 <- IHP_Cnty2[,c(1:27)]


#now joining data from AllCSCs based on this My_countyName####
  #so this will be a second join of the same data. Calling is M1 for missing
names(AllCSCs)
head(AllCSCs)
names(AllCSCs)[names(AllCSCs)=="�..cnty_fips"]<-"M1_cnty_fips"
names(AllCSCs)[names(AllCSCs)=="cntyname"]<-"M1_cntyname"
names(AllCSCs)[names(AllCSCs)=="st_fips"]<-"M1_st_fips"
names(AllCSCs)[names(AllCSCs)=="st_abbr"]<-"M1_st_abbr"
names(AllCSCs)[names(AllCSCs)=="ctystate"]<-"M1_ctystate"
names(AllCSCs)[names(AllCSCs)=="st_name"]<-"M1_st_name"

#fixing counties
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Mayagüez Municipio"] <- "Mayaguez Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Añasco Municipio"] <- "Anasco Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Bayamón Municipio"] <- "Bayamon Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Río Grande Municipio"] <- "Rio Grande Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Loíza Municipio"] <- "Loiza Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Cataño Municipio"] <- "Catano Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Guánica Municipio"] <- "Guanica Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Juana Díaz Municipio"] <- "Juana Diaz Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Manatí Municipio"] <- "Manati Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Peñuelas Municipio"] <- "Penuelas Municipio"
AllCSCs$M1_cntyname[AllCSCs$M1_cntyname=="Rincón Municipio"] <- "Rincon Municipio"



#creating My_countyName for join. Ideally, it would bring in all the leftovers,
  #but there is sure to be missing data
AllCSCs$My_countyName <- str_c(AllCSCs$M1_cntyname,","," ",AllCSCs$M1_st_abbr)
head(AllCSCs)


#joining and original examination 
IHP_cnty3 <- left_join(IHP_Cnty2, AllCSCs, by="My_countyName",copy=FALSE)
colSums(is.na(IHP_cnty3)) #15673 still missing
names(IHP_cnty3)

#examining remaining NAs
sum(IHP_cnty3$floodDamageAmount) #22728139396
seeNAs4 <- IHP_cnty3[is.na(IHP_cnty3$M1_cnty_fips),] #
seeNAs5 <- seeNAs4[is.na(seeNAs4$My_cntyfips),] 
seeNAs6 <- seeNAs5[rowSums(is.na(seeNAs5)) > 0, ] 
sum(seeNAs6$floodDamageAmountAdj) #129458056
names(seeNAs5)
seeNAs7 <- seeNAs5[,c(4,5,22,23)]
seeNAs7 <-distinct(seeNAs7) #122

names(IHP_cnty3)

#Manually integrating 133 counties missing, based on AllCSCs data and matching
#focused on counties or zips with +10
IHP_Cnty2a<- IHP_Cnty2
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Acadia (Parish)" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Orleans Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Adams (County)" & IHP_Cnty2a$damagedStateAbbreviation=="MS"] <- "Jackson County, MS"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Adjuntas (Municipio)"] <- "Anasco Municipio, PR"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Alexandria" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Alexandria city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Allen (Parish)" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Jefferson Davis Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Baker (County)" & IHP_Cnty2a$damagedStateAbbreviation=="FL"] <- "Duval County, FL"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Baltimore" & IHP_Cnty2a$damagedCity=="BALTIMORE" & IHP_Cnty2a$damagedStateAbbreviation=="MD"] <- "Baltimore city, MD"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Baltimore" & IHP_Cnty2a$damagedCity=="BROOKLYN" & IHP_Cnty2a$damagedStateAbbreviation=="MD"] <- "Baltimore city, MD"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Baltimore" & IHP_Cnty2a$damagedCity=="DUNDALK" & IHP_Cnty2a$damagedStateAbbreviation=="MD"] <- "Baltimore County, MD"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Baltimore" & IHP_Cnty2a$damagedCity=="BROOKLYN PARK" & IHP_Cnty2a$damagedStateAbbreviation=="MD"] <- "Baltimore city, MD"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Bamberg (County)" & IHP_Cnty2a$damagedStateAbbreviation=="SC"] <- "Colleton County, SC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Beauregard (Parish)" & IHP_Cnty2a$damagedCity=="DEQUINCY" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Calcasieu Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Beauregard (Parish)" & IHP_Cnty2a$damagedCity=="STARKS" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Calcasieu Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Bee (County)" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "San Patricio County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Bladen (County)" & IHP_Cnty2a$damagedStateAbbreviation=="NC"] <- "Pender County, NC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Bucks (County)" & IHP_Cnty2a$damagedStateAbbreviation=="PA"] <- "Philadelphia County, PA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Bradford (County)" & IHP_Cnty2a$damagedStateAbbreviation=="FL"] <- "Clay County, FL"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Canovanas (Municipio)" & IHP_Cnty2a$damagedCity=="LOIZA" & IHP_Cnty2a$damagedStateAbbreviation=="PR"] <- "Loiza Municipio, PR"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Chesapeake" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Chesapeake city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="District of Columbia (County-equivalent)" & IHP_Cnty2a$damagedStateAbbreviation=="DC"] <- "District of Columbia, DC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hampton" & IHP_Cnty2a$damagedCity=="HAMPTON" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Hampton city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hampton" & IHP_Cnty2a$damagedCity=="POQUOSON" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Portsmouth city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hampton" & IHP_Cnty2a$damagedCity=="ALEXANDRIA" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Alexandria city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hampton" & IHP_Cnty2a$damagedCity=="NORFOLK" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Norfolk city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hampton" & IHP_Cnty2a$damagedCity=="NEWPORT NEWS" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Newport News city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hopewell" & IHP_Cnty2a$damagedCity=="HOPEWELL" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Hopewell city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hopewell" & IHP_Cnty2a$damagedCity=="RICHMOND" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Richmond County, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Hopewell" & IHP_Cnty2a$damagedCity=="CHARLES CITY" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Charles City County, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Newport News" & IHP_Cnty2a$damagedCity=="NEWPORT NEWS" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Newport News city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Norfolk" & IHP_Cnty2a$damagedCity=="NORFOLK" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Norfolk city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Poquoson" & IHP_Cnty2a$damagedCity=="POQUOSON" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Poquoson city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Portsmouth" & IHP_Cnty2a$damagedCity=="PORTSMOUTH" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Portsmouth city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Richmond" & IHP_Cnty2a$damagedCity=="RICHMOND" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Richmond County, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Suffolk" & IHP_Cnty2a$damagedCity=="SUFFOLK" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Suffolk city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Virginia Beach" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Virginia Beach city, VA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$county=="Williamsburg" & IHP_Cnty2a$damagedStateAbbreviation=="VA"] <- "Williamsburg city, VA"


#Specific ZCTA search / approximation / primary county:
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G32666" & IHP_Cnty2a$damagedStateAbbreviation=="FL"] <- "Putnam County, FL"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G00926" & IHP_Cnty2a$damagedStateAbbreviation=="PR"] <- "San Juan Municipio, PR"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G00627" & IHP_Cnty2a$damagedStateAbbreviation=="PR"] <- "Camuy Municipio, PR"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G00745" & IHP_Cnty2a$damagedStateAbbreviation=="PR"] <- "Rio Grande Municipio, PR"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70115" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Orleans Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70114" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Orleans Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70117" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Orleans Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70118" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Orleans Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70129" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Orleans Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70053" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Jefferson Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70734" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Ascension Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70769" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Ascension Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70726" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Livingston Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70449" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Livingston Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G12571" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Dutchess County, NY"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77082" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77083" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77085" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77089" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77099" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77093" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77450" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77493" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77447" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Harris County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77430" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Brazoria County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77583" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Brazoria County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77632" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Orange County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G12480" & IHP_Cnty2a$damagedStateAbbreviation=="NY"] <- "Ulster County, NY"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G27828" & IHP_Cnty2a$damagedStateAbbreviation=="NC"] <- "Pitt County, NC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G28513" & IHP_Cnty2a$damagedStateAbbreviation=="NC"] <- "Pitt County, NC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G28530" & IHP_Cnty2a$damagedStateAbbreviation=="NC"] <- "Pitt County, NC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G08853" & IHP_Cnty2a$damagedStateAbbreviation=="NJ"] <- "Somerset County, NJ"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G08876" & IHP_Cnty2a$damagedStateAbbreviation=="NJ"] <- "Somerset County, NJ"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G77662" & IHP_Cnty2a$damagedStateAbbreviation=="TX"] <- "Orange County, TX"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G99557" & IHP_Cnty2a$damagedStateAbbreviation=="AK"] <- "Bethel Census Area, AK"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70517" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "St. Martin Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70582" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "St. Martin Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70555" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Vermilion Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70528" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Vermilion Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70563" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Iberia Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G29581" & IHP_Cnty2a$damagedStateAbbreviation=="SC"] <- "Horry County, SC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G29436" & IHP_Cnty2a$damagedStateAbbreviation=="SC"] <- "Berkeley County, SC"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G70422" & IHP_Cnty2a$damagedStateAbbreviation=="LA"] <- "Tangipahoa Parish, LA"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G12725" & IHP_Cnty2a$damagedStateAbbreviation=="NY"] <- "Ulster County, NY"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G12566" & IHP_Cnty2a$damagedStateAbbreviation=="NY"] <- "Ulster County, NY"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G12483" & IHP_Cnty2a$damagedStateAbbreviation=="NY"] <- "Ulster County, NY"
IHP_Cnty2a$My_countyName[IHP_Cnty2a$GZCTA=="G10940" & IHP_Cnty2a$damagedStateAbbreviation=="NY"] <- "Orange County, NY"



#rerunning and testing integration
IHP_cnty4 <- left_join(IHP_Cnty2a, AllCSCs, by="My_countyName",copy=FALSE)
colSums(is.na(IHP_cnty4))

#examining remaining NAs
seeNAs4 <- IHP_cnty4[is.na(IHP_cnty4$M1_cnty_fips),] #
seeNAs5 <- seeNAs4[is.na(seeNAs4$My_cntyfips),] 
seeNAs6 <- seeNAs5[rowSums(is.na(seeNAs5)) > 0, ] 
names(seeNAs5)
table(seeNAs6$county) # targeting counties with over 20 missing
seeNAs7 <- seeNAs5[,c(4,5,22,23)]
seeNAs7 <-distinct(seeNAs7) #133 --> 90

##Some counties I could not match to a CSC - appears they are not in the CSC database...
  #Colonial Heights - 18
  #Delaware (county) - 84
  #Mercer (county) - 26
  #Montgomery (County) -144
  #Williamsburg (county) SC = 263
#Total of 90 distinct counties and 922 obs still missing county data - these will be dropped.
sum(seeNAs6$floodDamageAmount) #6637738
##
names(IHP_cnty4)
colSums(is.na(IHP_cnty4)) #just missing 1887 now


####Pulling over M1 data where missing in My data####
IHP_cnty5 <- IHP_cnty4
names(IHP_cnty5)
head(IHP_cnty5)
colSums(is.na(IHP_cnty5))
IHP_cnty5$My_cntyfipsF <-IHP_cnty5$My_cntyfips
IHP_cnty5$My_cntyfipsF[is.na(IHP_cnty5$My_cntyfipsF)]<-999999
IHP_cnty5$My_cntyfipsF <- ifelse(IHP_cnty5$My_cntyfipsF==999999, print(IHP_cnty5$M1_cnty_fips), print(IHP_cnty5$My_cntyfips))
IHP_cnty5$My_cntyfipsF[IHP_cnty5$My_cntyfipsF==999999]<- NA
compare1 <- IHP_cnty5[,c(21,28,34)]

IHP_cnty5$My_cntynameF <-IHP_cnty5$My_cntyname
IHP_cnty5$My_cntynameF[is.na(IHP_cnty5$My_cntynameF)]<-999999
IHP_cnty5$My_cntynameF <- ifelse(IHP_cnty5$My_cntynameF==999999, print(IHP_cnty5$M1_cntyname), print(IHP_cnty5$My_cntyname))
IHP_cnty5$My_cntynameF[IHP_cnty5$My_cntynameF==999999]<- NA
compare1 <- IHP_cnty5[,c(17,23,29)]

IHP_cnty5$My_st_fipsF <-IHP_cnty5$My_st_fips
IHP_cnty5$My_st_fipsF[is.na(IHP_cnty5$My_st_fipsF)]<-999999
IHP_cnty5$My_st_fipsF <- ifelse(IHP_cnty5$My_st_fipsF==999999, print(IHP_cnty5$M1_st_fips), print(IHP_cnty5$My_st_fips))
IHP_cnty5$My_st_fipsF[IHP_cnty5$My_st_fipsF==999999]<- NA

IHP_cnty5$My_st_abbrF <-IHP_cnty5$My_st_abbr
IHP_cnty5$My_st_abbrF[is.na(IHP_cnty5$My_st_abbrF)]<-999999
IHP_cnty5$My_st_abbrF <- ifelse(IHP_cnty5$My_st_abbrF==999999, print(IHP_cnty5$M1_st_abbr), print(IHP_cnty5$My_st_abbr))
IHP_cnty5$My_st_abbrF[IHP_cnty5$My_st_abbrF==999999]<- NA

IHP_cnty5$My_ctystateF <-IHP_cnty5$My_ctystate
IHP_cnty5$My_ctystateF[is.na(IHP_cnty5$My_ctystateF)]<-999999
IHP_cnty5$My_ctystateF <- ifelse(IHP_cnty5$My_ctystateF==999999, print(IHP_cnty5$M1_ctystate), print(IHP_cnty5$My_ctystate))
IHP_cnty5$My_ctystateF[IHP_cnty5$My_ctystateF==999999]<- NA

IHP_cnty5$My_ctystateF <-IHP_cnty5$My_ctystate
IHP_cnty5$My_ctystateF[is.na(IHP_cnty5$My_ctystateF)]<-999999
IHP_cnty5$My_ctystateF <- ifelse(IHP_cnty5$My_ctystateF==999999, print(IHP_cnty5$M1_ctystate), print(IHP_cnty5$My_ctystate))
IHP_cnty5$My_ctystateF[IHP_cnty5$My_ctystateF==999999]<- NA

IHP_cnty5$My_st_nameF <-IHP_cnty5$My_st_name
IHP_cnty5$My_st_nameF[is.na(IHP_cnty5$My_st_nameF)]<-999999
IHP_cnty5$My_st_nameF <- ifelse(IHP_cnty5$My_st_nameF==999999, print(IHP_cnty5$M1_st_name), print(IHP_cnty5$My_st_name))
IHP_cnty5$My_st_nameF[IHP_cnty5$My_st_nameF==999999]<- NA

##Reducing to final data and renaming vars####
names(IHP_cnty5)
head(IHP_cnty5)
colSums(is.na(IHP_cnty5))
IHP_cnty6 <- IHP_cnty5[,c(1:20,27,34:39)]
head(IHP_cnty6)
colSums(is.na(IHP_cnty6))
sum(IHP_cnty6$rpfvlAdj)


table(IHP_cnty6$floodDamage)
table(IHP_cnty6$incidentType) #only 1 Severe Ice Storm - recoding to Severe Storm(s)
IHP_cnty6$incidentType[IHP_cnty6$incidentType=="Severe Ice Storm"]<-"Severe Storm(s)"


##Dropping missing data####
IHP_cnty7 <- na.omit(IHP_cnty6)

###AGG to COUNTYDN (CDN) level####
sum(IHP_cnty7$ihpAmountAdj)#8998196816
sum(IHP_cnty7$haAmountAdj) #6689626986
sum(IHP_cnty7$onaAmountAdj) #2308569830
sum(IHP_cnty7$floodDamageAmountAdj) #22721501658
sum(IHP_cnty7$rpfvlAdj) #19328944434
sum(IHP_cnty7$ppfvlAdj) #4406460970
sum(IHP_cnty7$rentalAssistanceAmountAdj) #2471945632
sum(IHP_cnty7$repairAmountAdj) #3440673556
sum(IHP_cnty7$replacementAmountAdj) #258706656

#creating CntyID
head(IHP_cnty7$My_cntyfipsF)
range(IHP_cnty7$My_cntyID, na.rm=T)
IHP_cnty7$My_cntyID <- IHP_cnty7$My_cntyfipsF
IHP_cnty7$My_cntyID<- str_pad(IHP_cnty7$My_cntyID,5, pad="0")
IHP_cnty7$My_cntyID <- str_c('C',IHP_cnty7$My_cntyID)
head(IHP_cnty7$My_cntyID)

##Creating unique CDN####
range(IHP_cnty7$disasterNumber)
IHP_cnty7$CDN <- str_c(IHP_cnty7$disasterNumber,IHP_cnty7$My_cntyID)
head(IHP_cnty7$CDN, rm.na=T)
CDN <- data.frame(unique(IHP_cnty7$CDN)) #1089 -  counties impacted by flood events
names(CDN)
head(CDN)
names(CDN)[names(CDN)=="unique.IHP_cnty7.CDN."] <- "CDN"

###Aggregating Flood Damage by CDN####
names(IHP_cnty7)
IHP_AmountAdjCDN <- aggregate(IHP_cnty7$ihpAmountAdj~IHP_cnty7$CDN, FUN=sum) 
IHP_haAmountAdjCDN <- aggregate(IHP_cnty7$haAmountAdj~IHP_cnty7$CDN, FUN=sum)
IHP_onaAmountAdjCDN <- aggregate(IHP_cnty7$onaAmountAdj~IHP_cnty7$CDN, FUN=sum)
IHP_fldDamAmountAdjCDN <- aggregate(IHP_cnty7$floodDamageAmountAdj~IHP_cnty7$CDN, FUN=sum) #14111 CDNs
IHP_rpfvlAdjCDN <- aggregate(IHP_cnty7$rpfvlAdj~IHP_cnty7$CDN, FUN=sum) 
IHP_ppfvlAdjCDN <- aggregate(IHP_cnty7$ppfvlAdj~IHP_cnty7$CDN, FUN=sum) 
IHP_rentalAssistanceAmountAdjCDN <- aggregate(IHP_cnty7$rentalAssistanceAmountAdj~IHP_cnty7$CDN, FUN=sum) 
IHP_repairAmountAdjCDN <- aggregate(IHP_cnty7$repairAmountAdj~IHP_cnty7$CDN, FUN=sum) 
IHP_replacementAmountAdjCDN <- aggregate(IHP_cnty7$replacementAmountAdj~IHP_cnty7$CDN, FUN=sum) 


#IHP_AmountAdjCDN####
head(IHP_AmountAdjCDN)
names(IHP_AmountAdjCDN)[names(IHP_AmountAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_AmountAdjCDN)[names(IHP_AmountAdjCDN)=="IHP_cnty7$ihpAmountAdj"] <- "IHP_AmountAdjCDN"
sum(IHP_AmountAdjCDN$IHP_AmountAdjCDN)#8998196816 good

#IHP_haAmountAdjCDN####
head(IHP_haAmountAdjCDN)
names(IHP_haAmountAdjCDN)[names(IHP_haAmountAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_haAmountAdjCDN)[names(IHP_haAmountAdjCDN)=="IHP_cnty7$haAmountAdj"] <- "IHP_haAmountAdjCDN"
sum(IHP_haAmountAdjCDN$IHP_haAmountAdjCDN)#6689626986 good

#IHP_onaAmountAdjCDN####
head(IHP_onaAmountAdjCDN)
names(IHP_onaAmountAdjCDN)[names(IHP_onaAmountAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_onaAmountAdjCDN)[names(IHP_onaAmountAdjCDN)=="IHP_cnty7$onaAmountAdj"] <- "IHP_onaAmountAdjCDN"
sum(IHP_onaAmountAdjCDN$IHP_onaAmountAdjCDN)#2308569830 good

#IHP_fldDamAmountAdjCDN####
head(IHP_fldDamAmountAdjCDN)
names(IHP_fldDamAmountAdjCDN)[names(IHP_fldDamAmountAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_fldDamAmountAdjCDN)[names(IHP_fldDamAmountAdjCDN)=="IHP_cnty7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjCDN"
sum(IHP_fldDamAmountAdjCDN$IHP_fldDamAmountAdjCDN) #22721501658 good

#IHP_rpfvlAdjCDN####
head(IHP_rpfvlAdjCDN)
names(IHP_rpfvlAdjCDN)[names(IHP_rpfvlAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_rpfvlAdjCDN)[names(IHP_rpfvlAdjCDN)=="IHP_cnty7$rpfvlAdj"] <- "IHP_rpfvlAdjCDN"
sum(IHP_rpfvlAdjCDN$IHP_rpfvlAdjCDN) #19328944434good

#IHP_ppfvlAdjCDN####
head(IHP_ppfvlAdjCDN)
names(IHP_ppfvlAdjCDN)[names(IHP_ppfvlAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_ppfvlAdjCDN)[names(IHP_ppfvlAdjCDN)=="IHP_cnty7$ppfvlAdj"] <- "IHP_ppfvlAdjCDN"
sum(IHP_ppfvlAdjCDN$IHP_ppfvlAdjCDN) #4406460970 good


#IHP_rentalAssistanceAmountAdjCDN####
head(IHP_rentalAssistanceAmountAdjCDN)
names(IHP_rentalAssistanceAmountAdjCDN)[names(IHP_rentalAssistanceAmountAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_rentalAssistanceAmountAdjCDN)[names(IHP_rentalAssistanceAmountAdjCDN)=="IHP_cnty7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjCDN"
sum(IHP_rentalAssistanceAmountAdjCDN$IHP_rentalAssistanceAmountAdjCDN) #2471945632 good

#IHP_repairAmountAdjCDN ####
head(IHP_repairAmountAdjCDN )
names(IHP_repairAmountAdjCDN )[names(IHP_repairAmountAdjCDN )=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_repairAmountAdjCDN )[names(IHP_repairAmountAdjCDN )=="IHP_cnty7$repairAmountAdj"] <- "IHP_repairAmountAdjCDN"
sum(IHP_repairAmountAdjCDN $IHP_repairAmountAdjCDN ) #34406735560 good

#IHP_replacementAmountAdjCDN ####
head(IHP_replacementAmountAdjCDN)
names(IHP_replacementAmountAdjCDN )[names(IHP_replacementAmountAdjCDN)=="IHP_cnty7$CDN"] <- "CDN"
names(IHP_replacementAmountAdjCDN )[names(IHP_replacementAmountAdjCDN)=="IHP_cnty7$replacementAmountAdj"] <- "IHP_replacementAmountAdjCDN"
sum(IHP_replacementAmountAdjCDN$IHP_replacementAmountAdjCDN) #258706656 good

##Joining####
IHP_CDN1 <- left_join(IHP_AmountAdjCDN, IHP_haAmountAdjCDN, by="CDN", copy=FALSE)
names(IHP_cnty7)
IHP_CDN2 <- left_join(IHP_CDN1,IHP_onaAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN3 <- left_join(IHP_CDN2, IHP_fldDamAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN4 <- left_join(IHP_CDN3, IHP_rpfvlAdjCDN, by="CDN", copy=FALSE)
IHP_CDN5 <- left_join(IHP_CDN4, IHP_ppfvlAdjCDN , by="CDN", copy=FALSE)
IHP_CDN6 <- left_join(IHP_CDN5, IHP_rentalAssistanceAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN7 <- left_join(IHP_CDN6, IHP_repairAmountAdjCDN, by="CDN", copy=FALSE)
IHP_CDN8 <- left_join(IHP_CDN7, IHP_replacementAmountAdjCDN, by="CDN", copy=FALSE)

names(IHP_CDN8)
colSums(is.na(IHP_CDN8))
head(IHP_CDN8) 
dim(IHP_CDN8) #1088




#####AGG to COUNTY level####


###Aggregating Flood Damage by My_cntyID####
names(IHP_cnty7)
IHP_AmountAdjMy_cntyID <- aggregate(IHP_cnty7$ihpAmountAdj~IHP_cnty7$My_cntyID, FUN=sum) 
IHP_haAmountAdjMy_cntyID <- aggregate(IHP_cnty7$haAmountAdj~IHP_cnty7$My_cntyID, FUN=sum)
IHP_onaAmountAdjMy_cntyID <- aggregate(IHP_cnty7$onaAmountAdj~IHP_cnty7$My_cntyID, FUN=sum)
IHP_fldDamAmountAdjMy_cntyID <- aggregate(IHP_cnty7$floodDamageAmountAdj~IHP_cnty7$My_cntyID, FUN=sum) #14111 My_cntyIDs
IHP_rpfvlAdjMy_cntyID <- aggregate(IHP_cnty7$rpfvlAdj~IHP_cnty7$My_cntyID, FUN=sum) 
IHP_ppfvlAdjMy_cntyID <- aggregate(IHP_cnty7$ppfvlAdj~IHP_cnty7$My_cntyID, FUN=sum) 
IHP_rentalAssistanceAmountAdjMy_cntyID <- aggregate(IHP_cnty7$rentalAssistanceAmountAdj~IHP_cnty7$My_cntyID, FUN=sum) 
IHP_repairAmountAdjMy_cntyID <- aggregate(IHP_cnty7$repairAmountAdj~IHP_cnty7$My_cntyID, FUN=sum) 
IHP_replacementAmountAdjMy_cntyID <- aggregate(IHP_cnty7$replacementAmountAdj~IHP_cnty7$My_cntyID, FUN=sum) 


#IHP_AmountAdjMy_cntyID####
head(IHP_AmountAdjMy_cntyID)
names(IHP_AmountAdjMy_cntyID)[names(IHP_AmountAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_AmountAdjMy_cntyID)[names(IHP_AmountAdjMy_cntyID)=="IHP_cnty7$ihpAmountAdj"] <- "IHP_AmountAdjMy_cntyID"
sum(IHP_AmountAdjMy_cntyID$IHP_AmountAdjMy_cntyID)#8998196816 good

#IHP_haAmountAdjMy_cntyID####
head(IHP_haAmountAdjMy_cntyID)
names(IHP_haAmountAdjMy_cntyID)[names(IHP_haAmountAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_haAmountAdjMy_cntyID)[names(IHP_haAmountAdjMy_cntyID)=="IHP_cnty7$haAmountAdj"] <- "IHP_haAmountAdjMy_cntyID"
sum(IHP_haAmountAdjMy_cntyID$IHP_haAmountAdjMy_cntyID)#6689626986 good

#IHP_onaAmountAdjMy_cntyID####
head(IHP_onaAmountAdjMy_cntyID)
names(IHP_onaAmountAdjMy_cntyID)[names(IHP_onaAmountAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_onaAmountAdjMy_cntyID)[names(IHP_onaAmountAdjMy_cntyID)=="IHP_cnty7$onaAmountAdj"] <- "IHP_onaAmountAdjMy_cntyID"
sum(IHP_onaAmountAdjMy_cntyID$IHP_onaAmountAdjMy_cntyID)#2308569830 good

#IHP_fldDamAmountAdjMy_cntyID####
head(IHP_fldDamAmountAdjMy_cntyID)
names(IHP_fldDamAmountAdjMy_cntyID)[names(IHP_fldDamAmountAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_fldDamAmountAdjMy_cntyID)[names(IHP_fldDamAmountAdjMy_cntyID)=="IHP_cnty7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjMy_cntyID"
sum(IHP_fldDamAmountAdjMy_cntyID$IHP_fldDamAmountAdjMy_cntyID) #22721501658 good

#IHP_rpfvlAdjMy_cntyID####
head(IHP_rpfvlAdjMy_cntyID)
names(IHP_rpfvlAdjMy_cntyID)[names(IHP_rpfvlAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_rpfvlAdjMy_cntyID)[names(IHP_rpfvlAdjMy_cntyID)=="IHP_cnty7$rpfvlAdj"] <- "IHP_rpfvlAdjMy_cntyID"
sum(IHP_rpfvlAdjMy_cntyID$IHP_rpfvlAdjMy_cntyID) #19328944434good

#IHP_ppfvlAdjMy_cntyID####
head(IHP_ppfvlAdjMy_cntyID)
names(IHP_ppfvlAdjMy_cntyID)[names(IHP_ppfvlAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_ppfvlAdjMy_cntyID)[names(IHP_ppfvlAdjMy_cntyID)=="IHP_cnty7$ppfvlAdj"] <- "IHP_ppfvlAdjMy_cntyID"
sum(IHP_ppfvlAdjMy_cntyID$IHP_ppfvlAdjMy_cntyID) #4406460970 good


#IHP_rentalAssistanceAmountAdjMy_cntyID####
head(IHP_rentalAssistanceAmountAdjMy_cntyID)
names(IHP_rentalAssistanceAmountAdjMy_cntyID)[names(IHP_rentalAssistanceAmountAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_rentalAssistanceAmountAdjMy_cntyID)[names(IHP_rentalAssistanceAmountAdjMy_cntyID)=="IHP_cnty7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjMy_cntyID"
sum(IHP_rentalAssistanceAmountAdjMy_cntyID$IHP_rentalAssistanceAmountAdjMy_cntyID) #2471945632 good

#IHP_repairAmountAdjMy_cntyID ####
head(IHP_repairAmountAdjMy_cntyID )
names(IHP_repairAmountAdjMy_cntyID )[names(IHP_repairAmountAdjMy_cntyID )=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_repairAmountAdjMy_cntyID )[names(IHP_repairAmountAdjMy_cntyID )=="IHP_cnty7$repairAmountAdj"] <- "IHP_repairAmountAdjMy_cntyID"
sum(IHP_repairAmountAdjMy_cntyID $IHP_repairAmountAdjMy_cntyID ) #34406735560 good

#IHP_replacementAmountAdjMy_cntyID ####
head(IHP_replacementAmountAdjMy_cntyID)
names(IHP_replacementAmountAdjMy_cntyID )[names(IHP_replacementAmountAdjMy_cntyID)=="IHP_cnty7$My_cntyID"] <- "My_cntyID"
names(IHP_replacementAmountAdjMy_cntyID )[names(IHP_replacementAmountAdjMy_cntyID)=="IHP_cnty7$replacementAmountAdj"] <- "IHP_replacementAmountAdjMy_cntyID"
sum(IHP_replacementAmountAdjMy_cntyID$IHP_replacementAmountAdjMy_cntyID) #258706656 good

##Joining####
IHP_My_cntyID1 <- left_join(IHP_AmountAdjMy_cntyID, IHP_haAmountAdjMy_cntyID, by="My_cntyID", copy=FALSE)
names(IHP_cnty7)
IHP_My_cntyID2 <- left_join(IHP_My_cntyID1,IHP_onaAmountAdjMy_cntyID, by="My_cntyID", copy=FALSE)
IHP_My_cntyID3 <- left_join(IHP_My_cntyID2, IHP_fldDamAmountAdjMy_cntyID, by="My_cntyID", copy=FALSE)
IHP_My_cntyID4 <- left_join(IHP_My_cntyID3, IHP_rpfvlAdjMy_cntyID, by="My_cntyID", copy=FALSE)
IHP_My_cntyID5 <- left_join(IHP_My_cntyID4, IHP_ppfvlAdjMy_cntyID , by="My_cntyID", copy=FALSE)
IHP_My_cntyID6 <- left_join(IHP_My_cntyID5, IHP_rentalAssistanceAmountAdjMy_cntyID, by="My_cntyID", copy=FALSE)
IHP_My_cntyID7 <- left_join(IHP_My_cntyID6, IHP_repairAmountAdjMy_cntyID, by="My_cntyID", copy=FALSE)
IHP_My_cntyID8 <- left_join(IHP_My_cntyID7, IHP_replacementAmountAdjMy_cntyID, by="My_cntyID", copy=FALSE)

names(IHP_My_cntyID8)
colSums(is.na(IHP_My_cntyID8))
head(IHP_My_cntyID8) 
dim(IHP_My_cntyID8) #1088

#fixing names
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_AmountAdjMy_cntyID"] <- "IHP_AmountAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_haAmountAdjMy_cntyID"] <- "IHP_haAmountAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_onaAmountAdjMy_cntyID"] <- "IHP_onaAmountAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_fldDamAmountAdjMy_cntyID"] <- "IHP_fldDamAmountAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_rpfvlAdjMy_cntyID"] <- "IHP_rpfvlAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_ppfvlAdjMy_cntyID"] <- "IHP_ppfvlAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_rentalAssistanceAmountAdjMy_cntyID"] <- "IHP_rentalAssistanceAmountAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_repairAmountAdjMy_cntyID"] <- "IHP_repairAmountAdj_cnty"
names(IHP_My_cntyID8)[names(IHP_My_cntyID8)=="IHP_replacementAmountAdjMy_cntyID"] <- "IHP_replacementAmountAdj_cnty"
names(IHP_My_cntyID8)



###AGG to CITYDN (CityDN) level####

#creating CityID
head(IHP_cnty7$damagedCity)
IHP_cnty7$IHP_CitySt <- str_c(IHP_cnty7$damagedCity, ", ", IHP_cnty7$damagedStateAbbreviation)
IHP_CitySt <- data.frame(table(IHP_cnty7$IHP_CitySt)) #4951 unique cities


##Creating unique CityDN####
IHP_cnty7$CityDN <- str_c(IHP_cnty7$disasterNumber," ", IHP_cnty7$IHP_CitySt)
head(IHP_cnty7$CityDN)
CityDN <- data.frame(unique(IHP_cnty7$CityDN)) #11165 -  CityDNs 
names(CityDN)
names(CityDN)[names(CityDN)=="unique.IHP_cnty7.CityDN."] <- "CityDN"

###Aggregating Flood Damage by CityDN####
names(IHP_cnty7)
IHP_AmountAdjCityDN <- aggregate(IHP_cnty7$ihpAmountAdj~IHP_cnty7$CityDN, FUN=sum) 
IHP_haAmountAdjCityDN <- aggregate(IHP_cnty7$haAmountAdj~IHP_cnty7$CityDN, FUN=sum)
IHP_onaAmountAdjCityDN <- aggregate(IHP_cnty7$onaAmountAdj~IHP_cnty7$CityDN, FUN=sum)
IHP_fldDamAmountAdjCityDN <- aggregate(IHP_cnty7$floodDamageAmountAdj~IHP_cnty7$CityDN, FUN=sum) #14111 CityDNs
IHP_rpfvlAdjCityDN <- aggregate(IHP_cnty7$rpfvlAdj~IHP_cnty7$CityDN, FUN=sum) 
IHP_ppfvlAdjCityDN <- aggregate(IHP_cnty7$ppfvlAdj~IHP_cnty7$CityDN, FUN=sum) 
IHP_rentalAssistanceAmountAdjCityDN <- aggregate(IHP_cnty7$rentalAssistanceAmountAdj~IHP_cnty7$CityDN, FUN=sum) 
IHP_repairAmountAdjCityDN <- aggregate(IHP_cnty7$repairAmountAdj~IHP_cnty7$CityDN, FUN=sum) 
IHP_replacementAmountAdjCityDN <- aggregate(IHP_cnty7$replacementAmountAdj~IHP_cnty7$CityDN, FUN=sum) 


#IHP_AmountAdjCityDN####
head(IHP_AmountAdjCityDN)
names(IHP_AmountAdjCityDN)[names(IHP_AmountAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_AmountAdjCityDN)[names(IHP_AmountAdjCityDN)=="IHP_cnty7$ihpAmountAdj"] <- "IHP_AmountAdjCityDN"
sum(IHP_AmountAdjCityDN$IHP_AmountAdjCityDN)#8998196816 good

#IHP_haAmountAdjCityDN####
head(IHP_haAmountAdjCityDN)
names(IHP_haAmountAdjCityDN)[names(IHP_haAmountAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_haAmountAdjCityDN)[names(IHP_haAmountAdjCityDN)=="IHP_cnty7$haAmountAdj"] <- "IHP_haAmountAdjCityDN"
sum(IHP_haAmountAdjCityDN$IHP_haAmountAdjCityDN)#6689626986 good

#IHP_onaAmountAdjCityDN####
head(IHP_onaAmountAdjCityDN)
names(IHP_onaAmountAdjCityDN)[names(IHP_onaAmountAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_onaAmountAdjCityDN)[names(IHP_onaAmountAdjCityDN)=="IHP_cnty7$onaAmountAdj"] <- "IHP_onaAmountAdjCityDN"
sum(IHP_onaAmountAdjCityDN$IHP_onaAmountAdjCityDN)#2308569830 good

#IHP_fldDamAmountAdjCityDN####
head(IHP_fldDamAmountAdjCityDN)
names(IHP_fldDamAmountAdjCityDN)[names(IHP_fldDamAmountAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_fldDamAmountAdjCityDN)[names(IHP_fldDamAmountAdjCityDN)=="IHP_cnty7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjCityDN"
sum(IHP_fldDamAmountAdjCityDN$IHP_fldDamAmountAdjCityDN) #22721501658 good

#IHP_rpfvlAdjCityDN####
head(IHP_rpfvlAdjCityDN)
names(IHP_rpfvlAdjCityDN)[names(IHP_rpfvlAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_rpfvlAdjCityDN)[names(IHP_rpfvlAdjCityDN)=="IHP_cnty7$rpfvlAdj"] <- "IHP_rpfvlAdjCityDN"
sum(IHP_rpfvlAdjCityDN$IHP_rpfvlAdjCityDN) #19328944434good

#IHP_ppfvlAdjCityDN####
head(IHP_ppfvlAdjCityDN)
names(IHP_ppfvlAdjCityDN)[names(IHP_ppfvlAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_ppfvlAdjCityDN)[names(IHP_ppfvlAdjCityDN)=="IHP_cnty7$ppfvlAdj"] <- "IHP_ppfvlAdjCityDN"
sum(IHP_ppfvlAdjCityDN$IHP_ppfvlAdjCityDN) #4406460970 good


#IHP_rentalAssistanceAmountAdjCityDN####
head(IHP_rentalAssistanceAmountAdjCityDN)
names(IHP_rentalAssistanceAmountAdjCityDN)[names(IHP_rentalAssistanceAmountAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_rentalAssistanceAmountAdjCityDN)[names(IHP_rentalAssistanceAmountAdjCityDN)=="IHP_cnty7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjCityDN"
sum(IHP_rentalAssistanceAmountAdjCityDN$IHP_rentalAssistanceAmountAdjCityDN) #2471945632 good

#IHP_repairAmountAdjCityDN ####
head(IHP_repairAmountAdjCityDN )
names(IHP_repairAmountAdjCityDN )[names(IHP_repairAmountAdjCityDN )=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_repairAmountAdjCityDN )[names(IHP_repairAmountAdjCityDN )=="IHP_cnty7$repairAmountAdj"] <- "IHP_repairAmountAdjCityDN"
sum(IHP_repairAmountAdjCityDN $IHP_repairAmountAdjCityDN ) #34406735560 good

#IHP_replacementAmountAdjCityDN ####
head(IHP_replacementAmountAdjCityDN)
names(IHP_replacementAmountAdjCityDN )[names(IHP_replacementAmountAdjCityDN)=="IHP_cnty7$CityDN"] <- "CityDN"
names(IHP_replacementAmountAdjCityDN )[names(IHP_replacementAmountAdjCityDN)=="IHP_cnty7$replacementAmountAdj"] <- "IHP_replacementAmountAdjCityDN"
sum(IHP_replacementAmountAdjCityDN$IHP_replacementAmountAdjCityDN) #258706656 good

##Joining####
IHP_CityDN1 <- left_join(IHP_AmountAdjCityDN, IHP_haAmountAdjCityDN, by="CityDN", copy=FALSE)
names(IHP_cnty7)
IHP_CityDN2 <- left_join(IHP_CityDN1,IHP_onaAmountAdjCityDN, by="CityDN", copy=FALSE)
IHP_CityDN3 <- left_join(IHP_CityDN2, IHP_fldDamAmountAdjCityDN, by="CityDN", copy=FALSE)
IHP_CityDN4 <- left_join(IHP_CityDN3, IHP_rpfvlAdjCityDN, by="CityDN", copy=FALSE)
IHP_CityDN5 <- left_join(IHP_CityDN4, IHP_ppfvlAdjCityDN , by="CityDN", copy=FALSE)
IHP_CityDN6 <- left_join(IHP_CityDN5, IHP_rentalAssistanceAmountAdjCityDN, by="CityDN", copy=FALSE)
IHP_CityDN7 <- left_join(IHP_CityDN6, IHP_repairAmountAdjCityDN, by="CityDN", copy=FALSE)
IHP_CityDN8 <- left_join(IHP_CityDN7, IHP_replacementAmountAdjCityDN, by="CityDN", copy=FALSE)

names(IHP_CityDN8)
colSums(is.na(IHP_CityDN8))
head(IHP_CityDN8) 
dim(IHP_CityDN8) #11165



#############################################################

###AGG to CITY  level####
names(IHP_cnty7)

#creating CityID
head(IHP_cnty7$IHP_CitySt) #4951 unique cities
IHP_CitySt <- data.frame(table(IHP_cnty7$IHP_CitySt)) #4951 unique cities
head(IHP_CitySt)

###Aggregating Flood Damage by IHP_CitySt####
names(IHP_cnty7)
IHP_AmountAdjIHP_CitySt <- aggregate(IHP_cnty7$ihpAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum) 
IHP_haAmountAdjIHP_CitySt <- aggregate(IHP_cnty7$haAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum)
IHP_onaAmountAdjIHP_CitySt <- aggregate(IHP_cnty7$onaAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum)
IHP_fldDamAmountAdjIHP_CitySt <- aggregate(IHP_cnty7$floodDamageAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum) #14111 IHP_CitySts
IHP_rpfvlAdjIHP_CitySt <- aggregate(IHP_cnty7$rpfvlAdj~IHP_cnty7$IHP_CitySt, FUN=sum) 
IHP_ppfvlAdjIHP_CitySt <- aggregate(IHP_cnty7$ppfvlAdj~IHP_cnty7$IHP_CitySt, FUN=sum) 
IHP_rentalAssistanceAmountAdjIHP_CitySt <- aggregate(IHP_cnty7$rentalAssistanceAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum) 
IHP_repairAmountAdjIHP_CitySt <- aggregate(IHP_cnty7$repairAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum) 
IHP_replacementAmountAdjIHP_CitySt <- aggregate(IHP_cnty7$replacementAmountAdj~IHP_cnty7$IHP_CitySt, FUN=sum) 


#IHP_AmountAdjIHP_CitySt####
head(IHP_AmountAdjIHP_CitySt)
names(IHP_AmountAdjIHP_CitySt)[names(IHP_AmountAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_AmountAdjIHP_CitySt)[names(IHP_AmountAdjIHP_CitySt)=="IHP_cnty7$ihpAmountAdj"] <- "IHP_AmountAdjIHP_CitySt"
sum(IHP_AmountAdjIHP_CitySt$IHP_AmountAdjIHP_CitySt)#8998196816 good

#IHP_haAmountAdjIHP_CitySt####
head(IHP_haAmountAdjIHP_CitySt)
names(IHP_haAmountAdjIHP_CitySt)[names(IHP_haAmountAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_haAmountAdjIHP_CitySt)[names(IHP_haAmountAdjIHP_CitySt)=="IHP_cnty7$haAmountAdj"] <- "IHP_haAmountAdjIHP_CitySt"
sum(IHP_haAmountAdjIHP_CitySt$IHP_haAmountAdjIHP_CitySt)#6689626986 good

#IHP_onaAmountAdjIHP_CitySt####
head(IHP_onaAmountAdjIHP_CitySt)
names(IHP_onaAmountAdjIHP_CitySt)[names(IHP_onaAmountAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_onaAmountAdjIHP_CitySt)[names(IHP_onaAmountAdjIHP_CitySt)=="IHP_cnty7$onaAmountAdj"] <- "IHP_onaAmountAdjIHP_CitySt"
sum(IHP_onaAmountAdjIHP_CitySt$IHP_onaAmountAdjIHP_CitySt)#2308569830 good

#IHP_fldDamAmountAdjIHP_CitySt####
head(IHP_fldDamAmountAdjIHP_CitySt)
names(IHP_fldDamAmountAdjIHP_CitySt)[names(IHP_fldDamAmountAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_fldDamAmountAdjIHP_CitySt)[names(IHP_fldDamAmountAdjIHP_CitySt)=="IHP_cnty7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjIHP_CitySt"
sum(IHP_fldDamAmountAdjIHP_CitySt$IHP_fldDamAmountAdjIHP_CitySt) #22721501658 good

#IHP_rpfvlAdjIHP_CitySt####
head(IHP_rpfvlAdjIHP_CitySt)
names(IHP_rpfvlAdjIHP_CitySt)[names(IHP_rpfvlAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_rpfvlAdjIHP_CitySt)[names(IHP_rpfvlAdjIHP_CitySt)=="IHP_cnty7$rpfvlAdj"] <- "IHP_rpfvlAdjIHP_CitySt"
sum(IHP_rpfvlAdjIHP_CitySt$IHP_rpfvlAdjIHP_CitySt) #19328944434good

#IHP_ppfvlAdjIHP_CitySt####
head(IHP_ppfvlAdjIHP_CitySt)
names(IHP_ppfvlAdjIHP_CitySt)[names(IHP_ppfvlAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_ppfvlAdjIHP_CitySt)[names(IHP_ppfvlAdjIHP_CitySt)=="IHP_cnty7$ppfvlAdj"] <- "IHP_ppfvlAdjIHP_CitySt"
sum(IHP_ppfvlAdjIHP_CitySt$IHP_ppfvlAdjIHP_CitySt) #4406460970 good


#IHP_rentalAssistanceAmountAdjIHP_CitySt####
head(IHP_rentalAssistanceAmountAdjIHP_CitySt)
names(IHP_rentalAssistanceAmountAdjIHP_CitySt)[names(IHP_rentalAssistanceAmountAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_rentalAssistanceAmountAdjIHP_CitySt)[names(IHP_rentalAssistanceAmountAdjIHP_CitySt)=="IHP_cnty7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjIHP_CitySt"
sum(IHP_rentalAssistanceAmountAdjIHP_CitySt$IHP_rentalAssistanceAmountAdjIHP_CitySt) #2471945632 good

#IHP_repairAmountAdjIHP_CitySt ####
head(IHP_repairAmountAdjIHP_CitySt )
names(IHP_repairAmountAdjIHP_CitySt )[names(IHP_repairAmountAdjIHP_CitySt )=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_repairAmountAdjIHP_CitySt )[names(IHP_repairAmountAdjIHP_CitySt )=="IHP_cnty7$repairAmountAdj"] <- "IHP_repairAmountAdjIHP_CitySt"
sum(IHP_repairAmountAdjIHP_CitySt $IHP_repairAmountAdjIHP_CitySt ) #34406735560 good

#IHP_replacementAmountAdjIHP_CitySt ####
head(IHP_replacementAmountAdjIHP_CitySt)
names(IHP_replacementAmountAdjIHP_CitySt )[names(IHP_replacementAmountAdjIHP_CitySt)=="IHP_cnty7$IHP_CitySt"] <- "IHP_CitySt"
names(IHP_replacementAmountAdjIHP_CitySt )[names(IHP_replacementAmountAdjIHP_CitySt)=="IHP_cnty7$replacementAmountAdj"] <- "IHP_replacementAmountAdjIHP_CitySt"
sum(IHP_replacementAmountAdjIHP_CitySt$IHP_replacementAmountAdjIHP_CitySt) #258706656 good

##Joining####
IHP_CitySt1 <- left_join(IHP_AmountAdjIHP_CitySt, IHP_haAmountAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)
names(IHP_cnty7)
IHP_CitySt2 <- left_join(IHP_CitySt1,IHP_onaAmountAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)
IHP_CitySt3 <- left_join(IHP_CitySt2, IHP_fldDamAmountAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)
IHP_CitySt4 <- left_join(IHP_CitySt3, IHP_rpfvlAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)
IHP_CitySt5 <- left_join(IHP_CitySt4, IHP_ppfvlAdjIHP_CitySt , by="IHP_CitySt", copy=FALSE)
IHP_CitySt6 <- left_join(IHP_CitySt5, IHP_rentalAssistanceAmountAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)
IHP_CitySt7 <- left_join(IHP_CitySt6, IHP_repairAmountAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)
IHP_CitySt8 <- left_join(IHP_CitySt7, IHP_replacementAmountAdjIHP_CitySt, by="IHP_CitySt", copy=FALSE)

names(IHP_CitySt8)
colSums(is.na(IHP_CitySt8))
head(IHP_CitySt8) 
dim(IHP_CitySt8) #4951

#fixing names
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_AmountAdjIHP_CitySt"] <- "IHP_AmountAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_haAmountAdjIHP_CitySt"] <- "IHP_haAmountAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_onaAmountAdjIHP_CitySt"] <- "IHP_onaAmountAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_fldDamAmountAdjIHP_CitySt"] <- "IHP_fldDamAmountAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_rpfvlAdjIHP_CitySt"] <- "IHP_rpfvlAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_ppfvlAdjIHP_CitySt"] <- "IHP_ppfvlAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_rentalAssistanceAmountAdjIHP_CitySt"] <- "IHP_rentalAssistanceAmountAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_repairAmountAdjIHP_CitySt"] <- "IHP_repairAmountAdj_City"
names(IHP_CitySt8)[names(IHP_CitySt8)=="IHP_replacementAmountAdjIHP_CitySt"] <- "IHP_replacementAmountAdj_City"
names(IHP_CitySt8)



####Agg to STATE DN (SDN)####

#creating stateDN
head(IHP_cnty7$My_st_abbrF) #4951 unique cities
IHP_State <- data.frame(table(IHP_cnty7$My_st_abbrF)) #29 unique states
head(IHP_State)

##Creating unique StateDN####
IHP_cnty7$SDN <- str_c(IHP_cnty7$disasterNumber," ", IHP_cnty7$My_st_abbrF)
head(IHP_cnty7$SDN)
SDN  <- data.frame(unique(IHP_cnty7$SDN )) #166 -  StateDN s 
names(SDN)
head(SDN)
names(SDN)[names(SDN )=="unique.IHP_cnty7.SDN."] <- "SDN"

###Aggregating Flood Damage by State_DN####
names(IHP_cnty7)
IHP_AmountAdjSDN <- aggregate(IHP_cnty7$ihpAmountAdj~IHP_cnty7$SDN, FUN=sum) 
IHP_haAmountAdjSDN <- aggregate(IHP_cnty7$haAmountAdj~IHP_cnty7$SDN, FUN=sum)
IHP_onaAmountAdjSDN <- aggregate(IHP_cnty7$onaAmountAdj~IHP_cnty7$SDN, FUN=sum)
IHP_fldDamAmountAdjSDN <- aggregate(IHP_cnty7$floodDamageAmountAdj~IHP_cnty7$SDN, FUN=sum) #14111 SDNs
IHP_rpfvlAdjSDN <- aggregate(IHP_cnty7$rpfvlAdj~IHP_cnty7$SDN, FUN=sum) 
IHP_ppfvlAdjSDN <- aggregate(IHP_cnty7$ppfvlAdj~IHP_cnty7$SDN, FUN=sum) 
IHP_rentalAssistanceAmountAdjSDN <- aggregate(IHP_cnty7$rentalAssistanceAmountAdj~IHP_cnty7$SDN, FUN=sum) 
IHP_repairAmountAdjSDN <- aggregate(IHP_cnty7$repairAmountAdj~IHP_cnty7$SDN, FUN=sum) 
IHP_replacementAmountAdjSDN <- aggregate(IHP_cnty7$replacementAmountAdj~IHP_cnty7$SDN, FUN=sum) 


#IHP_AmountAdjSDN####
head(IHP_AmountAdjSDN)
names(IHP_AmountAdjSDN)[names(IHP_AmountAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_AmountAdjSDN)[names(IHP_AmountAdjSDN)=="IHP_cnty7$ihpAmountAdj"] <- "IHP_AmountAdjSDN"
sum(IHP_AmountAdjSDN$IHP_AmountAdjSDN)#8998196816 good

#IHP_haAmountAdjSDN####
head(IHP_haAmountAdjSDN)
names(IHP_haAmountAdjSDN)[names(IHP_haAmountAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_haAmountAdjSDN)[names(IHP_haAmountAdjSDN)=="IHP_cnty7$haAmountAdj"] <- "IHP_haAmountAdjSDN"
sum(IHP_haAmountAdjSDN$IHP_haAmountAdjSDN)#6689626986 good

#IHP_onaAmountAdjSDN####
head(IHP_onaAmountAdjSDN)
names(IHP_onaAmountAdjSDN)[names(IHP_onaAmountAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_onaAmountAdjSDN)[names(IHP_onaAmountAdjSDN)=="IHP_cnty7$onaAmountAdj"] <- "IHP_onaAmountAdjSDN"
sum(IHP_onaAmountAdjSDN$IHP_onaAmountAdjSDN)#2308569830 good

#IHP_fldDamAmountAdjSDN####
head(IHP_fldDamAmountAdjSDN)
names(IHP_fldDamAmountAdjSDN)[names(IHP_fldDamAmountAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_fldDamAmountAdjSDN)[names(IHP_fldDamAmountAdjSDN)=="IHP_cnty7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjSDN"
sum(IHP_fldDamAmountAdjSDN$IHP_fldDamAmountAdjSDN) #22721501658 good

#IHP_rpfvlAdjSDN####
head(IHP_rpfvlAdjSDN)
names(IHP_rpfvlAdjSDN)[names(IHP_rpfvlAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_rpfvlAdjSDN)[names(IHP_rpfvlAdjSDN)=="IHP_cnty7$rpfvlAdj"] <- "IHP_rpfvlAdjSDN"
sum(IHP_rpfvlAdjSDN$IHP_rpfvlAdjSDN) #19328944434good

#IHP_ppfvlAdjSDN####
head(IHP_ppfvlAdjSDN)
names(IHP_ppfvlAdjSDN)[names(IHP_ppfvlAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_ppfvlAdjSDN)[names(IHP_ppfvlAdjSDN)=="IHP_cnty7$ppfvlAdj"] <- "IHP_ppfvlAdjSDN"
sum(IHP_ppfvlAdjSDN$IHP_ppfvlAdjSDN) #4406460970 good


#IHP_rentalAssistanceAmountAdjSDN####
head(IHP_rentalAssistanceAmountAdjSDN)
names(IHP_rentalAssistanceAmountAdjSDN)[names(IHP_rentalAssistanceAmountAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_rentalAssistanceAmountAdjSDN)[names(IHP_rentalAssistanceAmountAdjSDN)=="IHP_cnty7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjSDN"
sum(IHP_rentalAssistanceAmountAdjSDN$IHP_rentalAssistanceAmountAdjSDN) #2471945632 good

#IHP_repairAmountAdjSDN ####
head(IHP_repairAmountAdjSDN )
names(IHP_repairAmountAdjSDN )[names(IHP_repairAmountAdjSDN )=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_repairAmountAdjSDN )[names(IHP_repairAmountAdjSDN )=="IHP_cnty7$repairAmountAdj"] <- "IHP_repairAmountAdjSDN"
sum(IHP_repairAmountAdjSDN $IHP_repairAmountAdjSDN ) #34406735560 good

#IHP_replacementAmountAdjSDN ####
head(IHP_replacementAmountAdjSDN)
names(IHP_replacementAmountAdjSDN )[names(IHP_replacementAmountAdjSDN)=="IHP_cnty7$SDN"] <- "SDN"
names(IHP_replacementAmountAdjSDN )[names(IHP_replacementAmountAdjSDN)=="IHP_cnty7$replacementAmountAdj"] <- "IHP_replacementAmountAdjSDN"
sum(IHP_replacementAmountAdjSDN$IHP_replacementAmountAdjSDN) #258706656 good

##Joining####
SDN1 <- left_join(IHP_AmountAdjSDN, IHP_haAmountAdjSDN, by="SDN", copy=FALSE)
names(IHP_cnty7)
SDN2 <- left_join(SDN1,IHP_onaAmountAdjSDN, by="SDN", copy=FALSE)
SDN3 <- left_join(SDN2, IHP_fldDamAmountAdjSDN, by="SDN", copy=FALSE)
SDN4 <- left_join(SDN3, IHP_rpfvlAdjSDN, by="SDN", copy=FALSE)
SDN5 <- left_join(SDN4, IHP_ppfvlAdjSDN , by="SDN", copy=FALSE)
SDN6 <- left_join(SDN5, IHP_rentalAssistanceAmountAdjSDN, by="SDN", copy=FALSE)
SDN7 <- left_join(SDN6, IHP_repairAmountAdjSDN, by="SDN", copy=FALSE)
SDN8 <- left_join(SDN7, IHP_replacementAmountAdjSDN, by="SDN", copy=FALSE)

names(SDN8)
colSums(is.na(SDN8))
head(SDN8) 
dim(SDN8) #166


####Agg to STATE####

#creating state
head(IHP_cnty7$My_st_abbrF) 
names(IHP_cnty7)
State <- data.frame(table(IHP_cnty7$My_st_abbrF)) #29 unique states
names(IHP_cnty7)[names(IHP_cnty7)=="My_st_abbrF"] <- "stateAbr"
head(State)
State$test <- 1
State <- data.frame(State[,c(1,3)])

###Aggregating Flood Damage by State_DN####
names(IHP_cnty7)
IHP_AmountAdjstateAbr <- aggregate(IHP_cnty7$ihpAmountAdj~IHP_cnty7$stateAbr, FUN=sum) 
IHP_haAmountAdjstateAbr <- aggregate(IHP_cnty7$haAmountAdj~IHP_cnty7$stateAbr, FUN=sum)
IHP_onaAmountAdjstateAbr <- aggregate(IHP_cnty7$onaAmountAdj~IHP_cnty7$stateAbr, FUN=sum)
IHP_fldDamAmountAdjstateAbr <- aggregate(IHP_cnty7$floodDamageAmountAdj~IHP_cnty7$stateAbr, FUN=sum) #14111 stateAbrs
IHP_rpfvlAdjstateAbr <- aggregate(IHP_cnty7$rpfvlAdj~IHP_cnty7$stateAbr, FUN=sum) 
IHP_ppfvlAdjstateAbr <- aggregate(IHP_cnty7$ppfvlAdj~IHP_cnty7$stateAbr, FUN=sum) 
IHP_rentalAssistanceAmountAdjstateAbr <- aggregate(IHP_cnty7$rentalAssistanceAmountAdj~IHP_cnty7$stateAbr, FUN=sum) 
IHP_repairAmountAdjstateAbr <- aggregate(IHP_cnty7$repairAmountAdj~IHP_cnty7$stateAbr, FUN=sum) 
IHP_replacementAmountAdjstateAbr <- aggregate(IHP_cnty7$replacementAmountAdj~IHP_cnty7$stateAbr, FUN=sum) 


#IHP_AmountAdjstateAbr####
head(IHP_AmountAdjstateAbr)
names(IHP_AmountAdjstateAbr)[names(IHP_AmountAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_AmountAdjstateAbr)[names(IHP_AmountAdjstateAbr)=="IHP_cnty7$ihpAmountAdj"] <- "IHP_AmountAdjstateAbr"
sum(IHP_AmountAdjstateAbr$IHP_AmountAdjstateAbr)#8998196816 good

#IHP_haAmountAdjstateAbr####
head(IHP_haAmountAdjstateAbr)
names(IHP_haAmountAdjstateAbr)[names(IHP_haAmountAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_haAmountAdjstateAbr)[names(IHP_haAmountAdjstateAbr)=="IHP_cnty7$haAmountAdj"] <- "IHP_haAmountAdjstateAbr"
sum(IHP_haAmountAdjstateAbr$IHP_haAmountAdjstateAbr)#6689626986 good

#IHP_onaAmountAdjstateAbr####
head(IHP_onaAmountAdjstateAbr)
names(IHP_onaAmountAdjstateAbr)[names(IHP_onaAmountAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_onaAmountAdjstateAbr)[names(IHP_onaAmountAdjstateAbr)=="IHP_cnty7$onaAmountAdj"] <- "IHP_onaAmountAdjstateAbr"
sum(IHP_onaAmountAdjstateAbr$IHP_onaAmountAdjstateAbr)#2308569830 good

#IHP_fldDamAmountAdjstateAbr####
head(IHP_fldDamAmountAdjstateAbr)
names(IHP_fldDamAmountAdjstateAbr)[names(IHP_fldDamAmountAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_fldDamAmountAdjstateAbr)[names(IHP_fldDamAmountAdjstateAbr)=="IHP_cnty7$floodDamageAmountAdj"] <- "IHP_fldDamAmountAdjstateAbr"
sum(IHP_fldDamAmountAdjstateAbr$IHP_fldDamAmountAdjstateAbr) #22721501658 good

#IHP_rpfvlAdjstateAbr####
head(IHP_rpfvlAdjstateAbr)
names(IHP_rpfvlAdjstateAbr)[names(IHP_rpfvlAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_rpfvlAdjstateAbr)[names(IHP_rpfvlAdjstateAbr)=="IHP_cnty7$rpfvlAdj"] <- "IHP_rpfvlAdjstateAbr"
sum(IHP_rpfvlAdjstateAbr$IHP_rpfvlAdjstateAbr) #19328944434good

#IHP_ppfvlAdjstateAbr####
head(IHP_ppfvlAdjstateAbr)
names(IHP_ppfvlAdjstateAbr)[names(IHP_ppfvlAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_ppfvlAdjstateAbr)[names(IHP_ppfvlAdjstateAbr)=="IHP_cnty7$ppfvlAdj"] <- "IHP_ppfvlAdjstateAbr"
sum(IHP_ppfvlAdjstateAbr$IHP_ppfvlAdjstateAbr) #4406460970 good


#IHP_rentalAssistanceAmountAdjstateAbr####
head(IHP_rentalAssistanceAmountAdjstateAbr)
names(IHP_rentalAssistanceAmountAdjstateAbr)[names(IHP_rentalAssistanceAmountAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_rentalAssistanceAmountAdjstateAbr)[names(IHP_rentalAssistanceAmountAdjstateAbr)=="IHP_cnty7$rentalAssistanceAmountAdj"] <- "IHP_rentalAssistanceAmountAdjstateAbr"
sum(IHP_rentalAssistanceAmountAdjstateAbr$IHP_rentalAssistanceAmountAdjstateAbr) #2471945632 good

#IHP_repairAmountAdjstateAbr ####
head(IHP_repairAmountAdjstateAbr )
names(IHP_repairAmountAdjstateAbr )[names(IHP_repairAmountAdjstateAbr )=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_repairAmountAdjstateAbr )[names(IHP_repairAmountAdjstateAbr )=="IHP_cnty7$repairAmountAdj"] <- "IHP_repairAmountAdjstateAbr"
sum(IHP_repairAmountAdjstateAbr $IHP_repairAmountAdjstateAbr ) #34406735560 good

#IHP_replacementAmountAdjstateAbr ####
head(IHP_replacementAmountAdjstateAbr)
names(IHP_replacementAmountAdjstateAbr )[names(IHP_replacementAmountAdjstateAbr)=="IHP_cnty7$stateAbr"] <- "stateAbr"
names(IHP_replacementAmountAdjstateAbr )[names(IHP_replacementAmountAdjstateAbr)=="IHP_cnty7$replacementAmountAdj"] <- "IHP_replacementAmountAdjstateAbr"
sum(IHP_replacementAmountAdjstateAbr$IHP_replacementAmountAdjstateAbr) #258706656 good

##Joining####
stateAbr1 <- left_join(IHP_AmountAdjstateAbr, IHP_haAmountAdjstateAbr, by="stateAbr", copy=FALSE)
names(IHP_cnty7)
stateAbr2 <- left_join(stateAbr1,IHP_onaAmountAdjstateAbr, by="stateAbr", copy=FALSE)
stateAbr3 <- left_join(stateAbr2, IHP_fldDamAmountAdjstateAbr, by="stateAbr", copy=FALSE)
stateAbr4 <- left_join(stateAbr3, IHP_rpfvlAdjstateAbr, by="stateAbr", copy=FALSE)
stateAbr5 <- left_join(stateAbr4, IHP_ppfvlAdjstateAbr , by="stateAbr", copy=FALSE)
stateAbr6 <- left_join(stateAbr5, IHP_rentalAssistanceAmountAdjstateAbr, by="stateAbr", copy=FALSE)
stateAbr7 <- left_join(stateAbr6, IHP_repairAmountAdjstateAbr, by="stateAbr", copy=FALSE)
stateAbr8 <- left_join(stateAbr7, IHP_replacementAmountAdjstateAbr, by="stateAbr", copy=FALSE)

names(stateAbr8)
colSums(is.na(stateAbr8))
head(stateAbr8) 
dim(stateAbr8) #166

#fixing names
names(stateAbr8)[names(stateAbr8)=="IHP_AmountAdjstateAbr"] <- "IHP_AmountAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_haAmountAdjstateAbr"] <- "IHP_haAmountAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_onaAmountAdjstateAbr"] <- "IHP_onaAmountAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_fldDamAmountAdjstateAbr"] <- "IHP_fldDamAmountAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_rpfvlAdjstateAbr"] <- "IHP_rpfvlAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_ppfvlAdjstateAbr"] <- "IHP_ppfvlAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_rentalAssistanceAmountAdjstateAbr"] <- "IHP_rentalAssistanceAmountAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_repairAmountAdjstateAbr"] <- "IHP_repairAmountAdj_state"
names(stateAbr8)[names(stateAbr8)=="IHP_replacementAmountAdjstateAbr"] <- "IHP_replacementAmountAdj_state"
names(stateAbr8)


###Chcking###
dim(IHP_cnty7) #1210142 by 33
dim(IHP_CDN8) #1088 by 10
dim(IHP_My_cntyID8) #322 BY 10
dim(IHP_CityDN8) #11165 by 10
dim(IHP_CitySt8) #4951 by 10
dim(SDN8) #166 by 10
dim(stateAbr8) #29 by 10


#writing out files###
path2 <- 'G:/My Drive/Academic/Research/Data/FEMA/IA/Produced'
write.csv(IHP_cnty7, file.path(path2, "IHP_cnty7.csv"), row.names=TRUE)
write.csv(IHP_CDN8, file.path(path2, "IHP_CDN.csv"), row.names=TRUE)
write.csv(IHP_My_cntyID8, file.path(path2, "IHP_County.csv"), row.names=TRUE)
write.csv(IHP_CityDN8, file.path(path2, "IHP_CityDN.csv"), row.names=TRUE)
write.csv(IHP_CitySt8, file.path(path2, "IHP_City.csv"), row.names=TRUE)
write.csv(SDN8, file.path(path2, "IHP_State_DN.csv"), row.names=TRUE)
write.csv(stateAbr8, file.path(path2, "IHP_State.csv"), row.names=TRUE)


