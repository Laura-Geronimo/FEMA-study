##Creating database: universe of counties

##Setup####
getwd()
setwd('H:/My Drive/Scarlet/Academic/Research/')

##Libraries
library(dplyr)
library(stringr)
library(Hmisc)
library(tidycensus)
library(data.table)

#importing data####
county_FirstStreet <- read.csv('./Data/Firststreet/Aggregated/V2/fsf_flood_county_summary.csv')

County_FS_HMGP <- read.csv('./Data/FEMA/HMA/Raw_V3/ProducedV3/CountyLevel/County_FS_HMGP.csv')
County_FS_HMGP_v4 <- read.csv('C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/HMA/HMGP_Agg/County_HMGP.csv')

County_FS_IHP <- read.csv('./Data/FEMA/IA/Produced/FS/CountyLevel/County_FS_IHP.csv')
County_FS_NFIP <- read.csv('./Data/FEMA/NFIP/Claims/Produced/FS/County_FS_NFIP.csv')

VARS_y2022acs <- load_variables(2022, "acs5", cache = TRUE)

y2022acs <-get_acs(geography= "county",
                   variables = c(
                     y22_TotPop = "B01001_001", #BG
                     y22_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                     y22_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                     y22_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS), BG
                     y22_TotHU= "B25001_001", #Estimate!!Total #BG
                     y22_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                     y22_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                     y22_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                   year=2022,
                   output="wide")


#cleaning cntyID
names(county_FirstStreet)
head(county_FirstStreet$fips)
county_FirstStreet$My_cntyID <- str_pad(county_FirstStreet$fips,5, pad="0")
county_FirstStreet$My_cntyID <- str_c('C', county_FirstStreet$My_cntyID)
range(county_FirstStreet$My_cntyID)

names(County_FS_HMGP)
County_FS_HMGP <- County_FS_HMGP[,c(-1)]
head(County_FS_HMGP$CntyIDLG)
range(County_FS_HMGP$CntyIDLG)
names(County_FS_HMGP)[names(County_FS_HMGP)=="CntyIDLG"] <- "My_cntyID"

names(County_FS_NFIP)
County_FS_NFIP <- County_FS_NFIP[,c(-1)]
head(County_FS_NFIP$My_cntyID)

names(County_FS_IHP)
County_FS_IHP <- County_FS_IHP[,c(-1)]
names(County_FS_IHP)[names(County_FS_IHP)=="countyID"] <- "My_cntyID"
head(County_FS_IHP$My_cntyID)


names(y2022acs)
y2022acs$My_cntyID <- str_pad(y2022acs$GEOID,5, pad="0")
y2022acs$My_cntyID <- str_c('C', y2022acs$My_cntyID)
range(y2022acs$My_cntyID)

#joining all to census data####
UCounty1 <- left_join(y2022acs, county_FirstStreet, by="My_cntyID", copy=F)
UCounty2 <- left_join(UCounty1, County_FS_HMGP, by="My_cntyID", copy=F)
UCounty3 <- left_join(UCounty2, County_FS_NFIP, by="My_cntyID", copy=F)
UCounty4 <- left_join(UCounty3, County_FS_IHP, by="My_cntyID", copy=F)

names(UCounty4)
colSums(is.na(UCounty4))

#writing out files
path2 <- 'G:/My Drive/Academic/Research/Data/Fused/V3/FS/CountyLevel/Universe'
write.csv(UCounty4, file.path(path2, "UCounty.csv"), row.names=TRUE)


