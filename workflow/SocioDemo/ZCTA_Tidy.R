##Pulled Data with Tidyverse

#redid 06/23/2024

##Prior NOTES:
  #Tidyverse does not seem to like it when I pull data just for a vector of ZCTAs in my dataset
  #Workaround: pull data for specific variables by specific year for all ZCTAs, then join to dataset
  #For variable names, using: https://api.census.gov/data/2019/acs/acs5/profile/variables.html
  #Other links Will mentioned:
    #https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html
    #https://walker-data.com/tidycensus/articles/basic-usage.html
    #https://www.census.gov/programs-surveys/geography/guidance/geo-areas/zctas.html

  #Vars of interest:
      #White %: 
      #Median Home Value: 
      #Median Household Income
      #Tenure: % Owner occupied
      #InUrbanHUPct: 
          #2011 NA
      #Population Density
      #MedyrBuilt: 
      #ESL %: 


##Setup####

getwd()
setwd('G:/My Drive/Academic/Research')

library(tidycensus)
library(tidyverse)
library(Hmisc)



####identifying variables####
#VARS_y1990 <-load_variables(1990, "sf1", cache = TRUE) #not sure if 1990 data available yet - recommending nhgis and ipumsr
VARS_y2000sf1 <- load_variables(2000, "sf1", cache = TRUE)
VARS_y2000sf3 <- load_variables(2000, "sf3", cache = TRUE)
VARS_y2000sf4 <- load_variables(2000, "sf4", cache = TRUE)
VARS_y2010sf1<- load_variables(2010, "sf1", cache = TRUE)
VARS_y2010sf2 <- load_variables(2010, "sf2", cache = TRUE)
VARS_y2020pl <- load_variables(2020, "pl", cache=TRUE)

VARS_y2011acs <- load_variables(2011, "acs5", cache = TRUE)
VARS_y2012acs <- load_variables(2012, "acs5", cache = TRUE)
VARS_y2013acs <- load_variables(2013, "acs5", cache = TRUE)
VARS_y2014acs <- load_variables(2014, "acs5", cache = TRUE)
VARS_y2015acs <- load_variables(2015, "acs5", cache = TRUE)
VARS_y2016acs <- load_variables(2016, "acs5", cache = TRUE)
VARS_y2017acs <- load_variables(2017, "acs5", cache = TRUE)
VARS_y2018acs <- load_variables(2018, "acs5", cache = TRUE)
VARS_y2019acs <- load_variables(2019, "acs5", cache = TRUE)
VARS_y2020acs <- load_variables(2020, "acs5", cache = TRUE)
VARS_y2021acs <- load_variables(2021, "acs5", cache = TRUE)
VARS_y2022acs <- load_variables(2022, "acs5", cache = TRUE)
#VARS_y2023acs <- load_variables(2023, "acs5", cache = TRUE)


table(VARS_y2011acs$geography)

###SOURCING ZCTA Data####
##2000 Decennial ####
#not getting MOEs for these pulls

y2000dec <- get_decennial(geography = "zcta", 
                       variables = c(
                       y00_TotPop= "P001001", #Total Population from SF3
                       y00_White= "H010003", #Total!!Not Hispanic or Latino householder!!Householder who is White alone
                       y00_MHV="H085001", #MEDIAN VALUE (DOLLARS) FOR ALL OWNER-OCCUPIED HOUSING UNITS [1]
                       y00_MHI="P053001", #Median household income in 1999
                       y00_TotHU="H004001", #Total Housing Units
                       y00_OwnOcc="H004002", #Total!!Owner occupied
                       y00_MedyrBuilt="H035001", #Median year structure built
                       y00_Urban="H005003"), #Total!!Urban!!Inside urbanized areas
                       #y00_ESL = Difficult - would need to sum across different age groups
                       survey="sf3",
                       output = "wide",
                       year = 2000)



##2010 Decennial ####
y2010dec <- get_decennial(geography = "zcta", 
                       variables = c(
                         y10_TotPop= "P001001", #Total Population from SF3
                         y10_White= "H007003", #Total!!Not Hispanic or Latino householder!!Householder who is White alone
                         #y10_MHV= NA
                         #y10_MHI= NA
                         y10_TotHU="H001001", #Total Housing Units
                         y10_OwnOcc="H014002", #Total!!Owner occupied
                         #y10_MedyrBuilt= NA
                         y10_Urban="HCT001003"), #Total!!Urban!!Inside urbanized areas
                       #y10_ESL = Difficult - would need to sum across different age groups
                       survey="sf2",
                       output = "wide",
                       year = 2010)

##2020 Decennial####


#2011 ACS####
y2011acs <-get_acs(geography= "zcta",
                variables = c(
                  y11_TotPop = "B01001_001", #BG
                  y11_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                  y11_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract
                  y11_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2011 INFLATION-ADJUSTED DOLLARS) #tract
                  y11_TotHU= "B25001_001", #Estimate!!Total #tract
                  y11_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied #tract
                  y11_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #tract
                  #y11_Urban = NA
                  y11_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2011,
                survey= "acs5",
                output="wide")

#2012 ACS####
y2012acs <-get_acs(geography= "zcta",
                variables = c(
                  y12_TotPop = "B01001_001", #BG
                  y12_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y12_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y12_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2012 INFLATION-ADJUSTED DOLLARS) #Tract
                  y12_TotHU= "B25001_001", #Estimate!!Total #BG
                  y12_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y12_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y12_Urban = NA
                  y12_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2012,
                output="wide")

#2013 ACS####
y2013acs <-get_acs(geography= "zcta",
                variables = c(
                  y13_TotPop = "B01001_001", #BG
                  y13_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y13_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y13_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2013 INFLATION-ADJUSTED DOLLARS), #BG
                  y13_TotHU= "B25001_001", #Estimate!!Total #BG
                  y13_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y13_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y13_Urban=NA
                  y13_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2013,
                output="wide")

#2014 ACS####
y2014acs <-get_acs(geography= "zcta",
                variables = c(
                  y14_TotPop = "B01001_001", #BG
                  y14_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y14_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y14_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2014 INFLATION-ADJUSTED DOLLARS), #BG
                  y14_TotHU= "B25001_001", #Estimate!!Total #BG
                  y14_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y14_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y14_Urban = NA
                  y14_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2014,
                output="wide")

#2015 ACS####
y2015acs <-get_acs(geography= "zcta",
                variables = c(
                  y15_TotPop = "B01001_001", #BG
                  y15_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y15_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y15_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2015 INFLATION-ADJUSTED DOLLARS), #BG
                  y15_TotHU= "B25001_001", #Estimate!!Total #BG
                  y15_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y15_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y15_Urban = NA
                  y15_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2015,
                output="wide")

#2016 ACS####
y2016acs <-get_acs(geography= "zcta",
                variables = c(
                  y16_TotPop = "B01001_001", #BG
                  y16_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y16_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y16_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS), #BG
                  y16_TotHU= "B25001_001", #Estimate!!Total #BG
                  y16_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y16_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y16_Urban = NA
                  y16_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2016,
                output="wide")


#2017 ACS####
y2017acs <-get_acs(geography= "zcta",
                variables = c(
                  y17_TotPop = "B01001_001", #BG
                  y17_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y17_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y17_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS), #BG
                  y17_TotHU= "B25001_001", #Estimate!!Total #BG
                  y17_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y17_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y17_Urban = NA
                  y17_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2017,
                output="wide")

#2018 ACS####
y2018acs <-get_acs(geography= "zcta",
               variables = c(
                 y18_TotPop = "B00001_001", #BG
                 y18_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                 y18_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                 y18_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS), #BG
                 y18_TotHU= "B25001_001", #Estimate!!	HOUSING UNITS
                 y18_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied #BG
                 y18_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                 #y18_Urban =NA
                 y18_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
               year=2018,
               output="wide")

#2019 ACS####
y2019acs <-get_acs(geography= "zcta",
                variables = c(
                  y19_TotPop = "B01003_001", #BG
                  y19_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y19_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y19_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS), #BG
                  y19_TotHU= "B25001_001", #Estimate!!Total #BG
                  y19_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y19_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  #y19_Urban = NA
                  y19_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2019,
                output="wide")

#2020 ACS####
y2020acs <-get_acs(geography= "zcta",
                variables = c(
                  y20_TotPop = "B01001_001", #BG
                  y20_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                  y20_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                  y20_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS), BG
                  y20_TotHU= "B25001_001", #Estimate!!Total #BG
                  y20_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                  y20_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                  y20_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                year=2020,
                output="wide")

#2021 ACS####
y2021acs <-get_acs(geography= "zcta",
                   variables = c(
                     y21_TotPop = "B01001_001", #BG
                     y21_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #BG
                     y21_MHV = "B25077_001", #Estimate!!Median value (dollars) #BG
                     y21_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS), BG
                     y21_TotHU= "B25001_001", #Estimate!!Total #BG
                     y21_OwnOcc = "B25008_002", #Estimate!!Total:!!Owner occupied#BG
                     y21_MedyrBuilt = "B25035_001", #Estimate!!Median year structure built"), #BG
                     y21_ESL ="B06007_008"), 	#Estimate!!Total!!Speak other languages!!Speak English less than "very well" =tract
                   year=2021,
                   output="wide")

#2022 ACS####
y2022acs <-get_acs(geography= "zcta",
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


##ADJUSTING $$ for INFLATION####
year<- c(1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022)
CPI_M <-c(2.02,1.92,1.87,1.81,1.76,1.72,1.67,1.62,1.60,1.57,1.53,1.47,1.46,1.42,1.39,1.35,1.30,1.27,1.22,1.22,1.19,1.17,1.14,1.12,1.10,1.10,1.09,1.06,1.04,1.02,1.00,0.99,0.92)
CPI_M <- data.frame(year, CPI_M)

y2000dec$y00_MHVadj <- y2000dec$y00_MHV*1.53
#y2010dec$y10_MHVadj <- y2010dec$y10_MHV*1.19 NA
y2011acs$y11_MHVadj <- y2011acs$y11_MHVE*1.17
y2012acs$y12_MHVadj <- y2012acs$y12_MHVE*1.14
y2013acs$y13_MHVadj <- y2013acs$y13_MHVE*1.12
y2014acs$y14_MHVadj <- y2014acs$y14_MHVE*1.10
y2015acs$y15_MHVadj <- y2015acs$y15_MHVE*1.10
y2016acs$y16_MHVadj <- y2016acs$y16_MHVE*1.09
y2017acs$y17_MHVadj <- y2017acs$y17_MHVE*1.06
y2018acs$y18_MHVadj <- y2018acs$y18_MHVE*1.04
y2019acs$y19_MHVadj <- y2019acs$y19_MHVE*1.02
y2020acs$y20_MHVadj <- y2020acs$y20_MHVE*1
y2021acs$y21_MHVadj <- y2021acs$y21_MHVE*0.99
y2022acs$y22_MHVadj <- y2022acs$y22_MHVE*0.92


y2000dec$y00_MHIadj <- y2000dec$y00_MHI*1.53
#y2010dec$y10_MHIadj <- y2010dec$y10_MHI*1.19 NA
y2011acs$y11_MHIadj <- y2011acs$y11_MHIE*1.17
y2012acs$y12_MHIadj <- y2012acs$y12_MHIE*1.14
y2013acs$y13_MHIadj <- y2013acs$y13_MHIE*1.12
y2014acs$y14_MHIadj <- y2014acs$y14_MHIE*1.10
y2015acs$y15_MHIadj <- y2015acs$y15_MHIE*1.10
y2016acs$y16_MHIadj <- y2016acs$y16_MHIE*1.09
y2017acs$y17_MHIadj <- y2017acs$y17_MHIE*1.06
y2018acs$y18_MHIadj <- y2018acs$y18_MHIE*1.04
y2019acs$y19_MHIadj <- y2019acs$y19_MHIE*1.02
y2021acs$y21_MHIadj <- y2021acs$y21_MHIE*0.99
y2022acs$y22_MHIadj <- y2022acs$y22_MHIE*0.92

##CREATING PERCENTS####
#WhitePct - All years
y2000dec$y00_WhitePct <- y2000dec$y00_White / y2000dec$y00_TotPop
y2010dec$y10_WhitePct <- y2010dec$y10_White / y2010dec$y10_TotPop
y2011acs$y11_WhitePct <- y2011acs$y11_WhiteE / y2011acs$y11_TotPopE
y2012acs$y12_WhitePct <- y2012acs$y12_WhiteE / y2012acs$y12_TotPopE
y2013acs$y13_WhitePct <- y2013acs$y13_WhiteE / y2013acs$y13_TotPopE
y2014acs$y14_WhitePct <- y2014acs$y14_WhiteE / y2014acs$y14_TotPopE
y2015acs$y15_WhitePct <- y2015acs$y15_WhiteE / y2015acs$y15_TotPopE
y2016acs$y16_WhitePct <- y2016acs$y16_WhiteE / y2016acs$y16_TotPopE
y2017acs$y17_WhitePct <- y2017acs$y17_WhiteE / y2017acs$y17_TotPopE
y2018acs$y18_WhitePct <- y2018acs$y18_WhiteE / y2018acs$y18_TotPopE
y2019acs$y19_WhitePct <- y2019acs$y19_WhiteE / y2019acs$y19_TotPopE
y2020acs$y20_WhitePct <- y2020acs$y20_WhiteE / y2020acs$y20_TotPopE
y2021acs$y21_WhitePct <- y2021acs$y21_WhiteE / y2021acs$y21_TotPopE
y2022acs$y22_WhitePct <- y2022acs$y22_WhiteE / y2022acs$y22_TotPopE


#OwnOccPct - All years
names(y2000dec)
y2000dec$y00_OwnOccPct <- y2000dec$y00_OwnOcc / y2000dec$y00_TotHU
describe(y2000dec$y00_OwnOccPct)

y2010dec$y10_OwnOccPct <- y2010dec$y10_OwnOcc / y2010dec$y10_TotHU
y2011acs$y11_OwnOccPct <- y2011acs$y11_OwnOccE / y2011acs$y11_TotHUE
y2012acs$y12_OwnOccPct <- y2012acs$y12_OwnOccE / y2012acs$y12_TotHUE
y2013acs$y13_OwnOccPct <- y2013acs$y13_OwnOccE / y2013acs$y13_TotHUE
y2014acs$y14_OwnOccPct <- y2014acs$y14_OwnOccE / y2014acs$y14_TotHUE
y2015acs$y15_OwnOccPct <- y2015acs$y15_OwnOccE / y2015acs$y15_TotHUE
y2016acs$y16_OwnOccPct <- y2016acs$y16_OwnOccE / y2016acs$y16_TotHUE
y2017acs$y17_OwnOccPct <- y2017acs$y17_OwnOccE / y2017acs$y17_TotHUE
y2018acs$y18_OwnOccPct <- y2018acs$y18_OwnOccE / y2018acs$y18_TotHUE
y2019acs$y19_OwnOccPct <- y2019acs$y19_OwnOccE / y2019acs$y19_TotHUE
y2020acs$y20_OwnOccPct <- y2020acs$y20_OwnOccE / y2020acs$y20_TotHUE
y2021acs$y21_OwnOccPct <- y2021acs$y21_OwnOccE / y2021acs$y21_TotHUE
y2022acs$y22_OwnOccPct <- y2022acs$y22_OwnOccE / y2022acs$y22_TotHUE

#UrbanPct - 2000 and 2010 decennial
y2000dec$y00_UrbanPct <- y2000dec$y00_Urban / y2000dec$y00_TotHU
y2010dec$y10_UrbanPct <- y2010dec$y10_Urban / y2010dec$y10_TotHU

#ESLPct - All ACS
y2011acs$y11_ESLpct <- y2011acs$y11_ESLE / y2011acs$y11_TotPopE
y2012acs$y12_ESLpct <- y2012acs$y12_ESLE / y2012acs$y12_TotPopE
y2013acs$y13_ESLpct <- y2013acs$y13_ESLE / y2013acs$y13_TotPopE
y2014acs$y14_ESLpct <- y2014acs$y14_ESLE / y2014acs$y14_TotPopE
y2015acs$y15_ESLpct <- y2015acs$y15_ESLE / y2015acs$y15_TotPopE
y2016acs$y16_ESLpct <- y2016acs$y16_ESLE / y2016acs$y16_TotPopE
y2017acs$y17_ESLpct <- y2017acs$y17_ESLE / y2017acs$y17_TotPopE
y2018acs$y18_ESLpct <- y2018acs$y18_ESLE / y2018acs$y18_TotPopE
y2019acs$y19_ESLpct <- y2019acs$y19_ESLE / y2019acs$y19_TotPopE
y2020acs$y20_ESLpct <- y2020acs$y20_ESLE / y2020acs$y20_TotPopE
y2021acs$y21_ESLpct <- y2021acs$y21_ESLE / y2021acs$y21_TotPopE
y2022acs$y22_ESLpct <- y2022acs$y22_ESLE / y2022acs$y22_TotPopE


####CLEANING TABLES####
#y2000
names(y2000dec)
range(y2000dec$GEOID)
y2000dec$GZCTA <- str_c("G",y2000dec$GEOID)
head(y2000dec$GZCTA)


y2000dec_2 <- y2000dec[c("GZCTA",
                         "y00_TotPop",
                         "y00_TotHU",
                         "y00_OwnOcc",
                         "y00_MedyrBuilt",
                         "y00_MHVadj",
                         "y00_MHIadj",
                         "y00_WhitePct",
                         "y00_OwnOccPct")]


#y2010
names(y2010dec)
range(y2010dec$GEOID)
y2010dec$GZCTA <- str_c("G",y2010dec$GEOID)
head(y2010dec$GZCTA)

y2010dec_2 <- y2010dec[c("GZCTA",
                         "y10_TotPop",
                         "y10_TotHU",
                         "y10_OwnOcc",
                         #"y10_MedyrBuiltE",
                         #"y10_MHVadj",
                         #"y10_MHIadj",
                         "y10_WhitePct",
                         "y10_OwnOccPct")]
                         #"y10_ESLpct")]

#y2011
names(y2011acs)
head(y2011acs$GEOID)
y2011acs$GZCTA <- substr(y2011acs$GEOID,3,7)
y2011acs$GZCTA <- str_c("G",y2011acs$GZCTA)
head(y2011acs$GZCTA)

y2011acs_2 <- y2011acs[c("GZCTA",
                         "y11_TotPopE",
                         "y11_TotHUE",
                         "y11_OwnOccE",
                         "y11_MedyrBuiltE",
                         "y11_MHVadj",
                         "y11_MHIadj",
                         "y11_WhitePct",
                         "y11_OwnOccPct",
                         "y11_ESLpct")]

#y2012
names(y2012acs)
head(y2012acs$GEOID)
y2012acs$GZCTA <- substr(y2012acs$GEOID,3,7)
y2012acs$GZCTA <- str_c("G",y2012acs$GZCTA)
head(y2012acs$GZCTA)

y2012acs_2 <- y2012acs[c("GZCTA",
                         "y12_TotPopE",
                         "y12_TotHUE",
                         "y12_OwnOccE",
                         "y12_MedyrBuiltE",
                         "y12_MHVadj",
                         "y12_MHIadj",
                         "y12_WhitePct",
                         "y12_OwnOccPct",
                         "y12_ESLpct")]
head(y2012acs_2)


#y2013
names(y2013acs)
head(y2013acs$GEOID)
range(y2013acs$GEOID)
y2013acs$GZCTA <- str_c("G",y2013acs$GEOID)
head(y2013acs$GZCTA)

y2013acs_2 <- y2013acs[c("GZCTA",
                         "y13_TotPopE",
                         "y13_TotHUE",
                         "y13_OwnOccE",
                         "y13_MedyrBuiltE",
                         "y13_MHVadj",
                         "y13_MHIadj",
                         "y13_WhitePct",
                         "y13_OwnOccPct",
                         "y13_ESLpct")]
head(y2013acs_2)


#y2014
names(y2014acs)
head(y2014acs$GEOID)
range(y2014acs$GEOID)
y2014acs$GZCTA <- str_c("G",y2014acs$GEOID)
head(y2014acs$GZCTA)

y2014acs_2 <- y2014acs[c("GZCTA",
                         "y14_TotPopE",
                         "y14_TotHUE",
                         "y14_OwnOccE",
                         "y14_MedyrBuiltE",
                         "y14_MHVadj",
                         "y14_MHIadj",
                         "y14_WhitePct",
                         "y14_OwnOccPct",
                         "y14_ESLpct")]

#y2015
names(y2015acs)
head(y2015acs$GEOID)
range(y2015acs$GEOID)
y2015acs$GZCTA <- str_c("G",y2015acs$GEOID)
head(y2015acs$GZCTA)

y2015acs_2 <- y2015acs[c("GZCTA",
                         "y15_TotPopE",
                         "y15_TotHUE",
                         "y15_OwnOccE",
                         "y15_MedyrBuiltE",
                         "y15_MHVadj",
                         "y15_MHIadj",
                         "y15_WhitePct",
                         "y15_OwnOccPct",
                         "y15_ESLpct")]

#y2016
names(y2016acs)
head(y2016acs$GEOID)
range(y2016acs$GEOID)
y2016acs$GZCTA <- str_c("G",y2016acs$GEOID)
head(y2016acs$GZCTA)

y2016acs_2 <- y2016acs[c("GZCTA",
                         "y16_TotPopE",
                         "y16_TotHUE",
                         "y16_OwnOccE",
                         "y16_MedyrBuiltE",
                         "y16_MHVadj",
                         "y16_MHIadj",
                         "y16_WhitePct",
                         "y16_OwnOccPct",
                         "y16_ESLpct")]

#y2017
names(y2017acs)
head(y2017acs$GEOID)
range(y2017acs$GEOID)
y2017acs$GZCTA <- str_c("G",y2017acs$GEOID)
head(y2017acs$GZCTA)

y2017acs_2 <- y2017acs[c("GZCTA",
                         "y17_TotPopE",
                         "y17_TotHUE",
                         "y17_OwnOccE",
                         "y17_MedyrBuiltE",
                         "y17_MHVadj",
                         "y17_MHIadj",
                         "y17_WhitePct",
                         "y17_OwnOccPct",
                         "y17_ESLpct")]

#y2018
names(y2018acs)
head(y2018acs$GEOID)
range(y2018acs$GEOID)
y2018acs$GZCTA <- str_c("G",y2018acs$GEOID)
head(y2018acs$GZCTA)

y2018acs_2 <- y2018acs[c("GZCTA",
                         "y18_TotPopE",
                         "y18_TotHUE",
                         "y18_OwnOccE",
                         "y18_MedyrBuiltE",
                         "y18_MHVadj",
                         "y18_MHIadj",
                         "y18_WhitePct",
                         "y18_OwnOccPct",
                         "y18_ESLpct")]

#y2019
names(y2019acs)
head(y2019acs$GEOID)
range(y2019acs$GEOID)
y2019acs$GZCTA <- str_c("G",y2019acs$GEOID)
head(y2019acs$GZCTA)

y2019acs_2 <- y2019acs[c("GZCTA",
                         "y19_TotPopE",
                         "y19_TotHUE",
                         "y19_OwnOccE",
                         "y19_MedyrBuiltE",
                         "y19_MHVadj",
                         "y19_MHIadj",
                         "y19_WhitePct",
                         "y19_OwnOccPct",
                         "y19_ESLpct")]

#y2020
names(y2020acs)
head(y2020acs$GEOID)
range(y2020acs$GEOID)
y2020acs$GZCTA <- str_c("G",y2020acs$GEOID)
head(y2020acs$GZCTA)

y2020acs_2 <- y2020acs[c("GZCTA",
                         "y20_TotPopE",
                         "y20_TotHUE",
                         "y20_OwnOccE",
                         "y20_MedyrBuiltE",
                         "y20_MHVE",
                         "y20_MHIE",
                         "y20_WhitePct",
                         "y20_OwnOccPct",
                         "y20_ESLpct")]


#y2021
names(y2021acs)
head(y2021acs$GEOID)
range(y2021acs$GEOID)
y2021acs$GZCTA <- str_c("G",y2021acs$GEOID)
head(y2021acs$GZCTA)

y2021acs_2 <- y2021acs[c("GZCTA",
                         "y21_TotPopE",
                         "y21_TotHUE",
                         "y21_OwnOccE",
                         "y21_MedyrBuiltE",
                         "y21_MHVadj",
                         "y21_MHIadj",
                         "y21_WhitePct",
                         "y21_OwnOccPct",
                         "y21_ESLpct")]


#y2022
names(y2022acs)
head(y2022acs$GEOID)
range(y2022acs$GEOID)
y2022acs$GZCTA <- str_c("G",y2022acs$GEOID)
head(y2022acs$GZCTA)

y2022acs_2 <- y2022acs[c("GZCTA",
                         "y22_TotPopE",
                         "y22_TotHUE",
                         "y22_OwnOccE",
                         "y22_MedyrBuiltE",
                         "y22_MHVadj",
                         "y22_MHIadj",
                         "y22_WhitePct",
                         "y22_OwnOccPct",
                         "y22_ESLpct")]

#JOINING TABLES####
dim(y2010dec)
dim(y2011acs_2)
dim(y2012acs_2)


TS_ZCTA_0 <- left_join(y2000dec_2, y2010dec_2, by="GZCTA", copy=F)
TS_ZCTA_1 <- left_join(TS_ZCTA_0, y2011acs_2, by="GZCTA", copy=F)
TS_ZCTA_2 <- left_join(TS_ZCTA_1, y2012acs_2, by="GZCTA", copy=F)
TS_ZCTA_3 <- left_join(TS_ZCTA_2, y2013acs_2, by="GZCTA", copy=F)
TS_ZCTA_4 <- left_join(TS_ZCTA_3, y2014acs_2, by="GZCTA", copy=F)
TS_ZCTA_5 <- left_join(TS_ZCTA_4, y2015acs_2, by="GZCTA", copy=F)
TS_ZCTA_6 <- left_join(TS_ZCTA_5, y2016acs_2, by="GZCTA", copy=F)
TS_ZCTA_7 <- left_join(TS_ZCTA_6, y2017acs_2, by="GZCTA", copy=F)
TS_ZCTA_8 <- left_join(TS_ZCTA_7, y2018acs_2, by="GZCTA", copy=F)
TS_ZCTA_9 <- left_join(TS_ZCTA_8, y2019acs_2, by="GZCTA", copy=F)
TS_ZCTA_10 <- left_join(TS_ZCTA_9, y2020acs_2, by="GZCTA", copy=F)
TS_ZCTA_11 <- left_join(TS_ZCTA_10, y2021acs_2, by="GZCTA", copy=F)
TS_ZCTA_12 <- left_join(TS_ZCTA_11, y2022acs_2, by="GZCTA", copy=F)

#checking Time series data ####
names(TS_ZCTA_12)
colSums(is.na(TS_ZCTA_12))

#subsetting to Ortley Beach ####
OB_TS_ZCTA <- subset(TS_ZCTA_10, GZCTA=="G08751")

#writing out files ####

path1 <- 'C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census'
write.csv(TS_ZCTA_12, file.path(path1, "TS_ZCTA_Tidy.csv"), row.names=TRUE)
write.csv(OB_TS_ZCTA, file.path(path1, "OB_TS_ZCTA_Tidy.csv"), row.names=TRUE)
