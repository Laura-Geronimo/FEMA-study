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
      #Black %
      #Hisp %
      #Median Home Value: 
      #Median Household Income
      #Total Housing Units
      #Total Occupied Housing Units
      #Tenure: 
        #Owner occupied %
        #Renter Occupied %
      #2nd home %
      #Poverty %


#Wishlist
      #Population Density
      #Urban
      #MedyrBuilt
      #ESL %


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

VARS_y2010acs <- load_variables(2010, "acs5", cache = TRUE)
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
y2000dec_sf1 <- get_decennial(geography = "zcta", 
                       variables = c(
                       y00_TotPop= "P001001", #Total Population from SF1
                       y00_White= "P004005", #Total!!Not Hispanic or Latino!!Population of one race!!White alone SF1
                       y00_Black= "P004006", #Total!!Not Hispanic or Latino!!Population of one race!!Black or African American alone SF1
                       y00_Hisp = "P004002"), #Total!!Hispanic or Latino SF1
                       survey="sf1",
                       output = "wide",
                       year = 2000)

y2000dec_sf3 <- get_decennial(geography = "zcta", 
                      variables = c(
                       y00_MHV= "H085001", #MEDIAN VALUE (DOLLARS) FOR ALL OWNER-OCCUPIED HOUSING UNITS [1] SF3
                       y00_MHI= "P053001", #MEDIAN HOUSEHOLD INCOME IN 1999 (DOLLARS) SF3
                       y00_PovPop= "P087001", #total population for whom poverty status is determined (
                       y00_Pov= "P087002", #Total!!Income in 1999 below poverty level, POVERTY STATUS IN 1999 BY AGE [17] |population below the poverty level 
                       y00_TotHU= "H003001", #Total: 100-PERCENT COUNT OF HOUSING UNITS [1] SF3
                       y00_TotHU_VS ="H008001", #Total: Vacancy Status SF3
                       y00_TotOccHU ="H006002", #Total!!Occupied SF3
                       y00_OwnOcc= "H007002", #Total!!Owner occupied SF3
                       y00_RentOcc= "H007003", #Total!!Renter occupied SF3
                       y00_2ndHome = "H008005"), #Total!!For seasonal, recreational, or occasional use SF3
                       survey="sf3",
                       output = "wide",
                       year = 2000)

y2000dec <- left_join(y2000dec_sf1, y2000dec_sf3, by="GEOID")
y2000dec <- y2000dec %>%
  select(-c(NAME.x, NAME.y))

rm(y2000dec_sf1, y2000dec_sf3)

#2010 Decennial ####
y2010dec <- get_decennial(geography = "zcta", 
                          variables = c(
                            y10_TotPop= "P001001", #Total Population from SF1
                            y10_White= "P005003", #Total!!Not Hispanic or Latino!!White alone from SF1
                            y10_Black= "P005004", #Total!!Not Hispanic or Latino!!Black or African American alone from SF1
                            y10_Hisp = "P004003", #Total!!Hispanic or Latino from SF1                 
                            #y10_MHV= NA
                            #y10_MHI= NA
                            #y10_Pov= NA
                            y10_TotHU= "H001001", #Total: 100-PERCENT COUNT OF HOUSING UNITS [1] from SF1
                            y10_TotHU_VS ="H005001", #Total: Vacancy Status from SF1
                            y10_2ndHome = "H005006", #Total!!For seasonal, recreational, or occasional use from SF1
                            y10_TotOccHU ="H003002", #Total!!Occupied | OCCUPANCY STATUS SF1
                            y10_OwnOcc= "H014002", #Total!!Owner occupied |	TENURE BY RACE OF HOUSEHOLDER SF1
                            y10_RentOcc= "H014010"), #Total!!Renter occupied SF1
                          survey="sf1",
                          output = "wide",
                          year = 2010)

#2011 ACS####
y2011acs <-get_acs(geography= "zcta",
                variables = c(
                  y11_TotPop = "B01003_001", #Estimate!!Total: Total Population
                  y11_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                  y11_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                  y11_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                  y11_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                  y11_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2011 INFLATION-ADJUSTED DOLLARS) #tract 	
                  y11_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                  y11_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                  y11_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                  y11_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                  y11_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                  y11_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                  y11_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                  y11_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                year=2011,
                survey= "acs5",
                output="wide")


#2012 ACS ####
y2012acs <-get_acs(geography= "zcta",
                   variables = c(
                    y12_TotPop = "B01003_001", #Estimate!!Total: Total Population
                    y12_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                    y12_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                    y12_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                    y12_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                    y12_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2012 INFLATION-ADJUSTED DOLLARS) #tract 	
                    y12_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                    y12_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                    y12_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                    y12_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                    y12_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                    y12_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                    y12_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                    y12_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2012,
                   survey= "acs5",
                   output="wide")

#2013 ACS ####
y2013acs <-get_acs(geography= "zcta",
                   variables = c(
                     y13_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y13_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y13_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y13_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y13_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y13_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2013 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y13_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y13_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y13_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y13_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y13_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y13_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y13_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y13_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2013,
                   survey= "acs5",
                   output="wide")

#2014 ACS ####
y2014acs <-get_acs(geography= "zcta",
                   variables = c(
                     y14_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y14_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y14_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y14_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y14_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y14_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2014 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y14_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y14_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y14_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y14_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y14_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y14_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y14_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y14_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2014,
                   survey= "acs5",
                   output="wide")

#2015 ACS ####
y2015acs <-get_acs(geography= "zcta",
                   variables = c(
                     y15_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y15_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y15_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y15_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y15_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y15_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2015 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y15_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y15_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y15_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y15_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y15_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y15_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y15_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y15_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2015,
                   survey= "acs5",
                   output="wide")


#2016 ACS ####
y2016acs <-get_acs(geography= "zcta",
                   variables = c(
                     y16_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y16_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y16_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y16_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y16_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y16_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2016 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y16_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y16_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y16_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y16_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y16_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y16_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y16_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y16_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2016,
                   survey= "acs5",
                   output="wide")

#2017 ACS ####
y2017acs <-get_acs(geography= "zcta",
                   variables = c(
                     y17_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y17_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y17_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y17_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y17_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y17_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2017 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y17_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y17_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y17_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y17_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y17_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y17_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y17_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y17_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2017,
                   survey= "acs5",
                   output="wide")

#2018 ACS ####
y2018acs <-get_acs(geography= "zcta",
                   variables = c(
                     y18_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y18_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y18_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y18_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y18_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y18_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2018 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y18_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y18_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y18_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y18_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y18_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y18_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y18_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y18_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2018,
                   survey= "acs5",
                   output="wide")

#2019 ACS ####
y2019acs <-get_acs(geography= "zcta",
                   variables = c(
                     y19_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y19_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y19_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y19_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y19_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y19_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2019 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y19_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y19_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y19_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y19_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y19_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y19_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y19_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y19_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2019,
                   survey= "acs5",
                   output="wide")

#2020 ACS ####
y2020acs <-get_acs(geography= "zcta",
                   variables = c(
                     y20_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y20_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y20_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y20_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y20_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y20_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2020 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y20_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y20_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y20_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y20_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y20_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y20_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y20_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y20_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2020,
                   survey= "acs5",
                   output="wide")

#2021 ACS ####
y2021acs <-get_acs(geography= "zcta",
                   variables = c(
                     y21_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y21_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y21_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y21_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y21_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y21_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2021 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y21_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y21_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y21_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y21_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y21_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y21_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y21_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y21_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2021,
                   survey= "acs5",
                   output="wide")

#2022 ACS ####
y2022acs <-get_acs(geography= "zcta",
                   variables = c(
                     y22_TotPop = "B01003_001", #Estimate!!Total: Total Population
                     y22_White = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone #tract
                     y22_Black = "B03002_004", #Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
                     y22_Hisp= "B03002_012", #Estimate!!Total!!Hispanic or Latino
                     y22_MHV = "B25077_001", #Estimate!!Median value (dollars) #tract 
                     y22_MHI = "B19013_001", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN2012 INFLATION-ADJUSTED DOLLARS) #tract 	
                     y22_PovPop= "B17001_001", #Estimate!!Total - Total population for who poverty status was determined
                     y22_Pov = "B17001_002", #Estimate!!Total!!Income in the past 12 months below poverty level
                     y22_TotHU= "B25001_001", #Estimate!!Total ; HOUSING UNITS #tract
                     y22_TotHU_VS = "B25004_001", #Estimate!!Total ; Vacancy Status	
                     y22_2ndHome= "B25004_006", #Estimate!!Total!!For seasonal, recreational, or occasional use | VACANCY STATUS
                     y22_TotOccHU= "B25002_002", #Estimate!!Total!!Occupied 
                     y22_OwnOcc = "B25003_002", #Estimate!!Total!!Owner occupied |TENURE
                     y22_RentOcc = "B25003_003"), #Estimate!!Total!!Renter occupied |TENURE
                   year=2022,
                   survey= "acs5",
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


y2000dec$y00_MHIadj <- y2000dec$y00_MHI*1.57 #note original is 1999USD
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

#BlackPct - All years
y2000dec$y00_BlackPct <- y2000dec$y00_Black / y2000dec$y00_TotPop
y2010dec$y10_BlackPct <- y2010dec$y10_Black / y2010dec$y10_TotPop
y2011acs$y11_BlackPct <- y2011acs$y11_BlackE / y2011acs$y11_TotPopE
y2012acs$y12_BlackPct <- y2012acs$y12_BlackE / y2012acs$y12_TotPopE
y2013acs$y13_BlackPct <- y2013acs$y13_BlackE / y2013acs$y13_TotPopE
y2014acs$y14_BlackPct <- y2014acs$y14_BlackE / y2014acs$y14_TotPopE
y2015acs$y15_BlackPct <- y2015acs$y15_BlackE / y2015acs$y15_TotPopE
y2016acs$y16_BlackPct <- y2016acs$y16_BlackE / y2016acs$y16_TotPopE
y2017acs$y17_BlackPct <- y2017acs$y17_BlackE / y2017acs$y17_TotPopE
y2018acs$y18_BlackPct <- y2018acs$y18_BlackE / y2018acs$y18_TotPopE
y2019acs$y19_BlackPct <- y2019acs$y19_BlackE / y2019acs$y19_TotPopE
y2020acs$y20_BlackPct <- y2020acs$y20_BlackE / y2020acs$y20_TotPopE
y2021acs$y21_BlackPct <- y2021acs$y21_BlackE / y2021acs$y21_TotPopE
y2022acs$y22_BlackPct <- y2022acs$y22_BlackE / y2022acs$y22_TotPopE

#HispPct - All years
y2000dec$y00_HispPct <- y2000dec$y00_Hisp / y2000dec$y00_TotPop
y2010dec$y10_HispPct <- y2010dec$y10_Hisp / y2010dec$y10_TotPop
y2011acs$y11_HispPct <- y2011acs$y11_HispE / y2011acs$y11_TotPopE
y2012acs$y12_HispPct <- y2012acs$y12_HispE / y2012acs$y12_TotPopE
y2013acs$y13_HispPct <- y2013acs$y13_HispE / y2013acs$y13_TotPopE
y2014acs$y14_HispPct <- y2014acs$y14_HispE / y2014acs$y14_TotPopE
y2015acs$y15_HispPct <- y2015acs$y15_HispE / y2015acs$y15_TotPopE
y2016acs$y16_HispPct <- y2016acs$y16_HispE / y2016acs$y16_TotPopE
y2017acs$y17_HispPct <- y2017acs$y17_HispE / y2017acs$y17_TotPopE
y2018acs$y18_HispPct <- y2018acs$y18_HispE / y2018acs$y18_TotPopE
y2019acs$y19_HispPct <- y2019acs$y19_HispE / y2019acs$y19_TotPopE
y2020acs$y20_HispPct <- y2020acs$y20_HispE / y2020acs$y20_TotPopE
y2021acs$y21_HispPct <- y2021acs$y21_HispE / y2021acs$y21_TotPopE
y2022acs$y22_HispPct <- y2022acs$y22_HispE / y2022acs$y22_TotPopE


## OwnOccPct - All years (as percent of occupied HUs) ####
names(y2000dec)
y2000dec$y00_OwnOccPct <- y2000dec$y00_OwnOcc / y2000dec$y00_TotOccHU
y2010dec$y10_OwnOccPct <- y2010dec$y10_OwnOcc / y2010dec$y10_TotOccHU
y2011acs$y11_OwnOccPct <- y2011acs$y11_OwnOccE / y2011acs$y11_TotOccHUE
y2012acs$y12_OwnOccPct <- y2012acs$y12_OwnOccE / y2012acs$y12_TotOccHUE
y2013acs$y13_OwnOccPct <- y2013acs$y13_OwnOccE / y2013acs$y13_TotOccHUE
y2014acs$y14_OwnOccPct <- y2014acs$y14_OwnOccE / y2014acs$y14_TotOccHUE
y2015acs$y15_OwnOccPct <- y2015acs$y15_OwnOccE / y2015acs$y15_TotOccHUE
y2016acs$y16_OwnOccPct <- y2016acs$y16_OwnOccE / y2016acs$y16_TotOccHUE
y2017acs$y17_OwnOccPct <- y2017acs$y17_OwnOccE / y2017acs$y17_TotOccHUE
y2018acs$y18_OwnOccPct <- y2018acs$y18_OwnOccE / y2018acs$y18_TotOccHUE
y2019acs$y19_OwnOccPct <- y2019acs$y19_OwnOccE / y2019acs$y19_TotOccHUE
y2020acs$y20_OwnOccPct <- y2020acs$y20_OwnOccE / y2020acs$y20_TotOccHUE
y2021acs$y21_OwnOccPct <- y2021acs$y21_OwnOccE / y2021acs$y21_TotOccHUE
y2022acs$y22_OwnOccPct <- y2022acs$y22_OwnOccE / y2022acs$y22_TotOccHUE

## RentOccPct - All years (as percent of occupied HUs) ####
names(y2000dec)
y2000dec$y00_RentOccPct <- y2000dec$y00_RentOcc / y2000dec$y00_TotOccHU
y2010dec$y10_RentOccPct <- y2010dec$y10_RentOcc / y2010dec$y10_TotOccHU
y2011acs$y11_RentOccPct <- y2011acs$y11_RentOccE / y2011acs$y11_TotOccHUE
y2012acs$y12_RentOccPct <- y2012acs$y12_RentOccE / y2012acs$y12_TotOccHUE
y2013acs$y13_RentOccPct <- y2013acs$y13_RentOccE / y2013acs$y13_TotOccHUE
y2014acs$y14_RentOccPct <- y2014acs$y14_RentOccE / y2014acs$y14_TotOccHUE
y2015acs$y15_RentOccPct <- y2015acs$y15_RentOccE / y2015acs$y15_TotOccHUE
y2016acs$y16_RentOccPct <- y2016acs$y16_RentOccE / y2016acs$y16_TotOccHUE
y2017acs$y17_RentOccPct <- y2017acs$y17_RentOccE / y2017acs$y17_TotOccHUE
y2018acs$y18_RentOccPct <- y2018acs$y18_RentOccE / y2018acs$y18_TotOccHUE
y2019acs$y19_RentOccPct <- y2019acs$y19_RentOccE / y2019acs$y19_TotOccHUE
y2020acs$y20_RentOccPct <- y2020acs$y20_RentOccE / y2020acs$y20_TotOccHUE
y2021acs$y21_RentOccPct <- y2021acs$y21_RentOccE / y2021acs$y21_TotOccHUE
y2022acs$y22_RentOccPct <- y2022acs$y22_RentOccE / y2022acs$y22_TotOccHUE


## 2ndHomePct - All years (as percent of Total HUs) ####
names(y2000dec)
y2000dec$y00_2ndHomePct <- y2000dec$y00_2ndHome / y2000dec$y00_TotHU_VS #note that there were some with > 1 when using TotHU, so used TotHU_VS from vacancy table
y2010dec$y10_2ndHomePct <- y2010dec$y10_2ndHome / y2010dec$y10_TotHU 
y2011acs$y11_2ndHomePct <- y2011acs$y11_2ndHomeE / y2011acs$y11_TotHUE 
y2012acs$y12_2ndHomePct <- y2012acs$y12_2ndHomeE / y2012acs$y12_TotHUE 
y2013acs$y13_2ndHomePct <- y2013acs$y13_2ndHomeE / y2013acs$y13_TotHUE 
y2014acs$y14_2ndHomePct <- y2014acs$y14_2ndHomeE / y2014acs$y14_TotHUE 
y2015acs$y15_2ndHomePct <- y2015acs$y15_2ndHomeE / y2015acs$y15_TotHUE 
y2016acs$y16_2ndHomePct <- y2016acs$y16_2ndHomeE / y2016acs$y16_TotHUE 
y2017acs$y17_2ndHomePct <- y2017acs$y17_2ndHomeE / y2017acs$y17_TotHUE 
y2018acs$y18_2ndHomePct <- y2018acs$y18_2ndHomeE / y2018acs$y18_TotHUE 
y2019acs$y19_2ndHomePct <- y2019acs$y19_2ndHomeE / y2019acs$y19_TotHUE 
y2020acs$y20_2ndHomePct <- y2020acs$y20_2ndHomeE / y2020acs$y20_TotHUE 
y2021acs$y21_2ndHomePct <- y2021acs$y21_2ndHomeE / y2021acs$y21_TotHUE 
y2022acs$y22_2ndHomePct <- y2022acs$y22_2ndHomeE / y2022acs$y22_TotHUE 


## PovPct - All years (as percent of total population for whom poverty status was determined) ####
names(y2000dec)
y2000dec$y00_PovPct <- y2000dec$y00_Pov / y2000dec$y00_PovPop
#y2010dec$y10_PovPct <- y2010dec$y10_Pov / y2010dec$y10_PovPop NA
y2011acs$y11_PovPct <- y2011acs$y11_PovE / y2011acs$y11_PovPopE
y2012acs$y12_PovPct <- y2012acs$y12_PovE / y2012acs$y12_PovPopE
y2013acs$y13_PovPct <- y2013acs$y13_PovE / y2013acs$y13_PovPopE
y2014acs$y14_PovPct <- y2014acs$y14_PovE / y2014acs$y14_PovPopE
y2015acs$y15_PovPct <- y2015acs$y15_PovE / y2015acs$y15_PovPopE
y2016acs$y16_PovPct <- y2016acs$y16_PovE / y2016acs$y16_PovPopE
y2017acs$y17_PovPct <- y2017acs$y17_PovE / y2017acs$y17_PovPopE
y2018acs$y18_PovPct <- y2018acs$y18_PovE / y2018acs$y18_PovPopE
y2019acs$y19_PovPct <- y2019acs$y19_PovE / y2019acs$y19_PovPopE
y2020acs$y20_PovPct <- y2020acs$y20_PovE / y2020acs$y20_PovPopE
y2021acs$y21_PovPct <- y2021acs$y21_PovE / y2021acs$y21_PovPopE
y2022acs$y22_PovPct <- y2022acs$y22_PovE / y2022acs$y22_PovPopE




####CLEANING TABLES####
## y2000 ####
names(y2000dec)
range(y2000dec$GEOID)
y2000dec$GZCTA <- str_c("G",y2000dec$GEOID)
head(y2000dec$GZCTA)


y2000dec_2 <- y2000dec[c("GZCTA",
                         "y00_TotPop",
                         "y00_WhitePct",
                         "y00_BlackPct",
                         "y00_HispPct",
                         "y00_MHVadj",
                         "y00_MHIadj",
                         "y00_PovPct", 
                         "y00_TotHU",
                         "y00_TotOccHU",
                         "y00_OwnOccPct",
                         "y00_RentOccPct",
                         "y00_2ndHomePct")]

summary(y2000dec_2)


## y2010 ####
names(y2010dec)
range(y2010dec$GEOID)
y2010dec$GZCTA <- str_c("G",y2010dec$GEOID)
head(y2010dec$GZCTA)

y2010dec_2 <- y2010dec[c("GZCTA",
                         "y10_TotPop",
                         "y10_WhitePct",
                         "y10_BlackPct",
                         "y10_HispPct",
                         #"y10_MHVadj",
                         #"y10_MHIadj",
                         #"y10_PovPct", 
                         "y10_TotHU",
                         "y10_TotOccHU",
                         "y10_OwnOccPct",
                         "y10_RentOccPct",
                         "y10_2ndHomePct")]

summary(y2010dec_2)


## y2011 ####
names(y2011acs)
head(y2011acs$GEOID)
y2011acs$GZCTA <- substr(y2011acs$GEOID,3,7)
y2011acs$GZCTA <- str_c("G",y2011acs$GZCTA)
head(y2011acs$GZCTA)

y2011acs_2 <- y2011acs[c("GZCTA",
                         "y11_TotPopE",
                         "y11_WhitePct",
                         "y11_BlackPct",
                         "y11_HispPct",
                         "y11_MHVadj",
                         "y11_MHIadj",
                         "y11_PovPct", 
                         "y11_TotHUE",
                         "y11_TotOccHUE",
                         "y11_OwnOccPct",
                         "y11_RentOccPct",
                         "y11_2ndHomePct")]
summary(y2011acs_2)

## y2012 ####
names(y2012acs)
head(y2012acs$GEOID)
y2012acs$GZCTA <- substr(y2012acs$GEOID,3,7)
y2012acs$GZCTA <- str_c("G",y2012acs$GZCTA)
head(y2012acs$GZCTA)

y2012acs_2 <- y2012acs[c("GZCTA",
                         "y12_TotPopE",
                         "y12_WhitePct",
                         "y12_BlackPct",
                         "y12_HispPct",
                         "y12_MHVadj",
                         "y12_MHIadj",
                         "y12_PovPct", 
                         "y12_TotHUE",
                         "y12_TotOccHUE",
                         "y12_OwnOccPct",
                         "y12_RentOccPct",
                         "y12_2ndHomePct")]
summary(y2012acs_2)

## y2013 ####
names(y2013acs)
head(y2013acs$GEOID)
range(y2013acs$GEOID)
y2013acs$GZCTA <- str_c("G",y2013acs$GEOID)
head(y2013acs$GZCTA)

y2013acs_2 <- y2013acs[c("GZCTA",
                         "y13_TotPopE",
                         "y13_WhitePct",
                         "y13_BlackPct",
                         "y13_HispPct",
                         "y13_MHVadj",
                         "y13_MHIadj",
                         "y13_PovPct", 
                         "y13_TotHUE",
                         "y13_TotOccHUE",
                         "y13_OwnOccPct",
                         "y13_RentOccPct",
                         "y13_2ndHomePct")]

summary(y2013acs_2)

## y2014 ####
names(y2014acs)
head(y2014acs$GEOID)
range(y2014acs$GEOID)
y2014acs$GZCTA <- str_c("G",y2014acs$GEOID)
head(y2014acs$GZCTA)

y2014acs_2 <- y2014acs[c("GZCTA",
                         "y14_TotPopE",
                         "y14_WhitePct",
                         "y14_BlackPct",
                         "y14_HispPct",
                         "y14_MHVadj",
                         "y14_MHIadj",
                         "y14_PovPct", 
                         "y14_TotHUE",
                         "y14_TotOccHUE",
                         "y14_OwnOccPct",
                         "y14_RentOccPct",
                         "y14_2ndHomePct")]
summary(y2014acs_2)

## y2015 ####
names(y2015acs)
head(y2015acs$GEOID)
range(y2015acs$GEOID)
y2015acs$GZCTA <- str_c("G",y2015acs$GEOID)
head(y2015acs$GZCTA)

y2015acs_2 <- y2015acs[c("GZCTA",
                         "y15_TotPopE",
                         "y15_WhitePct",
                         "y15_BlackPct",
                         "y15_HispPct",
                         "y15_MHVadj",
                         "y15_MHIadj",
                         "y15_PovPct", 
                         "y15_TotHUE",
                         "y15_TotOccHUE",
                         "y15_OwnOccPct",
                         "y15_RentOccPct",
                         "y15_2ndHomePct")]
summary(y2015acs_2)

## y2016 ####
names(y2016acs)
head(y2016acs$GEOID)
range(y2016acs$GEOID)
y2016acs$GZCTA <- str_c("G",y2016acs$GEOID)
head(y2016acs$GZCTA)

y2016acs_2 <- y2016acs[c("GZCTA",
                         "y16_TotPopE",
                         "y16_WhitePct",
                         "y16_BlackPct",
                         "y16_HispPct",
                         "y16_MHVadj",
                         "y16_MHIadj",
                         "y16_PovPct", 
                         "y16_TotHUE",
                         "y16_TotOccHUE",
                         "y16_OwnOccPct",
                         "y16_RentOccPct",
                         "y16_2ndHomePct")]

summary(y2016acs_2)

## y2017 ####
names(y2017acs)
head(y2017acs$GEOID)
range(y2017acs$GEOID)
y2017acs$GZCTA <- str_c("G",y2017acs$GEOID)
head(y2017acs$GZCTA)

y2017acs_2 <- y2017acs[c("GZCTA",
                         "y17_TotPopE",
                         "y17_WhitePct",
                         "y17_BlackPct",
                         "y17_HispPct",
                         "y17_MHVadj",
                         "y17_MHIadj",
                         "y17_PovPct", 
                         "y17_TotHUE",
                         "y17_TotOccHUE",
                         "y17_OwnOccPct",
                         "y17_RentOccPct",
                         "y17_2ndHomePct")]

summary(y2017acs_2)

## y2018 ####
names(y2018acs)
head(y2018acs$GEOID)
range(y2018acs$GEOID)
y2018acs$GZCTA <- str_c("G",y2018acs$GEOID)
head(y2018acs$GZCTA)

y2018acs_2 <- y2018acs[c("GZCTA",
                         "y18_TotPopE",
                         "y18_WhitePct",
                         "y18_BlackPct",
                         "y18_HispPct",
                         "y18_MHVadj",
                         "y18_MHIadj",
                         "y18_PovPct", 
                         "y18_TotHUE",
                         "y18_TotOccHUE",
                         "y18_OwnOccPct",
                         "y18_RentOccPct",
                         "y18_2ndHomePct")]
summary(y2018acs_2)

## y2019 ####
names(y2019acs)
head(y2019acs$GEOID)
range(y2019acs$GEOID)
y2019acs$GZCTA <- str_c("G",y2019acs$GEOID)
head(y2019acs$GZCTA)

y2019acs_2 <- y2019acs[c("GZCTA",
                         "y19_TotPopE",
                         "y19_WhitePct",
                         "y19_BlackPct",
                         "y19_HispPct",
                         "y19_MHVadj",
                         "y19_MHIadj",
                         "y19_PovPct", 
                         "y19_TotHUE",
                         "y19_TotOccHUE",
                         "y19_OwnOccPct",
                         "y19_RentOccPct",
                         "y19_2ndHomePct")]
summary(y2019acs_2)

## y2020 ####
names(y2020acs)
head(y2020acs$GEOID)
range(y2020acs$GEOID)
y2020acs$GZCTA <- str_c("G",y2020acs$GEOID)
head(y2020acs$GZCTA)

y2020acs_2 <- y2020acs[c("GZCTA",
                         "y20_TotPopE",
                         "y20_WhitePct",
                         "y20_BlackPct",
                         "y20_HispPct",
                         "y20_MHVE",
                         "y20_MHIE",
                         "y20_PovPct", 
                         "y20_TotHUE",
                         "y20_TotOccHUE",
                         "y20_OwnOccPct",
                         "y20_RentOccPct",
                         "y20_2ndHomePct")]
summary(y2020acs_2)


## y2021 ####
names(y2021acs)
head(y2021acs$GEOID)
range(y2021acs$GEOID)
y2021acs$GZCTA <- str_c("G",y2021acs$GEOID)
head(y2021acs$GZCTA)

y2021acs_2 <- y2021acs[c("GZCTA",
                         "y21_TotPopE",
                         "y21_WhitePct",
                         "y21_BlackPct",
                         "y21_HispPct",
                         "y21_MHVadj",
                         "y21_MHIadj",
                         "y21_PovPct", 
                         "y21_TotHUE",
                         "y21_TotOccHUE",
                         "y21_OwnOccPct",
                         "y21_RentOccPct",
                         "y21_2ndHomePct")]
summary(y2021acs_2)

## y2022 ####
names(y2022acs)
head(y2022acs$GEOID)
range(y2022acs$GEOID)
y2022acs$GZCTA <- str_c("G",y2022acs$GEOID)
head(y2022acs$GZCTA)

y2022acs_2 <- y2022acs[c("GZCTA",
                                 "y22_TotPopE",
                                 "y22_WhitePct",
                                 "y22_BlackPct",
                                 "y22_HispPct",
                                 "y22_MHVadj",
                                 "y22_MHIadj",
                                 "y22_PovPct", 
                                 "y22_TotHUE",
                                 "y22_TotOccHUE",
                                 "y22_OwnOccPct",
                                 "y22_RentOccPct",
                                 "y22_2ndHomePct")]
summary(y2022acs_2)

#JOINING TABLES####
dim(y2000dec_2)
dim(y2010dec)
dim(y2011acs_2)
dim(y2012acs_2)
dim(y2013acs_2)


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

##joining population density data derived from Social Explorer #### 
SE_PD <- read.csv("C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census/SE_PopDense00_22.csv")

TS_ZCTA_13 <- left_join(TS_ZCTA_12, SE_PD, by="GZCTA", copy=F)

#checking Time series data ####
names(TS_ZCTA_13)
colSums(is.na(TS_ZCTA_13))

#subsetting to Ortley Beach ####
OB_TS_ZCTA <- subset(TS_ZCTA_13, GZCTA=="G08751")



#writing out files ####
path1 <- 'C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census'
write.csv(TS_ZCTA_13, file.path(path1, "TS_ZCTA_Tidy.csv"), row.names=TRUE)
write.csv(OB_TS_ZCTA, file.path(path1, "OB_TS_ZCTA_Tidy.csv"), row.names=TRUE)
