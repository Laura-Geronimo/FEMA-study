#purpose: Cleaning 1990 NHGIS data pull

# Setup
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)


# Importing data
y1990 <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/NHGIS/NHGIS_1990/nhgis0023_csv/nhgis0023_csv/nhgis0023_ts_geog2010_zcta.csv')
y1990_hisp <- read.csv('H:/My Drive/Scarlet/Academic/Research/Data/NHGIS/NHGIS_1990/nhgis0024_csv/nhgis0024_csv/nhgis0024_ts_geog2010_zcta.csv')

#subsetting to relevant vars via codebook
names(y1990)
y1990b <- y1990 %>%
  select(GISJOIN,
        CL8AA1990, # 1990 Persons Total
        CW7AA1990, #1990: Persons: Not Hispanic or Latino ~ White (single race)
        CW7AB1990, #1990: Persons: Not Hispanic or Latino ~ Black or African American (single race)
        CM7AA1990,   #1990: Housing units: Total
        CM9AA1990,   #1990: Housing units: Occupied
        CM9AB1990,   #1990: Housing units: Vacant
        CN0AD1990,   #1990: Housing units: Vacant--Seasonal, recreational, or occasional use
        CN1AA1990,   #1990: Housing units: Owner occupied
        CN1AB1990)   #1990: Housing units: Renter occupied

names(y1990_hisp)
y1990_hispb <- y1990_hisp %>%
  select(GISJOIN,
         CP4AB1990)   #1990: Persons: Hispanic or Latino

y1990dec <- left_join(y1990b, y1990_hispb, by="GISJOIN")

#renaming ####
names(y1990dec)[names(y1990dec)=="GISJOIN"] <- "GZCTA"
names(y1990dec)[names(y1990dec)=="CL8AA1990"] <- "y90_TotPop"
names(y1990dec)[names(y1990dec)=="CW7AA1990"] <- "y90_White"
names(y1990dec)[names(y1990dec)=="CW7AB1990"] <- "y90_Black"  
names(y1990dec)[names(y1990dec)=="CP4AB1990"] <- "y90_Hisp"  
names(y1990dec)[names(y1990dec)=="CM7AA1990"] <- "y90_TotHU" 
names(y1990dec)[names(y1990dec)=="CM9AA1990"] <- "y90_TotOcc_HU"  
names(y1990dec)[names(y1990dec)=="CM9AB1990"] <- "y90_TotHU_VS" 
names(y1990dec)[names(y1990dec)=="CN0AD1990"] <- "y90_2ndHome"  
names(y1990dec)[names(y1990dec)=="CN1AA1990"] <- "y90_OwnOcc"  
names(y1990dec)[names(y1990dec)=="CN1AB1990"] <- "y90_RentOcc"  

#creating Percents ####
y1990dec$y90_WhitePct <- y1990dec$y90_White / y1990dec$y90_TotPop
y1990dec$y90_BlackPct <- y1990dec$y90_Black / y1990dec$y90_TotPop
y1990dec$y90_HispPct <- y1990dec$y90_Hisp / y1990dec$y90_TotPop

y1990dec$y90_2ndHomePct <- y1990dec$y90_2ndHome / y1990dec$y90_TotHU
y1990dec$y90_OwnOccPct <- y1990dec$y90_OwnOcc / y1990dec$y90_TotOcc_HU
y1990dec$y90_RentOccPct <- y1990dec$y90_RentOcc / y1990dec$y90_TotOcc_HU

#Selecting Final vars ####
names(y1990dec)
y1990dec <- y1990dec %>%
  select(GZCTA,
         y90_TotPop,
         y90_WhitePct,
         y90_BlackPct,
         y90_HispPct,
         y90_TotHU,
         y90_TotOcc_HU,
         y90_2ndHomePct,
         y90_OwnOccPct,
         y90_RentOccPct
         )

#writing out files ####
path1 <- 'C:/Users/lgero/Box/Research/FEMA_project/Data/Edited/Census'
write.csv(y1990dec, file.path(path1, "y1990dec.csv"), row.names=TRUE)


