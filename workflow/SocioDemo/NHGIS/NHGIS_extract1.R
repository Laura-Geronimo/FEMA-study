# Pull NHGIS extract 1

#Vars of interest:
    #Total Population
    #Population Density
    #White %: 
    #Median Home Value: 
    #Median Household Income:
    #Tenure: % Owner occupied
    #InUrbanHUPct: 
    #MedyrBuilt: 
    #ESL %: 


# Setup 
getwd()

#libraries
library(devtools)
library(ipumsr)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(data.table)

## Search Metadata - all datasets ####
nhgis_ds <- get_metadata_nhgis("datasets")
nhgis_ds

## find the name of the table you are interested in ####
nhgis_ds |>
  filter(group %like% "1990 Census") |>
  select(name, description)

## select your data table & examine metadata ####
ds_meta90 <- get_metadata_nhgis(dataset = "1990_STF1")
str(ds_meta90,1)

## Filter for descriptions that = "Population" & decide what tables you want ####
ds_meta90$data_tables |>
  filter(str_detect(description, "Population"))
ds_meta22$data_tables |>
  filter(str_detect(description, "land area"))
head(ds_meta22$geographic_instances)

## check geographic levels ####
ds_meta22$geog_levels
contains_zcta <- "zcta" %in% unlist(ds_meta22$geog_levels)

## Define an extract request ####
nhgis_ext22 <- define_extract_nhgis(
  description = "2018_2022_ACS5a",    # you define the description
  datasets = ds_spec(  #this helps to bundle parameters 
    "2018_2022_ACS5a",
    data_tables = c("B01003"),
    geog_levels= c("state", "county", "zcta")
  )
)

## submit an extract request ####
nhgis_ext22 <- submit_extract(nhgis_ext22)

## Check the status of your request (check code from other tutorial) 
nhgis_ext22 <- wait_for_extract(nhgis_ext22) #this ensures that you wait until the request is complete to download

## set destination file & download ####
zip_file <- "C:/Users/lgero/Box/Research/FEMA_project/Data/NHGIS"
nhgis_files_test <- download_extract(nhgis_ext22, zip_file)

############## TESTING ####

nhgis_cnty22 <- read_nhgis("C:/Users/lgero/Box/Research/FEMA_project/Data/NHGIS/nhgis0022_csv.zip", file_select = "nhgis0022_csv/nhgis0022_ds262_20225_county.csv")

