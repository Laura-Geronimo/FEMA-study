#Purpose: pull time series data on socio-demographics from NHGIS


# Setup 
getwd()

#libraries
library(devtools)
library(ipumsr)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)

my_key <- "59cba10d8a5da536fc06b59dbccff59c2d48411e80516bfd02a2e925"

#TUTORIAL #1 ####
#Following: https://developer.ipums.org/docs/v2/workflows/explore_metadata/nhgis/datasets/

## get metadata for all datasets ####

url <- "https://api.ipums.org/metadata/nhgis/datasets?version=2"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, n = 20L) # Contains metadata

## get detailed metadata for a single dataset ####

url <- "https://api.ipums.org/metadata/nhgis/datasets/1790_cPop?version=2"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, n = 20L) # Contains metadata
res_df$name #The unique identifier of the dataset.
res_df$group #The group of datasets to which this dataset belongs.
res_df$description #A short description of the dataset.
res_df$sequence #The order in which the dataset will appear in the metadata API and extracts.
res_df$hasMultipleDataTypes #A boolean indicating if multiple data types exist for this dataset. For example, the American Community Survey datasets have margins of error as well as estimate data types. 
res_df$dataTables #A list of data tables available for this dataset.
res_df$geogLevels #A list of geographic levels available for this dataset

## get metadata for a table ####
url <- "https://api.ipums.org/metadata/nhgis/datasets/1990_STF1/data_tables/NP19?version=2"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, n = 20L) # Contains metadata

## get time Series Metadata ####
url <- "https://api.ipums.org/metadata/nhgis/time_series_tables?version=2&pageSize=10"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, n = 20L) # Contains metadata

## get shapefile metadata ####
url <- "https://api.ipums.org/metadata/nhgis/shapefiles?version=2"
result <- GET(url, add_headers(Authorization = my_key))
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
head(res_df, n = 20L) # Contains metadata

## creating Extracts ####

url <- "https://api.ipums.org/extracts/?collection=nhgis&version=2"
mybody <- '

{
  "datasets": {
    "1988_1997_CBPa": {
      "years": ["1988", "1989", "1990", "1991", "1992", "1993", "1994"],
      "breakdownValues": ["bs30.si0762", "bs30.si2026"],
      "dataTables": [
        "NT001"
      ],
      "geogLevels": [
        "county"
      ]
    },
    "2000_SF1b": {
      "dataTables": [
        "NP001A"
      ],
      "geogLevels": [
        "blck_grp"
      ]
    }
  },
  "timeSeriesTables": {
    "A00": {
      "geogLevels": [
        "state"
      ],
      "years": [
        "1990"
      ]
    }
  },
  "shapefiles": [
    "us_state_1790_tl2000"
  ],
  "timeSeriesTableLayout": "time_by_file_layout",
  "geographicExtents": ["010"],
  "dataFormat": "csv_no_header",
  "description": "example extract request",
  "breakdownAndDataTypeLayout": "single_file"
}

'
mybody_json <- fromJSON(mybody, simplifyVector = FALSE)
result <- POST(url, add_headers(Authorization = my_key), body = mybody_json, encode = "json", verbose())
res_df <- content(result, "parsed", simplifyDataFrame = TRUE)
my_number <- res_df$number

## get a request's Status ####
data_extract_status_res <- GET("https://api.ipums.org/extracts/6?collection=nhgis&version=2", add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df

## retrieving your extract ####
data_extract_status_res <- GET("https://api.ipums.org/extracts/6?collection=nhgis&version=2", add_headers(Authorization = my_key))
des_df <- content(data_extract_status_res, "parsed", simplifyDataFrame = TRUE)
des_df$downloadLinks

## download table data and read into a data frame ####

## destination file

zip_file <- "NHGIS_tables.zip"

## Download extract to destination file

download.file(des_df$downloadLinks$tableData, zip_file, headers=c(Authorization=my_key))

## List extract files in ZIP archive
unzip(zip_file, list=TRUE)
#3 Read 2000 block-group CSV file into a data frame
bg2000_table <- read_nhgis(zip_file, data_layer = contains("2000_blck_grp.csv"))
head(bg2000_table)


#TUTORIAL #2 ####
## Following YOUTUBE Tutorial 
#https://www.youtube.com/watch?v=U3tyFxxFYz8
set_ipums_api_key("59cba10d8a5da536fc06b59dbccff59c2d48411e80516bfd02a2e925", save=TRUE) #means you don't need to input key. Saves to your environment.

#Goal: examine racial inequities in homeownership rates
#Data: 2017 ACS 5-year
#Geography: county
#Method: 
  #calculate proportion of owner-occupied households to total households for each race/ethnicity group
  #calculate ratio of homeowner proportions between non-Hispanic white households and other race/ethnicity households
#disparity between non-Hispanic whites and other groups.

## Search Metadata - all datasets ####
nhgis_ds <- get_metadata_nhgis("datasets")

## find the name of the table you are interested in ####
nhgis_ds |>
  filter(group=="2017 American Community Survey") |>
  select(name, description)

## select your data table & examine metadata ####
ds_meta <- get_metadata_nhgis(dataset = "2013_2017_ACS5a")
str(ds_meta,1)

## Filter for descriptions that = "Tenure" & decide what tables you want ####
ds_meta$data_tables |>
  filter(str_detect(description, "Tenure"))
ds_meta$geog_levels

## Define an extract request ####
nhgis_ext <- define_extract_nhgis(
  description = "2017 ACS Tenure by race and ethnicity",    # you define the description
  datasets = ds_spec(  #this helps to bundle parameters 
    "2013_2017_ACS5a",
    data_tables = c("B25003B","B25003D", "B25003H", "B25003I"),
    geog_levels= c("county")
  )
)

## submit an extract request ####
nhgis_ext <- submit_extract(nhgis_ext)

## Check the status of your request (check code from other tutorial) 

nhgis_ext <- wait_for_extract(nhgis_ext) #this ensures that you wait until the reques is complete to download

## set destination file

zip_file <- "C:/Users/lgero/Box/Research/FEMA_project/Data/NHGIS"

nhgis_files_test <- download_extract(nhgis_ext, zip_file)

## load an NHGIS extract ####
basename(nhgis_files)
nhgis_data <- read_nhgis("C:/Users/lgero/Box/Research/FEMA_project/Data/NHGIS/nhgis0019_csv.zip")

## get info on a particular variable in data ####
#option 1
ipums_var_info(nhgis_data$AH39E001) #possible that you will still get ambiguous results

#option 2 - likely the better option
get_metadata_nhgis(dataset = "2013_2017_ACS5a", data_table = "B25003H")

## calculate Homeownership disparities ####
nhgis_data <- nhgis_data |>
  mutate(
    ho_prop_wanh = AH4FE002 / AH4FE001, #owner ocupied hh for white non hispanic / total owner occ households
    ho_prop_ba = AH39E002 / AH39E001,
    ho_prop_aa = AH4BE002 / AH4BE001,
    ho_prop_h = AH4GE002 / AH4GE001
  ) |>
  mutate(                                       #getting proportions
    ho_ratio_wanh_ba = ho_prop_wanh / ho_prop_ba,
    ho_ratio_wanh_h = ho_prop_wanh / ho_prop_h,
    ho_ratio_wanh_aa = ho_prop_wanh / ho_prop_aa
  ) |>
  select(YEAR, STATEA, STATE, COUNTY, starts_with("ho_ratio"))

nhgis_data

nhgis_data

##Flexible Workflow (More Flexible) ####

## create one file for data extraction ####
#Define an extract request 
nhgis_ext <- define_extract_nhgis(
  description = "2017 ACS Tenure by race and ethnicity",    # you define the description
  datasets = ds_spec(  #this helps to bundle parameters 
    "2013_2017_ACS5a",
    data_tables = c("B25003B","B25003D", "B25003H", "B25003I"),
    geog_levels= c("county")
  )
) |>
  submit_extract()|>
  wait_for_extract()|>
  download_extract("data/nhgis")

##create another file for data load and processing ####
file <- "data/nhgis/nhgis1307_csv.zip" #careful here! if you change your output data, read in good data
nhgis_data <- read_nhgis(nhgis_files)

read_nhgis(nhgis_data) |>
  mutate(
    ho_prop_wanh = AH4FE002 / AH4FE001, #owner ocupied hh for white non hispanic / total owner occ households
    ho_prop_ba = AH39E002 / AH39E001,
    ho_prop_aa = AH4BE002 / AH4BE001,
    ho_prop_h = AH4GE002 / AH4GE001
  ) |>
  mutate(                                       #getting proportions
    ho_ratio_wanh_ba = ho_prop_wanh / ho_prop_ba,
    ho_ratio_wanh_h = ho_prop_wanh / ho_prop_h,
    ho_ratio_wanh_aa = ho_prop_wanh / ho_prop_aa
  ) |>
  select(YEAR, STATEA, STATE, COUNTY, starts_with("ho_ratio"))

nhgis_data




