#-------------------------------------------------------------------------------------#
# Project: DNAsimpleDash
# Purpose: Update DB
# Author: Artemio Sison III
# R Version: 4.0.1 "See Things Now"
#-------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------#
# Load Dependencies
#-------------------------------------------------------------------------------------#
library(dplyr)
## for reading in csv
library(readr)

## dependencies for demographics
# library(zipcode) # not available in newest R version 4.0.1
## alternative zipcodes package https://www.gavinrozzi.com/post/an-r-package-for-zip-codes/
# library(zipcodeR)
library(usa) # resource https://stackoverflow.com/questions/50845838/map-zip-codes-to-their-respective-city-and-state-in-r
library(ggthemes)
library(maps)
library(viridis)
# devtools::install_github("hrbrmstr/albersusa")
library(albersusa)

#-------------------------------------------------------------------------------------#
# Read data 
#-------------------------------------------------------------------------------------#

## read in user data
users <- read.csv("../data_22Mar21/users_22Mar21.csv")
## read in users with conditions
users_conditions <- read.csv("../data_22Mar21/user_with_conditions_22Mar21.csv")
## read in conditions
conditions <-  read.csv("../data_22Mar21/conditions_22Mar21.csv")

#-------------------------------------------------------------------------------------#
# Clean data 
#-------------------------------------------------------------------------------------#

### users - select relevant columns
users <- users %>%
  select(user_id, city, state, zip_code, country, gender, ethnicity, height_feet, height_inches,
         birth_year, is_self_afflicted, is_carrier, is_genetic_testing)

### users_with_conditions - select relevant columns
users_conditions <- users_conditions %>%
  select(id, runs_in_family, years_present, diagnosed_by_physician, takes_medication, additional_details, 
         condition_id, user_id)

### conditions - select relevant columns
conditions <- conditions %>%
  select(condition_id, name, aact_name_1, aact_name_2, aact_name_3, aact_name_4, aact_name_5)


#-------------------------------------------------------------------------------------#
# Link tables 
#-------------------------------------------------------------------------------------#

## join users_with_conditions and conditions
condition.users_join <- left_join(users_conditions, conditions, by = 'condition_id')

## join with users table
condition.users_join <- left_join(condition.users_join, users, by = "user_id")

## write to csv
write.csv(condition.users_join, "../data_22Mar21/mergeDB_22Mar21.csv", row.names = FALSE)


#-------------------------------------------------------------------------------------#
# Create Additional Columns for Shiny
#-------------------------------------------------------------------------------------#

#---------------------------------------------#
# Demographics
# resource here: https://austinwehrwein.com/digital-humanities/creating-a-density-map-in-r-with-zipcodes/
#---------------------------------------------#

## add age
condition.users_join <- condition.users_join %>%
  mutate(age = 2020 - birth_year,
         age.group = ifelse(age <= 19, "0-19",
                            ifelse(age > 19 & age <= 29, "20-29",
                                   ifelse(age > 29 & age <= 39, "30-39",
                                          ifelse(age > 39 & age <= 49, "40-49",
                                                 ifelse(age > 49 & age <= 59, "50-59",
                                                        ifelse(age > 59 & age <= 69, "60-69",
                                                               ifelse(age > 69 & age <= 79, "70-79", 
                                                                      ifelse(age > 80 & age <= 89, "80-89",
                                                                             ifelse(age > 90 & age < 100, "90-99", NA))))))))))

## classify user race by ethnicity
condition.users_join <- condition.users_join %>%
  mutate(race = 
           if_else(ethnicity == 'EAST_ASIAN' |
                     ethnicity == 'SOUTHEAST_ASIAN' | 
                     ethnicity == 'MIXED_ASIAN_AND_WHITE'|
                     ethnicity == 'WEST_ASIAN',
                   'ASIAN',
                   if_else(ethnicity == 'WHITE_EUROPEAN' |
                             ethnicity == 'WHITE_HISPANIC'|
                             ethnicity == 'MIXED_ASIAN_AND_WHITE' |
                             ethnicity == 'WHITE_NORTH_AFRICAN'|
                             ethnicity == 'MIXED_BLACK_AND_WHITE' |
                             ethnicity == 'JEWISH_ASHENKAZI',
                           'WHITE',
                           if_else(ethnicity == 'MIXED_BLACK_AND_HISPANIC' |
                                     ethnicity == 'WHITE_HISPANIC'|
                                     ethnicity == 'HISPANIC_OTHER'|
                                     ethnicity == 'HISPANIC_CARIBBEAN',
                                   'HISPANIC',
                                   if_else(ethnicity == 'BLACK_AMERICAN' |
                                             ethnicity == 'BLACK_AFRICAN'|
                                             ethnicity == 'MIXED_BLACK_AND_WHITE'|
                                             ethnicity == 'MIXED_BLACK_AND_HISPANIC' |
                                             ethnicity == 'BLACK_CARIBBEAN',
                                           'BLACK',
                                           if_else(ethnicity == 'MIXED_BLACK_AND_HISPANIC' |
                                                     ethnicity == 'WHITE_HISPANIC'|
                                                     ethnicity == 'HISPANIC_OTHER'|
                                                     ethnicity == 'HISPANIC_CARIBBEAN',
                                                   'HISPANIC',
                                                   if_else(ethnicity == 'AMERINDIAN' |
                                                             ethnicity == 'ARABIC'|
                                                             ethnicity == 'NATIVE_AMERICAN'|
                                                             ethnicity == 'MIXED_OTHER',
                                                           'OTHER', 'OTHER')))))))

## height
condition.users_join <- condition.users_join %>%
  mutate(height_cm = 2.54 * (height_inches + (height_feet * 12)) )

#---------------------------------------------#
# Location/Coordinate Generation
#---------------------------------------------#

## obtain us zip codes
us_zipcodes <-  usa::zipcodes %>% 
  rename(latitude = lat,
         longitude = long,
         zip_code = zip) %>%
  select(zip_code, latitude, longitude)

## merge with masterDB
condition.users_join <- left_join(condition.users_join, us_zipcodes, by = "zip_code") 

## rename zip code column name
condition.users_join <- condition.users_join %>% 
  rename(zip = zip_code)


#-------------------------------------------------------------------------------------#
# Additional Parameters to consider? 
#-------------------------------------------------------------------------------------#

## write to csv before selecting columns present in current shiny data
write.csv(condition.users_join, "../data_22Mar21/shinydata_22Mar21.csv", row.names = FALSE)

## select columns based on shinydata
shinydata <- read.csv('../DNAsimpleDash/shinydata.csv')

## select columns
mergedDB_select <- condition.users_join %>%
  select(names(shinydata))

## write to csv
write.csv(mergedDB_select, "../data_22Mar21/mergedDB_22Mar21.csv", row.names = FALSE)
