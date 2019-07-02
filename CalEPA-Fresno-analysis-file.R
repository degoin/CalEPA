
rm(list=ls())
# CalEPA analysis
library(tidyverse)
library(readxl)

# read in raw data
df <- read.csv('/Users/danagoin/Documents/CalEPA/fresno_exposure_fmt.csv')
dim(df)


# prepare data for geocoding 
df_geocode <- df %>% select(enrollment_id, street, city, city_sp, zip)

# remove apartment numbers from address files for geocoding 
df_geocode$address <- gsub("Apt.*", "", df_geocode$street, ignore.case = T)
df_geocode$address <- gsub("#.*", "", df_geocode$address)
df_geocode$address <- gsub("Space.*", "", df_geocode$address)
df_geocode$address <- gsub("Spc.*", "", df_geocode$address, ignore.case = T)
df_geocode$address <- gsub("Sp. 29", "", df_geocode$address)

# add city, state, and zipcode 
df_geocode$address <- ifelse(df_geocode$city==1, paste(df_geocode$address, "Fresno CA", df_geocode$zip), 
                             paste(df_geocode$address, df_geocode$city_sp, "CA", df_geocode$zip))

df_geocode <- df_geocode %>% select(enrollment_id, address)

# delete missing addresses
dim(df)
dim(df_geocode)
df_geocode <- df_geocode %>% filter(!is.na(address))
dim(df_geocode)

write.csv(df_geocode, file='/Users/danagoin/Documents/CalEPA/fresno_addresses.csv', row.names = F)

# merge on geocoding -- originally did as suggested from ECHO Data Analysis center, but I think 
# their census tract mapping files were from 2000 rather than 2010, so used the DeGAUSS 
# census tracts instead because 2010 tracts are used in CalEnviroscreen 
# to do this just substitute degauss/census_tracts for echodat/census_tracts in terminal command

df_degauss <- read.csv('/Users/danagoin/Documents/CalEPA/fresno_addresses_geocoded_censustracts.csv')
df_g <- df_degauss %>% select(enrollment_id, tract, lon, lat)
dim(df_g)


# merge back to main data file 

df_calepa <- left_join(df, df_g)

# read in CalEnviroScreens data 

df_ces <- read_xlsx('/Users/danagoin/Documents/CalEPA/ces3results.xlsx')
names(df_ces)[names(df_ces)=="Census Tract"] <- "tract"

# merge onto women's data 

df_calepa <- left_join(df_calepa, df_ces, by="tract")
dim(df_calepa)


test <- df_calepa %>% filter(is.na(`Total Population`))
dim(test)

