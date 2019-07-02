
rm(list=ls())
# CalEPA analysis
library(tidyverse)


# read in raw data
df <- read.csv('/Users/danagoin/Documents/CalEPA/fresno_exposure_fmt.csv')
dim(df)
View(df)


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

write.csv(df_geocode, file='/Users/danagoin/Documents/CalEPA/fresno_address_geocoding.csv', row.names = F)

