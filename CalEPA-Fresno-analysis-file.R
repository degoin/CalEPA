
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

# merge CalEnviroScreen data with women's data 

df_calepa <- left_join(df_calepa, df_ces, by="tract")
dim(df_calepa)

# keep variables you need 

df_calepa <- df_calepa %>% select(enrollment_id, clinic_prenatal, clinic_prenatal_sp, date_visit, date_wb_return, date_wb_sent, 
                                  wb_return, wb_no_return_sp, date_svfu_expected, blood_m, blood_m_miss_reason, 
                                  urine_m, urine_m_miss_reason, blood_uc, blood_uc_miss_reason, date_entry_cl, 
                                  maternal_dob, maternal_age, maternal_edu, emp_status_m, emp_status_m_sp,
                                  emp_status_m_ftpt, marital, partner_emp_status, hh_inc, hh_number, healthcare, 
                                  medical, hispanic_f, hispanic_m, race_m___1, race_m___2, race_m___3, race_m___4, 
                                  race_m___5, race_m___66, race_m___77, race_m___88, race_msp, race_f___1, race_f___2, 
                                  race_f___3, race_f___4, race_f___5, race_f___66, race_f___77, race_f___88, 
                                  race_fsp, water_source, drink_tap, tap_no_ws, filter_water, tap_cooking, 
                                  tap_cooking_no, blood_m_coldate, blood_m_coltime, blood_m_rt_vol, blood_m_rt_n,
                                  blood_m_pt_vol, blood_m_pt_n,  blood_uc_coldate, blood_uc_coltime, blood_uc_rt_n, 
                                  blood_uc_rt_vol, blood_uc_pt_n, blood_uc_pt_vol, urine_m_coldate, urine_m_coltime, 
                                  urine_m_vol, urine_m_lastvoidtime, date_birth_c, id_echo_m, date_birth_m_mr, 
                                  ethnicity_m_mr, race_m_mr, race_m_sp_mr, cob_m_mr, insurance_m_mr, language_m_mr, language_m_sp_mr, 
                                  wic_mr, pnc_start_mr, lmp_mr, lmp_miss_mr, lmp_source_mr, prepreg_wt_mr, prepreg_wt_unit_mr, 
                                  height_m_unit, height_m_unit_mr, chart_prepreg_bmi, gravidity_mr, parity_mr, livebirths_n_mr, 
                                  mscarriage_n_mr, stillbirths_n_mr, prematurebirths_n_mr, n_lbwt_mr, diabetes_fhx_mr, 
                                  hypertension_fhx_mr, twins_fhx_mr, preg_hypertension_mr, preeclampsia_cp_mr, gdm_cp_mr, 
                                  travel_cp_mr, infections_cp_mr, infections_cp_sp_mr, pncomp_mr, pncomp_sp_mr, 
                                  pnv_mr, vit_mr, antibiotics_mr, iron_mr, remedies_mr, laxatives_mr, antacids_mr, othermeds_mr, 
                                  other_meds_sp_mr, preg_sub_use_mr, preg_sub_use_desc_mr, smoke_ever_mr, smoke_stop_mr, 
                                  smoke_stop_unit_mr, smoke_stop_unit_sp_mr, smoke_amt_mr, smoke_amt_unit_mr, smoke_amt_per_mr, 
                                  date_admit_ld_mr, age_delivery_mr, wt_delivery_mr, wt_delivery_unit_mr, bmi_delivery_mr, 
                                  labor_mr, delivery_method_mr, csec_plan_mr, csec_type_mr, date_birth_c_mr, ga_weeks_mr, 
                                  ga_miss_mr, sex_c_mr, ethnicity_c_mr, race_c_mr, race_c_mr_sp, wt_birth_unit_mr, wt_birth_mr, 
                                  length_birth_unit_mr, length_birth_mr, length_birth_missing_mr, apgar1_mr, apgar5_mr, apgar10_mr, 
                                  nicu_adm_mr___0, nicu_adm_mr___1, nicu_adm_mr___9, nicu_adm_days_mr, discharge_dx_c_mr, discharge_dx_c_sp_mr, 
                                  notes_mr, tract, lon, lat, `Total Population`, `California County`, ZIP, `Nearby City \r\n(to help approximate location only)`, 
                                  Longitude, Latitude,`CES 3.0 Score`, `CES 3.0 Percentile`, `CES 3.0 \r\nPercentile Range`, `SB 535 Disadvantaged Community`, 
                                  Ozone, `Ozone Pctl`, PM2.5, `PM2.5 Pctl`, `Diesel PM`, `Diesel PM Pctl`, `Drinking Water`, 
                                  `Drinking Water Pctl`, Pesticides, `Pesticides Pctl`, `Tox. Release`, `Tox. Release Pctl`, 
                                  Traffic, `Traffic Pctl`, `Cleanup Sites`, `Cleanup Sites Pctl`, `Groundwater Threats`, 
                                  `Groundwater Threats Pctl`, `Haz. Waste`, `Haz. Waste Pctl`, `Imp. Water Bodies`, `Imp. Water Bodies Pctl`, 
                                  `Solid Waste`, `Solid Waste Pctl`, `Pollution Burden`, `Pollution Burden Pctl`, `Pollution Burden Score`,
                                  Asthma, `Asthma Pctl`, `Low Birth Weight`, `Low Birth Weight Pctl`, `Cardiovascular Disease`, 
                                  `Cardiovascular Disease Pctl`, Education, `Education Pctl`, `Linguistic Isolation`, `Linguistic Isolation Pctl`, 
                                  Poverty, `Poverty Pctl`, Unemployment, `Unemployment Pctl`, `Housing Burden`, `Housing Burden Pctl`, 
                                  `Pop. Char.`, `Pop. Char. Pctl`, `Pop. Char. Score`)


dim(df_calepa)
                                           

# produce descriptive statistics 

# first recode and reclassify variables as needed 
table(df_calepa$maternal_age, exclude=NULL)


df_calepa$agecat <- ifelse(df_calepa$maternal_age<20,1,
                           ifelse(df_calepa$maternal_age>=20 & df_calepa$maternal_age<25,2,
                                  ifelse(df_calepa$maternal_age>=25 & df_calepa$maternal_age<30,3,
                                         ifelse(df_calepa$maternal_age>=30,4,NA))))
table(df_calepa$agecat, exclude=NULL)       
df_calepa$sb535_disadvanted <- ifelse(df_calepa$`SB 535 Disadvantaged Community`=="Yes", 1, 
                                      ifelse(df_calepa$`SB 535 Disadvantaged Community`=="No",0, NA))

df_calepa$Traffic <- as.numeric(df_calepa$Traffic)
df_calepa$`Traffic Pctl` <- as.numeric(df_calepa$`Traffic Pctl`)
df_calepa$`Low Birth Weight` <- as.numeric(df_calepa$`Low Birth Weight`)
df_calepa$`Low Birth Weight Pctl` <- as.numeric(df_calepa$`Low Birth Weight Pctl`)
df_calepa$Education <- as.numeric(df_calepa$Education)
df_calepa$`Education Pctl` <- as.numeric(df_calepa$`Education Pctl`)
df_calepa$`Linguistic Isolation` <- as.numeric(df_calepa$`Linguistic Isolation`)
df_calepa$`Linguistic Isolation Pctl` <- as.numeric(df_calepa$`Linguistic Isolation Pctl`)
df_calepa$Unemployment <- as.numeric(df_calepa$Unemployment)
df_calepa$`Unemployment Pctl` <- as.numeric(df_calepa$`Unemployment Pctl`)
df_calepa$`Housing Burden` <- as.numeric(df_calepa$`Housing Burden`)
df_calepa$`Housing Burden Pctl` <- as.numeric(df_calepa$`Housing Burden Pctl`)


# maternal education categories 
# 0 - less than high school 
# 1 - high school diploma or GED 
# 2 - some college or AA degree 
# 3 - bachelor's degree
# 4 - master's degree
# 5 - doctoral degree
# 7 - don't know 
# 8 - refused 


# maternal employment status 
# 1 - working for pay 
# 2 - looking for work 
# 3 - retired 
# 4 - homemaker or full time caretaker 
# 5 - student 
# 6 - something else 
# 77 - don't know 
# 88 - refused 

# current marital status 
# 1 - married 
# 2 - partnered 
# 3 - widowed 
# 4 - separated 
# 5 - divorced
# 6 - single 
# 8 - refused 

# household income 
# 1 - less than $5000
# 2 - $5-9000
# 3 - $10-14,999
# 4 - $15-19,999 
# etc 

# health care - covered under health insurance
# 0 - No
# 1 - Yes 
# 77 - don't know 
# 88 - refused 

# medi-cal -- has Medi-Cal for insurance
# 0 - No 
# 1 - Yes
# 77 - don't know 
# 88 - refused 

# race_m 
# 1 - black 
# 2 - asian 
# 3 - native american / alaska native 
# 4 native hawaiian / pacific islander 
# 5 - white 
# 66 - other 
# 77 - don't know 
# 88 - refused 


df_calepa$race_eth_m <- ifelse(df_calepa$hispanic_m==1, "Hispanic", 
                               ifelse(df_calepa$hispanic_m==0 & df_calepa$race_m___1==1,"Black", 
                                      ifelse(df_calepa$hispanic_m==0 & df_calepa$race_m___2==1,"Asian",
                                             ifelse(df_calepa$hispanic_m==0 & df_calepa$race_m___3==1,"American Indian/Alaska Native",
                                                    ifelse(df_calepa$hispanic_m==0 & df_calepa$race_m___4==1, "Native Hawaiian/Pacific Islander",
                                                           ifelse(df_calepa$hispanic_m==0 & df_calepa$race_m___5==1, "White", "Other"))))))

dem_vars <- c("agecat","maternal_edu", "emp_status_m", "marital", "hh_inc", "parity_mr", "wic_mr",
              "drink_tap", "filter_water", "race_eth_m") 


for (i in 1:length(dem_vars)) {
  print(paste("Distribution of CES score by", dem_vars[i]))

tab <- df_calepa %>%  group_by(get(dem_vars[i])) %>%  summarise(n = n(), mean=mean(`CES 3.0 Score`, na.rm=T), sd = sqrt(var(`CES 3.0 Score`, na.rm=T)), 
                                               min = min(`CES 3.0 Score`, na.rm=T), max = max(`CES 3.0 Score`, na.rm=T))

print(tab)
}

for (i in 1:length(dem_vars)) {
  print(paste("Proportion in disadvanted community by", dem_vars[i]))
  
  tab <- df_calepa %>%  group_by(get(dem_vars[i])) %>%  summarise(n = n(), mean=mean(sb535_disadvanted, na.rm=T), sd = sqrt(var(sb535_disadvanted, na.rm=T)), 
                                                                  min = min(sb535_disadvanted, na.rm=T), max = max(sb535_disadvanted, na.rm=T))
  
  print(tab)
}



# make plots 

# CES score 

ggplot(data=df_calepa) + geom_histogram(aes(x=`CES 3.0 Score`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# CES percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`CES 3.0 Percentile`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# ozone score 

ggplot(data=df_calepa) + geom_histogram(aes(x=Ozone), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# ozone percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Ozone Pctl`), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# PM2.5 
ggplot(data=df_calepa) + geom_histogram(aes(x=PM2.5), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# PM2.5 percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`PM2.5 Pctl`), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Diesel PM
ggplot(data=df_calepa) + geom_histogram(aes(x=`Diesel PM`), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Diesel PM percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Diesel PM Pctl`), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Drinking water 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Drinking Water`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Drinking water percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Drinking Water Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Pesticides  
ggplot(data=df_calepa) + geom_histogram(aes(x=Pesticides), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Pesticides percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Pesticides Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Toxic release  
ggplot(data=df_calepa) + geom_histogram(aes(x=`Tox. Release`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Toxic release percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Tox. Release Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Traffic   
ggplot(data=df_calepa) + geom_histogram(aes(x=Traffic), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Traffic percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Traffic Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Cleanup sites   
ggplot(data=df_calepa) + geom_histogram(aes(x=`Cleanup Sites`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Cleanup sites percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Cleanup Sites Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Groundwater threats   
ggplot(data=df_calepa) + geom_histogram(aes(x=`Groundwater Threats`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Groundwater threats percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Groundwater Threats Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Hazardous waste   
ggplot(data=df_calepa) + geom_histogram(aes(x=`Haz. Waste`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Hazardous waste percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Haz. Waste Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Imp water bodies    
ggplot(data=df_calepa) + geom_histogram(aes(x=`Imp. Water Bodies`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Imp water bodies percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Imp. Water Bodies Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Solid waste   
ggplot(data=df_calepa) + geom_histogram(aes(x=`Solid Waste`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Solid waste percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Solid Waste Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Pollution burden   
ggplot(data=df_calepa) + geom_histogram(aes(x=`Pollution Burden`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Pollution burden percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Pollution Burden Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Pollution burden score   
ggplot(data=df_calepa) + geom_histogram(aes(x=`Pollution Burden Score`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Asthma
ggplot(data=df_calepa) + geom_histogram(aes(x=Asthma), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Asthma percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Asthma Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Low birth weight
ggplot(data=df_calepa) + geom_histogram(aes(x=`Low Birth Weight`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Low birth weight percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Low Birth Weight Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")



# Cardiovascular disease
ggplot(data=df_calepa) + geom_histogram(aes(x=`Cardiovascular Disease`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Cardiovascular disease percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Cardiovascular Disease Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Education
ggplot(data=df_calepa) + geom_histogram(aes(x=`Education`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Education percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Education Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Linguistic isolation
ggplot(data=df_calepa) + geom_histogram(aes(x=`Linguistic Isolation`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Linguistic isolation percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Linguistic Isolation Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Poverty
ggplot(data=df_calepa) + geom_histogram(aes(x=Poverty), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Poverty percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Poverty Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Unemployment
ggplot(data=df_calepa) + geom_histogram(aes(x=Unemployment), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Unemployment percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Unemployment Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")


# Housing burden
ggplot(data=df_calepa) + geom_histogram(aes(x=`Housing Burden`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")

# Housing burden percentile
ggplot(data=df_calepa) + geom_histogram(aes(x=`Housing Burden Pctl`), bins=20, fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")




# maps 

library(sf)
library(rgdal)

# read in tract shape files from Cenus TIGER/Line Shapefile for 2016
ca_tract <- st_read('/Users/danagoin/Documents/CalEPA/tl_2016_06_tract/tl_2016_06_tract.shp')
names(ca_tract)[names(ca_tract)=="GEOID"] <- "tract"
ca_tract$tract <- as.character(ca_tract$tract)

# merge together CalEPA data with geographic data
# can't have missing location info 
df_c <- df_calepa %>% filter(!is.na(lon),)
dim(df_c)
df_c$tract <- paste0("0",df_c$tract)

# join Census tracts with CalEPA
df_c <- left_join(df_c, ca_tract, by="tract")
dim(df_c)

# convert to sf object
dat_sf <- st_as_sf(df_c) 


# plot Census tract boundaries and levels of traffic for the women in our study 

ggplot(data=ca_tract) + geom_sf() + theme_bw() + 
  geom_sf(data=dat_sf, aes(fill=`Asthma Pctl`)) + coord_sf(xlim=c(-120.75,-118.25), ylim=c(36, 37.5), expand=T) 





