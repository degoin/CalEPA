
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
                                  ethnicity_m_mr, race_m_mr, race_m_sp_mr, insurance_m_mr, language_m_mr, language_m_sp_mr, 
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

hist(df_calepa$`CES 3.0 Score`)


table(df_calepa$maternal_age, exclude=NULL)


df_calepa$agecat <- ifelse(df_calepa$maternal_age<20,1,
                           ifelse(df_calepa$maternal_age>=20 & df_calepa$maternal_age<25,2,
                                  ifelse(df_calepa$maternal_age>=25 & df_calepa$maternal_age<30,3,
                                         ifelse(df_calepa$maternal_age>=30,4,NA))))
table(df_calepa$agecat, exclude=NULL)       
df_calepa$sb535_disadvanted <- ifelse(df_calepa$`SB 535 Disadvantaged Community`=="Yes", 1, 
                                      ifelse(df_calepa$`SB 535 Disadvantaged Community`=="No",0, NA))



dem_vars <- c("agecat","maternal_edu", "emp_status_m", "marital", "hh_inc", "parity_mr", "wic_mr",
              "drink_tap", "filter_water")

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

# CES percentile 
ggplot(data=df_calepa) + geom_histogram(aes(x=`Ozone Pctl`), fill="#045a8d", color="white") + theme_bw() + labs(y="Frequency")
