###################################################################################################
#Script: 01-base-ihme.R
#Inputs: IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.csv
#        IHME_USA_COUNTY_ALCOHOL_USE_PREVALENCE_2002_2012_NATIONAL/IHME_USA_COUNTY_ALCOHOL_USE_PREVALENCE_2002_2012_NATIONAL_Y2015M04D23.xlsx
#        IHME_USA_COUNTY_DIABETES_PREVALENCE_1999_2012/IHME_USA_COUNTY_DIABETES_PREVALENCE_1999_2012_NATIONAL_Y2016M08D23.XLSX
#        IHME_USA_HYPERTENSION_BY_COUNTY_2001_2009/IHME_USA_HYPERTENSION_BY_COUNTY_2001_2009.CSV
#        IHME_USA_OBESITY_PHYSICAL_ACTIVITY_2001_2011/Data.csv
#Outputs: data/base_smoke_cnty.csv
#         data/base_alcohol_cnty.csv
#         data/base_diabetes_cnty.csv
#         data/base_hypertension_cnty.csv
#         data/base_obesity_cnty.csv
#Author: CM
#Date: 6/17/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(xlsx)
library(openxlsx)

###################################################################################################

fip_xwlk <- read.csv("../data/temp/cnty_fips.csv")

####################################      SMOKING      ############################################

smoke_raw <- read.csv("../data-raw/IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012/IHME_US_COUNTY_TOTAL_AND_DAILY_SMOKING_PREVALENCE_1996_2012.csv")

smoke <- smoke_raw %>%
  filter(county != "" & year >= 2010 & sex == "Both") %>%
  mutate(stateName = str_to_lower(state),
         cntyName = str_replace(str_to_lower(county), " county", "")) %>%
  mutate(cntyName = str_replace(cntyName, " parish", "")) %>%
  select(stateName, cntyName, year, total_mean, daily_mean)

#==========================  Create County Crosswalk ========================================#
check.merge <- smoke %>%
  group_by(stateName, cntyName) %>%
  summarize(n=n()) %>%
  full_join(fip_xwlk, by=c("stateName", "cntyName"))

check.merge %>% filter(!is.na(FIPS)) %>% 
  mutate(cntyName.smoke = ifelse(is.na(n), "", cntyName)) %>%
  select(-n) %>%
  write_csv("../data/temp/ahrf_smoke_cnty.csv")

check.merge %>% filter(is.na(FIPS)) %>% 
  write_csv("../data/temp/smoke_cntys.csv")

#==========================================================================================#
# load Rdata version of xwalk saved in 01-base-partisan
load("../data/temp/cnty_fip_xwlk.Rdata")

xwlk <- cnty_xwlk %>% 
  filter(!is.na(cntyName.smoke)) %>% 
  select( -cntyName.elect)

smoke_clean <- smoke %>%
  select(cntyName.smoke = cntyName, smk_dly = daily_mean, smk_tot = total_mean, everything()) %>%
  pivot_wider(names_from = year, values_from = c("smk_dly", "smk_tot")) %>%
  right_join(xwlk, by=c("stateName", "cntyName.smoke")) %>%
  select(FIPS, stateName, cntyName,  contains("smk")) %>%
  write_csv("../data/base_smoke_cnty.csv")

#check merge - before removing cntyName.smoke in line 59
# check <- smoke_clean %>%
#   group_by(stateName, cntyName.smoke) %>%
#   summarize(n=n()) %>%
#   filter(n>1)

# data is complete
for (x in names(smoke_clean)){
  print(paste0(x, " is missing ", sum(is.na(smoke_clean[,x])), " obs"))
}
####################################      ALCOHOL      ############################################
alc_hvy_raw <- openxlsx::read.xlsx("../data-raw/IHME_USA_COUNTY_ALCOHOL_USE_PREVALENCE_2002_2012_NATIONAL/IHME_USA_COUNTY_ALCOHOL_USE_PREVALENCE_2002_2012_NATIONAL_Y2015M04D23.xlsx", sheet = 3)

state.names <- alc_hvy_raw %>% group_by(State) %>% summarize()
state.names <- state.names$State

alc <- alc_hvy_raw %>%
  filter(!(Location %in% state.names) & Location != "United States") %>%
  select(stateName = State, cntyName = Location, 
        alc_2010 = `2010.Both.Sexes`, alc_2011 = `2011.Both.Sexes`, alc_2012 =`2012.Both.Sexes`) %>%
  mutate(stateName = str_to_lower(stateName),
         cntyName = str_replace_all(str_replace(str_to_lower(cntyName), " county", ""), ", ", "/")) %>%
  mutate(cntyName = str_replace(cntyName, " parish", ""))

#==========================================================================================#
#the county names are consistent other than some punctuation
ihme.cnys <- xwlk %>%
  group_by(stateName, cntyName.smoke) %>%
  summarize(n=n()) %>%
  select(stateName, cntyName = cntyName.smoke, n)

check.merge <- alc %>%
  full_join(ihme.cnys, by = c("stateName", "cntyName"))
check.merge %>% filter(is.na(n))
check.merge %>% filter(is.na(alc_2010))
#==========================================================================================#

alc_fips <- alc %>%
  select(cntyName.smoke = cntyName,  everything()) %>%
  right_join(xwlk, by=c("stateName", "cntyName.smoke")) %>%
  select(FIPS, stateName, cntyName,  contains("alc")) %>%
  write_csv("../data/base_alcohol_cnty.csv")

####################################     DIABETES      ############################################

diabetes_raw <- openxlsx::read.xlsx("../data-raw/IHME_USA_COUNTY_DIABETES_PREVALENCE_1999_2012/IHME_USA_COUNTY_DIABETES_PREVALENCE_1999_2012_NATIONAL_Y2016M08D23.XLSX", 
                                    sheet = 4, startRow = 2)
diabetes <- diabetes_raw %>%
  filter(!is.na(FIPS) & FIPS > 56) %>%
  mutate(FIPS = sprintf('%05d', FIPS)) %>%
  select(FIPS, Location, diabetes_2012 = `Prevalence,.2012,.Both.Sexes`,
         diabetes_2011 = `Prevalence,.2011,.Both.Sexes`,
         diabetes_2010 = `Prevalence,.2010,.Both.Sexes`)
  
chk.merge <- xwlk %>%
 full_join(diabetes, by="FIPS") %>%
  filter(is.na(stateName) | is.na(Location))

#this has the same issue of the updated counties
diabetes$FIPS[which(diabetes$FIPS == "02158")] <- "02270" # Wade Hampton AK
diabetes$FIPS[which(diabetes$FIPS == "46102")] <- "46113" # Shannon SD

#the diabetes data is just missing from 3 areas in Alaska and bedford city in Virginia
chk.merge <- xwlk %>%
  full_join(diabetes, by="FIPS") %>%
  filter(is.na(stateName) | is.na(Location))

write_csv(diabetes, "../data/base_diabetes_cnty.csv")

####################################    HYPERTENSION   ############################################

hyper_raw <- read.csv("../data-raw/IHME_USA_HYPERTENSION_BY_COUNTY_2001_2009/IHME_USA_HYPERTENSION_BY_COUNTY_2001_2009.CSV")

hyper <- hyper_raw %>%
  filter(County != "" & Race == "all") %>%
  mutate(FIPS = sprintf('%05d', FIPS)) %>%
  select(FIPS, cntyName.ihme = County,
         hyper_male_2009 = `Total..Male..2009`,
         hyper_female_2009 = `Total..Female..2009`) %>%
  mutate_at(vars(contains("2009")), function(x){as.numeric(str_replace_all(x, "%", ""))})

#the diabetes data is just missing from 5 areas in Alaska
chk.merge <- xwlk %>%
  full_join(hyper, by="FIPS") %>%
  filter(is.na(stateName) | is.na(cntyName.ihme))

write_csv(hyper, "../data/base_hypertension_cnty.csv")

####################################      OBESITY      ############################################

obesity_raw <- read.csv("../data-raw/IHME_USA_OBESITY_PHYSICAL_ACTIVITY_2001_2011/Data.csv")

obesity <- obesity_raw %>% 
  filter(Outcome %in% c("Obesity", "Sufficient PA")) %>%
  mutate(FIPS = sprintf('%05d', fips), 
         outcome = ifelse(Outcome == "Obesity", "obsty_", "phys_act_"),
         nms = paste0(outcome, str_to_lower(Sex), "_2011")) %>%
  select(FIPS, cntyName.ihme2 = County,
         nms, prev_2011 = `Prevalence.2011....`) %>%
  pivot_wider(names_from = nms, values_from = prev_2011)
  
# there is just not data for 3 counties in Alaska and Denali Alaska has a faulty FIPS
chk.merge <- xwlk %>%
  full_join(obesity, by="FIPS") %>%
  filter(is.na(stateName) | is.na(cntyName.ihme2))

obesity$FIPS[which(obesity$cntyName.ihme2 == "Denali, AK")] <- "02068" # Denali AK

write_csv(obesity, "../data/base_obesity_cnty.csv")

#this offers some insight into the ihme county names - the fips variable is correct to use
check <- obesity_raw %>%
  filter(fips != merged_fips)


########################################################################################

# CHECK FIPS/COUNTY SITUATION BETWEEN ALL IHME
all_ihme <- smoke_clean %>%
  full_join(alc_fips %>% select(-c("stateName", "cntyName")), by="FIPS") %>%
  full_join(diabetes, by="FIPS") %>%
  full_join(hyper, by = "FIPS") %>%
  full_join(obesity, by = "FIPS") %>%
  select(FIPS, stateName, cntyName, Location, cntyName.ihme, cntyName.ihme2, everything())

# and missing data 
for (x in names(all_ihme)){
  print(paste0(x, " is missing ", sum(is.na(all_ihme[,x])), " obs"))
}

#keep these for now for checking
all_ihme %>% #select(-c(Location:cntyName.ihme2)) %>%
  write_csv("../data/base_all_ihme_cnty.csv")

#EOF
