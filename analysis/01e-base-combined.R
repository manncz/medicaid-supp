###################################################################################################
#Script: 02-base-combined.R
#Inputs: data/base_cnty.csv
#        data/base_ahrf_cnty.csv
#        data/base_all_ihme.csv
#        data/base_partisan.csv
#        data/base_household.csv
#        data/medicaid_exp.csv
#Outputs: data/base_cnty_all.csv
#Author: CM
#Date: 6/21/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)

###################################################################################################

#use state and county names from AHRF data
#so remove any variables that would duplicate the merge

base_cnty <- read.csv("../data/base_cnty.csv", colClasses = c("FIPS"="character"))
ahrf <- read.csv("../data/base_ahrf_cnty.csv", colClasses = c("FIPS"="character")) %>%
  dplyr::select(-stateFIPS, -cntyFIPS)
ihme <- read.csv("../data/base_all_ihme_cnty.csv", colClasses = c("FIPS"="character")) %>%
  dplyr::select(-stateName, -cntyName)
partisan <- read.csv("../data/base_partisan_cnty.csv", colClasses = c("FIPS"="character"))%>%
  dplyr::select(-stateName, -cntyName)
acs <- read.csv("../data/base_household.csv", colClasses = c("FIPS"="character"))
medicaid <- read.csv("../data/medicaid_exp.csv") %>%
  dplyr::select(-stateName)
#flu_report <- read.csv("../data/base_data_reporting.csv")

base_all <- base_cnty %>%
  full_join(ahrf, by= c("FIPS")) %>%         #full join adds in the 5 counties in alaska
  left_join(medicaid, by= "stateFIPS") %>%
  left_join(ihme, by=c("FIPS")) %>%
  left_join(partisan, by=c("FIPS") ) %>%
  left_join(acs, by=c("FIPS")) 
# check_cnty_merge <- base_all %>%
#   dplyr::select(FIPS, stateFIPS, cntyFIPS, stateName, contains("cntyName"), Location) %>%
#   write_csv("../data/temp/check_cnty_merge_all.csv")

##interpolate issue in PM25 variable
cnty_adj <- read.csv("../data-raw/county_adjacency2010.csv", colClasses=c(fipscounty="character", fipsneighbor="character"))

fips.int.pm25 <- base_all %>%
  filter(avgPM25_2011 == 0)
broom.field.adj <- cnty_adj %>%
  filter(fipscounty == fips.int.pm25$FIPS) %>%
  filter(fipsneighbor != fipscounty)
interp.value <- mean(base_all$avgPM25_2011[base_all$FIPS %in% broom.field.adj$fipsneighbor])

base_all$avgPM25_2011[base_all$FIPS == fips.int.pm25$FIPS] <- interp.value

base_exp <- base_all %>%
  dplyr::select(FIPS, stateFIPS, cntyFIPS, stateName, cntyName, medcdExp:yearExp, everything(),
         -c(Location:cntyName.ihme2), -cntyName.acs)  %>%
  rename(avgPM25chk_2011 = avgPM25check_2011) %>%
  left_join(flu_report, by =c("stateName" = "state")) %>% #add flu reporting data
  write_csv("../data/base_cnty_all.csv")

