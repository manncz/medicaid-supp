###################################################################################################
#Script: 01-base-household.R
#Inputs: data-raw/ACSST5Y2013.S1002_data_with_overlays_2020-06-21T223140.csv
#Outputs: data/base_household.csv
#Author: CM
#Date: 6/21/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)

###################################################################################################
# load Rdata version of xwalk saved in 01-base-partisan, just to check
load("../data/temp/cnty_fip_xwlk.Rdata")
xwlk <- cnty_xwlk %>% group_by(FIPS, stateName, cntyName) %>% summarize()

# Variables of interest:
# S1002_C01_034E	Total!!Estimate!!HOUSEHOLDS!!Households
# S1002_C01_034M	Total!!Margin of Error!!HOUSEHOLDS!!Households
# S1002_C01_035E	Total!!Estimate!!HOUSEHOLDS!!Households!!With grandparents living with grandchildren
# S1002_C01_035M	Total!!Margin of Error!!HOUSEHOLDS!!Households!!With grandparents living with grandchildren
# S1002_C01_036E	Total!!Estimate!!PERCENT IMPUTED!!Grandparents living with grandchildren
# S1002_C01_036M	Total!!Margin of Error!!PERCENT IMPUTED!!Grandparents living with grandchildren

house_raw <- read.csv("../data-raw/ACSST5Y2013.S1002_data_with_overlays_2020-06-21T223140.csv")

house <- house_raw %>%
  filter(GEO_ID != "id") %>%
  select(GEO_ID, cntyName.acs = NAME, 
         total_house = S1002_C01_034E, multi_house = S1002_C01_035E,
         perc_grpr = S1002_C01_036E) %>%
  mutate(FIPS = str_replace(str_extract(GEO_ID, "US\\d{5}"), "US", ""),
         calc_multi_house = as.numeric(multi_house)/as.numeric(total_house)*100,
         total_house = as.numeric(total_house), multi_house = as.numeric(multi_house),
         perc_grpr = as.numeric(perc_grpr)) %>%
  select(FIPS, everything(), -GEO_ID)

# the ACS data includes puerto rico and doesn't have data for 3 counties in Alaska
check.merge <- house %>%
  full_join(xwlk, by="FIPS") %>%
  filter(is.na(stateName) | is.na(cntyName.acs))

house <- house %>%
  filter(FIPS %in% xwlk$FIPS) %>%
  write_csv("../data/base_household.csv")
