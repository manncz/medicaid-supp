###################################################################################################
#Script: 01-medicaid-exp.R
#Inputs: data-raw/expansion-status-interactive-map_1.2.19.csv
#Outputs: data/medicaid_exp.csv
#Author: CM
#Date: 6/21/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

###################################################################################################

medicaid_raw <- read.csv("../data-raw/expansion-status-interactive-map_1.2.19.csv")
# load Rdata version of xwalk saved in 01-base-partisan
load("../data/temp/cnty_fip_xwlk.Rdata")

#create a crosswalk for state names and fips to merge easily with baseline data
st_xwlk <- cnty_xwlk %>%
  group_by(stateFIPS, stateName) %>%
  summarize()

medicaid <- medicaid_raw %>%
  mutate(stateName = str_to_lower(State),
         medcdExp = ifelse(str_detect(Expansion.Status, "Not"), 0,1),
         dateExp = mdy(str_extract(Description, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}")),
         yearExp = year(dateExp)) %>%
  left_join(st_xwlk, by = "stateName") %>%
  select(stateFIPS, stateName:yearExp) %>%
  write_csv("../data/medicaid_exp.csv")
