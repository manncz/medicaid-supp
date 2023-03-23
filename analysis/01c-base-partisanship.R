###################################################################################################
#Script: 01-base-partisanship.R
#Inputs: data-raw/pres_2008_2016.csv
#Outputs: data/base_partisan _cnty.csv
#Author: CM
#Date: 6/17/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(xlsx)

###################################################################################################
fip_xwlk <- read.csv("../data/temp/cnty_fips.csv")
elec_raw <- read.csv("../data-raw/pres_2008_2016.csv", skip = 2) 

elec <- elec_raw %>%
  filter(RepVotesMajorPercent != "" & State != "State") %>%
  mutate(year = as.numeric(str_sub(RaceDate, 1,4)),
         pctRep = as.numeric(RepVotesMajorPercent),
         pop = as.numeric(str_replace_all(CensusPop, ",", ""))) %>%
  select(stateName = State,
         cntyName = Area,
         year, pop, pctRep) %>%
  mutate(cntyName = str_to_lower(cntyName),
         stateName = str_to_lower(stateName))

################################  Create County Crosswalk #########################
check.merge <- elec %>%
  group_by(stateName, cntyName) %>%
  summarize(n=n()) %>%
  full_join(fip_xwlk, by=c("stateName", "cntyName"))

check.merge %>% filter(!is.na(FIPS)) %>% 
  mutate(cntyName.elect = ifelse(is.na(n), "", cntyName)) %>%
  select(-n) %>%
  write_csv("../data/temp/cnty_fips_xwalk.csv")

check.merge %>% filter(is.na(FIPS)) %>% 
  write_csv("../data/temp/elect_cntys.csv")
###########################################################################################

cnty_fip_xwlk <- xlsx::read.xlsx("../data/temp/cnty_fips_xwalk.xlsx", sheetName="xwalk")
cnty_xwlk <- cnty_fip_xwlk %>% 
  select(-c("index")) %>% 
  mutate(FIPS = sprintf('%05d', FIPS))
save(cnty_xwlk, file = "../data/temp/cnty_fip_xwlk.Rdata")

# merge the county/fips xwalk
# take a weighted average of % for Kansas city Missouri and DC districts to match other data sources.
# manually enter population data for kansas city for these years (from google) to take weighted average
# reshape long by year
partisan <- elec %>%
  select(cntyName.elect = cntyName, everything()) %>%
  left_join(cnty_xwlk, by=c("stateName", "cntyName.elect")) %>%
  filter(!is.na(FIPS)) %>%
  mutate(pop = ifelse(cntyName.elect == "kansas city" & year == 2016, 481626,        
         ifelse(cntyName.elect == "kansas city" & year == 2012, 464607,
         ifelse(cntyName.elect == "kansas city" & year == 2008, 454811, pop)))) %>%
  mutate(wgtd.perc = ifelse(!is.na(pop), pop*pctRep, pctRep)) %>%
  group_by(stateName, cntyName, FIPS, year) %>%
  summarize(pop = sum(pop), pctRep = sum(wgtd.perc)) %>%
  mutate(pctRep = ifelse(!is.na(pop), pctRep/pop, pctRep)) %>%
  select(-pop) %>%
  pivot_wider(names_from = year, values_from = pctRep, names_prefix = "pctRep_") %>%
  write_csv("../data/base_partisan_cnty.csv")

# checkmerge2 <- partisan %>%
#   group_by(cntyName.elect, stateName, FIPS) %>%
#   summarize() %>%
#   filter(is.na(FIPS))
  
  