##' ---
##' title: "Combine detailed mortality, population, and previous covariate data together"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
cleanupunderway  <- TRUE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' We need to combine the detailed mortality and population data with the data used in 
##' matching to do our covariate adjustment models.
##' 
##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")
load(file.path(path, "pop.Rdata"))
load(file.path(path, "all_det_mort.Rdata"))
  
##' Clean up the population data in order to merge with mortality data.
pop_wide <- pop_clean %>%
  pivot_wider(names_from = year, values_from = pop,
              names_glue = "{.value}_{year}",
              values_fill = 0) %>%
  mutate(FIPS = paste0(stateFIPS, cntyFIPS)) %>%
  filter(!(stateFIPS %in% c("02", "15")))%>%
  ungroup() %>%
  dplyr::select(-stateFIPS, -cntyFIPS)

##' To combine with the population data in an accurate way due to differences in reporting racial categories,
##' we want to just keep the "race_bridged" variable.

table(mort_det$race_bridged)
table(mort_det$race, mort_det$race_bridged)
table(pop_wide$race)

mort <- mort_det %>%
  dplyr::select(-race) %>%
  rename(race = race_bridged) %>%
  mutate(race = case_when(race == "Non-Hispanic Asian" ~ "Non-Hispanic Asian or Pacific Islander",
                          TRUE~ race)) %>%
  group_by(FIPS, stateFIPS, cntyFIPS, race, age) %>%
  summarize_all(sum)


##' Combine the population and mortality data. We do a full join to fill in the combinations of race
##' and age that are not present in the mortality data.

pop_mort <- mort %>%
  full_join(pop_wide, by = c("FIPS","race","age")) %>%
  mutate(across(contains("mort"), ~replace(., is.na(.), 0)))

save(pop_mort, file = file.path(path, "temp", "pop_mort_comb.Rdata"))

##' There are 9 instances where we do not have the population for a specific group
table(pop_mort$race[is.na(pop_mort$pop_2013)])

##' ## Data for 2014 model
##' We also read in the data that we use in matching, which will be used in the models.
load("../data/mod.dat.Rdata")

mod_2014 <- pop_mort %>%
  left_join(mod.dat, by = "FIPS") %>%
  mutate(trimmed = as.numeric(is.na(matches.final))) %>%
  dplyr::select(FIPS, stateFIPS, cntyFIPS,  stateName, cntyName, everything(),-matches.nostab)

##' The observations that are missing population data are all in Bedford City, Virginia, which are
##' already removed from our analyses

mod_2014 %>%
  filter(is.na(pop_2013)) %>%
  dplyr::select(FIPS, stateName, cntyName, age, race, trimmed)

keep_cols <- names(mod_2014)

##' ## Data for 2013 model
##' For this model we want to replace all of the variables that were 5 year aggregates to the 
##' lagged aggregate that BH calculated in _base_cnty.csv.

base.county <- read.csv("../data/_base_cnty.csv",  colClasses = c("FIPS"="character")) %>%
  dplyr::select(-stateFIPS, -cntyFIPS)

##' We remove the columns that we want to replace with the lagged versions, and then finally
##' keep the same variables of interest.

mod_2013 <- mod_2014 %>%
  dplyr::select(FIPS:pop_2014, pctUrban_2010:trimmed) %>%
  left_join(base.county, by = "FIPS") %>%
  dplyr::select(all_of(keep_cols))


##' ## 2020 data combined

load("../data/temp/mort.2020.lag.adj.Rdata")
load("../data/temp/pop.2018.19.Rdata")

mort.2012.2013 <- mod_2014 %>%
  dplyr::select(FIPS,mort_all_cause_2012, mort_all_cause_2013,
         mort_HC_amenable_not_flu_2012, mort_HC_amenable_not_flu_2013) %>%
  group_by(FIPS) %>%
  summarize(across(starts_with("mort"), sum))

pop.2019 <- pop.2018.19 %>%
  filter(year == 2019, race == "Total") %>%
  group_by(FIPS) %>%
  summarize(pop = sum(pop))

##' Ogala Lakota County SD has a different FIPS in the Census population data than the mortality data
pop.2019$FIPS[pop.2019$FIPS == "46102"] <- "46113"

##' Checkout other FIPS codes that don't align between population / 2020 mortality data and 2014 mortality data

check <- lag.adjusted.mort.2020 %>%
  left_join(mod.dat) %>%
  filter(is.na(stateName))
##FIPS 12125 doesn't match between mod.dat and 2020 mortality, which is Union County Florida in the 2020 data,
## because union county florida was excluded

mod_2020 <- mod.dat %>%
  left_join(pop.2019, by = "FIPS") %>%
  left_join(lag.adjusted.mort.2020, by = "FIPS") %>%
  left_join(mort.2012.2013, by = "FIPS") %>%
  dplyr::select(-matches.nostab, -state, -lag)

##' Finally save the data for use in modeling 2013 and 2014.

save(mod_2013, file = file.path(path,"out_mod_dat_2013.Rdata"))
save(mod_2014, file = file.path(path,"out_mod_dat_2014.Rdata"))
save(mod_2020, file = "../data/out_mod_dat_2020.Rdata")
