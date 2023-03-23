##' ---
##' title: "Calculate m - weights for counties"
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

##' ## 2014 M data
##' First calculate the m's for the counties overall
##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")
load(file.path(path,"temp", "pop_mort_comb.Rdata"))

##' We will need (1) the mortality counts for each age group across all mainland US counties, and (2),
##' the population counts for each age group both overall and for each county, both for 2014.


##' 1) calculate the overall mortality rate by age across the entire continental US
##' 2) multiply the population by the overall us mortality rate for that age group
##' 3) sum all of these adjusted populations (i.e. an expected mortality rate) across age groups for a county
##' 4) divide by the overall mortality rate to get an age adjusted population value

overall_m <- pop_mort %>%
  ungroup() %>%
  dplyr::select(FIPS, age, mort_all_cause_2014, pop_2014) %>%
  group_by(FIPS, age) %>%
  summarize_all(sum) %>%
  ungroup()%>%
  group_by(age) %>%
  mutate(mort_all_us = sum(mort_all_cause_2014),
         pop_all_us = sum(pop_2014, na.rm = T)) %>%
  ungroup()%>%
  mutate(all_us_age_rate = mort_all_us / pop_all_us,
         adj_pop = pop_2014*all_us_age_rate) %>%
  mutate(all_us_rate = sum(mort_all_us) / sum(pop_all_us),
         adj_pop = adj_pop / all_us_rate) %>%
  group_by(FIPS)%>%
  summarize(m = sum(adj_pop))


save(overall_m, file = "../data/temp/overall.m.2014.Rdata")

  
m_by_race <- pop_mort %>%
  ungroup() %>%
  dplyr::select(FIPS, race, age, mort_all_cause_2014, pop_2014) %>%
  group_by(age, race) %>%
  mutate(mort_all_us = sum(mort_all_cause_2014),
         pop_all_us = sum(pop_2014, na.rm = T)) %>%
  ungroup()%>%
  mutate(all_us_age_rate = mort_all_us / pop_all_us,
         adj_pop = pop_2014*all_us_age_rate) %>%
  group_by(race) %>%
  mutate(all_us_rate = sum(mort_all_us) / sum(pop_all_us),
         adj_pop = adj_pop / all_us_rate) %>%
  group_by(FIPS, race) %>%
  summarize(m = sum(adj_pop))

save(m_by_race, file = "../data/temp/m.by.race.2014.Rdata")

##' ## 2020 M data
##' I am using the 2018 population data since that is what we specified in the protocol.

load("../data/temp/pop_mort_overall_2018.Rdata")

overall_m_2018 <- pop.mort.overall.2018 %>%
  ungroup() %>%
  dplyr::select(FIPS, age, mort, pop) %>%
  group_by(FIPS, age) %>%
  summarize_all(sum) %>%
  ungroup()%>%
  group_by(age) %>%
  mutate(mort_all_us = sum(mort),
         pop_all_us = sum(pop)) %>%
  ungroup()%>%
  mutate(all_us_rate = mort_all_us / pop_all_us,
         adj_pop = pop*all_us_rate) %>%
  mutate(all_us_rate = sum(mort_all_us) / sum(pop_all_us),
         adj_pop = adj_pop / all_us_rate) %>%
  group_by(FIPS)%>%
  summarize(m = sum(adj_pop))

save(overall_m_2018, file = "../data/temp/overall.m.2018.Rdata")
