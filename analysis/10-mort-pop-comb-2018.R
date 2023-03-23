##' ---
##' title: "Combine 2018 mortality and population data"
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

##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")
load(file.path(path, "mort_2018.Rdata"))

##' Population data is in github
load("../data/temp/pop.2018.19.Rdata")

##' Check out race variable

table(mort_2018$race, mort_2018$race_bridged)
table(mort_2018$race, mort_2018$race_bridged_uk)
table(mort_2018$race_bridged_uk, mort_2018$race_bridged)
table(pop.2018.19$race)

##' I am also saving an overall dataset, by age since this is actually what we need.
pop.all.2018  <- pop.2018.19 %>%
  filter(race == "Total" & year == 2018) %>%
  group_by(FIPS, stateName, cntyName) %>%
  filter(!(stateName %in% c("Hawaii", "Alaska"))) %>%
  summarize(pop = sum(pop))

mort.2018.all <- mort_2018 %>%
  group_by(FIPS, year) %>%
  summarize(mort = sum(mort, na.rm = T))

check <- pop.all.2018 %>%
  left_join(mort.2018.all, by = c("FIPS")) %>%
  filter(is.na(mort)) %>%
  dplyr::select(FIPS, cntyName, stateName) %>%
  distinct()

check2 <-  mort.2018.all %>%
  left_join(pop.all.2018, by = c("FIPS")) %>%
  filter(is.na(pop)) %>%
  dplyr::select(FIPS, cntyName, stateName) %>%
  distinct()


#Ogala Lakota County SD has a different FIPS in the Census population data than the mortality data
pop.2018.19$FIPS[pop.2018.19$FIPS == "46102"] <- "46113"

mort.2018.age <- mort_2018 %>%
  group_by(FIPS, year, age) %>%
  summarize(mort = sum(mort, na.rm = T))

##' The counties for which the mortality is missing should be 0 mortality
pop.mort.overall.2018 <- pop.2018.19 %>%
  filter(year == 2018 & !(stateName %in% c("Hawaii", "Alaska"))) %>%
  group_by(FIPS, stateName, cntyName, year, age) %>%
  summarize(pop = sum(pop)) %>%
  left_join(mort.2018.age, by = c("FIPS", "age", "year"))%>%
  dplyr::select(FIPS, stateName, cntyName, everything()) %>%
  mutate(mort = case_when(is.na(mort) ~ 0L,
                          TRUE ~ mort))

save(pop.mort.overall.2018, file = "../data/temp/pop_mort_overall_2018.Rdata")
