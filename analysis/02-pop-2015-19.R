##' ---
##' title: "Assemble/extract population variables for outcome analysis"
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
library(maps)
stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
##'


##' ### Race, sex & "hispanic" variables
##' 
##' We are interested in levels of race to match the CDC provisional COVID data by Race:
##' 
##' - Non-Hispanic White
##' - Non-Hispanic Black or African American
##' - Non-Hispanic American Indian or Alaska Native
##' - Non-Hispanic Asian
##' - Non-Hispanic Native Hawaiian and Other Pacific Islander
##' - Other (more than one race, and unknown)
##' - Hispanic
##' 
##' Age: 
##'5 = Age 20 to 24 years
##'6 = Age 25 to 29 years
##'7 = Age 30 to 34 years
##'8 = Age 35 to 39 years
##'9 = Age 40 to 44 years
##'10 = Age 45 to 49 years
##'11 = Age 50 to 54 years
##'12 = Age 55 to 59 years
##'13 = Age 60 to 64 years
##' 
##' Year:
##' 8 = 7/1/2015 population estimate 
##' 9 = 7/1/2016 population estimate
##' 10 = 7/1/2017 population estimate
##' 11 = 7/1/2018 population estimate
##' 12 = 7/1/2019 population estimate
##' 

pop.2019.raw <- read.csv(file = "../data-raw/cc-est2019-alldata.csv") %>%
  filter(YEAR %in% 8:12)

##' Save the raw data for 2019 for quicker read in.
save(pop.2019.raw, file = "../data/temp/pop19-raw.Rdata")


load( "../data/temp/pop.2015.19.Rdata")

pop.2019.clean <- pop.2019.raw %>%
  dplyr::select(STATE, COUNTY, stateName = STNAME, cntyName = CTYNAME,
                year = YEAR, AGEGRP, TOT_POP, TOM_MALE, TOM_FEMALE,
                NHWA_MALE:NHTOM_FEMALE, H_MALE, H_FEMALE, HTOM_MALE,
                HTOM_FEMALE) %>%
  mutate(NHAANH_MALE = NHAA_MALE + NHNA_MALE,
         NHAANH_FEMALE = NHAA_FEMALE + NHNA_FEMALE,
         age = case_when(AGEGRP %in% c(5,6,7) ~ "20_34",
                         AGEGRP %in% c(8,9) ~ "35_44",
                         AGEGRP %in% c(10,11) ~ "45_54",
                         AGEGRP %in% c(12,13) ~ "55_64",
                         TRUE ~ "")) %>%
  filter(age != "") %>%
  mutate(FIPS = paste0(sprintf('%02d', STATE), sprintf('%03d', COUNTY)),
         year = 2007+year) %>%
  dplyr::select(FIPS, stateName, cntyName, year, age, TOT_POP:NHAANH_FEMALE) %>%
  pivot_longer(TOT_POP:NHAANH_FEMALE, names_to = c("race_raw", "gender"),
               names_pattern = "(.*)_(.*)")

##' Saved this once, manually entered matching values and now can just read this back in
# race.xwalk <- names(table(pop.2019.clean$race_raw))
# write.csv(race.xwalk, file = "../data/temp/2019.pop.race.xwalk.csv")

race.xwalk <- read.csv("../data/temp/2019.pop.race.xwalk.csv")

pop.2015.19 <- pop.2019.clean %>%
  left_join(race.xwalk, by = "race_raw") %>%
  group_by(FIPS, stateName, cntyName, year, age, race) %>%
  summarize(pop = sum(value))

save(pop.2015.19, file = "../data/temp/pop.2015.19.Rdata")
