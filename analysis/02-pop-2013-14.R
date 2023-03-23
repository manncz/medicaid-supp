##' ---
##' title: "Assemble/extract mortality variables for outcome analysis"
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
##' - Other (Native Hawaiian and Other Pacific Islander, more than one race, and unknown)
##' - Hispanic
##' 
##' We need to create these categorizations based on the available race and Hispanic codes.
##' Since the CMF only includes White, Black, American Indian or Alaska Native, and Asian
##' or Pacific Islander, we won't be able to create exactly the groupings as above.
##'
##' 
##'You will note that there are two possible file paths, one for running on compute 1 and one for running 
##'on CM's local computer setup for testing.

##' Using code from 01-base-mortality.R, we read in the population data.
#f.path <- file.path("..","..","..", "compute1","data-raw","pop9914.txt")
f.path <- file.path("..","data-raw","pop9914.txt")

pop_tall  <- f.path %>%
  read_fwf(fwf_cols(stateFIPS=c(1,2), # 'state'
                    cntyFIPS=c(3,5),  # 'county'
                    year=c(6,9),
                    race_sex=c(10,10), # 'racesex'
                    hisp_code=c(11,11),# 'hisp'
                    births=c(12,19),   # 'birth'
                    #a0_1=c(20,27),     # 'l1'
                    #a1_4=c(28,35),     # 'a14'
                    #a5_9=c(36,43),
                    #a10_14=c(44, 51),
                    #a15_19=c(52,59),
                    a20_24=c(60,67),
                    a25_34=c(68,75),
                    a35_44=c(76,83),
                    a45_54=c(84,91),
                    a55_64=c(92,99),
                    #a65_74=c(100,107),
                    #a75_84=c(108,115),
                    #a85plus=c(116,123),    # 'a85'
                    #cnty_name=c(124,148),  # 'name'
                    record_type=c(149,149) # 'type'
  ),
  progress=FALSE#, n_max = 5000 #set n_max for working on local computer
  )

##' Clean the population data:
##' 
##' - Create race categories as desired.
##' - Reshape long for age group
##' - Group 20-24 and 25-34 into one group
##' - Sum population over these categories
##' - "Other" is only capturing cases for which Hispanic is unkown, so drop if the population count
##' is 0 anyways.

pop_clean <- pop_tall %>% 
  mutate(race = case_when(race_sex %in% 1:2 & hisp_code == 1 ~ "Non-Hispanic White",
                          race_sex %in% 3:4 & hisp_code == 1~ "Non-Hispanic Black",
                          race_sex %in% 5:6 & hisp_code == 1 ~ "Non-Hispanic AI or AN",
                          race_sex %in% 7:8 & hisp_code == 1  ~ "Non-Hispanic Asian or Pacific Islander",
                          hisp_code == 2 ~ "Hispanic",
                          TRUE ~ "Other")) %>% #this only includes hispanic unknown for this data
  filter(record_type==3, year %in% 2013:2014) %>%
  select(-births,-record_type, -race_sex, -hisp_code) %>%
  pivot_longer(a20_24:a55_64, names_to = "age", values_to = "pop", names_prefix = "a") %>%
  mutate(age = case_when(age %in% c("20_24", "25_34")~"20_34",
                         TRUE ~ age)) %>%
  group_by(stateFIPS, cntyFIPS, year, race, age) %>%
  summarize(pop = sum(pop)) %>%
  filter(!(race == "Other" & pop == 0))

#write.csv(pop_clean, file.path("..", "data","pop.csv"))
save(pop_clean, file = file.path("..", "data","pop.Rdata"))

pop_clean_2011_2012 <- pop_tall %>% 
  mutate(race = case_when(race_sex %in% 1:2 & hisp_code == 1 ~ "Non-Hispanic White",
                          race_sex %in% 3:4 & hisp_code == 1~ "Non-Hispanic Black",
                          race_sex %in% 5:6 & hisp_code == 1 ~ "Non-Hispanic AI or AN",
                          race_sex %in% 7:8 & hisp_code == 1  ~ "Non-Hispanic Asian or Pacific Islander",
                          hisp_code == 2 ~ "Hispanic",
                          TRUE ~ "Other")) %>% #this only includes hispanic unknown for this data
  filter(record_type==3, year %in% 2011:2012) %>%
  select(-births,-record_type, -race_sex, -hisp_code) %>%
  pivot_longer(a20_24:a55_64, names_to = "age", values_to = "pop", names_prefix = "a") %>%
  mutate(age = case_when(age %in% c("20_24", "25_34")~"20_34",
                         TRUE ~ age)) %>%
  group_by(stateFIPS, cntyFIPS, year, race, age) %>%
  summarize(pop = sum(pop)) %>%
  filter(!(race == "Other" & pop == 0))

save(pop_clean_2011_2012, file = file.path("..", "data","pop.Rdata"))
