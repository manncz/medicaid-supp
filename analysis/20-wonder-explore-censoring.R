##' ---
##' title: "Explore Censoring Levels"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(readr)
library(stringr)
library(glmnet)
library(MASS)
library(dplyr)
library(tidyr)

knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")
#path <- "../data"
set.seed(2795)

##' ## Explore censoring levels
load(file.path(path,"out_mod_dat_2014.Rdata"))

##' We are going to do some checks to see at what level of the data we could reasonably model
##' 

mod_2014 %>%
  ungroup() %>%
  select(FIPS, race, age, mort_all_cause_2013, mort_all_cause_2014) %>%
  mutate(mort_2013 = case_when(mort_all_cause_2013 < 10 ~ 0,
                               TRUE~ 1),
         mort_2014 = case_when(mort_all_cause_2014 < 10 ~ 0,
                               TRUE~ 1)) %>%
  summarize(mean(mort_2013), mean(mort_2014))

##' If we are considering data at the race, age, county level, only 15% of the data remains after censoring.

mod_2014 %>%
  group_by(FIPS, age) %>%
  summarize(mort_all_cause_2013 = sum(mort_all_cause_2013),
            mort_all_cause_2014 = sum(mort_all_cause_2014)) %>%
  ungroup() %>%
  mutate(mort_2013 = case_when(mort_all_cause_2013 < 10 ~ 0,
                               TRUE~ 1),
         mort_2014 = case_when(mort_all_cause_2014 < 10 ~ 0,
                               TRUE~ 1)) %>%
  summarize(mean(mort_2013), mean(mort_2014))

##' If we are considering data at the race, county level, only 29.7 of the data remains after censoring.

mod_2014 %>%
  group_by(FIPS, race) %>%
  summarize(mort_all_cause_2013 = sum(mort_all_cause_2013),
            mort_all_cause_2014 = sum(mort_all_cause_2014)) %>%
  ungroup() %>%
  mutate(mort_2013 = case_when(mort_all_cause_2013 < 10 ~ 0,
                               TRUE~ 1),
         mort_2014 = case_when(mort_all_cause_2014 < 10 ~ 0,
                               TRUE~ 1)) %>%
  summarize(mean(mort_2013), mean(mort_2014))

##' If we consider data only at the age level, then 57% of the data remains after censoring.

mod_2014 %>%
  group_by(FIPS) %>%
  summarize(mort_all_cause_2013 = sum(mort_all_cause_2013),
            mort_all_cause_2014 = sum(mort_all_cause_2014)) %>%
  ungroup() %>%
  mutate(mort_2013 = case_when(mort_all_cause_2013 < 10 ~ 0,
                               TRUE~ 1),
         mort_2014 = case_when(mort_all_cause_2014 < 10 ~ 0,
                               TRUE~ 1)) %>%
  summarize(mean(mort_2013), mean(mort_2014))

##' If we consider data at the county level, then 91% of the data remains after censoring.


##' Let's model at the county/age level

# aggregate to the county/age level and censor counts less than 10
mod <- mod_2014 %>%
  group_by(FIPS, age) %>%
  mutate(across(contains("all_cause"), ~sum(.x, na.rm=T)),
         pop_2014 = sum(pop_2014, na.rm = T)) %>%
  select(FIPS,age, contains("all_cause"), pop_2014, adult_w_a_cnt:trimmed) %>%
  distinct() %>%
  mutate(across(contains("all_cause"), ~ case_when(.x < 10 ~ NA_real_,
                                                   TRUE ~ .x)))

# subset to untrimmed counties with non-zero population
reg_dat_2014 <- mod %>%
  filter(trimmed == 0 & pop_2014 > 0 & mort_all_cause_2014 > 0) %>%
  rename(mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012)

# subset to complete cases
cc <- complete.cases(reg_dat_2014)
reg_dat = reg_dat_2014[cc,]

# get a list of variable names
vars <- reg_dat %>%
  ungroup() %>% 
  select(age,mort_all_cause_l1, mort_all_cause_l2, white_race:calc_multi_house, log_adult_w_a_cnt) %>%
  colnames()

interactions <- paste(vars[-(1:2)], "age", sep = ":")

# formula with age and interactions
form1 <- formula(paste0(" ~ ", paste(c(vars, interactions), collapse="+")))

# formula without interactions
form2 <- formula(paste0(" ~ ", paste(vars[-1], collapse="+")))

# 
X <- model.matrix(form1, data = reg_dat)
# y <- reg_dat$mort_all_cause_2014
# off <-  reg_dat$pop_2014
# # set.seed(2795)
# 
# ##' Running the lasso model still isn't working
# cv.out.all <- cv.glmnet(x=X, y=y, family = quasipoisson(),  offset = log(off),
#                         alpha = 1)




##' First, by age group and supermajority white - there is much more censoring in supermajority white counties
dat.2014 %>%
  mutate(cens = case_when(is.na(mort_all_cause_2014) ~ 1,
                          TRUE ~ 0)) %>%
  group_by(s, age) %>%
  summarize(cens = sum(cens), n = n()) %>%
  group_by(s) %>%
  mutate(perc = cens/n)

##' Second, by treatment status and supermajory white
dat.2014 %>%
  ungroup() %>%
  group_by(FIPS, s, treat) %>%
  summarize(mort = sum(mort_all_cause_2014, na.rm = T)) %>%
  mutate(cens = case_when(mort < 10 ~ 1,
                          TRUE ~ 0)) %>%
  group_by(s, treat) %>%
  summarize(cens = sum(cens), n = n()) %>%
  mutate(perc = cens/n)

