##' ---
##' title: "Covariance Adjustment Models - Censored Data"
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
library("methods")
library("SparseM")
library("PDQutils")
source("../R/StratifiedDesign.R")
source("../R/Zty_cumulants.R")
source("12-partial-order-helpers.R")
source("20-wonder-helpers.R")

##' Change to either run interval estimation or not
int.est = T
#int.est = F


##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")
#path <- "../data"

##' ## Explore censoring levels
load(file.path(path,"out_mod_dat_2014.Rdata"))

##' Load the necessary accompanying data
##' 
load("../data/temp/supermajority.white.xwalk.Rdata")
load("../data/temp/overall.m.2014.Rdata")
load("../data/temp/m.by.race.2014.Rdata")
load("../data/temp/mod.var.list.Rdata")

##' Set up m dat
m.dat.list <- list(all = overall_m)

m.dat.list.white <- list()
m.dat.list.white[["all"]] <- m_by_race %>%
  filter(race == "Non-Hispanic White") %>%
  ungroup() %>%
  dplyr::select(-race)


##' ## Create estimation data

##' ### County-level covariate data

dat.2014.county <- mod_2014 %>%
  ungroup() %>%
  group_by(FIPS) %>%
  mutate(across(contains("all_cause"), ~sum(.x, na.rm=T)),
         pop_2014 = sum(pop_2014, na.rm = T)) %>%
  select(FIPS, pop_2014, adult_w_a_cnt:trimmed,
         mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012,
         mort_dmf = mort_all_cause_2014) %>%
  distinct() %>%
  filter(trimmed == 0) %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  rename(treat = mdcdExp, pop = pop_2014, matches = matches.final, s = maj_white) %>%
  mutate(race = "", lag = 0, age = "25-64") ## need to create to work with the made function

#check
length(unique(dat.2014.county$FIPS)) == nrow(dat.2014.county)

##' ### Use county-level age adjusted mortality as outcome from WONDER

age_adj <- read.delim(file = "../data-raw/wonder/wonder_2014_ac_age_adj.txt")
age_adj <- age_adj %>%
  mutate(across(Deaths:Age.Adjusted.Rate, ~ as.numeric(case_when(.x %in% c("Suppressed", "Unreliable", "Missing") ~ "",
                                                     TRUE ~ .x)))) %>%
  mutate(FIPS = sprintf('%05d', County.Code),
         adj = Age.Adjusted.Rate/Crude.Rate,
         mort1 = Age.Adjusted.Rate/100000*Population,
         mort = Deaths*adj) %>%
  mutate(mort_wndr = case_when(is.na(Crude.Rate) & !is.na(Deaths) ~ Deaths,
                          TRUE ~ round(mort))) %>%
  select(FIPS, mort_wndr)

dat.2014.wonder <- dat.2014.county %>%
  left_join(age_adj, by = "FIPS")

##' ### Lag variables also from WONDER (2012 and 2013)

wonder_12 <- read.delim(file = "../data-raw/wonder/wonder_2012_age_adj.txt")
wonder_13 <- read.delim(file = "../data-raw/wonder/wonder_2013_age_adj.txt")
wonder_12$year = 2012
wonder_13$year = 2013

lag_vars <- rbind(wonder_12, wonder_13) %>%
  filter(!is.na(County.Code)) %>%
  mutate(across(Deaths:Age.Adjusted.Rate, ~ as.numeric(case_when(.x %in% c("Suppressed", "Unreliable", "Missing") ~ "",
                                                                 TRUE ~ .x)))) %>%
  mutate(FIPS = sprintf('%05d', County.Code),
         mort = round(Deaths*Age.Adjusted.Rate/Crude.Rate)) %>%
  select(FIPS, mort, year) %>%
  pivot_wider(names_from = "year", values_from = "mort", names_prefix = "mort_all_cause_")

dat.2014.wonder <- dat.2014.wonder %>%
  left_join(lag_vars, by = "FIPS") %>%
  rename(mort_all_cause_l1w = mort_all_cause_2013,
         mort_all_cause_l2w = mort_all_cause_2012)

##' ### White outcome from WONDER

age_adj_white <- read.delim(file = "../data-raw/wonder/wonder_2014_ac_white_age_adj.txt")
age_adj_white <- age_adj_white %>%
  #select(County, County.Code, Population, Deaths, Crude.Rate, Age.Adjusted.Rate) %>%
  mutate(across(Deaths:Age.Adjusted.Rate, ~ as.numeric(case_when(.x %in% c("Suppressed", "Unreliable", "Missing") ~ "",
                                                                 TRUE ~ .x)))) %>%
  mutate(FIPS = sprintf('%05d', County.Code),
         adj = Age.Adjusted.Rate/Crude.Rate,
         mort1 = Age.Adjusted.Rate/100000*Population,
         mort2 = round(Crude.Rate/100000*Population),
         mort = Deaths*adj) %>%
  mutate(mort_wht_wndr  = case_when(is.na(Crude.Rate) & !is.na(Deaths) ~ Deaths,
                          TRUE ~ round(mort))) %>%
  select(FIPS, mort_wht_wndr, pop.white = Population)

dat.2014.wonder <- dat.2014.wonder %>%
  left_join(age_adj_white, by = "FIPS")

##' spot check white population values from the dmf data - these are less because we are excluding ages 20-25

white.pop <- mod_2014 %>%
  filter(race == "Non-Hispanic White") %>%
  ungroup() %>%
  group_by(FIPS) %>%
  summarize(pop = sum(pop_2014)) %>%
  left_join(age_adj_white, by = "FIPS")
  
##' ### Healthcare Amenable Mortality

age_adj_hca <- read.delim(file = "../data-raw/wonder/wonder_2014_hca_age_adj.txt")
age_adj_hca <- age_adj_hca %>%
  #select(County, County.Code, Population, Deaths, Crude.Rate, Age.Adjusted.Rate) %>%
  mutate(across(Deaths:Age.Adjusted.Rate, ~ as.numeric(case_when(.x %in% c("Suppressed", "Unreliable", "Missing") ~ "",
                                                                 TRUE ~ .x)))) %>%
  mutate(FIPS = sprintf('%05d', County.Code),
         adj = Age.Adjusted.Rate/Crude.Rate,
         mort1 = Age.Adjusted.Rate/100000*Population,
         mort = Deaths*adj) %>%
  mutate(mort_hca_wndr = case_when(is.na(Crude.Rate) & !is.na(Deaths) ~ Deaths,
                          TRUE ~ round(mort))) %>%
  select(FIPS, mort_hca_wndr)

dat.2014.wonder <- dat.2014.wonder %>%
  left_join(age_adj_hca, by = "FIPS")

##' ### White Healthcare Amenable Mortality
age_adj_white_hca <- read.delim(file = "../data-raw/wonder/wonder_2014_hca_white_age_adj.txt")
age_adj_white_hca <- age_adj_white_hca %>%
  #select(County, County.Code, Population, Deaths, Crude.Rate, Age.Adjusted.Rate) %>%
  mutate(across(Deaths:Age.Adjusted.Rate, ~ as.numeric(case_when(.x %in% c("Suppressed", "Unreliable", "Missing") ~ "",
                                                                 TRUE ~ .x)))) %>%
  mutate(FIPS = sprintf('%05d', County.Code),
         adj = Age.Adjusted.Rate/Crude.Rate,
         mort = Deaths*adj) %>%
  mutate(mort_hca_wht_wndr = case_when(is.na(Crude.Rate) & !is.na(Deaths) ~ Deaths,
                                   TRUE ~ round(mort))) %>%
  dplyr::select(FIPS, mort_hca_wht_wndr)

dat.2014.wonder <- dat.2014.wonder %>%
  left_join(age_adj_white_hca, by = "FIPS")

##' ### Check censoring and spot checks

mean(is.na(dat.2014.wonder$mort_dmf))
mean(is.na(dat.2014.wonder$mort_wndr))
mean(is.na(dat.2014.wonder$mort_wht_wndr))
mean(is.na(dat.2014.wonder$mort_hca_wndr))
mean(is.na(dat.2014.wonder$mort_hca_wht_wndr))

##' For super white counties, the age adjusted # of white deaths might be one death larger
##' than the # of overall deaths

check <- dat.2014.wonder %>%
  filter(mort_wht_wndr > mort_wndr) %>%
  select(FIPS, pop, white_race, starts_with("mort_"))

##' The same thing for health care amenable, but there are only 14 cases

check2 <- dat.2014.wonder %>%
  filter(mort_hca_wndr > mort_wndr) %>%
  select(FIPS, pop, starts_with("mort_"))
  
##' ## Variable selection
##' 
##' ### Load in variables from DMF analysis

var.names <- var_list$all_cause

#there are some variables that we don't have now, so get the intersection of the two sets. The only things excluded are for race
vars.wonder <- var.names[-c(1:6)]
vars.wonder <- vars.wonder[!str_detect(vars.wonder, "age")]

save(vars.wonder, file = "../data/temp/wonder.vars.Rdata")  


##' ### LASSO on county level outcomes from WONDER

##' get a list of variable names
vars <- dat.2014.wonder %>%
  ungroup() %>% 
  select(mort_all_cause_l1, mort_all_cause_l2, white_race:calc_multi_house, log_adult_w_a_cnt) %>%
  colnames()

# formula with age and interactions
form <- formula(paste0(" ~ ", paste(vars, collapse="+")))


mod.dat <- dat.2014.wonder[complete.cases(dat.2014.wonder),]

X <- model.matrix(form, data = mod.dat)[,-1]
y <- mod.dat$mort_wndr
off <- mod.dat$pop

#the LASSO model with `lamda.min` wouldn't drop any variables
cv.out.all <- cv.glmnet(x=X, y=y, family = quasipoisson(),  offset = log(off), alpha = 1)
vars.wonder2 <- names(coef(cv.out.all, s = "lambda.min")[which(coef(cv.out.all, s = "lambda.min")[,1]!=0),1])[-1]                      

##' ## Calculate Interval Data

##' ### WONDER for outcome only

if(int.est){
  int.dat.wonder <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by =.01,
                                          data = dat.2014.wonder, forml = form, out_name = "mort_wndr",
                                          var.names = vars.wonder)
  save(int.dat.wonder, file = "../data/temp/wonder.int.dat")
}

##' ### WONDER for lag variables as well

dat.2014.wonder <- dat.2014.wonder %>%
  rename(mort_all_cause_l1o = mort_all_cause_l1,
         mort_all_cause_l2o = mort_all_cause_l2,
         mort_all_cause_l1 = mort_all_cause_l1w,
         mort_all_cause_l2 = mort_all_cause_l2w)

if(int.est){
  int.dat.wonder.lags <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by =.01,
                                          data = dat.2014.wonder, forml = form, out_name = "mort_wndr",
                                          var.names = vars.wonder)
  save(int.dat.wonder.lags, file = "../data/temp/wonder.int.dat.wl")
  
}

##' ### HCA outcome


if(int.est){
  int.dat.wonder.hca <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by =.01,
                                           data = dat.2014.wonder, forml = form, out_name = "mort_hca_wndr",
                                           var.names = vars.wonder, m.dat.list = m.dat.list)
  
  save(int.dat.wonder.hca, file = "../data/temp/wonder.int.dat.hca.Rdata")
}


##' ### White Outcome Subgroup Analysis

dat.2014.wonder.white <- dat.2014.wonder %>%
  select(-pop)  %>%
  rename(pop = pop.white)

if(int.est){
  int.dat.wonder.white <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by =.01,
                                        data = dat.2014.wonder.white, forml = form, out_name = "mort_wht_wndr",
                                        var.names = vars.wonder, m.dat.list = m.dat.list.white)
  
  save(int.dat.wonder.white, file = "../data/temp/wonder.int.dat.white.Rdata")
}


##' ### White HCA Outcome Subgroup Analysis


if(int.est){
  int.dat.wonder.hca.white <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by =.01,
                                           data = dat.2014.wonder.white, forml = form, out_name = "mort_hca_wht_wndr",
                                           var.names = vars.wonder, m.dat.list = m.dat.list.white)
  
  save(int.dat.wonder.hca.white, file = "../data/temp/wonder.int.dat.hca.white.Rdata")
}


##' Save all of the data for use in `31-wonder-results.R`

save(dat.2014.wonder, m.dat.list, m.dat.list.white,
     form, vars.wonder, vars.wonder2, 
     file = "../data/temp/all_wonder_res.Rdata")

