##' ---
##' title: "Assemble/extract mortality variables for pararllel trend analysis"
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
##' Read in the mortality data for a given year keeping variables that may be of interest in 
##' outcome analysis. (Adapted TL code, commenting out variables we don't need)

load("../data/icd10_strings.RData")
list(amenable=amenable_icd10) %>%
  map_chr(paste, collapse="|") ->
  icd10_regexps

##'You will note that there are two possible file paths, one for running on compute 1 and one for running 
##'on CM's local computer setup for testing.

summarize_det_dat <- function(year, icd10codes = icd10_regexps){
  file.name <- paste0("MULT", year, ".USAllCnty.txt")
  if(year == 2018){
    file.name <- paste0("Mort", year, "US.AllCnty.txt")
  }
  
  #f.path <- file.path("..","..","..", "compute1","data-raw",file.name)
  f.path <- file.path("..","data-raw",file.name)
  
  mort_detail  <-  f.path %>%
    read_fwf(fwf_cols(rec_type=c(19,19), # 'record type'
                      staterFIPS=c(29,30), # state of residence
                      cntyrFIPS=c(35,37), # county of residence
                      year=c(102,105), # year of death
                      age27=c(77,78), # age (Recode 27)
                      race_rec5=c(450,450), # race (Recode 5)
                      hisp_code=c(488,488), # hispanic origin recode
                      icd10=c(146,149) # ICD Code (10th Revision)
    ),
    progress=FALSE, n_max = 1000 #set n_max for working on local computer
    )
  
  
  #code some categories for use in the race categorizations
  non_hisp_codes <- c(6:8)
  hisp_codes <- c(1:5)
  asian <- c("04", "05","07", "18", "28", "48")
  pac_island <- c("38","58","06", "68","78")
  
  #create age and race variables and drop ages outside of interest
  mort_detail <- mort_detail %>% 
    mutate(age = case_when(age27 %in% 10:12 ~ "20_34",
                           age27 %in% 13:14 ~ "35_44",
                           age27 %in% 15:16 ~ "45_54",
                           age27 %in% 17:18 ~ "55_64",
                           TRUE ~ "")) %>%
    filter(age != "") %>% #drop ages we aren't interested in to make rest of computing faster
    mutate(race_bridged = case_when(race_rec5 == 1 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic White",
                                    race_rec5 == 2 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic Black",
                                    race_rec5 == 3 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic AI or AN",
                                    race_rec5 == 4 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic Asian",
                                    race_rec5 == 5 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic Pacific Islander",
                                    hisp_code %in% hisp_codes ~ "Hispanic",
                                    TRUE ~ "Other"))
  
  
  #create variable categorizing deaths like in 01-base-mortality.R
  mort_detail <- mort_detail %>% 
    mutate(c_o_d_cat=
             case_when(!str_detect(icd10, icd10codes['amenable']) ~
                         "HC_unamnbl",
                       str_detect(icd10, icd10codes['flu']) ~
                         "Flu",
                       str_detect(icd10, icd10codes['amenable']) ~
                         "HC_amenable_not_flu",
                       TRUE ~ as.character(icd10) # (not caught by any of above
             ))
  
  #collapse over county, age, race, mortality type
  mort_det_sum <- mort_detail %>%
    group_by(staterFIPS, cntyrFIPS, year, race_bridged, age, c_o_d_cat) %>%
    summarize(mort = n())
  
  #save year's data
  return(mort_det_sum)
}


##' Now run this process on the years of data 2005-2018 and combine

mort_det_comb <- summarize_det_dat(2005)

for (year in 2006:2018){
  temp.dat <- summarize_det_dat(year)
  mort_det_comb <- rbind(mort_det_comb, temp.dat)
}

mort_det <- mort_det_comb %>%
  pivot_wider(names_from = c_o_d_cat, values_from = mort,
              names_glue = "{.value}_{c_o_d_cat}",
              values_fill = 0) 

mort_det$mort_all_cause = rowSums(mort_det[grep("mort", colnames(mort_det))])

data("state.fips")
fips.xwalk <- state.fips %>% 
  dplyr::select(fips, abb) %>%
  distinct()

mort_det_0518 <- mort_det %>%
  left_join(fips.xwalk, by = c("staterFIPS"="abb")) %>%
  filter(!is.na(fips)) %>%
  dplyr::select(stateFIPS = fips, cntyFIPS = cntyrFIPS, everything()) %>%
  mutate(cntyFIPS = as.numeric(cntyFIPS),
         FIPS = paste0(sprintf('%02d', stateFIPS), sprintf('%03d', cntyFIPS))) %>%
  ungroup()%>%
  dplyr::select(FIPS, everything(), -staterFIPS)


save(mort_det_0518, file = file.path("..", "data","all_det_mort_2005_18.Rdata"))


