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
library(purrr)
stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
##'
##' Read in the mortality data for 2018, which is only needed to calculate the m weight in the
##' outcome analysis.


#' Values for the Age 27 variable (for the levels that we are interested in):
##'
##' |lev|definition|
##' |---|----------|
##' | 10 | 20-24 years |
##' | 11 | 25-29 years |
##' | 12 | 30-34 years |
##' | 13 | 35-39 years |
##' | 14 | 40-44 years |
##' | 15 | 45-49 years |
##' | 16 | 50-54 years |
##' | 17 | 55-59 years |
##' | 18 | 60-64 years |

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
##' The race variable is imputed when missing, so we will count individuals who have imputed race into the "Other"
##' category as defined above.
##' 
##' For each year of data (2011-2014) we run the following process:
##' 1. Read in columns of interest
##' 2. Drop non working aged adults (age 20-64), and code both of the race and age variables. as described above.
##' 5. Collapse/sum number of deaths by age group, race categorization, and county.
##' 6. Save data in the temp folder.
##' 
##'You will note that there are two possible file paths, one for running on compute 1 and one for running 
##'on CM's local computer setup for testing.

summarize_det_dat_race_age <- function(year){
  file.name <- paste0("Mort", year, "US.AllCnty.txt")
  #f.path <- file.path("..","..","..", "compute1","data-raw",file.name)
  f.path <- file.path("..","data-raw",file.name)
  
  mort_detail  <-  f.path %>%
    read_fwf(fwf_cols(rec_type=c(19,19), # 'record type'
                      #res_status=c(20,20),  # 'residency status'
                      #stateoFIPS=c(21,22), # 'state of occurrence"
                      #cntyoFIPS=c(23,25), # county of occurrence
                      #cntyopop=c(28,28), # county of occurrence population
                      staterFIPS=c(29,30), # state of residence
                      cntyrFIPS=c(35,37), # county of residence
                      #cityrFIPS=c(38,42), # city of residence
                      #cityrpop=c(43,43), # city of residence population
                      #cntymetro=c(44,44), # metropolitan county status
                      #statebFIPS=c(55,56), # state of birth
                      #edu2003=c(63,63), # education (2003 version)
                      #mo_death=c(65,66), # month of death
                      year=c(102,105), # year of death
                      #sex=c(69,69),
                      age27=c(77,78), # age (Recode 27)
                      #age_inf=c(81,82), # infant age (Recode 22)
                      #pl_death=c(83,83), # place of death
                      #marital=c(84,84), # marital status
                      race_code=c(445,446), # race
                      race_bridge=c(447,447), # race bridge flag
                      race_imp=c(448,448), # race imputation flag
                      #race_rec3=c(449,449), # race (Recode 3)
                      race_rec5=c(450,450), # race (Recode 5)
                      #race_rec40=c(489,490), # race (Recode 40, allows for multiple races)
                      #hisp=c(484,486), # hispanic origin
                      hisp_code=c(488,488), # hispanic origin recode
                      #manner=c(107,107),
                      icd10=c(146,149), # ICD Code (10th Revision)
                      cond_no=c(341,342), # Number of conditions
                      rcon1=c(344,348), # Record Axis condition 1
                      rcon2=c(349,353),
                      rcon3=c(354,358),
                      rcon4=c(359,363),
                      rcon5=c(364,368),
                      rcon6=c(369,373),
                      rcon7=c(374,378),
                      rcon8=c(379,383),
                      rcon9=c(384,388),
                      rcon10=c(389,393),
                      rcon11=c(394,398),
                      rcon12=c(399,403),
                      rcon13=c(404,408),
                      rcon14=c(409,413),
                      rcon15=c(414,418),
                      rcon16=c(419,423),
                      rcon17=c(424,428),
                      rcon18=c(429,433),
                      rcon19=c(434,438),
                      rcon20=c(439,443)
                      #tobacco=c(142,142), # tobacco use contributed to death
                      #preg_stat=c(143,143), # pregnancy status
                      #certifier=c(110,110) # death certifier
    ), cols(rcon12=col_character(),
            rcon13=col_character(),
            rcon14=col_character(),
            rcon15=col_character(),
            rcon16=col_character(),
            rcon17=col_character(),
            rcon18=col_character(),
            rcon19=col_character(),
            rcon20=col_character()),
    progress=FALSE
    #, n_max = 5000 #set n_max for working on local computer
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
    mutate(race = case_when(race_code == "01" & hisp_code %in% non_hisp_codes & is.na(race_imp) ~ "Non-Hispanic White",
                            race_code == "02" & hisp_code %in% non_hisp_codes & is.na(race_imp) ~ "Non-Hispanic Black",
                            race_code == "03" & hisp_code %in% non_hisp_codes & is.na(race_imp) ~ "Non-Hispanic AI or AN",
                            race_code %in% asian & hisp_code %in% non_hisp_codes & is.na(race_imp) ~ "Non-Hispanic Asian",
                            race_code %in% pac_island & hisp_code %in% non_hisp_codes & is.na(race_imp) ~ "Non-Hispanic other Asian or Pacific Islander",
                            hisp_code %in% hisp_codes ~ "Hispanic",
                            TRUE ~ "Other"),
           race_bridged = case_when(race_rec5 == 1 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic White",
                                    race_rec5 == 2 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic Black",
                                    race_rec5 == 3 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic AI or AN",
                                    race_rec5 == 4 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic Asian",
                                    race_rec5 == 5 & hisp_code %in% c(non_hisp_codes,9) ~ "Non-Hispanic Pacific Islander",
                                    hisp_code %in% hisp_codes ~ "Hispanic",
                                    TRUE ~ "Other"),
           race_bridged_uk = case_when(race_rec5 == 1 & hisp_code %in% c(non_hisp_codes,9) & is.na(race_imp) ~ "Non-Hispanic White",
                                    race_rec5 == 2 & hisp_code %in% c(non_hisp_codes,9) & is.na(race_imp) ~ "Non-Hispanic Black",
                                    race_rec5 == 3 & hisp_code %in% c(non_hisp_codes,9) & is.na(race_imp) ~ "Non-Hispanic AI or AN",
                                    race_rec5 == 4 & hisp_code %in% c(non_hisp_codes,9) & is.na(race_imp) ~ "Non-Hispanic Asian",
                                    race_rec5 == 5 & hisp_code %in% c(non_hisp_codes,9) & is.na(race_imp) ~ "Non-Hispanic Pacific Islander",
                                    hisp_code %in% hisp_codes ~ "Hispanic",
                                    TRUE ~ "Other"))
  
  print(table(mort_detail$race_bridge))
  print(table(mort_detail$race_imp))
  
  #collapse over county, age, race, mortality type, and whether opioids are involved in any multiple causes of death.
  mort_det_sum <- mort_detail %>%
    group_by(staterFIPS, cntyrFIPS, year, race, race_bridged, race_bridged_uk, age) %>%
    summarize(mort = n())
  
  #save year's data
  save(mort_det_sum, file = paste0("../data/temp/det_mort_", year ,".Rdata"))
}


##' Now run this process on the years of data 2011-2014:

sapply(2018, summarize_det_dat_race_age)

##' Next, we will combine the data in order to create a master dataset 
##' with the variables of interest for our outcome covariance adjustment modeling.
file <- paste0("data/temp/det_mort_", 2018 ,".Rdata")
path <- file.path("..", file)
#path <- file.path("..","..","..", "compute1", file)
load(path)

##' The detailed mortality data includes abbreviations rather than FIPs codes, so we use a crosswalk
##' from the `maps` package to get the FIPS code for each abbreviation, to be able to merge with our
##' other datasets. This crosswalk contains the 50 mainland US states, which is what we are interested in
##' anyway.

data("state.fips")
fips.xwalk <- state.fips %>% 
  dplyr::select(fips, abb) %>%
  distinct()

mort_2018 <- mort_det_sum %>%
  left_join(fips.xwalk, by = c("staterFIPS"="abb")) %>%
  filter(!is.na(fips)) %>%
  dplyr::select(stateFIPS = fips, cntyFIPS = cntyrFIPS, everything()) %>%
  mutate(cntyFIPS = as.numeric(cntyFIPS),
         FIPS = paste0(sprintf('%02d', stateFIPS), sprintf('%03d', cntyFIPS))) %>%
  ungroup()%>%
  dplyr::select(FIPS, everything(), -staterFIPS)

save(mort_2018, file = file.path(".." ,"data","mort_2018.Rdata"))
#save(mort_2018, file = file.path("..","..","..", "compute1","data","mort_2018.Rdata"))
