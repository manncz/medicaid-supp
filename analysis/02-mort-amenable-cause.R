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
##' Read in the mortality data for a given year keeping variables that may be of interest in 
##' outcome analysis. (Adapted TL code, commenting out variables we don't need)
##' 
##' 
##' ##' ### Opioid Overdose  
##' We use the CDC definitions of ICD-10 codes for opioid overdoses found 
##' [here](https://www.cdc.gov/drugoverdose/pdf/pdo_guide_to_icd-9-cm_and_icd-10_codes-a.pdf).
##' All of the codes listed are in the data. 
##' 
##' We add an indicator variable 'opioid_involved' that flags deaths
##' with any opioid primary or contributing cause of death 
##' (i.e. in rcon1-rcon20). We use the codes set up in 01b_read_icd10.

load("../data/icd10_strings.RData")

##' Since we now have the multiple causes of death data, we can get more specific about
##' opioid related overdoses. The codes included prior were for overdose in general.

list(amenable=amenable_icd10,
     flu=flu_icd10,
     overdose=opioid_icd10) %>%
  map_chr(paste, collapse="|") ->
  icd10_regexps


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

##' 
##' For each year of data (2011-2014) we run the following process:
##' 1. Read in columns of interest
##' 2. Drop non working aged adults (age 20-64), and code both of the race and age variables. as described above.
##' 3. Create cause of death variable based on ICD-10 code (primary cause of death) as in `01-base-mortality.R`, grouped into
##' healthcare amenable (non flu), flu, healthcare non-amenable (non opioid), and opioid.
##' 4. Collapse/sum number of deaths by age group, county, type of death only for healthcare unamenable
##' 5. Save data in the temp folder.
##' 
##'You will note that there are two possible file paths, one for running on compute 1 and one for running 
##'on CM's local computer setup for testing.

summarize_det_dat_amenable <- function(year, icd10codes = icd10_regexps){
  file.name <- paste0("MULT", year, ".USAllCnty.txt")
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
                      icd10=c(146,149),
                      #tobacco=c(142,142), # tobacco use contributed to death
                      #preg_stat=c(143,143), # pregnancy status
                      #certifier=c(110,110) # death certifier
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
    progress=FALSE#, n_max = 5000 #set n_max for working on local computer
    )
  
  #create age and race variables and drop ages outside of interest
  mort_detail <- mort_detail %>% 
    mutate(age = case_when(age27 %in% 10:12 ~ "20_34",
                    age27 %in% 13:14 ~ "35_44",
                    age27 %in% 15:16 ~ "45_54",
                    age27 %in% 17:18 ~ "55_64",
                    TRUE ~ "")) %>%
    filter(age != "") %>%
    select(-c(rcon1:rcon20)) 

  
  #create varible categorizing deaths like in 01-base-mortality.R
  mort_detail <- mort_detail %>% 
    mutate(c_o_d_cat=
             case_when(str_detect(icd10, icd10codes['flu']) ~
                          "Flu",
                        str_detect(icd10, icd10codes['amenable']) ~
                          "HC_amenable_not_flu",
                        TRUE ~ as.character(icd10) # (not caught by any of above
             )) %>%
    filter((c_o_d_cat %in% c("HC_amenable_not_flu","Flu"))) %>%
    mutate(c_o_d_det = case_when(c_o_d_cat == "Flu" ~ "Flu",
                                 str_detect(icd10, "A|B") ~ "Infectious_parasitic_diseases",
                                 str_detect(icd10, "C|D") ~ "Neoplasms",
                                 str_detect(icd10, "E00|E01|E02|E03|E04|E05|E06|E07") ~ "Thyroid_disorder",
                                 str_detect(icd10, "E10|E11|E12|E13|E14") ~ "Diabetes",
                                 str_detect(icd10, "G") ~ "Epilepsy",
                                 str_detect(icd10, "I") ~ "Heart_diseases",
                                 str_detect(icd10, "J") ~ "Other_respiratory",
                                 str_detect(icd10, "K") ~ "Ulcers_hernia_gallbladder",
                                 str_detect(icd10, "L|M") ~ "Infections",
                                 str_detect(icd10, "N") ~ "Kidney_diseases_failure",
                                 str_detect(icd10, "O") ~ "Pregnancy_childbirth",
                                 str_detect(icd10, "P") ~ "Prenatal",
                                 str_detect(icd10, "Y") ~ "Medical_care",
                                 TRUE ~ icd10
                                 ))
  
  print(table(mort_detail$c_o_d_cat))
  print(table(mort_detail$c_o_d_det))
  
  #collapse over county, age, race, mortality type, and whether opioids are involved in any multiple causes of death.
  amblmort_det_sum <- mort_detail %>%
    group_by(staterFIPS, cntyrFIPS, year, age, c_o_d_cat, c_o_d_det, icd10) %>%
    summarize(mort = n())
  
  #save year's data
  save(amblmort_det_sum, file = paste0("../data/temp/det_ambl_mort_", year ,".Rdata"))
}


##' Now run this process on the years of data 2011-2014:

sapply(2010:2014, summarize_det_dat_amenable)

##' Next, we will combine the data in order to create a master dataset 
##' with the variables of interest for our outcome covariance adjustment modeling.
file <- paste0("data/temp/det_ambl_mort_", 2010 ,".Rdata")
path <- file.path("..", file)
#path <- file.path("..","..","..", "compute1", file)
load(path)
mort_ambl_det_comb <- amblmort_det_sum

for (year in 2011:2014){
  path <- file.path("..", paste0("data/temp/det_ambl_mort_", year ,".Rdata"))
  #path <- file.path("..","..","..", "compute1", paste0("data/temp/det_unabl_mort_", year ,".Rdata"))
  load(path)
  mort_ambl_det_comb <- rbind(mort_ambl_det_comb, amblmort_det_sum)
}

data("state.fips")
fips.xwalk <- state.fips %>% 
  dplyr::select(fips, abb) %>%
  distinct()

mort_ambl_det <- mort_ambl_det_comb %>%
  left_join(fips.xwalk, by = c("staterFIPS"="abb")) %>%
  filter(!is.na(fips)) %>%
  dplyr::select(stateFIPS = fips, cntyFIPS = cntyrFIPS, everything()) %>%
  mutate(cntyFIPS = as.numeric(cntyFIPS),
         FIPS = paste0(sprintf('%02d', stateFIPS), sprintf('%03d', cntyFIPS))) %>%
  ungroup()%>%
  dplyr::select(FIPS, everything(), -staterFIPS)

save(mort_ambl_det, file = file.path("..", "data", "amenable_c_o_d.Rdata"))
#save(mort_unabl_det, file = file.path("..","..","..", "compute1","data","unamnbl_c_o_d.Rdata"))
