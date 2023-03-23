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
##' All of the codes listed are in the data. This involves having a primary cause of an overdose
##' and then contributing cause that is specific to opioids
##' 
##' We add an indicator variable 'opioid_involved' that flags deaths
##' with any opioid primary or contributing cause of death 
##' (i.e. in rcon1-rcon20). We use the codes set up in 01b_read_icd10.

##' Since we now have the multiple causes of death data, we can get more specific about
##' opioid related overdoses. The codes included prior were for overdose in general.
opioid_icd10_contributing <- c("T400","T401","T402","T403",
                               "T404", "T406")

load("../data/icd10_strings.RData")
list(amenable=amenable_icd10,
     flu=flu_icd10,
     overdose=opioid_icd10,
     opioid = opioid_icd10_contributing) %>%
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
##' 3. Create indicator that opioids were involved in any of the multiple causes of death.
##' 4. Create cause of death variable based on ICD-10 code (primary cause of death) as in `01-base-mortality.R`, grouped into
##' healthcare amenable (non flu), flu, healthcare non-amenable (non opioid), and opioid.
##' 5. Collapse/sum number of deaths by age group, race categorization, county, type of death, and involvement of opioids.
##' 6. Save data in the temp folder.
##' 
##'You will note that there are two possible file paths, one for running on compute 1 and one for running 
##'on CM's local computer setup for testing.

summarize_det_dat <- function(year, icd10codes = icd10_regexps){
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
    mutate(age = case_when(age27 %in% 19:20 ~ "65_74",
                           age27 %in% 21:22 ~ "75_84",
                           age27 %in% 23:24 ~ "85_94",
                           age27 %in% 25:26 ~ ">94",
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
                                    TRUE ~ "Other"))
  
  print(table(mort_detail$race_bridge))
  print(table(mort_detail$race_imp))
  
  #create indicator that opioids are involved in any multiple causes of death (record conditions)
  mort_detail <- mort_detail %>% 
    mutate(across(contains("rcon"), ~str_detect(.x, icd10codes["opioid"]))) %>%
    mutate(opioid_involved = case_when(rowSums(.[grep("rcon", names(.))], na.rm = TRUE) > 0 ~ 1,
                               TRUE ~ 0)) %>%
    select(-c(rcon1:rcon20))
    
  #create variable categorizing deaths like in 01-base-mortality.R
  mort_detail <- mort_detail %>% 
    mutate(c_o_d_cat=
             case_when(str_detect(icd10, icd10codes['overdose']) & opioid_involved == 1 ~
                          "Opioid",
                        !str_detect(icd10, icd10codes['amenable']) ~
                          "HC_unamnbl_not_opioid",
                        str_detect(icd10, icd10codes['flu']) ~
                          "Flu",
                        str_detect(icd10, icd10codes['amenable']) ~
                          "HC_amenable_not_flu",
                        TRUE ~ as.character(icd10) # (not caught by any of above
             ))
  
  #collapse over county, age, race, mortality type, and whether opioids are involved in any multiple causes of death.
  mort_det_sum <- mort_detail %>%
    group_by(staterFIPS, cntyrFIPS, year, race, race_bridged, age, c_o_d_cat, opioid_involved) %>%
    summarize(mort = n())
  
  #save year's data
  save(mort_det_sum, file = paste0("../data/temp/det_mort_65", year ,".Rdata"))
}


##' Now run this process on the years of data 2011-2014:

sapply(2011:2014, summarize_det_dat)

##' Next, we will combine the data in order to create a master dataset 
##' with the variables of interest for our outcome covariance adjustment modeling.
file <- paste0("data/temp/det_mort_65", 2011 ,".Rdata")
path <- file.path("..", file)
#path <- file.path("..","..","..", "compute1", file)
load(path)
mort_det_comb <- mort_det_sum

for (year in 2012:2014){
  path <- file.path("..", paste0("data/temp/det_mort_65", year ,".Rdata"))
  #path <- file.path("..","..","..", "compute1", paste0("data/temp/det_mort_65", year ,".Rdata"))
  load(path)
  mort_det_comb <- rbind(mort_det_comb, mort_det_sum)
}


mort_det <- mort_det_comb %>%
  pivot_wider(names_from = c(c_o_d_cat,opioid_involved), values_from = mort,
              names_glue = "{.value}_{c_o_d_cat}_{opioid_involved}",
              values_fill = 0) 

mort_det$mort_all_cause = rowSums(mort_det[grep("mort", colnames(mort_det))])
mort_det$mort_opioid_involved = rowSums(mort_det[grep("_1", colnames(mort_det))])

##' Check that sums make sense
sum(mort_det$mort_HC_amenable_not_flu_0 + mort_det$mort_HC_unamnbl_not_opioid_0 + mort_det$mort_Flu_0 + mort_det$mort_opioid_involved != mort_det$mort_all_cause)
sum(mort_det$mort_HC_amenable_not_flu_0 + mort_det$mort_HC_unamnbl_not_opioid_0 + mort_det$mort_Flu_0 + mort_det$mort_HC_amenable_not_flu_1 + mort_det$mort_HC_unamnbl_not_opioid_1 + mort_det$mort_Flu_1 + mort_det$mort_Opioid_1 != mort_det$mort_all_cause)

##' Check proportion of deaths that have opioid as one of the mulitple causes of death
sum(mort_det$mort_HC_amenable_not_flu_1[mort_det$year == 2013])/sum(mort_det$mort_HC_amenable_not_flu_1[mort_det$year == 2013] + mort_det$mort_HC_amenable_not_flu_0[mort_det$year == 2013])
sum(mort_det$mort_Flu_1[mort_det$year == 2013])/sum(mort_det$mort_Flu_1[mort_det$year == 2013] + mort_det$mort_Flu_0[mort_det$year == 2013])


##' Reshape data so that it contains each variable by year. Because the proportion of deaths that were
##' partially attributed to opioids was so small, we instead just great a healthcare amenable and flu variable
##' that includes all such deaths with that as the primary outcome.
mort_det <- mort_det %>%
  mutate(mort_HC_amenable_not_flu = mort_HC_amenable_not_flu_0 + mort_HC_amenable_not_flu_1,
         mort_Flu = mort_Flu_0 + mort_Flu_1) %>%
  dplyr::select(staterFIPS:age, mort_HC_amenable_not_flu, mort_Flu,
                mort_Opioid = mort_Opioid_1, mort_all_cause,
                mort_HC_amenable_not_flu_no_opioid = mort_HC_amenable_not_flu_0, 
                mort_Flu_no_opioid = mort_Flu_0, mort_opioid_involved) %>%
  pivot_wider(names_from = year, values_from = c(mort_HC_amenable_not_flu, mort_Flu , mort_opioid_involved, mort_Opioid, 
                                                 mort_all_cause, mort_HC_amenable_not_flu_no_opioid,mort_Flu_no_opioid),
              names_glue = "{.value}_{year}",
              values_fill = 0)

##' The detailed mortality data includes abbreviations rather than FIPs codes, so we use a crosswalk
##' from the `maps` package to get the FIPS code for each abbreviation, to be able to merge with our
##' other datasets. This crosswalk contains the 50 mainland US states, which is what we are interested in
##' anyway.

data("state.fips")
fips.xwalk <- state.fips %>% 
  dplyr::select(fips, abb) %>%
  distinct()

mort_det_65 <- mort_det %>%
  left_join(fips.xwalk, by = c("staterFIPS"="abb")) %>%
  filter(!is.na(fips)) %>%
  dplyr::select(stateFIPS = fips, cntyFIPS = cntyrFIPS, everything()) %>%
  mutate(cntyFIPS = as.numeric(cntyFIPS),
         FIPS = paste0(sprintf('%02d', stateFIPS), sprintf('%03d', cntyFIPS))) %>%
  ungroup()%>%
  dplyr::select(FIPS, everything(), -staterFIPS)

save(mort_det_65, file = file.path("..","data","all_det_mort_65.Rdata"))
#save(mort_det_65, file = file.path("..","..","..", "compute1","data","all_det_mort_65.Rdata"))
