##' ---
##' title: "Pulling and grouping ICD-10 Codes"
##' output: github_document
##' ---
##'
#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
library(dplyr)
library(tidyr)
library(stringr)

##' This document is setting up code to be incorporated into `1-base-mortality.R`. `base_mort_crosstab.csv` is currently outputted
##' by `1-base-mortality.R` and includes all of the ICD-10 codes in the CDC mortality data currently available.

#read in crosstab of ICD-10 codes from base mortality
base_mort_x <- read.csv("../data/base_mort_crosstab.csv")

#check the codes and create a list of unique codes to work with
codes <- base_mort_x %>%
  select(icd10, c_o_d_r) %>%
  distinct

##' ### Amenable Deaths  
##' We use the definitions from [Sommers et al. 2014](https://umich.box.com/s/76yvl2o90gk57s5ajrrxpwdgdwqp1f7f) for amenable deaths, with their table replicated below.
##' The first three characters of the ICD-10 codes indicated are used (i.e. [X##] where X is a letter) as the 
##' ICD-10 codes in the data seem to have other numbers as suffixes in addition to how the codes are described with these three characters in most resources.
##'
##' |   Condition(s)                                                                                           |   ICD-10 Codes                |   Nolte McKee Definition   |   Sommers Definition   |
##' |----------------------------------------------------------------------------------------------------------|-------------------------------|------------------------------|----------------------|
##' |   Infectious & Parasitic Diseases (ALL)                                                                  |   A00-B99                     |                              |   X                             |
##' |   -Tuberculosis                                                                                          |   A16-19, B90                 |   X                          |   X                             |
##' |   -Other specific infections (diphtheria, tetanus, septicemia, poliomyelitis, whooping cough, measles)   |   A35-A37, A40-41, A80, B05   |   X                          |   X                             |
##' |   Neoplasms (ALL)                                                                                        |   C00-D48                     |                              |   X                             |
##' |   -Malignant neoplasm of colon and rectum                                                                |   C18-C21                     |   X                          |   X                             |
##' |   -Malignant neoplasm of skin                                                                            |   C44                         |   X                          |   X                             |
##' |   -Malignant neoplasm of breast                                                                          |   C50                         |   X                          |   X                             |
##' |   -Malignant neoplasm of cervix or uterus                                                                |   C53-C55                     |   X                          |   X                             |
##' |   -Malignant neoplasm of testis                                                                          |   C62                         |   X                          |   X                             |
##' |   -Hodgkin’s disease                                                                                     |   C81                         |   X                          |   X                             |
##' |   -Leukemia                                                                                              |   C91-C95                     |   X                          |   X                             |
##' |   Disorders of thyroid gland                                                                             |   E00-E07                     |   X                          |   X                             |
##' |   Diabetes Mellitus                                                                                      |   E10-E14                     |   X                          |   X                             |
##' |   Epilepsy                                                                                               |   G40-G41                     |   X                          |   X                             |
##' |   Chronic rheumatic heart diseases                                                                       |   I05-I09                     |   X                          |   X                             |
##' |   Hypertensive diseases                                                                                  |   I10-I13, I15                |   X                          |   X                             |
##' |   Ischemic heart diseases                                                                                |   I20-I25                     |   X                          |   X                             |
##' |   Cardiomyopathy                                                                                         |   I42                         |                              |   X                             |
##' |   Atrial fibrillation and flutter                                                                        |   I48                         |                              |   X                             |
##' |   Other cardiac arrhythmias                                                                              |   I49                         |                              |   X                             |
##' |   Heart failure                                                                                          |   I50                         |                              |   X                             |
##' |   Cerebrovascular diseases                                                                               |   I60-I69                     |   X                          |   X                             |
##' |   All respiratory diseases                                                                               |   J00-J98                     |   X                          |   X                             |
##' |   Gastric and duodenal ulcers                                                                            |   K25-K27                     |   X                          |   X                             |
##' |   Gastrojejunal ulcers                                                                                   |   K28                         |                              |   X                             |
##' |   Diseases of appendix                                                                                   |   K35-K38                     |   X                          |   X                             |
##' |   Hernia                                                                                                 |   K40-K46                     |   X                          |   X                             |
##' |   Diseases of gallbladder and biliary tract                                                              |   K80-K83                     |   X                          |   X                             |
##' |   Acute pancreatitis                                                                                     |   K85                         |                              |   X                             |
##' |   Infections of the skin and subcutaneous tissue                                                         |   L00-L08                     |                              |   X                             |
##' |   Infectious arthropathies                                                                               |   M00-M02                     |                              |   X                             |
##' |   Glomerular diseases                                                                                    |   N00-N07                     |   X                          |   X                             |
##' |   Renal tubulo-interstitial diseases                                                                     |   N10-N15                     |                              |   X                             |
##' |   Renal failure                                                                                          |   N17-N19                     |   X                          |   X                             |
##' |   Unspecified contracted kidney, small kidney unknown cause                                              |   N26-N27                     |   X                          |                                 |
##' |   Hyperplasia of prostate                                                                                |   N40                         |   X                          |                                 |
##' |   Pregnancy, childbirth and the puerperium                                                               |   O00-O99                     |   X                          |   X                             |
##' |   Congenital malformations originating in the perinatal period                                           |   P00-P96                     |   X                          |                                 |
##' |   Misadventures to patients during surgical and medical care                                             |   Y60-Y69, Y83-Y84            |   X                          |   X                             |
##' 

#create all regex strings for ICD-10 code beginnings
amenable <- c("A", "B", "C",
              paste0("D", str_pad(0:48, 2, pad = "0")),
              paste0("E", str_pad(c(0:7,10:14) , 2, pad = "0")),
              paste0("G", str_pad(40:41, 2, pad = "0")),
              paste0("I", str_pad(c(5:9, 10:13, 15, 20:25, 42, 48:50, 60:69), 2, pad = "0")),
              paste0("J", str_pad(0:98, 2, pad = "0")),
              paste0("K", str_pad(c(25:28, 35:38, 40:46, 80:83, 85), 2, pad = "0")),
              paste0("L", str_pad(0:8, 2, pad = "0")),
              paste0("M", str_pad(0:2, 2, pad = "0")),
              paste0("N", str_pad(c(0:7, 10:15, 17:19, 26:27, 40), 2, pad = "0")),
              "O",
              paste0("P", str_pad(0:96, 2, pad = "0")),
              paste0("Y", str_pad(c(60:69, 83:84), 2, pad = "0"))
              )

amenable_str <- paste(amenable, collapse = "|")

#check that these codes exist in the data 
amenable_sub <- codes %>%
  filter(str_detect(icd10, amenable_str))

##' ### Opioid Overdose  
##' We use the CDC definitions of ICD-10 codes for opioid overdoses found 
##' [here](https://stacks.cdc.gov/view/cdc/59394).
##' All of the codes listed are in the data. 

opioid <- c("X40", "X41", "X42", "X43", "X44", 
            "X60", "X61", "X62", "X63", "X64", 
            "X85", "Y10", "Y11", "Y12", "Y13", "Y14")

#check that these codes exist in the data 
opioid_sub <- codes %>%
  filter(icd10 %in% opioid)

##' ### Flu & pneumonia  
##' We use the definitions from the [NCHS](http://health.utah.gov/opha/IBIShelp/codes/NCHS113.htm) J09-J18.
##' All of these codes are found in the data, but with some different suffixes that need exploring. 
##' Additionally, the 113 recodes in the data do not match that of those listed on line.
  
flu <- c("J09", "J10", "J11", "J12", "J13",
         "J14", "J15", "J16", "J17", "J18")

flu_str <- paste(flu, collapse = "|")

#check that these codes exist in the data 
# we need to be able to use strdetect instead - need to check that this is true and how they are stored
flu_sub <- codes %>%
  filter(str_detect(icd10, flu_str))

#in this data the 113 codes are 77 and 78.. not sure why this doesn't match the published table, which gives 66 and 67.
table(flu_sub$c_o_d_r)


##' ### Indicator variables for mortality type
##' Below there is sample code for adding indicator variables for different types of mortality
##' to the base-mortality code.

base_mort_x <- base_mort_x %>%
  mutate(amenable = case_when(str_detect(icd10, amenable_str) ~ 1,
                              TRUE ~ 0),
         opioid = case_when(icd10 %in% opioid ~ 1,
                              TRUE ~ 0),
         flu = case_when(str_detect(icd10, flu_str) ~ 1,
                              TRUE ~ 0))
##' ## Save artifacts
##'
##' ...for use in other scripts
##'
amenable_icd10  <- amenable
flu_icd10  <- flu
opioid_icd10  <- opioid
save(amenable_icd10, flu_icd10, opioid_icd10,
     file="../data/icd10_strings.RData")
##'
#code to render document (may need to be run in command line to prevent looping)
#rmarkdown::render("01b_read_icd10.R")
