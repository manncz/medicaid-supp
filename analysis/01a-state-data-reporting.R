##' ---
##' title: "Considering state data reporting of flu and influenza deaths"
##' output: github_document
##' ---
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "stringr")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' The [FluView](https://gis.cdc.gov/grasp/fluview/mortality.html) interactive site with the CDC reports an estimated \% of deaths reported in a given
##' week. We are hoping to use this to get an idea for which states may be reporting at different times
##' to address the issue that the CDC raises that provisional death counts should not be used in comparisons
##' across states. 
##' 
# ##' The data was downloaded on 8/17 and contains the data from 2012-2013 and 2013-2014. 
# 
# fluview <- read.csv("../data-raw/State_Custom_data.csv") %>%
#   filter(SEASON == "2012-13" & WEEK == 36) %>%
#   #filter( WEEK %in% c(40:52)) %>%
#   mutate(perc_report = as.numeric(str_replace_all(PERCENT.COMPLETE, ">\\s|%", "")))%>%
#   #group_by(SUB.AREA) %>%
#   #summarize(perc_report = mean(perc_report, na.rm = T)) %>%
#   mutate(report_cat = case_when(perc_report <= quantile(perc_report, .25) ~ "slow",
#                                 perc_report >= quantile(perc_report, .75) ~ "fast",
#                                 TRUE ~ "average")) %>%
#   dplyr::select(state = SUB.AREA, perc_report, report_cat) %>%
#   write_csv("../data/base_data_reporting.csv")

##' Realizing that the data is updated as new mortality comes into play, we will instead consider 2019-2020 mortality as
##' reported. We download the data each week.

weeks <- str_extract(list.files(path = "../data-raw/flu-view"), "\\d\\d")
raw.dat <- list()

for (week in weeks){
  raw.dat[[paste0("flu_view_", week)]] <- read.csv(paste0("../data-raw/flu-view/flu_view_", week, ".csv")) %>%
    filter(SEASON %in% c("2019-20", "2020-21"))
}

flu_view <- raw.dat[["flu_view_31"]] %>%
  dplyr::select(state = SUB.AREA, week = WEEK, season = SEASON, perc_report_ = PERCENT.COMPLETE)%>%
  rename_with(~paste0(.x, "31"),  contains("perc"))

for (wk in weeks[-1]){
  flu_view <-  raw.dat[[paste0("flu_view_", wk)]] %>%
    dplyr::select(state = SUB.AREA, week = WEEK, season = SEASON, perc_report_ = PERCENT.COMPLETE) %>%
    rename_with(~paste0(.x, wk),  contains("perc")) %>%
    full_join(flu_view, by = c("state", "week", "season"))
}

flu_view <- flu_view %>%
  filter(!(state %in% c("Alaska", "Hawaii")))

save(flu_view, file = "../data/flu_view_data_reporting.csv")

##' Instead of considering their % reporting value, we will look at actually all cause deaths reported.

flu_view_deaths <- raw.dat[["flu_view_31"]] %>%
  dplyr::select(state = SUB.AREA, week = WEEK,season = SEASON, deaths_ = TOTAL.DEATHS)%>%
  rename_with(~paste0(.x, "31"),  contains("deaths"))

for (wk in weeks[-1]){
  flu_view_deaths <-  raw.dat[[paste0("flu_view_", wk)]] %>%
    dplyr::select(state = SUB.AREA, week = WEEK, season = SEASON, deaths_ = TOTAL.DEATHS) %>%
    rename_with(~paste0(.x, wk),  contains("deaths")) %>%
    full_join(flu_view_deaths, by = c("state", "week", "season"))
}

flu_view_deaths <- flu_view_deaths %>%
  filter(!(state %in% c("Alaska", "Hawaii"))) 

save(flu_view_deaths, file = "../data/flu_view_data_reporting_deaths.csv")


##' The week that we use in the 2020 analyses (1/02/21 corresponds to week 53 for the 20-21 season)
##' Week 39 in 2019-2020 corresponds to week ending 9/26
##' We have full data 2019-2021 seasons starting in week 47 and we have this through week 58, but missing 55

##' Here, we will read in the data from only the 2020-2021 sets

