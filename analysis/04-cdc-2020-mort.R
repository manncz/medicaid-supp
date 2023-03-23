##'
##' title: "Combine and clean 2020 mortaltiy data from the CDC"
##' output: github_document
##' 
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "stringr", "lubridate")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' Load state reporting lag data calculated from FluView data in `09-calc-state-lag.R`

load("../data/temp/state.lag.05.05.Rdata")
load("../data/temp/state_lag_impute_perc.Rdata")

##' We have the provisional CDC mortality data downloaded since 8/01/2020 (41 weeks, since missing a couple)

weeks <- str_extract(list.files(path = "../data-raw/cdc-2020-mort/"), "\\d\\d.\\d{1,2}.\\d\\d")
raw.dat <- list()

##' We only care about the data starting on 1/2/21 for this analysis

analysis.weeks <- weeks[which(weeks == "21.01.02"):41]

##' ## Overall Mortality

for (week in analysis.weeks){
  raw.dat[[paste0("cdc_", week)]] <- read.csv(paste0("../data-raw/cdc-2020-mort/Provisional_COVID-19_Death_Counts_in_the_United_States_by_County_", week, ".csv")) %>%
    dplyr::select(FIPS = 6, state = State, covid_mort = Deaths.involving.COVID.19, all_mort = Deaths.from.All.Causes) %>%
    mutate(dat_week = week)
}

mort.2020.raw <- raw.dat[[paste0("cdc_", analysis.weeks[1])]]

for (week in analysis.weeks[-1]){
  mort.2020.raw  <-  rbind(mort.2020.raw , raw.dat[[paste0("cdc_", week)]])
}

mort.2020 <- mort.2020.raw %>%
  arrange(FIPS) %>%
  mutate(ind = case_when(dat_week == "21.01.02" ~ 1,
                         TRUE ~ 0)) %>%
  group_by(FIPS) %>%
  mutate(incl = sum(ind)) %>%
  filter(incl == 1) %>%
  mutate(date = ymd(dat_week),
         week = week(date),
         weeks = week-1,
         add_covid_mort = case_when(weeks == 0 ~ covid_mort,
                                    TRUE ~ covid_mort - lag(covid_mort)),
         add_all_mort = case_when(weeks == 0 ~ all_mort,
                                  TRUE ~ all_mort- lag(all_mort)),
         FIPS = sprintf('%05d', FIPS)) %>%
  dplyr::select(FIPS, state_ab = state, date, weeks, covid_mort, all_mort, starts_with("add")) %>%
  filter(state_ab != "HI" & state_ab != "AK")
  
save(mort.2020, file = "../data/temp/mort.2020.Rdata")

##' Create a crosswalk between abbreviations and state names to merge lag data
state.xwalk <- tibble(state = state.name) %>%
  bind_cols(tibble(state_ab = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", state_ab = "DC"))

##' Append the lag calculations from FluView.
lag.adjusted.mort.2020.calc <- mort.2020 %>%
  left_join(state.xwalk, by = "state_ab") %>%
  left_join(state.lag.xwalk, by = c("state" = "stateName")) %>%
  left_join(reporting.calc, by = c("state")) %>%
  filter(n_weeks >= -10 & weeks <=10) %>%
  filter(n_weeks > -weeks) %>%
  dplyr::select(FIPS, state, date, lag, weeks, n_weeks, covid_mort:add_all_mort, mean_perc, median_perc) %>%
  mutate(across(starts_with("add_"), list(mean = ~ case_when(.x < 0 ~ 0,
                                                             TRUE ~ .x * -mean_perc),
                                          median = ~ case_when(.x < 0 ~ 0,
                                                               TRUE ~ .x * -median_perc))),
         add_rep_mort_covid = case_when(n_weeks == 0 & weeks == 10 ~ covid_mort*1, TRUE ~ 0),
         add_rep_mort_all = case_when(n_weeks == 0 & weeks == 10 ~ all_mort*1, TRUE ~ 0),
         add.all_mort = case_when(n_weeks == 0 ~ add_all_mort,
                                  TRUE ~ 0L),
         add.covid_mort = case_when(n_weeks == 0 ~ add_covid_mort,
                                    TRUE ~ 0L)) %>%
  group_by(FIPS, state, lag) %>%
  summarize(across(starts_with("add_"), sum),
            across(starts_with("add."), sum)) %>%
  dplyr::select(-add_covid_mort, -add_all_mort) %>%
  mutate(across(starts_with("add_covid"), ~ add_rep_mort_covid + .x),
         across(starts_with("add_all"), ~ add_rep_mort_all + .x)) 

check.sums <- mort.2020 %>%
  filter(weeks == 0) %>%
  select(-add_covid_mort, -add_all_mort, -state_ab) %>%
  right_join(lag.adjusted.mort.2020.calc, by = "FIPS")

sum(check.sums$covid_mort != check.sums$add_rep_mort_covid - check.sums$add.covid_mort)
sum(check.sums$all_mort != check.sums$add_rep_mort_all - check.sums$add.all_mort)

##' Question: should we round these values at the end or use ceiling or floor?
lag.adjusted.mort.2020 <-  lag.adjusted.mort.2020.calc %>%
  dplyr::select(FIPS, state, lag, covid_mort = add_covid_mort_median, all_mort = add_all_mort_median)

save(lag.adjusted.mort.2020 , file = "../data/temp/mort.2020.lag.adj.Rdata")


##' ## Mortality by Race

raw.dat.race <- list()

for (week in analysis.weeks){
  raw.dat.race[[paste0("cdc_", week)]] <- read.csv(paste0("../data-raw/cdc-2020-mort-race/Provisional_COVID-19_Death_Counts_by_County_and_Race_", week, ".csv")) %>%
    mutate(dat_week = week) %>%
    dplyr::select(-1, -2, -3)
}

mort.2020.race <- raw.dat.race[[paste0("cdc_", analysis.weeks[1])]]

for (week in analysis.weeks[-1]){
  mort.2020.race <-  rbind(mort.2020.race, raw.dat.race[[paste0("cdc_", week)]])
}


mort.2020.race$check <- mort.2020.race %>%
  dplyr::select(Non.Hispanic.White:Other) %>%
  rowSums(na.rm = T)

mort.2020.race.clean <- mort.2020.race %>%
  filter(Indicator != "Distribution of population (%)") %>%
  dplyr::select(state = State, FIPS = FIPS.Code, Indicator, Total.deaths, COVID.19.Deaths,
         Non.Hispanic.White:Other, dat_week, check) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "COVID-19") ~ "covid",
                               TRUE ~ "all_cause")) %>%
  pivot_wider(names_from = Indicator, values_from = c(Non.Hispanic.White, Non.Hispanic.Black, Non.Hispanic.American.Indian.or.Alaska.Native,
                                                       Non.Hispanic.Asian, Non.Hispanic.Native.Hawaiian.or.Other.Pacific.Islander, Hispanic, Other, check), names_glue = "{Indicator}_{.value}") %>%
  dplyr::select(everything(), check_c = covid_check, check_a = all_cause_check) %>%
  mutate(across(starts_with("covid_"), ~ round(.x * COVID.19.Deaths)),
         across(starts_with("all_cause"), ~ round(.x * Total.deaths)),
         FIPS = sprintf('%05d', FIPS)) %>%
  filter(state != "HI" & state != "AK")

save(mort.2020.race.clean, file = "../data/temp/mort.2020.race.Rdata")
  
mort.2020.race.clean$check1 <- mort.2020.race.clean %>%
  dplyr::select(starts_with("covid_")) %>%
  rowSums(na.rm = T)

mort.2020.race.clean$check2 <- mort.2020.race.clean %>%
  dplyr::select(starts_with("all_cause_")) %>%
  rowSums(na.rm = T)


##' Since the proportions were rounded in the data, they sometimes add up to more than 1 and with rounding,
##' this leads to some counties having more deaths overall (across races) at the end of these calculations

mort.2020.race.clean <- mort.2020.race.clean %>%
  mutate(dif_c = COVID.19.Deaths - check1, dif_a = Total.deaths- check2)

#rmarkdown::render("05-cdc-censored-counties.R", "github_document")



##' Figure ideas:
##' - map with the uncensored counties also showind value of $d$

