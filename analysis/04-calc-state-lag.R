##'
##' title: "Calculating State Reporting Lags"
##' output: github_document
##' 
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "car", 
             "xtable", "lubridate", "maps", "mapproj", "ggplot2",
             "kableExtra", "arm", "reldist", "stringr")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' ## State reporting data

load("../data/flu_view_data_reporting_deaths.csv")

reporting <- flu_view_deaths %>%
  mutate_at(vars(contains("death")), ~as.numeric(str_replace(str_replace(.x, "Insufficient Data", "0"), ",", "")))

reporting1 <- reporting  %>%
  filter((week %in% 15:39 & season == "2019-20") | season == "2020-21") %>%
  mutate(week = case_when(season == "2020-21" & week < 10 ~ week + 53,
                          TRUE ~ week + 0)) %>%
  dplyr::select(state, week, season, deaths_58:deaths_47, deaths_39:deaths_31) %>%
  arrange(state, week)

reporting_long1 <- reporting1 %>%
  pivot_longer(cols = contains("death"), names_to = "week_rep", names_prefix = "deaths_",
               values_to = "deaths") %>%
  arrange(state,week, as.numeric(week_rep)) %>%
  group_by(state, week) %>%
  mutate(max_deaths = max(deaths, na.rm = T),
         change_deaths = (deaths - lag(deaths))/lag(deaths)) %>%
  ungroup() %>%
  mutate(perc_max = deaths/max_deaths*100) %>%
  filter(state != "New York City" & !is.na(deaths))

##' ## Calculate Lag
##' I realized that I should drop reporting weeks that don't have many other weeks to go off of or are missing a week
##' just prior in order not to inflate the values. (We are missing week 32 and 57 and also didn't correct data correctly between 40-46).         
lag_calc1 <- reporting_long1 %>%
  ungroup() %>%
  filter(week_rep > 33 & week_rep < 56 & !is.na(change_deaths) & week_rep != 47) %>%
  mutate(level_ind = as.numeric(change_deaths <= .01 & change_deaths >= -.01)) %>%
  arrange(state, week_rep, week) %>%
  group_by(state, week_rep, level_ind) %>%
  summarize(max_week = max(week),
            min_week = min(week)) %>%
  pivot_wider(names_from = level_ind, values_from = c(max_week, min_week),
              names_glue = "{.value}_{level_ind}") %>%
  dplyr::select(state, week_rep, max_week_1, min_week_0)%>%
  ungroup() %>%
  mutate(week_stable = case_when(max_week_1 < min_week_0 ~ as.numeric(max_week_1),
                                 is.na(min_week_0) ~ as.numeric(max_week_1),
                                 min_week_0 > 15 ~ min_week_0 - 1,
                                 min_week_0 == 15 ~ 15),
         lag = as.numeric(week_rep) - week_stable) %>%
  group_by(state) %>%
  mutate(mean_lag = mean(lag),
         median_lag = median(lag))    


state.lag.xwalk <- lag_calc1 %>%
  dplyr::select(stateName = state, mean_lag) %>%
  distinct %>%
  mutate(lag = round(mean_lag)) %>%
  dplyr::select(-mean_lag)

save(state.lag.xwalk, file = "../data/temp/state.lag.05.05.Rdata")

##' Checking how differs from what I previously calculated, they are pretty similar, only differing by a week most of the time if any

state.lag.new <- state.lag.xwalk

load("../data/temp/state.lag.05.05.Rdata")

check <- state.lag.xwalk %>%
  left_join(state.lag.new, by = "stateName")

##' ## Imputation percentage values
##' 
##' The goal is to determine the % of additional deaths that are reported each week that are actually attributed
##' to the weeks prior due to the reporting lag.

reporting21 <- reporting %>%
  mutate(week = case_when(season == "2020-21" & week < 10 ~ week + 53,
                          TRUE ~ week + 0)) %>%
  filter(week %in% 41:58 & season == "2020-21") %>%
  arrange(state, week) %>%
  dplyr::select(state, week,deaths_58:deaths_41)

##' First calculate the number of additional deaths for each actual week that are added each week
##' of reporting.
reporting.long21 <- reporting21 %>%
  pivot_longer(cols = contains("death"), names_to = "week_rep", names_prefix = "deaths_",
               values_to = "deaths") %>%
  arrange(state,week, as.numeric(week_rep)) %>%
  filter(state != "New York City" & !is.na(deaths)) %>%
  arrange(state, week, week_rep) %>%
  group_by(state, week) %>%
  mutate(add_death_week = case_when(week == week_rep ~ deaths,
                                    TRUE ~ deaths - lag(deaths)),
         n_weeks = as.numeric(week) - as.numeric(week_rep))

##' Then calculate the cumulative number of deaths reported (for weeks 41 - the current reporting week)
##' and how many deaths are added each week of reporting, for all actual weeks cumulatively.

cumulative.counts <- reporting.long21 %>%
  group_by(state, week_rep) %>%
  summarize(cum_deaths = sum(deaths)) %>%
  arrange(state, week_rep) %>%
  mutate(add_cum_death = cum_deaths - lag(cum_deaths))

##' Finally, combine these and calculate the % of the additional cumulative deaths weeks that are
##' allocated to each week that go into the cumulative total. Then, calculate the mean, median, and 
##' range of these percentages by the number of weeks that have passed for each state.

reporting.calc <- reporting.long21 %>%
  left_join(cumulative.counts, by = c("state", "week_rep")) %>%
  arrange(state, week_rep, week) %>%
  mutate(perc_add_deaths = case_when(cum_deaths == 0 ~ 0,
                                     TRUE ~ add_death_week / add_cum_death)) %>%
  filter(!is.na(perc_add_deaths)) %>%
  group_by(state, n_weeks) %>%
  summarize(mean_perc = mean(perc_add_deaths),
            median_perc = median(perc_add_deaths),
            min_perc = min(perc_add_deaths),
            max_perc = max(perc_add_deaths),
            sd_perc = sd(perc_add_deaths))

save(reporting.calc, file = "../data/temp/state_lag_impute_perc.Rdata")

check <- reporting.calc %>% 
  group_by(state) %>%
  filter(n_weeks >= -10) %>%
  summarize(mean_perc = sum(mean_perc), med_perc = sum(median_perc))
  