##'
##' title: "Protocol Figures"
##' output: github_document
##' 
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "optmatch", "RItools", "car", 
             "xtable", "lubridate", "maps", "mapproj", "ggplot2", "PISE",
             "kableExtra", "arm", "reldist")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' Need to run 03-matching-weighted first

##' We will save a table for the protocol of whether a state expanded and when

medicaid_raw <- read.csv("../data-raw/expansion-status-interactive-map_1.2.19.csv")

medicaid <- medicaid_raw %>%
  mutate(dateEff = str_extract(Description, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}"),
         yearEff = year(mdy(dateEff)),
         treat_2020 = case_when(is.na(dateEff) ~ "C [0]",
                           TRUE ~ paste0("T [", 2020-as.numeric(yearEff), "]")),
         treat_2014 = case_when(!is.na(dateEff)&mdy(dateEff)< ymd("2014-07-01") ~ "T",
                                TRUE ~ "C")) %>%
  dplyr::select(State, dateEff, treat_2014, treat_2020)

rownames(medicaid) <- medicaid$State
medicaid <- medicaid %>% dplyr::select(-State)
xtable <- xtable(medicaid, caption = "", table.placement = "ht")
print(xtable, comment = F, size="footnotesize",
      file='../paper/protocol/figures/state_exp.tex')

##' ### Lists of county matches

adj.matches.final %>%
  write.csv("figures/adj_matched_county_lists.csv")

close.matches <- close.matches.pen.stab %>%
  mutate(county = paste0(cntyName, " County, ",stateName)) %>%
  group_by(matches.final) %>%
  mutate(n = row_number(), n_count = n()) %>%
  arrange(n_count) %>%
  dplyr::select(county, n, matches.final) %>%
  pivot_wider(values_from = county, names_from = n, names_glue = "{.value}_{n}") %>%
  mutate_at(vars(contains("county")), ~ifelse(is.na(.x), "", .x)) %>%
  write.csv("figures/close_matched_county_lists.csv")

##' ### State reporting appendix figures

load("../data/flu_view_data_reporting_deaths.csv")

reporting <- flu_view_deaths %>%
  dplyr::select(state, week, deaths_37:deaths_31) %>%
  filter(week <= 37 & week >= 15) %>%
  mutate_at(vars(contains("death")), ~as.numeric(str_replace(str_replace(.x, "Insufficient Data", "0"), ",", "")))

reporting_long <- reporting %>%
  pivot_longer(cols = contains("death"), names_to = "week_rep", names_prefix = "deaths_",
               values_to = "deaths") %>%
  arrange(state,week, as.numeric(week_rep)) %>%
  group_by(state, week) %>%
  mutate(max_deaths = max(deaths, na.rm = T),
         change_deaths = (deaths - lag(deaths))/lag(deaths)) %>%
  ungroup() %>%
  mutate(perc_max = deaths/max_deaths*100) %>%
  filter(state != "New York City" & !is.na(deaths))

states <- unique(reporting_long$state)
funky_states <- c("Colorado", "Connecticut", "Delaware", "District of Columbia", "New Mexico", "North Carolina", 
                 "North Dakota", "South Carolina", "South Dakota","Rhode Island", "Vermont",
                 "West Virginia", "Wyoming")
stand_states <- states[!(states %in% funky_states)]

cols <- brewer.pal(8,"RdYlBu")[c(1,2,3,7,8)]

ggplot(aes(x = week, y = perc_max, group = week_rep, color=week_rep),  
            data = reporting_long %>% filter(week_rep != 37 & state %in% funky_states)) +
  geom_line() + facet_wrap(~state) +
  scale_color_manual(values = cols, name = "Reporting Week") +
  xlab("Week") + ylab("Percent of Maximum Mortality Reported Across All Reporting Weeks")


ggplot(aes(x = week, y = perc_max, group = week_rep, color=week_rep),  
       data = reporting_long %>% filter(week_rep != 37 & state %in% stand_states)) +
  geom_line() + facet_wrap(~state)+
  scale_color_manual(values = cols, name = "Reporting Week") +
  xlab("Week") + ylab("Percent of Maximum Mortality Reported Across All Reporting Weeks")

ex_irreg <- c( "Connecticut","North Carolina", "North Dakota", "West Virginia")
ex_reg <- c("Arizona", "Maine", "Ohio", "Indiana")


g1 <- ggplot(aes(x = week, y = perc_max, group = week_rep, color=week_rep),  
       data = reporting_long %>% filter(week_rep != 37 & state %in% ex_irreg)) +
  geom_line() + facet_wrap(~state) +
  scale_color_manual(values = cols, name = "Reporting Week") +
  xlab("Week") + ylab("Percent of Maximum Mortality Reported")+
  theme(
        legend.position = "bottom"
  )

g2 <- ggplot(aes(x = week, y = perc_max, group = week_rep, color=week_rep),  
       data = reporting_long %>% filter(week_rep != 37 & state %in% ex_reg)) +
  geom_line() + facet_wrap(~state) +
  scale_color_manual(values = cols, name = "Reporting Week") +
  xlab("Week") + ylab("Percent of Maximum Mortality Reported")+
  theme(
    legend.position = "bottom"
  )

g1
g2


png("../paper/protocol/figures/reporting_irr.png", width = 5, height = 4, units = 'in', res = 300)
g1
dev.off()

png("../paper/protocol/figures/reporting_reg.png", width = 5, height = 4, units = 'in', res = 300)
g2
dev.off()

## attempting to calculate lag

lag_calc1 <- reporting_long %>%
  ungroup() %>%
  mutate(level_ind = as.numeric(perc_max >= 99 & perc_max <= 101))%>%
  arrange(state, week_rep, week) %>%
  filter(week_rep != 37) %>%
  group_by(state, week_rep, level_ind) %>%
  summarize(max_week = max(week),
            min_week = min(week)) %>%
  pivot_wider(names_from = level_ind, values_from = c(max_week, min_week),
              names_glue = "{.value}_{level_ind}") %>%
  dplyr::select(state, week_rep, max_week_1, min_week_0) %>%
  ungroup() %>%
  mutate(week_stable = case_when(max_week_1 < min_week_0 ~ as.numeric(max_week_1),
                                 min_week_0 > 15 ~ min_week_0 - 1,
                                 min_week_0 == 15 ~ 15),
         lag = as.numeric(week_rep) - week_stable) %>%
  group_by(state) %>%
  mutate(mean_lag = mean(lag))
         
lag_calc2 <- reporting_long %>%
  ungroup() %>%
  mutate(level_ind = as.numeric(change_deaths <= .01 & change_deaths >= -.01)) %>%
  arrange(state, week_rep, week) %>%
  filter(week_rep != 31) %>%
  group_by(state, week_rep, level_ind) %>%
  summarize(max_week = max(week),
            min_week = min(week)) %>%
  pivot_wider(names_from = level_ind, values_from = c(max_week, min_week),
              names_glue = "{.value}_{level_ind}") %>%
  dplyr::select(state, week_rep, max_week_1, min_week_0) %>%
  ungroup() %>%
  mutate(week_stable = case_when(max_week_1 < min_week_0 ~ as.numeric(max_week_1),
                                 min_week_0 > 15 ~ min_week_0 - 1,
                                 min_week_0 == 15 ~ 15),
         lag = as.numeric(week_rep) - week_stable) %>%
  group_by(state) %>%
  mutate(mean_lag = mean(lag))      


compare <- lag_calc1 %>%
  rename_with(~paste0(.x, "_1"),max_week_1:mean_lag) %>%
  full_join(lag_calc2, by = c("state", "week_rep")) %>%
  arrange(state, week_rep)

state_lag <- compare %>%
  filter(week_rep == 33) %>%
  dplyr::select(state, mean_lag_1, mean_lag) %>%
  mutate(cat1 = case_when(mean_lag_1 <= 4.5 ~ "4 weeks or less",
                          mean_lag_1 > 4.5 & mean_lag_1 <= 6.5 ~ "4-6 weeks",
                          mean_lag_1 > 6.5 & mean_lag_1 <= 8.5  ~ "6-8 weeks",
                          mean_lag_1 > 8.5 & mean_lag_1 <= 10.5  ~ "8-10 weeks",
                          mean_lag_1 > 10.5 & mean_lag_1 <= 12.5  ~ "10-12 weeks",
                          TRUE ~ "more than 12 weeks"),
         cat2 = case_when(mean_lag <= 4.5 ~ "4 weeks or less",
                          mean_lag > 4.5 & mean_lag <= 6.5 ~ "4-6 weeks",
                          mean_lag > 6.5 & mean_lag <= 8.5  ~ "6-8 weeks",
                          mean_lag > 8.5 & mean_lag <= 10.5  ~ "8-10 weeks",
                          mean_lag > 10.5 & mean_lag <= 12.5  ~ "10-12 weeks",
                          TRUE ~ "more than 12 weeks"))
  
summary(state_lag$mean_lag_1)
summary(state_lag$mean_lag)   

table(state_lag$cat1)
table(state_lag$cat2)

##' Creating tables to illustrate calculation:

tab <- reporting_long %>%
  ungroup() %>%
  mutate(level_ind = as.numeric(perc_max >= 99 & perc_max <= 101))%>%
  arrange(state, week_rep, week) %>%
  filter(state %in% c("Connecticut", "Arizona"))%>%
  dplyr::select(state, week_rep, week, deaths, change_deaths, perc_max)

##' Save table for Arizona
tab %>%
  filter(state == "Arizona" & week_rep == 36) %>%
  dplyr::select(-state,-week_rep) %>%
  xtable(caption = "", table.placement = "ht", digits=c(0,0,0,2,1)) %>%
  print(comment = F, size="footnotesize",
      file='../paper/protocol/figures/lag_calc_ariz.tex',
      include.rownames = F)

lag_calc2 %>%
  filter(state == "Arizona") %>%
  ungroup()%>%
  dplyr::select(week_rep, week_stable, lag, mean_lag)%>%
  xtable(caption = "", table.placement = "ht", digits=c(0,0,0,0,2)) %>%
  print(comment = F, size="footnotesize",
        file='../paper/protocol/figures/lag_calc2_ariz.tex',
        include.rownames = F)

##' Save table for Connecticut
tab %>%
  filter(state == "Connecticut" & week_rep == 35) %>%
  dplyr::select(-state,-week_rep) %>%
  xtable(caption = "", table.placement = "ht", digits=c(0,0,0,2,1)) %>%
  print(comment = F, size="footnotesize",
        file='../paper/protocol/figures/lag_calc_conn.tex',
        include.rownames = F)

lag_calc2 %>%
  filter(state == "Connecticut") %>%
  ungroup()%>%
  dplyr::select(week_rep, week_stable, lag, mean_lag)%>%
  xtable(caption = "", table.placement = "ht", digits=c(0,0,0,0,2)) %>%
  print(comment = F, size="footnotesize",
        file='../paper/protocol/figures/lag_calc2_conn.tex',
        include.rownames = F)

##' Boxplots for Appendix of overlap.
##' 

##' Save full dataset with indications of which counties were excluded at which stage.
caliper.ex <- excluded.counties %>%
  dplyr::select(FIPS) %>%
  mutate(excl.calip = 1)

boxplot.dat <- mod.dat.no.exclusions %>%
  left_join(exclusions, by = c("cntyName", "stateName")) %>%
  left_join(caliper.ex, by = "FIPS") %>%
  mutate(exclude = case_when(excl.calip == 1 ~ "caliper",
                             excl ==1 ~ "outlier",
                             TRUE ~ ""))

plot.vars <- c("latino", "a20_34", "mortAC_20_64", "mortAC_20_34",
               "pctRep_2012", "vetPop_2013",  "smk_tot_2012", "alc_2012",
               "obsty_male_2011", "phys_act_female_2011")  

var.names <- c("% Latino",
               "% 20-34",
               "All Mortality",
               "20-34 Mortality",
               "Percent Republican",
               "% Veteran",
               "Smoking",
               "Heavy Drinking",
               "Male Obesity",
               "Female Physical Activity"
)

lets <- LETTERS[1:10]

png(paste0("../paper/protocol/figures/dist_overlap.png"), width = 6.5, height = 9, units = 'in', res = 300)

par(mfrow = c(5,2), mar = c(2,2,2,1))

for(i in 1:length(plot.vars)){
  svyboxplot(as.formula(paste0(plot.vars[i],"~factor(mdcdExp)")), surv.d2,
             main = lets[i], all.outliers = T)
}

dev.off()
