##'--
##' title: "EDA Figures"
##' output: github_document
##' --
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "RColorBrewer",
             "xtable", "lubridate", "maps", "mapproj", "ggplot2", "xlsx"
             )  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

save <- F

##' Load data from 03b-matching-weighted.R, 06-race-dist-county.R, and 09-calc-state-lag.R
load("../data/temp/map.exp.plot.dat.Rdata")
load("../data/temp/supermajority.white.xwalk.Rdata")
load("../data/temp/state.lag.05.05.Rdata")
load("../data/temp/mort.2020.Rdata")
load("../data/temp/mort.2020.race.Rdata")
load("../data/mod.dat.Rdata")


map.dat <- plot.dat %>%
  left_join(state.lag.xwalk, by = "stateName") %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  dplyr::select(-matches.nostab, -plt.fill2)

states <- map_data("state")

##' ## Supermajority White Counties

##' Some summary statistics
##' 
sum.dat <- mod.dat %>%
  filter(!is.na(matches.final)) %>%
  left_join(maj.white.xwalk, by = "FIPS")

sum(is.na(sum.dat$maj_white))

table(sum.dat$maj_white)
sum.dat %>%
  group_by(maj_white) %>%
  summarize(pop = sum(adult_w_a_cnt)) %>%
  ungroup() %>%
  mutate(total_pop = sum(pop)) %>%
  mutate(prop_pop = pop/total_pop)

sum.dat %>%
  group_by(matches.final) %>%
  mutate(k = sum(maj_white)) %>%
  mutate(maj.wht.analysis = case_when(is.na(matches.final) ~ "",
                               maj_white == 1 ~ "Supermajority White",
                               k >= 1 ~ "In Supermajority White Match",
                               TRUE ~ "Excluded from Analysis")) %>%
  group_by(maj.wht.analysis) %>%
  summarize(pop = sum(adult_w_a_cnt)) %>%
  ungroup() %>%
  mutate(total_pop = sum(pop)) %>%
  mutate(prop_pop = pop/total_pop)
  

maj.white.dat <- map.dat %>%
  ungroup()%>%
  group_by(matches.final) %>%
  mutate(k = sum(maj_white)) %>%
  mutate(plt.fill = case_when(is.na(matches.final) ~ "Trimmed",
                              TRUE ~ plt.fill1),
         maj.wht.analysis = case_when(is.na(matches.final) ~ "",
                                      maj_white == 1 ~ "Supermajority White",
                                      k >= 1 ~ "In Supermajority White Match",
                                  TRUE ~ "Excluded from Analysis")) %>%
  mutate(fill.maj.wht = case_when(maj.wht.analysis == "" ~ plt.fill,
                                  TRUE ~ paste(plt.fill, maj.wht.analysis, sep = " - ")))

maj.white.dat$fill.maj.wht <- factor(maj.white.dat$fill.maj.wht, levels = c("Medicaid Expansion - Supermajority White",
                                                                            "Medicaid Expansion - In Supermajority White Match",
                                                                            "Medicaid Expansion - Excluded from Analysis",
                                                                            "Later Expansion - Supermajority White",
                                                                            "Later Expansion - In Supermajority White Match",
                                                                            "Later Expansion - Excluded from Analysis",
                                                                            "No Expansion - Supermajority White",
                                                                            "No Expansion - In Supermajority White Match",
                                                                            "No Expansion - Excluded from Analysis",
                                                                            "Trimmed"))

fill.cols <- brewer.pal(11, "RdYlBu")
fill.cols <- c(fill.cols[c(11,10,9, 4,5,6,1,2,3)], "grey45")

g <- ggplot() +
  geom_polygon(data = maj.white.dat, aes(x=long, y=lat, group = group, fill = fill.maj.wht), color = 'white', size = 0.1) +
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = fill.cols, name = "", na.value='grey') + 
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
  )+
  guides(fill=guide_legend(nrow=4,byrow=TRUE))
g


png("figures/supermajority_white_analysis_map.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()

##' Balance on Supermajority White counties

smw.dat1 <- sum.dat %>%
  group_by(matches.final) %>%
  mutate(k = sum(maj_white)) %>%
  ungroup() %>%
  mutate(smw.match = as.numeric(k >= 1))

##' using balance test function and forms from 03b-matching-weighted.R

myb.smw <- get.balTest(match = "matches.final", dat = smw.dat1 %>% filter(smw.match == 1))
print(myb.smw)

love.plot(myb.smw, "smw", rownames.xwalk = var.labels.xwalk)

##' ## State Reporting Lag
##' 

g <- ggplot() +
  geom_polygon(data = maj.white.dat, aes(x=long, y=lat, group = group, fill = lag), color = 'grey50', size = 0.1) +
  scale_fill_gradient(low = "#2171b5",
                      high = "#fdae6b", name = "State Reporting Lag",
                      na.value='grey') + 
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "white", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
  )
g


##' ## Censored Counties

##' ### All race combined

plot.dat.cens <- mort.2020 %>%
  filter(weeks == 0) %>%
  dplyr::select(FIPS,covid_mort, all_mort) %>%
  right_join(map.dat, by = "FIPS") %>%
  mutate(plot.fill.cenc.all = case_when(is.na(matches.final) ~ "Trimmed",
                                        is.na(all_mort) ~ "Censored",
                                        TRUE ~ plt.fill1),
         excl.state = case_when(lag > 10 ~ "Excluded",
                                TRUE ~ "Interpolated"))



g <- ggplot() +
  geom_polygon(data = plot.dat.cens, aes(x=long, y=lat, group = group, fill = plot.fill.cenc.all), color = 'white', size = 0.1) +
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = c("grey80", "#fdae61", "#3288bd", "#fc8d59", "grey45"), name = "",
                    na.value='grey') + 
  geom_polygon(data = plot.dat.cens, aes(x=long, y=lat, group = group, alpha = excl.state), color = 'white', size = 0.1) +
  scale_alpha_discrete(name = "", range = c(.8, 0)) + 
  xlab("") + ylab("") + ggtitle("2020 All Cause and COVID-19 Mortality Censored Counties") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
  )+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
g

png("figures/2020_censored_all_map.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()

##' ### By Race
  
plot.dat.cens.race <- mort.2020.race.clean %>%
  filter(dat_week == "21.01.02") %>%
  right_join(plot.dat.cens, by = "FIPS") %>%
  mutate(across(starts_with("all_cause"), .names = "fill_{col}",
                ~case_when(is.na(matches.final) ~ "Trimmed",
                           is.na(.x) ~ "Censored",
                           TRUE ~ plt.fill1)),
         across(starts_with("covid"), .names = "fill_{col}",
                ~case_when(is.na(matches.final) ~ "Trimmed",
                           is.na(.x) ~ "Censored",
                           TRUE ~ plt.fill1)))


mort.names <- plot.dat.cens.race %>%
  dplyr::select(starts_with("all_cause"), starts_with("covid_")) %>%
  colnames()

mort.names <- mort.names[-15]

for(mort in mort.names){

  nice.name <- str_to_title(str_replace_all(mort, "\\W|_", " "))

  g <- ggplot() +
    geom_polygon(data = plot.dat.cens.race, aes(x=long, y=lat, group = group, fill = eval(parse(text = paste0("fill_", mort)))), color = 'white', size = 0.1) +
    geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
    coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
    scale_fill_manual(values = c("grey80", "#fdae61", "#3288bd", "#fc8d59", "grey45"), name = "",
                      na.value='grey') +
    geom_polygon(data = plot.dat.cens, aes(x=long, y=lat, group = group, alpha = excl.state), color = 'white', size = 0.1) +
    scale_alpha_discrete(name = "", range = c(.8, 0)) +
    xlab("") + ylab("") + ggtitle(paste0("2020 Censored Counties - ", nice.name)) +
    theme(plot.title = element_text(hjust = 0.5),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
    )+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))


  png(paste0("figures/2020_censored_", mort,".png"), width = 10, height = 6, units = 'in', res = 300)
  print(g)
  dev.off()

}

##' ## Mortality trends 2011 - 2014
##' 

path <- file.path("..","..","..", "compute1","data")

load(file.path(path,"all_det_mort.Rdata"))
load(file.path(path, "pop.Rdata"))
load(file.path(path, "pop.2011.12.Rdata"))

cols <- c(brewer.pal(8, "Set2")[1:4])

treat.xwalk <- mod.dat %>%
  dplyr::select(FIPS, mdcdExp, matches.final) %>%
  group_by(matches.final, mdcdExp) %>%
  mutate(n = n()) %>%
  group_by(matches.final) %>%
  mutate(n_treat = sum(mdcdExp)) %>%
  ungroup() %>%
  mutate(w = 1/n * n_treat)

sum(treat.xwalk$w)

pop_full <- rbind(pop_clean, pop_clean_2011_2012) %>%
  mutate(FIPS = paste0(stateFIPS, cntyFIPS)) %>%
  filter(!(stateFIPS %in% c("02", "15")))%>%
  ungroup() %>%
  dplyr::select(-stateFIPS, -cntyFIPS)

mort_det_long <- mort_det %>%
  dplyr::select(-race) %>%
  rename(race = race_bridged) %>%
  mutate(race = case_when(race == "Non-Hispanic Asian" ~ "Non-Hispanic Asian or Pacific Islander",
                          TRUE~ race)) %>%
  group_by(FIPS, stateFIPS, cntyFIPS, race, age) %>%
  summarize_all(sum) %>%
  pivot_longer(starts_with("mort"), names_to = c("cause", "year"),
               values_to = "mort",
               names_pattern = "mort_(.*)_(.*)") %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(pop_full, by = c("FIPS", "year", "race", "age")) %>%
  left_join(treat.xwalk, by = c("FIPS")) %>%
  filter(!is.na(matches.final))

mort_det_long$maj_white <- factor(mort_det_long$maj_white, levels = c(1,0),
                                  labels = c("Supermajority White", "Not"))


mort.raw <- c("all_cause", "HC_amenable_not_flu", "Flu", "Opioid")
mort.labels <- c("All Cause", "Healthcare Amenable", "Flu", "Opioid")

mort_det_long <- mort_det_long %>%
  filter(cause %in% mort.raw)

mort_det_long$cause <- factor(mort_det_long$cause, levels = mort.raw,
                             labels = mort.labels)


mort_year <- mort_det_long %>%
  filter(!is.na(maj_white)) %>%
  group_by(year, cause, maj_white) %>%
  summarize(mort = sum(mort), pop= sum(pop)) %>%
  filter(cause %in% mort.labels) %>%
  mutate(mort_rate = mort/pop)

mort_age_year <- mort_det_long %>%
  filter(!is.na(maj_white)) %>%
  group_by(age, year, cause, maj_white) %>%
  summarize(mort = sum(mort), pop= sum(pop)) %>%
  filter(cause %in% mort.labels) %>%
  mutate(mort_rate = mort/pop)

mort_race_year <- mort_det_long %>%
  filter(!is.na(maj_white)) %>%
  group_by(race, year, cause, maj_white) %>%
  summarize(mort = sum(mort), pop= sum(pop)) %>%
  filter(cause %in% mort.labels) %>%
  mutate(mort_rate = mort/pop)


ggplot(data = mort_year %>% filter(cause %in% mort.labels[1:2]), 
       aes(x = year, y = mort_rate, linetype = maj_white)) +
  geom_line() +
  facet_wrap(~cause)

g <- ggplot(data = mort_age_year %>% filter(cause %in% mort.labels[1:2]), 
       aes(x = year, y = mort_rate, color = age, linetype = maj_white)) +
  geom_line() +
  facet_wrap(~cause) +
  scale_color_manual(values = cols, name = "Age Group") +
  scale_linetype_discrete(name = "Supermajority White?") +
  xlab("Year")+ ylab("Mortality Rate") +
  ggtitle("Figure 2") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
  
png("figures/mort_rate_age_year.png", width = 8, height = 6, units = 'in', res = 300)
g
dev.off()

ggplot(data = mort_race_year %>% filter(cause %in% mort.labels[1:2]), 
       aes(x = year, y = mort_rate, color = race, linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause)


pop_year <- mort_age_year %>%
  filter(cause == "All Cause") %>%
  group_by(maj_white, year) %>%
  mutate(total_pop = sum(pop)) %>%
  mutate(prop = pop/total_pop)

g <- ggplot(data = pop_year ,
       aes(x = year, y = prop, linetype = maj_white, color = age)) +
  geom_line() +
  scale_color_manual(values = cols, name = "Age Group") +
  scale_linetype_discrete(name = "Supermajority White?") +
  xlab("Year")+ ylab("Proportion of Adult Population") +
  ggtitle("Figure 4") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

png("figures/pop_age_year.png", width = 8, height = 6, units = 'in', res = 300)
g
dev.off()

##' Look at treands for different treatment groups
mort_treat_year <- mort_det_long %>%
  ungroup() %>%
  mutate(mort = mort*w, pop = pop*w) %>%
  group_by(year, cause, mdcdExp, maj_white) %>%
  summarize(mort = sum(mort), pop = sum(pop)) %>%
  mutate(mort_rate = mort/pop)

ggplot(data = mort_treat_year %>% filter(cause %in% mort.labels[1:2]), 
       aes(x = year, y = mort_rate, color = factor(mdcdExp), linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause)

ggplot(data = mort_treat_year %>% filter(cause %in% mort.labels[3]), 
       aes(x = year, y = mort_rate, color = factor(mdcdExp), linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause)

ggplot(data = mort_treat_year %>% filter(cause %in% mort.labels[4]), 
       aes(x = year, y = mort_rate, color = factor(mdcdExp), linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause)

##' And by age
mort_treat_year_age <- mort_det_long %>%
  ungroup() %>%
  mutate(mort = mort*w, pop = pop*w) %>%
  group_by(year, age, cause, mdcdExp, maj_white) %>%
  summarize(mort = sum(mort), pop = sum(pop)) %>%
  mutate(mort_rate = mort/pop)

g <- ggplot(data = mort_treat_year_age %>% filter(cause %in% mort.labels[1:2]), 
       aes(x = year, y = mort_rate, color = factor(mdcdExp), linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause+age, nrow = 2) +
  scale_color_manual(values = c("#fdae61", "#2166ac"), name = "Treatment Status", labels = c("Control", "Treatment")) +
  scale_linetype_discrete(name = "Supermajority White?") +
  xlab("Year")+ ylab("Mortality Rate") +
  ggtitle("Figure 3a") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )



png("figures/mort_treat_age_smw_all_hca.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()

ggplot(data = mort_treat_year_age %>% filter(cause %in% mort.labels[3]), 
       aes(x = year, y = mort_rate, color = factor(mdcdExp), linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause+age, nrow = 1) +
  +
  scale_color_manual(values = c("#fdae61", "#2166ac"), name = "Treatment Status", labels = c("Control", "Treatment")) +
  scale_linetype_discrete(name = "Supermajority White?") +
  xlab("Year")+ ylab("Mortality Rate") +
  ggtitle("Figure 3a") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


g <- ggplot(data = mort_treat_year_age %>% filter(cause %in% mort.labels[4]), 
       aes(x = year, y = mort_rate, color = factor(mdcdExp), linetype = factor(maj_white))) +
  geom_line() +
  facet_wrap(~cause+age, nrow = 1) +
  scale_color_manual(values = c("#fdae61", "#2166ac"), name = "Treatment Status", labels = c("Control", "Treatment")) +
  scale_linetype_discrete(name = "Supermajority White?") +
  xlab("Year")+ ylab("Mortality Rate") +
  ggtitle("Figure 3b") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

png("figures/mort_treat_age_smw_opioid.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()

##' ## Healthcare unamenable deaths

load(file.path("..","..","..", "compute1","data","unamnbl_c_o_d.Rdata"))


unabl_sum <- mort_unabl_det %>%
  filter(year != 2014) %>%
  mutate(cod = case_when(c_o_d_cat == "Opioid" ~ "Opioid",
                         c_o_d_cat == "Other_overdose" ~ "Other_overdose",
                         TRUE ~ icd10)) %>%
  group_by(age, cod) %>%
  summarize(mort = sum(mort))

top_death_by_age <- unabl_sum %>%
  group_by(age) %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n = 5)  %>%
  data.frame()

top_death_overall <- unabl_sum %>%
  group_by(cod) %>%
  summarize(mort = sum(mort)) %>%
  ungroup() %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n =20)

if(save){write.xlsx2(top_death_overall, file = "../data/temp/top_unamenable_deaths.xlsx",
            sheetName = "overall", append = T)}

if(save){write.xlsx2(top_death_by_age, file = "../data/temp/top_unamenable_deaths.xlsx",
            sheetName = "by_age", append = T)}

unabl_sum_wht_treat <- mort_unabl_det %>%
  filter(year != 2014) %>%
  mutate(cod = case_when(c_o_d_cat == "Opioid" ~ "Opioid",
                         c_o_d_cat == "Other_overdose" ~ "Other_overdose",
                         TRUE ~ icd10))  %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  left_join(treat.xwalk, by = c("FIPS")) %>%
  group_by(age, cod, maj_white, mdcdExp) %>%
  filter(!is.na(maj_white) & !is.na(mdcdExp)) %>%
  summarize(mort = sum(mort))
  
top_death_age_wht_treat <- unabl_sum_wht_treat %>%
  group_by(age, maj_white, mdcdExp) %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n = 5) %>%
  data.frame()

top_death_wht_treat <- unabl_sum_wht_treat %>%
  group_by(maj_white,cod, mdcdExp) %>%
  summarize(mort = sum(mort)) %>%
  group_by(maj_white, mdcdExp) %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n = 5) %>%
  data.frame()

if(save){write.xlsx2(top_death_age_wht_treat, file = "../data/temp/top_unamenable_deaths.xlsx",
            sheetName = "age_wht_treat", append = T)}

if(save){write.xlsx2(top_death_wht_treat, file = "../data/temp/top_unamenable_deaths.xlsx",
            sheetName = "wht_treat", append = T)}

##' ## Healthcare amenable deaths
##' 
load(file.path("..","..","..", "compute1","data","amenable_c_o_d.Rdata"))

ambl_sum <- mort_ambl_det %>%
  filter(year != 2014) %>%
  group_by(age, c_o_d_det) %>%
  summarize(mort = sum(mort))

top_death_by_age <- ambl_sum %>%
  group_by(age) %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n = 5)  %>%
  data.frame()

top_death_overall <- ambl_sum %>%
  group_by(c_o_d_det) %>%
  summarize(mort = sum(mort)) %>%
  ungroup()%>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n =20)

if(save){write.xlsx2(top_death_overall, file = "../data/temp/top_amenable_deaths.xlsx",
                     sheetName = "overall", append = T)}

if(save){write.xlsx2(top_death_by_age, file = "../data/temp/top_amenable_deaths.xlsx",
                     sheetName = "by_age", append = T)}

ambl_sum_wht_treat <- mort_ambl_det %>%
  filter(year != 2014) %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  left_join(treat.xwalk, by = c("FIPS")) %>%
  group_by(age, c_o_d_det, maj_white, mdcdExp) %>%
  filter(!is.na(maj_white) & !is.na(mdcdExp)) %>%
  summarize(mort = sum(mort))

top_death_age_wht_treat <- ambl_sum_wht_treat %>%
  group_by(age, maj_white, mdcdExp) %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n = 5) %>%
  data.frame()


top_death_wht_treat <- ambl_sum_wht_treat %>%
  group_by(maj_white,c_o_d_det, mdcdExp) %>%
  summarize(mort = sum(mort)) %>%
  group_by(maj_white, mdcdExp) %>%
  mutate(tot_mort = sum(mort)) %>%
  mutate(perc_mort = mort / tot_mort) %>%
  slice_max(mort, n = 5) %>%
  data.frame()

if(save){write.xlsx2(top_death_age_wht_treat, file = "../data/temp/top_amenable_deaths.xlsx",
                     sheetName = "age_wht_treat", append = T)}

if(save){write.xlsx2(top_death_wht_treat, file = "../data/temp/top_amenable_deaths.xlsx",
                     sheetName = "wht_treat", append = T)}

#rmarkdown::render("20-eda-figures.R", "github_document")


