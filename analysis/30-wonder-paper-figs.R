##' ---
##' title: "WONDER paper calcs and figs"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(xtable)
library(ggplot2)
library(glmnet)
library(xlsx)
library(lubridate)

knitr::opts_chunk$set(warning=FALSE, echo=TRUE)


##' ## Table of variables included
##' 

load("../data/temp/glmnetcv.2013all.Rdata")
load("../data/temp/wonder.vars.Rdata")

vars <- data.frame(var = row.names(coef(cv.out.all, s = "lambda.min")), 
                   coef = coef(cv.out.all, s = "lambda.min")[,1]) %>%
  mutate(all_cause = case_when(coef != 0 ~ "X",
                               TRUE ~ "")) %>%
  select(-coef)

varswonder <- data.frame(var = vars.wonder, wonder = "X")

vars_full <- vars  %>%
  filter(var != "(Intercept)") %>%
  mutate(propensity = case_when(str_detect(var, ":") ~ "",
                                TRUE ~ "X")) %>%
  filter(all_cause == "X" | propensity == "X") %>%
  left_join(varswonder, by = "var") %>%
  mutate(wonder = case_when(wonder == "X" ~ wonder,
                            TRUE ~ "")) %>%
  select(var, propensity, all_cause, wonder)

vars_full$propensity[1:11] <- rep("", 11)

vars_noint <- vars_full %>% filter(!str_detect(var,":"))
vars_int <- vars_full %>% filter(str_detect(var,":")) %>%
  separate(var, into = c("age", "var"), sep = ":")

var.name.xwalk <- read.xlsx(file = "figures/var_table_xwalk.xlsx", sheetIndex = 1)


##' Save table for the non-interaction variables
vars_table1 <- vars_noint %>%
  left_join(var.name.xwalk, by = "var") %>%
  select(var_clean, year, propensity, all_cause, wonder)
  
xtable <- xtable(vars_table1, caption = "", table.placement = "ht")
print(xtable, comment = F, size="footnotesize", include.rownames = F, 
      file='figures/var_incl_nointeractions.tex')

##' Save table for the interactions
vars_table2 <- vars_int %>%
  left_join(var.name.xwalk, by = "var") %>%
  select(age, var_clean, year) %>%
  mutate(incl = "X") %>%
  pivot_wider(names_from = age, values_from = incl) %>%
  select(var_clean, year, age35_44, age45_54, age55_64)

xtable <- xtable(vars_table2, caption = "", table.placement = "ht")
print(xtable, comment = F, size="footnotesize", include.rownames = F, 
      file='figures/var_incl_interactions.tex')


##' ## Simpler supermajority white visualization
##' 
library(scales)
load("../data/temp/map.exp.plot.dat.Rdata")
load("../data/temp/supermajority.white.xwalk.Rdata")
states <- map_data("state")

maj.white.dat <- plot.dat %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  ungroup()%>%
  group_by(matches.final) %>%
  mutate(k = sum(maj_white)) %>%
  mutate(plt.fill = case_when(is.na(matches.final) ~ "Trimmed",
                              plt.fill1 == "Medicaid Expansion" ~ plt.fill1,
                              TRUE ~ "No Expansion"),
         maj.wht.analysis = case_when(is.na(matches.final) ~ "",
                                      maj_white == 1 ~ "Supermajority White",
                                      k >= 1 ~ "In Supermajority White Match",
                                      TRUE ~ "Excluded from Analysis")) %>%
  mutate(fill.maj.wht = case_when(maj.wht.analysis == "" ~ plt.fill,
                                  maj.wht.analysis == "Excluded from Analysis" ~ maj.wht.analysis,
                                  TRUE ~ paste(plt.fill, maj.wht.analysis, sep = " - ")))

maj.white.dat$fill.maj.wht <- factor(maj.white.dat$fill.maj.wht, levels = c("Medicaid Expansion - Supermajority White",
                                                                            "No Expansion - Supermajority White",
                                                                            "Excluded from Analysis",
                                                                            "Medicaid Expansion - In Supermajority White Match",
                                                                            "No Expansion - In Supermajority White Match",
                                                                            "Trimmed"),
                                     labels = c("Treatment supermajority White",
                                                "Control supermajority White",
                                                "Not in comparison",
                                                "Treatment comparison county",
                                                "Control comparison county",
                                                "Trimmed"))

states2 <- maj.white.dat %>%
  ungroup() %>%
  select(region, treat = plt.fill) %>%
  filter(treat != "Trimmed") %>%
  distinct() %>%
  right_join(states, by = "region") 
  

fill.cols <- c("#08306b", "#67000d","grey87", "#4292c6",  "#fe9929", "grey93") 
county.line.cols <- c("white","white","white","grey30", "grey30", "white")
state.line.size <- c(1,.4)

g <- ggplot() +
  geom_polygon(data = maj.white.dat, aes(x=long, y=lat, group = group, fill = fill.maj.wht, color = fill.maj.wht),  size = 0.1) +
  scale_fill_manual(values = fill.cols, name = "", na.value='grey') + 
  scale_color_manual(values = county.line.cols, name = "", na.value='grey') + 
  geom_polygon(data = states2, aes(x=long, y=lat, group=group, size = treat), color = "grey50", alpha = 0)+
  scale_size_manual(values = state.line.size, name = "", na.value='grey', guide = "none") + 
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "gray93")
  )+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
g


png("figures/supermajority_white_analysis_map_simple.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()

##' ## EDA for SMW
##' 

#========================================================================================#
#                                 SMW CHARACTERISTICS                                    #
#========================================================================================#

smw_age <- dat.2014.wonder %>% 
  ungroup() %>%
  select(s, pop = adult_w_a_cnt, a20_34:a55_64) %>%
  mutate(across(starts_with("a"), ~.x*pop, .names = "weighted_{.col}")) %>%
  group_by(s) %>%
  summarize(across(starts_with("a"), mean), across(starts_with("weighted"), sum), pop = sum(pop)) %>%
  mutate(across(starts_with("weighted"),~.x/pop))

smw_age

smw_age %>%
  pivot_longer(cols = (starts_with("a")|starts_with("weighted")), names_to = c("type", "age"),
               names_pattern = "(.*)(\\d\\d_\\d\\d)", values_to = "prop") %>%
  group_by(s, type) %>%
  mutate(prop_waa = prop/sum(prop)) %>%
  mutate(weighted = case_when(type == "a"~0,
                              TRUE ~ 1)) %>%
  ungroup() %>%
  select(-type, -pop) %>%
  pivot_wider(names_from = s, values_from = c(prop, prop_waa),
              names_glue = "{.value}_s{s}")

smw_insurance <- dat.2014.wonder %>%
  ungroup() %>%
  select(s, pop = adult_w_a_cnt, pctNoIns_2013) %>%
  mutate(wpct = pctNoIns_2013*pop) %>%
  group_by(s) %>%
  summarize(pct = mean(pctNoIns_2013), wpct = sum(wpct), pop= sum(pop)) %>%
  mutate(wpct = wpct / pop)

smw_insurance

dat.2014.wonder %>%
  mutate(highnoins = as.numeric(pctNoIns_2013 >= 21)) %>%
  group_by(s) %>%
  summarize( mean(highnoins))

tapply(dat.2014.wonder$pctNoIns_2013, dat.2014.wonder$s, summary)

tapply(dat.2014.wonder$pctUrban_2010,  dat.2014.wonder$s, summary)

dat.2014.wonder %>%
  ungroup() %>%
  group_by(s) %>%
  summarize(rep = sum(pctUrban_2010*adult_w_a_cnt), pop = sum(adult_w_a_cnt)) %>%
  mutate(prop = rep/pop)

tapply(dat.2014.wonder$pctRep_2012,  dat.2014.wonder$s, summary)
mean(dat.2014.wonder$pctRep_2012[dat.2014.wonder$s == 1] > 50)

dat.2014.wonder %>%
  ungroup() %>%
  group_by(s) %>%
  summarize(rep = sum(pctRep_2012*adult_w_a_cnt), pop = sum(adult_w_a_cnt)) %>%
  mutate(prop = rep/pop)

##' ## Results
##' 
##' ### detailed mortality

load("../data/temp/ci.plot.dat_05.24")

g1 <- ggplot(ests %>% filter(analysis %in% c("All Cause", "Healthcare Amenable")), aes(x = subgroup, y = est, color = sig)) +
  geom_abline(intercept = 1, slope = 0, color = "#2171b5", linetype = "dotted", size= .3) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = max_delta, ymin = min_delta), width = .2, size = .5) +
  facet_wrap(~ analysis, ncol = 1) +
  coord_flip() +
  scale_color_manual(name = "", values = c("black", "#0868ac")) +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  xlab("") + ylab("Estimate")
g1

png("figures/CI_allcause_hca.png", width = 6, height = 4, units = 'in', res = 300)
g1
dev.off()

g2 <- ggplot(ests %>% filter(analysis %in% c("All Cause") & subgroup %in% c("Supermajority White Counties", "Overall", "Non-Hispanic White")), aes(x = subgroup, y = est, color = sig)) +
  geom_abline(intercept = 1, slope = 0, color = "#2171b5", linetype = "dotted", size= .3) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = max_delta, ymin = min_delta), width = .2, size = .5) +
  facet_wrap(~ analysis, ncol = 1) +
  coord_flip() +
  scale_color_manual(name = "", values = c("black", "#0868ac")) +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  xlab("") + ylab("Estimate")
g2

png("figures/CI_allcause_zoom.png", width = 6, height = 4, units = 'in', res = 300)
g2
dev.off()


##' ### WONDER
##' 

load("../data/temp/bound.dat.wonder.Rdata")

wonder.ci <- bound.dat %>%
  select(analysis, subgroup, lag, lb, ub, est) %>%
  mutate(sig = case_when((ub - 1)*(lb - 1) > 0  ~ "significant",
                         TRUE ~ "")) %>%
  mutate(subgroup = case_when(str_detect(analysis, "white") ~ "white",
                              TRUE ~ subgroup),
         analysis = case_when(analysis == "white" ~ "all_cause",
                              analysis == "hca_white" ~ "hca",
                                     TRUE ~ analysis)) %>%
  mutate(data = "WONDER")

wonder.ci$analysis <- factor(wonder.ci$analysis, levels = c("all_cause", "hca"),
                             labels = c("All Cause", "Healthcare Amenable"))

wonder.ci$subgroup <- factor(wonder.ci$subgroup, levels = c("overall", "subgroup", "white"),
                        labels = c("Overall", "Supermajority White Counties", "Non-Hispanic White"))

plot.dat <- ests %>%
  filter(analysis %in% c("All Cause", "Healthcare Amenable") & subgroup %in% c("Overall", "Supermajority White Counties", "Non-Hispanic White")) %>%
  select(analysis, subgroup, lb = min_delta, ub = max_delta, est, sig) %>%
  mutate(data = "Detailed Mortality") %>%
  rbind(wonder.ci)

plot.dat <- plot.dat %>%
  filter(lag == 1 | is.na(lag))

plot.dat$data = factor(plot.dat$data, levels = c("WONDER", "Detailed Mortality"))

g3 <- ggplot(plot.dat, aes(x = subgroup, y = est, color = sig, linetype = data)) +
  geom_abline(intercept = 1, slope = 0, color = "#2171b5", linetype = "dotted", size= .3) +
  geom_point(size = 1, position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymax = ub, ymin = lb), width = .2, size = .5, position = position_dodge(width = .5)) +
  facet_wrap(~ analysis, ncol = 1) +
  coord_flip() +
  scale_color_manual(name = "", values = c("black", "#0868ac")) +
  scale_linetype(name = "") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
        ) +
  xlab("") + ylab("Estimate")
g3


png("figures/CI_wonder.png", width = 6, height = 4, units = 'in', res = 300)
g3
dev.off()

##' make a table

tab.dat <- ests %>%
  filter(analysis %in% c("All Cause", "Healthcare Amenable") & !(subgroup %in% c("Overall", "Supermajority White Counties", "Non-Hispanic White"))) %>%
  select(analysis, subgroup, lb = min_delta, ub = max_delta, est, sig) %>%
  mutate(data = "Detailed Mortality") %>%
  rbind(plot.dat)

tab.dat$data = factor(tab.dat$data, levels = c("WONDER", "Detailed Mortality"), labels = c("wonder", "dmf"))

tab.dat <- tab.dat %>%
  select(-lag, - sig) %>%
  mutate(ub_clean = case_when(ub == 5 ~ ">5",
                              TRUE ~ as.character(round(ub,2))),
         lb_clean = case_when(lb == .2 ~ "<0.2",
                              TRUE ~ as.character(round(lb,2)))) %>%
  mutate(ci = paste0("(", lb_clean, ",", ub_clean, ")"))%>%
  pivot_wider(names_from = data, values_from = c(est, ci), 
              names_glue = "{.value}_{data}")

tab.dat.clean <- tab.dat %>%
  filter(!is.na(est_dmf)  & analysis == "All Cause") %>%
  select(analysis, subgroup, est_dmf, ci_dmf) %>%
  left_join(tab.dat %>%
              filter(!is.na(est_wonder)) %>%
              select(analysis, subgroup, est_wonder, ci_wonder), by = c("analysis", "subgroup")) %>%
  filter(!is.na(est_wonder)) %>%
  arrange(analysis, subgroup)
 
xtable <- xtable(tab.dat.clean, caption = "", table.placement = "ht",
                 digits = c(1,1,1,2,1,2,1))
print(xtable, comment = F, size="footnotesize", include.rownames = F, 
      file='figures/estimate_results_wonder.tex')

tab.dat.clean2 <- tab.dat %>%
  filter(!is.na(est_dmf)  & analysis == "All Cause") %>%
  select(analysis, subgroup, est_dmf, ci_dmf) 

xtable <- xtable(tab.dat.clean2, caption = "", table.placement = "ht",
                 digits = c(1,1,1,2,1))
print(xtable, comment = F, size="footnotesize", include.rownames = F, 
      file='figures/estimate_results_dmf.tex')


##' ### Table of censored values in WONDER queries

load("../data/temp/all_wonder_res.Rdata")

cens.table <- dat.2014.wonder %>%
  group_by(s) %>%
  summarize(all_cause = sum(is.na(mort_wndr)), 
            hca = sum(is.na(mort_hca_wndr)),
            white = sum(is.na(mort_wht_wndr)),
            hca_white = sum(is.na(mort_hca_wht_wndr)),
            n = n())

cens.table <- cens.table %>%
  pivot_longer(all_cause:hca_white, names_to = "analysis") %>%
  mutate(prop= value/n) %>%
  pivot_wider(values_from = c(n,value,prop), names_from = s,
              names_glue = "{.value}{s}") %>%
  mutate(prop = (value0+value1)/(n0+n1)) 

tab <- cens.table %>%
  select(analysis, prop0, prop1, prop)

xtable <- xtable(tab, caption = "", table.placement = "ht")
print(xtable, comment = F, size="footnotesize", include.rownames = F, 
      file='figures/wonder_censoring.tex')


dat.2014.wonder %>%
  mutate(miss = case_when(is.na(mort_wndr)~1,
                          TRUE ~ 0)) %>%
  group_by(miss) %>%
  summarize(pop = sum(pop, na.rm = T), n_county = n()) %>%
  ungroup() %>%
  mutate(total_pop = sum(pop)) %>%
  mutate(perc = pop/total_pop)

##' only missing .2% of population

dat.2014.wonder %>%
  mutate(miss = case_when(is.na(mort_wndr)~1,
                        TRUE ~ 0)) %>%
  group_by(miss,s) %>%
  summarize(pop = sum(pop, na.rm = T), n_county = n()) %>%
  ungroup() %>%
  group_by(s) %>%
  mutate(total_pop = sum(pop)) %>%
  mutate(perc = pop/total_pop)

##' missing 3% of supermajority white population

dat.2014.wonder %>%
  mutate(miss = case_when(is.na(mort_hca_wndr)~1,
                          TRUE ~ 0)) %>%
  group_by(miss) %>%
  summarize(pop = sum(pop, na.rm = T), n_county = n()) %>%
  ungroup() %>%
  mutate(total_pop = sum(pop)) %>%
  mutate(perc = pop/total_pop)

##' .5% of population for hca


##' ## Map of Censoring
##' 
load("../data/temp/map.exp.plot.dat.Rdata")
cens.plot.dat <- dat.2014.wonder %>%
  mutate(fill = case_when(is.na(mort_wndr) ~ "Censored AC Mortality",
                          is.na(mort_wht_wndr) ~ "Censored White AC Mortality",
                          TRUE ~ "Not Censored"))

plot.dat.wonder <- cens.plot.dat %>%
  right_join(plot.dat, by = "FIPS") %>%
  mutate(fill = case_when(is.na(fill) ~ plt.fill1,
                          TRUE ~ fill))


cols <- c("#08306b","#4292c6",
          #"#4292c6","#6baed6","#c6dbef", 
          "grey85", "grey93")


g <- ggplot() +
  geom_polygon(data = plot.dat.wonder, aes(x=long, y=lat, group = group, fill = fill), color = 'white', size = 0.1) +
  geom_polygon(data = states2, aes(x=long, y=lat, group=group), size =.2, color = "gray50", alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = cols, name = "") +
  xlab("") + ylab("") + 
  #ggtitle("2014 Age Adjusted Mortality Censoring - CDC WONDER") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text= element_text(size = 12),
        legend.position = "bottom",
        panel.background = element_rect(fill = "gray93")
  )+
  guides(fill=guide_legend(nrow = 1,byrow=TRUE))

g

png("figures/wonder_censoring_discrete.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()

##' Table of treatment assignment (updated from protocol)
medicaid_raw <- read.csv("../data-raw/expansion-status-interactive-map_11.9.2022.csv")

medicaid <- medicaid_raw %>%
  mutate(dateEff = str_extract(Description, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}"),
         yearEff = year(mdy(dateEff)),
         treat = case_when(!is.na(dateEff)&mdy(dateEff)< ymd("2014-07-01") ~ "T",
                                TRUE ~ "C")) %>%
  mutate(treat = case_when(State == "Wisconsin" ~ "T",
                           TRUE ~ treat)) %>%
  select(State, dateEff, treat)

xtable <- xtable(medicaid, caption = "", table.placement = "ht")
print(xtable, include.rownames = F, size="footnotesize",
      file='figures/treat-condition.tex')

##' Table of matching structure for appendix


dat.2014.wonder %>%
  select(matches, treat) %>%
  group_by(matches, treat) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = treat, values_from = n,
              names_glue = "{.value}{treat}") %>%
  mutate(n0 = case_when(n0 > 3 ~ "4 or more",
                        TRUE ~ as.character(n0)),
         n1 = case_when(n1 > 3 ~ "4 or more",
                        TRUE ~ as.character(n1))) %>%
  group_by(n1, n0) %>% 
  summarize(nmatches = n()) ->
  matched_structure
 
xtable <- xtable(matched_structure, caption = "", table.placement = "ht")
print(xtable, include.rownames = F, size="footnotesize",
      file='figures/match-structure.tex')
