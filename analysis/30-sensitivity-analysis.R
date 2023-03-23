##' ---
##' title: "Supermajority White Sensitivity Analyses"
##' output: github_document
##' ---
##' 
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
cleanupunderway  <- TRUE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(MASS)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library("methods")
library("SparseM")
library("PDQutils")
source("../R/StratifiedDesign.R")
source("../R/Zty_cumulants.R")
source("12-partial-order-helpers.R")

stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")

load(file.path(path,"out_mod_dat_2014.Rdata"))

##' List of variables to be included in each outcome model 
load("../data/temp/mod.var.list.Rdata")

##' Datasets with calculated m (special population values)
load("../data/temp/overall.m.2014.Rdata")

##' Form that includes all possible variables for outcome model
load("../data/temp/form.Rdata")

##' Supermajority white xwalk
load("../data/temp/supermajority.white.xwalk.Rdata")

##' Load interval dat from main analysis
load("../data/temp/interval.dat.0.15_05.24.Rdata")

##' First, subset the data to the desired observations and rename variables to fit function structure
dat.2014 <- mod_2014 %>%
  filter(trimmed == 0) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2013,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2012,
         mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012) %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  rename(treat = mdcdExp, pop = pop_2014, matches = matches.final, s = maj_white)


##' Next, we create a list of the names of the outcome variables of interest, with names that match the
##' list of variables to be included in the models.
names(var_list)
outcome_vars <- list(all_cause = "mort_all_cause_2014",
                     hca = "mort_HC_amenable_not_flu_2014",
                     flu = "mort_Flu_2014",
                     opioid = "mort_opioid_involved_2014")

m.dat.list <- list(all = overall_m)

##' Now we could efficiently apply this function to all outcome analyses
analyses <- names(var_list)

dat.2014$lag <- 0
dat.2014.save <- dat.2014

##' ## Supermajority White Subgroup Analysis Sensitivity Analysis

##' Calculate confidence interval data for different definitions of SMW

##' First, if cutoff is top 95th weighted quantile
dat.2014 <- dat.2014.save %>%
  left_join(maj.white.xwalk.upd, by = "FIPS") %>%
  dplyr::select(-s) %>%
  rename(s = maj_white.95)

start.time <- Sys.time()
int.dat.smw.95 <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by = .02, 
                                   race_subgroup_analys = NULL, sub = T, overl = F)
end.time <- Sys.time()

end.time - start.time

int.dat.check.smw <- int.dat.smw.95 %>%
  mutate(cutoff = ".95")

##' Top 90th weighted quantile

dat.2014 <- dat.2014 %>%
  rename(maj_white.95 = s) %>%
  rename(s = maj_white.9)

end.time <- Sys.time()
int.dat.smw.9 <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by = .02, 
                                  race_subgroup_analys = NULL, sub = T, overl = F)
start.time <- Sys.time()
end.time - start.time

temp <- int.dat.smw.9 %>%
  mutate(cutoff = ".9")

int.dat.check.smw <- rbind(int.dat.check.smw, temp.dat) 

##' Top 86th weighted quantile

dat.2014 <- dat.2014 %>%
  rename(maj_white.9 = s) %>%
  rename(s = maj_white.86)

start.time <- Sys.time()
int.dat.smw.86 <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by = .02, 
                                   race_subgroup_analys = NULL, sub = T, overl = F)
end.time <- Sys.time()
end.time - start.time

dat.2014 <- dat.2014 %>%
  rename(maj_white.86 = s)

temp <- int.dat.smw.86 %>%
  mutate(cutoff = ".86")

int.dat.check.smw <- rbind(int.dat.check.smw, temp.dat) 

##' Now, just a random selection of 20% of the counties in the data

dat.2014$s = sample(c(0,1), nrow(dat.2014), prob = c(.8, .2), replace = T)

start.time <- Sys.time()
int.dat.smw.rand <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by = .02, 
                                     race_subgroup_analys = NULL, sub = T, overl = F)
end.time <- Sys.time()

end.time - start.time

temp <- int.dat.smw.rand %>%
  mutate(cutoff = "rand")

int.dat.check.smw <- rbind(int.dat.check.smw, temp.dat) 

##' Combine with the SMW interval data for using the .975 cutoff, which is the main result
temp.dat <- int.dat.0.15 %>%
  dplyr::select(delta, analysis, subgroup, pval.noadj) %>%
  filter(subgroup == "subgroup" & delta >= .5 & delta <= 1.5) %>%
  mutate(cutoff = ".975")

int.dat.check.smw <- rbind(int.dat.check.smw, temp.dat) 

int.dat.check.smw <- int.dat.check.smw %>%
  filter(!is.na(delta))

save(int.dat.check.smw, file = "../data/temp/interval.dat.smw.sens.Rdata")

##' SMW analysis by age group

ages <- unique(dat.2014$age)

int.dat.smw.by.age <- list()

for(x in ages){
  
  int.dat.smw.by.age[[x]] <- gen_conf_int_dat(delta.min = .5, delta.max = 1.5, delta.by = .02, 
                                              race_subgroup_analys = NULL, sub = T, overl = F, data = dat.2014.save,
                                              age_analys = x)
  
}


age.smw.dat <- int.dat.smw.by.age[[ages[1]]] %>%
  mutate(age = ages[1])

for(i in 2:4){
  
  age.smw.dat <- rbind(age.smw.dat, int.dat.smw.by.age[[ages[i]]]%>%
                         mutate(age = ages[i])) 
}
age.smw.dat <- age.smw.dat %>%
  filter(!is.na(subgroup))

##' Load the data if already calculated for ease
load("../data/temp/interval.dat.smw.sens.Rdata")

cols <- c("#fa9fb5", "#f768a1", "#dd3497", "#49006a", "#7fcdbb")

g <- ggplot(dat = int.dat.check.smw, aes(x = delta))+
  geom_vline(xintercept = 1, color = "grey") +
  geom_line(aes(y = pval.noadj, color = cutoff)) + 
  geom_abline(intercept = .05, slope = 0, color = "blue", linetype = "dotted") +
  facet_grid(subgroup ~ analysis) +
  scale_color_manual(values = cols)

png(paste0("figures/swm.sens.png"), width = 10, height = 6, units = 'in', res = 300)
print(g)
dev.off()

g <- ggplot(dat = int.dat.check.smw %>% filter(analysis == "all_cause"), aes(x = delta))+
  geom_vline(xintercept = 1, color = "grey") +
  geom_line(aes(y = pval.noadj, color = cutoff)) + 
  geom_abline(intercept = .05, slope = 0, color = "blue", linetype = "dotted")+
  scale_color_manual(values = cols)

png(paste0("figures/swm.sens.all.cause.png"), width = 6, height = 6, units = 'in', res = 300)
print(g)
dev.off()

smw.dat <- int.dat.0.15 %>%
  filter(delta >= .5 & delta <= 1.5) %>%
  filter(subgroup == "subgroup") %>%
  mutate(age = "all")

age.smw.dat <- rbind(age.smw.dat, smw.dat)

cols <- c(brewer.pal(8, "Set2")[1:4], "black")

g <- ggplot(dat = age.smw.dat, aes(x = delta))+
  geom_vline(xintercept = 1, color = "grey") +
  geom_line(aes(y = pval.noadj, color = age)) + 
  geom_abline(intercept = .05, slope = 0, color = "blue", linetype = "dotted")+
  facet_grid(subgroup ~ analysis)+
  scale_color_manual(values = cols, name = "Age Group") +
  xlab("Multiplicative Treatment Effect")+ ylab("P-value") +
  ggtitle("Figure 1") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
g

png("figures/supermajority_white_analysis_age_ci.png", width = 10, height = 6, units = 'in', res = 300)
g
dev.off()
