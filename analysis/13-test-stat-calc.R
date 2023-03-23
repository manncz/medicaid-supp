##' ---
##' title: "Test Statistic Calculation for All Analyses"
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
load("../data/temp/m.by.race.2014.Rdata")

##' Form that includes all possible variables for outcome model
load("../data/temp/form.Rdata")

##' Supermajority white xwalk
load("../data/temp/supermajority.white.xwalk.Rdata")

##' Reporting lag xwalk
load("../data/temp/state.lag.Rdata")

##' First, subset the data to the desired observations and rename variables to fit function structure
dat.2014 <- mod_2014 %>%
  filter(trimmed == 0) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2013,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2012,
         mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012) %>%
  left_join(state.lag.xwalk, by = "stateName") %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  rename(treat = mdcdExp, pop = pop_2014, matches = matches.final, s = maj_white)
  

##' Next, we create a list of the names of the outcome variables of interest, with names that match the
##' list of variables to be included in the models.
names(var_list)
outcome_vars <- list(all_cause = "mort_all_cause_2014",
                     hca = "mort_HC_amenable_not_flu_2014",
                     flu = "mort_Flu_2014",
                     opioid = "mort_opioid_involved_2014")

##' We also want a list with the m datasets for each race analysis, with elements labeled by the race

m.dat.list <- list(all = overall_m)

for(r in levels(factor(m_by_race$race))){
  m.dat.list[[r]] <- m_by_race %>%
    filter(race == r) %>%
    ungroup() %>%
    dplyr::select(-race)
}

##' Now we could efficiently apply this function to all outcome analyses
analyses <- names(var_list)
race_subgroup_analyses <- levels(factor(dat.2014$race))

#+ results = "hide", warnings = FALSE
tstat.list <- lapply(analyses, function(x){test_stat_wrapper(dat = dat.2014, mort_name = outcome_vars[[x]], var_names = var_list[[x]],
                                           delta = 1, m.dat.list = m.dat.list, form = form,
                                           race_analyses = race_subgroup_analyses, subgroup = T, overall = T)})
names(tstat.list) <- analyses

##' Here are all of the p-values for the null hypothesis delta = 1

for(analysis in analyses){
  for(group in c("overall", race_subgroup_analyses, "subgroup")){
    
    print(paste(group, ", ", analysis, ": ", tstat.list[[analysis]][[group]]$pval))
    
  }
}

##' ## Confidence intervals

gen_conf_int_dat <- function(delta.min = 0, delta.max = 2, delta.by = .01, data = dat.2014, analys = analyses,
                             race_subgroup_analys = race_subgroup_analyses, sub = T, overl = T, m.dat.list. = m.dat.list,
                             forml = form, age_analys = NULL){
  
  delta.vec <- seq(delta.min, delta.max, by = delta.by)
  n.analysis <- length(delta.vec)*length(analys)*(length(race_subgroup_analys)+2)
  int.dat <- data.frame(delta = rep(NA, n.analysis),
                        analysis = rep(NA, n.analysis),
                        subgroup = rep(NA, n.analysis),
                        pval.noadj = rep(NA, n.analysis),
                        pval.maxtadj = rep(NA, n.analysis),
                        W = rep(NA, n.analysis),
                        EW = rep(NA, n.analysis),
                        VW = rep(NA, n.analysis))
  
  i = 1
  
  for(delt in delta.vec){
  
    tstat.list <- lapply(analys, function(x){test_stat_wrapper(dat = data, mort_name = outcome_vars[[x]], var_names = var_list[[x]],
                                                                 delta = delt, m.dat.list = m.dat.list., form = forml,
                                                                 race_analyses = race_subgroup_analys, subgroup = sub, overall = overl,
                                                               age_analysis = age_analys
                                                                 )})
    names(tstat.list) <- analys
  
    for(analysis in analys){
      
      print(paste("The current analysis is:", analysis, delt))
      
      temp.vec <- tstat.list[[analysis]]$adjusted_p[[2]]
      
      groups <- race_subgroup_analys
      
      if(sub){
        groups <- c("subgroup", groups)
      }
      
      if(overl){
        groups <- c("overall", groups)
      }
      
      for(group in groups){
  
       int.dat$delta[i] <- delt
       int.dat$analysis[i] <- analysis
       int.dat$subgroup[i] <- group
       int.dat$pval.noadj[i] <-  tstat.list[[analysis]][[group]]$pval
      
       if(group != "subgroup"){
         int.dat$pval.maxtadj[i] <-  temp.vec[which(names(temp.vec) == group)]
       }
       
       int.dat$W[i] <-  tstat.list[[analysis]][[group]]$W
       int.dat$EW[i] <- tstat.list[[analysis]][[group]]$cumulants[1]
       int.dat$VW[i] <- tstat.list[[analysis]][[group]]$cumulants[2]
       
       i = i + 1
      }
    }


  }

return(int.dat)

}

##' ## Main Confidence Interval Calculation
##' Calculate confidence interval data for 2014 not considering the 2020 data lags
dat.2014$lag <- 0

dat.2014.save <- dat.2014

##' interval dat from .2 to 2 by intervals of .02
start.time <- Sys.time()
int.dat.2 <- gen_conf_int_dat(delta.min = .2, delta.max = 2, delta.by = .02)
end.time <- Sys.time()

end.time - start.time

##' interval dat from .2 to 2 by intervals of .02
start.time <- Sys.time()
int.dat.2 <- gen_conf_int_dat(delta.min = .2, delta.max = 2, delta.by = .02)
end.time <- Sys.time()

end.time - start.time

##' interval dat from 2.25 to 9.5 by intervals of .25
start.time <- Sys.time()
int.dat.2.9 <- gen_conf_int_dat(delta.min = 2.25, delta.max = 9.5, delta.by = .25)
end.time <- Sys.time()

end.time - start.time

##' interval dat from 10 to 15 by intervals of .5 for groups which do not already have a bound
start.time <- Sys.time()
int.dat.10.15 <- gen_conf_int_dat(delta.min = 11, delta.max = 15, delta.by = .5, analys = analyses[-3], sub = F)
end.time <- Sys.time()

end.time - start.time

##' combine
int.dat.0.15 <- int.dat.2 %>%
  rbind(int.dat.2.9) %>%
  rbind(int.dat.10.15)

save(int.dat.0.15, file = "../data/temp/interval.dat.0.15_05.24.Rdata")

##' ### Visualize Intervals
load("../data/temp/interval.dat.0.15_05.24.Rdata")

conf.ints <- int.dat.0.15 %>%
  filter(pval.noadj > .05) %>%
  group_by(analysis, subgroup) %>%
  dplyr::select(delta, analysis, subgroup, pval.noadj) %>%
  mutate(min_delta = min(delta),
         max_delta = max(delta)) %>%
  filter(delta == min_delta | delta == max_delta) %>%
  arrange(analysis, subgroup, delta) %>%
  mutate(min_max = case_when(delta == min_delta ~ "min_pval",
                             TRUE ~ "max_pval")) %>%
  dplyr::select(-delta)%>%
  pivot_wider(values_from = pval.noadj, names_from = min_max)

conf.ints %>%
  knitr::kable()

##' Visualizing the confidence interval and estimates

#+ fig.width=22, fig.height=10
ggplot(dat = int.dat.0.15, aes(x = delta))+
  geom_vline(xintercept = 1, color = "grey") +
  geom_line(aes(y = pval.noadj)) + 
  geom_line(aes(y = pval.maxtadj), color = "#a1d99b") +
  geom_abline(intercept = .05, slope = 0, color = "blue", linetype = "dotted") +
  facet_grid(subgroup ~ analysis)


##' ### Hodge's Lehman Estimates

##' Automate calculating the Hodge's Lehman estimate for all analyses:
##' first we want to get good upper and lower bounds for uniroot to use that we expect to be close to the estimate
##' from the grid interval data.

bound.dat <- int.dat.0.15 %>%
  group_by(analysis, subgroup) %>%
  mutate(maxpval = max(pval.noadj)) %>%
  filter(maxpval == pval.noadj) %>%
  select(analysis, subgroup, delta) %>%
  #filter(analysis == "flu" & subgroup == "Non-Hispanic AI or AN") %>% ## this still won't run
  arrange(analysis, subgroup)

bound.dat$est <- rep(NA, nrow(bound.dat))

start.time <- Sys.time()
for (i in 1:nrow(bound.dat)){
  
  print(paste("Current analysis:", bound.dat$analysis[i], bound.dat$subgroup[i]))
  
  out <- tryCatch(uniroot(myest_fun, interval = c(bound.dat$delta[i] - .05, bound.dat$delta[i] + .05),
                          dat = dat.2014, m.dat.list = m.dat.list, form = form,
                          mort_name = outcome_vars[[bound.dat$analysis[i]]],
                          var_names = var_list[[bound.dat$analysis[i]]],
                          analysis = bound.dat$subgroup[i]), finally = print("Error: bounds didn't work"))
  
  bound.dat$est[i] <- out$root
  
}
end.time <- Sys.time()

end.time - start.time

save(bound.dat, file = "../data/temp/2014.treat.est.Rdata")

#rmarkdown::render("13-test-stat-calc.R", "github_document")