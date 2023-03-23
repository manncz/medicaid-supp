##' ---
##' title: "Result Tables - Censored Data"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(readr)
library(stringr)
library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)
source("12-partial-order-helpers.R")

knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

load("../data/temp/all_wonder_res.Rdata")

load("../data/temp/wonder.int.dat")
load("../data/temp/wonder.int.dat.wl")
load("../data/temp/wonder.int.dat.white.Rdata")
load("../data/temp/wonder.int.dat.hca.Rdata")
load("../data/temp/wonder.int.dat.hca.white.Rdata")

int.dat.wonder.hca$analysis <- "hca"
int.dat.wonder$lag = 0

int.dat.wonder.hca.white <- int.dat.wonder.hca.white %>%
  mutate(analysis = "hca_white") %>%
  filter(subgroup == "overall")

int.dat <- int.dat.wonder.white %>%
  filter(subgroup == "overall") %>%
  mutate(analysis = "white") %>%
  rbind(int.dat.wonder.lags) %>%
  rbind(int.dat.wonder.hca) %>%
  rbind(int.dat.wonder.hca.white) %>%
  mutate(lag = 1) %>%
  rbind(int.dat.wonder)

conf.ints <- int.dat %>%
  filter(pval > .05) %>%
  group_by(analysis, subgroup, lag) %>%
  dplyr::select(delta, analysis, subgroup, pval) %>%
  mutate(min_delta = min(delta),
         max_delta = max(delta)) %>%
  filter(delta == min_delta | delta == max_delta) %>%
  arrange(analysis, subgroup, delta) %>%
  mutate(min_max = case_when(delta == min_delta ~ "min_pval",
                             TRUE ~ "max_pval")) %>%
  dplyr::select(-delta)%>%
  pivot_wider(values_from = pval, names_from = min_max) 

bound.dat <- int.dat %>%
  group_by(analysis, subgroup, lag) %>%
  mutate(maxpval = max(pval)) %>%
  filter(maxpval == pval) %>%
  left_join(conf.ints, by = c("analysis", "subgroup", "lag")) %>%
  mutate(est = NA, lb = NA, ub = NA) %>%
  mutate(analysis2 = paste0(analysis, lag))

dat.list <- list()
dat.list[["all_cause1"]] <- dat.2014.wonder
dat.list[["hca1"]] <- dat.2014.wonder
dat.list[["white1"]] <- dat.2014.wonder %>%
  select(-pop)  %>%
  rename(pop = pop.white)
dat.list[["hca_white1"]] <- dat.2014.wonder %>%
  select(-pop)  %>%
  rename(pop = pop.white)
dat.list[["all_cause0"]] <- dat.2014.wonder %>%
  select(-mort_all_cause_l1, -mort_all_cause_l2) %>%
  rename(mort_all_cause_l1 = mort_all_cause_l1o,
         mort_all_cause_l2 = mort_all_cause_l2o)

names(m.dat.list) <- "all_cause"
m.dat.list[["white"]] <- m.dat.list.white$all
m.dat.list[["hca_white"]] <- m.dat.list.white$all
m.dat.list[["hca"]] <- m.dat.list[["all_cause"]]

out_names <- list()
out_names[["white1"]] <- "mort_wht_wndr"
out_names[["hca_white1"]] <- "mort_hca_wht_wndr"
out_names[["hca1"]] <- "mort_hca_wndr"
out_names[["all_cause0"]] <- "mort_dmf"
out_names[["all_cause1"]] <- "mort_wndr"

start.time <- Sys.time()

for (i in 1:nrow(bound.dat)){
  
  print(paste("Current analysis:", bound.dat$analysis[i], bound.dat$subgroup[i], bound.dat$lag[i]))
  
  out <- uniroot(myest_fun, interval = c(bound.dat$delta[i] - .05, bound.dat$delta[i] + .05),
                 dat = dat.list[[bound.dat$analysis2[i]]], 
                 m.dat.list = list(all = m.dat.list[[bound.dat$analysis[i]]]), 
                 form = form,
                 mort_name = out_names[[paste(bound.dat$analysis2[i])]],
                 var_names = vars.wonder,
                 analysis = bound.dat$subgroup[i])
  
  bound.dat$est[i] <- out$root
  
  out <- uniroot(pval_fun, interval = c(bound.dat$min_delta[i] - .02, bound.dat$min_delta[i] + .02),
                 dat = dat.list[[bound.dat$analysis2[i]]], 
                 m.dat.list = list(all = m.dat.list[[bound.dat$analysis[i]]]),
                 form = form,
                 mort_name = out_names[[paste(bound.dat$analysis2[i])]],
                 var_names = vars.wonder,
                 analysis = bound.dat$subgroup[i],
                 maxt = F, alpha = .05)
  
  bound.dat$lb[i] <- out$root
  
  out <- uniroot(pval_fun, interval = c(bound.dat$max_delta[i] - .02, bound.dat$max_delta[i] + .02),
                 dat = dat.list[[bound.dat$analysis2[i]]], 
                 m.dat.list = list(all = m.dat.list[[bound.dat$analysis[i]]]), 
                 form = form,
                 mort_name = out_names[[paste(bound.dat$analysis2[i])]],
                 var_names = vars.wonder,
                 analysis = bound.dat$subgroup[i],
                 maxt = F, alpha = .05)
  
  bound.dat$ub[i] <- out$root
  
  
}
end.time <- Sys.time()

end.time - start.time

save(bound.dat, file = "../data/temp/bound.dat.wonder.Rdata")
