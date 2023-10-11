##' ---
##' title: "Further Investigating Subgroup Effects"
##' output: github_document
##' ---
##' 
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE

library(MASS)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(foreach)
library("methods")
library("SparseM")
library("PDQutils")
source("../R/StratifiedDesign.R")
source("../R/Zty_cumulants.R")
source("12-partial-order-helpers.R")
source("12a-conf-int-fun.R")

#library(devtools)
#install_version("optmatch", version = "0.10.5", repos = "http://cran.us.r-project.org")
library(optmatch)

#install.packages("../lib/source/RItools_0.2.0.9004.tar.gz", type = "source")
library(RItools)

source("00-balance-helpers.R")
#source("../R/helpers_from_development_optmatch.R")

##' ## Data
##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")

load(file.path(path,"out_mod_dat_2014.Rdata"))

##' List of variables to be included in each outcome model 
load("../data/temp/mod.var.list.Rdata")

##' Datasets with calculated m (special population values)
load("../data/temp/overall.m.2014.Rdata")
load("../data/temp/m.by.race.2014.Rdata")

##' Form that includes all possible variables for outcome model
load("../data/temp/all_wonder_res.Rdata")

##' Supermajority white xwalk
load("../data/temp/supermajority.white.xwalk.Rdata")

##' ## Simple regression with original matches
#========================================================================================#
#                                 SIMPLE REGRESSION                                      #
#========================================================================================#

form <- paste("mort_dmf ~ treat+", paste(vars.wonder2,collapse = "+"))

mod <- glm(formula(form), data = dat.2014.wonder %>% filter(s == 1), weights = log_adult_w_a_cnt,
           family = poisson)
summary(mod)

mod <- glm(formula(form), data = dat.2014.wonder, weights = log_adult_w_a_cnt, family = poisson)
summary(mod)

##' ## Redo matching with only supermajority white counties
#========================================================================================#
#                                    REDO MATCHES                                        #
#========================================================================================#

form0  <- treat ~ log_adult_w_a_cnt +
  white_race+black_race+latino + male+
  a20_34+a35_44+a45_54+a55_64+ mortAC_20_64+
  mortAC_20_34 + mortAC_35_44 + mortAC_45_54 + mortAC_55_64 +
  mortACWhite_M+mortACWhite_F+mortACBlack_M+mortACBlack_F+
  mortACother_M+mortACother_F+ 
  mortHC_amenable_not_flu + mortOpioid + mortFlu+
  popDens_2010+pctUrban_2010+vetPop_2013+medIncome_2013+
  pctPovty_2013 + snap_2013 + pctNoIns_2013+unplmtRate_2013+
  avgPM25_2011+smk_tot_2012+alc_2012+
  diabetes_2012 + hyper_male_2009 +
  hyper_female_2009 + obsty_male_2011 +
  obsty_female_2011 + phys_act_male_2011 + 
  phys_act_female_2011 + pctRep_2012 + calc_multi_house
  
##' supermajority white counties only
sub.dat <- dat.2014.wonder %>% filter(s == 1)

##' fit first PSM
surv.d1 <- survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=sub.dat)
psm <- survey::svyglm(form0,
                      design=surv.d1,
                      family=quasibinomial())

pooled_sd <-  standardization_scale(psm$linear.predictor,
                                    psm$y, 
                                    svydesign_=surv.d1)
pooled_sd

##' implement propensity score caliper
dist <- match_on(psm$linear.predictor, z=psm$y, data=mod.dat,
                 caliper=.25*pooled_sd)
summary(dist)

ps.pm.calip.25 <- fullmatch(dist, data = sub.dat)
summary(ps.pm.calip.25)

##' look at excluded counties
excl.cnty.calip <- which(is.na(ps.pm.calip.25))
length(excl.cnty.calip) # no. of counties excluded
with(sub.dat,
     sum(adult_w_a_cnt[excl.cnty.calip])/sum(adult_w_a_cnt)
)

##' refit propensity score model, excluding counties outside of caliper
surv.d2 <- survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=sub.dat[-excl.cnty.calip,])
psm2 <- survey::svyglm(form0,
                       design=surv.d2,
                       family=quasibinomial())
##' handle missing values
tmp  <- rep(NA_real_, nrow(sub.dat))
tmp[-excl.cnty.calip] <- psm2$linear.predictors
pscore2 <- tmp
table(is.na(pscore2))

##' update distances with new propensity score
dist.update <- match_on(treat ~ pscore2, dat = sub.dat) + caliper(dist, .25*pooled_sd) 

summary(as.numeric(dist.update))
summary(dist.update)

##' stability increment
dist.update.stab <- dist.update + .1
summary(as.numeric(dist.update.stab))

##' FINAL MATCH WITH CALIPERS AND STABILITY INCREMENT
ps.calip.stab <- fullmatch(dist.update.stab, data=sub.dat)

##' overall balance test
form1 <- update.formula(form0,  . ~ . - log_adult_w_a_cnt + log_10_adult_w_a_cnt)
myb <- get.balTest(match = "ps.calip.stab", form = form1, dat = sub.dat)
data.frame(myb$overall)

##' add new matches to supermajority white subset data
sub.dat$sub.match <- ps.calip.stab

##' save data
save(sub.dat, file = "../data/temp/smw.match.dat.Rdata")

#========================================================================================#
#                       SIMPLE REGRESSION WITH NEW MATCHES                               #
#========================================================================================#

form2 <- paste("mort_dmf ~ treat+sub.match+", paste(vars.wonder2,collapse = "+"))

mod <- glm(formula(form2), data = sub.dat, weights = log_adult_w_a_cnt,
           family = poisson)
summary(mod)

##' ## Analysis with new matched sets
#========================================================================================#
#                               OUR TEST WITH NEW MATCHES                                #
#========================================================================================#
##' Form that includes all possible variables for outcome model
load("../data/temp/form.Rdata")

##' First, subset the data to the desired observations and rename variables to fit function structure
sdatjoin <- sub.dat %>%
  select(FIPS, sub.match)

smw.dat <- mod_2014 %>%
  left_join(sdatjoin, by = "FIPS") %>%
  filter(!is.na(sub.match)) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2013,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2012,
         mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012) %>%
  rename(matches = sub.match, treat = mdcdExp, pop = pop_2014) %>%
  mutate(s = 1, lag = 0)

##' Test for one delta value

#+ results = "hide", warnings = FALSE
out <- test_stat_wrapper(dat = smw.dat, mort_name = "mort_all_cause_2014", var_names = var_list[["all_cause"]],
                  delta = 1, m.dat.list = m.dat.list, form = form,
                  race_analyses = NULL, subgroup = F, overall = T)


##' Confidence interval grid search

delta.min = .8
delta.max = 1.1
delta.by = .01
deltavec = seq(delta.min, delta.max, by = delta.by)

#+ results = "hide", warnings = FALSE
confint <- foreach(i = deltavec, .combine = rbind) %do%{
  
  it.res <- c()
  
  it.out <- test_stat_wrapper(dat = smw.dat, mort_name = "mort_all_cause_2014", var_names = var_list[["all_cause"]],
                           delta = i, m.dat.list = m.dat.list, form = form,
                           race_analyses = NULL, subgroup = F, overall = T)
  
  it.res["delta"] <- i
  it.res["pval"] <- it.out$overall$pval
  it.res["W"] <- it.out$overall$W
  it.res["EW"] <- it.out$overall$cumulants[1]
  it.res["VW"] <- it.out$overall$cumulants[2]
  
  it.res
}

##' Visualize interval
##' 

#+ fig.width=10, fig.height=6
ggplot(dat = data.frame(confint), aes(x = delta))+
  geom_line(aes(y = pval)) + 
  geom_abline(intercept = .05, slope = 0, color = "blue", linetype = "dotted")

##' Use uniroot to get Hodge's Lehman estimate and CI

grid.est <- confint %>%
  data.frame() %>%
  filter(pval > .05) %>%
  mutate(min_delta = min(delta), max_delta = max(delta)) %>%
  filter(pval == max(pval))
  
#+ results = "hide", warnings = FALSE
hlest <- uniroot(myest_fun, interval = c(grid.est$delta - .05, grid.est$delta+ .05),
        dat = smw.dat, m.dat.list = m.dat.list, form = form,
        mort_name = "mort_all_cause_2014", var_names = var_list[["all_cause"]],
        analysis = "overall")

lbest <- uniroot(pval_fun, interval = c(grid.est$min_delta - .05, grid.est$min_delta+ .05),
                 dat = smw.dat, m.dat.list = m.dat.list, form = form,
                 mort_name = "mort_all_cause_2014", var_names = var_list[["all_cause"]],
                 analysis = "overall", maxt = F, alpha = .05)


ubest <- uniroot(pval_fun, interval = c(grid.est$max_delta - .05, grid.est$max_delta+ .05),
                 dat = smw.dat, m.dat.list = m.dat.list, form = form,
                 mort_name = "mort_all_cause_2014", var_names = var_list[["all_cause"]],
                 analysis = "overall", maxt = F, alpha = .05)

#+ warnings = FALSE
print(paste0("This estimates that the effect on supermajority white counties was ", round(hlest$root,3),
            " with a 95% confidence interval of (", round(lbest$root,3), ",", round(ubest$root,3), ")."))

##' Save result

smw.result <- list(est = hlest$root, lbest = lbest$root, ubest = ubest$root)
save(smw.result, file = "../data/temp/smw.result.Rdata")
