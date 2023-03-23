##' ---
##' title: "Matching Balance"
##' output: github_document
##' ---
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

mypacks <- c("survey","dplyr","tidyr", "readr","optmatch", "car", 
             "xtable", "lubridate", "maps", "mapproj", "ggplot2",
             "kableExtra", "arm", "RColorBrewer", "tibble",
             "RItools")  # what packages are needed?
source("../R/helpers_from_development_optmatch.R")
source("00-balance-helpers.R")
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' Load necessary data

load("../data/temp/matches.Rdata")
load("../data/temp/matching.adj.dat.Rdata")
load("../data/temp/pooled.sd1.v2.Rdata")
load("../data/temp/pooled.sd2.v2.Rdata")
load("../data/temp/supermajority.white.xwalk.Rdata")

##' Add match variable to data

mod.dat$match <- ps.penal.calip.stab


##' Set up formula
form1 <- mdcdExp ~ log_10_adult_w_a_cnt +
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

##' ## Balance Overall

myb <- get.balTest(match = "match")
data.frame(myb$overall)

love.plot(myb, "overall", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")

##' ## Balance weighting by black population size


myb.bpop <- get.balTest.bpop(match = "match")
data.frame(myb.bpop$overall)

love.plot(myb.bpop, "black_pop", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")

##' ## Balance for Supermajority White analysis

mod.dat <- mod.dat %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  group_by(match) %>%
  mutate(k = as.numeric(sum(maj_white) >= 1))

myb_smw <- get.balTest(match = "match", dat = mod.dat %>% filter(k == 1))
data.frame(myb_smw$overall)

love.plot(myb_smw, "smw", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")

##' ## Balance by Race

mod.dat <- mod.dat %>%
  mutate(b.95 = as.numeric(black_race >= quantile(mod.dat$black_race, probs = .95)),
         b.9 = as.numeric(black_race >= quantile(mod.dat$black_race, probs = .9)),
         b.75 = as.numeric(black_race >= quantile(mod.dat$black_race, probs = .75))) %>%
  group_by(match) %>%
  mutate(across(starts_with("b."), ~as.numeric(sum(.x) >= 1), .names = "k_{.col}"))

check <- mod.dat %>%
  ungroup() %>%
  dplyr::select(starts_with("k_"), starts_with("b.")) %>%
  summarize(across(everything(), sum))


##' Upper 95th percentile black
myb_b95 <- get.balTest(match = "match", dat = mod.dat %>% filter(k_b.95 == 1))
data.frame(myb_b95$overall)

love.plot(myb_b95, "b95", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")

##' Upper 90th percentile black
myb_b9 <- get.balTest(match = "match", dat = mod.dat %>% filter(k_b.9 == 1))
data.frame(myb_b9$overall)

love.plot(myb_b9, "b90", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")

##' Upper 75th percentile black
myb_b75 <- get.balTest(match = "match", dat = mod.dat %>% filter(k_b.75 == 1))
data.frame(myb_b75$overall)

love.plot(myb_b75, "b75", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")


#rmarkdown::render("30-balance-eval.R", "github_document")

