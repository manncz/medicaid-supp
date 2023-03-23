##'--
##' title: "Exploring Race and Counties"
##' output: github_document
##' --
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "optmatch", "RItools", "car", 
             "xtable", "lubridate", "maps", "mapproj", "ggplot2", "PISE",
             "kableExtra", "arm", "reldist")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' ### Exploring Race and Counties
##' 

load("../data/mod.dat.Rdata")

(white.75 <- quantile(mod.dat$white_race, probs = .975))
(black.75 <- quantile(mod.dat$black_race, probs = .75))

##' weighted quantiles
##' 
(white.quant.975 <- wtd.quantile(mod.dat$white_race, q = .975, weight = mod.dat$adult_w_a_cnt))
(white.quant.95 <- wtd.quantile(mod.dat$white_race, q = .95, weight = mod.dat$adult_w_a_cnt))
(white.quant.9 <- wtd.quantile(mod.dat$white_race, q = .9, weight = mod.dat$adult_w_a_cnt))
(white.quant.86 <- wtd.quantile(mod.dat$white_race, q = .86, weight = mod.dat$adult_w_a_cnt))
(black.quant <- wtd.quantile(mod.dat$black_race, q = .99, weight = mod.dat$adult_w_a_cnt))

exp.race <- mod.dat %>%
  mutate(trimmed = as.numeric(is.na(matches.final))) %>%
  dplyr::select(FIPS, stateName,mdcdExp, mdcdExp2, white_race, black_race, trimmed) %>%
  mutate(maj_white.975 = as.numeric(white_race >= white.quant.975),
         maj_white.95 = as.numeric(white_race >= white.quant.95),
         maj_white.9 = as.numeric(white_race >= white.quant.9),
         maj_white.86 = as.numeric(white_race >= white.quant.86),
         maj_black = as.numeric(black_race >= black.quant))

check <- exp.race %>% filter(maj_white.975 == 1)

table(exp.race$maj_white.975[exp.race$trimmed == 0])

maj.white.xwalk <- exp.race %>%
  dplyr::select(FIPS, maj_white = maj_white.975)

maj.white.xwalk.upd <- exp.race %>%
  dplyr::select(FIPS, maj_white.975, maj_white.95, maj_white.9, maj_white.86)

save(maj.white.xwalk, file = "../data/temp/supermajority.white.xwalk.Rdata")
save(maj.white.xwalk.upd, file = "../data/temp/supermajority.white.xwalk_05.26.Rdata")

##' The majority of the "majority" black counties (in the 75th percentile) are in counties
##' that have not expanded Medicaid. If you go to the 90th percentile, 90\% of these counties have not
##' expanded Medicaid.

table(exp.race$mdcdExp, exp.race$maj_black)
table(exp.race$mdcdExp, exp.race$maj_white)

table(exp.race$mdcdExp2, exp.race$maj_black)
table(exp.race$mdcdExp2, exp.race$maj_white)

summary(exp.race$pscore[exp.race$maj_black == 1])
summary(exp.race$pscore[exp.race$maj_white == 1])

exp.race$matches1 <- ps.penal.calip.cont
exp.race$matches2 <- ps.penal.calip

check1 <- exp.race %>% filter(!is.na(matches1)) %>%
  ungroup() %>%
  group_by(matches1) %>%
  summarize(nmajblackt = sum(maj_black*mdcdExp2),
            nmajblackc = sum(maj_black*(1-mdcdExp2)),
            nmawhitet = sum(maj_white*mdcdExp2),
            nmawhitec = sum(maj_white*(1-mdcdExp2))) %>%
  ungroup() %>%
  mutate(maj_black_comp = nmajblackt > 1 & nmajblackc > 1,
         maj_white_comp = nmawhitet > 1 & nmawhitec > 1) %>%
  filter(maj_white_comp | maj_black_comp)



check2 <- exp.race %>% filter(!is.na(matches2)) %>%
  ungroup() %>%
  group_by(matches2) %>%
  summarize(nmajblackt = sum(maj_black*mdcdExp2),
            nmajblackc = sum(maj_black*(1-mdcdExp2)),
            nmawhitet = sum(maj_white*mdcdExp2),
            nmawhitec = sum(maj_white)*(1-mdcdExp2)) %>%
  ungroup() %>%
  mutate(maj_black_comp = nmajblackt > 1 & nmajblackc > 1,
         maj_white_comp = nmawhitet > 1 & nmawhitec > 1) %>%
  filter(maj_white_comp | maj_black_comp) %>%
  distinct()

##' Half of the majority Black counties are kicked out if the controls are set to 5. While only around 10\% of the
##' supermajority white counties are. When no controls are implemented, there is not this issue. However, with this
##' trimming, there is a more even number of Black counties in the treatment/control group (with around 2x as many in)
##' the control group, wile before it was closer to 5x). For the treatment variable we would be using in 2020, it is now very close.
##' 

sum(check1$maj_black == 1)/sum(exp.race$maj_black == 1)
sum(check1$maj_white)/sum(exp.race$maj_white == 1)

table(check1$mdcdExp, check1$maj_black)
table(check1$mdcdExp2, check1$maj_black)

sum(check2$maj_black == 1)/sum(exp.race$maj_black == 1)
sum(check2$maj_white)/sum(exp.race$maj_white == 1)

