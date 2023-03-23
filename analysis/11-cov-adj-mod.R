##' ---
##' title: "Covariance Adjustment Models"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(readr)
library(stringr)
library(glmnet)
library(MASS)
library(dplyr)
library(tidyr)
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' First, pull in the data from compute 1 (the files paths work with CM's local setup)
path <- file.path("..","..","..", "compute1","data")
#path <- "../data"
set.seed(2795)

##' # 2013 Regression
load(file.path(path,"out_mod_dat_2013.Rdata"))

##' ### Quasi-Poisson LASSO

##' We are only interested in the counties that were not trimmed and the cells for which the 2013 population
##' is greater than 0. The population is only 0 for 1225 cells out of 62168.
reg_dat_2013 <- mod_2013 %>%
  filter(trimmed == 0 & pop_2013 > 0) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2012,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2011,
         mort_all_cause_l1 = mort_all_cause_2012,
         mort_all_cause_l2 = mort_all_cause_2011)

##' In order to assist with greating the design matrix, we create vectors of all variable names and 
##' the interactions.
vars <- reg_dat_2013 %>%
  ungroup() %>%
  dplyr::select(race, age, mort_HC_amenable_not_flu_l1, mort_HC_amenable_not_flu_l2,
         mort_all_cause_l1, mort_all_cause_l2, white_race:calc_multi_house, log_adult_w_a_cnt) %>%
  colnames()

interactions <- paste(vars[-(1:2)], "age", sep = ":")

##' ### All Cause Mortality
##' The design matrix has 195 columns, but we have 60,076 observations so this should be fine.
form <- formula(paste0(" ~ ", paste(c(vars, interactions), collapse="+")))

X <- model.matrix(form, data = reg_dat_2013)[,-1]
y <- reg_dat_2013$mort_all_cause_2013
off <-  reg_dat_2013$pop_2013

##' We start with cross validation for the best lambda value.
#+ eval = FALSE
set.seed(2795)
# cv.out.all <- cv.glmnet(x=X, y=y, family = quasipoisson(), alpha = 1,
#        offset = log(off))
# 
# save(cv.out.all, file = "../data/temp/glmnetcv.2013all.Rdata")

##' ### Healthcare Amenable Mortality

y2 <- reg_dat_2013$mort_HC_amenable_not_flu_2013

##' We start with cross validation for the best lambda value.
##' #+ eval = FALSE
set.seed(2795)
# cv.out.hca <- cv.glmnet(x=X, y=y2, family = quasipoisson(), alpha = 1,
#                     offset = log(off))
# 
# save(cv.out.hca, file = "../data/temp/glmnetcv.2013hca.Rdata")

##' ### Influenza / Pneumonia Mortality

y3 <- reg_dat_2013$mort_Flu_2013

##' We start with cross validation for the best lambda value.
##' #+ eval = FALSE
set.seed(2795)
# cv.out.flu <- cv.glmnet(x=X, y=y3, family = quasipoisson(), alpha = 1,
#                     offset = log(off))
# 
# save(cv.out.flu, file = "../data/temp/glmnetcv.2013flu.Rdata")

##' ### Mortality with opioids involved (any cause of death)
y4 <- reg_dat_2013$mort_opioid_involved_2013

set.seed(2795)
# cv.out.opioid <- cv.glmnet(x=X, y=y4, family = quasipoisson(), alpha = 1,
#                    offset = log(off))
# 
# save(cv.out.opioid, file = "../data/temp/glmnetcv.2013opioid.Rdata")


##' ### Negative Binomial

load("../data/temp/glmnetcv.2013all.Rdata")
load("../data/temp/glmnetcv.2013hca.Rdata")
load("../data/temp/glmnetcv.2013flu.Rdata")
load("../data/temp/glmnetcv.2013opioid.Rdata")

##' ### All Cause Mortality

vars.allmort <- names(coef(cv.out.all, s = "lambda.min")[which(coef(cv.out.all, s = "lambda.min")[,1]!=0),1])[-1]
mod.dat <- data.frame(cbind(y,X[,vars.allmort])) %>%
  rename(mort = y)

mod.all <- glm.nb(mort ~ . + offset(log(off)), data=mod.dat)
summary(mod.all)

##' ### Healthcare Amenable Mortality

vars.hcamort <- names(coef(cv.out.hca, s = "lambda.min")[which(coef(cv.out.hca, s = "lambda.min")[,1]!=0),1])[-1]
mod.dat <- data.frame(cbind(y2,X[,vars.hcamort])) %>%
  rename(mort = y2)


mod.hca <- glm.nb(mort ~ . + offset(log(off)), data=mod.dat)
summary(mod.hca)

##' ### Influenza / Pneumonia Mortality
vars.flumort<- names(coef(cv.out.flu, s = "lambda.min")[which(coef(cv.out.flu, s = "lambda.min")[,1]!=0),1])[-1]


##' Save a list with the LASSO selected variables for use later
vars.opioidmort <- names(coef(cv.out.opioid, s = "lambda.min")[which(coef(cv.out.opioid, s = "lambda.min")[,1]!=0),1])[-1]

var_list <- list(all_cause = vars.allmort, hca = vars.hcamort, flu = vars.flumort, opioid = vars.opioidmort)
save(var_list, file = "../data/temp/mod.var.list.Rdata")


##' # 2014 Regression
##' 
##' Here we are trying to see if we can use the 2013 models for the 2014 analyses.

load(file.path(path,"out_mod_dat_2014.Rdata"))

reg_dat_2014 <- mod_2014 %>%
  filter(trimmed == 0 & pop_2014 > 0) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2013,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2012,
         mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012) %>%
  filter(mdcdExp == 0)

##' Setting up the regression data and offsets
form <- formula(paste0("mort_all_cause_2014 ~ ", paste(c(vars, interactions), collapse="+")))
save(form, file = "../data/temp/form.Rdata")

X <- model.matrix(form, data = reg_dat_2014)[,-1]
y <- reg_dat_2014$mort_all_cause_2014
y2 <- reg_dat_2014$mort_HC_amenable_not_flu_2014
y3 <- reg_dat_2014$mort_Flu_2014
off <-  reg_dat_2014$pop_2014

##' ### All Cause Mortality

##' Subset to the variables that were found by the 2013 LASSO with the 2014 data
mod.dat <- data.frame(cbind(y,X[,vars.allmort])) %>%
  rename(mort = y)

##' First, we make predictions from the 2013 model using the 2014 data. These are predictions on the scale of 
##' mortality counts, just so that I could take a look at them.
pred <- predict(mod.all, newdata=mod.dat, type = "response")

##' First, we fit a full model (unrestricted) with the 2014 data.
mod.all.2014 <- glm.nb(mort ~ . + offset(log(off)), data=mod.dat)

summary(mod.all.2014)

##' The `glm.convert` function converts a `glm.nb` object to a `glm` object so that the formula
##' can be updated without changing theta. So here, we refit the model (keeping the estimated theta from the full 2014 model),
##' with only an intercept and the predictions from the 2013 model on the 2014 data as an offset. This allows for the coefficients
##' from the 2013 model to remain the same in this model.

mod.all.2014.conv <- glm.convert(mod.all.2014)
mod.all.2014.2013 <- update(mod.all.2014.conv, . ~ 1 + offset(log(off)+log(pred)))

summary(mod.all.2014)
summary(mod.all.2014.2013)

mod.dat$pred2013 <- pred
mod.dat$pred2014 <- predict(mod.all.2014, type = "response")
mod.dat$pred2014.2013 <- predict(mod.all.2014.2013, type = "response")

check <- mod.dat %>%
  dplyr::select(mort,contains("pred"))

##' Comparing the model fit to the 2014 data versus the model fit to the 2013 data, we find that the
##' 2014 model is a better fit.
anova(mod.all.2014.conv, mod.all.2014.2013, test = "Chisq")


##' I was also curious to check if this looked similarly for a poisson model, and it did.
test1 <- glm(mort~., data = mod.dat, offset = log(off), family = "poisson")
test <- glm(mort~1, data = mod.dat, offset = log(off) + log(pred), family = "poisson")
pred2 <- predict(test1, type = "response")
test2 <- glm(mort~1, data = mod.dat, offset = log(off) + log(pred2), family = "poisson")

summary(test1)
summary(test2)

anova(test1, test2, test = "Chisq")
anova(test1, test, test = "Chisq")

##' ### Healthcare Amenable Mortality
##' Here we go through the same process for healthcare amenable mortality as an outcome.
##' And again find that the 2014 model fit is better.

mod.dat <- data.frame(cbind(y2,X[,vars.hcamort])) %>%
  rename(mort = y2)

pred <- predict(mod.hca, newdata = mod.dat, type = "response")

mod.hca.2014 <- glm.nb(mort ~ .+offset(log(off)), data=mod.dat)

mod.hca.2014.conv <- glm.convert(mod.hca.2014)

mod.hca.2014.2013 <- update(mod.hca.2014.conv, . ~ 1 + offset(log(off)+log(pred)))

summary(mod.hca.2014)
summary(mod.hca.2014.2013)

anova(mod.hca.2014.conv, mod.hca.2014.2013, test = "Chisq")


##' ### Influenza / Pneumonia Mortality
##' We won't try the two different model fits for the flu outcome, but rather just fit the 2014 model to take a look.

mod.dat <- data.frame(cbind(y3,X[,vars.flumort])) %>%
  rename(mort = y3)

mod.flu.2014 <- glm.nb(mort ~ ., data=mod.dat,
                  offset(log(off)), init.theta = 3.3)
summary(mod.flu.2014)


##' ## 2020 Analyses

##' I am going to assume that the same variables are fine for the 2020 models as the 2014 models. However, we need
##' to fit the models at the county level, instead of the county, age, race level for 2020.

reg_dat_2013_sum <- mod_2013 %>%
  filter(trimmed == 0 & pop_2013 > 0) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2012,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2011,
         mort_all_cause_l1 = mort_all_cause_2012,
         mort_all_cause_l2 = mort_all_cause_2011) %>%
  group_by(FIPS) %>%
  summarize(across(starts_with("mort_"), sum), 
            across(starts_with("pop_"), sum),
            across(white_race:calc_multi_house, max), log_adult_w_a_cnt = max(log_adult_w_a_cnt))

form2 <- formula(paste0(" ~ ", paste(vars[-c(1,2)], collapse="+")))

save(form2, file = "../data/temp/form.2020.Rdata")

##' We need to re-run the cross validation to determine the variables used when estimating the outcome
##' at the FIPS level.
X <- model.matrix(form2, data = reg_dat_2013_sum)[,-1]
y <- reg_dat_2013_sum$mort_all_cause_2013
off <-  reg_dat_2013_sum$pop_2013

##' All Cause
set.seed(2795)
cv.out.all.agg <- cv.glmnet(x=X, y=y, family = quasipoisson(), alpha = 1,
       offset = log(off))
save(cv.out.all.agg, file = "../data/temp/glmnetcv.2013all.agg.Rdata")


load("../data/temp/glmnetcv.2013all.agg.Rdata")
vars.allmort.agg <- names(coef(cv.out.all.agg, s = "lambda.min")[which(coef(cv.out.all.agg, s = "lambda.min")[,1]!=0),1])[-1]

save(vars.allmort.agg, file = "../data/temp/mod.vars.2020.Rdata")

##' Clean 2020 data to be used in modeling
load("../data/out_mod_dat_2020.Rdata")

reg_dat_2020 <- mod_2020 %>%
  mutate(trimmed = as.numeric(is.na(matches.final))) %>%
  filter(trimmed == 0 & pop > 0) %>%
  rename(mort_HC_amenable_not_flu_l1= mort_HC_amenable_not_flu_2013,
         mort_HC_amenable_not_flu_l2= mort_HC_amenable_not_flu_2012,
         mort_all_cause_l1 = mort_all_cause_2013,
         mort_all_cause_l2 = mort_all_cause_2012) %>%
  filter(mdcdExp2 == 0) %>%
  filter(!is.na(covid_mort)) %>%
  mutate(covid_mort = round(covid_mort),
         all_mort = round(all_mort)) %>%
  arrange(FIPS)

X_2020 <- model.matrix(form2, data = reg_dat_2020)[,-1]
off_2020 <- reg_dat_2020$pop
y_2020 <- reg_dat_2020$all_mort
y2_2020 <- reg_dat_2020$covid_mort

non.cens.fips <- reg_dat_2020$FIPS

##' All Cause 2013 model

reg_dat_2013_sum_cens <- reg_dat_2013_sum %>%
  filter(FIPS %in% non.cens.fips) %>%
  arrange(FIPS)
X <- model.matrix(form2, data = reg_dat_2013_sum_cens)[,-1]
y <- reg_dat_2013_sum_cens$mort_all_cause_2013
off <-  reg_dat_2013_sum_cens$pop_2013

##' Making sure that the two datasets are aligned since the offset is used in the prediction function
sum(reg_dat_2013_sum_cens$FIPS != reg_dat_2020$FIPS)

mod.dat <- data.frame(cbind(y,off,X[,vars.allmort.agg])) %>%
  rename(mort = y)

mod.all.2013.agg <- glm.nb(mort ~ . -off + offset(log(off)), data=mod.dat)
coefficients(mod.all.2013.agg)

preds <- predict(mod.all.2013.agg)

##' First, we fit a full model (unrestricted) with the 2014 data.
mod.dat.2020 <- data.frame(cbind(y_2020,off_2020,X_2020[,vars.allmort.agg])) %>%
  rename(mort = y_2020)

mod.all.2020 <- glm.nb(mort ~ . - off_2020 + offset(log(off_2020)), data=mod.dat.2020)
summary(mod.all.2020)

pred.2013.agg <- predict(mod.all.2013.agg, newdata = mod.dat.2020)

mod.all.2020.conv <- glm.convert(mod.all.2020)
mod.all.2020.2013 <- update(mod.all.2020.conv, . ~ 1 + offset(log(off_2020)+pred.2013.agg))

summary(mod.all.2020.2013)

##' Comparing the model fit to the 2020 data versus the model fit to the 2013 data, we find that the
##' 2020 model is a better fit.
anova(mod.all.2020.conv, mod.all.2020.2013, test = "Chisq")
