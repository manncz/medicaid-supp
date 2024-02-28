##' ---
##' title: "Building computation for test statistic"
##' output: github_document
##' ---
##' 
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
cleanupunderway  <- TRUE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(multcomp)
library("methods")
library("SparseM")
library("PDQutils")
source("../R/StratifiedDesign.R")
source("../R/Zty_cumulants.R")

stopifnot(getRversion() >="3.5.0") # cf below use of `factor(...,levels=,labels=)`
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

##' ## Helper functions for calculating test statistic


##' Generation of predictions using a negative binomial model and a given hypothesized treatment effect.
##' @title Generate negative binomial predictions
##' @param mod.dt data frame (n rows) that includes the outcome mortality variable named "mort", exactly the desired variables to fit the model,
##' plus the population size (named "off"), match id (named "matches") for each county, and treatment status (named "treat").
##' @param delta a numeric value of the hypothesized, multiplicative treatment effect
##' @return a vector of length n with the predicted values 
##' @author Charlotte Z Mann

gen_nb_pred_adj_out <- function(mod.dt, delta){
  
  stopifnot("pop" %in% colnames(mod.dt),
            "treat" %in% colnames(mod.dt),
            "mort" %in% colnames(mod.dt))
  
  #adjust outcome by hypothesized treatment effect
  
  mod.dt$delta.off <- delta^as.numeric(mod.dt$treat > 0)
  mod.dt$id <- 1:nrow(mod.dt)
  
  reg.dt <- mod.dt %>%
    dplyr::select(-FIPS, -matches, -race, -s, -lag, -age) %>%
    filter(pop > 0) %>%
    filter(!is.na(mort))
  
  #fit model
  nb.mod <- glm.nb(mort ~ . - pop - treat - id - delta.off + offset(log(pop)+log(delta.off)), data=reg.dt, na.action = "na.omit")
  print(summary(nb.mod))
  
  #temp.pred <- data.frame(id = reg.dt$id, pred = predict(nb.mod, type = "response"))
  
  #the predictions are of mortality counts, not dividing by the offsets, so we want to divide the prediction by delta
  mod.dt$pred <-  predict(nb.mod, newdata = mod.dt, type = "response")
  temp.dat <- mod.dt %>%
    mutate(pred = pred/delta.off,
           mort = mort/delta.off) %>%
    dplyr::select(-id)
  
  return(temp.dat)
}

##' Calculate the r double as describe on pp. 16-17 of the protocol for every county.
##' @title Calculate r = (e,c)
##' @param dat data frame (n rows) that includes the outcome mortality variable ("mort"), match id (named "matches") for each county, treatment status (named "treat"),
##' predicted values from gen_nb_pred ("pred"), FIPS ("FIPS"), and a race variable ("race"), the lag ("lag"), and county subset analysis indicator ("s"). The lag and subset
##' columns are not used in this analysis, but inclusion in the output makes future steps in the chain easier.
##' @param analysis_race indication of the race to subset to for an analysis if an analysis on a racial subgroup. If the analysis should include all races, this 
##' variable should be NULL (as is default).##' 
##' @param analysis_age indication of the race to subset to for an analysis if an analysis on an age subgroup. If the analysis should include all races, this 
##' variable should be NULL (as is default).
##' @param cutoff number of deaths that would be censored if equal or below
##' @return a data frame with one observation per county, with the original columns given and the added c and e columns.
##' @author Charlotte Z Mann


calc_r <- function(dat, analysis_race = NULL, analysis_age = NULL, cutoff = 0){

  stopifnot("treat" %in% colnames(dat),
            "mort" %in% colnames(dat),
            "pred" %in% colnames(dat),
            "matches" %in% colnames(dat),
            "FIPS" %in% colnames(dat),
            "s" %in% colnames(dat),
            "lag" %in% colnames(dat))
  
    if (!is.null(analysis_race)){
      stopifnot("race" %in% colnames(dat))
      dat.r <- dat %>%
        filter(race == analysis_race) %>%
        group_by(FIPS, matches, treat, s, lag) %>%
        summarize(mort = sum(mort), pred=sum(pred, na.rm =T)) %>%
        ungroup()
    }else if (!is.null(analysis_age)){
      stopifnot("age" %in% colnames(dat))
      dat.r <- dat %>%
        filter(age == analysis_age) %>%
        group_by(FIPS, matches, treat, s, lag) %>%
        summarize(mort = sum(mort), pred = sum(pred, na.rm = T)) %>%
        ungroup()
    }else{
      dat.r <- dat %>%
        group_by(FIPS, matches, treat, s, lag) %>%
        summarize(mort = sum(mort), pred=sum(pred, na.rm = T)) %>%
        ungroup()
      }
     
  print(paste0("The proportion of counties with zero deaths is: ", sum(dat.r$mort == 0, na.rm = T)/nrow(dat.r)))

  min.pred <- min(dat.r$pred[dat.r$pred != 0], na.rm = T)
  print(paste0("The minimum prediction is: ", print(min.pred)))
    
  dat.r <- dat.r %>%
      mutate(r = case_when(is.na(mort) ~ 0,
                           TRUE ~ log(pmax(mort, min.pred))-log(pmax(pred, min.pred)))) %>%
      group_by(matches) %>%
      mutate(mean_r = mean(r)) %>%
      ungroup()%>%
      mutate(e = r - mean_r,
             c = as.numeric(is.na(mort) | mort <= cutoff))
  
  return(dat.r)
}


##' Calculate the test statistic W as described on p. 18 of the protocol as well as save the sums q for each county
##' to be used in the permutation distribution.
##' @title Calculate sum statistic W
##' @param dat data outputted from calc_r with the correct value of m (the county weight) merged onto this data.
##' @return A list including the test statistic W, county-level sums q (Sums) and other county level information
##' to be used in calculating the permutation distribution.
##' @author Charlotte Z Mann

calc_test_stat <- function(dat){
  stopifnot("m" %in% colnames(dat),
            is.numeric(dat$treat),
            "lag" %in% colnames(dat),
            "s" %in% colnames(dat),
            "treat" %in% colnames(dat),
            "matches" %in% colnames(dat),
            "FIPS" %in% colnames(dat))
  
  #s is an indicator about whether the county has a specific characteristic of interest (i.e. supermajority white)
  #thus, we only keep matched groups that have at least one county as such. If no subgroup analysis is of interest,
  #s should equal 1 for all counties.
  dat_full <- dat
  
  dat <- dat %>%
    group_by(matches) %>%
    mutate(k = sum(s)) %>%
    filter(k >= 1)
  
  N <- nrow(dat)
  
  m_mat <-  dat$m %*% t(dat$m)
  
  # Calculate partial ordering
  
  e_mat <- dat$e %*% t(rep(1, N))
  c_mat <- dat$c %*% t(rep(1, N))
  l_mat <- dat$lag %*% t(rep(1, N))
  
  # If both are censored, then they are tied. Having a 1 for both the positive and negative matrices cancel out as a tie
  part_ord_pos <- (c_mat == 0 & t(c_mat) == 1) | (e_mat >= t(e_mat) & c_mat == 0 & t(c_mat) == 0) | (c_mat == 1 & t(c_mat) == 1)
  part_ord_neg <- (c_mat == 1 & t(c_mat) == 0) | (e_mat <= t(e_mat) & c_mat == 0 & t(c_mat) == 0) | (c_mat == 1 & t(c_mat) == 1)
  
  #quick check that logic is right
  #check <- which(part_ord_pos == part_ord_neg)
  #sum((e_mat[check] != t(e_mat)[check])& (c_mat[check] != 1 & t(c_mat)[check] != 1))
  
  part_ord <- part_ord_pos - part_ord_neg
  
  # Calculate q
  
  outlier_lag <- l_mat <= 10 & t(l_mat) <= 10
  
  q_mat <- m_mat * part_ord * outlier_lag
  
  #calculate w
  #if not a county with characteristic of interest, replace sum with 0
  sums <- rowSums(q_mat)*dat$s
  
  w <- sum(dat$treat * sums)

  #if doing the subgroup analysis, want to create a sums vector that is the same length as the original data, replacing
  # the sums for all nonsubgroup counties with 0 (this would result in the same ultimate test statistic)
  dat$Sums <- sums
  dat_full <- dat %>%
    ungroup() %>%
    dplyr::select(FIPS, Sums) %>%
    right_join(dat_full, by = "FIPS") %>%
    mutate(sums = case_when(is.na(Sums) ~ 0,
                            TRUE ~ Sums))
  
  #check
  #sum(dat_full$sums * dat_full$treat) == w
  
  return(list(W = w, Sums = dat_full$sums, 
              Matches = dat_full$matches, Treatment = dat_full$treat, 
              mort = dat_full$mort, pred = dat_full$pred,
              Fips = dat_full$FIPS, s = dat_full$s))
  
}


##' Given a specific analysis, calculate the test statistic and p-value for the test based on the permutation
##' distribution (with no multiplicity adjustment).
##' @title Calculate test statistic and p-value for a given analysis
##' @param pred.dat Data including predicted values (pred) to be fed into calc_r
##' @param m.dat Weight data (m) associated with the analysis of choice
##' @param race Race analysis of choice, or NULL if analysis aggregates races.
##' @param age Age analysis of choice, or NULL if analysis aggregates races.
##' @param cutoff0 number of deaths that would be censored if equal or below according to null hypothesis
##' @return A list including the test statistic W, calculated p-value, cumulants, and other values used in the calculation.
##' @author Charlotte Z Mann

test_stat_by_race <- function(pred.dat, m.dat, race = NULL, age = NULL, cutoff0 = 0){
  
  print(paste0("cutoff = ", cutoff0))
  
  r_dat <- calc_r(dat = pred.dat, analysis_race = race, analysis_age = age, cutoff = cutoff0)
  r_dat <- r_dat %>%
    left_join(m.dat, by = "FIPS")
  
  tstat <- calc_test_stat(r_dat)
  z <- (tstat$Treatment == 1)
  
  sdn  <- create_stratified_design(factor(tstat$Matches), z= z)
  cns  <- Zty_cumulants(sdn, tstat$Sums)
  cdf <- papx_edgeworth(crossprod(z, tstat$Sums),
                 raw.cumulants=cns[1:2])
  pval <- min(c(1-cdf, cdf))*2
  
  return(list(W = tstat$W, pval = pval, cumulants = cns,
              Sums = tstat$Sums, Matches = tstat$Matches, Treatment = tstat$Treatment,
              mort = tstat$mort, pred = tstat$pred, Fips = tstat$Fips, s = tstat$s))
  
}

##' We need to define methods for a made up class `smsts` in order to use the `ghlt` function from `multcomp.
##' I assume that `smsts` objects will be a list with elements `t.stats` and `cov`.

coef.sumsts <- function(obj){
  return(obj$t.stats)
}

vcov.sumsts <- function(obj, complete = FALSE){
  return(obj$cov)
}



##' Given a specific analysis, calculate the p-values for a given delta value (null hypothesized multiplicative treatment effect)
##' and outcome (all cause, healthcare amenable, flu or opioid) and conduct a max-t correction.

##' @title Calculate p-values and max-t corrected p-values for a given outcome mortality type
##' @param delta null hypothesized treatment effect (numeric value)
##' @param dat data at the county, age, race level for the year of interest that includes all covariates of interest
##' @param form formula object with the base form to be used to generate a model matrix
##' @param mort_name single character value, name of the outcome mortality variable to be used
##' @param var_names vector of the variables to be used in the covariance adjustment model
##' @param m.dat.list list of m data files for overall and each race 
##' @param race_analyses vector of names of races to be included as race subgroup analyses
##' @param age_analyses vector of names of ages to be included as age subgroup analyses
##' @param subgroup logical indicator of whether or not to run the supermajority white analysis
##' @param overall logical indicator of whether or not to run the overall (all races and counties included) analysis
##' @param subgroup_adj logical indicator of whether or not to include the supermajority white analysis in the max-t correction
##' @param censoring_cutoff largest number of deaths that were censored in the data (aka mortality count was censored if less than or equal to the cutoff)
##' @return A list including the output from `test_stat_by_race` for each race analysis and the max-t adjusted p-values.
##' @author Charlotte Z Mann

##' Note: Since the same predictions for every delta and alpha are used for all subgroup analyses, it is most efficient to 
##' calculate the test statistic for all at once, given the predictions.


test_stat_wrapper <- function(delta = 1, dat, form, mort_name = "mort_all_cause_2014", var_names, 
                              m.dat.list, age_analysis = NULL,
                              race_analyses = NULL, subgroup = F, overall = T, subgroup_adj = F,
                              censoring_cutoff = 0,
                              alt.mort = NULL){
  
  options(na.action='na.pass')
  
  reg.dat <- dat %>%
    ungroup() %>%
    dplyr::select(FIPS, pop, matches, treat, race, age, mort = all_of(mort_name), s, lag) %>%
    cbind(model.matrix(form, data = dat)[,var_names])
  
  options(na.action='na.omit')
  
  #now we have data with the adjusted outcomes and predictions
  mod.dat <- gen_nb_pred_adj_out(mod.dt = reg.dat, 
                              delta = delta)
  
  # a little bit hacky fix for using aggregated mortality outcomes when censored after fitting model at 
  # county/age level
  if(!is.null(alt.mort)){
    mod.dat <- mod.dat %>%
      group_by(FIPS, matches, treat, s, lag) %>%
      summarize(pred=sum(pred, na.rm = T), check = sum(mort, na.rm=T)) %>%
      ungroup() %>%
      left_join(alt.mort, by = "FIPS") %>%
      mutate(delta.off = delta^as.numeric(treat>0)) %>%
      mutate(mort2 = mort/delta.off) %>%
      dplyr::select(-delta.off)
  }
  
  test_stats <- list()
  
  W_vec <- c()
  analysis_name <- c()
  k1 <- c()
  
  #first calculate the test statistic for a subgroup analysis, if desired.
  if(subgroup){
    test_stats$subgroup <- test_stat_by_race(pred.dat = mod.dat, m.dat = m.dat.list[["all"]], race = NULL, 
                                             age = age_analysis, cutoff0 = censoring_cutoff/delta)
    
    if(subgroup_adj == T){
      W_vec <- c(W_vec, test_stats$subgroup$W)
      analysis_name <- c(analysis_name, "subgroup")
      k1 <- c(k1, test_stats$subgroup$cumulants[1])
    }
    
  }
  
  #for the rest of the analyses, we want the subgroup vector to be 1
  mod.dat$s <- 1
  
  if(overall){
    test_stats$overall <- test_stat_by_race(pred.dat = mod.dat, m.dat.list[["all"]], race = NULL,
                                            age = age_analysis, cutoff0 = censoring_cutoff/delta)
    W_vec <- c(W_vec, test_stats$overall$W)
    analysis_name <- c(analysis_name, "overall")
    k1 <- c(k1, test_stats$overall$cumulants[1])
  }
  
  if(!is.null(race_analyses)){
    for(race in race_analyses){
      test_stats[[race]] <-  test_stat_by_race(pred.dat = mod.dat, m.dat.list[[race]], race = race,
                                               age = age_analysis, cutoff0 = censoring_cutoff/delta)
      W_vec <- c(W_vec, test_stats[[race]][["W"]])
      analysis_name <- c(analysis_name, race)
      k1 <- c(k1, test_stats[[race]][["cumulants"]][1])
    }
  }
  
  
  # only do the max-t adjustment if the race analyses are being computed (this allows for only running the overall or 
  #supermajority white analyses without taking the time to do everything.)
  
  if(!is.null(race_analyses)) {
    
    #append all vectors with sums to the full dataset
    sum.dat <- mod.dat %>%
      group_by(FIPS, matches, treat) %>%
      summarize(mort = sum(mort))
    
    for(analysis in names(test_stats)){
      
      temp.dat <- data.frame(test_stats[[analysis]][["Sums"]],
                             test_stats[[analysis]][["Fips"]])
      colnames(temp.dat) <- c(paste0("sums_", analysis), "FIPS")
      
      sum.dat <- sum.dat %>%
        left_join(temp.dat, by = "FIPS")
    }
    
    if(!subgroup_adj & subgroup == T){
      sum.dat <- sum.dat %>%
        dplyr::select(-sums_subgroup)
    }
    
    #use full data matched sets and treatment indicator to define stratification design
    z <- (sum.dat$treat==1)
    sdn  <- create_stratified_design(factor(sum.dat$matches), z= z)
    
    #calculate covariance matrix
    xes <- sum.dat %>%
      ungroup() %>%
      dplyr::select(starts_with("sums_")) %>%
      as.matrix()
    
    cov.mat <- Zty_cov(sdn, xes)
    
    #create model object to feed into the glht function
    
    mod.obj <- list(t.stats = W_vec, cov = cov.mat)
    class(mod.obj) <- "sumsts"
    
    n.test <- length(W_vec)
    
    out <- glht(mod.obj, diag(n.test), rhs = k1)
    
    p.vals <- as.vector(summary(out)$test$pvalues)
    names(p.vals) <- analysis_name
    
    test_stats$adjusted_p <- list(glht.out = out, p.vals = p.vals)
    
  }  
 
  return(test_stats)
  
}

##' Functions for use in uniroot to determine treatment effect estimates (Hodges-Lehman) and 
##' confidence interval bounds.

##' @title Estimate the difference between W and the expected value of W for a given delta value and analysis
##' @param delta null hypothesized treatment effect (numeric value)
##' @param dat data at the county, age, race level for the year of interest that includes all covariates of interest
##' @param form formula object with the base form to be used to generate a model matrix
##' @param mort_name single character value, name of the outcome mortality variable to be used
##' @param var_names vector of the variables to be used in the covariance adjustment model
##' @param m.dat.list list of m data files for overall and each race 
##' @param analysis the name of the desired analysis (either "overall", "subgroup", or one of the race labels)
##' @return The difference between W and E(W)
##' @author Charlotte Z Mann

myest_fun <- function(delta = 1, dat, form, mort_name = "mort_all_cause_2014", var_names, 
                              m.dat.list, analysis, censoring_cutoff = 0){
  options(na.action='na.pass')
  
  reg.dat <- dat %>%
    ungroup() %>%
    dplyr::select(FIPS, pop, matches, treat, race, age, mort = all_of(mort_name), s, lag) %>%
    cbind(model.matrix(form, data = dat)[,var_names])
  
  options(na.action='na.omit')
  
  #now we have data with the adjusted outcomes and predictions
  mod.dat <- gen_nb_pred_adj_out(mod.dt = reg.dat, 
                                 delta = delta)
  
  #first calculate the test statistic for a subgroup analysis, if desired.
  if(analysis == "subgroup"){
    test_stats <- test_stat_by_race(mod.dat, m.dat = m.dat.list[["all"]], race = NULL, cutoff0 = censoring_cutoff/delta)
  }
  
  #for the rest of the analyses, we want the subgroup vector to be 1
  mod.dat$s <- 1
  
  if(analysis == "overall"){
    test_stats <- test_stat_by_race(mod.dat, m.dat.list[["all"]], race = NULL, cutoff0 = censoring_cutoff/delta)
  }
  
  if(analysis %in% c("Hispanic", "Non-Hispanic AI or AN", "Non-Hispanic Asian or Pacific Islander", 
                     "Non-Hispanic Black", "Non-Hispanic White")){
    test_stats <- test_stat_by_race(mod.dat, m.dat.list[[analysis]], race = analysis, cutoff0 = censoring_cutoff/delta)
  }
  
 
  dif <- test_stats$W - test_stats$cumulants[1]
  
  return(dif)
  
}


##' @title Calculate difference between p-value and desired alpha level.
##' @param delta null hypothesized treatment effect (numeric value)
##' @param alpha desired alpha level
##' @param dat data at the county, age, race level for the year of interest that includes all covariates of interest
##' @param form formula object with the base form to be used to generate a model matrix
##' @param mort_name single character value, name of the outcome mortality variable to be used
##' @param var_names vector of the variables to be used in the covariance adjustment model
##' @param m.dat.list list of m data files for overall and each race 
##' @param analysis the name of the desired analysis (either "overall", "subgroup", or one of the race labels)
##' @param race_analyses vector of names of races to be included as race subgroup analyses
##' @param maxt logical indicator of whether the maxt adjustment should be implemented
##' @return The difference between the calculated p-value and the alpha level.
##' @author Charlotte Z Mann

pval_fun <- function(delta = 1, alpha = .05, dat, form, mort_name = "mort_all_cause_2014", var_names, 
                      m.dat.list, analysis, race_analyses, censoring_cutoff = 0, maxt = TRUE){
  
  if(maxt == FALSE){
    
    options(na.action='na.pass')
    
    reg.dat <- dat %>%
      ungroup() %>%
      dplyr::select(FIPS, pop, matches, treat, race, age, mort = all_of(mort_name), s, lag) %>%
      cbind(model.matrix(form, data = dat)[,var_names])
    
    options(na.action='na.omit')
    
    #now we have data with the adjusted outcomes and predictions
    mod.dat <- gen_nb_pred_adj_out(mod.dt = reg.dat, 
                                   delta = delta)
    
    
    #first calculate the test statistic for a subgroup analysis, if desired.
    if(analysis == "subgroup"){
      test_stats <- test_stat_by_race(mod.dat, m.dat = m.dat.list[["all"]], race = NULL, cutoff0 = censoring_cutoff/delta)
    }
    
    #for the rest of the analyses, we want the subgroup vector to be 1
    mod.dat$s <- 1
    
    if(analysis == "overall"){
      test_stats <- test_stat_by_race(mod.dat, m.dat.list[["all"]], race = NULL, cutoff0 = censoring_cutoff/delta)
    }
    
    if(analysis %in% c("Hispanic", "Non-Hispanic AI or AN", "Non-Hispanic Asian or Pacific Islander", 
                       "Non-Hispanic Black", "Non-Hispanic White")){
      test_stats <- test_stat_by_race(mod.dat, m.dat.list[[analysis]], race = analysis, cutoff0 = censoring_cutoff/delta)
    }
    
    
    pval.dif <- test_stats$pval - alpha
    
  }else{
    
    
    out <- test_stat_wrapper(delta = delta, dat=dat, form=form, mort_name = mort_name, var_names = var_names, 
                             m.dat.list = m.dat.list, 
                             race_analyses = race_analyses, subgroup = F, overall = T, subgroup_adj = F,
                             censoring_cutoff = censoring_cutoff)
    
    temp.vec <- out$adjusted_p$p.vals
    pval.dif <- temp.vec[which(names(temp.vec) == analysis)] - alpha
  }
  
  return(pval.dif)
  
}

#rmarkdown::render("12-partial-order-helpers.R", "github_document")

