##' ---
##' title: "Standard error calculation"
##' output: github_document
##' ---
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "stringr", "lubridate")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' Calculates pooled standard deviation values for the variables that we calculate balance on 
##' 
##' ## Data
##' 
##' We use the same data read in as for the modeling in 03b.

base.dat <- read.csv("../data/base_cnty_all.csv", colClasses=c(FIPS="character"))

mod.dat1  <- base.dat %>%
  filter(!is.na(a20_34))%>% #take out counties for now that aren't in CDC mort data
  dplyr::select(FIPS, stateName, cntyName, dateExp, yearExp, adult_w_a_cnt, white_race, black_race, latino, male, 
         mortAC_20_34:mortAC_20_64, contains("mortACWhite"), contains("mortACBlack"),
         contains("mortACother"), mortHC_amenable_not_flu, mortOpioid, mortFlu,
         a20_34:a55_64, pctUrban_2010,  medIncome_2013, pctPovty_2013,
         unplmtRate_2013, popDens_2010, smk_tot_2012, pctRep_2012, avgPM25_2011,
         vetPop_2013, snap_2013, pctNoIns_2013, alc_2012,
         diabetes_2012, hyper_male_2009:phys_act_female_2011, calc_multi_house) %>%
  mutate(mdcdExp_det = ifelse(is.na(dateExp), 0, ifelse(ymd(dateExp) < ymd("2014-07-01"), 1, yearExp))) %>%
  dplyr::select(-yearExp) %>%
  filter(!(stateName %in% c("Alaska", "Hawaii"))) %>%
  mutate(log_10_adult_w_a_cnt = log10(adult_w_a_cnt))

mod.dat <- mod.dat1 %>% 
  mutate(mdcdExp = case_when(mdcdExp_det > 1 ~ 0, 
                             stateName == "Wisconsin" ~ 1,
                             TRUE ~ mdcdExp_det),
         mdcdExp2 = case_when(mdcdExp_det > 1 |  stateName == "Wisconsin" ~ 1, 
                              TRUE ~ mdcdExp_det)) %>%
  dplyr::select(-mdcdExp_det, -dateExp)

cc <- complete.cases(mod.dat)
mod.dat <- mod.dat %>% filter(cc)

##' ##Pooled weighted standard deviation

##' We will need to treat the variables that are proportions separately from the variables that are averages
non.prop <- c("medIncome_2013","popDens_2010", "avgPM25_2011")
prop <- setdiff(colnames(mod.dat), c(non.prop, "FIPS","stateName","cntyName","mdcdExp","mdcdExp2","adult_w_a_cnt"))

##' Most of the rate variables need to be mutated to be a proportion (divide by 100 or in the case of the 
##' mortality variables, divide by 100,000)

trans.vars.100 <- c("white_race", "black_race", "latino", "male", "a20_34", "a35_44", 
                "a45_54", "a55_64",  "pctUrban_2010", "pctPovty_2013", "unplmtRate_2013",
                "smk_tot_2012", "pctRep_2012", "pctNoIns_2013", "alc_2012", "diabetes_2012", "hyper_male_2009", 
                "hyper_female_2009", "obsty_male_2011", "obsty_female_2011", "phys_act_male_2011",     
                "phys_act_female_2011", "calc_multi_house", "vetPop_2013","snap_2013")

prop.clean <- mod.dat %>%
  mutate_at(vars(all_of(trans.vars.100)), function(x){x/100}) %>%
  mutate(across(contains("mort"), ~./1e5))

##' To calculate the pooled standard deviations, we
##' 1. calculate weighted variances for the treatment and control group by
##' a. Taking a weighted variance for non-proportion variables
##' b. Taking a weighted mean of proportion variables and then calculating p*(1-p)
##' 2. Calculating a standard pooled standard deviation using these two variances

##' ## Calculations calculating differently for proportion variables.
##' 2014 medicaid expansion
sd.calc1 <- prop.clean %>%
  group_by(mdcdExp) %>%
  summarize(across(all_of(non.prop), 
              .fns = list(wvar = ~Hmisc::wtd.var(x = ., weights = adult_w_a_cnt, normwt=TRUE))),
         across(all_of(prop),
                .fns = list(wmean = ~weighted.mean(x = ., weights = adult_w_a_cnt))),
         n = sum(adult_w_a_cnt)) %>%
  ungroup() %>%
  pivot_longer(medIncome_2013_wvar:calc_multi_house_wmean, names_to = "var", values_to = "val") %>%
  mutate(variance = case_when(str_detect(var, "wvar") ~ val,
                         TRUE ~ val*(1-val))) %>%
  dplyr::select(-val) %>%
  pivot_wider(names_from = mdcdExp, values_from = c(variance, n),
              names_glue = "{.value}_{mdcdExp}")%>%
  mutate(pooled_sd = sqrt(((n_1-1)*variance_1 + (n_0-1)*variance_0)/(n_1+n_0-2)),
         var = str_replace(var, "_wvar|_wmean", "")) %>%
  mutate(pooled_sd = case_when(str_detect(var, "mort")~ pooled_sd *1e5,
                               var %in% trans.vars.100 ~ pooled_sd * 100,
                               TRUE ~ pooled_sd)) #adjust back to data

##' 2020 medicaid expansion
sd.calc2 <- prop.clean %>%
  group_by(mdcdExp2) %>%
  summarize(across(all_of(non.prop), 
                   .fns = list(wvar = ~Hmisc::wtd.var(x = ., weights = adult_w_a_cnt, normwt=TRUE))),
            across(all_of(prop),
                   .fns = list(wmean = ~weighted.mean(x = ., weights = adult_w_a_cnt))),
            n = sum(adult_w_a_cnt)) %>%
  pivot_longer(medIncome_2013_wvar:calc_multi_house_wmean, names_to = "var", values_to = "val") %>%
  mutate(variance = case_when(str_detect(var, "wvar") ~ val,
                              TRUE ~ val*(1-val))) %>%
  dplyr::select(-val) %>%
  pivot_wider(names_from = mdcdExp2, values_from = c(variance, n),
              names_glue = "{.value}_{mdcdExp2}") %>%
  mutate(pooled_sd = sqrt(((n_1-1)*variance_1 + (n_0-1)*variance_0)/(n_1+n_0-2)),
         var = str_replace(var, "_wvar|_wmean", "")) %>%
  mutate(pooled_sd = case_when(str_detect(var, "mort")~ pooled_sd *1e5,
                               var %in% trans.vars.100  ~ pooled_sd * 100,
                               TRUE ~ pooled_sd)) #adjust back to data

##' Save files for use in balance tables
save(sd.calc1, file = "../data/temp/pooled.sd1.Rdata")
save(sd.calc2, file = "../data/temp/pooled.sd2.Rdata")


##' ## Calculations calculating the weighted sd for all variables

vars <- c(prop, non.prop, "log_10_adult_w_a_cnt")

##' 2014 medicaid expansion
sd.calc1 <- mod.dat %>%
  group_by(mdcdExp) %>%
  summarize(across(all_of(vars), 
                   .fns = list(wvar = ~Hmisc::wtd.var(x = ., weights = adult_w_a_cnt, normwt=TRUE))),
            log_10_adult_w_a_cnt_var = var(log_10_adult_w_a_cnt),
            n = sum(adult_w_a_cnt)) %>%
  ungroup() %>%
  dplyr::select(-log_10_adult_w_a_cnt_wvar) %>%
  rename(log_10_adult_w_a_cnt_wvar= log_10_adult_w_a_cnt_var) %>%
  pivot_longer(white_race_wvar:log_10_adult_w_a_cnt_wvar, names_to = "var", values_to = "variance") %>%
  pivot_wider(names_from = mdcdExp, values_from = c(variance, n),
              names_glue = "{.value}_{mdcdExp}")%>%
  mutate(pooled_sd = sqrt(((n_1-1)*variance_1 + (n_0-1)*variance_0)/(n_1+n_0-2)),
         var = str_replace(var, "_wvar", ""))

##' 2020 medicaid expansion
sd.calc2 <- mod.dat %>%
  group_by(mdcdExp2) %>%
  summarize(across(all_of(vars), 
                   .fns = list(wvar = ~Hmisc::wtd.var(x = ., weights = adult_w_a_cnt, normwt=TRUE))),
            log_10_adult_w_a_cnt_var = var(log_10_adult_w_a_cnt),
            n = sum(adult_w_a_cnt)) %>%
  ungroup() %>%
  dplyr::select(-log_10_adult_w_a_cnt_wvar) %>%
  rename(log_10_adult_w_a_cnt_wvar= log_10_adult_w_a_cnt_var) %>%
  pivot_longer(white_race_wvar:log_10_adult_w_a_cnt_wvar,names_to = "var", values_to = "variance") %>%
  pivot_wider(names_from = mdcdExp2, values_from = c(variance, n),
              names_glue = "{.value}_{mdcdExp2}")%>%
  mutate(pooled_sd = sqrt(((n_1-1)*variance_1 + (n_0-1)*variance_0)/(n_1+n_0-2)),
         var = str_replace(var, "_wvar", ""))


##' Calculate Standard deviation for adjacent counties

##' Read in county adjacency data
cnty_adj <- read.csv("../data-raw/county_adjacency2010.csv", colClasses=c(fipscounty="character", fipsneighbor="character"))

##' Set up adjacency matrix (with 0 on the diagonal)
N = nrow(mod.dat)
adj_mat  <- matrix(FALSE, nrow=N, ncol=N)
dimnames(adj_mat)  <- list(mod.dat$FIPS, mod.dat$FIPS)

cdc_fips <- mod.dat$FIPS
adj_fips <- unique(cnty_adj$fipscounty)

cdc_fips[which(!(cdc_fips %in% adj_fips))]
ex.fips <- adj_fips[which(!(adj_fips %in% cdc_fips))]

cnty_adj <- cnty_adj %>%
  filter(!(fipscounty %in% ex.fips) & !(fipsneighbor %in% ex.fips)) %>%
  filter(fipscounty != fipsneighbor) %>%
  dplyr::select(fipscounty, fipsneighbor)
cnty.adj <- as.matrix(cnty_adj)

adj_mat[cnty.adj] <- TRUE
adj_mat <- adj_mat | t(adj_mat)

##' Function to calculate Root Mean Square of differences for a variable given the adjacency matrix

calc_adj_rms <- function(varname, adj_matrix, dat){

  #save vector of variable
  var <- unlist(dat[,colnames(dat) == varname])
  
  N <- nrow(dat)
  #create two matrices: in matc the value of the variable for each county is a column and in matr it is a row 
  matc <- t(var %*% t(rep(1, N)))
  matr <- var %*% t(rep(1, N))
  dimnames(matc) <- dimnames(matr)  <- dimnames(adj_matrix)
  
  #calculate squared difference between all counties
  sq.dif.mat <- (matc - matr)^2
  
  #subset the squared differences to only those between adjacent counties and take only
  #the upper triangle so that the differences are not duplicated
  adj.sq.dif.mat <- sq.dif.mat
  adj.sq.dif.mat[!adj_matrix] <- NA
  adj.sq.dif.mat[lower.tri(adj.sq.dif.mat)] <- NA
  diag(adj.sq.dif.mat) <- NA
  
  #get vector of all squared differences between adjacent counties
  sq.difs <- adj.sq.dif.mat[!is.na(adj.sq.dif.mat)]
  
  #calculate root mean square of differences
  return(list(rms = sqrt(mean(sq.difs)), sq.difs = sq.difs))
}

##' Create full adjacency matrix to check that function aligns as expected with sample standard deviation calculation
adj_mat2  <- matrix(TRUE, nrow=N, ncol=N)
dimnames(adj_mat2)  <- list(mod.dat$FIPS, mod.dat$FIPS)

check <- calc_adj_rms("white_race", adj_mat, mod.dat)
check2 <- calc_adj_rms("white_race", adj_mat2, mod.dat)
check2$rms/sqrt(2)
sd(mod.dat$white_race)

##' Calculate RMS for all variables and add this as a column to the sd.calc data frame

sd.calc1$adj_rms <- rep(NA, nrow(sd.calc1))

for (var in sd.calc1$var){
  
  sd.calc1$adj_rms[sd.calc1$var == var] <- calc_adj_rms(var, adj_mat, mod.dat)$rms/sqrt(2)
  
}

sd.calc2$adj_rms <- sd.calc1$adj_rms

sd.calc1 %>%
  knitr::kable(format = "markdown", digits=c(0,2,2,0,0,2,2))

sd.calc2 %>%
  knitr::kable(format = "markdown", digits=c(0,2,2,0,0,2,2))

##' Save files for use in balance tables
save(sd.calc1, file = "../data/temp/pooled.sd1.v2.Rdata")
save(sd.calc2, file = "../data/temp/pooled.sd2.v2.Rdata")
