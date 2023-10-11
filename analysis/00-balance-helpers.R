weighted_ave  <- function(x, w)
{
  if (missing(w)) stop("no weight given")
  if (length(w)!=length(x)) stop("lengths of x and w differ")
  rep(weighted.mean(x, w), length(x))
}

get.balTest <- function(match, form = form1, dat = mod.dat, with_unstratified=TRUE){
  if (!match %in% colnames(dat)) # if match lives in workspace not dat
    dat[[match]]  <- get(match, -1L) 
  formul <- update.formula(form, paste0(".~.+ strata(", match, ")"))
  if (!with_unstratified) formul  <- update.formula(formul, ".~.-1")
  myb0 <- RItools::balanceTest(formul,
                               data = dat,
                               unit.weights = adult_w_a_cnt)
  cols_to_align  <- purrr::map_lgl(dat, is.numeric) # OK for us b/c no factor or Boolean x's
  cols_to_align  <- cols_to_align & colnames(dat)!=as.character(formul[[2]])
  
  dat2  <- dat[colnames(dat)==match | cols_to_align] %>%
    group_by(.data[[match]]) %>%
    mutate_at(vars(-group_cols()), 
              ~(.x - weighted_ave(.x, adult_w_a_cnt))
    ) %>% dplyr::select(!adult_w_a_cnt) %>% ungroup()
  dat2  <- cbind(dat[,c(as.character(formul[[2]]), 'adult_w_a_cnt'),
                     drop=FALSE],
                 dat2)
  myb2 <- RItools::balanceTest(update.formula(formul, .~.-1),
                               data = dat2,
                               unit.weights = adult_w_a_cnt)
  myb0$results[,c("z", "p"),match]  <-
    myb2$results[,c("z", "p"),,drop=FALSE]
  if (with_unstratified)
  {
    myb0$overall[match,]  <- myb2$overall[,]
    return(myb0)
  } else {
    myb2$results  <- myb0$results
    return(myb2)
  }
  
}


get.balTest.bpop <- function(match, form = form1, dat = mod.dat, with_unstratified=TRUE){
  if (!match %in% colnames(dat)) # if match lives in workspace not dat
    dat[[match]]  <- get(match, -1L) 
  formul <- update.formula(form, paste0(".~.+ strata(", match, ")"))
  if (!with_unstratified) formul  <- update.formula(formul, ".~.-1")
  dat <- dat %>%
    mutate(black_pop = black_race/100*adult_w_a_cnt)
  myb0 <- RItools::balanceTest(formul,
                               data = dat,
                               unit.weights = black_pop)
  cols_to_align  <- purrr::map_lgl(dat, is.numeric) # OK for us b/c no factor or Boolean x's
  cols_to_align  <- cols_to_align & colnames(dat)!=as.character(formul[[2]])
  
  dat2  <- dat[colnames(dat)==match | cols_to_align] %>%
    group_by(.data[[match]]) %>%
    mutate_at(vars(-group_cols()), 
              ~(.x - weighted_ave(.x, adult_w_a_cnt))) %>% 
    dplyr::select(!adult_w_a_cnt) %>% 
    ungroup()
  dat2  <- cbind(dat[,c(as.character(formul[[2]]),"black_pop"),
                     drop=FALSE],
                 dat2)
  myb2 <- RItools::balanceTest(update.formula(formul, .~.-1),
                               data = dat2,
                               unit.weights = black_pop)
  myb0$results[,c("z", "p"),match]  <-
    myb2$results[,c("z", "p"),,drop=FALSE]
  if (with_unstratified)
  {
    myb0$overall[match,]  <- myb2$overall[,]
    return(myb0)
  } else {
    myb2$results  <- myb0$results
    return(myb2)
  }
  
}


##' Define variable names of interest

var.names <- c("% White", "% Black", "% Latino", "% Male",
               "All Mortality", "20-34 Mortality", "35-44 Mortality", "45-54 Mortality", "55-64 Mortality",
               "White Male Mortality", "White Female Mortality",
               "Black Male Mortality", "Black Female Mortality",
               "Other Race Male Mortality", "Other Race Female Mortality",
               "Healcare Amenable (non-flu) Mortality", "Opioid Mortality", "Flu Mortality",
               "% 20-34", "% 35-44", "% 45-54", "% 55-64", "Population Density",
               "% Urban",  "% Veteran", "Median Income", "% Poverty",  "% SNAP", 
               "% No Health Insurance", "Unemployment Rate",
               "PM2.5", "Smoking",  "Heavy Drinking",
               "Diabetes", "Male Hypertension", "Female Hypertension",
               "Male Obesity", "Female Obesity", "Male Physical Activity",
               "Female Physical Activity","% Republican", "% Multigenerational Households", "Population"
)

vars <- c("white_race", "black_race", "latino", "male", 
          "mortAC_20_64", "mortAC_20_34", "mortAC_35_44", "mortAC_45_54", "mortAC_55_64", 
          "mortACWhite_M", "mortACWhite_F", "mortACBlack_M", "mortACBlack_F",
          "mortACother_M", "mortACother_F", "mortHC_amenable_not_flu", "mortOpioid", "mortFlu", 
          "a20_34", "a35_44", "a45_54", "a55_64", "popDens_2010",
          "pctUrban_2010", "vetPop_2013", "medIncome_2013",  
          "pctPovty_2013", "snap_2013", "pctNoIns_2013","unplmtRate_2013", 
          "avgPM25_2011", "smk_tot_2012",   "alc_2012",
          "diabetes_2012", "hyper_male_2009", "hyper_female_2009",
          "obsty_male_2011", "obsty_female_2011", "phys_act_male_2011", "phys_act_female_2011",
          "pctRep_2012", "calc_multi_house", "log_10_adult_w_a_cnt")

#create a cross walk for variable labels to reduce error
var.labels.xwalk <- data.frame(vars = vars, labels = var.names)

#function to create love plots
#+ include = FALSE
love.plot <- function(bal.out, name = "psm", rownames.xwalk, print = F, 
                      fpath = "../paper/protocol/figures/love_plot_w_"){
  
  #get matrix of differences
  b_dat <- data.frame(bal.out$results[,3,])
  
  #fix row names and column names for plotting
  b_dat <- b_dat %>%
    rownames_to_column(var = "vars") %>%
    left_join(rownames.xwalk, by = "vars") %>%
    column_to_rownames(var = "labels") %>%
    dplyr::select(-vars)
  
  colnames(b_dat) <- c("Matching with Full Model", "Unadjusted")
 
  
  png(paste0(fpath, name, ".png"), width = 7, height = 10, units = 'in', res = 300)
  RItools:::balanceplot(b_dat,
                        colors = c("#7fcdbb","#2c7fb8"),
                        xlab = "Balance Before/After Matching")
  abline(v=c(-.25, .25), lty=2)
  dev.off()
  
  if(print){
    
    RItools:::balanceplot(b_dat,
                          colors = c("#7fcdbb","#2c7fb8"),
                          xlab = "Balance Before/After Matching")
    abline(v=c(-.25, .25), lty=2)
    dev.off()
  }
}


#+ include = FALSE
#function to create balance tables
bal.table <- function(bal.out, adj = TRUE, rownames.xwalk, standard.errs){
  if(adj){
    indx = 1
    cap = "Balance on Covariates after Matching (Adjusted)"  
  } else {
    indx = 2
    cap = "Balance on Covariates before Matching (Unadjusted)"
  }
  
  #extract table from xBalance output
  tab <- data.frame(bal.out$results[,,indx])
  
  tab$var <- rownames(tab) 
  tab.upd <- tab %>%
    left_join(standard.errs, by = "var") %>%
    mutate(std.diff2 = case_when(var == "log_10_adult_w_a_cnt" ~ std.diff,
                                 TRUE ~ std.diff*pooled.sd/pooled_sd)) %>%
    rename(vars = var) %>%
    left_join(rownames.xwalk, by = "vars") %>%
    column_to_rownames(var = "labels") %>%
    dplyr::select(Control, Treatment, adj.diff, std.diff2, pooled_sd, adj_rms, z, p)
  
  #add column with significance stars
  stars <- factor(cut(as.numeric(tab.upd$p), breaks=c(0, .001, .01, .05, .1, 1), labels = c("***", "**", "*", ".","")))
  #levels(stars) <- c("***", "**", "*", ".","")
  tab.upd$stars <- stars
  
  #fix column names
  colnames(tab.upd) <- c("Control", "Treatment","Dif", "Std Dif", "Pooled SD", "Adjcnt SD","z", "p-value", "")
  
  #for markdown use kable
  kbl <- knitr::kable(tab.upd, caption = cap, format = "markdown", digits=c(2,2,2,3,3,3,3,3))
  
  return(list(tab = tab.upd, kbl = kbl))
  
}
