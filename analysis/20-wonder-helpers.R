##' ---
##' title: "Helper functions for WONDER analysis"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.


# define confidence interval generation function for this case
gen_conf_int_dat <- function(delta.min = 0, delta.max = 2, delta.by = .01, data = dat.2014, analys = "all_cause",
                             race_subgroup_analys = NULL, sub = T, overl = T, m.dat.list. = m.dat.list,
                             forml = form1, age_analys = NULL, alt_mort = NULL, var.names = vars.wonder,
                             out_name = NULL){
  
  delta.vec <- seq(delta.min, delta.max, by = delta.by)
  n.analysis <- length(delta.vec)*length(analys)*(length(race_subgroup_analys)+2)
  int.dat <- data.frame(delta = rep(NA, n.analysis),
                        analysis = rep(NA, n.analysis),
                        subgroup = rep(NA, n.analysis),
                        pval = rep(NA, n.analysis),
                        W = rep(NA, n.analysis),
                        EW = rep(NA, n.analysis),
                        VW = rep(NA, n.analysis))
  
  i = 1
  
  for(delt in delta.vec){
    
    tstat.list <- lapply(analys, function(x){test_stat_wrapper(dat = data, mort_name = out_name, var_names = var.names,
                                                               delta = delt, m.dat.list = m.dat.list., form = forml,
                                                               race_analyses = race_subgroup_analys, subgroup = sub, overall = overl,
                                                               age_analysis = age_analys, alt.mort = alt_mort
    )})
    names(tstat.list) <- analys
    
    for(analysis in analys){
      
      print(paste("The current analysis is:", analysis, delt))
      
      temp.vec <- tstat.list[[analysis]]$adjusted_p[[2]]
      
      
      if(sub){
        groups <- "subgroup"
      }
      
      if(overl){
        groups <- c("overall", groups)
      }
      
      for(group in groups){
        
        int.dat$delta[i] <- delt
        int.dat$analysis[i] <- analysis
        int.dat$subgroup[i] <- group
        int.dat$pval[i] <-  tstat.list[[analysis]][[group]]$pval
        
        int.dat$W[i] <-  tstat.list[[analysis]][[group]]$W
        int.dat$EW[i] <- tstat.list[[analysis]][[group]]$cumulants[1]
        int.dat$VW[i] <- tstat.list[[analysis]][[group]]$cumulants[2]
        
        i = i + 1
      }
    }
    
    
  }
  
  return(int.dat)
  
}