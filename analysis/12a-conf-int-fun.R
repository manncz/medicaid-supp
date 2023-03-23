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
