#'
#'A function to find outliers, defined as counties at least len.iqr
#'IQRs away from the closest county with the opposite treatment status,
#' for a given variable.

outlier_find = function(df,var,len.iqr){
  
  # Filter out unnecessary variables and divide into treatment and control
  df %>% 
    dplyr::select(cntyName,stateName,mdcdExp,adult_w_a_cnt,all_of(var)) -> df_sub
  df_sub %>%
    filter(mdcdExp==1) -> df_t
  df_sub %>%
    filter(mdcdExp==0) -> df_c
  # Calculate the minimum distance for each county
  dists = calc_dists(df_c[,5],df_t[,5])
  df_c = cbind(df_c,dists[[1]])
  df_t = cbind(df_t,dists[[2]])
  
  # Determine whether the minimum distance is greater than len.iqr IQRs away
  df_c %>%
    filter(min_dist > len.iqr*IQR(!!sym(var))) %>%
    mutate(variable = var) %>%
    rename(value = !!sym(var)) -> outlier_c
  df_t %>%
    filter(min_dist > len.iqr*IQR(!!sym(var))) %>%
    mutate(variable = var) %>%
    rename(value = !!sym(var)) -> outlier_t
  
  outliers = rbind(outlier_c,outlier_t)
  
  return(outliers)
}

#'
#'A function to calculate the minimum distance from each county
#'to a county with the opposite treatment status.

calc_dists = function(v1,v2){
  min_dist1 = rep(0,length(v1))
  for(i in 1:length(v1)){
    min_dist1[i] = min(abs(v1[i] - v2))
  }
  min_dist2 = rep(0,length(v2))
  for(i in 1:length(v2)){
    min_dist2[i] = min(abs(v2[i] - v1))
  }
  return(list(data.frame(min_dist = min_dist1),data.frame(min_dist=min_dist2)))
}
