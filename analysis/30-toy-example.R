##' title: "Protocol Figures"
##' output: github_document
##' 
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "car", 
             "xtable", "lubridate", "maps", "mapproj", "ggplot2")  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' Pulling the data for adjacent and other close matches to create a toy example

load("../data/temp/all_wonder_res.Rdata")
load(file.path(path,"out_mod_dat_2014.Rdata"))

## county name xwalk
name.xwalk <- mod_2014 %>%
  ungroup() %>%
  dplyr::select(FIPS, stateName, cntyName) %>%
  distinct() 

##' read in the match names for the adjacent county matches

adj.matches <- read.csv("figures/adj_matched_county_lists.csv") 
close.matches <- read.csv("figures/close_matched_county_lists.csv") 

adj.close.dat <- dat.2014.wonder %>%
  filter(matches %in% c(adj.matches$matches.final, close.matches$matches.final)) %>%
  arrange(matches) %>%
  dplyr::select(matches, everything())
  
ex.dat <- adj.close.dat %>%
  filter(matches %in% c(1.1092, 1.1012, 1.1176, 1.1169, 1.93, 1.995, 1.1043, 1.598))

ex.dat.clean <- ex.dat %>%
  dplyr::select(matches, treat, s, everything(),
                -lag, -age, -race, -trimmed, -starts_with("log")) %>%
  left_join(name.xwalk, by = "FIPS") %>%
  mutate(ac_rate = mort_dmf/pop)

write.csv(ex.dat.clean, file= "figures/toy-example.csv")


##' get predictions, etc for toy example

out <- test_stat_wrapper(delta = 1, dat = dat.2014.wonder, form = form, mort_name = "mort_hca_wndr",
                         var_names = vars.wonder, m.dat.list = m.dat.list)

inf.dat <- data.frame(FIPS = out$overall$Fips, 
                      mort = out$overall$mort,
                      pred = out$overall$pred
)

ex.dat.res <- ex.dat.clean %>%
  select(FIPS, matches, cntyName, stateName, treat, s, mort_hca_wndr) %>%
  left_join(inf.dat, by = "FIPS") %>%
  left_join(m.dat.list$all, by = "FIPS") %>%
  select(-mort_hca_wndr) %>%
  mutate(r = case_when(is.na(mort) ~ 0,
                       TRUE ~ log(mort)-log(pred))) %>%
  group_by(matches) %>%
  mutate(mean_r = mean(r)) %>%
  ungroup()%>%
  mutate(e = r - mean_r,
         c = as.numeric(is.na(mort)))

# calculate qs
N <- nrow(ex.dat.res)

m_mat <-  ex.dat.res$m %*% t(ex.dat.res$m)
e_mat <- ex.dat.res$e %*% t(rep(1, N))
c_mat <- ex.dat.res$c %*% t(rep(1, N))

# If both are censored, then they are tied. Having a 1 for both the positive and negative matrices cancel out as a tie
part_ord_pos <- (c_mat == 1 & t(c_mat) == 0) | (e_mat >= t(e_mat) & c_mat == 0 & t(c_mat) == 0) | (c_mat == 1 & t(c_mat) == 1)
part_ord_neg <- (c_mat == 0 & t(c_mat) == 1) | (e_mat <= t(e_mat) & c_mat == 0 & t(c_mat) == 0) | (c_mat == 1 & t(c_mat) == 1)

#quick check that logic is right
#check <- which(part_ord_pos == part_ord_neg)
#sum((e_mat[check] != t(e_mat)[check])& (c_mat[check] != 1 & t(c_mat)[check] != 1))

part_ord <- part_ord_pos - part_ord_neg

# Calculate q

q_mat <- m_mat * part_ord

#calculate w
#if not a county with characteristic of interest, replace sum with 0
ex.dat.res$q <- rowSums(q_mat)

sum(ex.dat.res$treat * ex.dat.res$q)


ex.dat.res.clean <- read.xlsx("figures/toy example table.xlsx", sheetName = "tab_clean_match") %>%
  select(match, cntyName, stateName) %>%
  left_join(ex.dat.res, by = c("cntyName", "stateName")) %>%
  group_by(match) %>%
  mutate(K = as.numeric(sum(s)>0)) %>%
  select(match, cntyName, stateName, treat, s, K, mort, pred, e, m, q)

write.csv(ex.dat.res.clean, file= "figures/toy-example-res.csv")


