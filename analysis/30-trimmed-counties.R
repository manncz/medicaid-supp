##'--
##' title: "EDA for Trimmed Counties"
##' output: github_document
##' --
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)
mypacks <- c("dplyr","tidyr", "readr", "RColorBrewer",
             "xtable", "lubridate", "maps", "mapproj", "ggplot2", "xlsx"
)  # what packages are needed?
lapply(mypacks, library, character.only=TRUE)  # load all packages

##' Load data
load("../data/mod.dat.Rdata")
load("../data/temp/mod.dat.no.excl.Rdata")

matchxwalk <- mod.dat %>%
  select(FIPS, matches.final)

dat <- mod.dat.no.exclusions %>%
  left_join(matchxwalk, by = "FIPS")  %>%
  mutate(excluded = case_when(is.na(matches.final) ~ 1,
                              TRUE ~ 0)) %>%
  mutate(reason = case_when(FIPS %in% mod.dat$FIPS & excluded == 1 ~ "trimmed",
                            FIPS %in% mod.dat$FIPS & excluded == 0 ~ "",
                            TRUE ~ "excluded"))

##' # of expansion and non-expansion counties in trimmed

dat %>%
  ungroup() %>%
  mutate(poptrimmed = excluded*adult_w_a_cnt) %>%
  group_by(mdcdExp) %>%
  summarize(n_county = n(), ntrim = sum(excluded),
            pop = sum(adult_w_a_cnt), poptrimmed=sum(poptrimmed)) %>%
  mutate(prop_trimmed = ntrim / n_county, prop_pop = poptrimmed/pop)

##' What states do they come from?
  
dat %>%
  ungroup() %>%
  mutate(poptrimmed = excluded*adult_w_a_cnt) %>%
  summarize(pop = sum(adult_w_a_cnt), poptrimmed=sum(poptrimmed)) %>%
  mutate(prop_pop = poptrimmed/pop)
  
pop_trimmed <- dat %>% group_by(stateName, mdcdExp) %>%
  mutate(poptrimmed = excluded*adult_w_a_cnt) %>%
  summarize(n_county = n(), ntrim = sum(excluded),
            pop = sum(adult_w_a_cnt), poptrimmed=sum(poptrimmed)) %>%
  filter(ntrim > 0) %>%
  mutate(prop_trimmed = ntrim / n_county, prop_pop = poptrimmed/pop) %>%
  arrange(-prop_pop)


pop_trimmed_clean <- pop_trimmed %>%
  mutate(treat = case_when(mdcdExp == 1 ~ "T",
                           TRUE ~ "C"),
         prop_trimmed = prop_trimmed*100,
         prop_pop = prop_pop*100,
         pop = pop/1000000) %>%
  select(stateName, treat, n_county, pop, prop_trimmed, prop_pop) %>%
  arrange(-prop_trimmed)
  
xtable <- xtable(pop_trimmed_clean, caption = "", table.placement = "ht",
                 digits = c(0,0,0, 0,2,1,1))
print(xtable, comment = F, size="footnotesize", include.rownames =F,
      file='../paper/protocol/figures/excl_counties.tex')


##' Looking at CA and MA counties

ca <- dat %>%
  filter(stateName == "California" & excluded == 1)

ma <- dat %>%
  filter(stateName == "Massachusetts" & excluded == 1)


##' dist of covariates in trimmed versus non-trimmed

num.vars <- names(dat)[4:46]

pdf(file = paste0("figures/trimmed/trimmed_dist.pdf"), height = 8.5, width = 11)

for(var in num.vars){
  
  g <- ggplot(data = dat, aes(x = factor(excluded), y = eval(parse(text=var)))) +
    geom_boxplot() +facet_grid(~mdcdExp) +
    ylab(var)
  print(g)
}

dev.off()

pdf(file = paste0("figures/trimmed/trimmed_dist_ca.pdf"), height = 8.5, width = 11)

for(var in num.vars){
  
  g <- ggplot() +
    geom_boxplot(data = dat, aes(x = factor(excluded), y = eval(parse(text=var)))) + 
    geom_point(data = dat %>% filter(stateName=="California"), aes(x = factor(excluded), y = eval(parse(text=var))), color="#fc8d59")+
    facet_grid(~mdcdExp) +
    ylab(var)
  print(g)
}

dev.off()

pdf(file = paste0("figures/trimmed/trimmed_dist_ma.pdf"), height = 8.5, width = 11)

for(var in num.vars){
  
  g <- ggplot() +
    geom_boxplot(data = dat, aes(x = factor(excluded), y = eval(parse(text=var)))) + 
    geom_point(data = dat %>% filter(stateName=="Massachusetts"), aes(x = factor(excluded), y = eval(parse(text=var))), color="#fdae61")+
    facet_grid(~mdcdExp) +
    ylab(var)
  print(g)
}

dev.off()

