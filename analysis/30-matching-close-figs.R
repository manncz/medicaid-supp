##' ---
##' title: "Matching Closeness Figures"
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
library(xtable)
library(ggplot2)

##' Load all of the required data

load("../data/temp/matching.adj.dat.Rdata")
load("../data/adj.matches.final.Rdata")
load("../data/close.matches.final.Rdata")
load("../data/temp/all_wonder_res.Rdata")

match.xwalk <- dat.2014.wonder %>%
  select(FIPS,  matches = matches)

##' Create indicator of closest matches

row <- row.names(adj_mat1)
col <- colnames(adj_mat1)

adj_close <- adj_mat1 * close_mat %>%
  as.data.frame()

colnames(adj_close) <- col

adj_close <- adj_close %>%
  mutate(fipscounty = row) %>%
  pivot_longer(cols = all_of(col),
               names_to = "fipsneighbor",
               values_to = "close") %>%
  filter(close == 1)

##' Create a "pair" variable so that we can create lines between adjacent counties

adj.pairs <- pos_adj_countys %>%
  left_join(adj_close, by = c("fipscounty", "fipsneighbor")) %>%
  mutate(pair = row_number()) %>%
  group_by(fipscounty) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  group_by(fipsneighbor) %>%
  mutate(m = row_number()) %>%
  pivot_wider(names_from = c("n", "m"),
              values_from = c("fipsneighbor", "pair", "close"),
              names_glue = "{.value}_{n}{m}")

pairs.mdcd <- adj.pairs %>%
  select(FIPS = fipscounty, starts_with("pair"), starts_with("close"))

pairs.nomdcd <- adj.pairs %>%
  pivot_longer(fipsneighbor_11:fipsneighbor_34,
               names_to = c("var", "n"),
               values_to = "FIPS",
               names_sep = "_") %>%
  pivot_longer(pair_11:close_34,
               names_to = c("var2", "n2"),
               values_to = "val",
               names_sep = "_") %>%
  filter(!is.na(val) & !is.na(FIPS)) %>%
  filter(n2 == n) %>%
  select(FIPS, n2, val, var2) %>%
  pivot_wider(names_from = c("var2", "n2"),
              values_from = "val",
              names_glue = "{var2}_{n2}") %>%
  select(FIPS, starts_with("pair"), starts_with("close"))

colnames(pairs.nomdcd)
colnames(pairs.mdcd)

# a dataset that has pair indicators for all of the adjacent counties in addition to
# indicators if the pair was "particularly close" in terms of MH distance
pairs <- rbind(pairs.nomdcd, pairs.mdcd[,colnames(pairs.nomdcd)]) %>%
  arrange(FIPS)

##' #### plotting

trimmed <- dat.2014.wonder %>%
  select(FIPS) %>%
  mutate(orig = 1) %>%
  right_join(mod.dat[,1:3], by = "FIPS") %>%
  filter(is.na(orig))

colnames(mod.dat)

plot.dat <- mod.dat %>%
  left_join(pairs, by = "FIPS") %>%
  mutate(across(starts_with("close"), ~case_when(is.na(.x) ~ 0,
                                                 TRUE ~ as.numeric(.x))))


combos <- str_replace(colnames(plot.dat %>% select(starts_with("pair"))),"pair_", "")
combos_close <- str_replace(colnames(plot.dat %>% select(starts_with("close"))),"close_", "")
dif <- setdiff(combos, combos_close)

for(x in dif){
  print(x)
  plot.dat[,ncol(plot.dat)+1] <- rep(0, nrow(plot.dat))
  colnames(plot.dat)[ncol(plot.dat)] <- paste0("close_", x)
}

##' Save data

save(plot.dat, file = "../data/temp/adjacent.county.plot.dat.Rdata")

##' Create long data with an observation for every variable and pair number
plot.dat1 <- plot.dat %>%
  select(-mdcdExp2) %>%
  select(FIPS, stateName, cntyName, mdcdExp, everything()) %>%
  pivot_longer(adult_w_a_cnt:log_10_adult_w_a_cnt,
               names_to = "var",
               values_to = "val") %>%
  pivot_longer(pair_11:close_34,
               names_to = c("type", "n"),
               values_to = "pair_close",
               names_sep = "_") %>%
  filter(!is.na(pair_close)) %>%
  pivot_wider(names_from = "type",
              values_from = "pair_close") %>%
  filter(n == "11" | !is.na(pair)) %>%
  left_join(match.xwalk, by = "FIPS")

plot.dat1$treat <- factor(plot.dat1$mdcdExp, levels = c(0,1), labels = c("C","T"))

# add indicators for whether the match is close or adjacent
plot.dat1 <- plot.dat1 %>%
  mutate(adj.match = case_when(matches %in% factor(adj.matches.final$matches.final) ~1,
                               TRUE ~ 0),
         close.match = case_when(matches %in% factor(close.matches.pen.stab$matches.final) ~1,
                                 TRUE ~ 0))

##' Subset to some variables of interest

vars1 <- vars[c(1:4, 22, 24, 27, 29, 32, 34, 41)]
varnames1 <- var.names[c(1:4, 22, 24, 27, 29, 32, 34, 41)]
varnames1[8] <- "No Ins."
varnames1[9] <- "Smoker"
varnames1[10] <- "Diabetic"
varnames1[3] <- "Hispanic"
varnames1 <- str_replace(varnames1, "% ", "")

plot.dat2 <- plot.dat1 %>%
  filter(var %in% vars1)

plot.dat2$var <- factor(plot.dat2$var, levels = vars1,
                        labels = varnames1)

##' Define some macros so that they can be adjusted overall for all plots.
alf = .6
close.size = .6
oth.size = .3
close.col = "#225ea8"
oth.col = "#feb24c"


##' Plot for all adjacent counties

g <- ggplot(aes(x = treat, y = val), data = plot.dat2 %>% filter(n == 11)) +
  geom_boxplot(outlier.size = .7) +
  geom_point(data = plot.dat2 %>% filter(!is.na(pair) & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(!is.na(pair) & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_point(data = plot.dat2 %>% filter(!is.na(pair) & close == 1), color = close.col, size = close.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(!is.na(pair) & close == 1), color = close.col, size = close.size, alpha = alf) +
  theme(legend.position = "none")+
  xlab("Control or Treatment Group") +
  ylab("%") +
  facet_grid(~var)
g

png("figures/adj_counties_covs.png", width = 12, height = 4, units = 'in', res = 300)
g
dev.off()

##' Take a random sample to thin out a bit

pos.pairs <- unique(plot.dat2$pair)[-1]
set.seed(2787)
samp <- sample(pos.pairs, size = 30)

alf <- 1

g <- ggplot(aes(x = treat, y = val), data = plot.dat2 %>% filter(n == 11)) +
  geom_boxplot(outlier.size = .7) +
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(pair %in% samp & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 1), color = close.col, size = close.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(pair %in% samp & close == 1), color = close.col, size = close.size, alpha = alf) +
  theme(legend.position = "none")+
  xlab("Control or Treatment Group") +
  ylab("%") +
  facet_grid(~var)
g

png("figures/adj_samp_covs.png", width = 12, height = 4, units = 'in', res = 300)
g
dev.off()

##' Now actually matched adjacent matches

g <- ggplot(aes(x = treat, y = val), data = plot.dat2 %>% filter(n == 11)) +
  geom_boxplot(outlier.size = .7) +
  geom_point(data = plot.dat2 %>% filter(adj.match == 1 & close == 0 & n == 11), color = oth.col, size = oth.size) +
  geom_line(aes(group=factor(matches)), data = plot.dat2 %>% filter(adj.match == 1 &  close == 0 & n == 11), color = oth.col, size = oth.size) +
  geom_point(data = plot.dat2 %>% filter(adj.match == 1& close == 1 & n == 11), color = close.col, size = close.size) +
  geom_line(aes(group=factor(matches)), data = plot.dat2 %>% filter(adj.match == 1& close == 1 & n == 11), color = close.col, size = close.size) +
  theme(legend.position = "none")+
  xlab("Control or Treatment Group") +
  ylab("%") +
  facet_grid(~var)
g

png("figures/adj_matches_covs.png", width = 12, height = 4, units = 'in', res = 300)
g
dev.off()


##' Scale the variables to be more comparable

scaled_vars <- plot.dat %>%
  select(FIPS, mdcdExp, all_of(vars1)) %>%
  mutate(across(all_of(vars1), ~scale(.x))) %>%
  pivot_longer(all_of(vars1),
               names_to = "var",
               values_to = "val") %>%
  rename(val_scaled = val)

scaled_vars$var = factor(scaled_vars$var, 
                         levels = vars1,
                         varnames1)

plot.dat2 <- plot.dat2 %>%
  left_join(scaled_vars, by = c("FIPS", "mdcdExp", "var"))

save(plot.dat2, file = "../data/temp/close.match.plot.dat.Rdata")

##' This is the plot that is actually used in the paper

plot.dat2  <- plot.dat2 %>%
  filter(!(var %in% c("Male", "White")))

plot.dat2$var <- factor(plot.dat2$var,
                        levels = c("Urban", "Black", "Hispanic", "55-64",
                                   "Republican", "Poverty", "No Ins.",
                                   "Diabetic", "Smoker"),
                        labels = c("Urbanicity", "Black", "Hispanic", "Age 55-64",
                                   "Republican", "Poverty", "No Ins.",
                                   "Diabetic", "Smoker"))

g <- ggplot(aes(x = treat, y = val_scaled), data = plot.dat2 %>% filter(n == 11)) +
  geom_boxplot(outlier.size = .7,color = "dark grey") +
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(pair %in% samp & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 1), color = close.col, size = close.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(pair %in% samp & close == 1), color = close.col, size = close.size, alpha = alf) +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))+
  xlab("Control or Treatment Group") +
  ylab("Standard Deviations from Mean") +
  facet_grid(~var)
g

png("figures/scaled_samp_covs.png", width = 11, height = 5, units = 'in', res = 300)
g
dev.off()

adj.match.pair <- unique(plot.dat2$pair[plot.dat2$adj.match == 1])
samp.adj <- union(samp, adj.matches.final)

check <- plot.dat2 %>%
  filter(pair %in% samp) %>%
  group_by(matches)

plot.dat.samp <- plot.dat2 %>%
  filter(pair %in% samp | (adj.match == 1 & n == 11))

oth.col2 = "#fee090"
close.col2 = "#74add1"
alf2 = 1
match.size = .6
nomatch.size = .5


colors <- c("Other Adjacent Counties" = oth.col2, 
            "Close Adjacent Counties" = close.col2,
            "Matched Other Adjacent Counties" = oth.col, 
            "Matched Close Adjacent Counties" = close.col)


g <- ggplot(aes(x = treat, y = val_scaled), data = plot.dat2 %>% filter(n == 11)) +
  geom_boxplot(outlier.size = .7, color = "dark grey") +
  
  # other adjacent counties
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 0), aes(color = "Other Adjacent Counties"), size = nomatch.size, alpha = alf2) +
  geom_line(aes(group=factor(pair), color = "Other Adjacent Counties"), data = plot.dat2 %>% filter(pair %in% samp & close == 0), size = nomatch.size, alpha = alf2) +
  
  # other actual adjacent matches
  geom_point(data = plot.dat2 %>% filter(adj.match == 1 & close == 0 & n == 11), aes(color = "Matched Other Adjacent Counties"), size = match.size) +
  geom_line(aes(group=factor(matches),color = "Matched Other Adjacent Counties"), data = plot.dat2 %>% filter(adj.match == 1 &  close == 0 & n == 11),  size = match.size) +
  
  # close non-match adjacent counties
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 1), aes(color = "Close Adjacent Counties"), size = nomatch.size, alpha = alf2) +
  geom_line(aes(group=factor(pair), color = "Close Adjacent Counties"), data = plot.dat2 %>% filter(pair %in% samp & close == 1),  size = nomatch.size, alpha = alf2) +
  
  # close actual adjacent matches
  geom_point(data = plot.dat2 %>% filter(adj.match == 1& close == 1 & n == 11), aes(color = "Matched Close Adjacent Counties"), size = match.size) +
  geom_line(aes(group=factor(matches), color = "Matched Close Adjacent Counties"), data = plot.dat2 %>% filter(adj.match == 1& close == 1 & n == 11), size = match.size) +
 
  #formatting
  scale_color_manual(values = colors) +
  theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 12)
        )+
  xlab("Control or Treatment Group") +
  ylab("Standard Deviations from Mean") +
  
  #facet by variable
  facet_grid(~var)
g

png("figures/adj_matches_scaled_covs.png", width = 11, height = 5, units = 'in', res = 300)
g
dev.off()


##' Now look at this for the toy example counties
##' 
# 
# toy <-  c(1.1092, 1.1012, 1.1176, 1.1169, 1.93, 1.995, 1.1043, 1.598)
# 
# g <- ggplot(aes(x = treat, y = val), data = plot.dat1) +
#   geom_boxplot() +
#   geom_point(data = plot.dat1 %>% filter(matches %in% toy), aes(color = matches)) +
#   geom_line(aes(group=matches, color = matches), data = plot.dat1 %>% filter(matches %in% toy)) +
#   #
#   theme(legend.position = "none") +
#   facet_grid(~var)
# g
# 
# 
# g <- ggplot(aes(x = treat, y = val), data = plot.dat1) +
#   geom_boxplot() +
#   geom_point(data = plot.dat1 %>% filter(as.character(matches) %in% as.character(adj.matches$matches.final)), aes(color = matches)) +
#   geom_line(aes(group=matches, color = matches), data = plot.dat1 %>% filter(as.character(matches) %in% as.character(adj.matches$matches.final))) +
#   #theme(legend.position = "none") +
#   facet_grid(~var)
# g

##' Estimate scaled average differences after matching
##' 

##' Create an xwalk for the treatment assignment and weights based on matches to calculate
##' thungs after matching
weights <- dat.2014.wonder %>%
  dplyr::select(FIPS, treat, matches) %>%
  group_by(matches, treat) %>%
  mutate(n = n()) %>%
  group_by(matches) %>%
  mutate(n_treat = sum(treat)) %>%
  ungroup() %>%
  mutate(w = 1/n * n_treat) %>%
  select(FIPS, matches, w)

matched.avg <- plot.dat2 %>%
  filter(!is.na(matches)) %>%
  left_join(weights, by = c("FIPS","matches")) %>%
  mutate(valsw = val_scaled*w,
         valw = val*w) %>%
  group_by(var, treat) %>%
  summarize(avg_val_scaled = sum(valsw)/sum(w),
            avg_val = sum(valw)/sum(w))
  
  
g <- ggplot(aes(x = treat, y = val_scaled), data = plot.dat2 %>% filter(n == 11)) +
  geom_boxplot(outlier.size = .7,color = "dark grey") +
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(pair %in% samp & close == 0), color = oth.col, size = oth.size, alpha = alf) +
  geom_point(data = plot.dat2 %>% filter(pair %in% samp & close == 1), color = close.col, size = close.size, alpha = alf) +
  geom_line(aes(group=factor(pair)), data = plot.dat2 %>% filter(pair %in% samp & close == 1), color = close.col, size = close.size, alpha = alf) +
  geom_point(data = matched.avg, aes(x = treat, y = avg_val_scaled), color = "#A50026", size = 1, shape = 17) +
  geom_line(data = matched.avg, aes(group = factor(var), x = treat, y = avg_val_scaled), color = "#A50026")+
  theme(legend.position = "none",
        strip.text.x = element_text(size = 15),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 15))+
  xlab("Control or Treatment Group") +
  ylab("Standard Deviations from Mean") +
  facet_grid(~var)
g

png("figures/scaled_samp_covs_w_overall.png", width = 11, height = 5, units = 'in', res = 300)
g
dev.off()