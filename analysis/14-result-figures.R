##' ---
##' title: "Result Figures"
##' output: github_document
##' ---
##' 
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
cleanupunderway  <- TRUE
### library(survey) # If loading, this comes first. Avoids masking tidyverse functions.
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)

##' Load data

load("../data/temp/interval.dat.0.15_05.24.Rdata")
load("../data/temp/2014.treat.est.Rdata")

##' This is the hacky way to get the confidence intervals and estimates from the grid data, to use in 
##' the meanwhile while I get uniroot to fully work.
int.dat <- int.dat.0.15

conf.ints <- int.dat %>%
  mutate(pval = case_when(subgroup %in% c("subgroup", "overall") ~ pval.noadj,
                          TRUE ~ pval.maxtadj)) %>%
  filter(pval > .05) %>%
  group_by(analysis, subgroup) %>%
  dplyr::select(delta, analysis, subgroup, pval) %>%
  mutate(min_delta = min(delta),
         max_delta = max(delta)) %>%
  filter(delta == min_delta | delta == max_delta) %>%
  arrange(analysis, subgroup, delta) %>%
  mutate(min_max = case_when(delta == min_delta ~ "min_pval",
                             TRUE ~ "max_pval")) %>%
  dplyr::select(-delta)%>%
  pivot_wider(values_from = pval, names_from = min_max) %>%
  mutate(min_delta = case_when(min_delta < .2 ~ .2,
                               TRUE ~ min_delta),
         max_delta = case_when(max_delta > 5 ~ 5,
                               TRUE ~ max_delta))

ests <- conf.ints %>%
  left_join(bound.dat, by = c("analysis", "subgroup")) %>%
  mutate(sig = case_when((max_delta - 1)*(min_delta - 1) > 0 ~ "significant",
                            TRUE ~ "")) %>%
  mutate(min_delta = case_when(est < min_delta ~ est,
                               TRUE ~ min_delta))


ests$subgroup <- factor(ests$subgroup, levels = c("overall", "subgroup",
                                                  "Non-Hispanic White", "Non-Hispanic Black",
                                                  "Non-Hispanic Asian or Pacific Islander",
                                                  "Non-Hispanic AI or AN",
                                                  "Hispanic"),
                        labels = c("Overall", "Supermajority White Counties",
                                   "Non-Hispanic White", "Non-Hispanic Black",
                                   "Non-Hispanic Asian or Pacific Islander",
                                   "Non-Hispanic American Indian or Alaskan Native",
                                   "Hispanic"))

ests$analysis <- factor(ests$analysis, levels = c("all_cause", "hca", "flu", "opioid"),
                        labels = c("All Cause", "Healthcare Amenable", "Flu", "Opioid"))

save(ests, file = "../data/temp/ci.plot.dat_05.24")

##' The following was used when using the grid data to estimate the treatment effect
# ests <- int.dat %>%
#   group_by(analysis, subgroup) %>%
#   mutate(max.pval = max(pval.noadj)) %>%
#   filter(pval.noadj == max.pval) %>%
#   select(analysis, subgroup, est = delta) %>%
#   left_join(conf.ints, by = c("analysis", "subgroup")) %>%
#   mutate(sig = case_when((max_delta - 1)*(min_delta - 1) > 0 ~ "significant",
#                          TRUE ~ ""))

#+ fig.width=10, fig.height=10
g1 <- ggplot(ests, aes(x = subgroup, y = est, color = sig)) +
  geom_abline(intercept = 1, slope = 0, color = "#2171b5", linetype = "dotted") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = max_delta, ymin = min_delta), width = .2, size = .5) +
  facet_wrap(~ analysis, ncol = 1) +
  coord_flip() +
  scale_color_manual(name = "", values = c("black", "#41b6c4")) +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  xlab("") + ylab("Estimate")
g1

png("figures/CI_all_08.09.png", width = 8, height = 6, units = 'in', res = 300)
g1
dev.off()

g2 <- ggplot(ests %>% filter(subgroup %in% c("Overall", "Supermajority White Counties",
                                       "Non-Hispanic White")), aes(x = subgroup, y = est, color = sig)) +
  geom_abline(intercept = 1, slope = 0, color = "#2171b5", linetype = "dotted") +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = max_delta, ymin = min_delta), width = .2, size = .5) +
  facet_wrap(~ analysis, ncol = 1) +
  coord_flip() +
  scale_color_manual(name = "", values = c("black", "#41b6c4")) +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none") +
  xlab("") + ylab("Estimate")
g2

png("figures/CI_overall_08.09.png", width = 6, height = 4, units = 'in', res = 300)
g2
dev.off()


#rmarkdown::render("21-result-figures.R", "github_document")