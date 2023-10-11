##' ---
##' title: "Matching and Diagnostics"
##' output: github_document
##' ---
##'
#+ echo=FALSE, include = FALSE
knitr::opts_chunk$set(warning=FALSE, echo=TRUE)

mypacks <- c("dplyr","tidyr", "readr","optmatch", "car", 
             "xtable", "lubridate", "maps", "mapproj", "ggplot2",
             "kableExtra", "arm", "RColorBrewer", "survey",  "tibble")  # what packages are needed?
source("../R/helpers_from_development_optmatch.R")
source("00-outliers-helpers.R")
source("00-balance-helpers.R")

# create a function that installs packages if they aren't already installed
load_fun <- function(pack){
  out <- require(pack, character.only=TRUE)
  if(out == F){
    install.packages(pack, lib.loc = lib2)
    library(pack, character.only=TRUE, lib =)
  }
}
lapply(mypacks, load_fun)  # load all packages

save_me <- FALSE
##' This code prepares the baseline data for county matching and runs matching procedues.
##' 
##' ## Data

#read in full pre-treatment dataset
base.dat <- read.csv("../data/base_cnty_all.csv", colClasses=c(FIPS="character"))

#read in county adjacency data
cnty_adj <- read.csv("../data-raw/county_adjacency2010.csv", colClasses=c(fipscounty="character", fipsneighbor="character"))
  
#read in standard deviation calculations from 03a
load("../data/temp/pooled.sd1.v2.Rdata")
load("../data/temp/pooled.sd2.v2.Rdata")

##' To make things easier, subset to the variables of interest for checking balance and for matching.
##' We also remove Alaska and Hawaii at this point and define our outcome as whether expansion occured before July 2014.

#subsetting to variables to balance on
mod.dat1  <- base.dat %>%
  filter(!is.na(a20_34))%>% #take out counties for now that aren't in CDC mort data
  dplyr::select(FIPS, stateName, cntyName, dateExp, yearExp, adult_w_a_cnt, white_race, black_race, latino, male, 
         mortAC_20_34:mortAC_20_64, contains("mortACWhite"), contains("mortACBlack"),
         contains("mortACother"), mortHC_amenable_not_flu, mortOpioid, mortFlu,
         a20_34:a55_64, pctUrban_2010,  medIncome_2013, pctPovty_2013,
         unplmtRate_2013, popDens_2010, smk_tot_2012, pctRep_2012, avgPM25_2011,
         vetPop_2013, snap_2013, pctNoIns_2013, alc_2012,
         diabetes_2012, hyper_male_2009:phys_act_female_2011, calc_multi_house) %>%
  mutate(mdcdExp_det = ifelse(is.na(dateExp), 0, ifelse(ymd(dateExp) < ymd("2014-07-01"), 1, yearExp)),
         avgPM25_2011 = avgPM25_2011/100) %>%
  dplyr::select(-yearExp) %>%
  filter(!(stateName %in% c("Alaska", "Hawaii")))

##' ### Cleaning
##' States that expanded by June 2014 as well as Wisconsin are in treatent group one and states that
##' expanded by January 2020 are added in addition for treatment group 2.

mod.dat <- mod.dat1 %>% 
  mutate(mdcdExp = case_when(mdcdExp_det > 1 ~ 0, 
                             stateName == "Wisconsin" ~ 1,
                             TRUE ~ mdcdExp_det),
         mdcdExp2 = case_when(mdcdExp_det > 1 |  stateName == "Wisconsin" ~ 1, 
                              TRUE ~ mdcdExp_det)) %>%
  dplyr::select(-mdcdExp_det, -dateExp)


##' Some functions (e.g. `svyglm()` need the log transform done in advance)

mod.dat <- mod.dat %>% 
  mutate(log_adult_w_a_cnt = log(adult_w_a_cnt), 
                              log_10_adult_w_a_cnt = log10(adult_w_a_cnt))


#this helps with matching with adjacency matrix
rownames(mod.dat) <- mod.dat$FIPS

##' Bedford City, VI is missing SNAP and diabetes, so is excluded from the analysis
#exclude these counties
cc <- complete.cases(mod.dat)
mod.dat <- mod.dat %>% filter(cc)

##' ### Remove Outliers
##' Find and outliers outside of 1.5 the IQR for each variable
outliers = data.frame(cntyName = character(),
                    stateName = character(),
                    mdcdExp = numeric(),
                    log_adult_w_a_cnt = numeric(),
                    value = numeric(),
                    min_dist = numeric(),
                    variable = character())
# Loop through variables finding outliers. Note that this is currently
# removing counties in excl.cnty1.
for(var in vars){
  temp_df = outlier_find(mod.dat,var,1.5)
  outliers = rbind(outliers,temp_df)
}

outliers = outliers[,c(1,2,3,7,5,6,4)]
#write.csv(outliers, file="../data/temp/outliers.csv")
outliers <- read.csv("../data/temp/outliers.csv")

##' Exclude mortality variable outlying counties

exclusions <- outliers %>%
  filter(variable %in% c("mortHC_amenable_not_flu", "mortOpioid", "mortFlu")) %>%
  left_join(sd.calc1, by = c("variable" = "var")) %>%
  mutate(n.sd = min_dist / pooled_sd) %>%
  filter(n.sd > 2) %>%
  mutate(excl = 1) %>%
  dplyr::select(cntyName, stateName, excl)

exclusions

##' Save the data with out these exclusions first
mod.dat.no.exclusions <- mod.dat

#save(mod.dat.no.exclusions, file = "../data/temp/mod.dat.no.excl.Rdata")

mod.dat <- mod.dat %>%
  left_join(exclusions, by = c("cntyName", "stateName")) %>%
  filter(is.na(excl)) %>%
  dplyr::select(-excl)
  
##' ###  Matching variables
##'
##+
form0  <- mdcdExp ~ log_adult_w_a_cnt +
                     white_race+black_race+latino + male+
                     a20_34+a35_44+a45_54+a55_64+ mortAC_20_64+
                     mortAC_20_34 + mortAC_35_44 + mortAC_45_54 + mortAC_55_64 +
                     mortACWhite_M+mortACWhite_F+mortACBlack_M+mortACBlack_F+
                     mortACother_M+mortACother_F+ 
                     mortHC_amenable_not_flu + mortOpioid + mortFlu+
                     popDens_2010+pctUrban_2010+vetPop_2013+medIncome_2013+
                     pctPovty_2013 + snap_2013 + pctNoIns_2013+unplmtRate_2013+
                     avgPM25_2011+smk_tot_2012+alc_2012+
                     diabetes_2012 + hyper_male_2009 +
                     hyper_female_2009 + obsty_male_2011 +
                     obsty_female_2011 + phys_act_male_2011 + 
                     phys_act_female_2011 + pctRep_2012 + calc_multi_house


##' ## Penalties

##' ### County Adjacency

adj_mat  <- matrix(FALSE, nrow=nrow(mod.dat), ncol=nrow(mod.dat))
dimnames(adj_mat)  <- list(mod.dat$FIPS, mod.dat$FIPS)

cdc_fips <- mod.dat$FIPS
adj_fips <- unique(cnty_adj$fipscounty)

#all nonoverlapping counties are in AK, HI and territories
cdc_fips[which(!(cdc_fips %in% adj_fips))]
ex.fips <- adj_fips[which(!(adj_fips %in% cdc_fips))]

cnty_adj <- cnty_adj %>%
  filter(!(fipscounty %in% ex.fips) & !(fipsneighbor %in% ex.fips)) %>%
  dplyr::select(fipscounty, fipsneighbor)
cnty.adj <- as.matrix(cnty_adj)

adj_mat[cnty.adj] <- TRUE
adj_mat <- adj_mat | t(adj_mat)

#now subset to the rows that are treatment and columns being control
medicaid_expanded <- mod.dat$FIPS[mod.dat$mdcdExp == 1]
medicaid_no_exp <- cdc_fips[!(cdc_fips %in% medicaid_expanded)]

adj_mat1 <- adj_mat[medicaid_expanded, medicaid_no_exp]
dim(adj_mat1)

#there are 522 possible pairs of adjacent counties between treatment and control which includes
# 575 distinct counties (251 treatment and 259 control)
pos_adj_countys <- cnty_adj %>% 
  filter(fipscounty %in% medicaid_expanded & !(fipsneighbor %in%  medicaid_expanded))

c(treatment=length(unique(pos_adj_countys$fipscounty)),
  control=length(unique(pos_adj_countys$fipsneighbor))
  )

##' ### Calculate average distances for adjacent counties

##' Create a matrix of MH distances on all PSM variables and calculate a lower percentile of MH between 
##' adjacent counties to consider close.

all_mh <- match_on(form0, data = mod.dat)

#pull out the MH distances between adjacent counties
adj_dist <- as.numeric(all_mh)[adj_mat1]
cutoff <-quantile(adj_dist, probs = .2)
summary(adj_dist)
summary(as.numeric(all_mh))

#matrix indicating if distance is close like adjacent counties (bottom quantile)
close_mat <- as.matrix(all_mh < cutoff)
dim(close_mat)

save(close_mat, adj_mat1, pos_adj_countys, vars, var.names, mod.dat, file = "../data/temp/matching.adj.dat.Rdata")

##' We are interested in setting up a penalty on the distance that will penalize matching
##' counties that fulfill none of:
##' 
##' 1. adjacency, 
##' 2. well-matched from close states, 
##' 3. well matched from far states.
##' 
##' This boils down to penalizing counties that are not adjacent and penalizing counties
##' that are not close on the Mahalanobis distance - which I chose to penalize less than adjacency.
##' Thus, counties that are adjacent will have a penalty of 0, counties that are close on MD but 
##' not adjacent will have a penalty of .5 and all others will have a penalty of 1. This will be multiplied
##' by a factor less than 1 later to scale appropriately to the distances calculated from the PSM.

penalty_mat <- 1 - .5*(close_mat)
penalty_mat[adj_mat1] <- 0

##' ## Propensity Score Model


##' fit propensity score model, using survey package for frequency weighting by population size.
surv.d1 <- survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=mod.dat)
psm <- survey::svyglm(form0,
                      design=surv.d1,
                      family=quasibinomial())


##' Score overlap: 

#evaluate score overlap
#should we exclude those outside of overlap?
boxplot(psm, main = "Overlap on Fitted Scores", xlab = "Treatment or Control Municipalities", ylab = "Propensity Score")

##' ## Matching

##' ### Matching with the propensity score
##'
##' We use development code from optmatch in order to calculate
##' the pooled s.d. of propensity scores with attention to survey weights. 
pooled_sd <-  standardization_scale(psm$linear.predictor,
                                    psm$y, svy_sd,
                                    svydesign_=surv.d1)
pooled_sd

##' Full match with a caliper
dist <- match_on(psm$linear.predictor, z=psm$y, data=mod.dat,
                 caliper=.25*pooled_sd)
summary(dist)

ps.pm.calip.25 <- fullmatch(dist, data = mod.dat)
summary(ps.pm.calip.25)

##' Check out excluded counties for just the standard caliper
##' 
excl.cnty.calip <- which(is.na(ps.pm.calip.25))
length(excl.cnty.calip) # no. of counties excluded

with(mod.dat,
     sum(adult_w_a_cnt[excl.cnty.calip])/sum(adult_w_a_cnt)
     )
summary(mod.dat$adult_w_a_cnt[excl.cnty.calip])
excluded.counties <- mod.dat[excl.cnty.calip,] %>%
  dplyr::select(FIPS, stateName, cntyName, mdcdExp, adult_w_a_cnt)
excluded.counties

##' Now we update propensity score and distance, excluding counties outside of the PS caliper. 

surv.d2 <- survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=mod.dat[-excl.cnty.calip,])
psm2 <- survey::svyglm(form0,
                      design=surv.d2,
                      family=quasibinomial())

##' Account for missing values.
tmp  <- rep(NA_real_, nrow(mod.dat))
tmp[-excl.cnty.calip] <- psm2$linear.predictors
pscore2 <- tmp
table(is.na(pscore2))

mod.dat$pscore2 <- pscore2
if (save_me) save(mod.dat, file = "../data/temp/pscores.Rdata")

##' Updated distance

dist.update <- match_on(mdcdExp ~ pscore2, dat = mod.dat) + caliper(dist, .25*pooled_sd) 

summary(as.numeric(dist.update))
summary(dist.update)

##' full matched with penalty
ps.penal.calip <- fullmatch(dist.update + .1*penalty_mat, data=mod.dat)
summary(ps.penal.calip)

##' full matched with penalty and stability increment
dist.update.stab <- dist.update + .1
summary(as.numeric(dist.update.stab))
ps.penal.calip.stab <- fullmatch(dist.update.stab + .12*penalty_mat, data=mod.dat)
summary(ps.penal.calip.stab)

if (same_me) save(ps.penal.calip, ps.penal.calip.stab, file= "../data/temp/matches.Rdata")

##' ## Balance Diagnostics
##' We will look at diagnostics for our two matching structures - 
##' 
##' 1. Full matching with a penalty on non-adjacency or not being close on the Mahalanobis distance and a propensity score caliper
##' 2. Restricted full matching with a penalty on non-adjacency or not being close on the Mahalanobis distance and a propensity score caliper with a stability increment
##+ echo=FALSE
form1 <- update.formula(form0, 
                        . ~ . - log_adult_w_a_cnt + log_10_adult_w_a_cnt)
form2 <- update.formula(form1, mdcdExp2 ~.)

##' First we look at balance considering test and control at 2014. 
myb.ps.penal.calip <- get.balTest(match = "ps.penal.calip")
myb.ps.penal.calip.stab <- get.balTest(match = "ps.penal.calip.stab")

##' Balance not including the stability increment:
data.frame(myb.ps.penal.calip$overall)
##' Balance including the stability increment:
data.frame(myb.ps.penal.calip.stab$overall)

##' Next we consider balance with test and control in 2020. The overall
##' p-value is expected to be larger when considering the new test and
##' control (binary) outcome measure. After changing the treatment indicator,
##' many of the matched sets lack either control or treatment units, and
##' consequently make no contribution to the imbalance test statistic. 
suppressWarnings(myb.ps.penal.calip2 <- get.balTest(match = "ps.penal.calip", form = form2))
suppressWarnings(myb.ps.penal.calip.stab2 <- get.balTest(match = "ps.penal.calip.stab", form = form2))

##' Balance not including the stability increment:
data.frame(myb.ps.penal.calip2$overall)
##' Balance including the stability increment:
data.frame(myb.ps.penal.calip.stab2$overall)

##' ### Love Plots

#+ include = FALSE

##' You can review the love plots in analysis/figures.
#+ include = FALSE
#save love plots
love.plot(myb.ps.penal.calip, "psm_cnty_penal_caliper", rownames.xwalk = var.labels.xwalk)
love.plot(myb.ps.penal.calip.stab, "psm_cnty_penal_caliper_cont", rownames.xwalk = var.labels.xwalk)

love.plot(myb.ps.penal.calip2, "psm_cnty_penal_caliper2", rownames.xwalk = var.labels.xwalk)
love.plot(myb.ps.penal.calip.stab2, "psm_cnty_penal_caliper_cont2", rownames.xwalk = var.labels.xwalk)

##' ### Balance Tables

##' ### 2014 Treatment

##' Most of the standardized differences are above .25 before matching and are significant.

bal.unadj <- bal.table(myb.ps.penal.calip, adj=FALSE, rownames.xwalk = var.labels.xwalk, standard.errs = sd.calc1)
bal.unadj$kbl

##' Looking at our full match with penalties but not stability increments, we hope 
##' for the larges standard differences to decrease.

bal.adj1 <- bal.table(myb.ps.penal.calip, adj=TRUE, rownames.xwalk = var.labels.xwalk, standard.errs = sd.calc1)
bal.adj1$kbl

##' Adding stability increments to the matching distance, we hope to see similar
##' balance results as above..
bal.adj2 <- bal.table(myb.ps.penal.calip.stab, adj=TRUE, rownames.xwalk = var.labels.xwalk, standard.errs = sd.calc1)
bal.adj2$kbl

##' We also separately look into the balance of the population variable, not considering weighting on population size.
pop.bal <- RItools::balanceTest(mdcdExp ~ log_10_adult_w_a_cnt + strata(ps.penal.calip.stab),
                                    data = mod.dat,
                                    report = c("adj.means", "std.diffs",
                                                "z.scores", "chisquare.test"))
pop.bal

##' saving table for protocol
#+ include = FALSE
tab <- cbind(bal.unadj$tab[,-c(4:9)], bal.adj2$tab[,-c(4, 7:9)])
xtable <- xtable(tab, caption = "", table.placement = "ht", digits=c(0,1,1,1,1,1,1,1,1))
print(xtable, comment = F, size="footnotesize",
      file='../paper/protocol/figures/bal_table.tex')

##'saving only balance for population to a table for the protocol
#+ include = FALSE
tab <- c(pop.bal$results[,,2][1:3], pop.bal$results[,,1][1:3])
xtable <- xtable(t(matrix(tab)), caption = "", table.placement = "ht", digits=c(0,1,1,2,1,1,2))
print(xtable, comment = F, size="footnotesize",
      file='../paper/protocol/figures/bal_table_pop.tex')

##' ### 2020 Treatment

##' Now we consider the 2020 treatment indicator.  (This decreases the proportion
##' of matched sets contributing to the comparison, while increasing the overall
##' size of the treatment group relative to control.) 
bal.unadj <- bal.table(myb.ps.penal.calip2, adj=FALSE, rownames.xwalk = var.labels.xwalk, standard.errs = sd.calc2)
bal.unadj$kbl
bal.adj1 <- bal.table(myb.ps.penal.calip2, adj=TRUE, rownames.xwalk = var.labels.xwalk, standard.errs = sd.calc2)
bal.adj1$kbl

##' The restricted full match doesn't do much better or worst.
bal.adj2 <- bal.table(myb.ps.penal.calip.stab2, adj=TRUE, rownames.xwalk = var.labels.xwalk, standard.errs = sd.calc2)
bal.adj2$kbl

#+ include = FALSE
tab <- cbind(bal.unadj$tab[,-c(4:9)], bal.adj2$tab[,-c(4, 7:9)])
xtable <- xtable(tab, caption = "", table.placement = "ht", digits=c(0,1,1,1,1,1,1,1,1))
print(xtable, comment = F, size="footnotesize",
      file='../paper/protocol/figures/bal_table_2020.tex')

pop.bal <- RItools::balanceTest(mdcdExp2 ~ log_10_adult_w_a_cnt + strata(ps.penal.calip.stab),
                                data = mod.dat,
                                report = c("adj.means", "std.diffs",
                                           "z.scores", "chisquare.test"))
pop.bal
tab <- c(pop.bal$results[,,2][1:3], pop.bal$results[,,1][1:3])
xtable <- xtable(t(matrix(tab)), caption = "", table.placement = "ht", digits=c(0,1,1,2,1,1,2))
print(xtable, comment = F, size="footnotesize",
      file='../paper/protocol/figures/bal_table_pop_2020.tex')

##' ## Boxplots of variable overlap
##' Check out the distribution of all variables given the excluded counties from the match.

pdf("figures/boxplots/var.dist.overlap.pdf", height = 11, width = 8.5)
for(var in vars){
  
  #png(paste0("figures/boxplots/", var ,".dist.overlap.png"), width = 7, height = 10, units = 'in', res = 300)
  svyboxplot(as.formula(paste0(var,"~factor(mdcdExp)")), surv.d2,
             main = var, all.outliers = T)
  #dev.off()
}
dev.off()


##' ## Maps of matched counties and states included 

#+ include = FALSE
states <- map_data("state")

#create county data to merge with - from maps package
data("county.fips")
county.fips.clean <- county.fips %>%
  separate(polyname, sep = ":", into = c("polyname", "subsub"), remove = T) %>%
  suppressWarnings() %>%
  dplyr::select(-subsub) %>%
  distinct()

county <- map_data("county")
county_mapping <- county %>% 
  mutate(polyname = paste(region, subregion, sep=",")) %>%
  left_join(county.fips.clean, by= 'polyname') %>%
  dplyr::select(-polyname) %>%
  mutate(FIPS = sprintf('%05d', as.numeric(fips)))

#get matches and inclusion information for data by county
mod.dat$matches.final <-ps.penal.calip.stab
mod.dat$matches.nostab <-ps.penal.calip

if (save_me) save(mod.dat, file ="../data/mod.dat.Rdata")

plot.dat <- mod.dat %>%
  dplyr::select(FIPS, matches.final, matches.nostab, adult_w_a_cnt) %>%
  right_join(mod.dat1) %>%
  dplyr::select(FIPS, cntyName, stateName, adult_w_a_cnt, mdcdExp_det, matches.final, matches.nostab) %>%
  mutate(plt.fill1 = case_when(is.na(matches.final) ~ "Trimmed",
                              mdcdExp_det>1 ~ "Later Expansion",
                              stateName == "Wisconsin"| mdcdExp_det == 1  ~ "Medicaid Expansion",
                              TRUE ~ "No Expansion"),
         plt.fill2 = case_when(is.na(matches.nostab) ~ "Trimmed",
                              mdcdExp_det>1 ~ "Later Expansion",
                              stateName == "Wisconsin"| mdcdExp_det == 1  ~ "Medicaid Expansion",
                              TRUE ~ "No Expansion")) %>%
  right_join(county_mapping)

if(save_me) save(plot.dat, file = "../data/temp/map.exp.plot.dat.Rdata")

##' ### Map of status of counties
#+ include = FALSE
plot.dat2 <- plot.dat %>%
  mutate(plt.fill1 = case_when(plt.fill1 == "Trimmed" ~ plt.fill1,
                              FIPS %in% unique(pos_adj_countys$fipsneighbor) ~ "Adjacent Counties - No Expansion",
                              FIPS %in% unique(pos_adj_countys$fipscounty) ~ "Adjacent Counties - Medicaid Expansion",
                              TRUE ~ plt.fill1),
         plt.fill2 = case_when(plt.fill2 == "Trimmed" ~ plt.fill2,
                              FIPS %in% unique(pos_adj_countys$fipsneighbor) ~ "Adjacent Counties - No Expansion",
                              FIPS %in% unique(pos_adj_countys$fipscounty) ~ "Adjacent Counties - Medicaid Expansion",
                              TRUE ~ plt.fill2)) %>%
  right_join(county_mapping)

g <- ggplot() +
  geom_polygon(data = plot.dat2, aes(x=long, y=lat, group = group, fill = plt.fill1), color = 'white', size = 0.1) +
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = c("#74add1", "#d73027","#fdae61", "#4575b4", "#fc8d59", "grey45"), name = "",
                    na.value='grey') +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
  )

##' The following map shows the states inclusion:
g

png("../paper/protocol/figures/map_inclusion.png", width = 6, height = 4, units = 'in', res = 300)
g
dev.off()


##' ### Explore adjacent matches

##' First we consider adjacent matches with no stability increment.
#+ include = FALSE
adj.exp <- mod.dat %>%
  dplyr::select(FIPS:male, mdcdExp, mdcdExp2, matches.final, matches.nostab) %>%
  mutate(cont_adj = as.numeric(FIPS %in% unique(pos_adj_countys$fipsneighbor)),
         treat_adj = as.numeric(FIPS %in% unique(pos_adj_countys$fipscounty))) %>%
  group_by(matches.nostab) %>%
  arrange(matches.nostab) %>%
  mutate(n.cont2 = sum(cont_adj), n.treat2 = sum(treat_adj),
         size2 = n()) %>%
  ungroup() %>%
  group_by(matches.final) %>%
  mutate(n.cont1 = sum(cont_adj), n.treat1 = sum(treat_adj),
         size1 = n())

adj.matches.final <- adj.exp %>%
  ungroup %>%
  filter(n.cont1 > 0 & n.treat1 >0)
length(unique(adj.matches.final$matches.final)) 

adj.matches.nostab <- adj.exp %>%
  ungroup %>%
  filter(n.cont2 > 0 & n.treat2 >0)
length(unique(adj.matches.nostab$matches.nostab)) 

nrow(adj.matches.nostab %>% filter(size2 == 2))

adj.matches.adj <- adj.matches.nostab %>% filter(size2 == 2)

adj.matches.list.nostab <- adj.matches.adj %>%
  mutate(county = paste0(cntyName, " County, ",stateName)) %>%
  dplyr::select(FIPS, mdcdExp, county, matches.nostab) %>%
  pivot_wider(values_from = c(county, FIPS), names_from = mdcdExp)

fips <- adj.matches.list.nostab[,4:5]
keep <- adj_mat[as.matrix(fips)] == TRUE
adj.matches.final.nostab <- adj.matches.list.nostab[keep,]

adj.matches <- unique(adj.matches.final.nostab$matches.nostab)

plot.dat <- plot.dat %>%
  mutate(plt.fill.adj = case_when(matches.nostab %in% adj.matches ~ as.character(matches.nostab),
                              TRUE ~ plt.fill2))

##' There are this many adjacent pair matches with the matching structure with no stability increment:
(n.col <- length(unique(plot.dat$plt.fill.adj))-4)

#+ include = FALSE
cols <- c(brewer.pal(9, "YlGnBu"), brewer.pal(9, "YlGn"),brewer.pal(9, "RdPu"),
         brewer.pal(9, "YlOrRd"), brewer.pal(9, "PuBu"), brewer.pal(9, "PuRd"),
         brewer.pal(11, "BrBG"), brewer.pal(9, "Greys"), brewer.pal(10, "PRGn"))[sample(1:84, n.col)]

g1 <- ggplot() +
  geom_polygon(data = plot.dat, aes(x=long, y=lat, group = group, fill = plt.fill.adj), color = 'white', size = 0.1) +
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = c(cols, "#fdae61", "#3288bd", "#fc8d59", "grey45"), name = "",
                    na.value='grey') +
  ggtitle("Status of Counties Using Full Matching with a Caliper and Penalty with Adjacent County Matches") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
  )


##' Next we look into the adjacent matches with the stability increment.
#+ include = FALSE
adj.matches.stab <- adj.matches.final %>% filter(size1 == 2)

adj.matches.list.stab <- adj.matches.stab %>%
  mutate(county = paste0(cntyName, " County, ",stateName)) %>%
  dplyr::select(FIPS, mdcdExp, county, matches.final) %>%
  pivot_wider(values_from = c(county, FIPS), names_from = mdcdExp)

fips <- adj.matches.list.stab[,4:5]
keep <- adj_mat[as.matrix(fips)] == TRUE
adj.matches.final <- adj.matches.list.stab[keep,]

save(adj.matches.final, file = "../data/adj.matches.final.Rdata")

adj.matches <- unique(adj.matches.final$matches.final)

plot.dat <- plot.dat %>%
  mutate(plt.fill.stab = case_when(matches.final %in% adj.matches ~ as.character(matches.final),
                               TRUE ~ plt.fill1))

##' There are this many adjacent pair matches with the matching structure with the stability increment:
(n.col <- length(unique(plot.dat$plt.fill.stab))-4)

#+ include = FALSE
cols <- rep(c(brewer.pal(9, "Greens")[2:6], brewer.pal(9, "YlGn")[3:6], brewer.pal(9, "BuGn")[3:6]),2)[1:n.col]

g2 <- ggplot() +
  geom_polygon(data = plot.dat, aes(x=long, y=lat, group = group, fill = plt.fill.stab), color = 'white', size = 0.1) +
  geom_polygon(data = states, aes(x=long, y=lat, group=group), color = "gray50", size = .2, alpha = 0)+
  coord_map(projection = "albers", 30, 40, xlim = c(-122, -72)) +
  scale_fill_manual(values = c(cols, "#fdae61", "#4575b4", "#fc8d59", "grey45"), name = "",
                    na.value='grey') +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = margin(0, 0, 0, 0, "cm")
  )


##' The following plots highlight matches for which there are two counties in the match and they are both counties
##' that are adjacent to a county with the opposite treatment indicator. There are plenty of matching with adjacent 
##' using this matching structure.

g1

g2

png("../paper/protocol/figures/map_adj_matches.png", width = 6, height = 4, units = 'in', res = 300)
g2
dev.off()

##' ### Balance for Adjacent matches.

bal.dat <- mod.dat %>%
  filter(matches.final %in% adj.matches)

myb <- RItools::balanceTest(update.formula(form0, paste0(".~.+ strata(matches.final)")),
                             data = bal.dat,
                             report = c("adj.means", "std.diffs",
                                        "z.scores"),
                             unit.weights = adult_w_a_cnt)

print(myb)

##' ### Average distances for adjacent counties - evaluating matched sets


##' This is a quick look at the proportion of matches that could be considered "close" in terms of
##' the mahalanobis distance. I look at matched sets for which at least 2/3 of the counties in that
##' set have a Mahalanobis distance less than the 1st quantile of the Mahalanobis distances between
##' adjacent counties. There are plenty of matches that fall into this category.

hist(adj_dist)
hist(all_mh)

ps.pen.d <- matched.distances(ps.penal.calip, all_mh)
ps.pen.stab.d <- matched.distances(ps.penal.calip.stab, all_mh)
mean(unlist(ps.pen.d))
mean(unlist(ps.pen.stab.d)) #smallest mean distance


cutoff <- quantile(adj_dist, probs = .15)
prop.close.pen <- sapply(ps.pen.d, function(x){mean(x <= cutoff)})
prop.close.pen.stab <- sapply(ps.pen.stab.d, function(x){mean(x <= cutoff)})


#pull the names of matches that have at least 50% of counties within the cutoff 
#of less than the median MH for adjacent counties
matches.close.pen <- names(prop.close.pen[prop.close.pen > .8])
matches.close.pen.stab <- names(prop.close.pen.stab[prop.close.pen.stab > .8])


##' The number of close matches is similar between using the stability increment and not.
length(matches.close.pen)
length(matches.close.pen.stab)

close.matches.pen.stab <- mod.dat %>%
  filter(matches.final %in% matches.close.pen.stab) %>%
  dplyr::select(stateName, cntyName, matches.final) %>%
  arrange(matches.final)

close.matches.pen <- mod.dat %>%
  filter(matches.nostab %in% matches.close.pen) %>%
  dplyr::select(stateName, cntyName, matches.nostab) %>%
  arrange(matches.nostab)

save(close.matches.pen.stab, file = "../data/close.matches.final.Rdata")

#rmarkdown::render("03b-matching-weighted.R", "github_document")

