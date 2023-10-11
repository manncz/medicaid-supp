##' ---
##' title: "Closeness of Matching on Underlying Propensity Score"
##' output: github_document
##' ---
##'
#+ setup0, message=FALSE, warning=FALSE, echo=FALSE
requireNamespace("optmatch")# for glm, svyglm boxplot methods
library("readr")
library("tibble")
library("survey")
op  <- options(digits=3)
##' Necessary private library:
##+
library("PISE")
stopifnot(packageVersion("PISE") >= '0.2.0.9006')
##' Load required data:
load("../data/mod.dat.Rdata")
##' ## Propensity Score Model


##' Main propensity score model from 03b-matching. Uses `survey::svyglm()`
##' to facilitate frequency weighting by population size.
##+ echo=-1L
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
surv.d1 <- survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=mod.dat)
psm <- survey::svyglm(form0,
                      design=surv.d1,
                      family=quasibinomial())
##+ eval=FALSE, echo=FALSE
optmatch:::boxplot.glm(psm,
                       main = "Overlap on Fitted Scores",
                       xlab = "Treatment or Control Counties",
                       ylab = "Propensity Score")

##' A priori, notional experiment PIC errors can be characterized
##' as follows.
##+
psm_pic_me  <-
    pic_maxerr(x=psm, treatment=as.logical(psm$y), covariance.estimator='sandwich')
psm_pic_me
##' Having settled on the match `matches.final`, corresponding PIC
##' errors may be estimated as follows.
##+
pic_maxerr(mod.dat$matches.final, x=psm, treatment=as.logical(psm$y),
           covariance.estimator='sandwich')
##' The notional experiment presumes fewer treatment-control pairings
##' than `matches.final` has, and the difference accounts for a bit of
##' their discrepancy in maximum PIC error. 
##+
tcpairs  <- list(notional=min(table(psm$y)),
                 matches.final=sum(!is.na(mod.dat$matches.final)) -
                     nlevels(mod.dat$matches.final)
                 )
unlist(tcpairs)
with(tcpairs, log(matches.final)/log(notional))
##'
##' pairwise index standard errors for matched treatment and control
##' counties are
##+
psm_paired_ses  <- psm |> paired_se_dist()
setNames(mod.dat$matches.final, row.names(mod.dat)) |>
optmatch:::matched.distances(psm_paired_ses) |>
unlist() |> (\(d) 
list(`below rms_err`=mean(d<psm_pic_me$rms_err),
     `betw rms & max_pic_se`=mean(psm_pic_me$rms_err<=d & d< psm_pic_me$max_pic_se),
     `above max_pic_se`=mean(psm_pic_me$max_pic_se<=d),
     `above max_err`=sort(d[psm_pic_me$max_err<=d])
     )
)()
##' where
##+
psm_pic_me[c("rms_err", "max_pic_se", "max_err")]
##' 
##' For the record, treatment-control differences on the estimated
##' propensity are like so.
##+
mds  <-
    optmatch::matched.distances(setNames(mod.dat$matches.final,
                                         rownames(mod.dat)),
                                optmatch::match_on(psm,
                                                   data=mod.dat, standardization.scale=1)
                                )
summary(unlist(mds))
##' Also for the record, the following share of counties, and of working-age adjust population, are excluded by our calipers.
##+
with(mod.dat, mean(is.na(matches.final)))
with(mod.dat, weighted.mean(is.na(matches.final), w=adult_w_a_cnt))
with(mod.dat, tapply(is.na(matches.final), mdcdExp, mean))
split(mod.dat, mod.dat$mdcdExp) |>
sapply({\(x) weighted.mean(is.na(x$matches.final),
                           w=x$adult_w_a_cnt)
       })
##+ echo=FALSE
options(op)
