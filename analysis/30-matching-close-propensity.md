Closeness of Matching on Underlying Propensity Score
================
bbh
2023-10-09

Necessary private library:

``` r
library("PISE")
stopifnot(packageVersion("PISE") >= '0.2.0.9006')
```

Load required data:

``` r
load("../data/mod.dat.Rdata")
```

## Propensity Score Model

Main propensity score model from 03b-matching. Uses `survey::svyglm()`
to facilitate frequency weighting by population size.

``` r
surv.d1 <- survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=mod.dat)
psm <- survey::svyglm(form0,
                      design=surv.d1,
                      family=quasibinomial())
```

A priori, notional experiment PIC errors can be characterized as
follows.

``` r
psm_pic_me  <-
    pic_maxerr(x=psm, treatment=as.logical(psm$y), covariance.estimator='sandwich')
psm_pic_me
```

    ## pic_me_info object (a list) with `c(max_err, rms_err)`= c(6.09, 1.54)

Having settled on the match `matches.final`, corresponding PIC errors
may be estimated as follows.

``` r
pic_maxerr(mod.dat$matches.final, x=psm, treatment=as.logical(psm$y),
           covariance.estimator='sandwich')
```

    ## pic_me_info object (a list) with `c(max_err, rms_err)`= c(10, 1.66)

The notional experiment presumes fewer treatment-control pairings than
`matches.final` has, and the difference accounts for a bit of their
discrepancy in maximum PIC error.

``` r
tcpairs  <- list(notional=min(table(psm$y)),
                 matches.final=sum(!is.na(mod.dat$matches.final)) -
                     nlevels(mod.dat$matches.final)
                 )
unlist(tcpairs)
```

    ##      notional matches.final 
    ##          1246          2368

``` r
with(tcpairs, log(matches.final)/log(notional))
```

    ## [1] 1.09

pairwise index standard errors for matched treatment and control
counties are

``` r
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
```

    ## $`below rms_err`
    ## [1] 0.742
    ## 
    ## $`betw rms & max_pic_se`
    ## [1] 0.131
    ## 
    ## $`above max_pic_se`
    ## [1] 0.127
    ## 
    ## $`above max_err`
    ## 1.1041.1193  1.1041.158   1.1041.77   1.1015.68  1.1041.195 1.1041.1022 
    ##        6.21        6.24        6.24        6.24        6.35        6.40 
    ##  1.1041.196 1.1041.1004  1.1041.208  1.530.2656  1.671.2389 
    ##        6.49        6.65        6.80        7.30       10.00

where

``` r
psm_pic_me[c("rms_err", "max_pic_se", "max_err")]
```

    ## $rms_err
    ## [1] 1.54
    ## 
    ## $max_pic_se
    ## [1] 2.18
    ## 
    ## $max_err
    ## [1] 6.09

For the record, treatment-control differences on the estimated
propensity are like so.

``` r
mds  <-
    optmatch::matched.distances(setNames(mod.dat$matches.final,
                                         rownames(mod.dat)),
                                optmatch::match_on(psm,
                                                   data=mod.dat, standardization.scale=1)
                                )
summary(unlist(mds))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.007   0.017   0.044   0.048   0.507

Also for the record, the following share of counties, and of working-age
adjust population, are excluded by our calipers.

``` r
with(mod.dat, mean(is.na(matches.final)))
```

    ## [1] 0.0342

``` r
with(mod.dat, weighted.mean(is.na(matches.final), w=adult_w_a_cnt))
```

    ## [1] 0.0374

``` r
with(mod.dat, tapply(is.na(matches.final), mdcdExp, mean))
```

    ##      0      1 
    ## 0.0242 0.0490

``` r
split(mod.dat, mod.dat$mdcdExp) |>
sapply({\(x) weighted.mean(is.na(x$matches.final),
                           w=x$adult_w_a_cnt)
       })
```

    ##       0       1 
    ## 0.00133 0.06631
