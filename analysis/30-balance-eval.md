Matching Balance
================
manncz
2022-06-15

Load necessary data

``` r
load("../data/temp/matches.Rdata")
load("../data/temp/matching.adj.dat.Rdata")
load("../data/temp/pooled.sd1.v2.Rdata")
load("../data/temp/pooled.sd2.v2.Rdata")
load("../data/temp/supermajority.white.xwalk.Rdata")
```

Add match variable to data

``` r
mod.dat$match <- ps.penal.calip.stab
```

Set up formula

``` r
form1 <- mdcdExp ~ log_10_adult_w_a_cnt +
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
```

## Balance Overall

``` r
myb <- get.balTest(match = "match")
data.frame(myb$overall)
```

    ##       chisquare df      p.value
    ## match  25.06936 25 4.584921e-01
    ## --    168.92823 14 1.127172e-28

``` r
love.plot(myb, "overall", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")
```

    ## null device 
    ##           1

## Balance weighting by black population size

``` r
myb.bpop <- get.balTest.bpop(match = "match")
data.frame(myb.bpop$overall)
```

    ##       chisquare df      p.value
    ## match  22.08057 24 5.744578e-01
    ## --    114.39244 15 2.301930e-17

``` r
love.plot(myb.bpop, "black_pop", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")
```

    ## null device 
    ##           1

## Balance for Supermajority White analysis

``` r
mod.dat <- mod.dat %>%
  left_join(maj.white.xwalk, by = "FIPS") %>%
  group_by(match) %>%
  mutate(k = as.numeric(sum(maj_white) >= 1))

myb_smw <- get.balTest(match = "match", dat = mod.dat %>% filter(k == 1))
data.frame(myb_smw$overall)
```

    ##       chisquare df      p.value
    ## match  18.31034 23 7.404451e-01
    ## --    148.80346 13 3.589971e-25

``` r
love.plot(myb_smw, "smw", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")
```

    ## null device 
    ##           1

## Balance by Race

``` r
mod.dat <- mod.dat %>%
  mutate(b.95 = as.numeric(black_race >= quantile(mod.dat$black_race, probs = .95)),
         b.9 = as.numeric(black_race >= quantile(mod.dat$black_race, probs = .9)),
         b.75 = as.numeric(black_race >= quantile(mod.dat$black_race, probs = .75))) %>%
  group_by(match) %>%
  mutate(across(starts_with("b."), ~as.numeric(sum(.x) >= 1), .names = "k_{.col}"))

check <- mod.dat %>%
  ungroup() %>%
  dplyr::select(starts_with("k_"), starts_with("b.")) %>%
  summarize(across(everything(), sum))
```

Upper 95th percentile black

``` r
myb_b95 <- get.balTest(match = "match", dat = mod.dat %>% filter(k_b.95 == 1))
data.frame(myb_b95$overall)
```

    ##       chisquare df     p.value
    ## match  19.71932 20 4.75606e-01
    ## --     81.86419 14 1.27440e-11

``` r
love.plot(myb_b95, "b95", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")
```

    ## null device 
    ##           1

Upper 90th percentile black

``` r
myb_b9 <- get.balTest(match = "match", dat = mod.dat %>% filter(k_b.9 == 1))
data.frame(myb_b9$overall)
```

    ##       chisquare df      p.value
    ## match  28.35566 23 2.026304e-01
    ## --     96.08231 15 7.176803e-14

``` r
love.plot(myb_b9, "b90", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")
```

    ## null device 
    ##           1

Upper 75th percentile black

``` r
myb_b75 <- get.balTest(match = "match", dat = mod.dat %>% filter(k_b.75 == 1))
data.frame(myb_b75$overall)
```

    ##       chisquare df      p.value
    ## match  16.77042 25 8.899517e-01
    ## --    172.64919 14 1.995592e-29

``` r
love.plot(myb_b75, "b75", rownames.xwalk = var.labels.xwalk, print = T,
          fpath = "figures/love_plot_")
```

    ## null device 
    ##           1

``` r
#rmarkdown::render("30-balance-eval.R", "github_document")
```
