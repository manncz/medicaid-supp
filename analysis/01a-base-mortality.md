Assemble/extract baseline variables for matching
================
bbh
Tue Sep 15 00:17:27 2020

``` r
load("../data/icd10_strings.RData")
list(amenable=amenable_icd10,
     flu=flu_icd10,
     opioid=opioid_icd10) %>%
    map_chr(paste, collapse="|") ->
    icd10_regexps
```

Read in and collapse CDC cross-tabs
===================================

Population
----------

Read in the data and filter it down to the "prereform period". For Sommers, Long and Baicker (*Ann. Intern. Med.* 2014; henceforth "SLB") this was either 2001-2005 or 2000-2006 (cf note accompanying Appendix Table 2, propensity score coefs; p.7). For us, 2009-2013, inclusive (I think).

Values for the Record type variable, per file w/ specs (1999-2013CMFDocumentation.docx):

1 National population record 2 State pop record 3 County pop record

This being an assembling of baseline data, we immediately filter out observations following the baseline period.

``` r
pop_tall  <- file.path("..", "data-raw", "pop9914.txt") %>%
    read_fwf(fwf_cols(stateFIPS=c(1,2), # 'state'
                      cntyFIPS=c(3,5),  # 'county'
                      year=c(6,9),
                      race_sex=c(10,10), # 'racesex'
                      hisp_code=c(11,11),# 'hisp'
                      births=c(12,19),   # 'birth'
                      a0_1=c(20,27),     # 'l1'
                      a1_4=c(28,35),     # 'a14'
                      a5_9=c(36,43),
                      a10_14=c(44, 51),
                      a15_19=c(52,59),
                      a20_24=c(60,67),
                      a25_34=c(68,75),
                      a35_44=c(76,83),
                      a45_54=c(84,91),
                      a55_64=c(92,99),
                      a65_74=c(100,107),
                      a75_84=c(108,115),
                      a85plus=c(116,123),    # 'a85'
                      cnty_name=c(124,148),  # 'name'
                      record_type=c(149,149) # 'type'
                      ),
             progress=FALSE,
             )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   stateFIPS = col_character(),
    ##   cntyFIPS = col_character(),
    ##   cnty_name = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
pop_tall  %>% filter(record_type==3, year<=2013) %>%
    select(-births,-record_type) ->
    pop_tall
pop_tall %>% map_lgl(~any(is.na(.x)))
```

    ## stateFIPS  cntyFIPS      year  race_sex hisp_code      a0_1      a1_4      a5_9 
    ##     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE 
    ##    a10_14    a15_19    a20_24    a25_34    a35_44    a45_54    a55_64    a65_74 
    ##     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE 
    ##    a75_84   a85plus cnty_name 
    ##     FALSE     FALSE     FALSE

``` r
pop_tall %>% map_dbl(~length(unique(.x)))
```

    ## stateFIPS  cntyFIPS      year  race_sex hisp_code      a0_1      a1_4      a5_9 
    ##        51       325        15         8         3      5008     11447     13191 
    ##    a10_14    a15_19    a20_24    a25_34    a35_44    a45_54    a55_64    a65_74 
    ##     13472     13702     13929     19372     20232     20089     17275     13591 
    ##    a75_84   a85plus cnty_name 
    ##     10702      6354      1850

Mutate the variables to required forms, discarding what neither appears in their Appendix Table 2 nor looks otherwise essential to our purposes.

### Race, sex & "hispanic" variables

Values for `race_sex`, from POP9914 file spec:

| lev | definition                              |
|-----|-----------------------------------------|
| 1   | White male                              |
| 2   | White female                            |
| 3   | Black male                              |
| 4   | Black female                            |
| 5   | American Indian or Alaska Native male   |
| 6   | American Indian or Alaska Native female |
| 7   | Asian or Pacific Islander male          |
| 8   | Asian or Pacific Islander female        |

Here we create race and gender variables, also mutate `race_sex` by collapsing racial categories other than B. & W. (NB: the code solution I'm using here requires R version 3.5.0, per ["tim" on StackOverflow](https://stackoverflow.com/questions/19410108/cleaning-up-factor-levels-collapsing-multiple-levels-labels) .)

``` r
pop_tall %>% 
    mutate(white_race=(race_sex==1 | race_sex==2),
           black_race=(race_sex==3 | race_sex==4),
           other_race=(race_sex>=5 & race_sex<=8),
           male=(race_sex==1 |  race_sex==3 |
                 race_sex==5 | race_sex==7),
           race_sex=factor(race_sex, levels=1:8,
                           labels=c("White_M",
                                    "White_F",
                                    "Black_M",
                                    "Black_F",
                                    "other_M",
                                    "other_F",
                                    "other_M",
                                    "other_F")
                           )
           ) ->
    pop_tall
```

Values for the Hispanic origin variable:

| lev | definition             |
|-----|------------------------|
| 1   | Not Hispanic or Latino |
| 2   | Hispanic or Latino     |
| 9   | Not stated             |

``` r
pop_tall %>% mutate(latino=(hisp_code==2)) %>%
    select(-hisp_code) ->
    pop_tall
```

### Age categories & county population

Here `ax_y` records population in age group ranging from x to y. SLB merged some age categories. (They also say they weighted logistic regressions for "size of each county’s non-elderly adult population.")

``` r
pop_tall %>% mutate(a20_34=(a20_24 +a25_34)) ->
    pop_tall
pop_tall$adult_w_a <- pop_tall %>% select(a20_24:a55_64) %>%
    rowSums(na.rm=TRUE)
pop_tall$adult_all <- pop_tall %>% select(a20_24:a85plus) %>%
    rowSums(na.rm=TRUE)
pop_tall %>% select(-starts_with("a"),
               a20_34, a35_44:a85plus, adult_w_a, adult_all) ->
    pop_tall
```

### Lookup table for county names vs FIPS keys

``` r
cnty_IDs  <- pop_tall %>% select(stateFIPS, cntyFIPS, cnty_name) 
cnty_IDs  <- cnty_IDs[!duplicated(cnty_IDs),]
```

### County by year population table

For age-standardization within county and race/sex subcategory, we need the age distribution across county, race and sex. We'll follow WONDER in using the 2000 distribution.

``` r
pop_tall %>% filter(year==2000) %>% ungroup() %>% 
    summarise_at(c("a20_34", "a35_44", "a45_54", "a55_64"), sum) %>%
    unlist() -> agesY2K_w_a 
(agesY2K_w_a  <- agesY2K_w_a/sum(agesY2K_w_a))
```

    ##    a20_34    a35_44    a45_54    a55_64 
    ## 0.3547772 0.2719927 0.2269816 0.1462485

That's all the use we have currently for data before 2008.

``` r
pop_tall %>% filter(2008<=year) -> pop_tall
```

The table `pop_tall` has distinct rows not only for distinct counties and/or years, but also for distinct values of gender, race and ethnicity variables.

First marginalize over race, hispanic origin, gender and age, but leave out interaction categories.

``` r
pop_tall %>% group_by(stateFIPS, cntyFIPS, year) %>%
    summarise(
        white_race=sum(white_race*adult_all),
        black_race=sum(black_race*adult_all),
        other_race=sum(other_race*adult_all),
        male=sum(male*adult_all),
        latino=sum(latino*adult_all),
        a20_34=sum(a20_34),
        a35_44=sum(a35_44),
        a45_54=sum(a45_54),
        a55_64=sum(a55_64),
        a65_74=sum(a65_74),
        a75_84=sum(a75_84),
        a85plus=sum(a85plus),
        adult_w_a=sum(adult_w_a),
        adult_all=sum(adult_all)
        ) %>% mutate(
                  white_race=100*white_race/adult_all,
                  black_race=100*black_race/adult_all,
                  other_race=100*other_race/adult_all,
                  latino=100*latino/adult_all,
                  male=100*male/adult_all,
                  adjcnt_adult_w_a=(agesY2K_w_a["a20_34"]*a20_34 +
                                 agesY2K_w_a["a35_44"]*a35_44 +
                                 agesY2K_w_a["a45_54"]*a45_54 +
                                 agesY2K_w_a["a55_64"]*a55_64),
                  a20_34=100*a20_34/adult_all,
                  a35_44=100*a35_44/adult_all,
                  a45_54=100*a45_54/adult_all,
                  a55_64=100*a55_64/adult_all,
                  a65_74=100*a65_74/adult_all,
                  a75_84=100*a75_84/adult_all,
                  a85plus=100*a85plus/adult_all,
                  adult_w_a_cnt=adult_w_a, adult_w_a=NULL,
                  adult_all_cnt=adult_all, adult_all=NULL
              ) %>% filter(adult_w_a_cnt>0) -> margins_by_year
```

Now create version of `pop_tall` collapsing cells that differ only by hispanic origin.

``` r
pop_tall %>% select(-latino) %>%
    group_by(stateFIPS, cntyFIPS,
             race_sex, year) %>%
    summarise(
        white_race=sum(white_race*adult_all),
        black_race=sum(black_race*adult_all),
        other_race=sum(other_race*adult_all),
        male=sum(male*adult_all),
        a20_34=sum(a20_34),
        a35_44=sum(a35_44),
        a45_54=sum(a45_54),
        a55_64=sum(a55_64),
        a65_74=sum(a65_74),
        a75_84=sum(a75_84),
        a85plus=sum(a85plus),
        adult_w_a=sum(adult_w_a),
        adult_all=sum(adult_all)
        ) %>%    mutate(
        white_race=100*white_race/adult_all,
        black_race=100*black_race/adult_all,
        other_race=100*other_race/adult_all,
        male=100*male/adult_all,
        adjcnt_adult_w_a=(agesY2K_w_a["a20_34"]*a20_34 +
                          agesY2K_w_a["a35_44"]*a35_44 +
                          agesY2K_w_a["a45_54"]*a45_54 +
                          agesY2K_w_a["a55_64"]*a55_64),
        a20_34=100*a20_34/adult_all,
        a35_44=100*a35_44/adult_all,
        a45_54=100*a45_54/adult_all,
        a55_64=100*a55_64/adult_all,
        a65_74=100*a65_74/adult_all,
        a75_84=100*a75_84/adult_all,
        a85plus=100*a85plus/adult_all,
        adult_w_a_cnt=adult_w_a, adult_w_a=NULL,
        adult_all_cnt=adult_all, adult_all=NULL        
    ) %>% filter(adult_w_a_cnt>0) -> interactions_by_year

if (cleanupunderway) rm(pop_tall)
```

That's where we'll leave things in terms of demographics...

``` r
margins_by_year %>% filter(2009<=year, year<=2013) %>%
    ungroup() %>%  group_by(stateFIPS, cntyFIPS) %>%
    summarise_at(vars(white_race:a85plus),
                 ~weighted.mean(., w=adult_all_cnt)
                 ) -> base_cnty
```

... and a one-year lagged variant:

``` r
margins_by_year %>% filter(2008<=year, year<=2012) %>%
    ungroup() %>%  group_by(stateFIPS, cntyFIPS) %>%
    summarise_at(vars(white_race:a85plus),
                 ~weighted.mean(., w=adult_all_cnt)
                 ) -> .base_cnty
```

SLB weight their propensity scores for population size. We'll follow, using the average of 2008--2013 population sizes.

``` r
margins_by_year  %>% filter(2009<=year, year<=2013) %>%
    ungroup() %>%  group_by(stateFIPS, cntyFIPS) %>%
    summarise(adult_w_a_cnt=mean(adult_w_a_cnt),
              adult_all_cnt=mean(adult_all_cnt),
              adjcnt_adult_w_a=mean(adjcnt_adult_w_a)) %>%
    right_join(base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> base_cnty
```

(The averages of 2009-13 adult population sizes being `adult_w_a_cnt` and `adult_all_cnt`, respectively, for working age adults and for all adults. The `adjcnt` version standarizes each age distribution to that of the country, and doesn't seem to play a role for SLB.) Lagged variant:

``` r
margins_by_year  %>% filter(2008<=year, year<=2012) %>%
    ungroup() %>%  group_by(stateFIPS, cntyFIPS) %>%
    summarise(adult_w_a_cnt=mean(adult_w_a_cnt),
              adult_all_cnt=mean(adult_all_cnt),
              adjcnt_adult_w_a=mean(adjcnt_adult_w_a)) %>%
    right_join(.base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> .base_cnty
```

Mortality
---------

Again we read in only what looks important for baseline analyses paralleling those of SLB.

``` r
mort_tall  <- file.path("..", "data-raw", "mort9914.txt") %>%
    read_fwf(fwf_cols(stateFIPS=c(1,2), 
                      cntyFIPS=c(3,5),  
                      year=c(6,9),
                      race_sex=c(10,10), 
                      hisp_code=c(11,11),
                      longevity=c(12,13),
                      icd10=c(14,17),
                      c_o_d_r=c(18,20), #cause-of-death recode
                      freq=c(21,24)
                      ),
             progress=FALSE,
             ) %>% filter(year>=2008, year<=2013,
                          longevity>=09, # exclude deaths @ <20yrs
                          longevity<=13) # exclude deaths @ >64yrs
```

    ## Parsed with column specification:
    ## cols(
    ##   stateFIPS = col_character(),
    ##   cntyFIPS = col_character(),
    ##   year = col_double(),
    ##   race_sex = col_double(),
    ##   hisp_code = col_double(),
    ##   longevity = col_double(),
    ##   icd10 = col_character(),
    ##   c_o_d_r = col_double(),
    ##   freq = col_double()
    ## )

``` r
mort_tall %>% map_lgl(~any(is.na(.x)))
```

    ## stateFIPS  cntyFIPS      year  race_sex hisp_code longevity     icd10   c_o_d_r 
    ##     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE     FALSE 
    ##      freq 
    ##     FALSE

``` r
mort_tall %>% map_dbl(~length(unique(.x)))
```

    ## stateFIPS  cntyFIPS      year  race_sex hisp_code longevity     icd10   c_o_d_r 
    ##        51       324         6         8         3         5      4079       112 
    ##      freq 
    ##       154

Save the mortality cross-tabulation after aggregating over the full pre-intervention period. (Keeping us in line with the "aggregate over at least 3 years" rule.) For the purpose of capturing codes of death in a shareable file.

``` r
mort_tall %>% group_by(stateFIPS, cntyFIPS,
                       race_sex, longevity,
                       icd10, c_o_d_r) %>%
    summarise(deaths=sum(freq)) %>%
    mutate(deaths=ifelse(deaths<10, NA, deaths)) %>%
    write_csv("../data/base_mort_crosstab.csv")
```

### Aggregated cause-of-death designations

It so happens that several of the aggregated cause-of-death designations of interest to us are neatly nested.

``` r
mort_tall %>%
    mutate(c_o_d_cat=
               case_when( str_detect(icd10, icd10_regexps['opioid']) ~
                             "Opioid",
                         !str_detect(icd10, icd10_regexps['amenable']) ~
                             "HC_unamnbl_not_opioid",
                          str_detect(icd10, icd10_regexps['flu']) ~
                             "Flu",
                          str_detect(icd10, icd10_regexps['amenable']) ~
                              "HC_amenable_not_flu",
                         TRUE ~ as.character(icd10) # (not caught by any of above
                         )                          # conditions; shouldn't occur)
           ) -> mort_tall
c_o_d_cats  <- unique(mort_tall$c_o_d_cat)
stopifnot(length(c_o_d_cats)==4)
mort_tall %>% filter(year >=2008, year <=2013) %>%
    group_by(year, c_o_d_cat) %>%
    summarise(deaths=sum(freq))
```

    ## # A tibble: 24 x 3
    ## # Groups:   year [6]
    ##     year c_o_d_cat             deaths
    ##    <dbl> <chr>                  <dbl>
    ##  1  2008 Flu                     7380
    ##  2  2008 HC_amenable_not_flu   390350
    ##  3  2008 HC_unamnbl_not_opioid 189630
    ##  4  2008 Opioid                 33800
    ##  5  2009 Flu                     9433
    ##  6  2009 HC_amenable_not_flu   393428
    ##  7  2009 HC_unamnbl_not_opioid 189712
    ##  8  2009 Opioid                 34365
    ##  9  2010 Flu                     6844
    ## 10  2010 HC_amenable_not_flu   392641
    ## # … with 14 more rows

### Reshaping

First rewrite race\_sex variable, collapsing races other than B/W as in SLB, as well as age at death variable.

``` r
mort_tall %>% mutate(race_sex=factor(race_sex, levels=1:8,
                                    labels=c("White_M",
                                             "White_F",
                                             "Black_M",
                                             "Black_F",
                                             "other_M",
                                             "other_F",
                                             "other_M",
                                             "other_F")
                                    ),
                     longevity=factor(longevity, levels=09:13,
                                         labels=c(rep("20_34",2), "35_44",
                                                  "45_54", "55_64")
                                         )
                     ) ->
    mort_tall
```

Capture some names for later...

``` r
longevity_levs  <- levels(mort_tall$longevity)
```

Reduce the table to categories we're likely to use, reshape as necessary for joining with demographics & economics tables.

``` r
mort_tall %>% 
group_by(stateFIPS, cntyFIPS, year,
         longevity) %>%
    summarise(deaths=sum(freq)) %>%
    pivot_wider(names_from=longevity, values_from=deaths,
                names_prefix="dead",
                values_fill = list(deaths=0)
                ) ->
    mort_by_year

mort_tall %>% 
group_by(stateFIPS, cntyFIPS, year) %>%
    summarise(dead20_64=sum(freq)) %>%
    right_join(mort_by_year) ->
    mort_by_year
```

    ## Joining, by = c("stateFIPS", "cntyFIPS", "year")

``` r
mort_tall %>% 
group_by(stateFIPS, cntyFIPS, race_sex, 
         year, longevity) %>%
    summarise(deaths=sum(freq)) %>%
    pivot_wider(names_from=longevity, values_from=deaths,
                names_prefix="dead",
                values_fill = list(deaths=0)
                ) ->
    mort_interactions_by_year

mort_tall %>% 
group_by(stateFIPS, cntyFIPS, race_sex, 
         year) %>%
    summarise(dead20_64=sum(freq)) %>%
    right_join(mort_interactions_by_year) ->
    mort_interactions_by_year
```

    ## Joining, by = c("stateFIPS", "cntyFIPS", "race_sex", "year")

``` r
mort_tall %>% 
group_by(stateFIPS, cntyFIPS, c_o_d_cat, 
         year, longevity) %>%
    summarise(deaths=sum(freq)) %>%
    pivot_wider(names_from=longevity, values_from=deaths,
                names_prefix="dead",
                values_fill = list(deaths=0)
                ) -> c_o_d_cat_by_year
```

Not needing big table `mort_tall` any more,

``` r
if (cleanupunderway)  rm(mort_tall)
```

Now join up the restructured tables. (Before merging I record row counts, so I'll be able to identify unexpected increase.)

``` r
ns <- list(c_o_d_cat_by_year=nrow(c_o_d_cat_by_year),
           margins_by_year=nrow(margins_by_year)
           )
margins_by_year %>%# select columns
    right_join(c_o_d_cat_by_year,
               by = c("stateFIPS", "cntyFIPS", "year")
               ) -> c_o_d_cat_by_year
margins_by_year %>%
    left_join(mort_by_year, by = c("stateFIPS", "cntyFIPS", "year")
              ) -> margins_by_year
for (col in paste0("dead", c(longevity_levs, "20_64")) )
    {
        theNAs  <- is.na(margins_by_year[[col]])
        margins_by_year[theNAs, col] <- 0
        }

###for (col in paste0("dead", longevity_levs) )
###    {
###        theNAs  <- is.na(c_o_d_cat_by_year[[col]])
###        c_o_d_cat_by_year[theNAs, col] <- 0        
###    }
stopifnot(ns$c_o_d_cat_by_year==nrow(c_o_d_cat_by_year),
          all(complete.cases(c_o_d_cat_by_year)),
          ns$margins_by_year==nrow(margins_by_year),
          all(complete.cases(margins_by_year))
          )

ns$interactions_by_year  <- nrow(interactions_by_year)
interactions_by_year %>%
    left_join(mort_interactions_by_year, by = c("stateFIPS", "cntyFIPS", "race_sex", "year")
              ) -> interactions_by_year
for (col in paste0("dead", c(longevity_levs, "20_64")) )
    {
        theNAs  <- is.na(interactions_by_year[[col]])
        interactions_by_year[theNAs, col] <- 0
        }
stopifnot(nrow(interactions_by_year)==ns$interactions_by_year,
          all(complete.cases(interactions_by_year))
          )
```

### Overall mortality

<!-- All-cause mortality rates among non-elderly
     working age adults by county and year, 
     as in SLB's propensity score model.  Following SLB,
     record this in deaths per 100K.
     (Used by SLB but not, presently, by us;
      re-enable if this changes.)
 -->
lagged variant:

As aggregated across a 5-year pre-intervention period.

``` r
margins_by_year %>% filter(year>=2009, year<=2013) %>%
    group_by(stateFIPS, cntyFIPS) %>%
    summarise(
        mortAC_20_34=1e5*mean(dead20_34/(a20_34*0.01*adult_all_cnt)),
        mortAC_35_44=1e5*mean(dead35_44/(a35_44*0.01*adult_all_cnt)),
        mortAC_45_54=1e5*mean(dead45_54/(a45_54*0.01*adult_all_cnt)),
        mortAC_55_64=1e5*mean(dead55_64/(a55_64*0.01*adult_all_cnt)),
        mortAC_20_64=1e5*sum(dead20_64)/sum(adult_w_a_cnt)) %>%
    right_join(base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> base_cnty
```

Similar calculations for the 5 year window lagged one year back.

``` r
margins_by_year %>% filter(year>=2008, year<=2012) %>%
    group_by(stateFIPS, cntyFIPS) %>%
    summarise(
        mortAC_20_34=1e5*mean(dead20_34/(a20_34*0.01*adult_all_cnt)),
        mortAC_35_44=1e5*mean(dead35_44/(a35_44*0.01*adult_all_cnt)),
        mortAC_45_54=1e5*mean(dead45_54/(a45_54*0.01*adult_all_cnt)),
        mortAC_55_64=1e5*mean(dead55_64/(a55_64*0.01*adult_all_cnt)),
        mortAC_20_64=1e5*sum(dead20_64)/sum(adult_w_a_cnt)) %>%
    right_join(.base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> .base_cnty
```

### Mortality by race/sex group, & by C.O.D.

For age-standardization within county and race/sex subcategory, we need the age distribution across county, race and sex.

``` r
agesY2K_w_a
```

    ##    a20_34    a35_44    a45_54    a55_64 
    ## 0.3547772 0.2719927 0.2269816 0.1462485

Here we figure mortality rates by age group, for each combination of county, sex and gender, and go on immediately to aggregate across age groups using proportions given above. Also mortality rates by age group, for each cause-of-death and county, aggregated by age groups using precisely the same proportions. Again we use a 5 year window, then separately perform parallel calculations for the 5 year window lagged one year back. Preceding underscores again mark the lagged versions.

``` r
interactions_by_year %>% filter(year>=2009, year<=2013) %>% 
    select(stateFIPS, cntyFIPS, race_sex, dead20_34:dead55_64,
           a20_34:a55_64, adult_w_a_cnt, adult_all_cnt) %>%
    group_by(stateFIPS, cntyFIPS, race_sex) %>%
    summarise(
        mortAC_20_34=ifelse(sum(a20_34*adult_all_cnt), 1e5*sum(dead20_34)/sum(a20_34*0.01*adult_all_cnt), 0),
        mortAC_35_44=ifelse(sum(a35_44*adult_all_cnt), 1e5*sum(dead35_44)/sum(a35_44*0.01*adult_all_cnt), 0), 
        mortAC_45_54=ifelse(sum(a45_54*adult_all_cnt), 1e5*sum(dead45_54)/sum(a45_54*0.01*adult_all_cnt), 0), 
        mortAC_55_64=ifelse(sum(a55_64*adult_all_cnt), 1e5*sum(dead55_64)/sum(a55_64*0.01*adult_all_cnt), 0)
    ) %>%
    transmute(race_sex,
              mort_=(agesY2K_w_a["a20_34"]*mortAC_20_34 +
                     agesY2K_w_a["a35_44"]*mortAC_35_44 +
                     agesY2K_w_a["a45_54"]*mortAC_45_54 +
                     agesY2K_w_a["a55_64"]*mortAC_55_64 )
              ) %>% 
    pivot_wider(names_from=race_sex, values_from=mort_,
                names_prefix="mortAC",
                values_fill=list('mort_'=0)
                ) %>%
    right_join(base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> base_cnty

c_o_d_cat_by_year %>% filter(year>=2009, year<=2013) %>% 
    select(stateFIPS, cntyFIPS, c_o_d_cat, dead20_34,
           dead35_44, dead45_54, dead55_64, 
           a20_34:a55_64, adult_w_a_cnt, adult_all_cnt) %>%
    group_by(stateFIPS, cntyFIPS, c_o_d_cat) %>%
    summarise(
        mortAC_20_34=ifelse(sum(a20_34*adult_all_cnt), 1e5*sum(dead20_34)/sum(a20_34*0.01*adult_all_cnt), 0),
        mortAC_35_44=ifelse(sum(a35_44*adult_all_cnt), 1e5*sum(dead35_44)/sum(a35_44*0.01*adult_all_cnt), 0), 
        mortAC_45_54=ifelse(sum(a45_54*adult_all_cnt), 1e5*sum(dead45_54)/sum(a45_54*0.01*adult_all_cnt), 0), 
        mortAC_55_64=ifelse(sum(a55_64*adult_all_cnt), 1e5*sum(dead55_64)/sum(a55_64*0.01*adult_all_cnt), 0)
    ) %>%
    transmute(c_o_d_cat,
              mort_=(agesY2K_w_a["a20_34"]*mortAC_20_34 +
                     agesY2K_w_a["a35_44"]*mortAC_35_44 +
                     agesY2K_w_a["a45_54"]*mortAC_45_54 +
                     agesY2K_w_a["a55_64"]*mortAC_55_64 )
              ) %>% 
    pivot_wider(names_from=c_o_d_cat, values_from=mort_,
                names_prefix="mort",
                values_fill = list(mort_=0) ) %>%
    right_join(base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> base_cnty
for (col in paste0("mort", c_o_d_cats) )
    {
        theNAs  <- is.na(base_cnty[[col]])
        base_cnty[theNAs, col] <- 0        
    }
```

... and parallel calculations for the one-year-lagged version, `.base_cnty` (code suppressed).

Age-adjusted pop sizes of county race-sex subgroups.

``` r
interactions_by_year %>% filter(year>=2009, year<=2013) %>% 
    group_by(stateFIPS, cntyFIPS, race_sex) %>%
    summarise(adjcnt_adult_w_a=mean(adjcnt_adult_w_a)) %>% 
    pivot_wider(names_from=race_sex, values_from=adjcnt_adult_w_a,
                names_prefix="adjcnt_",
                values_fill=list('adjcnt_adult_w_a'=0)
                ) %>%
    right_join(base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> base_cnty
```

... and the lagged variant:

``` r
interactions_by_year %>% filter(year>=2008, year<=2012) %>% 
    group_by(stateFIPS, cntyFIPS, race_sex) %>%
    summarise(adjcnt_adult_w_a=mean(adjcnt_adult_w_a)) %>% 
    pivot_wider(names_from=race_sex, values_from=adjcnt_adult_w_a,
                names_prefix="adjcnt_",
                values_fill=list('adjcnt_adult_w_a'=0)
                ) %>%
    right_join(.base_cnty, by = c("stateFIPS", "cntyFIPS")
               ) -> .base_cnty
```

The survey package has a glm function that interprets weights as SLB did. Below is the call I used to replicate their PS using earlier data:

``` r
    survey::svydesign(id=~1, weights=~adult_w_a_cnt, data=base_cnty) %>%
    survey::svyglm(I(stateFIPS==25)~a20_34+a35_44+a45_54+a55_64+
                       male+white_race+black_race+other_race+latino+
                       pov+inc+unemp+unins+
                       mortAC2001+mortAC2002+mortAC2003+
                       mortAC2004+mortAC2005+mortAC2006,
                   design=., data=base_cnty,
                   model=TRUE, # FALSE caused svyglm to choke
                   family=quasibinomial)
```

Shareable extracts
------------------

``` r
base_cnty %>%
  mutate(FIPS=paste0(sprintf('%s', stateFIPS), sprintf('%s', cntyFIPS))) %>%
  select(FIPS, everything() #reorder so FIPS is first variable
         ) -> base_cnty
.base_cnty %>%
  mutate(FIPS=paste0(sprintf('%s', stateFIPS), sprintf('%s', cntyFIPS))) %>%
  select(FIPS, everything() #reorder so FIPS is first variable
         ) -> .base_cnty
```

``` r
base_cnty %>% select(-starts_with("mortAC20")) %>% write_csv("../data/base_cnty.csv")
.base_cnty %>% select(-starts_with("mortAC20")) %>% write_csv("../data/_base_cnty.csv")
```
