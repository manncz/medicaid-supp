# Supplemental Codes to "Evaluating the Effects of ACA Medicaid Expansion on Mortality with Design-Based Inference and Censored Outcomes"


## Contents

- `analysis/` all scripts - numbers and letters indicate the order in which scripts should be run

  - 00: scripts containing helper functions

      - `00-balance-helpers.R`
      - `00-outliers-helpers.R`
      - `00-read-icd10.R`

  - 01: scripts reading in and cleaning baseline covariates and combining them

    - `01a-base-mortality.R`
    - `01a-state-data-reporting.R`
    - `01b-base-health-ahrf.R`
    - `01c-base-partisanship.R`
    - `01d-base-ihme.R`
    - `01d-base-household.R`
    - `01d-medicaid-exp.R`
    - `01e-base-combined.R`

  - 02: scripts reading in and cleaning mortality and population data for outcome analysis
    + data for main outcome analysis
      - `02-mort-det.R`
      - `02-pop-2013-14.R`  
    + data for EDA and sensitivity analyses
      - `02-mort-det-0514.R`
      - `02-mort-det-65.R`
      - `02-mort-unabm-cause.R`
      - `02-pop-2005-14.R`
      - `02-pop-2015-19.R`
      - `02-pop-65.R`

  - 03: matching analysis and figures presented in protocol

    - `03a-sd-calc.R`
    - `03b-matching.R`
    - `03c-protocol-figures.R`

  - 04: exploration of censoring, reporting lag, and race and ethnicity

    - `04-calc-state-lag.R`
    - `04-cdc-2020-mort.R`
    - `04-cdc-censored-counties.R`
    - `04-race-dist-county.R`

  - 10: preparing data for covariate adjustment models

    - `10-cov-adj-dat-comb.R`
    - `10-mort-pop-comb-2018.R`

  - 11-13: outcome analysis
    - `11-cov-adj-mod.R`
    - `12-caclulate-m.R`
    - `12-partial-order-helpers.R`
    - `12a-conf-int-fun.R`
    - `13-test-stat-calc.R`

  - 14: outcome analysis and exploratory figures
    - `14-eda-figures.R`
    - `14-result-figures.R`

  - 20-22: censored outcome analysis
    - `20-wonder-explore-censoring.R`
    - `20-wonder-helpers.R`
    - `21-wonder-analysis.R`
    - `22-wonder-results.R`

  - 30: paper calculations and figures
    - `30-balance-eval.R`
    - `30-matching-close-figs.R`
    - `30-matching-close-propensity.R`
    - `30-mort-trends.R`
    - `30-toy-example.R`
    - `30-placebo-tests.R`
    - `30-sensitivity-analysis.R`
    - `30-subgroup.R`
    - `30-trimmed-counties.R`
    - `30-wonder-paper-figs.R`

  - 99: additional scripts
    - `99-demo-Zty-based-pvals.R`

- `data/` our data artifacts
  - Includes data artifacts that can be publicly shared


- `raw_data/` input data files
  + Most publicly available data inputs are included
  + See "Raw Data" below

- `reference/` data documentation, etc.
- `R/` helper functions for cumulant calculations
- `lib/` local package library


## Raw Data

Data files with a DUA that doesn't permit free sharing are not included.  Other raw data files are stored in `raw_data`.

Input files that are not included due to size or a DUA, but can be downloaded:

- `raw_data/cc-est2019-alldata.csv`: Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2019 from the US Census Bureau downloaded from https://www.census.gov/newsroom/press-kits/2020/population-estimates-detailed.html
- `raw_data/AHRF2019.acs`: 2018-2019 County Level AHRF data downloaded from https://data.hrsa.gov/data/download
- `raw_data/AHRF2020/AHRF2020.acs`: 2019-2020 County Level AHRF data downloaded from https://data.hrsa.gov/data/download
- `raw_data/pres_2008_2016.csv`: Election data, which contributes to the \% Republican variable was downloaded from [CQ Press Voting and Elections Collection](http://library.cqpress.com/elections/download-data.php) (access through the University of Michigan library). See `reference/notes` for download selections.


Input files that are not included and would need to be requested from the NCHS/CDC through a NCHS restricted data request ([link](https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm)):

- `pop9914.txt`: county-level population from 1999-2014
- `mort9914.txt`: Compressed Mortality Files - All Counties from 1999-2014
- `MULT[year].USAllCnty.txt`: Detailed Mortality Files (Multiple Causes of Death) - All Counties

## R Package Setup


Project specific R package sources go into lib/sources . Then they're installed to an appropriate subdirectory of lib/R . You'll need to create that subdirectory; analysis/.Rprofile indicates where it needs to go.  To install such a package, start R from analysis/ and do e.g.

    install.packages("../lib/source/RItools_0.2.0.9003.tar.gz", repos=NULL)

(Starting R from within analysis/ has the side effect of loading analysis/.Rprofile on startup; read that file to see what this means.) Troubleshooting:

1. Confirm that you did start R from within analysis/, via `getwd()`.
2. Confirm that the package tarball (e.g. RItools_0.2.0.9003.tar.gz) exists at lib/source/RItools_0.2.0.9003.tar.gz .
3. If the `install.packages()` fails with an error message about dependency packages about not being found, try installing the release version from CRAN, then installing the development version over the released version:



    install.packages("RItools", repos="https://cran.case.edu")

    install.packages("../lib/source RItools_0.2.0.9003.tar.gz", repos=NULL)

    packageVersion("RItools") # should be 0.2.0.9003 in this example

Another valid approach would be to jot down the dependency packages it asked for, then feed them directly into `install.packages(., repos="https://your.cran.preference.edu")`.
