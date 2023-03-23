For the time being, I'm unable to invoke these tests via
 `devtools::test()`. Instead I'm running them w/ `source()`, after `source()`-ing
 the relevant code files.  Immediate blocker has to do w/ non-recognition of matrix.csr class from within the devtools environment; perhaps that'll go better once the code is set to run as a proper package.

Code setup steps:
```r
library(SparseM)
source("R/StratifiedDesign.R")
source("R/Zty_cumulants.R")
```
After which files here in tests/testthat/ can be source()-ed.
