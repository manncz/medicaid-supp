##' # Demonstration of moment-based sum statistic p value calculations
##'
##+ echo=FALSE
library("methods")
library("SparseM")
library("PDQutils")
library("ggplot2")
source("../R/StratifiedDesign.R")
source("../R/Zty_cumulants.R")
set.seed(202011)
##' Generate y's and a design.
##'
##' Starting with binary treatment case. 
##+
n  <- 7
dat <- data.frame(x1=rnorm(n), x2=rnorm(n),
                  s=rep(c("a", "b"),
                        c(floor(n/2), ceiling(n/2))),
                  z=0)
while (with(dat, any( tapply(z, s,
                             function(z) sum(!duplicated(z))
                             ) < 2
                     )
            )
       )
    dat <- transform(dat,
                     z= x1+x2+2*rnorm(n)>0
                     )
##' Let's have these y variables be lognormal (in
##' order to have some skewness). 
ys  <- with(dat, matrix(c(exp(x1), exp(x2)),
                        nrow=nrow(dat), 2)
            )
##' Setup for p-value calculations with moment-based
##' Normal approximation with skewness correction.
sdn  <- create_stratified_design(dat$s, z=dat$z)
cns  <- Zty_cumulants(sdn, ys)
##' ##  p-value calculation examples
##' 
##' P-values via PDQutils package functions.
##+
papx_edgeworth(crossprod(dat$z, ys[,1]),
               raw.cumulants=cns[,1])
papx_edgeworth(crossprod(dat$z, ys[,2]),
               raw.cumulants=cns[,2])
##' For contrast, the ordinary two-moment
##' Normal approximation gives:
papx_edgeworth(crossprod(dat$z, ys[,1]),
               raw.cumulants=cns[1:2,1])
papx_edgeworth(crossprod(dat$z, ys[,2]),
               raw.cumulants=cns[1:2,2])
##' ## QQ plots
##'
##' ### Cornish-Fisher approximation to quantile function
##' 
##' To compare quantiles we use the Cornish-Fisher approximation,
##' which is based on the Edgeworth expansion to the density. Here
##' the ordinary roles of "sample" and "theoretical distribution"
##' are somewhat reversed: our "sample" will be the sum statistic
##' as computed under a complete enumeration of the sample space,
##' while the "theoretical distribution" will be the Edgeworth
##' approximation to the CDF.
##'
##+
all_zs  <- block_permutations(as.numeric(dat$z), dat$s)
all_ztys  <- crossprod(all_zs, ys)
all_ztys  <- as.data.frame(all_ztys)
colnames(all_ztys)  <- paste0('y', 1:2)
ggplot(all_ztys, aes(sample=y1)) +  
    stat_qq(distribution=function(p) qapx_cf(p, raw.cumulants=cns[,1],
                                             support=range(all_ztys)
                                             )
            ) + geom_abline(slope=1, intercept=0, colour="red")
