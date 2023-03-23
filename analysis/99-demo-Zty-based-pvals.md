# Demonstration of moment-based sum statistic p value calculations




Generate y's and a design.

Starting with binary treatment case. 


```r
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
```

Let's have these y variables be lognormal (in
order to have some skewness). 


```r
ys  <- with(dat, matrix(c(exp(x1), exp(x2)),
                        nrow=nrow(dat), 2)
            )
```

Setup for p-value calculations with moment-based
Normal approximation with skewness correction.


```r
sdn  <- create_stratified_design(dat$s, z=dat$z)
cns  <- Zty_cumulants(sdn, ys)
```

P-values via PDQutils package functions.


```r
papx_edgeworth(crossprod(dat$z, ys[,1]),
               raw.cumulants=cns[,1])
```

```
## [1] 0.975
```

```r
papx_edgeworth(crossprod(dat$z, ys[,2]),
               raw.cumulants=cns[,2])
```

```
## [1] 0.862
```

For contrast, the ordinary two-moment
Normal approximation gives:


```r
papx_edgeworth(crossprod(dat$z, ys[,1]),
               raw.cumulants=cns[1:2,1])
```

```
## [1] 0.976
```

```r
papx_edgeworth(crossprod(dat$z, ys[,2]),
               raw.cumulants=cns[1:2,2])
```

```
## [1] 0.862
```

