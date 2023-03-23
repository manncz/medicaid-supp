library("testthat")
context("Permutation enumerators & other helpers")
test_that("helpers for within-stratum permutation distribution", {
    s <- factor(c("A", "B", "A", "B", "B"))
    z <- c(T, T, F, F, T)
    z3  <- factor(c(1,1,0,0,2))
    sdz <- create_stratified_design(s, z = z)
    sdz3 <- create_stratified_design(s, z=z3)

    perms_2  <- permutations(1:2)
    expect_true(setequal(as.data.frame(perms_2),
                         data.frame(1:2, 2:1)
                         )
                )
    perms_3  <- permutations(letters[1:3])
    expect_true(setequal(as.data.frame(perms_3),
                         data.frame(c('a', 'b','c'),
                                    c('a', 'c', 'b'),
                                    c('b', 'a', 'c'),
                                    c('b', 'c', 'a'),
                                    c('c', 'a', 'b'),
                                    c('c', 'b', 'a')
                                    )
                         )
                )

    ## Really these are here to be inspected by hand.
    apzA  <- all_condition_perms_1strat("A", sdz, 0:1)
    apzB  <- all_condition_perms_1strat("B", sdz, 0:1)
    expect_equal(all_condition_perms_1strat("A", sdz), apzA)
    expect_equal(all_condition_perms_1strat("B", sdz), apzB)     
    
    apz3A  <- all_condition_perms_1strat("A", sdz3, 0:2)
    apz3B  <- all_condition_perms_1strat("B", sdz3, 0:2)
    expect_equal(all_condition_perms_1strat("A", sdz3), apz3A)
    expect_equal(all_condition_perms_1strat("B", sdz3), apz3B)
    
})

test_that("block permutations",{
    s <- factor(c("A", "B", "A", "B", "B"))
    x  <- 1:5
    ap1  <- block_permutations(x[1:3], s[1:3])
    expect_true(setequal(as.data.frame(ap1),
                         data.frame(c(1, 2, 3),
                                    c(3, 2, 1)
                                    )
                         )
                )
    ap2  <- block_permutations(x[1:4], s[1:4])
    expect_true(setequal(as.data.frame(ap2),
                         data.frame(c(1,2,3,4),
                                    c(3,2,1,4),
                                    c(1,4,3,2),
                                    c(3,4,1,2)
                                    )
                         )
                )
    ap3  <- block_permutations(x, s)
    expect_true(setequal(as.data.frame(ap3),
                         data.frame(c(1,2,3,4,5),
                                    c(3,2,1,4,5),
                                    c(1,2,3,5,4),
                                    c(3,2,1,5,4),
                                    c(1,4,3,2,5),
                                    c(3,4,1,2,5),
                                    c(1,4,3,5,2),
                                    c(3,4,1,5,2),
                                    c(1,5,3,2,4),
                                    c(3,5,1,2,4),
                                    c(1,5,3,4,2),
                                    c(3,5,1,4,2)  
                                    )
                         )
                )    
    
})

context("Brute force sum statistic (Z'y) cumulants")

test_that("cumulants via full enumeration w/in strata", {
    set.seed(4456)
    n <- 7
    c_  <- 2
    dat <- data.frame(x1=rnorm(n), x2=rnorm(n),
                      s=rep(c("a", "b"), c(floor(n/2), ceiling(n/2))),
                      z=0)
    xes  <- as.matrix(dat[paste0('x',1:2)])
    while (with(dat, any( tapply(z, s,
                                 function(z) sum(!duplicated(z))
                                 ) < 2
                         )
                )
           )
            dat <- transform(dat, z= cut(x1+x2+2*rnorm(n), c_) )

    sdn  <- create_stratified_design(factor(dat$s), z=dat$z)

    cnsa  <- Zty_complete_randomization_cumulants_by_brute_force('a', sdn,
                                                                 y=xes)
    expect_equal(Zty_complete_randomization_cumulants_by_brute_force(1L, sdn,
                                                                     y=xes),
                 cnsa)
    cns0  <- Zty_cumulants_by_brute_force(sdn, xes, simplify=FALSE)
    expect_equal(cnsa, cns0[,,1, drop=TRUE])
    cns1  <- Zty_cumulants_by_brute_force(sdn, xes, simplify=TRUE)
    expect_equal(cns0[,,'a'] + cns0[,,'b'], cns1) # not a surprise
    ## simplify defaults to T, scores defaults to 0:(# conditions -1)
    expect_equal(Zty_cumulants_by_brute_force(sdn, xes, scores=0:(c_-1)),
                 cns1)
    cns1a <- Zty_cumulants_by_brute_force(sdn, dat$x1, simplify=TRUE)
    expect_equivalent(cns1a, cns1[,1])
})
context("Faster sum statistic (Zty) cumulant calcs")
test_that("E(Zty) & V(Zty), two strata",{
    set.seed(4456)
    for (n in seq(3, 15, by=3))
        for (c_  in  2:4)
            replicate(3,
            {
                dat <- data.frame(x1=rnorm(n), x2=rnorm(n),
                                  s=rep(c("a", "b"),
                                        c(floor(n/2), ceiling(n/2))),
                                  z=0)
                dat$s  <- as.factor(dat$s)
                xes <- as.matrix(dat[paste0('x',1:2)])
                while (with(dat, any( tapply(z, s,
                                             function(z) {sum(!duplicated(z)) < min(2, length(z))}
                                             )
                                     )
                            )
                       )
                    dat <- transform(dat, z= cut(x1+x2+2*rnorm(n), c_) )

                sdn  <- create_stratified_design(dat$s, z=dat$z)
                cns_bf_1  <- Zty_cumulants_by_brute_force(sdn, xes,
                                                          simplify=TRUE)
                cns  <- Zty_cumulants(sdn, xes)
                expect_equivalent(cns, cns_bf_1[paste0('kappa',1:3),])
                cns1  <- Zty_cumulants(sdn, dat$x1)
                expect_equivalent(cns1, cns[,1])
                cns0  <- Zty_cumulants(sdn, xes, simplify=FALSE)
                expect_equal(apply(cns0, 1:2, sum), cns)
            })
})

context("Etimated sum statistic covariance matrix")
test_that("Cov(Zty1, Zty2), two strata",{
    set.seed(4456)
    for (n in seq(3, 15, by=3))
        for (c_  in  2:4)
            replicate(3,
                      {
                          dat <- data.frame(x1=rnorm(n), x2=rnorm(n),
                                            s=rep(c("a", "b"),
                                                  c(floor(n/2), ceiling(n/2))),
                                            z=0)
                          dat$s  <- as.factor(dat$s)
                          xes <- as.matrix(dat[paste0('x',1:2)])
                          while (with(dat, any( tapply(z, s,
                                                       function(z) {sum(!duplicated(z)) < min(2, length(z))}
                          )
                          )
                          )
                          )
                              dat <- transform(dat, z= cut(x1+x2+2*rnorm(n), c_) )
                          
                          sdn  <- create_stratified_design(dat$s, z=dat$z)
                          cns  <- Zty_cumulants(sdn, xes)
                          cov  <- Zty_cov(sdn, xes)
                          expect_equivalent(cns[2,], diag(cov))
                      })
})

test_that("Cov(Zty1, Zty2), one strata",{
    set.seed(4456)
    for (n in seq(3, 9, by=3))
        for (c_  in  2:4)
            replicate(3,
                      {
                          dat <- data.frame(x1=rnorm(n), x2=rnorm(n),
                                            s=rep(c("a"), n),
                                            z=0)
                          dat$s  <- as.factor(dat$s)
                          xes <- as.matrix(dat[paste0('x',1:2)])
                          while (with(dat, any( tapply(z, s,
                                                       function(z) {sum(!duplicated(z)) < min(2, length(z))}
                          )
                          )
                          )
                          )
                              dat <- transform(dat, z= cut(x1+x2+2*rnorm(n), c_) )
                          
                          sdn  <- create_stratified_design(dat$s, z=dat$z)
                          cov_bf <- Zty_complete_randomization_covariance_by_brute_force("a", sdn, y =xes)
                          cov  <- Zty_cov(sdn, xes)
                          
                          expect_equivalent(cov, cov_bf)
      
                      })
})

