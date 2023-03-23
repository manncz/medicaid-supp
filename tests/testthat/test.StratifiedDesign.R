library("testthat")
library('SparseM')

context("Stratified Designs")
test_that("Basic stratified designs using either counts or Z", {
    s <- factor(c("A", "B", "A", "B", "B"))
    z <- c(T, T, F, F, T)
    z3  <- factor(c(1,1,0,0,2))
    sn1 <- c(B = 2L, A = 1L)

    sdz <- create_stratified_design(s, z = z)
    sdn <- create_stratified_design(s, treated = sn1)
    sdz3 <- create_stratified_design(s, z = z3)
    
    sm <- matrix(c(1, 0, 1, 0, 0,
                   0, 1, 0, 1, 1), ncol = 2)

    expect_equal(as.matrix(sdz@Units), sm)
    expect_equal(as.matrix(sdn@Units), sm)
    expect_equal(as.matrix(sdz3@Units), sm)    

    expect_equivalent(colSums(sdz@Condition), c(2, 3))
    expect_equivalent(colSums(sdn@Condition), c(2, 3))
    expect_equivalent(colSums(sdz3@Condition), c(2, 3))
    
    expect_equivalent(sdz@Condition[2,], c(1, 2))
    expect_equivalent(sdn@Condition[2,], c(1, 2))
    
    expect_equivalent(sdz3@Condition['0',], c(1, 1))
    expect_equivalent(sdz3@Condition['1',], c(1, 1))
    expect_equivalent(sdz3@Condition['2',], c(0, 1))
})

