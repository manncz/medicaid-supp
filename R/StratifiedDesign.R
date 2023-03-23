## A set of virtual classes to be used with specific kinds of designs
setClass("RandomizedDesign")
## Stratified design of n units into k strata and c conditions,
## with fixed numbers for each combination of condition and stratum. 
##
## @slot Unit A n by k (sparse) matrix with a single 1 in each row. Rows
##   indicated the randomized units (clusters in cluster randomized trials).
##   Columns indicate stratum membership.
## @slot Condition A c by k integer matrix recording number of units in each combination of condition and stratum
setClass("StratifiedDesign", contains = "RandomizedDesign",
         slots = c(
             Units = "matrix.csr",
             Condition = "matrix"
         ))
## Create stratified design object.
##
##' Either `treated`, `condition` or `z` must be included.
## @param strata A factor of length n, with k levels.
## @param treated (Optional) A integer vector of length k, with names
##   corresponding to levels of strata, indicating treated units. 
## @param condition (Optional) c by k matrix of nonnegative integers, a cross-tabulation of treated and condition
## @param z A logical or factor flagging treatment condition
## @return An object of type "StratifiedDesign"
create_stratified_design <- function(strata, treated = NULL, condition= NULL, z = NULL) {
    stopifnot(!is.null(treated) | is.logical(z) | is.factor(z),
              !is.null(treated) | length(strata)==length(z),             
              is.null(treated) | length(treated)==nlevels(strata),
              is.null(treated) | is.integer(treated),
              (is.null(treated) + is.null(condition) + is.null(z))==2
              )
    n <- length(strata)
    k <- nlevels(strata)

    units <- SparseMMFromFactor(strata)

    if (!is.null(condition)) stop("not implemented yet")
    
    if (!is.null(treated))
        {
            if (!is.null(names(treated))) {
                treated <- treated[levels(strata)]
            }
            count <- as.vector(table(strata))
            condition  <- rbind(as.integer(count-treated),
                                as.integer(treated)
                                )
            colnames(condition)  <- levels(strata)
            rownames(condition)  <- c('TRUE', 'FALSE')
            
        }
              
    if (!is.null(z)) {
        zz  <- as.factor(z)
        conditions  <- SparseMMFromFactor(zz)
        condition  <- t(conditions) %*% units
        condition  <- as.matrix(condition)
        rownames(condition)  <- levels(zz)
        colnames(condition)  <- levels(strata)
    }

    
    new("StratifiedDesign",
        Units   = units,
        Condition = condition)
}


##' Miscellaneous imports from RItools
##'
##' Turn a factor variable into a sparse matrix of 0's and 1's, such that if observation i
##' has the jth level then there is a 1 at position (i,j) (but nowhere else in row i).
##'
##' NA's give rise to rows with no 1s.
##' As the result is only meaningful in the context of the SparseM package,
##' function requires that SparseM be loaded.
##' @title Sparse matrix dummy coding of a factor variable (omitting the intercept)
##' @param thefactor Factor variable, or object inheriting from class factor
##' @return Sparse csr matrix the columns of which are dummy variables for levels of thefactor
##' @export
##' @author Ben Hansen
##' @examples
##' sparse_mod_matrix <-  SparseMMFromFactor(iris$Species)
##' mod_matrix <- model.matrix(~Species-1, iris)
##' all.equal(as.matrix(sparse_mod_matrix),
##'           mod_matrix, check.attributes=FALSE)
SparseMMFromFactor <- function(thefactor) {
  stopifnot(inherits(thefactor, "factor"))
  theNA <- ##if (inherits(thefactor, "optmatch")) !matched(thefactor) else
    is.na(thefactor)

  if (all(theNA)) stop("No non-NA's in thefactor") 

  nlev <- nlevels(thefactor)
  nobs <- length(thefactor)
  theint <- as.integer(thefactor)
  if (any(theNA)) theint[theNA] <- 1L# odd; but note how we compensate in def of ra slot below
  new("matrix.csr",
      ja=theint,
      ia=1L:(nobs+1L),
      ra=(1L-theNA),
      dimension = c(nobs, nlev) #+sum(theNA)
      )
}


##' slm.fit.csr with a fix
##'
##' SparseM's slm.fit.csr has a bug for intercept only models
##' (admittedly, these are generally a little silly to be done as a
##' sparse matrix), but in order to avoid duplicate code, if
##' everything is in a single strata, we use the intercept only model.
##' @param x As slm.fit.csr
##' @param y As slm.fit.csr
##' @param ... As slm.fit.csr
##' @return As slm.fit.csr
##' @importFrom SparseM chol backsolve
slm.fit.csr.fixed <- function (x, y, ...)
{
    if (is.matrix(y))
        {
            n <- nrow(y)
            ycol <- ncol(y)
        } else {
            n <- length(y)
            ycol <- 1
        }
    p <- x@dimension[2]
    if (n != x@dimension[1])
        stop("x and y don't match n")
    chol <- SparseM::chol(t(x) %*% x, ...)
    xy <- t(x) %*% y
    coef <- SparseM::backsolve(chol, xy)

    if (is.vector(coef)) {
      coef <- matrix(coef, ncol = ycol, nrow = p)
  }

    fitted <- as.matrix(x %*% coef)
    resid <- y - fitted
    df <- n - p
    list(coefficients = coef, chol = chol, residuals = resid,
        fitted = fitted, df.residual = df)
}

##' slm.wfit with two fixes
##'
##' slm.wfit shares the intercept-only issue with slm.fit,
##' and in addition has an issue where it carries forward
##' incorrect residuals and fitted values.
##'
##' @param x As slm.wfit
##' @param y As slm.wfit
##' @param weights As slm.wfit
##' @param ... As slm.wfit
##' @return As slm.wfit
##' @importFrom SparseM is.matrix.csr


slm.wfit.csr <- function (x, y, weights, ...) 
{

    if (!is.matrix.csr(x)) 
        stop("model matrix must be in sparse csr mode")
    if (!is.numeric(y)) 
        stop("response must be numeric")
    if (any(weights < 0)) 
        stop("negative weights not allowed")
    contr <- attr(x, "contrasts")
    w <- sqrt(weights)
    wx <- as(w, "matrix.diag.csr") %*% x
    wy <- y * w
    fit <- slm.fit.csr.fixed(wx, wy, ...)

    fit$fitted <- as.matrix(x %*% fit$coef)
    fit$residuals <- y - fit$fitted

    fit$contrasts <- attr(x, "contrasts")
    fit
}
##' Helper function to turn vectors into binary treatment indicators
##'
##' @param x The object to render as a treatment assignment vector of 1s and 0s
##' @return A numeric vector of 1s and 0s
toZ <- function(x) { UseMethod("toZ") }

toZ.numeric <- function(x) { as.numeric(x > 0) }
toZ.logical <- function(x) { as.numeric(x) }
toZ.factor <- function(x) {
    fst <- levels(x)[1]
    as.numeric(fst != x)
}
