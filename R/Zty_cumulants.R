##" below from https://stackoverflow.com/a/29023189
##" Creates a matrix of size length(x) * factorial(x),
##' so only suitable for smaller x
##' 
##' @param x vector to enumerate permutations of
##' @param prefix used in internal recursion
##' @return matrix of same type as x, cols representing the permutations 
permutations  <- function( x, prefix = c())
{
    if(length(x) == 0 ) return(prefix)
    theperms  <- sapply(1:length(x),
                        FUN = function(idx) permutations( x[-idx],
                                                         c( prefix, x[idx])
                                                         ),
                        simplify = FALSE)
    do.call(cbind, theperms) # to express as a matrix
    }
##' Enumerate all blockwise permutations of vector
##'
##' (For very small stratified setups; otherwise things will
##' quickly get out of hand, clearly.)
##' @title Permutations within blocks
##' @param x matrix or character vector to permute 
##' @param stratifier factor variable encoding blocks
##' @return matrix of same type as x, cols representing permutations
##' @author Hansen
block_permutations  <- function(x, stratifier)
{
    stopifnot(is.numeric(x) | is.character(x),
              is.factor(stratifier), # (in principle `split()` could take list of stratifers...)
              length(x)==length(stratifier)
              )
    n  <- length(x)
    reordered_by_stratum  <- split(1L:n, stratifier, drop=TRUE)
    undo_reorder <- match(1L:n, unlist(reordered_by_stratum))

    ns_by_stratum  <- sapply(reordered_by_stratum, length)
    tot_assignments  <- sapply(ns_by_stratum, factorial)
    tot_assignments  <- prod(tot_assignments)
    ans  <- if (is.numeric(x)) { numeric(tot_assignments*n)
            } else character(tot_assignments*n)
    dim(ans)  <- c(n, tot_assignments)

    cols_in_progress  <- 0L
    ## first block permutations
    n_new_perms  <- factorial(ns_by_stratum[1])
    ans[ undo_reorder[ 1L:ns_by_stratum[1] ],
        1L:n_new_perms
        ]  <- permutations(x[ reordered_by_stratum[[1]] ])
    cols_in_progress  <- cols_in_progress + n_new_perms
    if (length(ns_by_stratum)>1)
        for (b in 2L:length(ns_by_stratum))
        {
            ## extract already-built submatrix before overwriting it
            tmp <- ans[ undo_reorder[ 1:sum(ns_by_stratum[1L:(b-1L)]) ],
                       1L:cols_in_progress, drop=FALSE]
            n_new_perms  <- factorial(ns_by_stratum[b])
            for (rr in 0L:(cols_in_progress-1))
            {
                ans[ undo_reorder[ 1L:sum(ns_by_stratum[1L:b]) ],
                     ( rr * n_new_perms + 1 ):( (rr+1) * n_new_perms)
                    ]  <- permutations(x[ reordered_by_stratum[[b]] ],
                                       prefix=tmp[,rr+1L]
                                       )
            }
            cols_in_progress <- cols_in_progress * n_new_perms
        }
    ans
    }
##' For a particular stratum of a StratifiedDesign, assemble all
##' permutations of conditions, in matrix aligned row-wise w/ units
##' of the design and with as many columns as there are such permutations.
##' Calls for a numeric score to associate with each treatment level.
##'
##' If `stratum` is not provided, assumed to be 1.  If no `scores`, assumed
##' to be integers from 0 up to one minus the number of conditions. 
##' @param stratum length-1 integer or character vector ID-ing stratum of interest
##' @param stratdesign a StratifiedDesign
##' @param scores numeric score for each condition in stratdesign, or NULL
##' @return \eqn{n \times n_s!} numeric matrix, \eqn{n} and \eqn{n_s} being numbers
##' of units overall and in `stratum`, respectively, per `stratdesign`. 
##' 
all_condition_perms_1strat  <- function(stratum, stratdesign, 
                                        scores=NULL
                                        )
{
    stopifnot(is(stratdesign, "StratifiedDesign"),
              length(stratum)==1,
              is.integer(stratum) | is.character(stratum),
              !is.integer(stratum) |
              ( 1<= stratum & stratum <= ncol(stratdesign@Condition) ),
              !is.character(stratum) | 
              stratum %in% colnames(stratdesign@Condition),
              is.null(scores) | is.numeric(scores),
              is.null(scores) | length(scores)==nrow(stratdesign@Condition)
              )
    if (is.character(stratum))
        stratum  <- which(colnames(stratdesign@Condition)==stratum)
    if (is.null(scores)) scores  <- 0:(nrow(stratdesign@Condition)-1)
    
    condition_margins  <- stratdesign@Condition[,stratum, drop=TRUE]
    scores_occurring  <- rep(scores, condition_margins)
    allperms  <- permutations(scores_occurring)
    ## Now we have permutations of n_s scores, corresponding to a subset
    ## of n >= n_s ordered objects.  we'll store these in a dense
    ## matrix with n rows, only n_s of which have nonzero entries.

     thisstratum  <- stratdesign@Units[,stratum]
    thisstratum  <- as.matrix(thisstratum)
    dim(thisstratum)  <- NULL
    this_stratum_positions  <- which(as.logical(thisstratum))
    this_strat_perms  <- matrix(0, nrow(stratdesign@Units), ncol(allperms))
    this_strat_perms[this_stratum_positions,]  <- allperms
    this_strat_perms
    }

Zty_complete_randomization_cumulants_by_brute_force  <-
    function(stratum, stratdesign, scores=NULL, y)
{
    perms_mat  <- all_condition_perms_1strat(stratum, stratdesign, scores=scores)
    zty_values  <- crossprod(y,perms_mat)
    EZty  <- rowMeans(zty_values)
    zty_centered  <- zty_values - EZty
    ## Using mu<k> to denote k-th *central* moments, as in Stuart & Ord, 
    mu2  <- rowMeans(zty_centered^2)
    mu3  <- rowMeans(zty_centered^3)
    mu4  <- rowMeans(zty_centered^4)
    ## so by Stuart & Ord / wikipedia,
    kappa2  <- mu2
    kappa3  <- mu3
    kappa4  <- mu4 - 3*mu2^2
    rbind(kappa1=EZty, kappa2, kappa3, kappa4)
    }
##' Randomization cumulants for sum statistics via stratum-wise
##' enumeration of permutations of the condition variable
##'
##' (The advantage of cumulants, as opposed to moments or central moments,
##'  being that cumulants of any order can simply be added across strata.)
##' @title Randomization 1st-4th cumulants for Z'y, brute force method
##' @param stratdesign a StratifiedDesign
##' @param y numeric vector or p-row matrix, aligned with units of stratdesign
##' @param scores scores to associate with levels of Condition, defaulting to 0,1,...
##' @param simplify logical
##' @return numeric array: 4 by p, if simplify is T; OW 4 by p by no. strata 
##' @author Ben Hansen
Zty_cumulants_by_brute_force  <-
    function(stratdesign, y, scores=NULL, simplify=TRUE)
{
    thestrata  <- 1L:ncol(stratdesign@Condition) 
    cums_by_strata  <-
        lapply(thestrata,
           Zty_complete_randomization_cumulants_by_brute_force,
           stratdesign=stratdesign, scores=scores, y=y)
    N  <- length(dim(cums_by_strata[[1]]))
    if (N==0) N  <- 1
    if (simplify) { Reduce(`+`, cums_by_strata)
    } else {
        abind::abind(cums_by_strata, along=N+1,
                     new.names=c(dimnames(cums_by_strata[[1]]),
                                 dimnames(stratdesign@Condition)[2])
                     )
        }
    }


Zty_complete_randomization_covariance_by_brute_force  <-
    function(stratum, stratdesign, scores=NULL, y)
    {
        perms_mat  <- all_condition_perms_1strat(stratum, stratdesign, scores=scores)
        zty_values  <- crossprod(y,perms_mat)
        EZty  <- rowMeans(zty_values)
        zty_centered  <- zty_values - EZty
        
        (zty_centered) %*% t(zty_centered) / ncol(zty_values)
    }

##' Randomization covariance of sum statistics
##'
##' @title Randomization Cov(Z'y), y a matrix
##' @param stratdesign a StratifiedDesign w/ c experimental conditions
##' @param y numeric vector or p-row matrix
##' @param scores numeric score for each expermental condition, defaults to 0:(c-1)
##' @param simplify logical
##' @return numeric scalar
Zty_cov  <-
    function(stratdesign, y, scores=NULL, simplify=TRUE)
{
    stopifnot(is(stratdesign, "StratifiedDesign"),
              is.numeric(y), 
              is.null(scores) | is.numeric(scores),
              is.null(scores) | length(scores)==nrow(stratdesign@Condition)
              )

    if (is.null(scores)) scores  <- 0:(nrow(stratdesign@Condition)-1)
    
    k  <- ncol(stratdesign@Condition)
    n_st  <- colSums(stratdesign@Condition) # length-k vector
    p  <- if (is.null(dim(y))) 1L else prod(dim(y)[-1]) # in case we can handle 3+dimension arrays
    y_sum_by_stratum  <- t(stratdesign@Units) %*% y # k by p
    y_sum_by_stratum  <- as.matrix(y_sum_by_stratum)
    y_bar_by_stratum  <- y_sum_by_stratum/n_st 
    
    scoresum_by_stratum  <- # length-k vector
        colSums(scores*stratdesign@Condition)
    tmp  <- rep(as.data.frame(y_bar_by_stratum),
                each=p)
    tmp <- as.matrix(as.data.frame(tmp)) 
    y_bar_cross_y_bar_by_stratum  <- # also
        tmp * as.vector(y_bar_by_stratum)       # k by p^2
        
    tmp  <- rep(as.data.frame(y), each=p) 
    tmp  <- as.matrix(as.data.frame(tmp)) #n by p^2
    y_by_y  <- tmp * as.vector(y)
    y_cross_y_sum_by_stratum  <- t(stratdesign@Units) %*% y_by_y #k by p^2
    y_cross_y_sum_by_stratum  <- as.matrix(y_cross_y_sum_by_stratum)
    y_cross_y_bar_by_stratum  <- y_cross_y_sum_by_stratum/n_st 
    sigma_yw_by_stratum  <- y_cross_y_bar_by_stratum - 
        y_bar_cross_y_bar_by_stratum
    
    score_bar_by_stratum  <- scoresum_by_stratum/n_st
    score2_bar_by_stratum  <-
        colSums(scores^2*stratdesign@Condition)/n_st
    sigma2_scores_by_stratum  <-
        score2_bar_by_stratum - score_bar_by_stratum^2
    CovZty_by_stratum  <- (n_st^2/(n_st -1)) *
        sigma2_scores_by_stratum *
        sigma_yw_by_stratum
    ## Small stratum special casing
    ## If the stratum has 1 unit, then Zty - E(Zty)=0.
    CovZty_by_stratum[n_st==1,]  <- 0

    ## reshape
    dim(CovZty_by_stratum)  <- c(k, p, p)
    dimnames(CovZty_by_stratum)  <-
        list(colnames(stratdesign@Condition),
             colnames(y),
             colnames(y)
             )
    if (simplify)
        apply(CovZty_by_stratum,
              seq_along(dim(CovZty_by_stratum))[-1],
              sum
              ) else CovZty_by_stratum
                         
}
##' 
##' @author Ben Hansen
##' Randomization cumulants for sum statistics
##' (The advantage of cumulants, as opposed to moments or central moments,
##'  being that cumulants of any order can simply be added across strata.)
##'
##' With `simplify=TRUE` and p=1, the return value is suitable to be passed
##' to the PDQutils package's function `papx_edgeworth()` as its second
##' argument `raw.cumulants`. Then one has a function similar to
##' `base::pnorm()` but returning Edgeworth-corrected Normal probabilities.
##' 
##' @title Randomization mean, variance for Z'y
##' @param stratdesign a StratifiedDesign w/ c experimental conditions
##' @param y numeric vector or p-row matrix, aligned with units of stratdesign
##' @param scores numeric score for each expermental condition, defaults to 0:(c-1)
##' @param simplify logical
##' @return numeric array: 2 by p, if simplify is T; OW 2 by p by no. strata 
##' @author Ben Hansen
Zty_cumulants  <-
    function(stratdesign, y, scores=NULL, simplify=TRUE)
{
    stopifnot(is(stratdesign, "StratifiedDesign"),
              is.null(scores) | is.numeric(scores),
              is.null(scores) | length(scores)==nrow(stratdesign@Condition)
              )

    if (is.null(scores)) scores  <- 0:(nrow(stratdesign@Condition)-1)
    
    n_st  <- colSums(stratdesign@Condition) # length-k vector
    p  <- if (is.null(dim(y))) 1L else prod(dim(y)[-1]) # in case we can handle 3+dimension arrays
    
    y_sum_by_stratum  <- t(stratdesign@Units) %*% y # k by p
    y_sum_by_stratum  <- as.matrix(y_sum_by_stratum)
    y_bar_by_stratum  <- y_sum_by_stratum/n_st 

    scoresum_by_stratum  <- # length-k vector
        colSums(scores*stratdesign@Condition) 
    EZty_by_stratum  <- y_bar_by_stratum *
        scoresum_by_stratum # k by p
    
    y2_sum_by_stratum  <- t(stratdesign@Units) %*% y^2
    y2_sum_by_stratum  <- as.matrix(y2_sum_by_stratum)
    y2_bar_by_stratum  <- y2_sum_by_stratum/n_st
    sigma2_y_by_stratum  <- y2_bar_by_stratum - y_bar_by_stratum^2

    score_bar_by_stratum  <- scoresum_by_stratum/n_st
    score2_bar_by_stratum  <-
        colSums(scores^2*stratdesign@Condition)/n_st
    sigma2_scores_by_stratum  <-
        score2_bar_by_stratum - score_bar_by_stratum^2
    VarZty_by_stratum  <- (n_st^2/(n_st -1)) *
        sigma2_scores_by_stratum * sigma2_y_by_stratum

    y3_sum_by_stratum  <- t(stratdesign@Units) %*% y^3
    y3_sum_by_stratum  <- as.matrix(y3_sum_by_stratum)
    y3_bar_by_stratum  <- y3_sum_by_stratum/n_st
    mu3_y_by_stratum  <- y3_bar_by_stratum  -
        3*y2_bar_by_stratum*y_bar_by_stratum +
        2*y_bar_by_stratum^3

    score3_bar_by_stratum  <-
        colSums(scores^3*stratdesign@Condition)/n_st
    mu3_scores_by_stratum  <- score3_bar_by_stratum  -
        3*score2_bar_by_stratum*score_bar_by_stratum +
        2*score_bar_by_stratum^3

    mu3_Zty_by_stratum  <-
        (n_st^3 / ( (n_st -1) * (n_st -2) ) )*
        mu3_scores_by_stratum * mu3_y_by_stratum
    ## ###################################
    ## ### Small strata special casing ###
    ## ###################################    
    ## If the stratum has 0 units -- if this can arise, not sure --
    ## then we should regard it as contributing 0 to all cumulants.
    EZty_by_stratum[n_st==0,]  <- 0
    VarZty_by_stratum[n_st==0,]  <- 0
    mu3_Zty_by_stratum[n_st==0,]  <- 0
    ## If the stratum has 1 unit, then Zty - E(Zty)=0.
    VarZty_by_stratum[n_st==1,]  <- 0
    mu3_Zty_by_stratum[n_st==1,]  <- 0
    ## If the stratum has 2 units, then Zty - E(Zty) takes
    ## just two values, which are opposite to one another.
    ## Antisymmetry of the 3rd central moment means it'll be 0. 
    mu3_Zty_by_stratum[n_st==2,]  <- 0

    if (simplify)
        {
            ## Combine stratum-wise cumulants across strata
            EZty  <- colSums(EZty_by_stratum)
            VarZty  <- colSums(VarZty_by_stratum)
            mu3_Zty  <- colSums(mu3_Zty_by_stratum)
            names(EZty)  <- colnames(y)
            rbind(kappa1=EZty, kappa2=VarZty, kappa3=mu3_Zty)
        } else {
            k  <- ncol(y)
            ynames  <- colnames(y)
            if (is.null(ynames))
            {
                ynames  <- 'y'
            }
            ans  <- array(0, dim=c(3, p, length(n_st)),
                          dimnames=list(paste0('kappa',1:3),
                                        ynames,
                                        colnames(stratdesign@Condition)
                                        )
                          )
            ans[1,,]  <- t(EZty_by_stratum)
            ans[2,,]  <- t(VarZty_by_stratum)
            ans[3,,]  <- t(mu3_Zty_by_stratum)
            ans
            }
    }


