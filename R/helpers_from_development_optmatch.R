##* from optmatch / R / match_on.R [issue194-survey  deec9c8]
##'
#' pooled dispersion for a numeric variable
#' 
#' Dispersion as pooled across a treatment and a control group. By default, 
#' the measure of dispersion calculated within each group is not the 
#' ordinary standard deviation as in \code{stats::sd} but rather the robust alternative 
#' encoded in \code{stats::mad}.  The dispersion measurements are combined
#' by squaring, averaging with weights proportional to one minus the sizes of
#' the groups and then taking square roots.  Used in \code{\link{match_on.glm}}. 
#' 
#' A non-NULL \code{svydesign_} parameter indicates that the dispersion 
#' calculations are to be made respecting the weighting scheme implicit in
#' that \code{survey.design2} object. If \code{standardizer} is \code{NULL}, 
#' one gets a calculation in the style of \code{stats::mad} but with weights,
#' performed by \code{optmatch:::svy_sd}; for a pooling of weighted standard 
#' deviations, one would pass a non-\code{NULL} \code{svydesign_} parameter along
#' with \code{standardizer=optmatch:::svy_sd}.
#' (More generally, the provided \code{standardizer}
#' function should accept as a sole argument a \code{survey.design2} object,
#' with \code{nrows(svydesign_$variables)} equal to the lengths of \code{x} and
#' \code{trtgrp}.  This object is expected to carry a numeric variable \sQuote{\code{x}},
#' and the \code{standardizer} function is to return the dispersion of this variable.)
#' 
#' @param x numeric variable
#' @param trtgrp logical or numeric.  If numeric, coerced to `T`/`F` via `!`
#' @param standardizer function, \code{NULL} or numeric of length 1
#' @param svydesign_ ordinarily \code{NULL}, but may also be a \code{survey.design2}; see Details.
#' @value numeric of length 1
#' @export
#' @keywords internal
standardization_scale <- function(x, trtgrp, standardizer = NULL, svydesign_=NULL) 
    {
    stopifnot(is.null(svydesign_) || is(svydesign_, "survey.design2"),
              is.null(standardizer) || is.function(standardizer) || is.numeric(standardizer)
              )
    if (is.numeric(standardizer))
    {
        if (length(standardizer)>1) 
            warning("Multiple element standardizer, only the first is used")
        return(standardizer)
    }
    n_c <- sum(!trtgrp)
    n <- length(x)
    n_t <- n - n_c
    if (is.null(svydesign_))
        {
	if (is.null(standardizer)) standardizer <- stats::mad
	s_c <- standardizer(x[!trtgrp])
	s_t <- standardizer(x[as.logical(trtgrp)])
    } else {
        if (is.null(standardizer)) standardizer <- svy_mad
        des <- update(svydesign_, x=x, trtgrp=as.logical(trtgrp))
        des_t <- subset(des, trtgrp)
        des_c <- subset(des, !trtgrp)
        s_t <- standardizer(des_t)
        s_c <- standardizer(des_c)
    } 
    s2_t <- s_t^2
    s2_c <- s_c^2    
    sqrt(((n_t - 1) * s2_t +
          (n_c - 1) * s2_c) / (n - 2))
}

svy_mad <- function(design)
{
  med <- as.vector(svyquantile(~x, design, 0.5))
  design <- update(design, 
                   abs_dev=abs( x - med )
  )
  mad <- as.vector(svyquantile(~abs_dev, design, 0.5))
  constant <- formals(stats::mad)$constant
  s2_t <- constant * mad
}

svy_sd <- function(design)
{
  var_ <- svyvar(~x, design)
  sqrt(unname(var_)[1])
}
