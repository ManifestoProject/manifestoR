#' Generalized linear scaling function
#' 
#' Computes the scaling position for the cases
#' in the data.frame data according to the logic of a generalized linear model:
#' the values of the variables are weighted,
#' summed up, and the link function is applied
#'
#' @param data A data.frame with cases to be scaled
#' @param vars variable names that should contribute to the linear combination
#' @param weights weights of the linear combination in the same order as `vars`.
#' @param link.fun link function: (vectorized) function applied to the sums
#' (default: none)
#' @export
gl.scaling <- function(data,
                       vars = grep("per.*", names(data), value=TRUE),
                       weights = 1,
                       link.fun = identity) {
  link.fun(colSums(t(data[,vars])*weights)) # apply weighting to rows, sum, link
}


#' Bipolar scaling function
#' 
#' Computes the scaling position for the cases
#' in the data.frame data by adding up the variable
#' values in pos and substracting the variable
#' values in neg.
#'
#' @param data A data.frame with cases to be scaled
#' @param pos variable names that should contribute positively
#' @param neg variable names that should contribute negatively
#' @export
bipolar.scaling <- function(data, pos, neg, ...) {
  gl.scaling(data,
             vars = c(pos, neg),
             weights = c(rep(1, length(pos)), rep(-1, length(neg))),
             ...)
}

#' Bipolar scaling function creator
#' 
#' Convenience function to create a bipolar scaling function
#' by fixing the arguments
#' pos and neg of \code{\link{bipolar.scaling}}
#'
#' @param pos codes that should contribute positively
#' @param neg codes that should contribute negatively
#' @param base.fun Generic bipolar scaling function to be used (must have arguments pos and neg)
#' @param var.prefix Prefix used for all variable names
#' @export
create.scaling <- function(pos, neg,
                           base.fun = bipolar.scaling,
                           var.prefix = "per",
                           ...) {
  Curry(base.fun,
        pos=paste0(var.prefix, pos),
        neg=paste0(var.prefix, neg),
        ...)
}

rile.r <- c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606)
rile.l <- c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701)

#' RILE
#' 
#' Computes the RILE for each case in a data.frame
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @export
rile <- create.scaling(pos=rile.r, neg=rile.l)
