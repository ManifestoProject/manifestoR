#' Generalized linear scaling function
#' 
#' Computes the scaling position for the cases
#' in the data.frame data according to the logic of a generalized linear model:
#' the values of the variables are weighted,
#' summed up, and the link function is applied.
#' 
#' @details
#' If variable names used for the definition of the scale
#' are not present in the data frame they are assumed to be 0.
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
  
  weights <- weights[vars %in% names(data)]
  vars <- vars[vars %in% names(data)]
  link.fun(colSums(t(data[,vars])*weights)) # apply weighting to rows, sum, link

}

#' Logit scaling function
#' 
#' Computes the scaling position for the cases
#' in the data.frame data according to logit scaling as in Lowe 2013
#'
#' @param data A data.frame with cases to be scaled
#' @param pos variable names that should contribute to the numerator ("positively")
#' @param neg variable names that should contribute to the denominator ("negatively")
#' @param N vector of numbers of quasi sentences to convert percentages to counts
#' (choose 1 if ) data is already in counts
#' @export
logit.scaling <- function(data, pos, neg, N = data[,"total"]) {
  abs.data <- data[,union(pos, neg)]*N
  log(gl.scaling(abs.data, pos)/ gl.scaling(abs.data, neg))
}

#' Bipolar linear scaling function
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
#' Computes the RILE or other bipolar linear scaling measures for each case in a data.frame
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @export
rile <- create.scaling(pos=rile.r, neg=rile.l)

#' @rdname rile
#' @export
logit.rile <- create.scaling(pos=rile.r, neg=rile.l, base.fun=logit.scaling)

#' @rdname rile
#' @export
planeco <- create.scaling(pos=c(403, 404, 412), neg=c())

#' @rdname rile
#' @export
markeco <- create.scaling(pos=c(401, 414), neg=c())

#' @rdname rile
#' @export
welfare <- create.scaling(pos=c(503, 504), neg=c())

#' @rdname rile
#' @export
intpeace <- create.scaling(pos=c(102, 105, 106), neg=c())