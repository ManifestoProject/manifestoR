#' Generic linear scaling function
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
linear.scaling <- function(data, pos, neg) {
  rowSums(data[,pos]) - rowSums(data[,neg])
}

#' Bipolar scaling function creator
#' 
#' Creates a scaling function by fixing the arguments
#' pos and neg of generic scaling function
#'
#' @param pos codes that should contribute positively
#' @param neg codes that should contribute negatively
#' @param base.fun Generic scaling function to be used
#' @param var.prefix Prefix used for all variable names
#' @export
create.scaling <- function(pos, neg,
                           base.fun = linear.scaling,
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
