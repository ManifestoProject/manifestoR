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

rile_r <- c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606)
rile_l <- c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701)

table_to_df <- function(tt, prefix = "per", relative = TRUE) {
  df <- as.data.frame(t(as.matrix(tt)))
  names(df) <- paste0(prefix, names(df))
  if (relative) {
    n <- sum(df[1,])
    df[1,] <- df[1,]/n
    df$total <- n
  }
  return(df)
}

#' Make a scaling function applicable to a ManifestoDocument
#'
#' @param scalingfun a scaling function, taking a data.frame with variables
#' named "per..." and returning a scaling measure (such as the rile) in a vector.
#' @param returndf if this flag is TRUE, a data.frame with category percentage values,
#' scaling result and, if available party and date is returned by the returned function
#' @return a function that takes a ManifestoDocument and computes the scaled value
#' for it
#' 
#' @export
#' @rdname corpus_scaling
document_scaling <- function(scalingfun, returndf = FALSE, scalingname = "scaling") {
  
  return(function(doc) {
  
    df <- data.frame(party=meta(doc, "party"), date=meta(doc, "date"))
    df <- bind_cols(df, table_to_df(table(codes(doc))))

    df[,scalingname] <- scalingfun(df)
    
    if (returndf) {
      return(df)
    } else {
      return(df[1,scalingname])
    }
    
  })
}

#' Make a scaling function applicable to a ManifestoCorpus
#'
#' @param scalingfun a scaling function, taking a data.frame with variables
#' named "per..." and returning a scaling measure (such as the rile) in a vector.
#' @return a function that takes a ManifestoCorpus and returns a data.frame with
#' the code percentages and the scaling function value (e.g. rile) for each document
#' in the corpus
#' 
#' @export
#' @rdname corpus_scaling
corpus_scaling <- function(scalingfun, scalingname = "scaling") {
  function(corpus) {
    df <- data.frame(party = unlist(lapply(content(corpus), function(doc) { meta(doc, "party")})),
               date = unlist(lapply(content(corpus), function(doc) { meta(doc, "date")})),
               scaling = unlist(lapply(content(corpus), document_scaling(scalingfun))))
    names(df)[3] <- scalingname
    df
  }
}

#' RILE
#' 
#' Computes the RILE or other bipolar linear scaling measures for each case in a
#' data.frame or ManifestoCorpus
#'
#' @param data A data.frame with cases to be scaled, variables named "per..."
#' @export
rile <- function(x) { UseMethod("rile", x) }
#' @rdname rile
#' @export
rile.default <- create.scaling(pos=rile_r, neg=rile_l)
#' @rdname rile
#' @export
rile.ManifestoDocument <- document_scaling(rile.default, scalingname = "rile")
#' @rdname rile
#' @export
rile.ManifestoCorpus <- corpus_scaling(rile.default, scalingname = "rile")

#' @rdname rile
logit_rile <- function(x) { UseMethod("logit_rile", x) }
#' @rdname rile
logit_rile.default <- create.scaling(pos=rile_r, neg=rile_l, base.fun=logit.scaling)
#' @rdname rile
logit_rile.ManifestoDocument <- document_scaling(logit_rile.default, scalingname = "logit_rile")
#' @rdname rile
logit_rile.ManifestoCorpus <- corpus_scaling(logit_rile.default, scalingname = "logit_rile")


#' @rdname rile
#' @export
planeco <- function(x) { UseMethod("planeco", x) }
#' @rdname rile
#' @export
planeco.default <- create.scaling(pos=c(403, 404, 412), neg=c())
#' @rdname rile
#' @export
planeco.ManifestoDocument <- document_scaling(planeco.default, scalingname = "planeco")
#' @rdname rile
#' @export
planeco.ManifestoCorpus <- corpus_scaling(planeco.default, scalingname = "planeco")

#' @rdname rile
#' @export
markeco <- function(x) { UseMethod("markeco", x) }
#' @rdname rile
#' @export
markeco.default <- create.scaling(pos=c(401, 414), neg=c())
#' @rdname rile
#' @export
markeco.ManifestoDocument <- document_scaling(markeco.default, scalingname = "markeco")
#' @rdname rile
#' @export
markeco.ManifestoCorpus <- corpus_scaling(markeco.default, scalingname = "markeco")

#' @rdname rile
#' @export
welfare <- function(x) { UseMethod("welfare", x) }
#' @rdname rile
#' @export
welfare.default <- create.scaling(pos=c(503, 504), neg=c())
#' @rdname rile
#' @export
welfare.ManifestoDocument <- document_scaling(welfare.default, scalingname = "welfare")
#' @rdname rile
#' @export
welfare.ManifestoCorpus <- corpus_scaling(welfare.default, scalingname = "welfare")

#' @rdname rile
#' @export
intpeace <- function(x) { UseMethod("intpeace", x) }
#' @rdname rile
#' @export
intpeace.default <- create.scaling(pos=c(102, 105, 106), neg=c())
#' @rdname rile
#' @export
intpeace.ManifestoDocument <- document_scaling(intpeace.default, scalingname = "intpeace")
#' @rdname rile
#' @export
intpeace.ManifestoCorpus <- corpus_scaling(intpeace.default, scalingname = "intpeace")
