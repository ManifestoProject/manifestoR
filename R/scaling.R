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
#' @export
scale_gl <- function(data,
                     vars = grep("per[0-9]+", names(data), value=TRUE),
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
scale_logit <- function(data, pos, neg, N = data[,"total"]) {
  abs.data <- data[,union(pos, neg)]*N
  log(scale_gl(abs.data, pos)/ scale_gl(abs.data, neg))
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
#' @param ... further parameters passed on to \code{link{scale_gl}}
#' @export
scale_bipolar <- function(data, pos, neg, ...) {
  scale_gl(data,
           vars = c(pos, neg),
           weights = c(rep(1, length(pos)), rep(-1, length(neg))),
           ...)
}

#' Bipolar scaling function creator
#' 
#' Convenience function to create a bipolar scaling function
#' by fixing the arguments
#' pos and neg of \code{\link{scale_bipolar}}
#'
#' @param pos codes that should contribute positively
#' @param neg codes that should contribute negatively
#' @param base.fun Generic bipolar scaling function to be used (must have arguments pos and neg)
#' @param var.prefix Prefix used for all variable names
#' @param ... further arguments to be fixed in \code{base.fun}
#' @export
create_scaling <- function(pos, neg,
                           base.fun = scale_bipolar,
                           var.prefix = "per",
                           ...) {
  functional::Curry(base.fun,
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
    df[1,] <- df[1,]/n * 100
    df$total <- n
  }
  return(df)
}

#' Construct text scaling functions
#' 
#' Make a scaling function applicable to a ManifestoDocument
#'
#' @param scalingfun a scaling function, taking a data.frame with variables
#' named "per..." and returning a scaling measure (such as the rile) in a vector.
#' @param returndf if this flag is TRUE, a data.frame with category percentage values,
#' scaling result and, if available party and date is returned by the returned function
#' @param scalingname the name of the scale which will be used as a column name when a data.frame is produced
#' @return \code{document_scaling} returns a function that takes a ManifestoDocument and computes the scaled value
#' for it
#' 
#' @export
#' @rdname corpus_scaling
document_scaling <- function(scalingfun, returndf = FALSE, scalingname = "scaling") {
  
  return(function(x) {
    
    df <- data.frame(party=meta(x, "party"), date=meta(x, "date"))
    df <- bind_cols(df, table_to_df(table(codes(x))))
    
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
#' @return \code{corpus_scaling} returns a function that takes a ManifestoCorpus and returns a data.frame with
#' the code percentages and the scaling function value (e.g. rile) for each document
#' in the corpus
#' 
#' @export
#' @rdname corpus_scaling
corpus_scaling <- function(scalingfun, scalingname = "scaling") {
  function(x) {
    df <- data.frame(party = unlist(lapply(content(x), function(doc) { meta(doc, "party")})),
                     date = unlist(lapply(content(x), function(doc) { meta(doc, "date")})),
                     scaling = unlist(lapply(content(x), document_scaling(scalingfun))))
    names(df)[3] <- scalingname
    df
  }
}

#' RILE
#' 
#' Computes the RILE or other bipolar linear scaling measures for each case in a
#' data.frame or ManifestoCorpus
#'
#' @rdname rile
#' @param x A data.frame with cases to be scaled, variables named "per..."
#' @param ... A ManifestoCorpus or ManifestoDocument with annotated texts to be be scaled
#' @export
rile <- function(x) { UseMethod("rile", x) }
#' @rdname rile
#' @export
rile.default <- create_scaling(pos=rile_r, neg=rile_l)
#' @rdname rile
#' @export
rile.ManifestoDocument <- document_scaling(rile.default, scalingname = "rile")
#' @rdname rile
#' @export
rile.ManifestoCorpus <- corpus_scaling(rile.default, scalingname = "rile")

#' @rdname rile
logit_rile <- function(x) { UseMethod("logit_rile", x) }
#' @rdname rile
logit_rile.default <- create_scaling(pos=rile_r, neg=rile_l, base.fun=scale_logit)
#' @rdname rile
logit_rile.ManifestoDocument <- document_scaling(logit_rile.default, scalingname = "logit_rile")
#' @rdname rile
logit_rile.ManifestoCorpus <- corpus_scaling(logit_rile.default, scalingname = "logit_rile")


#' Vanilla Scaling by Gabel & Huber
#' 
#' Computes scores based on the Vanilla method suggested by Gabel & Huber. 
#' A factor analysis identifies the dominant dimension in the data. 
#' Factor scores using the regression method are then considered as party positions on this dominant dimension. 
#' Gabel, M. J., & Huber, J. D. (2000). Putting Parties in Their Place: Inferring Party Left-Right Ideological Positions from Party Manifestos Data. American Journal of Political Science, 44(1), 94–103.
#'
#' @param a dataframe or matrix
#' @param variable names that should be used for the scaling (usually the variables per101,per102,...)
#' @param invert scores (to change the direction of the dimension to facilitate comparison with other indices) (default is FALSE)

vanilla <- function(data,
                    vars = grep("per[0-9]*", names(data), value=TRUE),
                    invert=FALSE) {
   fa.results <- fa(data[,vars],1,scores="regression")
   vanilla.scores <- fa.results$scores[,1] 
   if (invert==TRUE) vanilla.scores <- vanilla.scores*-1
   return(vanilla.scores)
}