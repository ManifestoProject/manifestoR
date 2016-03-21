#' Scaling annotated manifesto documents
#'
#' Since scaling functions such as \code{\link{scale_weighted}} only apply to
#' data.frames with code percentages, the function \code{mp_scale} makes them
#' applies them to a \code{ManifestoCorpus} or \code{ManifestoDocument}.
#'
#' @param data \code{ManifestoDocument} or \code{ManifestoCorpus} with coding
#' annotations or a data.frame with category percentages
#' @param scalingfun a scaling function, i.e. a function that takes a data.frame with
#' category percentages and returns scaled positions, e.g. \code{\link{scale_weighted}}.
#' @param ... further arguments passed on to the scaling function \code{scalingfun},
#' or \code{\link{count_codes}}
#' @param recode_v5_to_v4 recode handbook version 5 scheme to version 4 before scaling; this
#' parameter is only relevant if data is a ManifestoDocument or ManifestoCorpus, but not for 
#' data.frames with code percentages
#' @seealso \code{\link{scale}}
#' @export
mp_scale <- function(data,
                     scalingfun = rile,
                     scalingname = as.character(substitute(scalingfun)),
                     recode_v5_to_v4 = (scalingname == "rile"),
                     ...) {
  UseMethod("mp_scale", data)
}

#' @method mp_scale default
#' @export
mp_scale.default <- function(data,
                             scalingfun = rile,
                             scalingname = as.character(substitute(scalingfun)),
                             recode_v5_to_v4 = (scalingname == "rile"),
                             ...) {
  scalingfun(data, ...)
}

#' @method mp_scale ManifestoDocument
#' @export
mp_scale.ManifestoDocument <- function(data,
        scalingfun = rile,
        scalingname = as.character(substitute(scalingfun)),
        recode_v5_to_v4 = (scalingname == "rile"),
        ...) {

  do.call(document_scaling(scalingfun,
                           returndf = FALSE,
                           scalingname = scalingname,
                           recode_v5_to_v4),
          list(data, ...))

}

#' @method mp_scale ManifestoCorpus
#' @export
mp_scale.ManifestoCorpus <- function(data,
        scalingfun = rile,
        scalingname = as.character(substitute(scalingfun)),
        ...) {

    do.call(corpus_scaling(scalingfun,
                           scalingname = scalingname),
            list(data, ...))

}

default_list <- function(the_names, default_val = 0L) {
  ll <- as.list(rep(default_val, times = length(the_names)))
  names(ll) <- the_names
  return(ll)
}

#' Scaling functions
#' 
#' Scaling functions take a data.frame of variables with information about
#' political parties/text and position the cases on a scale, i.e. output a
#' vector of values. For applying scaling functions directly to text documents,
#' refer to \code{\link{mp_scale}}.
#'
#' \code{scale_weighted} scales the data as a weighted sum of the variable values
#' 
#' @param data A data.frame with cases to be scaled
#' @param vars variable names that should contribute to the linear combination;
#' defaults to all CMP category percentage variables in the Manifesto Project's Main Dataset
#' 
#' @details
#' If variable names used for the definition of the scale
#' are not present in the data frame they are assumed to be 0.
#' \code{scale_weighted} scales the data as a weighted sum of the category percentages
#' 
#' @param weights weights of the linear combination in the same order as `vars`.
#' @seealso \code{\link{mp_scale}}
#' @export
#' @rdname scale
scale_weighted <- function(data,
                     vars = grep("per((\\d{3}(_\\d)?)|\\d{4}|(uncod))$", names(data), value=TRUE),
                     weights = 1) {

  data <- select(data, one_of(vars[vars %in% names(data)]))

  if (is.matrix(weights)) {

    if (is.data.frame(weights)) {
      weights <- weights[vars[vars %in% names(data)]]
    } else {
      weights <- as.data.frame(weights)
      names(weights) <- names(data)
    }

    if (ncol(weights) != ncol(data) || nrow(weights) != nrow(data)) {
      stop("Size of weights matrix does not equal size of data matrix; cannot apply weighting for scaling")
    }

  } else {

    if (!(is.null(names(weights)))) {
      weights <- weights[names(weights) %in% names(data)]
      weights <- c(weights, default_list(setdiff(names(data), names(weights))))
    } else if (length(weights) > 1) {
      weights <- weights[vars %in% names(data)]
    }

    if (is.list(weights)) {
      weights <- as.data.frame(weights)
      if (nrow(weights) == 1 && nrow(data) > 1) {
        weights <- rep(weights, nrow(data))
      }
    } else {
      weights <- matrix(weights, ncol = ncol(data), nrow = nrow(data), byrow = TRUE)
    }

    weights <- as.data.frame(weights)
    if (is.null(names(weights))) {
      names(weights) <- names(data)      
    }
  }

  vars <- vars[vars %in% names(data)]
  data <- select(data, one_of(vars))
  weights <- select(weights, one_of(vars))

  rowSums(data*weights) # apply weighting to rows, sum

}

#' Replicates cases in a data.frame
#' 
#' @param x data.frame to replicate
#' @param times number of replications
#' @param ... unused
#' @return data.frame with cases replicated
#' 
#' @method rep data.frame
rep.data.frame <- function(x, times = 1, ...) {
  if (times == 1) {
    return(x)
  } else {
    return(rbind(x, rep.data.frame(x, times = times - 1, ...)))
  }
}

#' Convert NULL to NA
#' 
#' @param x element
#' @return NA if the element is NULL, the element otherwise
null_to_na <- function(x) {
  if (is.null(x)) {
    return(NA)
  } else {
    return(x)
  }
}


#' \code{scale_logit} scales the data on a logit scale as described by Lowe et al. (2011).
#'
#' @param pos variable names that should contribute to the numerator ("positively")
#' @param neg variable names that should contribute to the denominator ("negatively")
#' @param N vector of numbers of quasi sentences to convert percentages to counts
#' @param zero_offset Constant to be added to prevent 0/0 and log(0); defaults to 0.5 (smaller than any possible non-zero count)
#' @param ... further parameters passed on to \code{\link{scale_weighted}}
#' @references Lowe, W., Benoit, K., Mikhaylov, S., & Laver, M. (2011). Scaling Policy Preferences from Coded Political Texts. Legislative Studies Quarterly, 36(1), 123-155. 
#' @rdname scale
#' @export
scale_logit <- function(data, pos, neg, N = data[,"total"], zero_offset = 0.5, ...) {
  abs.data <- data[,intersect(union(pos, neg), names(data))]/100*unlist(N)  ## convert percentages to counts
  log( (scale_weighted(abs.data, pos, ...) + zero_offset) /
       (scale_weighted(abs.data, neg, ...) + zero_offset) )
}

#' \code{scale_bipolar} scales the data by adding up the variable
#' values in pos and substracting the variable values in neg.
#'
#' @rdname scale
#' @export
scale_bipolar <- function(data, pos, neg, ...) {
  scale_weighted(data,
           vars = c(pos, neg),
           weights = c(rep(1, length(pos)), rep(-1, length(neg))),
           ...)
}

#' \code{scale_ratio} scales the data taking the ratio of the sum of the variable
#' values in pos and the sum of the variable values in neg as suggested by Kim and Fording (1998) and by Laver & Garry (2000).
#'
#' @references Kim, H., & Fording, R. C. (1998). Voter ideology in western democracies, 1946-1989. European Journal of Political Research, 33(1), 73-97.
#' @references Laver, M., & Garry, J. (2000). Estimating Policy Positions from Political Texts. American Journal of Political Science, 44(3), 619-634.
#' @rdname scale
#' @export
scale_ratio <- function(data, pos, neg, ...) {
   scale_bipolar(data, pos = pos, neg = c(), ...) /
      scale_bipolar(data, pos = neg, neg = c(), ...)
}

#' \code{document_scaling} creates a function applicable to
#' a \code{ManifestoDocument} from the scaling function
#'
#' @param returndf if this flag is TRUE, a data.frame with category percentage values,
#' scaling result and, if available party and date is returned by the returned function
#' @param scalingname the name of the scale which will be used as a column name when a data.frame is produced
#' 
#' @export
#' @rdname mp_scale
document_scaling <- function(scalingfun,
                             returndf = FALSE,
                             scalingname = "scaling",
                             recode_v5_to_v4 = FALSE,
                             ...) {
  
  count_codes_loc <- functional::Curry(count_codes, aggregate_v5_subcategories = FALSE, ...)

  return(function(x) {
    
    if (recode_v5_to_v4) {
      x <- recode_v5_to_v4(x)
    }

    df <- count_codes_loc(x)

    df[,scalingname] <- scalingfun(df)

    if (returndf) {
      return(df)
    } else {
      return(df[[scalingname]][1])
    }

  })
}

#' \code{corpus_scaling} creates a function applicable to
#' a \code{ManifestoCorpus} from the scaling function
#' 
#' @export
#' @rdname mp_scale
corpus_scaling <- function(scalingfun, scalingname = "scaling", ...) {

  doc_scale_loc <- document_scaling(scalingfun, ...)

  function(x) {
    scalings <- lapply(content(x), doc_scale_loc)
    
    has_party_date <- unlist(lapply(scalings, function(scale) {
      c("party" %in% names(scale), "date" %in% names(scale))
    }))

    if (all(has_party_date)) {
      return(bind_rows(scalings))
    } else {
      if (any(has_party_date)) {
        warning("Some scaled documents have party and date and some don't!")
      }
      df <- data.frame(party = unlist(lapply(content(x), function(doc) { null_to_na(meta(doc, "party")) })),
                       date = unlist(lapply(content(x), function(doc) { null_to_na(meta(doc, "date")) })),
                       scaling = unlist(scalings))
      names(df)[3] <- scalingname
      return(df)
    }
  }
}


#' Simple linear rescaling of positions
#' 
#' @param pos position data to be rescaled
#' @param newmin indicates the minimum of the new scale (default is -1)
#' @param newmax indicates the maximum of the new scale (default is +1) 
#' @param oldmin indicates the minimum of the existing scale. Can be used to rescale from a known theoretical scale (e.g. -100). If left empty the empirical minimum is used. 
#' @param oldmax indicates the maximum of the existing. See above.
#' @export
rescale <- function(pos,newmin=-1,newmax=1,oldmin=min(pos),oldmax=max(pos)) {
  
  if(newmin>newmax & oldmin>oldmax) {
    stop("newmin > newmax or oldmin > oldmax")
  }
  
  if(!is.numeric(c(pos,newmin, newmax, oldmin, oldmax))) {
    stop("input variables are not numbers")
  }
  
  oldcenter <- (oldmax + oldmin)/2
  oldrange <- oldmax - oldmin
  newcenter <- (newmax + newmin)/2
  newrange <- newmax - newmin
  
  # shift center to zero
  newpos <- pos - oldcenter
  
  # stretch
  newpos <- newpos*newrange/oldrange
  
  # shift to new mean
  newpos <- newpos + newcenter
  
  return(newpos)
}


