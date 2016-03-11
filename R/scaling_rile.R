#' @export
#' @rdname categories
rile_r <- function() {
  c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606)
}

#' @export
#' @rdname categories
rile_l <- function() {
  c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701)
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
#' @export
rile.default <- function(x) {
  x %>%
    { suppressMessages(aggregate_pers(., overwrite = "peruncod")) } %>%  ## Aggregation from Handbook 5 to 4, peruncod might change
    scale_bipolar(pos = paste0("per", rile_r()), neg = paste0("per", rile_l()))
}

#' @export
rile.ManifestoDocument <- function(x) {
  .Deprecated("mp_scale(doc, scalingfun = rile)")
  f <- document_scaling(rile.default, scalingname = "rile")
  f(x)
}
#' @export
rile.ManifestoCorpus <- function(x) {
  .Deprecated("mp_scale(corpus, scalingfun = rile)")
  f <- corpus_scaling(rile.default, scalingname = "rile")
  f(x)
}

#' @rdname rile
#' @export
logit_rile <- function(x) { UseMethod("logit_rile", x) }
#' @export
logit_rile.default <- functional::Curry(scale_logit,
                                        pos=paste0("per", rile_r()),
                                        neg=paste0("per", rile_l()))
#' @export
logit_rile.ManifestoDocument <- function(x) {
  .Deprecated("mp_scale(doc, scalingfun = logit_rile)")
  f <- document_scaling(logit_rile.default, scalingname = "logit_rile")
  f(x)
}
#' @export
logit_rile.ManifestoCorpus <- function(x) {
  .Deprecated("mp_scale(corpus, scalingfun = logit_rile)")
  f <- corpus_scaling(logit_rile.default, scalingname = "logit_rile")
  f(x)
}
