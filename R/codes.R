#' Process CMP codings
#' 
#' Several functions to process the CMP codings
#' 
#' @param x Vector of codes, ManifestoDocument or ManifestoCorpus
#' 
#' @details
#' \code{aggregate_cee_codes} Aggregates the sub-categories used
#' in coding several manifestos in Central and Eastern Europe (4 digits) to
#' the main categories in the coding scheme (3 digits).
#' 
#' @rdname cmp_codes
#' @export
aggregate_cee_codes <- function(x) {
  UseMethod("aggregate_cee_codes", x)
}
#' @rdname cmp_codes
#' @export
aggregate_cee_codes.default <- function(x) {
  x[!is.na(x) & x >= 1000] <- floor(x[!is.na(x) & x >= 1000]/10)
  return(x)
}
#' @rdname cmp_codes
#' @export
aggregate_cee_codes.ManifestoDocument <- function(x) {
  doc <- x
  codes(doc) <- aggregate_cee_codes(codes(doc))
  return(doc)
}
#' @rdname cmp_codes
#' @export
aggregate_cee_codes.ManifestoCorpus <- function(x) {
  tm_map(x, aggregate_cee_codes)
}