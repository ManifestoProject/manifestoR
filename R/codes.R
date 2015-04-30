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

#' Count the codings from a ManifestoDocument
#'
#' @param doc ManifestoDocument to use
#' @param with_eu_codes Whether to include special EU code layer; by default taken
#' from the document's metadata
#' @param prefix prefix for naming the count/percentage columns in the resulting data.frame
#' @param relative If true, percentages are returned, absolute counts else
#' @return A data.frame with onw row and the counts/percentages as columns
#'
#' @export
count_codes <- function(doc,
                        with_eu_codes = meta(doc, "has_eu_code"),
                        prefix = "per",
                        relative = TRUE) {

  the_codes <- codes(doc)
  if (length(with_eu_codes) > 0 && with_eu_codes) {
    eu_codes <- codes(doc, "eu_code")
    the_codes <- c(the_codes, eu_codes[!is.na(eu_codes) & eu_codes != 0L])
  }
  tt <- table(the_codes)

  df <- as.data.frame(t(as.matrix(tt)))
  names(df) <- paste0(prefix, names(df))
  if (relative) {
    n <- sum(df[1,])
    df[1,] <- df[1,]/n * 100
    df$total <- n
  }
  return(df)
}