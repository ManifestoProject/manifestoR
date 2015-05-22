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
#' @method aggregate_cee_codes default
#' @export
aggregate_cee_codes.default <- function(x) {
  cee_codes <- grepl("^\\d{4}$", x)
  x[cee_codes] <- floor(as.integer(x[cee_codes])/10)
  return(x)
}
#' @method aggregate_cee_codes ManifestoDocument
aggregate_cee_codes.ManifestoDocument <- function(x) {
  doc <- x
  codes(doc) <- aggregate_cee_codes(codes(doc))
  return(doc)
}
#' @method aggregate_cee_codes ManifestoCorpus
#' @export
aggregate_cee_codes.ManifestoCorpus <- function(x) {
  tm_map(x, aggregate_cee_codes)
}

#' @rdname cmp_codes
#' 
#' @details
#' \code{aggregate_v5_to_v4} aggregates the CMP codings according to
#' the more specialized Coding Handbook Version 5 to the more general
#' categories of Handbook Version 4. Codes 202.2, 605.2 and 703.2 are
#' converted to a 000, while all other subcategory codes with an appended
#' dot and fourth digit are aggregated to the corresponding three-digit 
#' main category.
#' 
#' @export
aggregate_v5_to_v4 <- function(x) {
  UseMethod("aggregate_v5_to_v4", x)
}

#' @method aggregate_v5_to_v4 default
#' @export
aggregate_v5_to_v4.default <- function(x) {
  x <- as.character(x)
  x[x %in% c("202.2", "605.2", "703.2")] <- 0L
  return(as.integer(gsub("^(\\d{3})\\.\\d$", "\\1", x)))
}

#' @method aggregate_v5_to_v4 ManifestoDocument
aggregate_v5_to_v4.ManifestoDocument <- function(x) {
  doc <- x
  codes(doc) <- aggregate_v5_to_v4(codes(doc))
  return(doc)
}
#' @method aggregate_v5_to_v4 ManifestoCorpus
#' @export
aggregate_v5_to_v4.ManifestoCorpus <- function(x) {
  tm_map(x, aggregate_v5_to_v4)
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
  if (ncol(df) > 0) {
    names(df) <- paste0(prefix, names(df))
    if (relative) {
      n <- sum(df[1,])
      df[1,] <- df[1,]/n * 100
      df$total <- n
    }
    return(df)    
  } else {
    if (relative) {
      return(data.frame(total = 0))
    }
    return(df)
  }
}