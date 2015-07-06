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
#' \code{recode_v5_to_v4} recode the CMP codings according to
#' the more specialized Coding Handbook Version 5 to the more general
#' categories of Handbook Version 4. Codes 202.2, 605.2 and 703.2 are
#' converted to a 000, while all other subcategory codes with an appended
#' dot and fourth digit are aggregated to the corresponding three-digit 
#' main category.
#' 
#' @export
recode_v5_to_v4 <- function(x) {
  UseMethod("recode_v5_to_v4", x)
}

#' @method recode_v5_to_v4 default
#' @export
recode_v5_to_v4.default <- function(x) {
  x <- as.character(x)
  x[x %in% c("202.2", "605.2", "703.2")] <- 0L
  return(as.integer(gsub("^(\\d{3})\\.\\d$", "\\1", x)))
}

#' @method recode_v5_to_v4 ManifestoDocument
recode_v5_to_v4.ManifestoDocument <- function(x) {
  doc <- x
  codes(doc) <- recode_v5_to_v4(codes(doc))
  return(doc)
}
#' @method recode_v5_to_v4 ManifestoCorpus
#' @export
recode_v5_to_v4.ManifestoCorpus <- function(x) {
  tm_map(x, recode_v5_to_v4)
}



#' Count the codings from a ManifestoDocument
#'
#' @param doc ManifestoDocument, ManifestoCorpus or vector of codes
#' @param code_layers vector of names of code layers to use, defaults to cmp_code; Caution:
#' The layer eu_code is handled separately in the parameter with_eu_codes due to its
#' different logic
#' @param with_eu_codes Whether to include special EU code layer; by default ("auto") taken
#' from the document's metadata
#' @param prefix prefix for naming the count/percentage columns in the resulting data.frame
#' @param relative If true, percentages are returned, absolute counts else
#' @return A data.frame with onw row and the counts/percentages as columns
#'
#' @export
count_codes <- function(doc,
                        code_layers = c("cmp_code"),
                        with_eu_codes = "auto",
                        prefix = "per",
                        relative = TRUE) {
  UseMethod("count_codes", doc)
}

fix_names_code_table <- function(df, prefix) {
  
  the_order <- order(names(df))
  df %>%
    select(the_order) %>%
    select(matches("party"),
           matches("date"),
           starts_with(prefix),
           matches("total"))
  
}

fix_missing_counted_codes <- function(df) {
  
  m <- is.na(df)
  m[which(df$total <= 0),] <- FALSE
  df[m] <- 0L
  
  return(df)
  
}

#' @export
count_codes.ManifestoCorpus <- function(doc,
                                        code_layers = c("cmp_code"),
                                        with_eu_codes = "auto",
                                        prefix = "per",
                                        relative = TRUE) {
  
  lapply(content(doc), count_codes, code_layers, with_eu_codes, prefix, relative) %>%
    bind_rows() %>%
    fix_missing_counted_codes() %>%
    fix_names_code_table(prefix)
  
}

#' @export
count_codes.ManifestoDocument <- function(doc,
                        code_layers = c("cmp_code"),
                        with_eu_codes = "auto",
                        prefix = "per",
                        relative = TRUE) {
  
  if (with_eu_codes == "auto") {
    with_eu_codes <- meta(doc, "has_eu_code")
    if (is.null(with_eu_codes)) {
      with_eu_codes <- FALSE
    }
  }
  the_codes <- c()
  if ("eu_code" %in% code_layers) {
    warning("eu_code is included in code_layers, but should be included via the with_eu_codes parameter to respect its logic!")
  }
  for (layer in code_layers) {
    the_codes <- c(the_codes, as.character(codes(doc, layer)))
  }
  if (length(with_eu_codes) > 0 && with_eu_codes) {
    eu_codes <- codes(doc, "eu_code")
    the_codes <- c(the_codes, eu_codes[!is.na(eu_codes) & eu_codes != 0L])
  }
  
  data.frame(party = null_to_na(meta(doc, "party")),
             date = null_to_na(meta(doc, "date"))) %>%
    bind_cols(count_codes(the_codes, code_layers, with_eu_codes, prefix, relative))
  
  
}

#' @export
count_codes.default <- function(doc,
                                code_layers = c("cmp_code"),
                                with_eu_codes = "auto",
                                prefix = "per",
                                relative = TRUE) {
  
  tt <- table(doc)
  
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
