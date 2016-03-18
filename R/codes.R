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
  gsub("^(\\d{3})\\d$", "\\1", x)
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
  x[x %in% c("202.2", "605.2", "703.2")] <- "0"
  return(gsub("^(\\d{3})\\.\\d$", "\\1", x))
}

#' @method recode_v5_to_v4 ManifestoDocument
#' @export
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

#' Lists of categories and category relations
#' 
#' Code numbers of the Manifesto Project's category scheme. For documentation
#' see \url{https://manifesto-project.wzb.eu/datasets}.
#' 
#' @rdname categories
#' @export 
v4_categories <- function() {
  c("uncod",  101, 102, 103, 104, 105,
    106, 107, 108, 109, 110, 201,
    202, 203, 204, 301, 302, 303,
    304, 305, 401, 402, 403, 404,
    405, 406, 407, 408, 409, 410,
    411, 412, 413, 414, 415, 416,
    501, 502, 503, 504, 505, 506,
    507, 601, 602, 603, 604, 605,
    606, 607, 608, 701, 702, 703,
    704, 705, 706)
}

v5_categories <- function() {
  v5_v4_aggregation_relations() %>%
    unlist() %>%
    { gsub("per", "", .) } %>%
    { gsub("_", ".", .) } %>%
    c(v4_categories()) %>%
    unique() %>%
    sort()
}

#' @rdname categories
#' @export 
v5_v4_aggregation_relations <- function() {
  list(peruncod = c("peruncod", "per202_2", "per605_2", "per703_2"),
       per103 = c("per103", "per103_1", "per103_2"),
       per201 = c("per201", "per201_1", "per201_2"),
       per202 = c("per202", "per202_1", "per202_3", "per202_4"),
       per305 = c("per305", "per305_1", "per305_2", "per305_3", "per305_4", "per305_5", "per305_6"),
       per416 = c("per416", "per416_1", "per416_2"),
       per601 = c("per601", "per601_1", "per601_2"),
       per602 = c("per602", "per602_1", "per602_2"),
       per605 = c("per605", "per605_1"),
       per606 = c("per606", "per606_1", "per606_2"),
       per607 = c("per607", "per607_1", "per607_2", "per607_3"),
       per608 = c("per608", "per608_1", "per608_2", "per608_3"),
       per703 = c("per703", "per703_1")
  )
}

baeck_policy_dimensions <- function() {
  list(foreign = c(101, 102, 103, 106, 107, 108, 109),
       defence = c(104, 105),
       interior = c(201, 202, 203, 204, 301, 302,
                    303, 304, 605, 607, 608),
       justice = c(201, 202, 203, 204, 303, 304, 605),
       finance = c(402, 414),
       economy = c(401, 403, 404, 405, 406, 407, 408, 409, 410, 412, 413, 415),
       labour = c(504, 505, 701, 702),
       education = c(506, 507),
       health = c(504, 505, 706),
       agriculture = c(703),
       industry = c(401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 412, 413, 414),
       environment = c(416, 501),
       social_affairs = c(503, 603, 604, 606, 705, 706)
       ) %>%
    lapply(prefix, "per")
}

bischof_issue_groups <- function() {
  list(
    ecology = c(416, 410, 501, 106),
    nationalist = c(601, 602, 605, 607, 608),
    agrarian = c(703),
    regional = c(301, 302, 706),
    europe = c(406, 407, 108, 110)
    ) %>%
    lapply(prefix, "per")
}

meyer_miller_2013_policy_dimensions <- function() {
  within(baeck_policy_dimensions(), {
    industry <- NULL
  })
}

#' Default programmatic clarity dimensions from 
#' Giebler/Lacewell/Regel/Werner 2015.
#'
#' @references Giebler/Lacewell/Regel/Werner (2015). Mass, Catch-all, or 
#' Programmatic? Toward an Empirical Classification of Party Types. Manuscript.
#'
#' @export
clarity_dimensions <- function() {
  list(
    "fsp" = list(pole_1 = c(101), pole_2 = c(102)),
    "mil" = list(pole_1 = c(104), pole_2 = c(105)),
    "int" = list(pole_1 = c(107), pole_2 = c(109)),
    "eui" = list(pole_1 = c(108), pole_2 = c(110)),
    "con" = list(pole_1 = c(203), pole_2 = c(204)),
    "cen" = list(pole_1 = c(301), pole_2 = c(302)),
    "mre" = list(pole_1 = c(401), pole_2 = c(403)),
    "pro" = list(pole_1 = c(406), pole_2 = c(407)),
    "fis" = list(pole_1 = c(409), pole_2 = c(414)),
    "wel" = list(pole_1 = c(504), pole_2 = c(505)),
    "edu" = list(pole_1 = c(506), pole_2 = c(507)),
    "nwl" = list(pole_1 = c(601), pole_2 = c(602)),
    "tmo" = list(pole_1 = c(603), pole_2 = c(604)),
    "mul" = list(pole_1 = c(607), pole_2 = c(608)),
    "lab" = list(pole_1 = c(701), pole_2 = c(702))
  ) %>%
    lapply(lapply, prefix, "per")
}


#' Aggregate category percentages in groups
#' 
#' General function to aggregate percentage variables by creating a new
#' variable holding the sum. If a variable with the name for the aggregate
#' already exists, it is overwritten, giving a warning if it is changed, not NA,
#' not zero and not named "peruncod".
#' 
#' @param data dataset to use in aggregation
#' @param groups (named) list of variable name vectors to aggregate to a new one
#' (as given in the name); see default value for an example of the format
#' @param na.rm passed on to \code{\link{sum}}
#' @param keep keep variables that were aggregated in result?
#' @param overwrite Names of the variables that are allowed to be overwritten by
#' aggregate. Defaults to all aggregate variable names. If a variable is
#' overwritten, a message is issued in any case.
#' 
#' @export
aggregate_pers <- function(data,
                           groups = v5_v4_aggregation_relations(),
                           na.rm = FALSE,
                           keep = FALSE,
                           overwrite = names(groups)) {
  
  data <- 
    Reduce(function(data, aggregate) {
        aggregated <- data %>%
          select(one_of(intersect(groups[[aggregate]], names(data))))
        if (ncol(aggregated) != 0L) {
          aggregated <- rowSums(aggregated, na.rm = na.rm)
            if (aggregate %in% names(data) &&
                any(!is.na(data[,aggregate]) & 
                    data[,aggregate] != 0.0 & 
                    na_replace(data[,aggregate] != aggregated), TRUE)) {
              if (aggregate %in% overwrite) {
                message(paste0("Changing non-zero supercategory per value ", aggregate, 
                               "when aggregating subcateogory percentages"))
                data[,aggregate] <- aggregated
              }
            } else {
              data[,aggregate] <- aggregated
            }
          }
        data
      },
      names(groups),
      init = data)
  
  if (!keep) {
    data %>%
      select(-one_of(setdiff(unlist(groups), names(groups))))
  } else {
    data
  }
    
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
#' @param include_codes Vector of categories that should be included even if they are not
#' present in the data; the value of the created variables then defaults to 0.0 (or NA if
#' no codes are present at all);
#' @param aggregate_v5_subcategories if TRUE, for handbook version 5 subcategories, the aggregate
#' category's count/percentage is computed as well
#' @return A data.frame with onw row and the counts/percentages as columns
#'
#' @export
count_codes <- function(doc,
                        code_layers = c("cmp_code"),
                        with_eu_codes = "auto",
                        prefix = "per",
                        relative = TRUE,
                        include_codes = if("cmp_code" %in% code_layers)
                                           { v4_categories() } else { c() },
                        aggregate_v5_subcategories = TRUE) {
  UseMethod("count_codes", doc)
}

fix_names_code_table <- function(df, prefix, include_codes) {
  
  ensure_names <- paste0(prefix, include_codes) %>%
    { gsub(".", "_", ., fixed = TRUE)}
  ensure_names <- ensure_names %>%
      subset(!grepl(paste0("$(", prefix, ").+^"), ensure_names)) %>%
    as.character()

  the_order <- order(names(df))
  df %>%
    select(the_order) %>%
    select(matches("party"),
           matches("date"),
           starts_with(prefix),
           one_of(ensure_names),
           matches("total"))
  
}

fix_missing_counted_codes <- function(df, relative = TRUE) {
  
  m <- is.na(df)
  m[which(df$total <= 0),] <- FALSE
  df[m] <- if (relative) 0.0 else 0L
  
  return(df)
  
}

#' @export
count_codes.ManifestoCorpus <- function(doc,
                                        code_layers = c("cmp_code"),
                                        with_eu_codes = "auto",
                                        prefix = "per",
                                        relative = TRUE,
                                        include_codes = if("cmp_code" %in% code_layers)
                                                           { v4_categories() } else { c() },
                                        aggregate_v5_subcategories = TRUE) {
  
  lapply(content(doc),
         count_codes,
         code_layers, with_eu_codes, prefix, relative, include_codes, aggregate_v5_subcategories) %>%
    bind_rows() %>%
    fix_missing_counted_codes(relative = relative) %>%
    fix_names_code_table(prefix,
                         include_codes = include_codes)
  
}

#' @export
count_codes.ManifestoDocument <- function(doc,
                        code_layers = c("cmp_code"),
                        with_eu_codes = "auto",
                        prefix = "per",
                        relative = TRUE,
                        include_codes = if("cmp_code" %in% code_layers)
                                           { v4_categories() } else { c() },
                        aggregate_v5_subcategories = TRUE) {
  
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
    bind_cols(count_codes(the_codes, code_layers, with_eu_codes, prefix, relative, include_codes, aggregate_v5_subcategories))
  
  
}

#' @export
count_codes.default <- function(doc,
                                code_layers = c("cmp_code"),
                                with_eu_codes = "auto",
                                prefix = "per",
                                relative = TRUE,
                                include_codes = if("cmp_code" %in% code_layers)
                                                   { v4_categories() } else { c() },
                                aggregate_v5_subcategories = TRUE) {
  
  tt <- table(doc)

  df <- as.data.frame(t(as.matrix(tt)))
  if (ncol(df) > 0) {
    names(df) <- paste0(prefix, names(df))
    if (relative) {
      n <- sum(df[1,])
      df[1,] <- df[1,]/n * 100
      df$total <- n
    }
  } else {
    df <- data.frame(total = 0L)
  }
  
  df <- aggregate_pers(df,
                       groups = list(peruncod = c("per0", "per000")),
                       keep = FALSE,
                       na.rm = TRUE)

  names(df) <- gsub(".", "_", names(df), fixed = TRUE)
    
  if (length(include_codes) > 0) {
    for (the_name in setdiff(paste0(prefix, gsub(".", "_", include_codes, fixed = TRUE)), names(df))) {
      df[,the_name] <- ifelse(df$total == 0L, NA, 0.0)
    }
  }
  
  if (aggregate_v5_subcategories) {
    df <- aggregate_pers(df,
                         groups = v5_v4_aggregation_relations(),
                         keep = TRUE)
  }

  return(df)    
  
}
