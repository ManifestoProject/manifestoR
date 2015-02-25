#' Access the Manifesto Project's Main Dataset
#' 
#' Gets the Manifesto Project's Main Dataset from the project's web API or
#' the local cache, if it was already downloaded before.
#'
#' @param version Specify the version of the dataset you want to access. Use
#'                "current" to obtain the most recent, or use
#'                \code{\link{manifesto.listversion()}} for a list of available
#'                versions.
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{mp_globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @export
#' @examples
#' ## mpds <- mp_maindataset()
#' ## head(mpds)
#' 
mp_maindataset <- function(version="current", apikey=NULL, cache=TRUE) {
  
  if (version == "current") {
    versions <- mp_coreversions(apikey=apikey, cache=cache)
    version <- as.character(versions[nrow(versions), "datasets.id"]) # TODO date in dataset
  }
  
  parameters <- list(key=version)

  mpds <- get_viacache(kmtype.main, ids = parameters,
                       cache = cache, apikey = apikey)
  
  return(mpds)
  
}


#' List the available versions of the Manifesto Project's Main Dataset
#' 
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{mp_globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @export
#' @examples
#' ## mp_coreversions()
mp_coreversions <- function(apikey=NULL, cache=TRUE) {
  
  versions <- get_viacache(kmtype.versions, apikey=apikey, cache=cache)
  
  return(versions)
}


#' Format ids for web API queru
#' 
#' Formats a data.frame of ids such that it can be used for querying
#' the Manifesto Project Database. That is, it must have non-NA-fields
#' party and date.
#'
#' @param ids ids data.frame, information used: party, date, edate
formatids <- function(ids) {
  
  names(ids) <- tolower(names(ids))
  ids <- ids[,intersect(c("party", "date", "edate"), names(ids))]
  
  suppressWarnings({
    nodate.idxs <- which(is.null(ids$date) | is.na(ids$date))
    ids$date[nodate.idxs] <- as.numeric(format(ids[nodate.idxs,
                                                   "edate"], format="%Y%m"))
  })
  
  n.before <- nrow(ids)
  suppressWarnings(ids <- ids[which(!is.na(ids$party) & !is.na(ids$date)),])
  n.after <- nrow(ids)
  if (n.after < n.before) {
    warning(paste(n.before - n.after, "rows were ommitted from querying the database,",
                  "because they are NULL or NA."))
  }
  
  return(ids)
}

#' Get meta data for election programmes
#' 
#' @details
#' Meta data contain information on the available documents for a given party
#' and election date. This information comprises, language, checksums and links
#' to the text as well as original documents if available.
#' 
#' @param ids list of partys (as ids) and dates of elections, paired. Dates must
#'            be given either in the \code{date} or the \code{edate} variable,
#'            formatted in the way they are in the main data set in this package
#'            (date: as.numeric, YYYYMM, edate: as.Date())
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{mp_globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @export
#' @examples
#' ## wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' ## mp_metadata(wanted)
mp_metadata <- function(ids, apikey=NULL, cache=TRUE) {

  # convert ids to parameter list for the api call
  ids <- formatids(ids)  
  
  metadata <- get_viacache(kmtype.meta,
                             ids=ids,
                             cache=cache,
                             apikey=apikey)

  if (is.null(metadata$manifesto_id)) {
    metadata$manifesto_id <- rep(NA, times = nrow(metadata))
  }

  class(metadata) <- c("ManifestoMetadata", class(metadata))
  
  return(metadata)
  
}

as.metaids <- function(ids, apikey=NULL, cache=TRUE) {
  if ( !("ManifestoMetadata" %in% class(ids)) ) {
    ids <- mp_metadata(ids, apikey=apikey, cache=cache)
  }
  return(ids)
}

is.naorstringna <- function(v) {
  return(is.na(v) | v=="NA")
}

#' Get availability information for election programmes
#' 
#' @details
#' This calls manifest.meta() on the respective ids and hence might add to the
#' cache!
#' 
#' @param ids Information on which documents to get. This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{mp_globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @return an object of class \code{\link{ManifestoAvailability}}
#'         containing availability information
#' @export
#' @examples
#' ## wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' ## avl <- mp_availability(wanted)
#' ## print(avl)
mp_availability <- function(ids, apikey=NULL, cache=TRUE) {
  
  columns <- c("party", "date", "language", "is_primary_doc",
               "may_contradict_core_dataset", "annotations")
  
  metadata <- suppressWarnings(as.metaids(ids, apikey=apikey, cache=cache))
  
  availability <- select(metadata, one_of(columns))

  availability$manifestos <- !is.naorstringna(metadata$manifesto_id)
  availability$originals <- !is.naorstringna(metadata$url_original)

  availability <-
      ids %>%
        select(one_of("party", "date")) %>%
        anti_join(availability, by = c("party", "date")) %>%
        mutate(manifestos = FALSE,
               originals = FALSE,
               annotations  = FALSE,
               language = NA,
               is_primary_doc = NA,
               may_contradict_core_dataset = NA) %>%
        bind_rows(availability)

  availability <- list(query=ids, date=date(), availability=availability)
  class(availability) <- c("ManifestoAvailability", class(availability))
  return(availability)
}


#' Manifesto Availability Information class
#' 
#' Objects returned by \code{\link{mp_availability}}. Use
#' \code{summary} to display information.
#' 
#' @details
#' ManifestoAvailability objects are lists with a key \code{query}, containing
#' the original id set which was queried, and a key \code{availability},
#' containing information derived from the Manifesto Project's document metadata 
#' database about which types of documents are available for the query.
#' 
#' @name ManifestoAvailability
#' @docType class
#' @examples
#' ## wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' ## avl <- mp_availability(wanted)
#' ## summary(avl)
NULL

#' Print availability information of a manifesto document query
#' 
#' @export
print.ManifestoAvailability <- function(x, ...) {
  
  avl <- x ## for better readability but S3 consistency of parameters
  decs <- 3
  
  nqueried <- nrow(unique(avl$query)[,c("party", "date")])
  ncoveredtexts <- length(which(unique(avl$availability[which(
    avl$availability$manifestos),])$manifestos))
  ncovereddocs <- nrow(avl$availability[which(avl$availability$annotations),])
  ncoveredorigs <- length(which(unique(avl$availability[which(
                             avl$availability$originals),])$originals))
  languages <- unique(avl$availability$language)
  
  summary <- list('Queried for'=nqueried,
                  'Raw Texts found'=paste(length(which(avl$availability$manifestos)),
                                          " (", round(100*ncoveredtexts/nqueried, decs), "%)",
                                          sep=""),
                  'Coded Documents found'=paste(length(which(avl$availability$annotations)),
                                      " (", round(100*ncovereddocs/nqueried, decs), "%)",
                                      sep=""),
                  'Originals found'=paste(length(which(avl$availability$originals)),
                                        " (", round(100*ncoveredorigs/nqueried, decs), "%)",
                                        sep=""),
                  Languages=paste(length(languages),
                                  " (", Reduce(paste, languages), ")", sep=""))
  
  class(summary) <- c("summaryDefault", "table")
  print(summary)
  
}

#' Download annotated documents
#' 
#' Download annotated documents from the Manifesto Project Corpus Database
#'
#' @param ids Information on which documents to get. This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{mp_globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @return an object of \code{tm Corpus}'s subclass \code{ManifestoCorpus}
#' @export
#' @examples
#' ## wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' ## corpus <- mp_corpus(wanted)
#' ## summary(corpus)
mp_corpus <- function(ids, apikey=NULL, cache=TRUE) {

  ids <- as.metaids(ids, apikey=apikey, cache=cache)

  ids <- subset(ids, !is.naorstringna(manifesto_id))
  
  if (nrow(ids) > 0) {
    
    texts <- get_viacache(kmtype.text, ids, apikey=apikey, cache=cache)
  
    if (nrow(texts) > 0) {
  
      ## Format the documents into a tm Corpus of ManifestoDocuments
      the.names <- names(texts)
      the.names <- the.names[which(the.names != "items")]
      
      textToManifestoDocument <- function(idx) {    
        the.meta <- structure(as.list(left_join(
          within(texts[idx, the.names], {
            manifesto_id <- as.integer(manifesto_id)
          }), ids, by = "manifesto_id")))
        class(the.meta) <- "TextDocumentMeta"
        items <- texts[idx, "items"][[1]][[1]] ## what the hack...
        names(items)[which(names(items)=="content")] <- "text" ## rename from json
        items[which(is.nacode(items$code)),"code"] <- NA
        suppressWarnings( ## string codes might have become factor
          items[,"code"] <- as.integer(as.character(items[,"code"]))
        ) 
        
        
        elem <- structure(list(content=items, meta=the.meta))
        return(elem)    
      }
      corpus <- ManifestoCorpus(ManifestoSource(lapply(1:nrow(texts),
                                                       textToManifestoDocument)))    
    } else {
      corpus <- ManifestoCorpus(ManifestoSource(c()))
    }
  } else {
    corpus <- ManifestoCorpus(ManifestoSource(c()))
  }
  return(corpus)
  
}

is.nacode <- function(x) {
  return(is.na(x) | as.character(x) %in% c("NA", "n.a."))
}
