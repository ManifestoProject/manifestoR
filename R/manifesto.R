#' Access the Manifesto Project's Main Dataset
#' 
#' Gets the Manifesto Project's Main Dataset from the project's web API or
#' the local cache, if it was already downloaded before.
#'
#' @param version Specify the version of the dataset you want to access. Use
#'                "current" to obtain the most recent, or use
#'                \code{\link{mp_coreversions}} for a list of available
#'                versions.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#'              
#' @return The Manifesto Project Main Dataset with classes \code{data.frame} and
#' \code{\link[dplyr]{tbl_df}}
#'
#' @examples
#' \dontrun{
#' mpds <- mp_maindataset()
#' head(mpds)
#' median(subset(mpds, countryname == "Switzerland")$rile, na.rm = TRUE)
#' }
#' @export
mp_maindataset <- function(version="current", apikey=NULL, cache=TRUE) {
  
  if (version == "current") {
    versions <- mp_coreversions(apikey=apikey, cache=cache)
    version <- as.character(versions[nrow(versions), "datasets.id"]) # TODO date in dataset
  }
  
  parameters <- list(key=version)

  mpds <- get_viacache(kmtype.main, ids = parameters,
                       cache = cache, apikey = apikey)
  
  return(tbl_df(mpds))
  
}


#' List the available versions of the Manifesto Project's Main Dataset
#' 
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#'
#' @details
#' For the available versions of the corpus, see \code{\link{mp_corpusversions}}
#'
#' @examples
#' \dontrun{mp_coreversions()}
#' @export
mp_coreversions <- function(apikey=NULL, cache=TRUE) {
  
  versions <- get_viacache(kmtype.versions, apikey=apikey, cache=cache)
  
  return(versions)
}


#' Format ids for web API queries
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
    ids$date[nodate.idxs] <- as.numeric(format(ids[nodate.idxs,]$edate,
                                               format="%Y%m"))
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
#' and election date. This information comprises links to the text as well as
#' original documents if available, language, versions checksums and more.
#' 
#' @param ids list of partys (as ids) and dates of elections, paired. Dates must
#'            be given either in the \code{date} or the \code{edate} variable,
#'            formatted in the way they are in the main data set in this package
#'            (date: as.numeric, YYYYMM, edate: as.Date()), see \code{\link{mp_maindataset}}
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#'              
#' @return an object of class \code{ManifestoMetadata}, subclassing \code{data.frame}
#'         as well as \code{\link[dplyr]{tbl_df}} and containing the requested
#'         metadata in rows per election programme
#' @examples
#' \dontrun{
#' mp_metadata(party == 21221)
#' 
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_metadata(wanted)
#' }
#' @export
mp_metadata <- function(ids, apikey=NULL, cache=TRUE) {

  ## non standard evaluation handling
  ## one frame up is where the user was if we did not get a data.frame

  id_is_df <- tryCatch(is.data.frame(ids), error = function(e) { FALSE } )
  
  if (!id_is_df) {
    ids <- mp_maindataset()[eval(substitute(ids),
                                 envir = mp_maindataset(),
                                 enclos = parent.frame()),]
  }

  # convert ids to parameter list for the api call
  ids <- formatids(ids)  
  
  metadata <- get_viacache(kmtype.meta,
                             ids=ids,
                             cache=cache,
                             apikey=apikey)

  ## type conversion for certain metadata entries
  metadata <- within(metadata, {
    if (exists("manifesto_id")) {
      manifesto_id <- as.character(manifesto_id)
    } else {
      manifesto_id <- NA
    }
    if (exists("is_primary_doc")) {
      is_primary_doc <- as.logical(is_primary_doc)
    }
    if (exists("may_contradict_core_dataset")) {
      may_contradict_core_dataset <- as.logical(may_contradict_core_dataset)
    }
    if (exists("has_eu_code")) {
      has_eu_code <- as.logical(has_eu_code)
      has_eu_code[is.na(has_eu_code)] <- FALSE
    }
  })
  
  metadata <- tbl_df(metadata)

  class(metadata) <- c("ManifestoMetadata", class(metadata))
  
  return(metadata)
  
}

## ids must be quoted for this function
as.metaids <- function(ids, apikey=NULL, cache=TRUE) {

  ## non standard evaluation handling
  ## two frames up is where the user was, as.metaids is not exported
  
  id_is_df <- tryCatch(is.data.frame(eval(ids, envir = parent.frame(n = 2))), error = function(e) { FALSE } )

  if (id_is_df) {
    ids <- eval(ids, envir = parent.frame(n = 2))
  } else {
    ids <- mp_maindataset()[eval(ids, envir = mp_maindataset(),
                                 enclos = parent.frame(n = 2)),]
  } 

  if ( !("ManifestoMetadata" %in% class(ids)) ) {
    ids <- mp_metadata(ids, apikey=apikey, cache=cache)
  }

  if ("is_primary_doc" %in% names(ids)) {
    ids <- subset(ids, is.na(is_primary_doc) | is_primary_doc)
  }

  return(ids)
}

is.naorstringna <- function(v) {
  return(is.na(v) | v=="NA")
}

#' Availability information for election programmes
#' 
#' @param ids Information on which documents to get. This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @return an object of class \code{\link{ManifestoAvailability}}
#'         containing availability information. Accessing \code{$availability}
#'         on it gives a \code{data.frame} with detailed availability information
#'         per document
#' @examples
#' \dontrun{
#' mp_availability(countryname == "New Zealand")
#' 
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_availability(wanted)
#' }
#' @export
mp_availability <- function(ids, apikey=NULL, cache=TRUE) {
  
  columns <- c("party", "date", "language", "annotations")
  
  metadata <- suppressWarnings(as.metaids(substitute(ids), apikey=apikey, cache=cache))
  
  ## handler for non standard evaluation: convert ids to subset of maindataset
  ## one frame up is where the user was if we did not get a data.frame
  id_is_df <- tryCatch(is.data.frame(ids), error = function(e) { FALSE } )
  if (!id_is_df) {
    ids <- mp_maindataset()[eval(substitute(ids),
                                 envir = mp_maindataset(),
                                 enclos = parent.frame()),]
  }

  if (!("language" %in% names(metadata))) {
    metadata <- mutate(metadata, language = NA)
  }
  if (!("annotations" %in% names(metadata))) {
    metadata <- mutate(metadata, annotations = FALSE)
  }
  if (!("url_original" %in% names(metadata))) {
    metadata <- mutate(metadata, url_original = NA)
  }
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
               language = NA) %>%
        bind_rows(availability)

  availability <- list(query=ids, date=date(), availability=availability)
  class(availability) <- c("ManifestoAvailability", class(availability))
  return(availability)
}


#' Manifesto Availability Information class
#' 
#' Objects returned by \code{\link{mp_availability}}.
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
#' \dontrun{
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_availability(wanted)
#' }
NULL

#' @method print ManifestoAvailability
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
  languages <- na.omit(unique(avl$availability$language))
  
  summary <- list('Queried for'=nqueried,
                  'Documents found'=paste(length(which(avl$availability$manifestos)),
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

#' Get documents from the Manifesto Corpus Database
#' 
#' Documents are downloaded from the Manifesto Project Corpus Database. If 
#' CMP coding annotations are available, they are attached to the documents,
#' otherwise raw texts are provided. The documents are cached in the working
#' memory to ensure internal consistency, enable offline use and
#' reduce online traffic.
#' 
#' See \code{\link{mp_save_cache}} for ensuring reproducibility by
#' saving cache and version identifier to the hard drive.
#' See \code{\link{mp_update_cache}} for updating the locally saved content with
#' the most recent version from the Manifesto Project Database API.
#'
#' @param ids Information on which documents to get. This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @param codefilter A vector of CMP codes to filter the documents: only quasi-sentences
#'        with the codes specified in \code{codefilter} are returned. If \code{NULL},
#'        no filtering is applied
#' @param codefilter_layer layer to which the codefilter should apply, defaults to cmp_code
#'              
#' @return an object of \code{\link[tm]{Corpus}}'s subclass
#' \code{\link{ManifestoCorpus}} holding the available of the requested documents
#' @export
#' @examples
#' \dontrun{
#' corpus <- mp_corpus(party == 61620 & rile > 10)
#' 
#' wanted <- data.frame(party=c(41320, 41320), date=c(200909, 201309))
#' mp_corpus(wanted)
#' 
#' mp_corpus(subset(mp_maindataset(), countryname == "France"))
#' 
#' partially_available <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' mp_corpus(partially_available)
#' }
mp_corpus <- function(ids,
                      apikey=NULL,
                      cache=TRUE,
                      codefilter = NULL,
                      codefilter_layer = "cmp_code") {

  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache)

  ids <- base::subset(ids, !is.naorstringna(manifesto_id))
  
  if (nrow(ids) > 0) {
    
    texts <- get_viacache(kmtype.text, ids, apikey=apikey, cache=cache)
  
    if (nrow(texts) > 0) {
  
      ## Format the documents into a tm Corpus of ManifestoDocuments
      the.names <- names(texts)
      the.names <- the.names[which(the.names != "items")]
      
      textToManifestoDocument <- function(idx) {    
        the.meta <- structure(as.list(left_join(
          within(texts[idx, the.names], {
            manifesto_id <- as.character(manifesto_id)
          }), ids, by = "manifesto_id")))
        the.meta$kind <- NULL
        class(the.meta) <- "TextDocumentMeta"
        items <- texts[idx, "items"][[1]][[1]] ## what the hack...
        names(items)[which(names(items)=="content")] <- "text" ## rename from json
        names(items)[which(names(items)=="code")] <- "cmp_code"
        items[which(is.nacode(items$cmp_code)),"cmp_code"] <- NA
        if ("eu_code" %in% names(items)) {
          items[which(is.nacode(items$eu_code)),"eu_code"] <- NA 
        }
        suppressWarnings({ ## string codes might have become factor
          items[,"cmp_code"] <- as.integer(as.character(items[,"cmp_code"]))
          if ("eu_code" %in% names(items)) {
            items[,"eu_code"] <- as.integer(as.character(items[,"eu_code"]))
          }
        }) 
        
        
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
  
  ## codefilter
  if (!is.null(codefilter)) {
    corpus <- tm_map(corpus, function(doc) {
          return(subset(doc, codes(doc, codefilter_layer) %in% codefilter))
      })
  }
  
  return(corpus)
  
}

is.nacode <- function(x) {
  return(is.na(x) | as.character(x) %in% c("NA", "n.a."))
}

#' View original documents from the Manifesto Corpus Database
#' 
#' Original documents are opened in the system's browser window. All original
#' documents are stored on the Manifesto Project Website and the URLs opened
#' are all from this site.
#' 
#'
#' @param ids Information on which originals to view This can either be a
#'            list of partys (as ids) and dates of elections as given to
#'            \code{\link{mp_metadata}} or a \code{ManifestoMetadata} object
#'            (\code{data.frame}) as returned by \code{\link{mp_metadata}}.
#'        Alternatively, ids can be a logical expression specifying a subset of
#'        the Manifesto Project's main dataset. It will be evaluated within the
#'        data.frame returned by \code{\link{mp_maindataset}} such that all its
#'        variables and functions thereof can be used in the expression.
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available. The original documents themselves are not cached locally,
#'              but the metadata required to find them is.
#' @param maxn maximum number of documents to open simultaneously in browser,
#'       defaults to 5.
#' @examples
#' \dontrun{
#' mp_view_originals(party == 41320 & date == 200909)
#' }
#' @export
mp_view_originals <- function(ids, maxn = 5, apikey = NULL, cache = TRUE) {
  
  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache)
  ids <- subset(ids, !is.na(url_original))
  
  if (nrow(ids) > maxn) {
    warning(paste("Attempt to open more than", maxn,
                  "URLs in browser prevented. If you are sure that this",
                  "is what you want, please increase the maxn parameter",
                  "of mp_view_originals"))
  } else {
    for (url in ids$url_original) {
      browseURL(paste0(kmurl.originalsroot, url))
    }
  }

}

#' Print Manifesto Corpus citation information
#'
#' @export
mp_cite <- function() {
  message(paste0(kcitemessage, "\n\n",
                 "You're currently using corpus version ",
                    getn(kmetaversion, envir = mp_cache()), "."))
}