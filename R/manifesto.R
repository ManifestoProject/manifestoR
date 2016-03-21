#' Access the Manifesto Project's Main Dataset
#' 
#' Gets the Manifesto Project's Main Dataset from the project's web API or
#' the local cache, if it was already downloaded before.
#' 
#' \code{mp_southamerica_dataset} is a shorthand for getting the Manifesto
#' Project's South America Dataset (it is equivalent to 
#' \code{mp_maindataset(..., south_america = TRUE)}).
#'
#' @param version Specify the version of the dataset you want to access. Use
#'                "current" to obtain the most recent, or use
#'                \code{\link{mp_coreversions}} for a list of available
#'                versions.
#' @param south_america flag whether to download corresponding South America dataset instead of Main Dataset
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @param download_format Download format. If not NULL, instead of the dataset
#' being returned as an R data.frame, a file path to a temporary file in the specified
#' binary format is returned. Can be one of \code{c("dta", "xlsx", "sav")}. With
#' the "dta" option, labeled columns can be obtained.
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
#' \dontrun{
#' mp_maindataset(download_format = "dta") %>% read_dta() ## requires package haven
#' }
#' @export
#' @import base64enc
mp_maindataset <- function(version="current", south_america = FALSE, download_format = NULL, apikey=NULL, cache=TRUE) {
  
  if (version == "current") {
    versions <- mp_coreversions(apikey=apikey, cache=cache)
    version <- as.character(versions[nrow(versions), "datasets.id"]) # TODO date in dataset
  }
  
  if (south_america) {
    if (as.numeric(gsub(".*?(\\d+).*", "\\1", version)) < 2015) {
      warning("No south america dataset available before 2015!")
      return(tbl_df(data.frame()))
    }
    version <- gsub("MPDS", "MPDSSA", version)
  }
  
  parameters <- c(key=version, kind=download_format) %>% as.list()

  mpds <- get_viacache(kmtype.main, ids = parameters,
                       cache = cache, apikey = apikey)
  
  if (!is.null(download_format)) {
    tmp <- tempfile(fileext = paste0(".", download_format))
    mpds %>%
      base64enc::base64decode() %>%
      writeBin(tmp)
    return(tmp)
  } else {
    return(tbl_df(mpds))
  }
  
}

#' @rdname mp_maindataset
#' @param ... all arguments of \code{mp_southamerica_data} are passed on to \code{mp_maindataset}
#' @export
mp_southamerica_dataset <- functional::Curry(mp_maindataset, south_america = TRUE)


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
  ids <- as.metaids(substitute(ids), apikey = apikey, cache = cache, envir = parent.frame(), attach_meta = FALSE)

  # convert ids to parameter list for the api call
  ids <- formatids(ids)  
  
  metadata <- get_viacache(kmtype.meta,
                             ids=ids,
                             cache=cache,
                             apikey=apikey)

  ## type conversion for certain metadata entries
  metadata <- within(metadata, {
    if (exists("manifesto_id", inherits = FALSE)) {
      manifesto_id <- as.character(manifesto_id)
    } else {
      manifesto_id <- as.character(rep(NA, times = nrow(metadata)))
    }
    if (exists("is_primary_doc", inherits = FALSE)) {
      is_primary_doc <- as.logical(is_primary_doc)
    }
    if (exists("may_contradict_core_dataset", inherits = FALSE)) {
      may_contradict_core_dataset <- as.logical(may_contradict_core_dataset)
    }
    if (exists("has_eu_code", inherits = FALSE)) {
      has_eu_code <- as.logical(has_eu_code)
      has_eu_code[is.na(has_eu_code)] <- FALSE
    }
  })
  
  metadata <- tbl_df(metadata)

  class(metadata) <- c("ManifestoMetadata", class(metadata))
  
  return(metadata)
  
}



## ids must be quoted for this function
as.metaids <- function(ids, apikey=NULL, cache=TRUE, envir = parent.frame(n = 2),
                       attach_meta = TRUE,
                       include_southamerica = TRUE) {

  ## non standard evaluation handling
  ## two frames up is where the user was, as.metaids is not exported
  
  id_is_df <- tryCatch(is.data.frame(eval(ids, envir = envir)), error = function(e) { FALSE } )
  

  if (id_is_df) {
    ids <- eval(ids, envir = envir)
  } else {
    
    search_data <- mp_maindataset(apikey = apikey, cache = cache) %>%
      iff(include_southamerica, bind_rows, mp_southamerica_dataset(apikey = apikey, cache = cache)) %>%
      attach_year()
    
    ids <- search_data[eval(ids, envir = search_data,
                                 enclos = envir),]
  } 

  if (attach_meta && !("ManifestoMetadata" %in% class(ids))) {
    ## TODO fix south america disappearance here
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
#'         containing availability information. Can be treated as a
#'         \code{data.frame} and contains detailed availability information
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
  
  metadata <- suppressWarnings(as.metaids(substitute(ids),
                                          apikey=apikey,
                                          cache=cache,
                                          envir = parent.frame()))

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
      metadata %>%
        select(one_of("party", "date")) %>%
        anti_join(availability, by = c("party", "date")) %>%
        mutate(manifestos = FALSE,
               originals = FALSE,
               annotations  = FALSE,
               language = NA) %>%
        bind_rows(availability)

  
  attr(availability, "query") <- metadata
  attr(availability, "date") <- date()
  attr(availability, "corpus_version") <- mp_which_corpus_version()
  
  class(availability) <- c("ManifestoAvailability", class(availability))
  return(availability)
}


#' Manifesto Availability Information class
#' 
#' Objects returned by \code{\link{mp_availability}}.
#' 
#' @details
#' ManifestoAvailability objects are data.frames with variables \code{party}
#' and \code{date} identifying the requested manifestos as in the Manifesto
#' Project's Main & South America Datasets. The additional variables
#' specify whether a machine readable document is available (\code{manifestos}),
#' whether digital CMP coding annotations are available (\code{annotations}) or
#' whether an orignal PDF is available (\code{originals}).
#' 
#' Additional a ManifestoAvailability object has attributes \code{query}, containing
#' the original id set which was queried, \code{corpus_version}, specifying the
#' Corpus version ID used for the query, and \code{date} with the timestamp of the query. 
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
  
  nqueried <- avl %>%
    attr("query") %>%
    select(party, date) %>%
    unique() %>%
    nrow()
    
  ncoveredtexts <- avl %>%
    subset(manifestos) %>%
    unique() %>%
    nrow()
    
  ncovereddocs <- avl %>%
    subset(annotations) %>%
    unique() %>%
    nrow()
  
  ncoveredorigs <- avl %>%
    subset(originals) %>%
    unique() %>%
    nrow()
  
  languages <- stats::na.omit(unique(avl$language))
  
  summary <- list('Queried for'=nqueried,
                  'Corpus Version'=attr(avl, "corpus_version"),
                  'Documents found'=paste(sum(avl$manifestos, na.rm = TRUE),
                                          " (", round(100*ncoveredtexts/nqueried, decs), "%)",
                                          sep=""),
                  'Coded Documents found'=paste(sum(avl$annotations, na.rm = TRUE),
                                      " (", round(100*ncovereddocs/nqueried, decs), "%)",
                                      sep=""),
                  'Originals found'=paste(sum(avl$originals, na.rm = TRUE),
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

  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache, envir = parent.frame())

  if (nrow(ids) > 0) {
    ids <- base::subset(ids, !is.naorstringna(manifesto_id) &
                          (is.null(codefilter) | annotations))
  }
  
  if (nrow(ids) > 0) {
    
    corpus <- get_viacache(kmtype.text, ids, apikey=apikey, cache=cache) %>%
      ManifestoJSONSource(query_meta = ids) %>%
      ManifestoCorpus()
    
  } else {
    
    corpus <- ManifestoCorpus()
    
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
  
  ids <- as.metaids(substitute(ids), apikey=apikey, cache=cache, envir = parent.frame())
  ids <- subset(ids, !is.na(url_original))
  
  if (nrow(ids) > maxn) {
    warning(paste("Attempt to open more than", maxn,
                  "URLs in browser prevented. If you are sure that this",
                  "is what you want, please increase the maxn parameter",
                  "of mp_view_originals"))
  } else {
    for (url in ids$url_original) {
      utils::browseURL(paste0(kmurl.originalsroot, url))
    }
  }

}

#' Print Manifesto Corpus citation information
#'
#' @param corpus_version corpus version for which citation should be printed
#' @param core_versions core version for which citation should be printed
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @export
mp_cite <- function(corpus_version = mp_which_corpus_version(),
                    core_versions = mp_which_dataset_versions(),
                    apikey = NULL) {
  
  cite_message <- kcitemessage
  
  if (is.null(apikey) && is.na(getn("apikey", envir = mp_globalenv))) {
    cite_message <- paste0(cite_message, "\n\n",
        "No API key specified. For generation as well as citation information ",
        "please go to https://manifesto-project.wzb.eu.")
  } else {
    
    if (!is.null(corpus_version) && !is.na(corpus_version)) {
      cite_message <- paste0(cite_message, "\n\n",
                             "You're currently using corpus version ", corpus_version, ", ",
                             "please cite as\n\n",
                             get_citation(corpus_version, kmtype.corpuscitation, apikey = apikey))
      
      corpus_cache <- manifestos_in_cache() %>%
                        select(party, date) %>%
                        mp_metadata(apikey = apikey)
      if (!is.null(corpus_cache) && 
          !is.null(corpus_cache$source)) {
        if(any(corpus_cache$source == "CEMP")) {
          cite_message <- paste0(cite_message, "\n\n",
                                 "You have downloaded uncoded machine-readable manifesto texts, ",
                                 "which have been originally created in the Comparative ",
                                 "Electronic Manifestos Project. ",
                                 "Please cite additionally", "\n\n",
                                 get_citation("CEMP", kmtype.corpuscitation, apikey = apikey))
        }
        if(any(corpus_cache$source == "MZES")) {
          cite_message <- paste0(cite_message, "\n\n",
                                 "You have downloaded uncoded machine-readable manifesto texts, ",
                                 "which have been originally created in cooperation with the ",
                                 "Mannheimer Zentrum fuer Europaeische Sozialforschung.",
                                 "Please cite additionally", "\n\n",
                                 get_citation("MZES", kmtype.corpuscitation, apikey = apikey))
        }
      }
    } else {
      cite_message <- paste0(cite_message, "\n\n",
                             "You're manifestoR cache does not contain any corpus version identification. ",
                             "Please load a cache, download data or specify the corpus version ",
                             "manually in mp_cite() to obtain citation information.")
    }
    
    if (length(core_versions) > 0) {
      cite_message <- paste0(cite_message, "\n\n",
                             "You are using Manifesto Project Dataset version(s) ",
                             paste(core_versions, collapse = ", "), ", please cite as \n\n", 
                             core_versions %>%
                               sapply(get_citation, type = kmtype.corecitation, apikey = apikey) %>% 
                               paste(collapse = "\n\n"))
    }
  }

  message(cite_message)
}