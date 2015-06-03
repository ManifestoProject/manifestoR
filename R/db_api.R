kmerror.keymissing <- 
    paste("No API key specified. Specify apikey via mp_setapikey()",
          "or go to https://manifesto-project.wzb.eu to create key and/or",
          "account.")

kmurl.apiroot <- "https://manifesto-project.wzb.eu/tools/"
kmurl.originalsroot <- "https://manifesto-project.wzb.eu"

#' Set the API key for the Manifesto Documents Database.
#' 
#' If you do not have an API key for the Manifesto Documents Database,
#' you can create one via your profile page on 
#' \url{https://manifesto-project.wzb.eu}.
#' If you do not have an account, you can register on the webpage.
#' 
#' The key is read from the file specified in \code{key.file}. If this
#' argument is \code{NULL}, the key given in the argument \code{key} is used.  
#'
#' @param key new API key
#' @param key.file file name containing the API key
#' @export
mp_setapikey <- function(key.file = NULL, key = NA) {
  if (!is.null(key.file)) {
    tryCatch({
      fl <- file(key.file)
      key <- readLines(fl, 1, warn = FALSE)      
      # check key?
    }, finally = { close.connection(fl)})
  }
  assign(kapikey, key, envir = mp_globalenv)
}
mp_setapikey(key = NA)

toamplist <- function(params) {
  pairs <- paste(names(params), params, sep="=")
  return(Reduce(function(x, y){ paste(x, y, sep="&") }, pairs))
}

formatmetaparams <- function(ids) {
  
  ids <- paste(ids$party, ids$date, sep="_")
  parameters <- as.list(ids)
  names(parameters) <- rep("keys[]", length(parameters))
  
  return(parameters)
  
}

formattextparams <- function(ids) {
  
  parameters <- as.list(ids$manifesto_id)
  names(parameters) <- rep("keys[]", length(parameters))
  
  return(parameters)
  
}

separate_missings <- function(robj, request="") {

  missings <- robj$missing_items

  robj <- robj$items

  for (misskey in missings) {
    
    if (request == "metadata") {

    } else if (request == "text") {
      
      warning(paste0("No document found with id ", misskey, ". ",
                     "This should not happen if you did not request ",
                     "documents by manifesto_ids manually."))
      
    } else {
      
      warning(paste0("No information returned from API for key ", misskey))
      
    }
  }
  
  
  return(robj)
}


#' Format the main data set
#' 
#' Creates the format that is visible to the R user
#' from the internal data.frames files (in cache or from the API)
#'
#' @param mpds A data.frame with a main data set version to be formatted
formatmpds <- function(mpds) {
    
  # fix names
  names(mpds) <- tolower(make.names(as.vector(as.matrix(mpds[1,])))) 
  mpds <- mpds[-1,] # names are in first row
  row.names(mpds) <- NULL # or: paste(mpds$party, mpds$date, sep="-")  

  for (name in names(mpds)) {

    if (!name %in% c("edate", "countryname", "partyname")) {
      mpds[,name] <- as.numeric(as.character(mpds[,name]))
    }

    if (name == "edate") {
      mpds[,name] <- as.Date(as.character(mpds[,name]), format="%d/%m/%Y")
    }

  }

  return(mpds)

}

#' Manifesto Project DB API request
#' 
#' gets the requested url and passes HTTP header error codes on to raise R
#' errors with the same text
#'
#' @param file file to request below apiroot url
#' @param body body text of the posted request: should contain the parameters
#' as specified by the Manifesto Project Database API
mpdb_api_request <- function(file, body) {

  response <- httr::POST(url=paste0(kmurl.apiroot, file),
                         body=body)
  content <- httr::content(response, as="text")
  if (response$status_code != "200") {
    msg <- paste("HTTP Error", response$status_code,
                 "when connecting to Manifesto Corpus Database")
    try({
      msg <- paste0(msg, ": ", fromJSON(content)$error)
    }, silent = TRUE)
    stop(msg)
  } else {
    return(content[1])
  }
}

#' Download content from the Manifesto Database
#' 
#' Internal implementation. For more convenient access and caching use one of 
#' \code{\link{mp_corpus}}, 
#' \code{\link{mp_availability}},  
#' \code{\link{mp_maindataset}}.
#'
#' @param type string of \code{"meta", "text", "original", "main", "versions"} 
#'             to indicate type of content to get
#' @param parameters content filter parameters specific to type
#' @param versionid character string specifying the corpus version to use, format
#'        as returned by \code{\link{mp_corpusversions}} and the API
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{mp_globalenv} is used.
get_mpdb <- function(type, parameters=c(), versionid=NULL, apikey=NULL) {

  # check api key
  if (is.null(apikey)) {
    apikey <- get(kapikey, envir = mp_globalenv)
  }
  if (is.na(apikey)) {
    stop(kmerror.keymissing)
  }


  # select URL
  if (type == kmtype.versions) {
    requestfile <- "api_list_core_versions.json"
  } else if (type == kmtype.main) {
    requestfile <- "api_get_core.json"    
  } else if (type == kmtype.meta) {
    requestfile <- "api_metadata.json"
  } else if (type == kmtype.text) {
    requestfile <- "api_texts_and_annotations.json"
  } else if (type == kmtype.metaversions) {
    requestfile <- "api_list_metadata_versions.json"
  }

  # prepare version parameter if needed
  if (type %in% c(kmtype.meta, kmtype.text)) {
    if (is.null(versionid)) {
      versionid <- last(mp_corpusversions(apikey = apikey))
    }
    parameters <- c(parameters, version = versionid)
  }

  # get content from web
  jsonstr <- mpdb_api_request(file=requestfile,
                              body=paste0("api_key=", apikey, "&",
                                          toamplist(parameters)))

  # convert to desired format (before caching)
  if (type == kmtype.versions) {

    return(data.frame(fromJSON(jsonstr)))

  } else if (type == kmtype.metaversions) {

    return(fromJSON(jsonstr)$versions)    

  } else if (type == kmtype.main) {

    mpds <- formatmpds(data.frame(fromJSON(jsonstr)))
  
    return(mpds)

  } else if (type == kmtype.meta) {

    metadata <- data.frame(separate_missings(fromJSON(jsonstr), request="metadata"))

    if (nrow(metadata) > 0) {
      names(metadata)[which(names(metadata)=="party_id")] <- "party"
      names(metadata)[which(names(metadata)=="election_date")] <- "date"
      
      ## convert types
      metadata <- within(metadata, {
        party <- as.numeric(party)
        date <- as.numeric(date)
      })      
    }

    return(metadata)
  
  } else if (type == kmtype.text) {

    texts <- separate_missings(fromJSON(jsonstr), request="text")
    names(texts)[which(names(texts)=="key")] <- "manifesto_id"
    
    return(texts)

  }
}
