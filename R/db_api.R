kmerror.keymissing <- 
    paste("No API key specified. Specify apikey via manifestodb.setapikey()",
          "or go to http://manifesto-project.wzb.eu to create key and/or",
          "account.")

kmurl.apiroot <- "https://manifesto-project.wzb.eu/tools/"

#' Set the API key for the Manifesto Documents Database.
#' 
#' If you do not have an API key for the Manifesto Documents Database,
#' you can create one via your profile page on 
#' \url{http://manifesto-project.wzb.eu}.
#' If you do not have an account, you can register on the webpage.
#'
#' @param key new API key
#' @export
manifestodb.setapikey <- function(key = NA, key.file = NULL) {
  if (!is.null(key.file)) {
    tryCatch({
      fl <- file(key.file)
      key <- readLines(fl, 1, warn = FALSE)      
      # TODO check key?
    }, finally = { close.connection(fl)})
  }
  assign(kapikey, key, envir = manifesto.globalenv)
}
manifestodb.setapikey(NA)
# tryCatch({manifestodb.setapikey(key.file = "manifesto_apikey.txt")},
#          error = function(e) { warning("No key set!") },
#          finally = {NULL})

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
        
      split <- strsplit(misskey, "_")
      party_id <- split[[1]][1]
      election_date <- split[[1]][2]
      
      warning(paste("No ", request, " information for party ", party_id,
                    " at election date ", election_date,
                    " in the Manifesto Project database! ",
                    "Please verify correctness of you query.",
                    sep=""))
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
#' from the internal .csv files (in cache or from the API)
#'
#' @param mpds A data.frame with a main data set version to be formatted
formatmpds <- function(mpds) {
    
  # fix names
  names(mpds) <- tolower(make.names(as.vector(as.matrix(mpds[1,])))) 
  mpds <- mpds[-1,] # names are in first row
  row.names(mpds) <- NULL # or: paste(mpds$party, mpds$date, sep="-")
  
#   print(head(mpds))
  
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

#' gets URL and handles Error
#' 
#' gets the requested url and passes HTTP header error codes on to raise R
#' errors with the same text
#'
#' @param file file to request below apiroot url
#' @param body body text of the posted request: should contain the parameters
#' as specified by the Manifesto Project Database API
manifestodb.request <- function(file, body) {
  
  response <- POST(url=paste0(kmurl.apiroot, file),
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
#' \code{\link{manifesto.corpus}}, 
#' \code{\link{manifesto.originals}}, 
#' \code{\link{manifesto.availability}},  
#' \code{\link{manifesto.maindataset}}.
#'
#' @param type string of \code{"meta", "text", "original", "main", "versions"} 
#'             to indicate type of content to get
#' @param parameters content filter parameters specific to type
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{manifesto.globalenv} is used.
#' 
manifestodb.get <- function(type, parameters=c(), apikey=NULL) {
  
  # check api key
  if (is.null(apikey)) {
    apikey <- get(kapikey, envir = manifesto.globalenv)
  }
  if (is.na(apikey)) {
    stop(kmerror.keymissing)
  }
  
  # prepare parameters
  if (type == kmtype.versions) {
    requestfile <- "api_list_core_versions.json"
  } else if (type == kmtype.main) {
    requestfile <- "api_get_core.json"    
  } else if (type == kmtype.meta) {
    requestfile <- "api_metadata.json"
  } else if (type == kmtype.text) {
    requestfile <- "api_texts_and_annotations.json"
  }
  
  # get content from web
#   requesturl <- paste(kmurl.apiroot, requestfile, "?",
#                       "api_key=", apikey,
#                       "&", toamplist(parameters), sep="")
  jsonstr <- manifestodb.request(file=requestfile,
                                 body=paste0("api_key=", apikey, "&", 
                                             toamplist(parameters)))
  
  # convert to desired format for caching
  if (type == kmtype.versions) {
  
    return(data.frame(fromJSON(jsonstr)))
    
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
