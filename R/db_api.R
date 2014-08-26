library(RCurl)
library(jsonlite)

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
manifestodb.setapikey <- function(key) {
  # TODO check key?
  assign(kapikey, key, envir = manifesto.globalenv)
}
manifestodb.setapikey(NA)

toamplist <- function(params) {
  pairs <- paste(names(params), params, sep="=")
  return(Reduce(function(x, y){ paste(x, y, sep="&") }, pairs))
}

separate_missings <- function(robj, request="") {
  
  missings <- robj$missing_items
  
  for (misskey in missings) {
    
    split <- strsplit(misskey, "_")
    party_id <- split[[1]][1]
    election_date <- split[[1]][2]
    
    warning(paste("No ", request, " information for party ", party_id,
                  " at election date ", election_date,
                  " in the Manifesto Project database! ",
                  "Please verify correctness of you query.",
                  sep=""))
  }
  
  robj <- robj$items
  return(robj)
}

extract_text <- function(robj) {
  
}

#' Download content from the Manifesto Database
#' 
#' Internal implementation. For more convenient access and caching use one of 
#' \code{\link{manifesto.texts}}, 
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
  requesturl <- paste(kmurl.apiroot, requestfile, "?",
                      "api_key=", apikey,
                      "&", toamplist(parameters), sep="")
  jsonstr <- getURL(requesturl)
  
  # convert to desired format for caching
  if (type == kmtype.versions) {
  
    return(data.frame(fromJSON(jsonstr)))
    
  } else if (type == kmtype.main) {
    mpds <- data.frame(fromJSON(jsonstr))
    
    # fix names
    names(mpds) <- make.names(as.vector(as.matrix(mpds[1,]))) # names are in first row
    mpds <- mpds[-1,]
    row.names(mpds) <- NULL # or: paste(mpds$party, mpds$date, sep="-")
  
    return(mpds)
    
  } else if (type == kmtype.meta) {
      
    metadata <- data.frame(separate_missings(fromJSON(jsonstr), request="metadata"))
    names(metadata)[which(names(metadata)=="party_id")] <- "party"
    names(metadata)[which(names(metadata)=="election_date")] <- "date"
    
    return(metadata)
  
  } else if (type == kmtype.text) {
    
    texts <- separate_missings(fromJSON(jsonstr), request="text")
    names(texts)[which(names(texts)=="key")] <- "manifesto_id"
    
    return(texts)
      
  }
    
    
  
}
