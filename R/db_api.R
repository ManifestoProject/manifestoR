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
  reducand <- function(left, right) {
    paste(left, "&", right, "=", params[right], sep="")
  }
  return(Reduce(reducand, names(params), ""))
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
#' @param saveto folder in which to put downloaded documents, usually the cache 
#' 
manifestodb.get <- function(type, parameters=c(), apikey=NULL, saveto=NULL) {
  
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
  }
  
  # get content from web
  requesturl <- paste(kmurl.apiroot, requestfile, "?",
                      "api_key=", apikey,
                      toamplist(parameters), sep="")
  jsonstr <- getURL(requesturl)
  
  # convert to desired format
  if (type == kmtype.versions) {
    return(data.frame(fromJSON(jsonstr)))
  } else if (type == kmtype.main) {
    mpds <- data.frame(fromJSON(jsonstr))
    names(mpds) <- make.names(as.vector(as.matrix(mpds[1,]))) # names are in first row
    mpds <- mpds[-1,]
    row.names(mpds) <- NULL # or: paste(mpds$party, mpds$date, sep="-")
    return(mpds)
  }
}
