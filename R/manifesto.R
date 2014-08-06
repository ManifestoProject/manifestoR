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
#'               environment \code{manifesto.globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @export
#' @examples
#' ## mpds <- manifesto.maindataset()
#' ## head(mpds)
#' 
manifesto.maindataset <- function(version="current", apikey=NULL, cache=TRUE) {
  
  if (version == "current") {
    versions <- manifesto.listversions(apikey=apikey, cache=cache)
    version <- as.character(versions[nrow(versions), "datasets.id"]) # TODO date in dataset
  }
  
  parameters <- list(key=version)

  mpds <- viacache(manifestodb.get(kmtype.main, parameters=parameters,
                                   apikey=apikey),
                   filename=cachefilename(kmtype.main, parameters),
                   usecache=cache)
  
  ## format for accesibility
  mpds <- formatmpds(mpds)
  
  return(mpds)
  
}

#' Format the main data set
#' 
#' Creates the format that is visible to the R user
#' from the internal .csv files (in cache or from the API)
#'
#' @param mpds A data.frame with a main data set version to be formatted

formatmpds <- function(mpds) {
  
  names(mpds) <- tolower(names(mpds))
  
  for (name in names(mpds)) {
    
    if (!name %in% c("edate", "countryname", "partyname")) {
      mpds[,name] <- as.numeric(as.character(mpds[,name]))
    }
    
    if (name == "edate") {
      mpds[,name] <- as.Date(as.character(mpds[,name]), format="%m/%d/%Y")
    }
    
  }
  
  return(mpds)
  
}


#' List the available versions of the Manifesto Project's Main Dataset available
#' 
#' @param apikey API key to use, defaults to \code{NULL}, which means the key 
#'               currently stored in the variable \code{apikey} of the
#'               environment \code{manifesto.globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @export
#' @examples
#' ## manifesto.listversions()
manifesto.listversions <- function(apikey=NULL, cache=TRUE) {
  
  versions <- viacache(manifestodb.get(kmtype.versions, apikey=apikey),
                       filename=cachefilename(kmtype.versions),
                       usecache=cache)
  
  return(versions)
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
#'               environment \code{manifesto.globalenv} is used.
#' @param cache Boolean flag indicating whether to use locally cached data if
#'              available.
#' @export
#' @examples
#' ## wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
#' ## manifesto.meta(wanted)
manifesto.meta <- function(ids, apikey=NULL, cache=TRUE) {
  
  # convert ids to parameter list for the api call
  names(ids) <- tolower(names(ids))
  ids <- ids[,intersect(c("party", "date", "edate"), names(ids))]
  
  if (is.null(ids$date)) {
    ids$date <- as.numeric(format(ids$edate, format="%Y%m"))
  }
  
  # for mergeintocache() the call has a data.frame with ids as argument
  call <- function(ids) {
    ids <- paste(ids$party, ids$date, sep="_")
    parameters <- as.list(ids)
    names(parameters) <- rep("keys[]", length(parameters))
    return(manifestodb.get(type = kmtype.meta,
                           parameters = parameters,
                           apikey = apikey))
    
  }
  
  metadata <- mergeintocache(call,
                             filename=cachefilename(kmtype.meta),
                             ids=ids,
                             usecache=cache)
  
  return(metadata)
  
}


## TODO build, document and export
manifesto.texts <- function(ids, apikey=NULL, cache=TRUE) {
  
  # if meta-data complete for the required cases (possibly marked as unavailable) use this;
  # otherwise get missing metadata via manifestodb.get("meta", ...), attach and save to cache
  
  
  # check for all cases in meta-data whether text is available (only if not marked as unavailable) in cache;
  # get missing texts via manifestodb.get("text", ...), attach to cache
  
  
}
