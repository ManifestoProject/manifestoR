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
