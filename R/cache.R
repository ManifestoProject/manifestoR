
#' Set location of the cache for downloaded Manifesto Project data
#' 
#' @param path new path for the download cache; when this function is never
#'             called, \code{\link{manifesto.getcachelocation()}} will return
#'             the working directory's subfolder \code{manifestofiles}
#' @export
#' @examples
#' ## manifesto.setcachelocation(file.path(getwd(), "manifestofiles"))
#' 
manifesto.setcachelocation <- function(path) {
  assign(kcachelocation, path, envir = manifesto.globalenv)
  if (!is.null(get(kcachelocation, envir = manifesto.globalenv))) {
    ensurecacheexists(path)
  }
}
manifesto.setcachelocation(NULL)

#' Get location of the cache for downloaded Manifesto Project data
#' 
#' Returns the location of the cache and ensures that it exists. When
#' no cache location is set, the working directory's subfolder
#' \code{manifestofiles} will upon call be set, created and returned.
#' 
#' @export
#' @examples
#' ## manifesto.getcachelocation()
#' 
manifesto.getcachelocation <- function() {
  if (is.null(get(kcachelocation, envir = manifesto.globalenv))) {
    manifesto.setcachelocation(file.path(getwd(), kdefaultcachename))
  }
  return(get(kcachelocation, envir = manifesto.globalenv))
}


ensurecacheexists <- function(path) {
  if (!file.exists(path)) {
    dir.create(path=path, recursive=TRUE)    
  }
  textsdir <- file.path(path, ktexts)
  if (!file.exists(textsdir)) {
    dir.create(path=textsdir)    
  }
  originalsdir <- file.path(path, koriginals)
  if (!file.exists(originalsdir)) {
    dir.create(path=originalsdir)    
  }
}

cachefilename <- function(type, parameters=c()) {
  
  if (type == kmtype.main) {
    return(file.path(manifesto.getcachelocation(), paste(kdatasetname, parameters$key, ".csv", sep="")))
  }
  
  if (type == kmtype.versions) {
    return(file.path(manifesto.getcachelocation(), paste(kversions, ".csv", sep="")))    
  }
  
}
