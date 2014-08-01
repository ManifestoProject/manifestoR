
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


#' Function call via cache 
#' 
#' If \code{usecache==FALE}, \code{call} is executed and its return value is
#' returned.
#' Otherwise \code{call} is executed "via the cache", which means, if there
#' is a file name \code{filename} in the cache, this is read and the content
#' returned, otherwise \codel{call} is executed and its result is written to
#' \code{filename} in the cache as well as returned.
#' 
#' @details
#' Return values are data.frames, file formats in the cache are .csv.
#' 
#' @param call call to be executed
#' @param filename name of file in cache where results are found/stored
#' @param usecache can be set to \code{FALSE} to bypass cache functionality.
#' 
#' @examples
#' ## manifesto.getcachelocation()
#' 
viacache <- function(call, filename, usecache=TRUE) {
  
  if (usecache) {
    if (file.exists(filename)) {
      # read from cache
      content <- read.csv(filename)
    } else {
      # download and write to cache
      content <- call
      write.csv(content, file=filename, row.names=FALSE)
    }
  } else {
    content <- call
  }
  
  return(content)
  
}
