
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
  } else if (type == kmtype.versions) {
    return(file.path(manifesto.getcachelocation(), paste(kversions, ".csv", sep="")))    
  } else if (type == kmtype.meta) {
    return(file.path(manifesto.getcachelocation(), paste(kmetadata, ".csv", sep="")))
  }
  
}


#' Function call via cache 
#' 
#' If \code{usecache==FALE}, \code{call} is executed and its return value is
#' returned.
#' Otherwise \code{call} is executed "via the cache", which means, if there
#' is a file name \code{filename} in the cache, this is read and the content
#' returned, otherwise \code{call} is executed and its result is written to
#' \code{filename} in the cache as well as returned.
#' 
#' @details
#' Return values are data.frames, file formats in the cache are .csv.
#' 
#' @param call call to be executed
#' @param filename name of file in cache where results are found/stored
#' @param usecache can be set to \code{FALSE} to bypass cache functionality.
#' 
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


# a functional programming solution for a simple data.frame filter with combined ids
filterids <- function(data, filter, ids=NULL, setminus=TRUE) {
  
  if (is.null(ids)) {
    ids <- intersect(names(filter), names(data))
  }
  
  reducanddata <- function(left, right) { paste(left, data[,right]) }
  reducandfilter <- function(left, right) { paste(left, filter[,right]) }
  
  dataids <- Reduce(reducanddata, ids, "")
  filterids <- Reduce(reducandfilter, ids, "")
    
  filtered <- which(dataids %in% filterids)
  if (setminus) {
    if (length(filtered)==0) {  # [-] op seems to not work for empty vector
      return(data)
    } else {
      return(data[-filtered,])      
    }
  } else {
    return(data[filtered,])
  }
}

## TODO document (and export?)
mergeintocache <- function(call, filename, ids, usecache=TRUE) {
  
  if (usecache) {
    if (file.exists(filename)) {
      # read from cache
      oldcontent <- read.csv(filename)
      
      # filter all ids which are in oldcontent
      filteredids <- unique(filterids(ids, oldcontent, ids=names(ids)))
      
      # download new ids
      if (nrow(filteredids) > 0) {
        newcontent <- call(filteredids)
        content <- rbind(oldcontent, newcontent)
      } else {
        content <- oldcontent
      }
      
      # write to cache and prepare return value
      write.csv(content, file=filename, row.names=FALSE)
      newcontent <- filterids(content, ids, setminus=FALSE)
      
    } else {
      # download and write to cache
      newcontent <- call(ids)
      write.csv(newcontent, file=filename, row.names=FALSE)
    }
    
  } else {
    newcontent <- call(ids)
  }
  
  return(newcontent)
}

#' Empty the current cache
#' 
#' Empty the current cache
#' 
#' @export
#' @examples
#' ## manifesto.emptycache()
#' 
manifesto.emptycache <- function() {
  system(paste("rm -rf ", manifesto.getcachelocation())) ## remove cache
  ensurecacheexists(manifesto.getcachelocation())
}

#' Copy the current cache
#' 
#' Copy the current cache to a specified location, e.g. for permanently
#' storing the data snapshot used for an analysis
#' 
#' @export
#' @examples
#' ## manifesto.copycache("myproject/manifestofiles")
#' 
manifesto.copycache <- function(destination) {
  system(paste("cp -r", manifesto.getcachelocation(), destination)) ## remove cache
}