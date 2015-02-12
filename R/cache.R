wrap_mp_call <- function(call) {
  function() {
    print("Connecting to Manifesto Project DB API...")
    return(call)
  }
}

clear_env <- function(env) {
  names <- ls(envir = env)
  rm(list = names, envir = env)
  env
}

single_var_caching <- function(varname, call, cache = TRUE, ...) {
  
  if (cache) {
    if (exists(varname, envir = mp_cache)) {
      data <- get(varname, envir = mp_cache)
    } else {
      data <- call()
      assign(varname, data, envir = mp_cache)
    }
  } else {
    data <- call()
  }
  
  return(data)
  
}


#' Get API results via cache 
#' 
#' TODO documentation
#' 
#' @param type type of objects to get (metadata, documents, ...)
#' @param ids identifiers of objects to get. Depending on the type a data.frame or vector of identifiers.
#' 
get_viacache <- function(type, ids = c(), cache = TRUE, ...) {
  
  if (type == kmtype.versions) {
    
    call <- wrap_mp_call(manifestodb.get(kmtype.versions, ...))
    
    return(single_var_caching(kversions, call, cache = cache, ...))
    
  }
  
}


# a functional programming solution for a simple data.frame filter with combined ids
filterids <- function(data, filter, ids=NULL, setminus=TRUE) {
  
  if (is.null(ids)) {
    ids <- intersect(names(filter), names(data))
  }
  ids <- intersect(ids, intersect(names(filter), names(data)))
  
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

writeitemstocache <- function(content, filename) {

#   TODO cache should be rewritten
# 
#   if (nrow(content) != length(filename)) {
#     stop("cannot write data to cache, because number of filenames and data frames do not match!")
#   }
#   
#   for (i in 1:length(filename)) {
#     write.csv(content$items[[i]], file=filename[i], row.names=FALSE)
#   }
  
}

readitemsfromcache <- function(ids, filenames) {
  
  if (nrow(ids) != length(filenames)) {
    stop("cannot write data to cache, because number of filenames and data frames do not match!")
  }
  
  if (nrow(ids) > 0) {
    
    ids$items <- vector("list", nrow(ids)) 
    for (i in 1:nrow(ids)) {
      ids$items[[i]] <- read.csv(filenames[i], stringsAsFactors = FALSE)
    }
    
    return(ids)
    
  } else {
    return(data.frame())
  }
  
}

## TODO document (and export?)
mergeintocache <- function(call, filename, ids, multifile=FALSE, usecache=TRUE) {
    
  if (usecache) {
    
    if (!multifile) {
                     
      if (file.exists(filename)) {
        # read from cache
        oldcontent <- read.csv(filename, stringsAsFactors = FALSE)
        
        # filter all ids which are in oldcontent
        filteredids <- unique(filterids(ids, oldcontent, ids=names(ids)))
        
        # download new ids
        if (nrow(filteredids) > 0) {
          newcontent <- call(filteredids)
          content <- rbind.fill(oldcontent, newcontent)
        } else {
          content <- oldcontent
        }
        
        # write to cache and prepare return value
        write.csv(content, file=filename, row.names=FALSE)
        content <- filterids(content, ids, setminus=FALSE)
      
      } else {
        # download and write to cache
        content <- call(ids)
        write.csv(content, file=filename, row.names=FALSE)
      }
    
    } else { # multifile case
      
      if (!is.null(filename)) {
        newidxs <- which(!file.exists(filename))
        oldidxs <- which(file.exists(filename))
      } else {
        newidxs <- c()
        oldidxs <- c()
      }

      oldcontent <- readitemsfromcache(ids[oldidxs,], filename[oldidxs])

      if (length(newidxs) > 0) {
        newcontent <- call(ids[newidxs,])
        writeitemstocache(newcontent, filename[newidxs])
        content <- rbind.fill(newcontent, oldcontent)  
      } else {
        content <- oldcontent
      }
      
    }
    
  } else {
    # TODO warn about number of manifesto_ids which are NA ? Or change api?
    content <- call(ids)
  }
  
  return(content)
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
  clear_env(mp_cache)
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
  ## TODO unclear
  system(paste("cp -r", manifesto.getcachelocation(), destination)) ## remove cache
}