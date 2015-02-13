wrap_mpdb_call <- function(call) {
  function() {
    print("Connecting to Manifesto Project DB API...")
    return(call)
  }
}

wrap_mpdb_call_with_ids <- function(fun) {
  function(ids) {
    print("Connecting to Manifesto Project DB API...")
    return(fun(ids))
  }
}

clear_env <- function(env) {
  names <- ls(envir = env)
  rm(list = names, envir = env)
  env
}

single_var_caching <- function(varname, call, cache = TRUE) {
  
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


## in the cache things are stored in db

write_multivar_to_cache <- function(df, ids) {
  
  sapply(df$manifesto_id, function(id) {
    
    vname <- ids$cache_varname[which(ids$manifesto_id == id)]
    assign(vname, subset(df, manifesto_id == id), envir = mp_cache)
    
  })
  
}

read_multivar_from_cache <- function(varnames) {
  
  Reduce(function(df, id) {
      
      bind_rows(df, get(id, envir = mp_cache))
      
    },
    varnames,
    init = data.frame())

}


multi_var_caching <- function(ids, get_fun, varname_fun,
                              cache = TRUE) {
  
  ids <- within(ids, {
     cache_varname <- varname_fun(ids)
     is_cached <- sapply(cache_varname, Curry(exists, envir = mp_cache))
  })
  
  fromcache <- read_multivar_from_cache(subset(ids, is_cached)$cache_varname)
  idstoget <- subset(ids, !is_cached)
  if (nrow(idstoget) > 0) {
    fromdb <- get_fun(idstoget)
    write_multivar_to_cache(fromdb, idstoget)    
    return(bind_rows(fromcache, fromdb))
  } else {
    return(fromcache)
  }
  
}

table_caching <- function(varname, fun, ids,
                          id.names = names(ids), cache = TRUE) {
  
  ids <- select(ids, one_of(id.names))
  
  if (cache) {
    
    ## load cache, create if !exists
    if (!exists(varname, envir = mp_cache)) {
      assign(varname, filter(ids, FALSE), envir = mp_cache)
    }
    cachedata <- get(varname, envir = mp_cache)
    
    ## check which ids are and are not already in cache
    datatoget <- anti_join(ids, cachedata, by = id.names)
    datafromcache <- semi_join(cachedata, ids, by = id.names)
    
    if (nrow(datatoget) > 0) {
      
      ## get missing ids
      requested <- fun(datatoget)
      
      if (nrow(datatoget) > 0) {
        
        ## write missings to cache
        cachedata <- bind_rows(cachedata, requested)
        assign(varname, cachedata, envir = mp_cache)
        
        ## return all the requested ids
        data <- bind_rows(requested, datafromcache) 
        
      } else { ## only invalid queries
        
        data <- datafromcache
        
      }
 
    } else {
      
      data <- datafromcache

    }
    
  } else {
    data <- fun(ids)
  }
  
  return(data)
  
}


#' Get API results via cache 
#' 
#' TODO documentation
#' 
#' @param type type of objects to get (metadata, documents, ...)
#' @param ids identifiers of objects to get. Depending on the type a data.frame or vector of identifiers.
#' @param ... additional parameters handed over to manifestodb.get
#' 
get_viacache <- function(type, ids = c(), cache = TRUE, ...) {
  
  if (type == kmtype.versions) {
    
    call <- wrap_mpdb_call(manifestodb.get(kmtype.versions, ...))
    
    return(single_var_caching(kversions, call,
                              cache = cache))
    
  } else if (type == kmtype.main) {
    
    call <- wrap_mpdb_call(manifestodb.get(kmtype.main,
                                           parameters=ids,
                                           ...))
    return(single_var_caching(paste0(kdatasetname, ids$key), call,
                              cache = cache))
    
  } else if (type == kmtype.meta) {
    
    fun <- wrap_mpdb_call_with_ids(function(ids) {
      
      return(manifestodb.get(type = kmtype.meta,
                             parameters = formatmetaparams(ids),
                             ...))
    })
    
    return(table_caching(kmetadata, fun, ids, id.names = c("party", "date"),
                         cache = cache))
    
  } else if (type == kmtype.text) {
    
    get_fun <- wrap_mpdb_call_with_ids(function(ids) {
      
      return(manifestodb.get(type = kmtype.text,
                             parameters = formattextparams(ids),
                             ...))      
    })
    
    varname_fun <- function(ids) {
      paste(ktextname, ids$party, ids$date, ids$manifesto_ids, sep = "_")
    }
    
    return(multi_var_caching(ids, get_fun, varname_fun,
                             cache = cache))
  }
  
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
#   system(paste("cp -r", manifesto.getcachelocation(), destination)) ## remove cache
}