wrap_mpdb_call <- function(call, version = NULL) {
  message <- "Connecting to Manifesto Project DB API..."
  if (!is.null(version)) {
    message <- paste(message, "corpus version:", version)
  }
  function() {
    cat(message, "\n")
    return(call)
  }
}

wrap_mpdb_call_with_ids <- function(fun, version = NULL) {
  message <- "Connecting to Manifesto Project DB API..."
  if (!is.null(version)) {
    message <- paste(message, "corpus version:", version)
  }
  function(ids) {
    cat(message, "\n")
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
    if (exists(varname, envir = mp_cache())) {
      data <- get(varname, envir = mp_cache())
    } else {
      data <- call()
      assign(varname, data, envir = mp_cache())
    }
  } else {
    data <- call()
  }
  
  return(data)
  
}


write_multivar_to_cache <- function(df, ids) {
  
  sapply(df$manifesto_id, function(id) {
    
    vname <- ids$cache_varname[which(ids$manifesto_id == id)]
    assign(vname, subset(df, manifesto_id == id), envir = mp_cache())
    
  })
  
}

read_multivar_from_cache <- function(varnames) {
  
  Reduce(function(df, id) {
      
      bind_rows(df, get(id, envir = mp_cache()))
      
    },
    varnames,
    init = data.frame())

}


multi_var_caching <- function(ids, get_fun, varname_fun,
                              cache = TRUE) {
  
  ids <- within(ids, {
     cache_varname <- varname_fun(ids)
     is_cached <- sapply(cache_varname, Curry(exists, envir = mp_cache()))
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
    if (!exists(varname, envir = mp_cache())) {
      assign(varname, filter(ids, FALSE), envir = mp_cache())
    }
    cachedata <- get(varname, envir = mp_cache())

    ## check which ids are and are not already in cache
    datatoget <- anti_join(ids, cachedata, by = id.names)
    datafromcache <- semi_join(cachedata, ids, by = id.names)

    if (nrow(datatoget) > 0) {

      ## get missing ids
      requested <- fun(datatoget)

      if (nrow(datatoget) > 0) {

        ## write missings to cache
        cachedata <- bind_rows(cachedata, requested)
        assign(varname, cachedata, envir = mp_cache())

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

#' Check for Updates of Corpus in Manifesto Project DB
#' 
#' \code{mp_check_for_copus_update} checks if the currently cached version of corpus text and metadata
#' is older than the most recent version available via the Manifesto Project
#' DB API.
#' 
#' \code{mp_update_cache} checks if a new corpus version is available and loads
#' the new version via: \code{\link{mp_use_corpus_version}}. That is, 
#' the internal cache of manifestoR will automatically be updated to newer version
#' and all future calls to the API will request for the newer version.
#' 
#' @details
#' Note that this versioning applies to the corpus' texts and metadata, and not the
#' versions of the core dataset. For this see \code{\link{mp_coreversions}}
#' 
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param only_stable Consider only for versions marked as stable by the Manifesto
#'        Projec Team, defaults to TRUE
#' @return \code{mp_update_cache} returns a list with a boolean
#'         \code{update_available} and \code{versionid},
#'         a character string identifying the most recent online version available
#' @rdname corpusupdate
#' @export
mp_check_for_corpus_update <- function(apikey = NULL, only_stable = TRUE) {
  
  cacheversion <- getn(kmetaversion, envir = mp_cache())
  dbversion <- last_corpus_version(apikey = apikey, onlytag = only_stable)
  
  return(list(update_available = (is.null(cacheversion) || (cacheversion != dbversion)),
              versionid = dbversion))

}

#' @rdname corpusupdate
#' @param cache_env Cache environment
#' @return \code{mp_which_corpus_version} returns the current version id of the
#' corpus and metadata stored in the cache
#' @export
mp_which_corpus_version <- function(cache_env = mp_cache()) {
  
  cacheversion <- getn(kmetaversion, envir = cache_env)
  
  if (is.null(cacheversion)) {
    return(NA)
  } else {
    return(cacheversion)
  }
}

#' @rdname corpusupdate
#' @return \code{mp_which_dataset_versions} returns the names of the main dataset
#' versions which are in the cache, i.e. have been downloaded
#' @export
mp_which_dataset_versions <- function(cache_env = mp_cache()) {
  gsub(paste0(kdatasetname, "(.*)"), "\\1", ls(cache_env, pattern = kdatasetname))
}


#' Use a specific version of the Manifesto Project Corpus
#' 
#' The internal cache of manifestoR will be updated to the specified version
#' and all future calls to the API will request for the specified version. Note
#' that this versioning applies to the corpus' texts and metadata, and not the
#' versions of the core dataset. For this see \code{\link{mp_coreversions}}
#' 
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @param versionid character id of the version to use (as received from API and
#' \code{\link{mp_corpusversions}})
#' 
#' @export
mp_use_corpus_version <- function(versionid, apikey=NULL) {

  cache_versionid <- getn(kmetaversion, envir = mp_cache())
  
  if (is.null(cache_versionid) || versionid != cache_versionid) {

    ## create new cache environment to prevent data loss if update fails
    new_cache <- new.env()
    assign(kmetaversion, versionid, envir = new_cache)

    meta_from_cache <- getn(kmetadata, envir = mp_cache())
    texts_in_cache <- manifestos_in_cache(mp_cache())

    if (!is.null(meta_from_cache)) {

      if (nrow(meta_from_cache) > 0) {
        ## update metadata
        newmeta <- get_viacache(kmtype.meta,
                                ids = union(select(meta_from_cache, one_of("party", "date")),
                                            select(texts_in_cache, one_of("party", "date"))),
                                versionid = versionid,
                                cache = FALSE)
      } else {
        newmeta <- meta_from_cache
      }


      if (!("md5sum_text" %in% names(meta_from_cache))) {
        meta_from_cache <- mutate(meta_from_cache, md5sum_text = NA)
      }
      if (!("md5sum_text" %in% names(newmeta))) {
        newmeta <- mutate(newmeta, md5sum_text = NA)
      }

      assign(kmetadata, newmeta, envir = new_cache)        

    }

    ## update corpus texts
    if (!is.null(meta_from_cache)) {
      texts_in_cache <- texts_in_cache %>%
                  left_join(select(meta_from_cache,
                                   one_of("party", "date", "md5sum_text")),
                            by = c("party", "date")) %>%
                  left_join(select(newmeta,
                                   one_of("party", "date", "md5sum_text")),
                            by = c("party", "date")) %>%
                  mutate(download = (
                           is.na(md5sum_text.x) | 
                           is.na(md5sum_text.y) | 
                           (md5sum_text.x != md5sum_text.y)))

    } else {
      texts_in_cache <- mutate(texts_in_cache, download = TRUE)
    }    

    ## copy other cache content
    to_copy <- setdiff(ls(envir = mp_cache()),
                       c(kmetaversion, kmetadata,
                         as.character(texts_in_cache[texts_in_cache$download,]$vname)))
    copy_to_env(to_copy,
                mp_cache(),
                new_cache)
 
    mp_load_cache(new_cache)

    if (nrow(subset(texts_in_cache, download)) > 0) { ## prevent warning of querying empty corpus
      ## download documents to write them automatically to new cache
      nupdated <- length(mp_corpus(select(subset(texts_in_cache, download),
                                          one_of("party", "date"))))
      message(paste(nupdated, "documents updated"))      
    }

  }
  
}

manifestos_in_cache <- function(cache_env = mp_cache()) {

  re <- paste0(ktextname, "_(\\d+)_(\\d+)_.*")
  
  extract_ids <- function(x, repl) {
    return(as.numeric(gsub(re, repl, x)))
  }
  
  nms <- ls(envir = cache_env)
  nms <- subset(nms, grepl(re, nms))
  
  return(data.frame(vname = nms,
                    party = extract_ids(nms, "\\1"),
                    date = extract_ids(nms, "\\2")))
  
}

copy_to_env <- function(vars, env1, env2) {
  for (var in vars) {
    assign(var, get(var, envir = env1), envir = env2)
  }
}

getn <- function(...) {
  tryCatch(get(...),
           error = function(e) { return(NULL) })
}

#' @rdname corpusupdate
#' @return \code{mp_update_cache} returns the character identifier of the version updated to
#' @export
mp_update_cache <- function(apikey=NULL, only_stable = TRUE) {
  
  ## get list of versions, take most current one
  versionid <- last_corpus_version(apikey = apikey, onlytag = only_stable)
  mp_use_corpus_version(versionid)
  
  return(versionid)
}


#' Get API results via cache 
#' 
#' @details
#' This function is internal to manifestoR and not designed for use from
#' other namespaces
#' 
#' @param type type of objects to get (metadata, documents, ...) as a string. Types
#' are defined as constants in globals.R
#' @param ids identifiers of objects to get. Depending on the type a data.frame or vector of identifiers.
#' @param cache whether to use (TRUE) or bypass (FALSE) cache, defaults to TRUE
#' @param versionid string identifier of version to use
#' @param ... additional parameters handed over to get_mpdb
get_viacache <- function(type, ids = c(), cache = TRUE, versionid = NULL, ...) {
    
  if (cache) {
    
    if (is.null(versionid)) {
      
      ## check for versionid in cache
      if (exists(kmetaversion, envir = mp_cache())) {
        
        versionid <- get(kmetaversion, envir = mp_cache())
        
      } else { ## This case should never happen
        
        ## TODO: try to keep the cache content in sync with the stored versionid! U
        versionid <- last_corpus_version(...)
        assign(kmetaversion, versionid, envir = mp_cache())
        
      }    
      
    }
  }
  
  if (type == kmtype.versions) {
    
    call <- wrap_mpdb_call(get_mpdb(kmtype.versions, versionid = versionid, ...))
    
    return(single_var_caching(kversions, call,
                              cache = cache))
    
  } else if (type == kmtype.main) {
    
    call <- wrap_mpdb_call(get_mpdb(kmtype.main,
                                    parameters=ids,
                                    versionid = versionid,
                                    ...),
                           version = versionid)
    return(single_var_caching(paste0(kdatasetname, ids$key, ids$kind), call,
                              cache = cache))
    
  } else if (type == kmtype.meta) {
    
    fun <- wrap_mpdb_call_with_ids(function(ids) {
      
      return(get_mpdb(type = kmtype.meta,
                      parameters = formatmetaparams(ids),
                      versionid = versionid,
                      ...))
    },
    version = versionid)
    
    return(table_caching(kmetadata, fun, ids, id.names = c("party", "date"),
                         cache = cache))
    
  } else if (type == kmtype.text) {
    
    get_fun <- wrap_mpdb_call_with_ids(function(ids) {
      
      return(get_mpdb(type = kmtype.text,
                      parameters = formattextparams(ids),
                      versionid = versionid,
                      ...))      
    },
    version = versionid)
    
    varname_fun <- function(ids) {
      paste(ktextname, ids$party, ids$date, ids$manifesto_id, sep = "_")
    }
    
    return(multi_var_caching(ids, get_fun, varname_fun,
                             cache = cache))
  }
  
}


#' Empty the manifestoR's cache
#'  
#' @export
mp_emptycache <- function() {
  clear_env(mp_cache())
  return()
}

#' List the available versions of the Manifesto Project's Corpus
#' 
#' The Manifesto Project Database API assigns a new version code whenever changes
#' to the corpus texts or metadata are made.
#' 
#' @details
#' This function always bypasses the cache.
#' 
#' @param apikey API key to use. Defaults to \code{NULL}, resulting in using
#'        the API key set via \code{\link{mp_setapikey}}.
#' @return a character vector with the available version ids
#' @export
mp_corpusversions <- function(apikey=NULL) {
  
  versions <- get_mpdb(kmtype.metaversions, apikey=apikey)
  
  return(versions)
}


#' Load manifestoR's cache
#' 
#' Load a cache from a variable or file to manifestoR's current working
#' environment.
#' 
#' @param cache an environment that should function as manifestoR's new cache.
#' If this is NULL, the environment is loaded from the file specified by argument file.
#' @param file a file name from where the cache environment should be loaded
#' 
#' @examples
#' \dontrun{mp_load_cache() ## loads cache from file "mp_cache.RData"}
#' @export
mp_load_cache <- function(cache = NULL, file = "mp_cache.RData") {
  
  tmp_env <- new.env()
  
  if (is.null(cache)) {
    load(file, envir = tmp_env)
  } else {
    assign("mp_cache", cache, envir = tmp_env)
  }
  
  assign("mp_cache", get("mp_cache", envir = tmp_env), envir = mp_globalenv)
}

#' Save manifestoR's cache
#' 
#' Saves manifestoR's cache to the file system. This function can and should be
#' used to store downloaded snapshots of the Manifesto Project Corpus Database
#' to your local hard drive. They can then be loaded via \code{\link{mp_load_cache}}.
#' Caching data in the file system ensures reproducibility of the scripts and
#' analyses, enables offline use of the data and reduces unnecessary traffic
#' and waiting times.
#' 
#' @param file a file from which to load the cache environment
#' @examples
#' \dontrun{mp_save_cache() ## save to "mp_cache.RData" in current working directory}
#' @export
mp_save_cache <- function(file = "mp_cache.RData") {
    
  save("mp_cache", file = file, envir = mp_globalenv)
  
}