gsubx <- function(x, pattern, replacement, ...) {
  gsub(pattern, replacement, x, ...)
} 

#' Rename function names to to manifestoR v0.9
#' 
#' @rdname renamev9
#' 
#' @param infile name of file with old script
#' @param outfile name of destination for new script, defaults to overwrite the old one
#' @return success flag
#' @export
rename_to_v9 <- function(infile, outfile = infile) {

  .Deprecated(new ="", package = "manifestoR",
              msg = "renaming functions in manifestoR where only used for internal purposes and are no longer maintained")

  fl <- file(infile)
  content <- readLines(fl)
  close(fl)
  
  content <- content %>%
    gsubx("manifesto.emptycache", "mp_emptycache", fixed = TRUE) %>%
    gsubx("manifestodb.setapikey", "mp_setapikey", fixed = TRUE) %>%
    gsubx("manifestodb.request", "mpdb_api_request", fixed = TRUE) %>%
    gsubx("manifestodb.get", "get_mpdb", fixed = TRUE) %>%
    gsubx("manifesto.globalenv", "mp_globalenv", fixed = TRUE) %>%
    gsubx("manifesto.maindataset", "mp_maindataset", fixed = TRUE) %>%
    gsubx("manifesto.listversions", "mp_coreversions", fixed = TRUE) %>%
    gsubx("manifesto.meta", "mp_metadata", fixed = TRUE) %>%
    gsubx("manifesto.availability", "mp_availability", fixed = TRUE) %>%
    gsubx("manifesto.corpus", "mp_corpus", fixed = TRUE) %>%
    gsubx("gl.scaling", "scale_weighted", fixed = TRUE) %>%
    gsubx("logit.scaling", "scale_logit", fixed = TRUE) %>%
    gsubx("bipolar.scaling", "scale_bipolar", fixed = TRUE) %>%
    gsubx("create.scaling", "create_scaling", fixed = TRUE)
  
  fl <- file(outfile, open = "w")
  writeLines(content, fl)
  close(fl)
  
  return(TRUE)
    
}


#' Rename function names to to manifestoR v0.9
#' 
#' @rdname renamev9
#' 
#' @param pattern pattern of file names, defaults to R and R markdown files
#' @param recursive passen on to \code{\link{list.files}}
#' @param ... more arguments passed on to \code{\link{list.files}}
#' @return success flag
#' @export
rename_files_to_v9 <- function(pattern = "*.R((md)|(markdown))?$",
                             recursive = TRUE,
                             ...) {
  
  for (file in list.files(pattern = pattern, recursive = recursive , ...)) {
  
    rename_to_v9(file)
  }
  
  return(TRUE)
  
}
