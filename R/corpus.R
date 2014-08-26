library(tm)

#' Manifesto Documents class
#' 
#' Objects representing Manifestos as parts of Corpora
#' 
#' @details
#' \code{ManifestoDocument}s are the central objects of \code{manifestoR}
#' and subclasses of the \code{\link{Document}} class from the package 
#' \code{\link{tm}}. Hence they can be and usually are collected in a \code{tm}
#' \code{\link{Corpus}} to interface easily with text mining and other linguistic
#' analysis functions.
#' 
#' As in \code{tm} any ManifestoDocument has metadata which can be accessed and
#' modified via the \code{meta} function, as well as content, accessible via
#' \code{content}.
#' Internally, a ManifestoDocument is either simply a string, if no coding
#' information is available, or it is a \code{data.frame} or quasi-sentences
#' with attached codes according to the CMP category scheme.
#' The CMP category scheme can be found online at
#' \url{https://manifesto-project.wzb.eu/coding_schemes/1}
#'  
#' @name ManifestoDocument
#' @docType class
#' @examples
#' ## TODO convenience function
NULL


#' Get the content of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @export
content.ManifestoDocument <- function(doc) {
  return(as.character(doc$content$text))
}

#' Modify the content of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param value new content
#' @export
`content<-.ManifestoDocument` <- function(doc, value) {
  doc$content$text <- value
  return(doc)
}


#' Get the codes of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @export
codes <- function(x) {
  UseMethod("codes", x)
}
#' @export
codes.ManifestoDocument <- function(doc) {
  return(as.numeric(doc$content$code))
}

#' Modify the codes of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param value new codes
#' @export
`codes<-` <- function(x, value) {
  UseMethod("codes<-", x)
}
#' @export
`codes<-.ManifestoDocument` <- function(doc, value) {
  doc$content$code <- value
  return(doc)
}

#' Get the metadata of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param tag tag of specific metadata to get
#' @export
meta.ManifestoDocument <- function(doc, tag=NULL) {
  if (!is.null(tag)) {
    return(doc$meta[[tag]])
  } else {
    return(doc$meta)
  }
}

#' Modify the metadata of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param tag tag of specific metadata to modify
#' @param value new value of metadata tag
#' @export
`meta<-.ManifestoDocument` <- function(doc, tag, ..., value) {
  doc$meta[[tag]] <- value
  return(doc)
}


## document: param language is ignored
readManifesto <- function(elem, language, id) {
 doc <- ManifestoDocument(content = elem$content[[1]]$content,
                          meta = elem$content[[1]]$meta,
                          id = id)
 return(doc)
}

ManifestoDocumentMeta <- function(meta = list(), id = character(0)) {
  if (!is.null(id)) {
    meta$id <- id
  } else {
    meta$id <- character(0)
  }
  structure(meta, class = c("ManifestoDocumentMeta", "TextDocumentMeta"))
}

ManifestoDocument <- function(content = data.frame(names = c("text", "code")),
                              id = character(0),
                              meta = ManifestoDocumentMeta()) {
  structure(list(content = content,
                 meta = ManifestoDocumentMeta(meta = meta, id = id)),
            class = (c("ManifestoDocument", "PlainTextDocument", "TextDocument")))
}

ManifestoSource <- function(texts) {
  SimpleSource(length = length(texts),
               reader = readManifesto,
               content = texts,
               class = c("ManifestoSource"))
}

#' @export
getElem.ManifestoSource <- function(x) {
  list(content = x$content[x$position],
       uri = NULL)    
}

ManifestoCorpus <- function(csource) {
  corpus <- VCorpus(csource)
  class(corpus) <- c("ManifestoCorpus", class(corpus))
  return(corpus)
}
#' @export
codes.ManifestoCorpus <- function(corpus) {
  c(unlist(lapply(corpus, codes)))
}