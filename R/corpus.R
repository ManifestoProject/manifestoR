#' Manifesto Corpus class
#' 
#' A \code{tm} \code{\link{Corpus}} storing \code{\link{ManifestoDocument}s}
#' 
#' @description
#' Objects of this class are returned by \code{\link{manifesto.corpus}}.
#' 
#' @details
#' For usage and structure of the stored documents see
#' \code{\link{ManifestoDocument}}.
#'  
#' @name ManifestoCorpus
#' @docType class
ManifestoCorpus <- function(csource) {
  corpus <- VCorpus(csource)
  class(corpus) <- c("ManifestoCorpus", class(corpus))
  return(corpus)
}
#' @method codes ManifestoCorpus
#' @export
#' @rdname generics
codes.ManifestoCorpus <- function(corpus) {
  c(unlist(lapply(corpus, codes)))
}

#' Manifesto Documents class
#' 
#' Objects representing Manifestos as parts of Corpora
#' 
#' @description
#' \code{ManifestoDocument}s are the central objects of \code{manifestoR}
#' and subclasses of the \code{\link{Document}} class from the package 
#' \code{\link{tm}}. Hence they can be and usually are collected in a \code{tm}
#' \code{\link{Corpus}} to interface easily with text mining and other linguistic
#' analysis functions. \code{manifestoR} uses the subclass
#' \code{\link{ManifestoCorpus}} of \code{tm}s \code{\link{Corpus}}, but
#' \code{ManifestoDocument}s can be stored in any kind of \code{Corpus}.
#' 
#' As in \code{tm} any ManifestoDocument has metadata which can be accessed and
#' modified via the \code{meta} function, as well as content, accessible via
#' \code{content}. Additionally, via \code{\link{codes}()}, the coding of the
#' (quasi-)sentence ccording to the CMP category scheme can be accessed 
#' (and modified).The CMP category scheme can be found online at
#' \url{https://manifesto-project.wzb.eu/coding_schemes/1}.
#' 
#' @details
#' Internally, a ManifestoDocument is a \code{data.frame} with a row for
#' every quasi-sentence and the columns \code{text} and \code{code}.
#'  
#' @name ManifestoDocument
#' @docType class
#' @examples
#' ## TODO convenience function
ManifestoDocument <- function(content = data.frame(names = c("text", "code")),
                              id = character(0),
                              meta = ManifestoDocumentMeta()) {
  structure(list(content = content,
                 meta = ManifestoDocumentMeta(meta = meta, id = id)),
            class = (c("ManifestoDocument", "PlainTextDocument", "TextDocument")))
}



#' Get the content of a \code{\link{ManifestoDocument}}
#' 
#' 
#' @param doc ManifestoDocument
#' @rdname generics
#' @method content ManifestoDocument
#' @export
content.ManifestoDocument <- function(doc) {
  return(as.character(doc$content$text))
}

#' Modify the content of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param value new content text (as `character`)
#' @rdname generics
#' @method content<- ManifestoDocument
#' @export
`content<-.ManifestoDocument` <- function(doc, value) {
  doc$content$text <- value
  return(doc)
}


#' Get the codes of a document of corpus
#' 
#' @param x ManifestoDocument
#' @rdname codes
#' @export
codes <- function(x) {
  UseMethod("codes", x)
}

#' @rdname codes
#' @method codes ManifestoDocument
#' @export
codes.ManifestoDocument <- function(doc) {
  return(as.integer(doc$content$code))
}

#' Modify the codes of a document or corpus
#' 
#' @rdname generics
#' @param x document or corpus
#' @param value new codes
#' @export
`codes<-` <- function(x, value) {
  UseMethod("codes<-", x)
}

#' @rdname generics
#' @method codes<- ManifestoDocument
#' @export
`codes<-.ManifestoDocument` <- function(doc, value) {
  doc$content$code <- value
  return(doc)
}

#' Get the metadata of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param tag tag of specific metadata to get
#' @method meta ManifestoDocument
#' @export
meta.ManifestoDocument <- function(doc, tag=NULL) {
  if (!is.null(tag)) {
    return(doc$meta[[tag]])
  } else {
    return(doc$meta)
  }
}

#' @method length ManifestoDocument
#' @export
length.ManifestoDocument <- function(doc) {
  length(content(doc))
}

#' @method str ManifestoDocument
#' @export
str.ManifestoDocument <- function(doc, ...) {
  doc2 <- doc
  class(doc2) <- "list"
  return(str(doc2, ...))
}

#' @method subset ManifestoDocument
#' @export
subset.ManifestoDocument <- function(doc, subset, ...) {
  cpdoc <- doc
  cpdoc$content <- base::subset(cpdoc$content, subset, ...)
  return(cpdoc)
}
  
#' @method as.data.frame ManifestoDocument
#' @export
as.data.frame.ManifestoDocument <- function(doc, with.meta = FALSE, ...) {
  dftotal <- data.frame(content=content(doc), code=codes(doc),
                        pos = 1:length(doc), stringsAsFactors = FALSE, ...)
  if (with.meta) {
    metadata <- data.frame(t(unlist(meta(doc))))
    dftotal <- data.frame(dftotal, metadata)
  }
  return(dftotal)
}

#' @method as.data.frame ManifestoCorpus
#' @export
as.data.frame.ManifestoCorpus <- function(corp, ...) {
  dfslist <- lapply(corp, as.data.frame, c(..., stringsAsFactors = FALSE))
  return(do.call(rbind.fill, dfslist))
}

#' @method head ManifestoDocument
#' @export
head.ManifestoDocument <- function(doc, n = 6) {
  n <- min(length(doc), n)
  subset(doc, c(rep(TRUE, n), rep(FALSE, length(doc) - n)))
}

#' @method tail ManifestoDocument
#' @export
tail.ManifestoDocument <- function(doc, n = 6) {
  n <- min(length(doc), n)
  subset(doc, c(rep(FALSE, length(doc) - n), rep(TRUE, n)))
}



#' Modify the metadata of a \code{\link{ManifestoDocument}}
#' 
#' @param doc ManifestoDocument
#' @param tag tag of specific metadata to modify
#' @param value new value of metadata tag
#' @method meta<- ManifestoDocument
#' @export
`meta<-.ManifestoDocument` <- function(doc, tag, ..., value) {
  doc$meta[[tag]] <- value
  return(doc)
}

#' Reader for \code{\link{ManifestoDocumentSource}}
#' 
#' @details
#' Used internally for constructing \code{\link{ManifestoCorpus}} objects.
#' For the general mechanism refer to \code{tm}s \code{\link{Reader}}
#' documentation.
#'
#' @param language is ignored
readManifesto <- function(elem, language, id) {
 doc <- ManifestoDocument(content = elem$content[[1]]$content,
                          meta = elem$content[[1]]$meta,
                          id = id)
 return(doc)
}

#' Manifesto Document Metadata
#' 
#' Constructor
ManifestoDocumentMeta <- function(meta = list(), id = character(0)) {
  if (!is.null(id)) {
    meta$id <- id
  } else {
    meta$id <- character(0)
  }
  structure(meta, class = c("ManifestoDocumentMeta", "TextDocumentMeta"))
}

#' Data Source for Manifesto Corpus
#' 
#' @details
#' Used internally for constructing \code{\link{ManifestoCorpus}} objects.
#'  
#' @rdname ManifestoSource
#' @docType class
ManifestoSource <- function(texts) {
  SimpleSource(length = length(texts),
               reader = readManifesto,
               content = texts,
               class = c("ManifestoSource"))
}

#' @rdname ManifestoSource
#' @export
getElem.ManifestoSource <- function(x) {
  list(content = x$content[x$position],
       uri = NULL)    
}
