#' Manifesto Corpus class
#' 
#' A \code{tm} \code{\link[tm]{Corpus}} storing \code{\link{ManifestoDocument}s}
#' 
#' @description
#' Objects of this class are returned by \code{\link{mp_corpus}}.
#' 
#' @details
#' For usage and structure of the stored documents see
#' \code{\link{ManifestoDocument}}.
#'  
#' @name ManifestoCorpus
#' @param csource a \code{\link{ManifestoJSONSource}}, see \code{\link[tm]{Source}}
#' @docType class
#' @examples
#' \dontrun{corpus <- mp_corpus(subset(mp_maindataset(), countryname == "Russia"))}
ManifestoCorpus <- function(csource = ManifestoJSONSource()) {
  corpus <- VCorpus(csource)
  class(corpus) <- c("ManifestoCorpus", class(corpus))
  return(corpus)
}

#' Manifesto Document
#' 
#' 
#'  
#' @description
#' A ManifestoDocument represents a document from the Manifesto Corpus and contains
#' text, coding and meta information.
#' ManifestoDocument objects need not be constructed manually but are the content
#' of the \code{\link{ManifestoCorpus}} objects downloaded from the Manifesto
#' Corpus Database API via \code{\link{mp_corpus}}. 
#' 
#' \code{ManifestoDocument}s subclass the \code{\link[tm]{TextDocument}} class
#' from the package \code{tm}. Hence they can be and usually are collected in a \code{tm}
#' \code{\link[tm]{Corpus}} to interface easily with text mining and other linguistic
#' analysis functions. \code{manifestoR} uses the subclass
#' \code{\link{ManifestoCorpus}} of \code{tm}s \code{\link[tm]{Corpus}}, but
#' \code{ManifestoDocument}s can be stored in any kind of \code{Corpus}.
#' 
#' As in \code{tm} any ManifestoDocument has metadata which can be accessed and
#' modified via the \code{meta} function, as well as content, accessible via
#' \code{content}. Additionally, via \code{\link{codes}()}, the coding of the
#' (quasi-)sentence ccording to the CMP category scheme can be accessed 
#' (and modified).The CMP category scheme can be found online at
#' \url{https://manifesto-project.wzb.eu/coding_schemes/1}.
#' 

#' 
#' @details
#' Internally, a ManifestoDocument is a \code{data.frame} with a row for
#' every quasi-sentence and the columns \code{text} and \code{code}.
#'  
#' @name ManifestoDocument
#' 
#' @param content data.frame of text and codes for the ManifestoDocument to be constructed.
#' There can be multiple columns of codes, but by default the accessor method \code{\link{codes}}
#' searches for the column named "cmp_code".
#' @param id an id to identify the Document
#' @param meta an object of class \code{\link{ManifestoDocumentMeta}} containing the metadata for this document
#' 
#' @docType class
#' @examples
#' \dontrun{
#' corpus <- mp_corpus(subset(mp_maindataset(), countryname == "New Zealand"))
#' doc <- corpus[[1]]
#' print(doc)
#' }
#' @export
ManifestoDocument <- function(content = data.frame(),
                              id = character(0),
                              meta = ManifestoDocumentMeta()) {
  structure(list(content = content,
                 meta = ManifestoDocumentMeta(meta = meta, id = id)),
            class = (c("ManifestoDocument", "PlainTextDocument", "TextDocument")))
}



#' @method content ManifestoDocument
#' @export
content.ManifestoDocument <- function(x) {
  return(as.character(x$content$text))
}

#' @method content<- ManifestoDocument
#' @export
`content<-.ManifestoDocument` <- function(x, value) {
  x$content$text <- value
  return(x)
}


#' Access the codes of a Manifesto Document or Corpus
#' 
#' With the accessor the codes of a Manifesto Document can be
#' read and modified. The codes of a Manifesto Corpus can only be read,
#' modification needs to be done document-wise.
#' 
#' @param x document or corpus to get the codes from
#' @param layer layer of codings to access, defaults to cmp_code, alternative: eu_code
#' @rdname codes
#' @export
codes <- function(x, layer = "cmp_code") {
  UseMethod("codes", x)
}

#' @rdname codes
#' @method codes ManifestoDocument
#' @export
codes.ManifestoDocument <- function(x, layer = "cmp_code") {
  return(unlist(x$content[,layer]))
}

#' @method codes ManifestoCorpus
#' @rdname codes
#' @export
codes.ManifestoCorpus <- function(x, layer = "cmp_code") {
  l <- lapply(x, codes, layer)
  names(l) <- NULL
  return(unlist(l))
}

#' @param value new codes
#' @rdname codes
#' @export
`codes<-` <- function(x, layer = "cmp_code", value) {
  UseMethod("codes<-", x)
}

#' @rdname codes
#' @method codes<- ManifestoDocument
#' @export
`codes<-.ManifestoDocument` <- function(x, layer = "cmp_code", value) {
  x$content[,layer] <- value
  return(x)
}

#' \code{codelayers} gives a list of the names of the coding layers present in the ManifestoDocument
#' @rdname codes
#' @export
code_layers <- function(x) {
  names(x$content)[-1]
}

#' @method meta ManifestoDocument
#' @export
meta.ManifestoDocument <- function(x, tag=NULL, ...) {
  if (!is.null(tag)) {
    return(x$meta[[tag]])
  } else {
    return(x$meta)
  }
}

#' @method length ManifestoDocument
#' @export
length.ManifestoDocument <- function(x) {
  length(content(x))
}

#' @method str ManifestoDocument
#' @export
str.ManifestoDocument <- function(object, ...) {
  doc2 <- object
  class(doc2) <- "list"
  return(utils::str(doc2, ...))
}

#' @method subset ManifestoDocument
#' @export
subset.ManifestoDocument <- function(x, subset, ...) {
  cpdoc <- x
  cpdoc$content <- base::subset(cpdoc$content, subset, ...)
  return(cpdoc)
}
  
#' @method as.data.frame ManifestoDocument
#' @export
as.data.frame.ManifestoDocument <- function(x,
                                            row.names = NULL,
                                            optional = TRUE,
                                            stringsAsFactors = FALSE,
                                            with.meta = FALSE,
                                            ...) {
  dftotal <- data.frame(x$content,
                        pos = if (length(x) > 0) {
                          1:length(x)
                        } else {
                          integer(0)
                        },
                        row.names = row.names,
                        stringsAsFactors = stringsAsFactors,
                        ...)
  if (with.meta & nrow(dftotal) > 0) {
    metadata <- data.frame(t(unlist(meta(x))),
                            stringsAsFactors = stringsAsFactors) %>%
                mutate(party = as.numeric(as.character(party)),
                       date = as.numeric(as.character(date)))
    dftotal <- data.frame(dftotal, metadata)
  }
  return(dftotal)
}

#' @method as.data.frame ManifestoCorpus
#' @export
as.data.frame.ManifestoCorpus <- function(x,
                                          row.names = NULL,
                                          optional = TRUE,
                                          stringsAsFactors = FALSE,
                                          with.meta = FALSE,
                                          
                                          ...) {
  suppressWarnings({
    dfslist <- lapply(content(x), Curry(as.data.frame,
                                          stringsAsFactors = stringsAsFactors,
                                          with.meta = with.meta,
                                          row.names = row.names,
                                          optional = optional,
                                          ...))
    do.call(bind_rows, dfslist)
  })
}

#' @method head ManifestoDocument
#' @export
head.ManifestoDocument <- function(x, n = 6, ...) {
  n <- min(length(x), n)
  subset(x, c(rep(TRUE, n), rep(FALSE, length(x) - n)))
}

#' @method tail ManifestoDocument
#' @export
tail.ManifestoDocument <- function(x, n = 6, ...) {
  n <- min(length(x), n)
  subset(x, c(rep(FALSE, length(x) - n), rep(TRUE, n)))
}



#' @method meta<- ManifestoDocument
#' @export
`meta<-.ManifestoDocument` <- function(x, tag, ..., value) {
  x$meta[[tag]] <- value
  return(x)
}

#' Reader for \code{\link{ManifestoSource}}
#' 
#' @details
#' Used internally for constructing \code{\link{ManifestoCorpus}} objects.
#' For the general mechanism refer to \code{tm}s \code{\link[tm]{Reader}}
#' documentation.
#'
#' @param language is ignored
#' @param elem a named list with the component \code{content}
#' @param id a character giving a unique identifier for the created text document
readManifesto <- function(elem, language, id) {
  document <- elem$content
  if ("content" %in% names(document)) {
    document <- rename(document, text = content)
  }
  if ("code" %in% names(document)) {
    document <- rename(document, cmp_code = code)
  }
  ManifestoDocument(content = document %>%
                      mutate_each(funs(as.character), contains("code")) %>%
                      mutate_each(funs(ifelse(is.nacode(.), NA, .)), contains("code")),
                    id = elem$id,
                    meta = elem$meta)
}

#' Manifesto Document Metadata
#' 
#' @docType class
#' @name ManifestoDocumentMeta
#' @param meta a named list with tag-value pairs of document meta information
#' @param id a character giving a unique identifier for the text document
#' @export
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
#' @param texts texts of the manifesto documents
ManifestoSource <- function(texts) {
  .Deprecated("ManifestoJSONSource")
  SimpleSource(length = length(texts),
               reader = readManifesto,
               content = texts,
               class = c("ManifestoSource"))
}

#' @method getElem ManifestoSource
#' @export
getElem.ManifestoSource <- function(x) {
  .Deprecated("getElem.ManifestoJSONSource",
              package = "manifestoR",
              msg = "class ManifestoSource is deprecated, please use ManifestoJSONSource instead!")
  list(content = x$content[[x$position]],
       uri = NULL)    
}

#' @rdname ManifestoSource
#' @param query_meta metadata to attach to document by joining on manifesto_id
ManifestoJSONSource <- function(texts = list(manifesto_id = c(),
                                             items = c()),
                                query_meta = data.frame()) {
  SimpleSource(length = length(texts$manifesto_id),
               reader = readManifesto,
               ids = texts$manifesto_id,
               query_meta = query_meta,
               content = texts$items,
               class = c("ManifestoJSONSource"))
}

#' @method getElem ManifestoJSONSource
#' @export
getElem.ManifestoJSONSource <- function(x) {
  suppressWarnings(
    list(content = x$content[[x$position]],
         meta = data.frame(manifesto_id = x$ids[[x$position]]) %>%
           left_join(x$query_meta, by = "manifesto_id"),
         id = x$ids[[x$position]]))
}
