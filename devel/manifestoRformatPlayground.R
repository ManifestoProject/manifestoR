library(tm)

## definition elements of class ManifestoDocument

content.ManifestoDocument <- function(x) {
  as.character(x$df$text)
}
`content<-.ManifestoDocument` <- function(x, value) {
  x$df$text <- value
  x
}
meta.ManifestoDocument <- function(x, tag=NULL) {
  if (!is.null(tag)) {
    x$meta[[tag]]
  } else {
    x$meta
  }
}
`meta<-.ManifestoDocument` <- function(x, tag, ..., value) {
  x$meta[[tag]] <- value
  x
}

# TODO readManifesto.csv

readManifesto.df <- function(elem, language, id) {
  elem$content[[1]]
}

## definition elements of class ManifestoSource
ManifestoSource <- function(x) {
  SimpleSource(length = length(x), reader=readManifesto.df, content = x, class =c("ManifestoSource", "VectorSource"))
}




## data for playing around
df1 <- data.frame(text=c("sentence of  number 1   ", "sentence  number 2"),
                 code=c(101, 406))
meta1 <- structure(list(id=1, language="en", title="First test document", author="jlewando"))
class(meta1) <- "TextDocumentMeta"
df2 <- data.frame(text=c("third sentence", "and some blabla"),
                  code=c(406, 352))
meta2 <- structure(list(id=2, language="en", title="Second test document", author="jlewando"))
class(meta2) <- "TextDocumentMeta"
mdoc1 <- structure(list(df=df1, meta=meta1))
mdoc2 <- structure(list(df=df2, meta=meta2))
class(mdoc1) <- c("ManifestoDocument", "PlainTextDocument", "TextDocument")
class(mdoc2) <- c("ManifestoDocument", "PlainTextDocument", "TextDocument")


## the actual playing around

# elementary corpus manipulations
mcorp <- Corpus(ManifestoSource(c(mdoc1, mdoc2)))
mcorp <- c(mcorp, mcorp)
print(mcorp[[3]])

# corpus transformations
mcorp <- tm_map(mcorp, stripWhitespace)
removeStopWords <- function (x) {
  content(x) <- removeWords(content(x), stopwords(meta(x, "language")))
  x
}
mcorp <- tm_map(mcorp, removeStopWords)

# linguistic analysis functions
tdm <- TermDocumentMatrix(mcorp)
inspect(tdm)

# filtering
wordfilter <- function(word) {
  function(doc) {
    return(word %in% content(doc))
  }
}
fcorp <- tm_filter(mcorp, wordfilter("blabla"))
inspect(fcorp)

codefilter <- function(code) { # more complex, based on the codes
  function(doc) {
    return(code %in% doc$df$code)
  }
}
fcorp <- tm_filter(mcorp, codefilter(101))
inspect(fcorp)
