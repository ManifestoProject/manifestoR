mp_globalenv <- new.env()
assign("mp_cache", new.env(parent = emptyenv()), envir = mp_globalenv)
mp_cache <- function() {
  get("mp_cache", envir = mp_globalenv)
}

kcachelocation <- "cachelocation"
kapikey <- "apikey"


kversions <- "versions"
kmetaversion <- "metaversionid"
kdefaultcachename <- "manifestofiles"
ktextname <- "manifesto_doc"
kmetadata <- "docmetadata"
koriginals <- "originals"
kdatasetname <- "MPDataset_"

kmtype.meta <- "meta"
kmtype.text <- "text"
kmtype.original <- "original"
kmtype.main <- "main"
kmtype.versions <- "versions"
kmtype.metaversions <- "metaversions"
