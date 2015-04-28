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

kcitation <- "Lehmann, Pola / Merz, Nicolas / Regel, Sven / Werner, Annika (2015): Manifesto Corpus. Berlin: WZB Berlin Social Science Center."
kcitemessage <- paste0("When publishing work using the Manifesto Corpus, please ",
                       "make sure to give the identification number of the corpus version ",
                       "used for your analysis and to reference\n\n",
                       kcitation)

message(paste0(kcitemessage, "\n\n",
               "You can print citation and version information with ",
               "the function mp_cite()."))
