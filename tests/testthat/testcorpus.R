manifestodb.setapikey(key.file = "../manifesto_apikey.txt")

manifesto.emptycache()

cache_vname <- function(party, date, manifesto_id) {
  paste(ktextname, party, date, manifesto_id, sep = "_")
}

check_cache_mapping <- function(doc) {
  vname <- cache_vname(meta(doc, "party"), meta(doc, "date"), meta(doc, "manifesto_id"))
  cachedoc <- get(vname, envir = mp_cache)
  expect_equal(head(cachedoc$items[[1]]$content), head(content(doc)))
}

test_that("simple subset corpus download works", {
  
  mpds <- manifesto.maindataset()
  swe <- subset(mpds, countryname == "Sweden")
  corp <- manifesto.corpus(swe)
  
  expect_is(corp, c("ManifestoCorpus", "Corpus"))
  expect_more_than(length(corp), 10)
})

test_that("caching of corpus works correctly", {
  
  mpds <- manifesto.maindataset()
  swe <- subset(mpds, countryname == "Sweden")
  corpswe <- manifesto.corpus(swe)
  
  ## check that corpus parts are now in cache
  corpids <- manifesto.meta(subset(manifesto.availability(swe)$availability, annotations))
  vnames <- cache_vname(corpids$party, corpids$date, corpids$manifesto_id)
  expect_true(all(exists(vnames, envir = mp_cache)))
  
  ## check that mapping of ids and texts is the same in corpus and cache
  lapply(content(corpswe), check_cache_mapping)
  corpnsw <- manifesto.corpus(subset(mpds, countryname %in% c("Sweden", "Norway")))
  lapply(content(corpnsw), check_cache_mapping)
  
})

## more checks with one specific test document ?

# skn <- subset(mpds, countryname == "Sweden" | countryname == "Norway")
# corp2 <- manifesto.corpus(skn)
# 
# nor <- subset(mpds, countryname == "Norway")
# corp3 <- manifesto.corpus(nor)
# 
# head(as.data.frame(corp2))
# head(as.data.frame(corp2, with.meta = TRUE))
