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

valid_code <- function(x) {
  return(x == 0 | is.na(x) | x >= 100)
}

test_that("simple corpus download works", {
  
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

test_that("requesting an empty corpus works", {
  
  ## requesting only a not available document
  meta.spd04 <- data.frame(party=c(41320), date=c(200409)) ## this does not exist
  expect_warning(corp.spd04 <- manifesto.corpus(meta.spd04))
  expect_equal(length(corp.spd04), 0)
  
})

test_that("corpus does tm stuff", {
  
  mpds <- manifesto.maindataset()
  wanted <- subset(mpds, party==41320 &
                         edate < as.Date("2010-01-01") &
                         edate > as.Date("2001-01-01"))
  corpus <- manifesto.corpus(wanted)
  
  ## basic tm corpus functionality
#   summary(content(corpus[[2]]))
  expect_equal(meta(corpus[[2]], "party"), 41320)
  
  tdm <- TermDocumentMatrix(corpus)
  expect_is(tdm, "TermDocumentMatrix")
  expect_true(all(as.vector(tdm["spd",c("1","2")]) > 0)) ## spd should appear in both docs
  
  ## specific ManifestoDocument functionality
  expect_true(all(valid_code(codes(corpus))))
  tt <- table(codes(corpus[[2]]), useNA = "always")
  expect_is(tt, "table")
  expect_true(tt["503"] > 0)
  
  ## check for subset functionality
  expect_more_than(length(subset(corpus[[2]], codes(corpus[[2]]) == 305)), 0)
})

## more checks with one specific test document ?

test_that("corpus to data.frame works", {

  mpds <- manifesto.maindataset()
  corpus <- manifesto.corpus(subset(mpds, countryname == "Germany"))
  
  corpdf <- as.data.frame(corpus)
  expect_is(corpdf, "data.frame")
  expect_more_than(nrow(corpdf), 100)
  expect_true(all(c("content", "code") %in% names(corpdf)))
  expect_false("party" %in% names(corpdf))
  
  corpmetadf <- as.data.frame(corpus, with.meta = TRUE)
  expect_is(corpmetadf, "data.frame")
  expect_equal(nrow(corpmetadf), nrow(corpdf))
  expect_true(all(c("content", "code", "party", "language") %in% names(corpmetadf)))
  
  
})

# skn <- subset(mpds, countryname == "Sweden" | countryname == "Norway")
# corp2 <- manifesto.corpus(skn)
# 
# nor <- subset(mpds, countryname == "Norway")
# corp3 <- manifesto.corpus(nor)
# 
# head(as.data.frame(corp2))
# head(as.data.frame(corp2, with.meta = TRUE))
