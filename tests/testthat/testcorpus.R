mp_setapikey("../manifesto_apikey.txt")

mp_emptycache()

cache_vname <- function(party, date, manifesto_id) {
  paste(ktextname, party, date, manifesto_id, sep = "_")
}

check_cache_mapping <- function(doc) {
  vname <- cache_vname(meta(doc, "party"), meta(doc, "date"), meta(doc, "manifesto_id"))
  cachedoc <- get(vname, envir = mp_cache())
  expect_equal(head(cachedoc$items[[1]]$content), head(content(doc)))
}

valid_code <- function(x) {
  return(x == 0 | is.na(x) | x >= 100)
}

test_that("simple corpus download works", {
  
  mpds <- mp_maindataset()
  swe <- subset(mpds, countryname == "Sweden")
  corp <- mp_corpus(swe)
  
  expect_is(corp, c("ManifestoCorpus", "Corpus"))
  expect_more_than(length(corp), 10)
  
})

test_that("non-standard evaluation corpus download works", {
  
  partycorp <- mp_corpus(party == 80510)
  expect_is(partycorp, c("ManifestoCorpus", "Corpus"))
  expect_equal(length(partycorp), nrow(subset(mp_availability(
    subset(mp_maindataset(), party == 80510)),
    manifestos)))  
  
  rilecorp <- mp_corpus(rile > 40.0 & edate > as.Date("2000-01-01"))
  expect_is(rilecorp, c("ManifestoCorpus", "Corpus"))
  expect_equal(length(rilecorp), nrow(subset(mp_availability(
    subset(mp_maindataset(), rile > 40.0 & edate > as.Date("2000-01-01"))),
    manifestos)))
  
})

test_that("South America Corpus Download works", {
  
  old_version <- mp_which_corpus_version()
  
  mp_use_corpus_version("20160315172932")
  
  corp <- mp_corpus(countryname == "Brazil")
  expect_is(corp, c("ManifestoCorpus", "Corpus"))
  expect_more_than(length(corp), 3)
  
  mp_use_corpus_version(old_version)
  
})

test_that("getting codes works", {

  eu_corp <- mp_corpus(party == 15328 & date == 200705)
  eu_doc <- eu_corp[[1]]


  ## getting
  expect_equal(code_layers(eu_doc), c("cmp_code", "eu_code"))
  expect_is(codes(eu_doc), "character")
  expect_equal(codes(eu_doc), codes(eu_doc, "cmp_code"))
  expect_is(codes(eu_doc, "eu_code"), "character")
  expect_false(all(na.omit(codes(eu_doc, "eu_code")) == 0L))


  ## modifying

  the_codes <- codes(eu_doc)
  codes(eu_doc) <- rev(codes(eu_doc))
  expect_equal(codes(eu_doc), rev(the_codes))
  codes(eu_doc) <- 5
  expect_true(all(codes(eu_doc) == 5))

  the_codes <- codes(eu_doc, "eu_code")
  codes(eu_doc, "eu_code") <- rev(codes(eu_doc, "eu_code"))
  expect_equal(codes(eu_doc, "eu_code"), rev(the_codes))
  codes(eu_doc, "eu_code") <- 5
  expect_true(all(codes(eu_doc, "eu_code") == 5))
  
  ## custom code layer
  codes(eu_doc, "my_code") <- rep_len(c("A", "B"), length.out = length(eu_doc))
  expect_equal(code_layers(eu_doc), c("cmp_code", "eu_code", "my_code"))
  expect_equal(codes(eu_doc, "my_code")[1:2], c("A", "B"))
  

})

test_that("codefilter works", {
  
  allowed_codes <- c(503, 103)
  
  corp_filtered <- mp_corpus(party == 41320, codefilter = allowed_codes)

  expect_true(all(lapply(content(corp_filtered), function(doc) {
    return(meta(doc, "annotations") & all(codes(doc) %in% allowed_codes))
  })))
  
  ## eu_codes
  allowed_codes <- c(108)
  corp_filtered <- mp_corpus(party == 15328,
                             codefilter = allowed_codes,
                             codefilter_layer = "eu_code")
  expect_true(all(lapply(content(corp_filtered), function(doc) {
    return(all(codes(doc, "eu_code") %in% allowed_codes))
  })))
  
})

test_that("caching of corpus works correctly", {
  
  mpds <- mp_maindataset()
  swe <- subset(mpds, countryname == "Sweden")
  corpswe <- mp_corpus(swe)
  
  ## check that corpus parts are now in cache
  corpids <- mp_metadata(subset(mp_availability(swe), annotations))
  vnames <- cache_vname(corpids$party, corpids$date, corpids$manifesto_id)
  expect_true(all(exists(vnames, envir = mp_cache())))
  
  ## check that mapping of ids and texts is the same in corpus and cache
  lapply(content(corpswe), check_cache_mapping)
  corpnsw <- mp_corpus(subset(mpds, countryname %in% c("Sweden", "Norway")))
  lapply(content(corpnsw), check_cache_mapping)
  
})

test_that("requesting an empty corpus works", {
  
  ## requesting only a not available document
  meta.spd04 <- data.frame(party=c(41320), date=c(200409)) ## this does not exist
  corp.spd04 <- mp_corpus(meta.spd04)
  expect_equal(length(corp.spd04), 0)
  
})

test_that("Different ways of indexing corpus work", {
  
  mpds <- mp_maindataset()
  wanted <- subset(mpds, party==41320 &
                     edate < as.Date("2010-01-01") &
                     edate > as.Date("2001-01-01"))
  corpus <- mp_corpus(wanted)
  
  expect_named(corpus)
  expect_true(all(grepl("\\d{5}_\\d{4}", names(corpus))))
  
  expect_is(corpus[[1]], "ManifestoDocument")
  expect_is(corpus[["41320_2002"]], "ManifestoDocument")
  expect_equivalent(corpus[["41320_2002"]], corpus[[1]])
  
  expect_is(corpus[1], "ManifestoCorpus")
  expect_is(corpus[1:2], "ManifestoCorpus")
  expect_is(corpus[c("41320_2005", "41320_2009")], "ManifestoCorpus")
  

})

test_that("corpus does tm stuff", {
  
  mpds <- mp_maindataset()
  wanted <- subset(mpds, party==41320 &
                         edate < as.Date("2010-01-01") &
                         edate > as.Date("2001-01-01"))
  corpus <- mp_corpus(wanted)
  
  ## basic tm corpus functionality
#   summary(content(corpus[[2]]))
  expect_equal(meta(corpus[[2]], "party"), 41320)
  
  tdm <- TermDocumentMatrix(corpus)
  expect_is(tdm, "TermDocumentMatrix")
  expect_true(all(as.vector(tdm["spd",c("41320_2002", "41320_2005" )]) > 0)) ## spd should appear in both docs
  
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

  mpds <- mp_maindataset()
  corpus <- mp_corpus(subset(mpds, countryname == "Germany"))
  
  corpdf <- as.data.frame(corpus)
  expect_is(corpdf, "data.frame")
  expect_more_than(nrow(corpdf), 100)
  expect_true(all(c("text", "cmp_code") %in% names(corpdf)))
  expect_false("party" %in% names(corpdf))
  
  corpmetadf <- as.data.frame(corpus, with.meta = TRUE)
  expect_is(corpmetadf, "data.frame")
  expect_equal(nrow(corpmetadf), nrow(corpdf))
  expect_true(all(c("text", "cmp_code", "party", "language") %in% names(corpmetadf)))

  codes(corpus[[1]], "my_code") <- "X"
  corpdf <- as.data.frame(corpus)
  expect_true(all(c("text", "cmp_code", "my_code") %in% names(corpdf)))
  
  
})

test_that("you can create ManifestoDocument objects from raw data", {
  
  md <- ManifestoDocument(data.frame(text = "Naturstrom",
                                     cmp_code = 501))
  expect_equal(codes(md), 501)
  expect_equal(content(md), "Naturstrom")
  expect_equal(length(md), 1)
  
})

# skn <- subset(mpds, countryname == "Sweden" | countryname == "Norway")
# corp2 <- mp_corpus(skn)
# 
# nor <- subset(mpds, countryname == "Norway")
# corp3 <- mp_corpus(nor)
# 
# head(as.data.frame(corp2))
# head(as.data.frame(corp2, with.meta = TRUE))
