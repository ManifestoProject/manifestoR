api_key <- readChar("../manifesto_apikey.txt", nchars = 32)
mp_setapikey(key = api_key)

mp_emptycache()

test_that("list metadata versions works", {
  
  mdvs <- mp_corpusversions()
  expect_is(mdvs, "data.frame") ## this is important since numbers might be very large
  expect_named(mdvs, c("name", "tag"))
  expect_is(mdvs$name, "character")
  expect_is(mdvs$tag, "character")
  expect_gt(nrow(mdvs), 0)
  
})

test_that("specifying and updating versions works", {
  
  oldversion <- "20150218100957"
  mp_use_corpus_version(oldversion)
  expect_equal(mp_which_corpus_version(), oldversion)
  
  germeta <- suppressWarnings(mp_metadata(subset(mp_maindataset(), countryname == "Germany")))
  expect_equal(nrow(germeta), 0)

  oldversion <- "20150603140744"
  mp_use_corpus_version(oldversion)
  swecorp <- suppressWarnings(mp_corpus(subset(mp_maindataset(), countryname == "Sweden")))
  expect_gt(length(swecorp), 0)
    
  expect_true(mp_check_for_corpus_update()$update_available)
  
  ## test corpus content: between this and the next version there should be a change in MD5 sums
  gercorp <- suppressWarnings(mp_corpus(subset(mp_maindataset(), countryname == "Germany")))
  expect_gt(length(gercorp), 0)
  
  
  expect_message(mp_update_cache(), regexp = "\\d+.*updated") ## now we should get 1 updated document
  expect_equal(mp_which_corpus_version(), mp_check_for_corpus_update()$versionid)
  expect_false(mp_check_for_corpus_update()$update_available)
  germeta <- suppressWarnings(mp_metadata(subset(mp_maindataset(), countryname == "Germany")))
  expect_gt(nrow(germeta),0)  
  
})

test_that("downgrading of corpus and metadata works", {
  
  mp_update_cache() ## definitely newer than olderversion
  gercorp <- suppressWarnings(mp_corpus(subset(mp_maindataset(), countryname == "Germany")))
  avl <- suppressWarnings(mp_availability(mp_maindataset()))

  expect_gt(length(gercorp), 0)

  intermedversion <- "20150603140744"
  expect_message(mp_use_corpus_version(intermedversion), regexp = "\\d+.*updated")
  gercorp <- suppressWarnings(mp_corpus(subset(mp_maindataset(), countryname == "Germany")))

  oldversion <- "20150603140744"
  suppressWarnings(mp_use_corpus_version(oldversion))
  avl_old <- suppressWarnings(mp_availability(mp_maindataset()))

  expect_true(sum(na.omit(avl$annotations)) >
              sum(na.omit(avl_old$annotations)))

})
