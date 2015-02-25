mp_setapikey(key.file = "../manifesto_apikey.txt")

mp_emptycache()

test_that("list metadata versions works", {
  
  mdvs <- mp_corpusversions()
  expect_is(mdvs, "character") ## this is important since numbers might be very large
  expect_more_than(length(mdvs), 0)
  
})

test_that("specifying and updating versions works", {
  
  oldversion <- "20150218100957"
  mp_use_corpus_version(oldversion)
  expect_equal(get(kmetaversion, envir = mp_cache), oldversion)
  
  germeta <- suppressWarnings(mp_metadata(subset(mp_maindataset(), countryname == "Germany")))
  expect_equal(nrow(germeta), 0)
    
  swecorp <- suppressWarnings(mp_corpus(subset(mp_maindataset(), countryname == "Sweden")))
  expect_more_than(length(swecorp), 0)
    
  expect_true(mp_check_for_corpus_update()$update_available)
  
  mp_update_cache()
  expect_equal(get(kmetaversion, envir = mp_cache), mp_check_for_corpus_update()$versionid)
  expect_false(mp_check_for_corpus_update()$update_available)
  germeta <- suppressWarnings(mp_metadata(subset(mp_maindataset(), countryname == "Germany")))
  expect_more_than(nrow(germeta),0)

  
  ## TODO test corpus content
  
  
})


## TODO test downgrading

## TODO test loading cache from file in testcache.R