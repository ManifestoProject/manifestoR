mp_setapikey(key.file = "../manifesto_apikey.txt")

mp_emptycache()

test_that("simple caching of listversions() works", {
  
  before_caching <- capture.output(list1 <- mp_coreversions())
  after_caching <- capture.output(list2 <- mp_coreversions())
  
  expect_equal(list1, list2)
  expect_true(length(setdiff(before_caching, after_caching)) > 0)  ## connection message
  expect_true(exists(kversions, envir = mp_cache()))
  
  mp_emptycache()
  expect_equal(length(ls(envir = mp_cache())), 0)
  
  without_caching <- capture.output(list3 <- mp_coreversions(cache = FALSE))
  expect_equal(length(ls(envir = mp_cache())), 0)
  
  after_emptying <- capture.output(mp_coreversions())
  expect_true(length(setdiff(after_emptying, after_caching)) > 0) 
})


test_that("caching of main data set works", {
  
  versions <- mp_coreversions()
  versionname <- as.character(versions[nrow(versions), "datasets.id"])  

  expect_false(exists(paste0(kdatasetname, versionname), envir = mp_cache()))
  
  before_caching <- capture.output(mpds1 <- mp_maindataset())
  after_caching <- capture.output(mpds2 <- mp_maindataset())
  expect_equal(mpds1, mpds2)
  expect_true(length(setdiff(before_caching, after_caching)) > 0)  ## connection message
  expect_true(exists(paste0(kdatasetname, versionname), envir = mp_cache()))
  
})

test_that("saving and loading of cache works", {
  
  mp_emptycache()

  versions <- mp_coreversions()
  versionname <- as.character(versions[nrow(versions), "datasets.id"])  
  before_caching <- capture.output(mpds1 <- mp_maindataset())

  mp_save_cache()
  mp_emptycache()
  expect_false(exists(paste0(kdatasetname, versionname), envir = mp_cache()))

  mp_load_cache()
  expect_true(exists(paste0(kdatasetname, versionname), envir = mp_cache()))
  after_caching <- capture.output(mpds2 <- mp_maindataset())
  expect_equal(mpds1, mpds2)
  expect_true(length(setdiff(before_caching, after_caching)) > 0)  ## connection message

  mm1 <- mp_metadata(subset(mpds2, countryname == "Austria"))
  expect_true(exists(kmetadata, envir = mp_cache()))
  mp_save_cache(file = "another_cache.RData")

  ## empty cache
  mp_emptycache()
  expect_false(exists(kmetadata, envir = mp_cache()))
  expect_false(exists(paste0(kdatasetname, versionname), envir = mp_cache()))  

  ## load cache
  mp_load_cache(file = "another_cache.RData")
  expect_true(exists(kmetadata, envir = mp_cache()))
  expect_true(exists(paste0(kdatasetname, versionname), envir = mp_cache()))
  mpds_out <- capture.output(mpds2 <- mp_maindataset())  

  ## TODO This does download metadata because the ones where you get
  ## the error from separate_missings are not stored in the cache.
  ## This should disappear once this is fixed on THE API side.
  ## Then please uncomment the expect_false ... line
  after_loading <- capture.output(
    mm2 <- mp_metadata(subset(mpds2, countryname == "Austria")))
#   expect_false(grepl("Connecting", after_loading))
  expect_equal(sort(mm1$party), sort(mm2$party))

  ## load standard cache again and check that no metadata is in it
  mp_load_cache(file = "mp_cache.RData")
  expect_false(exists(kmetadata, envir = mp_cache()))
  expect_true(exists(paste0(kdatasetname, versionname), envir = mp_cache()))  

  ## clean up
  file.remove("mp_cache.RData")
  file.remove("another_cache.RData")
  
})

test_that("versioning is respected in cache", {
  
  
  
})

## for more caching tests with metadata see testmetadata.R
 

# TODO test copying and restoring of cache
# old version here:
# 
# head(mpds)
# mpds <- mp_maindataset() ## this not
# 
# removecache()
# 
# manifesto.setcachelocation("altcache")
# mpds <- mp_maindataset() # again long
# 
# lscache()
# 
# manifesto.copycache("storedcache")
# 
# mp_emptycache()
# 
# mpds <- mp_maindataset() # again long
# 
# removecache()
# 
# manifesto.setcachelocation("storedcache")
# 
# mpds <- mp_maindataset() # should be fast and use cache
# 
# ## corpus from cache
# train.sample <- data.frame(party=c(41113,41223,41320,41420,41521,
#                                    #41113,41223,41320,41420,41521,
#                                    41113,41320,41420,41521,
#                                    41113,41320,41420,41521
# ), date=c(200909,200909,200909,200909,200909,
#           #201309,201309,201309,201309,201309,
#           200209,200209,200209,200209,
#           199809,199809,199809,199809
# ))
# mp_emptycache()
# wanted <- subset(mpds, country==41 & edate > as.Date("2006-01-01"))
# corp <- mp_corpus(wanted)
# class(content(corp[[1]])) ## should be character
# corp <- mp_corpus(wanted)
# class(content(corp[[1]])) ## should be character
# 
# removecache() ## cleanup
# 
# 
# 
# NULL
