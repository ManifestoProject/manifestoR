mp_setapikey(key.file = "../manifesto_apikey.txt")

mp_emptycache()

test_that("simple caching of listversions() works", {
  
  before_caching <- capture.output(list1 <- mp_listversions())
  after_caching <- capture.output(list2 <- mp_listversions())
  
  expect_equal(list1, list2)
  expect_true(length(setdiff(before_caching, after_caching)) > 0)  ## connection message
  expect_true(exists(kversions, envir = mp_cache))
  
  mp_emptycache()
  expect_equal(length(ls(envir = mp_cache)), 0)
  
  without_caching <- capture.output(list3 <- mp_listversions(cache = FALSE))
  expect_equal(length(ls(envir = mp_cache)), 0)
  
  after_emptying <- capture.output(mp_listversions())
  expect_true(length(setdiff(after_emptying, after_caching)) > 0) 
})


test_that("caching of main data set works", {
  
  versions <- mp_listversions()
  versionname <- as.character(versions[nrow(versions), "datasets.id"])  

  expect_false(exists(paste0(kdatasetname, versionname), envir = mp_cache))
  
  before_caching <- capture.output(mpds1 <- mp_maindataset())
  after_caching <- capture.output(mpds2 <- mp_maindataset())
  expect_equal(mpds1, mpds2)
  expect_true(length(setdiff(before_caching, after_caching)) > 0)  ## connection message
  expect_true(exists(paste0(kdatasetname, versionname), envir = mp_cache))
  
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
