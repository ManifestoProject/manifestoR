manifestodb.setapikey(key.file = "../manifesto_apikey.txt")

manifesto.emptycache()

metadata_as_request <- function(request,
                                metadata,
                                expected_names = c("party", "date",
                                                   "manifesto_id", "annotations")) {
  
  expect_equal(nrow(metadata), nrow(request))
  expect_true(all(expected_names %in% names(metadata)))
  expect_equal(nrow(anti_join(metadata, request, by=c("party", "date"))), 0)
  
}

## first get documents via ids
test_that("simple metadata download works", {
  
  wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
  before_caching <- capture.output(metadata <- manifesto.meta(wanted))
  metadata_as_request(wanted, metadata)
  
  metadatacache <- get(kmetadata, envir = mp_cache)
  metadata_as_request(wanted, metadatacache)
  
  after_caching <- capture.output(metadata <- manifesto.meta(wanted))
  metadata_as_request(wanted, metadata)
  
  expect_true(length(setdiff(before_caching, after_caching)) > 0) ## connection message
  
  
})

test_that("metadata download half cache/half new works", {
  
  oldcache <- get(kmetadata, envir = mp_cache)
  
  wanted2 <- data.frame(party=c(41320, 41320), date=c(200909, 200209))
  metadata2 <- manifesto.meta(wanted2)
  metadata_as_request(wanted2, metadata2)
  
  newcache <- get(kmetadata, envir = mp_cache)
  
  expect_equal(nrow(newcache), nrow(oldcache) +
                 nrow(anti_join(wanted2, oldcache, by = c("party", "date"))))
  
  
})

test_that("get metadata based on main data set works", {
  
  mpds <- manifesto.maindataset()
  wanted3 <- subset(mpds,
                    party==41320 &
                    edate < as.Date("2010-01-01") &
                    edate > as.Date("2001-01-01"))
  metadata3 <- manifesto.meta(wanted3)
  metadata_as_request(wanted3, metadata3)
  
})

test_that("disabled cache does not change metadata cache", {
  
  oldcache <- get(kmetadata, envir = mp_cache)
  
  mpds <- manifesto.maindataset()
  wanted <- subset(mpds, countryname == "Norway")
  newmeta <- manifesto.meta(wanted, cache = FALSE)
  metadata_as_request(wanted, newmeta)
  
  newcache <- get(kmetadata, envir = mp_cache)
  
  expect_equal(length(setdiff(oldcache, newcache)), 0)
  
})

test_that("querying wrong ids gives warning", {
  
  wantedfail <- data.frame(party=c(41320, 41320), date=c(200909, 200409))
  wantedwork <- data.frame(party=c(41320), date=c(200909))
  
  expect_warning(metadatafail <- manifesto.meta(wantedfail))  
  metadata_as_request(wantedwork, metadatafail)
  
})

test_that("availability summary works", {
  
  mpds <- manifesto.maindataset()
  swe <- subset(mpds, countryname == "Sweden")
  avl <- manifesto.availability(swe)
  
  expect_true("availability" %in% names(avl))
  
  metadata_as_request(swe, avl$availability,
                      expected_names = c("party", "date", "annotations",
                                         "manifestos", "originals"))
  
})

# ## get documents based on an old core data set version
# # TODO This test is currently disabled, until the database formats are fixed
# # mpdsold <- manifesto.maindataset("MPPI")
# # wanted4 <- mpdsold[which(mpdsold$party==41320
# #                          & mpdsold$edate < as.Date("1960-01-01")
# #                          & mpdsold$edate > as.Date("1955-01-01")),]
# # print(nrow(wanted4)) ## should give 1
# # manifesto.emptycache()
# # metadata4 <- manifesto.meta(wanted4)
# # print(metadata4)
# 
# 
