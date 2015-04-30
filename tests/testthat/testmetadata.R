mp_setapikey(key.file = "../manifesto_apikey.txt")

mp_emptycache()

metadata_as_request <- function(request,
                                metadata,
                                expected_names = c("party", "date",
                                                   "manifesto_id", "annotations")) {
  
  expect_equal(nrow(metadata), nrow(request))
  expect_true(all(expected_names %in% names(metadata)))
  expect_equal(nrow(anti_join(metadata, request, by=c("party", "date"))), 0)
  
}

test_that("simple metadata download works", {
  
  wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
  before_caching <- capture.output(metadata <- mp_metadata(wanted))
  metadata_as_request(wanted, metadata)
  
  metadatacache <- get(kmetadata, envir = mp_cache())
  metadata_as_request(wanted, metadatacache)
  
  after_caching <- capture.output(metadata <- mp_metadata(wanted))
  metadata_as_request(wanted, metadata)
  
  expect_true(length(setdiff(before_caching, after_caching)) > 0) ## connection message
  
  
})

test_that("metadata download half cache/half new works", {
  
  oldcache <- get(kmetadata, envir = mp_cache())
  
  wanted2 <- data.frame(party=c(41320, 41320), date=c(200909, 200209))
  metadata2 <- mp_metadata(wanted2)
  metadata_as_request(wanted2, metadata2)
  
  newcache <- get(kmetadata, envir = mp_cache())
  
  expect_equal(nrow(newcache), nrow(oldcache) +
                 nrow(anti_join(wanted2, oldcache, by = c("party", "date"))))
  
  
})

test_that("get metadata based on main data set works", {
  
  mpds <- mp_maindataset()
  wanted3 <- subset(mpds,
                    party==41320 &
                    edate < as.Date("2010-01-01") &
                    edate > as.Date("2001-01-01"))
  metadata3 <- mp_metadata(wanted3)
  metadata_as_request(wanted3, metadata3)
  
})

test_that("disabled cache does not change metadata cache", {
  
  oldcache <- get(kmetadata, envir = mp_cache())
  
  mpds <- mp_maindataset()
  wanted <- subset(mpds, countryname == "Norway")
  newmeta <- mp_metadata(wanted, cache = FALSE)
  metadata_as_request(wanted, newmeta)
  
  newcache <- get(kmetadata, envir = mp_cache())
    
  expect_equal(length(base::setdiff(oldcache, newcache)), 0)
  
})

# test_that("querying wrong ids gives warning", {
#   
#   wantedfail <- data.frame(party=c(41320, 41320), date=c(200909, 200409))
#   wantedwork <- data.frame(party=c(41320), date=c(200909))
#   
#   expect_warning(metadatafail <- mp_metadata(wantedfail))  
#   metadata_as_request(wantedwork, metadatafail)
#   
# })

test_that("availability summary works", {

  mpds <- mp_maindataset()
  swe <- subset(mpds, countryname == "Sweden")
  avl <- mp_availability(swe)
  
  expect_true("availability" %in% names(avl))
  
  metadata_as_request(swe, avl$availability,
                      expected_names = c("party", "date", "annotations",
                                         "manifestos", "originals"))

})

test_that("non-standard evaluation for metadata works", {
  
  nse_meta <- mp_metadata(party == 64901, cache = FALSE)
  metadata_as_request(subset(mp_maindataset(), party == 64901),
                      nse_meta)
  
  nse_avl <- mp_availability(party == 61620, cache = FALSE)
  metadata_as_request(subset(mp_maindataset(), party == 61620),
                      nse_avl$availability,
                      expected_names = c("party", "date", "annotations",
                                         "manifestos", "originals"))
  
})

# ## get documents based on an old core data set version
# # TODO This test is currently disabled, until the database formats are fixed
# # mpdsold <- mp_maindataset("MPPI")
# # wanted4 <- mpdsold[which(mpdsold$party==41320
# #                          & mpdsold$edate < as.Date("1960-01-01")
# #                          & mpdsold$edate > as.Date("1955-01-01")),]
# # print(nrow(wanted4)) ## should give 1
# # mp_emptycache()
# # metadata4 <- mp_metadata(wanted4)
# # print(metadata4)
# 
# 
