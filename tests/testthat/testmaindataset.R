mp_setapikey(key.file = "../manifesto_apikey.txt")

mp_emptycache()

test_that("main data set is formatted correctly", {
  
  mpds <- mp_maindataset()
  expect_more_than(nrow(mpds), 3800)
  expect_more_than(ncol(mpds), 130)
  expect_true(all(c("country", "countryname",
                    "date", "edate",
                    "party", "per101", "rile") %in% names(mpds)))
  
})
