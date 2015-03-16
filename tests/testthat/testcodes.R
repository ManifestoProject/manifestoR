mp_setapikey(key.file = "../manifesto_apikey.txt")

mp_emptycache()

check_cee_aggregation <- function(orig, mod) {
  
  table_orig <- table(orig)
  table_mod <- table(mod)
  expect_is(table_mod, "table")
  expect_equal(sum(table_orig), sum(table_mod))
  expect_true(all(as.integer(names(table_mod)) < 1000))
  
  orig <- na.omit(orig)
  mod <- na.omit(mod)
  cee_codes <- (orig >= 1000)
  expect_equal(orig[!cee_codes], mod[!cee_codes])
  expect_equal(floor(orig[cee_codes]/10), mod[cee_codes])
  
}

test_that("aggregating cee codes works", {
  
  corp <- mp_corpus(countryname == "Czech Republic")
  
  ## on vector
  codes_orig <- codes(corp)
  codes_mod <- aggregate_cee_codes(codes_orig)
  check_cee_aggregation(codes_orig, codes_mod)
  
  ## on document & corpus
  check_cee_aggregation(codes(corp[[1]]),
                        codes(aggregate_cee_codes(corp[[1]])))
  check_cee_aggregation(codes(corp),
                        codes(aggregate_cee_codes(corp)))
})