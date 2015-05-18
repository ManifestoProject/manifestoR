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

test_that("aggregating handbook version 5 codes works", {
  
  test_codes <- c(101, "201.2", "6014", "202.6", NA)
  aggregated_test_codes <- c(101, 201, 6014, 202, NA)
  expect_equal(aggregate_v5_to_v4(test_codes), aggregated_test_codes)
  
  doc <- ManifestoDocument(data.frame(text = "notext",
                                      cmp_code = test_codes))
  expect_equal(codes(aggregate_v5_to_v4(doc)), aggregated_test_codes)
  
  ## nothing should be changed here
  corp <- mp_corpus(countryname == "Czech Republic" & edate < as.Date("2010-01-01"))
  corp2 <- aggregate_v5_to_v4(corp)
  expect_equal(codes(corp), codes(corp2))
  
})