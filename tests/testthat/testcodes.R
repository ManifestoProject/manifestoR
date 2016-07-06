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
  cee_codes <- (as.integer(orig) >= 1000)
  expect_equal(orig[!cee_codes], mod[!cee_codes])
  expect_equal(as.character(floor(as.numeric(orig[cee_codes])/10)),
               mod[cee_codes])
  
}

test_that("general per aggregation function works", {
  
  test_data <- data.frame(per101 = c(1,2),
                          per102 = c(3,4),
                          per305 = c(5,6))
  
  aggregated <- aggregate_pers(test_data,
                               groups = list(per101_2 = c("per101", "per102")),
                               keep = TRUE)
  expect_equal(nrow(aggregated), nrow(test_data))
  expect_equal(setdiff(names(aggregated), "per101_2"), names(test_data))
  expect_equivalent(aggregated$per101_2, c(4, 6))
  
  aggregated <- aggregate_pers(test_data,
                               groups = list(per101_2 = c("per101", "per102")),
                               keep = FALSE)
  expect_equal(nrow(aggregated), nrow(test_data))
  expect_equal(setdiff(names(aggregated), "per101_2"),
               setdiff(names(test_data), c("per101", "per102")))
  expect_equivalent(aggregated$per101_2, c(4, 6))
  
  
})

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
  
  test_codes <- c("101", "201.2", "6014", "202.6", NA)
  aggregated_test_codes <- c("101", "201", "6014", "202", NA)
  expect_equal(recode_v5_to_v4(test_codes), aggregated_test_codes)
  
  doc <- ManifestoDocument(data.frame(text = "notext",
                                      cmp_code = test_codes))
  expect_equal(codes(recode_v5_to_v4(doc)), aggregated_test_codes)
  
  ## nothing should be changed here
  corp <- mp_corpus(countryname == "Czech Republic" & edate < as.Date("2010-01-01"))
  corp2 <- recode_v5_to_v4(corp)
  expect_equal(codes(corp), codes(corp2))
  
})

code_table_as_expected <- function(code_table, partydate = TRUE, prefix = "per",
                                   include_codes = v4_categories(),
                                   sum_regex = paste0(prefix, "(\\d{3}|(uncod))$")) {

  expect_is(code_table, "data.frame")
  expect_true("total" %in% names(code_table))
  if (partydate) {
    expect_true("party" %in% names(code_table))
    expect_true("date" %in% names(code_table))
  }
  if (length(include_codes) > 0) {
    expect_true(all(gsub(".", "_", paste0(prefix, include_codes), fixed = TRUE) %in% names(code_table)))
  }
  expect_false(code_table %>%
                select(starts_with(prefix), dplyr::matches("total")) %>%
                subset(total > 0L) %>%
                anyNA())
  code_table %>% 
    subset(total > 0L) %>%
    select(dplyr::matches(sum_regex)) %>%
    apply(1, sum) %>%
    na.omit() %>%
    { format(.) == "100" } %>%  ## Workaround for rounding errors
    all() %>%
    expect_true()
  code_table %>%
    subset(total == 0L) %>%
    select(starts_with(prefix)) %>%
    unlist() %>%
    sapply(function(x) { expect_true(is.na(x)) })
}

test_that("count_codes works for all intended types of objects", {
  
  corp <- mp_corpus(countryname == "Sweden")

  corp %>%
    count_codes() %>%
    code_table_as_expected()
  
  corp[[1]] %>%
    count_codes() %>%
    code_table_as_expected()
  
  c(101, 102, 608) %>%
    count_codes() %>%
    code_table_as_expected(partydate = FALSE)

})

test_that("include_codes works", {

  corp <- mp_corpus(countryname == "Sweden")
  
  corp %>%
    count_codes(include_codes = v5_categories()) %>%
    code_table_as_expected(include_codes = v5_categories())

  corp %>%
    count_codes(include_codes = c()) %>%
    code_table_as_expected(include_codes = c())

  corp[[1]] %>%
    count_codes(include_codes = c()) %>%
    code_table_as_expected(include_codes = c())

  c(101, 102, 608) %>%
    count_codes(include_codes = c()) %>%
    code_table_as_expected(partydate = FALSE, include_codes = c())

  c(101, 102, 608) %>%
    count_codes(include_codes = c("foo", "bar")) %>%
    code_table_as_expected(partydate = FALSE, include_codes = c("foo", "bar"))
})

test_that("count_codes works for handbook version 5", {
  
  corp <- mp_corpus(countryname == "Greece" & date == 200910)
  
  corp %>%
    count_codes() %>%
    code_table_as_expected()
  
  corp %>%
    count_codes(include_codes = v5_categories()) %>%
    code_table_as_expected(include_codes = v5_categories())
  
})

test_that("count_codes works for manually created ManifestoDocument", {

  df <- data.frame(text = c("bla", "bla"),
                   cmp_code = c(104, 108))

  doc <- ManifestoDocument(df)
  doc %>%
    count_codes() %>%
    code_table_as_expected(partydate = FALSE)

  doc <- ManifestoDocument(df, meta = ManifestoDocumentMeta(list(party = 12345,
                                                                 date = 201507)))
  doc %>%
    count_codes() %>%
    code_table_as_expected()

})

test_that("count_codes works for different code layers", {

  df <- data.frame(text = c("bla", "bla"),
                   cmp_code = c(104, 108),
                   eu_code = c(108, 0L),
                   additional_code = c("foo", NA))

  expect_without_eu <- function(code_table) {
    code_table_as_expected(code_table)
    expect_equal(code_table$total, 2)
    expect_equal(code_table$per104, 50)
    expect_equal(code_table$per108, 50)
  }

  expect_with_eu <- function(code_table) {
    code_table_as_expected(code_table)
    expect_equal(code_table$total, 3)
    expect_equal(code_table$per104, 1/3*100)
    expect_equal(code_table$per108, 2/3*100)
  }

  ManifestoDocument(df) %>%
    count_codes() %>%
    expect_without_eu()

  ManifestoDocument(df) %>%
    count_codes(with_eu_codes = TRUE) %>%
    expect_with_eu()

  ManifestoDocument(df, meta = ManifestoDocumentMeta(list(has_eu_code = TRUE))) %>%
    count_codes() %>%
    expect_with_eu()

  ManifestoDocument(df, meta = ManifestoDocumentMeta(list(has_eu_code = TRUE))) %>%
    count_codes(with_eu_codes = FALSE) %>%
    expect_without_eu()

  ## with additional codes
  code_table <- df %>%
                  ManifestoDocument() %>%
                  count_codes(code_layers = c("cmp_code", "additional_code"))
  code_table %>%
    code_table_as_expected(sum_regex = "per.*")
  expect_equal(code_table$total, 3)
  expect_equal(code_table$per104, 1/3*100)
  expect_equal(code_table$per108, 1/3*100)
  expect_equal(code_table$perfoo, 1/3*100)

  expect_warning(code_table <- df %>%
                   ManifestoDocument() %>%
                   count_codes(code_layers = c("cmp_code", "eu_code")))
  code_table %>%
    code_table_as_expected()
  expect_equal(code_table$total, 4)
  expect_equal(code_table$per104, 1/4*100)
  expect_equal(code_table$per108, 1/2*100)
  expect_equal(code_table$peruncod, 1/4*100)
  
  ## only additional code
  code_table <- df %>%
    ManifestoDocument() %>%
    count_codes(code_layers = c("additional_code")) %>%
    code_table_as_expected(include_codes = c(), sum_regex = "per.*")

  code_table <- df %>%
    ManifestoDocument() %>%
    count_codes(code_layers = c("additional_code"), include_codes = c("check")) %>%
    code_table_as_expected(include_codes = c("check"), sum_regex = "per.*")

})