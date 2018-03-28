mp_setapikey(key = "INVALID") ## all test in this file (Codebook tests) should run without valid API key

mp_emptycache()

test_that("Codebook has correct format", {
  
  codebook <- mp_codebook()
  
  ## check for column names
  codebook %>%
    names() %>%
    { setdiff(c("type", "domain_code", "domain_name", "code", "variable_name", "title", "description_md", "label"), .) } %>%
    expect_length(0L)
  
  ## all columns should be of type character
  codebook %>%
    lapply(class) %>%
    unlist() %>%
    unique() %>%
    expect_equal("character")
  
})

test_that("Codebook is correctly cached", {
  
  codebook <- mp_codebook(version = "MPDS2017b", cache = TRUE, chapter = "categories")
  
  ## Cache should contain codebook storage name
  mp_cache() %>%
    ls(envir = .) %>% 
    setdiff("MPCodebook_MPDS2017bcategories", .) %>%
    expect_length(0L)
  
  ## Cached value and function return should be identical
  cached_codebook <- get("MPCodebook_MPDS2017bcategories", envir = mp_cache())
  expect_identical(cached_codebook, codebook)
  
  ## Cache should not contain codebook after emptying
  mp_emptycache()
  mp_cache() %>%
    ls(envir = .) %>% 
    setdiff("MPCodebook_MPDS2017bcategories", .) %>%
    expect_length(1L)
  
  ## previously cached version should be identical to newly downloaded
  expect_identical(cached_codebook,
                   mp_codebook(version = "MPDS2017b", cache = FALSE, chapter = "categories"))
  
  ## Cache should not contain codebook after download with bypass
  mp_cache() %>%
    ls(envir = .) %>% 
    setdiff("MPCodebook_MPDS2017bcategories", .) %>%
    expect_length(1L)
  
})

test_that("Errors and warnings are output correctly", {
  
  ## only categories chapter is available
  expect_warning(mp_codebook(chapter = "all"))
  expect_error(mp_codebook(chapter = "all"))
  
  ## only codebooks after MPDS2017b are available
  expect_error(mp_codebook(version = "MPDS2017a"), "MPDS2017b.*refer")
  expect_error(mp_codebook(version = "MPDS2016b"), "MPDS2017b.*refer")
  
  ## other invalid versions throw a regular HTTP Error
  expect_error(mp_codebook(version = "MPDS2199z"), "HTTP.*404")
  
  
})
