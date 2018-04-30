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
  
  ## Some codes should be definitely be in there
  codebook %>%
    filter(code %in% c("101", "405")) %>%
    nrow() %>%
    expect_gt(0L)
  
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
  tryCatch(expect_warning(mp_codebook(chapter = "all")), error = function(e){})
  tryCatch(expect_error(mp_codebook(chapter = "all")), warning = function(w){})
  
  ## only codebooks after MPDS2017b are available
  expect_error(mp_codebook(version = "MPDS2017a"), "MPDS2017b.*refer")
  expect_error(mp_codebook(version = "MPDS2016b"), "MPDS2017b.*refer")
  
  ## other invalid versions throw a regular HTTP Error
  expect_error(mp_codebook(version = "MPDS2199z"), "HTTP.*404")
  
  
})

test_that("Output format of mp_describe_code is correct", {
  
  cols <- c("code", "title", "description_md")
  description <- mp_describe_code("101")
  
  expect_true(is.list(description))
  expect_named(description, cols)
  
  for (i in description) expect_length(i, 1L)

})

test_that("Output format of mp_view_codebook is correct", {
  
  wid <- mp_view_codebook(version = "MPDS2017b")
  expect_equal(class(wid), c("datatables", "htmlwidget"))
  
  html_file <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(wid, html_file)
  html_code <- readLines(html_file)
  
  ## Check for keywords/key code in produced HTML
  expect_true(grepl("Manifesto Project Codebook", html_code) %>% any())
  expect_true(grepl("MPDS2017b", html_code) %>% any())
  expect_true(grepl("different version", html_code) %>% any())
  expect_true(grepl("condensed descriptions", html_code) %>% any())
  expect_true(grepl("datatables html-widget", html_code) %>% any())
  
})
