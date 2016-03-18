mp_setapikey("../manifesto_apikey.txt")

test_that("Relative measure of party size works and produces correct results", {

  c(1, 2, 5) %>%
    mp_rmps() %>%
    expect_equal(c(0.07, 0.23, 0.71), tolerance = 0.01)
  c(12.5, 25, 62.5) %>%
    mp_rmps() %>%
    expect_equal(c(0.07, 0.23, 0.71), tolerance = 0.01)

  read.csv("../data/clarity_replication.csv") %>%
    group_by(country, date) %>%
      mutate(rmps2 = mp_rmps(pervote)) %>% 
    ungroup() %>%
    { expect_equal(.$rmps, .$rmps2, tolerance = 0.02) }
})


test_that("Programmatic clarity works and produces correct results", {
  
  fake_data <- data.frame(
    country = c(1,   1,   1),
    party   = c(11,  12,  13),
    date    = c(1,   1,   1),
    pervote = c(1,   2,   5),
    issue1  = c(10,  0,   0),
    issue2  = c(20,  20,  0.5),
    issue3  = c(40,  10,  80),
    issue4  = c(5,   30,  9.5),
    issue5  = c(25,  40,  10),
    pc1     = c(0.309, 0.272, 0.683),
    pc2     = c(0.318, 0.268, 0.666),
    pc3     = c(0.300, 0.200, 0.800)
  )
  
  clarity_test_dimensions <- list(
      "dim1" = list(pole_1 = c("issue1", "issue2"), pole_2 = c("issue3")),
      "dim2" = list(pole_1 = c("issue4"), pole_2 = c("issue5"))
    )
  
  fake_data %>%
    mutate(rmps = mp_rmps(pervote)) %>%
    mp_clarity(weighting_kind = "country", 
               weighting_source = "rmps",
               dimensions = clarity_test_dimensions) %>%
    expect_equal(fake_data$pc1, tolerance = 0.005)
  
  fake_data %>%
    mp_clarity(weighting_kind = "country", 
               weighting_source = "pervote",
               dimensions = clarity_test_dimensions) %>%
    expect_equal(fake_data$pc2, tolerance = 0.005)
  
  fake_data %>%
    mp_clarity(weighting_kind = "party", 
               dimensions = clarity_test_dimensions) %>%
    expect_equal(fake_data$pc3, tolerance = 0.005)
  
  read.csv("../data/clarity_replication.csv") %>%
    mutate(., pc2 = mp_clarity(., 
                                weighting_kind = "country", 
                                weighting_source = "rmps",
                                dimensions = clarity_dimensions())) %>%
    { expect_equal(.$pc2, .$pc, tolerance = 0.01) }

  expect_error(
    fake_data %>%
    mp_clarity(weighting_kind = "party", 
               weighting_source = "rmps",
               dimensions = clarity_test_dimensions)
  )

  expect_error(
    fake_data %>%
    mp_clarity(weighting_kind = "unknown", 
               weighting_source = "pervote",
               dimensions = clarity_test_dimensions)
  )

  expect_error(
    fake_data %>%
    mp_clarity(weighting_kind = "party", 
               weighting_source = "unknown",
               dimensions = clarity_test_dimensions)
  )
  
})
