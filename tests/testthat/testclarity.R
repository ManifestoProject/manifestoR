mp_setapikey("../manifesto_apikey.txt")

test_that("Relative measure of party size works and produces correct results", {

  c(1, 2, 5) %>%
    mp_rmps(threshold_sum = 0) %>%
    expect_equal(c(0.07, 0.23, 0.71), tolerance = 0.01)
  c(1, 2, 5, NA) %>%
    mp_rmps(threshold_sum = 0) %>%
    expect_equal(c(0.07, 0.23, 0.71, NA), tolerance = 0.01)
  c(12.5, 25, 62.5) %>%
    mp_rmps() %>%
    expect_equal(c(0.07, 0.23, 0.71), tolerance = 0.01)

  read.csv("../data/clarity_replication.csv") %>%
    group_by(country, edate) %>%
      mutate(rmps2 = mp_rmps(pervote)) %>% 
    ungroup() %>%
    { expect_equal(.$rmps, .$rmps2, tolerance = 0.01) }
})


test_that("Programmatic clarity works and produces correct results", {
  
  fake_data <- data.frame(
    country = c(1,  1,  1,   1,  1,  1,  1,  1),
    party   = c(11, 12, 13,  14, 15, 16, 17, 18),
    edate   = c(1,  1,  1,   1,  1,  1,  1,  1),
    pervote = c(10, 20, 50,  NA, NA, NA, NA, NA),
    issue1  = c(10, 0,  0,   NA, 10, 20, NA, 0),
    issue2  = c(20, 20, 0.5, NA, 10, 20, 25, 0),
    issue3  = c(40, 10, 80,  NA, 0,  20, 25, 0),
    issue4  = c(5,  30, 9.5, NA, 40, 20, 25, 0),
    issue5  = c(25, 40, 10,  NA, 40, 20, 25, 0),
    pc1     = c(0.309, 0.272, 0.683, NA, NA,    NA,    NA, NA),
    pc2     = c(0.318, 0.268, 0.666, NA, NA,    NA,    NA, NA),
    pc3     = c(0.300, 0.200, 0.800, NA, 0.200, 0.200, NA, 0)
  )
  fake_data_2 = filter(fake_data, party == 17)
  
  clarity_test_dimensions <- list(
      "dim1" = list(pole_1 = c("issue1", "issue2"), pole_2 = c("issue3")),
      "dim2" = list(pole_1 = c("issue4"), pole_2 = c("issue5"))
    )
  
  fake_data %>%
    mp_clarity(weighting_kind = "election", 
               weighting_source = "pervote",
               dimensions = clarity_test_dimensions) %>%
    expect_equal(fake_data$pc2, tolerance = 0.005)
  
  fake_data %>%
    group_by(country, edate) %>%
      mutate(rmps = mp_rmps(pervote)) %>%
    ungroup() %>%
    mp_clarity(weighting_kind = "election", 
               weighting_source = "rmps",
               dimensions = clarity_test_dimensions) %>%
    expect_equal(fake_data$pc1, tolerance = 0.005)
  
  fake_data %>%
    mp_clarity(weighting_kind = "manifesto",
               dimensions = clarity_test_dimensions) %>%
    expect_equal(fake_data$pc3, tolerance = 0.005)
  
  read.csv("../data/clarity_replication.csv") %>%
    mutate(., pc2 = mp_clarity(., 
                                weighting_kind = "election", 
                                weighting_source = "rmps",
                                dimensions = clarity_dimensions())) %>%
    { expect_equal(.$pc2, .$pc, tolerance = 0.01) }

  read.csv("../data/clarity_replication.csv") %>%
    group_by(country, edate) %>%
      mutate(rmps = mp_rmps(pervote)) %>%
    ungroup() %>%
    mutate(., pc2 = mp_clarity(., 
                                weighting_kind = "election", 
                                weighting_source = "rmps",
                                dimensions = clarity_dimensions())) %>%
    { expect_equal(.$pc2, .$pc, tolerance = 0.01) }

  expect_error(
    fake_data %>%
    mp_clarity(weighting_kind = "manifesto", 
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
    mp_clarity(weighting_kind = "manifesto", 
               weighting_source = "unknown",
               dimensions = clarity_test_dimensions)
  )
  
})
