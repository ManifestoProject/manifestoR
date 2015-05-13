mp_setapikey("../manifesto_apikey.txt")

test_that("main data set is formatted correctly", {
  
  mpds <- mp_maindataset()
  expect_more_than(nrow(mpds), 3800)
  expect_more_than(ncol(mpds), 130)
  expect_true(all(c("country", "countryname",
                    "date", "edate",
                    "party", "per101", "rile") %in% names(mpds)))
  
})

interpolation_as_expected <- function(interpol, mpds, vars) {
  
  expect_true(nrow(interpol) >= nrow(mpds))
  
  expect_true(all(vars %in% names(interpol)))
  
  expect_equal(unique(mpds$party), unique(interpol$party))
  
  sapply(unique(mpds$party), function(the_party) {
    sub_mpds <- subset(mpds, party == the_party)
    sub_interpol <- subset(interpol, party == the_party)
    expect_true(all(between(sub_interpol$edate,
                    min(sub_mpds$edate),
                    max(sub_mpds$edate))))
  })
  
  
}
test_that("interpolation works", {
  
  mpds <- subset(mp_maindataset(), countryname %in% c("Sri Lanka", "Switzerland"))
  vars <- grep("(^rile$)|(^per\\d{3,4}$)", names(mpds), value = TRUE)
  
  ## constant interpolation
  c_interpol <- mp_interpolate(mpds, by = "month", method = "constant")
  interpolation_as_expected(c_interpol, mpds, vars)

  expect_true(all(!is.na(c_interpol[,vars])))
  
  all_unique_sd <- function(df) {
    df  %>%
      select(one_of("party", vars)) %>%
      group_by(party) %>%
      summarise_each(funs(sd(unique(.))))
  }
  expect_equal(all_unique_sd(mpds), all_unique_sd(c_interpol))
  
  ## another zoo function
  s_interpol <- mp_interpolate(mpds, approx = na.spline, maxgap = 3)
  interpolation_as_expected(s_interpol, mpds, vars)
  
  ## custom function with modifiable parameters
  test_approx <- function(x, value = 1) {
    rep(value, times = length(x))
  }
  t_interpol <- mp_interpolate(mpds, approx = test_approx, value = 3)
  interpolation_as_expected(t_interpol, mpds, vars)
  expect_true(all(t_interpol %>%
                    anti_join(mpds, by = c("party", "edate")) %>%
                    select(one_of(vars))
                  == 3))
  
})

test_that("median voter computations work", {

  ## extracted data where adjusted makes no difference
  expect_equal(median_voter_single(
                    c(9.6, -37.8, 9.5, 28, 23.81),
                    c(10.3, 46.5, 12.9, 15.8, 13.6)),
              -8.546512, tolerance = 0.01)
  
  expect_equal(median_voter_single(
                    c(9.6, -37.8, 9.5, 28, 23.81),
                    c(10.3, 46.5, 12.9, 15.8, 13.6),
                    adjusted = TRUE),
              -8.546512, tolerance = 0.01)
  
  ## extracted data where adjusted makes a difference
  expect_equal(median_voter_single(
                    c(-36.111, -9.048, -11.574, 5.91),
                    c(65.895, 16.661, 7.415, 4.549)),
               -45.37972, tolerance = 0.01)
  
  expect_equal(median_voter_single(
                    c(-36.111, -9.048, -11.574, 5.91),
                    c(65.895, 16.661, 7.415, 4.549),
                    adjusted = TRUE),
               -30.781635, tolerance = 0.01)
  
})

median_voter_as_expected <- function(mm, mpds, adjusted = FALSE, scale = "rile", voteshare = "pervote") {

  expect_true("median_voter" %in% names(mm))
  expect_is(mm$median_voter %>% unlist(), "numeric")
  expect_equal(nrow(mm),
               nrow(mpds %>% group_by(country, edate) %>% summarise(n = n())))
  
  ## median_voter should be NA only if one of the input variables is NA
  mpds[,"scale"] <- mpds[,scale]
  mpds[,"voteshare"] <- mpds[,voteshare]
  mm %>%
    subset(is.na(median_voter)) %>%
    select(country, edate) %>%
    left_join(select(mpds, country, edate, scale, voteshare)) %>%
    group_by(country, edate) %>%
    summarise(scale_na = any(is.na(scale)),
              voteshare_na = any(is.na(voteshare)),
              too_few = (n() <= 1 & adjusted)) %>%
    mutate(any_problem = scale_na | voteshare_na | too_few) %>%
    ungroup() %>%
    summarise(all_any_problem = all(any_problem)) %>%
    select(all_any_problem) %>%
    as.logical() %>%
    expect_true()
    
}

test_that("median voter works on main data set", {
  
  mpds <- mp_maindataset()
  
  median_voter_as_expected(median_voter(mpds), mpds)

  median_voter_as_expected(median_voter(mpds, adjusted = TRUE), mpds, adjusted = TRUE)

  median_voter_as_expected(median_voter(mpds, scale = "per104"), mpds, scale = "per104")
  
})

