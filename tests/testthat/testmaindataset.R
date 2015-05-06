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

