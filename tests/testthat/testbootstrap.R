mp_setapikey(key.file = "../manifesto_apikey.txt")

test_that("bootstrapping from dataset with default parameters works", {

  mpds_ger <- subset(mp_maindataset(), countryname == "Germany")

  bootstrapped_default <- mp_bootstrap(mpds_ger)

  expect_named(bootstrapped_default, c("rile", "sd"))
  expect_equal(bootstrapped_default$rile, mpds_ger$rile, tolerance = 0.1)

})

test_that("bootstrapping quantiles and arbitrary functions works", {

  mpds_ger <- subset(mp_maindataset(), countryname == "Germany")

  test_fun <- function(x) {
    return(1.0)
  }

  bootstrapped_quantiles <- mp_bootstrap(mpds_ger,
                                         statistics = list(sd, mean,
                                                      0.025, 0.975,
                                                      test_fun))
  expect_named(bootstrapped_quantiles, c("rile", "sd", "mean", "q0.025", "q0.975", "test_fun"))
  expect_true(all(bootstrapped_quantiles$test_fun == 1.0))

})

test_that("bootstrapping from dataset with alternative scaling functions works", {

  mpds_ger <- subset(mp_maindataset(), countryname == "Germany")

  bootstrapped_logit <- mp_bootstrap(mpds_ger,
                                   fun = logit_rile,
                                   statistics = list(sd, mean))
  expect_named(bootstrapped_logit, c("logit_rile", "sd", "mean"))

  test_fun <- function(x) {
    return(rep(1.0, times = nrow(x)))
  }

  bootstrapped_constant <- mp_bootstrap(mpds_ger,
                                         fun = test_fun,
                                         statistics = list(sd, mean))

  expect_named(bootstrapped_constant, c("test_fun", "sd", "mean"))
  expect_true(all(bootstrapped_constant$test_fun == 1.0))
  expect_true(all(bootstrapped_constant$sd == 0.0))
  expect_true(all(bootstrapped_constant$mean == 1.0))

})
  