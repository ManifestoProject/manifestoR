mp_setapikey(key.file = "../manifesto_apikey.txt")

test_that("bootstrapping from dataset with default parameters works", {
  
  mpds_ger <- subset(mp_maindataset(), countryname == "Germany")
  
  bootstrapped_default <- mp_bootstrap(mpds_ger)
  
  expect_named(bootstrapped_default, c("rile", "sd"))
  expect_equal(bootstrapped_default$rile, mpds_ger$rile, tolerance = 0.1)
    
})

test_that("bootstrapping quantiles works", {

  mpds_ger <- subset(mp_maindataset(), countryname == "Germany")

  test_fun <- function(x) {
    return(1.0)
  }
  
  bootstrapped_quantiles <- mp_bootstrap(mpds_ger,
                                         statistics = list("sd", "mean",
                                                           0.025, 0.975,
                                                           test_fun))
  print(names(bootstrapped_quantiles))
  
  
})
  