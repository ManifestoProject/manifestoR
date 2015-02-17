manifesto.emptycache()
manifestodb.setapikey(key.file = "../manifesto_apikey.txt")

mpds <- manifesto.maindataset()

test_that("rile computation from dataset equals dataset value", {
  
  mpds.fr <- subset(mpds, countryname=="France")

  expect_equal(as.vector(mpds.fr$rile), as.vector(rile(mpds.fr)), tolerance = 0.001)
  
})

scaling_as_expected <- function(corp, scalingfun, scalingname) {
  
  scale1 <- scalingfun(corp[[1]])
  expect_is(scale1, "numeric")
  expect_false(is.na(scale1))
  
  scale_corp <- scalingfun(corp)
  expect_is(scale_corp, "data.frame")
  expect_true(all(c("party", "date", scalingname) %in% names(scale_corp)))
  expect_false(any(is.na(scale_corp[,scalingname])))
  expect_equal(nrow(scale_corp), length(corp))
}

test_that("corpus and document scaling works", {
  
  corp <- manifesto.corpus(mpds.fr)

  scaling_as_expected(corp, rile, "rile")
#   scaling_as_expected(corp, logit_rile, "logit_rile")
  scaling_as_expected(corp, planeco, "planeco")
  scaling_as_expected(corp, markeco, "markeco")
  scaling_as_expected(corp, intpeace, "intpeace")
  scaling_as_expected(corp, welfare, "welfare")
  
})


## TODO more tests, of other functions

# comparedf$logit.rile <- logit.rile(mpds.fr)

# print(comparedf$logit.rile)
