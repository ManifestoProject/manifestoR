manifesto.emptycache()
manifestodb.setapikey(key.file = "../manifesto_apikey.txt")

mpds <- manifesto.maindataset()

test_that("rile computation from dataset equals dataset value", {
  
  mpds.fr <- subset(mpds, countryname=="France")

  expect_equal(as.vector(mpds.fr$rile), as.vector(rile(mpds.fr)), tolerance = 0.001)
  
})

## TODO more tests, of other functions

# comparedf$logit.rile <- logit.rile(mpds.fr)

# print(comparedf$logit.rile)
