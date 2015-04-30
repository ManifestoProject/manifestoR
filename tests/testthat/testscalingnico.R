
mp_setapikey(key.file = "../manifesto_apikey.txt")

mpds <- mp_maindataset()

test_that("Vanilla scaling produces no error", {

  westeurope <- filter(mpds, country<70) %>% 
    filter(date > 198000) %>%
    select(1:76,rile)

  allpers <- select(westeurope, starts_with("per")) %>%
    select(-pervote) %>%
    select(1:56)

  ### vanilla test

  westeurope$vanilla.inv <- vanilla(westeurope, invert=1)
  westeurope$vanilla <- vanilla(westeurope, invert=0)

})
