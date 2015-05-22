
mp_setapikey(key.file = "../manifesto_apikey.txt")

mpds <- mp_maindataset()

test_that("Vanilla scaling produces no error", {

  allpers <- filter(mpds, country<70) %>% 
    filter(date > 198000) %>%
    select(matches("(^per(\\d{3}|(uncod))$)|(rile)"))

  ### vanilla test

  allpers$vanilla.inv <- vanilla(allpers, invert=1)
  allpers$vanilla <- vanilla(allpers, invert=0)

})
