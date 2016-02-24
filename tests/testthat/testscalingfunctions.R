
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

test_that("Franzmann Kaiser scaling produces no error", {
  
  sample <- mpds %>% filter(country==41, date==199809) ## crashes if you use a country or election which has no weights, better error checking.
  fk <- franzmann(sample,basevalues=FALSE,smoothing=FALSE)
  s <- cbind(sample,fk)
  franzmann(sample,basevalues=TRUE,smoothing=FALSE)
  franzmann(sample,basevalues=FALSE,smoothing=TRUE) # does not work yet
  franzmann(sample,basevalues=TRUE,smoothing=TRUE) # does not work yet  
  
})
