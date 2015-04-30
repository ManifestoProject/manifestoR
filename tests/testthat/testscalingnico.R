
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


test_that("Franzmann Kaiser scaling produces no error", {
  
  sample <- mpds %>% filter(country==41, date==199809) ## crashes if you use a country or election which has no weights, better error checking.
  vars <- grep("per\\d{3}$", names(sample), value=TRUE)
  fk <- franzmann(sample,vars=vars,basevalues=FALSE,smoothing=FALSE)
  s <- cbind(sample,fk)
  franzmann(sample,vars=vars,basevalues=TRUE,smoothing=FALSE)
  franzmann(sample,vars=vars,basevalues=FALSE,smoothing=TRUE) # does not work yet
  franzmann(sample,vars=vars,basevalues=TRUE,smoothing=TRUE) # does not work yet  
  
})



###