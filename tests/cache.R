library(manifestoR)
manifestodb.setapikey(key.file = "manifesto_apikey.txt")

removecache <- function() {
  system(paste("rm -rf ", manifesto.getcachelocation()))
}
lscache <- function() {
  system(paste("ls -R", manifesto.getcachelocation()))
}

manifesto.listversions()

mpds <- manifesto.maindataset() ## this should take long
head(mpds)
mpds <- manifesto.maindataset() ## this not

removecache()

manifesto.setcachelocation("altcache")
mpds <- manifesto.maindataset() # again long

lscache()

manifesto.copycache("storedcache")

manifesto.emptycache()

mpds <- manifesto.maindataset() # again long

removecache()

manifesto.setcachelocation("storedcache")

mpds <- manifesto.maindataset() # should be fast and use cache

## corpus from cache
train.sample <- data.frame(party=c(41113,41223,41320,41420,41521,
                                   #41113,41223,41320,41420,41521,
                                   41113,41320,41420,41521,
                                   41113,41320,41420,41521
), date=c(200909,200909,200909,200909,200909,
          #201309,201309,201309,201309,201309,
          200209,200209,200209,200209,
          199809,199809,199809,199809
))
manifesto.emptycache()
wanted <- subset(mpds, country==41 & edate > as.Date("2006-01-01"))
corp <- manifesto.corpus(wanted)
class(content(corp[[1]])) ## should be character
corp <- manifesto.corpus(wanted)
class(content(corp[[1]])) ## should be character

removecache() ## cleanup



NULL