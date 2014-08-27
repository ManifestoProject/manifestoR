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

removecache() ## cleanup


NULL