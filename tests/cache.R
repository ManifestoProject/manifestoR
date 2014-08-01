library(manifestoR)

removecache <- function() {
  system(paste("rm -rf ", manifesto.getcachelocation()))
}
lscache <- function() {
  system(paste("ls -R", manifesto.getcachelocation()))
}

fl <- file("apikey.txt")
manifestodb.setapikey(key = readLines(fl, 1))
close.connection(fl)

manifesto.listversions()

mpds <- manifesto.maindataset() ## this should take long
head(mpds)
mpds <- manifesto.maindataset() ## this not

removecache()

manifesto.setcachelocation("altcache")
mpds <- manifesto.maindataset() # again long

lscache()

removecache()

NULL