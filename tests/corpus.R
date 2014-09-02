library(manifestoR)

manifesto.emptycache()

try(manifesto.maindataset(), silent=FALSE) # should show error


manifestodb.setapikey(key.file = "manifesto_apikey.txt")

# setwd("~/Documents/manifestoR/tests")

mpds <- manifesto.maindataset()

idxs <- which(mpds$party==41320
              & mpds$edate < as.Date("2010-01-01")
              & mpds$edate > as.Date("2001-01-01"))
wanted <- mpds[idxs,]
print(wanted)

corpus <- manifesto.corpus(wanted) ## this should take long
print(corpus)
print(length(na.omit(content(corpus[[1]])))) ## should be 1
print(length(na.omit(content(corpus[[2]])))) ## should be 2278

## TODO test merging into cache

## this should yield the same
manifesto.emptycache()
metadata <- manifesto.meta(wanted)
corpus <- manifesto.corpus(metadata)

## basic tm corpus functionality
print(corpus)
summary(content(corpus[[2]]))
print(meta(corpus[[2]], "party"))
print(TermDocumentMatrix(corpus))

## specific ManifestoDocument functionality
print(table(codes(corpus[[2]])))
print(table(codes(corpus), useNA = "always"))

