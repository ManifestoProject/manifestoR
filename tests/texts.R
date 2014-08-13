library(manifestoR)

setwd("~/Documents/manifestoR/tests")
manifesto.emptycache()

fl <- file("apikey.txt")
manifestodb.setapikey(key = readLines(fl, 1))
close.connection(fl)

mpds <- manifesto.maindataset()

idxs <- which(mpds$party==41320
              & mpds$edate < as.Date("2010-01-01")
              & mpds$edate > as.Date("2001-01-01"))
wanted <- mpds[idxs,]
print(wanted)
metadata <- manifesto.meta(wanted)

corpus <- manifesto.texts(metadata) ## this should take long
corpus <- manifesto.texts(metadata) ## this not

## TODO more tests 


## - merging into cache

## basic tm corpus functionality
print(corpus)
summary(content(corpus[[2]]))
inspect(TermDocumentMatrix(corpus))

