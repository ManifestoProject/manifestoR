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

## test merging into cache
manifesto.emptycache()
meta.spd <- manifesto.meta(mpds[which(mpds$party==41320),])
meta.a2000 <- manifesto.meta(mpds[which(mpds$edate > as.Date("2010-01-01")),])
meta.kpd <- manifesto.meta(data.frame(party=41220, date=194908))
meta.kpd <- manifesto.meta(data.frame(party=41220, edate=as.Date("1949-08-14")))
meta.mixed <- manifesto.meta(data.frame(party=c(41320, 41220), date=c(200909, NA), edate=as.Date(c(NA, "1949-08-14"))))
meta.kpd <- manifesto.meta(data.frame(party=c(41220, NA), date=194908)) # should warn about ids

## requesting only a not available document
manifesto.emptycache()
meta.spd04 <- data.frame(party=c(41320), date=c(200409)) ## this does not exist
corp.spd04 <- manifesto.corpus(meta.spd04)
length(corp.spd04)

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

