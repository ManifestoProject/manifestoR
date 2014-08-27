library(manifestoR)
manifestodb.setapikey(key.file = "manifesto_apikey.txt")

manifesto.emptycache()

## first get documents via ids
wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
metadata <- manifesto.meta(wanted)
print(read.csv(file.path(manifesto.getcachelocation(), "docmetadata.csv")))

wanted2 <- data.frame(party=c(41320, 41320), date=c(200509, 200209))
metadata2 <- manifesto.meta(wanted2)
print(read.csv(file.path(manifesto.getcachelocation(), "docmetadata.csv")))
print(metadata2)


## get documents based on the current core data set version
manifesto.emptycache()
mpds <- manifesto.maindataset()
print(names(mpds))
print(class(mpds$edate))

idxs <- which(mpds$party==41320
              & mpds$edate < as.Date("2010-01-01")
              & mpds$edate > as.Date("2001-01-01"))
wanted3 <- mpds[idxs,]
print(nrow(wanted3)) ## should give 3
metadata3 <- manifesto.meta(wanted3)
print(metadata3)

## get documents based on an old core data set version
mpdsold <- manifesto.maindataset("MPPI")
wanted4 <- mpdsold[which(mpdsold$party==41320
                         & mpdsold$edate < as.Date("1960-01-01")
                         & mpdsold$edate > as.Date("1955-01-01")),]
print(nrow(wanted4)) ## should give 1
manifesto.emptycache()
metadata4 <- manifesto.meta(wanted4)
print(metadata4)


## now try with a non-existant id
wantedfail <- data.frame(party=c(41320, 41320), date=c(200909, 200409))
metadatafail <- manifesto.meta(wantedfail) ## you should be warned
print(metadatafail)


## test the summary function
avl <- manifesto.availability(wanted)
summary(avl)
