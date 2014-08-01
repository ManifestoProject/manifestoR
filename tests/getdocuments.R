library(manifestoR)

setwd("~/Documents/manifestoR/tests")
manifesto.emptycache()

fl <- file("apikey.txt")
manifestodb.setapikey(key = readLines(fl, 1))
close.connection(fl)

## first get documents via ids
wanted <- data.frame(party=c(41320, 41320), date=c(200909, 200509))
metadata <- manifesto.meta(wanted)
read.csv(file.path(manifesto.getcachelocation(), "docmetadata.csv"))

wanted2 <- data.frame(party=c(41320, 4132), date=c(200509, 200209))
metadata2 <- manifesto.meta(wanted2)
read.csv(file.path(manifesto.getcachelocation(), "docmetadata.csv"))
print(metadata2)


## then try from dataset TODO
