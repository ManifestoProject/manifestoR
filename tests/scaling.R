library(manifestoR)

manifesto.emptycache()
manifestodb.setapikey(key.file = "manifesto_apikey.txt")

mpds <- manifesto.maindataset()

mpds.fr <- subset(mpds, countryname=="France")

comparedf <- data.frame(from.dataset=mpds.fr$rile,
                        from.manifestoR=rile(mpds.fr))

tolerance <- 0.01
abs.error <- abs(comparedf$from.dataset - comparedf$from.manifestoR)
stopifnot(max(abs.error) < tolerance)
