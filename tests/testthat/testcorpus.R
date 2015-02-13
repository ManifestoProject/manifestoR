manifestodb.setapikey(key.file = "../manifesto_apikey.txt")

manifesto.emptycache()
mpds <- manifesto.maindataset()
swe <- subset(mpds, countryname == "Sweden")
corp <- manifesto.corpus(swe)

skn <- subset(mpds, countryname == "Sweden" | countryname == "Norway")
corp2 <- manifesto.corpus(skn)

nor <- subset(mpds, countryname == "Norway")
corp3 <- manifesto.corpus(nor)

head(as.data.frame(corp2))
head(as.data.frame(corp2, with.meta = TRUE))
