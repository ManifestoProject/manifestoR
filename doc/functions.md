This is a list of functions that I think will/should be part of the manifestoR package


Database
========

* manifestor.get.codeddocs(dataset, caching=TRUE)

  * checks for codeddocs in the cache
  * if not in cache connect to database and get the text
  * returns either a corpus object or a dataframe with a new variable codeddocs -> document object
  * warn if more than one document (useprimaryonly=TRUE parameter?)

* manifestor.get.originaldocs(dataset, caching=TRUE)

    * similar to manifestor.get.codeddocs, but gets PDF into cache

* manifestor.setAPIkey(key)

* manifestor.setcachelocation(location="manifestocache")

* manifestor.emptycache()



Simple Manifesto Corpus Processing
==================================

* manifestor.viewpdf(observation or pdfhandle)

    * if pdfhandle: view directly; if an entire observation get pdf first

* manifestor.tofulltext(vector of document objects)

    * ?

* manifestor.getcodedunits(vector of document objects)

    * ?
    
* ... ?


Scaling?
========

...

