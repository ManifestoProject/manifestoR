This is a list of functions that I think will/should be part of the manifestoR package


Database & Cache Organization
=============================

Cache: ein Metadaten-JSON-Objekt & ein Textdaten-JSON-Objekt in wd/manifestocache

Metadaten-Objekt enthaelt auch unavailables



* manifestodb.setAPIkey(key)

* manifestodb.setcachelocation(location="manifestocache")

* manifestodb.emptycache()

* manifestodb.iscacheuptodate()

    * get meta from cache
    * compare md5 sums to those from cache [create from files and do not use values from metadata --> also good download success check if they are computed and compared in manifestodb.get("text")]
    * how to handle originals?


Getting and Handling of Files/DB subsets
========================================

* manifestodb.get(type=["meta", "text", "original", "core", "versions"], parameters=c(), apikey=NULL, savelocation=NULL)

    * treat parameters differently, depending on type --> URL
    * If folder==NULL type gives standard name for cache [**add** to this file, but return only newly got structure]
    * If apikey==NULL use variable from workspace
    * meta: return data.frame
    * text: return list of data.frames
    * original: return list of paths to .pdfs/.docs
    * core: return data.frame
    * versions: return data.frame


* manifesto.texts([list of partyid-electiondate, subset of coredataset data.frame], cache=TRUE)

    * if first argument is data.frame extract partyid-electiondate pairs
    * check cache first if cache==TRUE
    * if meta-data complete for the required cases (possibly marked as unavailable) use this; otherwise get missing metadata via manifestodb.get("meta", ...), attach and save to cache
    * check for all cases in meta-data whether text is available (only if not marked as unavailable) in cache; get missing texts via manifestodb.get("text", ...), attach to cache
    * return corpus object [these would be writable directly into a subset of a data.frame as well, since they have [[]] operators]

* manifesto.originals([list of partyid-electiondate, subset of coredataset data.frame], cache=TRUE)

    * similar to manifesto.texts, but with originals
    * return data.frame with request ids and filehandles


* manifesto.availability(...)

    * same parameters as manifestotexts (also convenience version working with data.frame subset of core dataset)
    * gets the metadata and returns a dataframe for which observations a document was found and could be downloaded
    * drei columns: textavailable, originalaviable, textconsistentwithcoredataset
    * summary statistic: availability percentage
    

* manifesto.maindataset(version="current")

* manifesto.listversions()


Helper functions not needed by end user:

* a tm corpus source that creates from the two JSON objects, meta and text, (and a filtering list of ids?) a tm Corpus

* getting, unzipping, curling JSON objects into R structures, ...



Simple Manifesto Corpus Processing
==================================

* vieworiginal(observation or filehandle)

    * if filehandle: view directly; if an entire observation get pdf first
    
    * view: depends on 
    

* factor corpus by code

* code frequencies to percentage-row