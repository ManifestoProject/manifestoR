## db_api

set variable in manifestodb.apikey upfront to NULL

* manifestodb.get
* manifestodb.setAPIkey

json here or in manifesto_corpus?

## manifesto

(cache handling in here)

* export: manifesto.texts
* export: manifesto.originals
* export: manifesto.availability
* export: manifesto.getcore
* export: manifesto.listcoreversions

## cache

(export all)

* manifesto.setcachelocation
* manifesto.emptycache
* manifesto.iscacheuptodate

* manifesto.updatefolder
* manifesto.updatecache

* manifesto.savecache
* manifesto.loadfolder


## corpus

(export all -- but what about documentation?)

define tm class ManifestoDocument, ManifestoSource

## convenience (bad name)

(export all)

* vieworiginal
* factor corpus by code
* code frequencies to percentage-row
* filters (tm format)
* visitwebpage() ?