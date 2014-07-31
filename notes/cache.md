## The Cache

The cache should in principle be a human- and machine-readable folder.
It contains the core dataset and corpus metadata as .csv in its root,
and has subfolders `texts` and `originals` for the respective parts.
It is managed in the .R file `cache.R`, although other parts might
write to it if given its path explicitly.

Hence, It can also be treated as a file and "saved" as well as "loaded"
for reproducing analyses. "Updating" of exactly the cache content is also
possible.



default name: ./manifestofiles/