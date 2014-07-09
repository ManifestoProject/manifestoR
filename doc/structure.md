Notizen
=======

1 In welchem Format werden die Dokumente intern gehandlet?

    * ManifestoDocument S3 class definieren, die ...
      * ... erbt von PlainTextDocument, TextDocument --> content(), meta() implementieren
      * ... eine struct ist aus [data.frame mit text= code= vorhaelt], meta=[meta object aus tm]
      * siehe devel/manifestoRformatPlayground.R
    
    * zu Corpus konvertieren:
      * a tm corpus source that creates from the two JSON objects, meta and text, (and a filtering list of ids?) a tm Corpus

    
    
2 Wie sieht das DB Schema aus?

    * 1 Meta-Datenbank mit
        * partyid
        * electiondate
        * language
        * isprimarydoc flag
        * maycontradictcoredataset flag?
        * manifestoid
        * md5sum of text json object
        * link to original
        * md5sum of original
    
    * 1 Text-Datenbank mit
        * manifestoid
        * quasi sentence number
        * quasi sentence text
        * code
    
3 Wie wird die Textdatenbank mit dem Core Dataset verknuepft?

    * PartyCode+electiondate bilden ID fuer document (collection?)
    * isprimarydoc flag falls mehrere Dokumente
    * manifestos, deren Text wir nich haben/bereitstellen tauch nicht auf --> R package kuemmert sich drum und meldet das
    
4 API, die dem R package Zugriff auf die Text Datenbank gibt:

    * API keys werden erzeugt mit Anmeldung und bei jedem Zugriff mitgeschickt; muessen aber einmal aus dem User Profil im Browser kopiert werden
    
    * Funktionen:
        * get metadata(list of partyid-electiondate pairs) --> gzipped json des entsprechenden Datenbank subsets; wenn partyid-electiondate unavailable still include id in in JSON-return with unavailability flag!
        * get texts(list of manifestoids) --> gzipped json des entsprechenden Datenbank subsets
        * get coredataset(versionid)
        * get list of coredataset versions
        
        * get originals(list of manifestoids) --> gzipped files; stores in a folder on the server, named by manifestoid?
    
5 Wie kommen die Dokumente in die DB?

    * quasi-manuell, python Script bauen, dass einzelne .csv Dateien eincheckt
    * Wird die DB versioniert?
    

    
    